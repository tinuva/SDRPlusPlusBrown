package main

import (
	"bytes"
	"flag"
	"fmt"
	"net"
	"os"
	"sync"
	"time"
)

var backendAddr net.UDPAddr
var clientCount = 0
var serverCount = 0
var lastClientReport = int64(0)
var lastServerReport = int64(0)

type TranscieverState struct {
	transmit               bool
	registers              [256][4]byte
	knownregs              [256]bool
	temperature            float64
	revpower               uint16
	fwdpower               uint16
	sendPackets            [][]byte
	snapshotPacketBuffer   []byte
	lowStateBits           byte
	fillLevel              int
	fillLevelTime          time.Time
	sentSinceFillLevelTime int
}

type ClientState struct {
	addr                    net.UDPAddr
	udpSendSequence         uint32
	udpInsertions           uint32 // added to sequence
	transmit                bool
	transmitChangeTime      time.Time
	registers               [256][4]byte
	knownregs               [256]bool
	lastClientPacket        []byte
	lastReceived            time.Time
	lastSentTowards         time.Time
	lastSentRegister        int
	transmitFramesSentToTRX int // how many frames sent to trx since transmitChangeTime if transmitting
}

type LogMessage struct {
	when  time.Time
	what  string
	fatal bool
}

type MyLog struct {
	pipe chan LogMessage
}

var logg MyLog

func (self *MyLog) Println(x ...any) {
	self.pipe <- LogMessage{time.Now(), fmt.Sprintln(x...), false}
}

func (self *MyLog) Printf(f string, x ...any) {
	self.pipe <- LogMessage{time.Now(), fmt.Sprintf(f, x...), false}
}

func (self *MyLog) Fatal(x ...any) {
	self.pipe <- LogMessage{time.Now(), fmt.Sprintln(x...), true}
	time.Sleep(100000 * time.Second)
}

func (s *TranscieverState) updateStateFromFrame(frame []byte) {
	// wants 512 bytes in frame
	if frame[0] == 0x7F && frame[1] == 0x7F && frame[2] == 0x7F {
		// sync ok.
		var control [5]byte
		copy(control[:], frame[3:8])
		//receivers := 1
		// 8 bytes above (3=sync, 5=control.
		//niq_samples := (512 - 8) / ((receivers * 6) + 2) // how samples are packed, so it's one sample size
		s.updateStateFromControl(control)
	}
}

func (s *TranscieverState) updateStateFromUDP(udpData []byte) {
	if udpData[0] == 0xEF && udpData[1] == 0xFE {
		if udpData[2] == 2 || udpData[2] == 3 { // discovery response
			logg.Println("TRX -> APP sent 28 code as information about proxy")
			dup := make([]byte, len(udpData))
			copy(dup, udpData)
			dup[2] = 28 // indicates extended proxy (hl_2 proxy) presence.
			s.sendPackets = append(s.sendPackets, dup)
		}
		if udpData[2] == 1 { // iq data stream
			endpoint := udpData[3]
			if endpoint == 6 {
				s.updateStateFromFrame(udpData[8 : 8+504])
				s.updateStateFromFrame(udpData[520 : 520+504])
			}
		}
	}
}

func (s *TranscieverState) updateStateFromControl(control [5]byte) {
	reg := (control[0] >> 3) & 0x1F
	s.lowStateBits = control[0] & 0x7
	s.transmit = control[0]&0x1 != 0
	s.knownregs[reg] = true

	switch reg {
	case 0:
		recovery := (control[3] & 0xC0) >> 6
		if recovery == 3 {
			s.fillLevel = 10000.0 // overflow
		} else if recovery == 2 {
			s.fillLevel = -1 // underflow
		} else {
			msb := control[3] & 0b00111111
			s.fillLevel = int(float64(msb) * 16.0 / 48.0) // 0..21
			s.fillLevelTime = time.Now()
			s.sentSinceFillLevelTime = 0
		}
	case 1:
		adc := ((uint16(control[1])) << 8) | (uint16(control[2]) & 0xFF)
		thisTemp := (3.26*(float64(adc)/4096.0) - 0.5) / 0.01
		// Exponential moving average filter
		alpha := 0.7
		s.temperature = (alpha * thisTemp) + (1-alpha)*s.temperature
	case 2:
		alex_reverse_power := (uint16(control[1]) << 8) | uint16(control[2]) // from Alex or Apollo
		//AIN3 := (control_in[3] << 8) + control_in[4];                                 // from Pennelope or Hermes
		s.revpower = alex_reverse_power
	}

}

func (s *TranscieverState) CreateSnapshotPacket() []byte {
	if s.snapshotPacketBuffer == nil {
		s.snapshotPacketBuffer = make([]byte, 300) // it's basically only few regs
	}
	// will produce standard packet to 28 th endpoint
	buf := s.snapshotPacketBuffer
	buf[0] = 0xEF
	buf[1] = 0xFE
	buf[2] = 28
	buf[3] = 6
	buf[4] = 0
	buf[5] = 0
	buf[6] = 0
	buf[7] = 0
	buf[8] = 0x7F
	buf[9] = 0x7F
	buf[10] = 0x7F
	dest := 11
	for ix, r := range s.knownregs {
		if r {
			buf[dest+0] = s.lowStateBits | uint8(ix<<3)
			buf[dest+1] = s.registers[ix][0]
			buf[dest+2] = s.registers[ix][1]
			buf[dest+3] = s.registers[ix][2]
			buf[dest+4] = s.registers[ix][3]
			dest += 5
		}
	}
	buf[dest+0] = 0xFF // end marker
	dest++
	return buf[:dest]
}

func (s *ClientState) updateStateFromUDP(udpData []byte) {
	if udpData[0] == 0xEF && udpData[1] == 0xFE {
		switch udpData[2] {
		case 1: // iq data from APP
			endpoint := udpData[3]
			// force update sequence. Keeping same sequence numbers, adding inserted (from proxy only -> TRX) packets.
			s.udpSendSequence = uint32(udpData[4])<<24 | uint32(udpData[5])<<16 | uint32(udpData[6])<<8 | uint32(udpData[7])
			insertions := s.udpInsertions
			udpData[4] = byte((s.udpSendSequence + insertions) >> 24)
			udpData[5] = byte((s.udpSendSequence + insertions) >> 16)
			udpData[6] = byte((s.udpSendSequence + insertions) >> 8)
			udpData[7] = byte(s.udpSendSequence + insertions)
			if endpoint == 2 {
				s.updateStateFromFrame(udpData[8 : 8+504])
				s.updateStateFromFrame(udpData[520 : 520+504])
				s.lastClientPacket = udpData
			}
		case 4: // start request from app
			logg.Printf("START/STOP stream request from APP: %d %d ( source: %v )", udpData[3]&0x01, (udpData[3]&0x02)>>1, s.addr)
			break
		}
	}
}

func (s *ClientState) updateStateFromFrame(frame []byte) {
	if frame[0] == 0x7F && frame[1] == 0x7F && frame[2] == 0x7F {
		newTransmit := frame[3]&0x01 == 1
		if newTransmit != s.transmit {
			s.transmit = newTransmit
			s.transmitChangeTime = time.Now()
			if !newTransmit {
				s.transmitFramesSentToTRX = 0
			}
		}
		register := frame[3] >> 1
		s.knownregs[register] = true
		cin1 := frame[4]
		cin2 := frame[5]
		cin3 := frame[6]
		cin4 := frame[7]
		if s.registers[register][0] != cin1 ||
			s.registers[register][1] != cin2 ||
			s.registers[register][2] != cin3 ||
			s.registers[register][3] != cin4 {
			// changed
			s.registers[register][0] = cin1
			s.registers[register][1] = cin2
			s.registers[register][2] = cin3
			s.registers[register][3] = cin4

			changed := true

			if changed {
				logg.Printf("APP register: %x %x %x %x\n", register, frame[4], frame[5], frame[6])
			}
		}
	}
}

func handleFrontend(frontend *net.UDPConn, backend *net.UDPAddr, wg *sync.WaitGroup) {
	defer wg.Done()

	buffer := make([]byte, 10240)

	var transcieverState TranscieverState
	var clientState ClientState

	sendToTrx := func(packet []byte) {
		_, _ = frontend.WriteToUDP(packet, backend) // simulate to trx
		clientState.transmitFramesSentToTRX++
		transcieverState.sentSinceFillLevelTime++
	}

	for {
		n, addr, err := frontend.ReadFromUDP(buffer)
		if err != nil {
			logg.Fatal(err)
		}
		if addr.IP.String() == backendAddr.IP.String() {
			// message from backend (trx)
			if clientState.addr.Port != 0 {
				transcieverState.updateStateFromUDP(buffer[:n])
				// reply to incoming addr (client).
				if clientState.transmit {
					// don't send anything to the APP while transmitting
					if time.Since(clientState.lastSentTowards) > 250*time.Millisecond {
						snapshotPacket := transcieverState.CreateSnapshotPacket()
						logg.Println("Sent snapshot packet towards APP, len=", len(snapshotPacket))
						_, err = frontend.WriteToUDP(snapshotPacket, &clientState.addr)
						clientState.lastSentTowards = time.Now()
					}
				} else {
					_, err = frontend.WriteToUDP(buffer[:n], &clientState.addr)
				}
				for _, pkt := range transcieverState.sendPackets {
					_, err = frontend.WriteToUDP(pkt, &clientState.addr)
				}
				transcieverState.sendPackets = nil
				tm := time.Now().UnixMilli()
				if tm-lastServerReport > 1000 {
					logg.Printf("[%d] Sent message (%d) to client %s\n", serverCount, n, clientState.addr.String())
					lastServerReport = tm
				}
				serverCount++
			}
		} else {
			// message from software
			if !bytes.Equal(clientState.addr.IP, addr.IP) || clientState.addr.Port != addr.Port {
				clientState = ClientState{}
				clientState.addr = *addr
			}
			clientState.updateStateFromUDP(buffer[:n])
			clientCount++
			sendToTrx(buffer[:n])
			dt := time.Since(clientState.lastReceived)
			clientState.lastReceived = time.Now()
			if err != nil {
				logg.Println(err)
			}
			tm := time.Now().UnixMilli()
			if tm-lastClientReport > 1000 || (clientState.transmit && *verboseTxTiming) {
				logg.Printf("[%d] dt=%v tx=%v Relayed message (len=%d) from APP to TRX  %s\n", clientCount, dt, clientState.transmit, n, addr.String())
				lastClientReport = tm
			}
		}

		timeToSimulateFromAPP := time.Since(clientState.lastReceived) > 300*time.Millisecond
		if clientState.transmit {
			if transcieverState.fillLevel < 4 && transcieverState.fillLevel >= 0 {
				timeToSimulateFromAPP = true
				logg.Println("filling missing packet: queue len = ", transcieverState.fillLevel, " since ", time.Since(transcieverState.fillLevelTime))
			}
		}

		if timeToSimulateFromAPP && clientState.lastClientPacket != nil {
			// client does not send anything. Maybe it knows we're hl2 proxy
			// maybe nothing changed.
			clientState.udpInsertions++
			newSeq := clientState.udpSendSequence + clientState.udpInsertions
			clientState.lastClientPacket[3] = 0x02 // dest endpoint
			clientState.lastClientPacket[4] = byte(newSeq >> 24)
			clientState.lastClientPacket[5] = byte(newSeq >> 16)
			clientState.lastClientPacket[6] = byte(newSeq >> 8)
			clientState.lastClientPacket[7] = byte(newSeq)
			clientState.lastClientPacket[8] = 0x7F
			clientState.lastClientPacket[9] = 0x7F
			clientState.lastClientPacket[10] = 0x7F

			clientState.lastClientPacket[8+3] = byte(clientState.lastSentRegister<<3) | (clientState.lastClientPacket[8+3] & 0x7)
			clientState.lastClientPacket[8+4] = clientState.registers[clientState.lastSentRegister][0]
			clientState.lastClientPacket[8+5] = clientState.registers[clientState.lastSentRegister][1]
			clientState.lastClientPacket[8+6] = clientState.registers[clientState.lastSentRegister][2]
			clientState.lastClientPacket[8+7] = clientState.registers[clientState.lastSentRegister][3]

			clientState.lastSentRegister++
			// fill register
			for ; clientState.lastSentRegister < 255 && !clientState.knownregs[clientState.lastSentRegister]; clientState.lastSentRegister++ {
			}
			if clientState.lastSentRegister >= 255 {
				clientState.lastSentRegister = 0
			}

			// fill in registers, in loop
			clientState.lastClientPacket[520+0] = 0x7F
			clientState.lastClientPacket[520+1] = 0x7F
			clientState.lastClientPacket[520+2] = 0x7F
			clientState.lastClientPacket[520+3] = byte(clientState.lastSentRegister<<3) | (clientState.lastClientPacket[8+3] & 0x7)
			clientState.lastClientPacket[520+4] = clientState.registers[clientState.lastSentRegister][0]
			clientState.lastClientPacket[520+5] = clientState.registers[clientState.lastSentRegister][1]
			clientState.lastClientPacket[520+6] = clientState.registers[clientState.lastSentRegister][2]
			clientState.lastClientPacket[520+7] = clientState.registers[clientState.lastSentRegister][3]

			sendToTrx(clientState.lastClientPacket)
			clientState.lastReceived = time.Now() // kinda received
			logg.Println("Simulated client packet, newseq=", newSeq, " register=", clientState.lastSentRegister, ": error?=", err)
		}
	}
}

var verboseTxTiming = flag.Bool("verbosetx", false, "Verbose log tx packets timings")

func main() {
	logg.pipe = make(chan LogMessage, 10000)
	go func() {
		lastTime := time.Now()
		for {
			ch := <-logg.pipe
			str := fmt.Sprintf("%02d:%02d:%02d.%03d (+%0.3f) %s",
				ch.when.Hour(), ch.when.Minute(), ch.when.Second(),
				ch.when.Nanosecond()/1000000, float64(ch.when.Sub(lastTime).Nanoseconds())/1000000.0, ch.what)
			fmt.Print(str)
			lastTime = ch.when
			if ch.fatal {
				os.Exit(1)
			}
		}
	}()
	serverAddress := flag.String("hermes", "192.168.8.130:1024", "Hermes server address")
	clientAddress := flag.String("bind", ":1024", "Listen port for client connection")
	flag.Parse()
	if !flag.Parsed() {
		flag.Usage()
		logg.Fatal("ERROR: unable to parse flags")
	}

	var wg sync.WaitGroup

	// Resolve server and client addresses
	backendAddrx, err := net.ResolveUDPAddr("udp", *serverAddress)
	if err != nil {
		logg.Fatal(err)
	}
	backendAddr = *backendAddrx
	frontendAddr, err := net.ResolveUDPAddr("udp", *clientAddress)
	if err != nil {
		logg.Fatal(err)
	}

	// Create the server and client connections
	frontendConn, err := net.ListenUDP("udp", frontendAddr)
	if err != nil {
		logg.Fatal(err)
	}
	logg.Println("Listening on frontend " + frontendAddr.String() + " (wanted: " + *clientAddress + ")")
	defer frontendConn.Close()

	backendConn, err := net.DialUDP("udp", nil, backendAddrx)
	if err != nil {
		logg.Fatal(err)
	}
	defer backendConn.Close()

	logg.Println("Proxy started...")

	// Handle incoming connections in separate goroutines
	wg.Add(2)
	go handleFrontend(frontendConn, backendAddrx, &wg)

	wg.Wait()
}
