package main

import (
	"errors"
	"flag"
	"fmt"
	"log"
	"net"
	"net/netip"
	"os"
	"runtime/pprof"
	"strings"
	"sync"
	"syscall"
	"time"
)

var backendAddr netip.AddrPort
var clientCount = 0
var serverCount = 0
var lastClientReport = int64(0)
var lastServerReport = int64(0)
var APPTXsuspended = false

type TranscieverState struct {
	registers              [256][4]byte
	knownregs              [256]bool
	revpower               uint16
	fwdpower               uint16
	sendPackets            [][]byte
	snapshotPacketBuffer   []byte
	lowStateBits           byte
	fillLevel              int
	fillLevelTime          time.Time
	sentSinceFillLevelTime int // towards TRX
}

type ClientState struct {
	addr                    netip.AddrPort
	udpSendSequence         uint32
	udpInsertions           uint32 // added to sequence
	transmit                bool
	transmitChangeTime      time.Time
	registers               [256][4]byte
	knownregs               [256]bool
	lastReceived            time.Time
	last83                  byte
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

func (self *MyLog) PrintfStd(f string, x ...any) {
	self.pipe <- LogMessage{time.Time{}, fmt.Sprintf(f, x...), false}
}

func (self *MyLog) Fatal(x ...any) {
	self.pipe <- LogMessage{time.Now(), fmt.Sprintln(x...), true}
	time.Sleep(100000 * time.Second)
}

func (s *TranscieverState) updateStateFromFrame(ctm time.Time, frame []byte) {
	// wants 512 bytes in frame
	if frame[0] == 0x7F && frame[1] == 0x7F && frame[2] == 0x7F {
		// sync ok.
		var control [5]byte
		copy(control[:], frame[3:8])
		//receivers := 1
		// 8 bytes above (3=sync, 5=control.
		//niq_samples := (512 - 8) / ((receivers * 6) + 2) // how samples are packed, so it's one sample size
		s.updateStateFromControl(ctm, control)
	}
}

func (s *TranscieverState) updateStateFromUDP(ctm time.Time, udpData []byte) {
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
				s.updateStateFromFrame(ctm, udpData[8:8+504])
				s.updateStateFromFrame(ctm, udpData[520:520+504])
			}
		}
	}
}

func (s *TranscieverState) getFillLevel(ctm time.Time) int {
	if s.fillLevel < 1 {
		s.fillLevel = 0
	}
	timeSince := int(ctm.Sub(s.fillLevelTime).Microseconds() * 1000) // microseconds
	samplesPerPacket := 2 * 63
	sentRate := 48000
	packetDuration := (1000000 * samplesPerPacket) / sentRate // microseconds
	packetsTransmittedToAir := timeSince / packetDuration
	packetsAddedSinceReport := s.sentSinceFillLevelTime
	return s.fillLevel - packetsTransmittedToAir + packetsAddedSinceReport
}

func (s *TranscieverState) updateStateFromControl(ctm time.Time, control [5]byte) {
	reg := (control[0] >> 3) & 0x1F
	s.lowStateBits = control[0] & 0x7
	s.knownregs[reg] = true

	switch reg {
	case 0:
		recovery := (control[3] & 0xC0) >> 6
		if recovery == 3 {
			s.fillLevel = 10000 // overflow
		} else if recovery == 2 {
			s.fillLevel = -1 // underflow
		} else {
			msb := control[3] & 0b00111111
			s.fillLevel = (int(msb) * 16) / 48
		}
		s.sentSinceFillLevelTime = 0
		s.fillLevelTime = ctm
	case 1:
		//adc := ((uint16(control[1])) << 8) | (uint16(control[2]) & 0xFF)
		//thisTemp := (3.26*(float64(adc)/4096.0) - 0.5) / 0.01
		//// Exponential moving average filter
		//alpha := 0.7
		//s.temperature = (alpha * thisTemp) + (1-alpha)*s.temperature
	case 2:
		//alex_reverse_power := (uint16(control[1]) << 8) | uint16(control[2]) // from Alex or Apollo
		//AIN3 := (control_in[3] << 8) + control_in[4];                                 // from Pennelope or Hermes
		//s.revpower = alex_reverse_power
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

func (s *ClientState) updateStateFromUDP(ctm time.Time, udpData []byte) {
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
				s.updateStateFromFrame(ctm, udpData[8:8+504])
				s.updateStateFromFrame(ctm, udpData[520:520+504])
				s.last83 = udpData[8+3] // ptt state etc
			}
		case 4: // start request from app
			logg.Printf("START/STOP stream request from APP: %d %d ( source: %v )", udpData[3]&0x01, (udpData[3]&0x02)>>1, s.addr)
			break
		}
	}
}

func (s *ClientState) updateStateFromFrame(ctm time.Time, frame []byte) {
	if frame[0] == 0x7F && frame[1] == 0x7F && frame[2] == 0x7F {
		newTransmit := frame[3]&0x01 == 1
		if newTransmit != s.transmit {
			logg.Println("\nTRANSMIT PTT FROM APP: %d", newTransmit)
			s.transmit = newTransmit
			s.transmitChangeTime = ctm
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

var buffers = make(chan []byte, 1000)

func obtainBuffer() []byte {
	select {
	case b := <-buffers:
		return b
	default:
		return make([]byte, 1200)
	}
}

func returnBuffer(b []byte) {
	if len(b) == 1200 {
		buffers <- b
	}
}

type Job struct {
	buffer []byte
	n      int
	addr   netip.AddrPort
}

func recvfromInet4(fd int, p []byte, flags int) (n int, from netip.AddrPort, err error) {
	type _Socklen uint32
	var rsa syscall.Sockaddr
	//var socklen _Socklen = syscall.SizeofSockaddrAny
	n, rsa, err = syscall.Recvfrom(fd, p, flags)
	if err != nil {
		return
	}
	inet6, ok := rsa.(*syscall.SockaddrInet6)
	if ok {
		var a4 netip.Addr
		a4 = netip.AddrFrom4([4]byte{inet6.Addr[12], inet6.Addr[13], inet6.Addr[14], inet6.Addr[15]})
		from = netip.AddrPortFrom(a4, uint16(inet6.Port))
	} else {
		inet4, ok := rsa.(*syscall.SockaddrInet4)
		if ok {
			var a4 netip.Addr
			a4 = netip.AddrFrom4(inet4.Addr)
			from = netip.AddrPortFrom(a4, uint16(inet4.Port))
		} else {
			err = errors.New("cannot deduce an address after syscall recvfrom")
		}
	}
	return
}

func handleFrontend(frontend *net.UDPConn, backend *net.UDPAddr, wg *sync.WaitGroup) {
	defer wg.Done()

	var transcieverState TranscieverState
	var clientState ClientState

	var recvChan = make(chan Job, 1000) // receive -> process
	var sendChan = make(chan Job, 1000) // process -> send

	sendToTrx := func(packet []byte, n int) {
		sendChan <- Job{packet, n, backendAddr}
		clientState.transmitFramesSentToTRX++
		transcieverState.sentSinceFillLevelTime++
	}

	go func() {
		// receiving loop
		for {
			var j Job
			var err error
			j.buffer = obtainBuffer()
			j.n, j.addr, err = frontend.ReadFromUDPAddrPort(j.buffer)
			if err != nil {
				logg.Fatal(err)
			}
			recvChan <- j
		}
	}()
	//go func() {
	//	// receiving loop
	//	osf, err := frontend.File()
	//	if err != nil {
	//		panic(err)
	//	}
	//	rfd := osf.Fd()
	//	for {
	//		var j Job
	//		var err error
	//		j.buffer = obtainBuffer()
	//		j.n, j.addr, err = recvfromInet4(int(rfd), j.buffer, 0)
	//		if err != nil {
	//			logg.Fatal(err)
	//		}
	//		//logg.Println("Received: ", j.n, j.addr)
	//		recvChan <- j
	//	}
	//}()
	go func() {
		// transmitting loop
		for {
			var j Job
			var err error
			j = <-sendChan
			_, _ = frontend.WriteToUDPAddrPort(j.buffer[:j.n], j.addr) // simulate to trx
			if err != nil {
				logg.Fatal(err)
			}
			returnBuffer(j.buffer)
		}
	}()
	go func() {
		for {
			j := <-recvChan
			ctm := time.Now()
			var err error

			ctmMilli := ctm.UnixMilli()
			//log.Println("recvd from: ", j.addr)
			if isSameAddress(j.addr, backendAddr) {
				// message from backend (trx)
				if clientState.addr.Port() != 0 {
					transcieverState.updateStateFromUDP(ctm, j.buffer[:j.n])
					// reply to incoming addr (client).
					if clientState.transmit {
						// don't send anything to the APP while transmitting
						if ctm.Sub(clientState.lastSentTowards) > 250*time.Millisecond {
							snapshotPacket := transcieverState.CreateSnapshotPacket()
							//logg.Println("Sent snapshot packet towards APP, len=", len(snapshotPacket))
							if !APPTXsuspended {
								sendChan <- Job{snapshotPacket, len(snapshotPacket), clientState.addr}
							}
							clientState.lastSentTowards = ctm
						}
					} else {
						if !APPTXsuspended {
							sendChan <- Job{j.buffer, j.n, clientState.addr}
						}
						//_, err = frontend.WriteToUDP(buffer[:n], &clientState.addr)
					}
					for _, pkt := range transcieverState.sendPackets {
						if !APPTXsuspended {
							sendChan <- Job{pkt, len(pkt), clientState.addr}
						}
						//_, err = frontend.WriteToUDP(pkt, &clientState.addr)
					}
					transcieverState.sendPackets = nil
					if ctmMilli-lastServerReport > 1000 {
						logg.Printf("[%d] Sent message (%d) to client %s\n", serverCount, j.n, clientState.addr.String())
						lastServerReport = ctmMilli
					}
					serverCount++
				}
			} else {
				// message from software
				if !isSameAddress(clientState.addr, j.addr) {
					clientState = ClientState{}
					clientState.addr = j.addr
					if *doSuspendClientTX {
						go func() {
							time.Sleep(5 * time.Second)
							APPTXsuspended = true
						}()
					}
					if *doCpuProfile {
						cpuProfile, err := os.Create("cpu.pprof")
						if err != nil {
							panic(err)
						}
						if err := pprof.StartCPUProfile(cpuProfile); err != nil {
							panic(err)
						}
						go func() {
							time.Sleep(5 * time.Second)
							pprof.StopCPUProfile()
							cpuProfile.Close()
							log.Println("ending cpu profile.")
							os.Exit(0)
						}()
					}
				}
				clientState.updateStateFromUDP(ctm, j.buffer[:j.n])
				clientCount++
				sendToTrx(j.buffer, j.n)
				dt := ctm.Sub(clientState.lastReceived)
				clientState.lastReceived = ctm
				if err != nil {
					logg.Println(err)
				}
				if ctmMilli-lastClientReport > 1000 || (clientState.transmit && *verboseTxTiming) {
					logg.Printf("[%d] dt=%v tx=%v Relayed message (len=%d) from APP to TRX  %s\n", clientCount, dt, clientState.transmit, j.n, j.addr.String())
					lastClientReport = ctmMilli
				}
			}

			verboseSimu := true
			timeToSimulateFromAPP := ctm.Sub(clientState.lastReceived) > 300*time.Millisecond
			if clientState.transmit {
				fl := transcieverState.getFillLevel(ctm)
				if fl < *fillLevel && fl >= 0 {
					timeToSimulateFromAPP = true
					//logg.Println("filling missing packet: queue len = ", transcieverState.fillLevel, "(", transcieverState.getFillLevel(ctm), ")", " since ", ctm.Sub(transcieverState.fillLevelTime))
					logg.PrintfStd("[T%d] ", fl)
					verboseSimu = false
				}
			}

			if timeToSimulateFromAPP {
				simulatedPacket := obtainBuffer()
				// client does not send anything. Maybe it knows we're hl2 proxy
				// maybe nothing changed.
				clientState.udpInsertions++
				newSeq := clientState.udpSendSequence + clientState.udpInsertions
				simulatedPacket[0] = 0xEF // dest endpoint
				simulatedPacket[1] = 0xFE // dest endpoint
				simulatedPacket[2] = 1    // iq data from APP
				simulatedPacket[3] = 0x02 // dest endpoint
				simulatedPacket[4] = byte(newSeq >> 24)
				simulatedPacket[5] = byte(newSeq >> 16)
				simulatedPacket[6] = byte(newSeq >> 8)
				simulatedPacket[7] = byte(newSeq)
				simulatedPacket[8] = 0x7F
				simulatedPacket[9] = 0x7F
				simulatedPacket[10] = 0x7F

				simulatedPacket[8+3] = byte(clientState.lastSentRegister<<3) | (clientState.last83 & 0x7)
				simulatedPacket[8+4] = clientState.registers[clientState.lastSentRegister][0]
				simulatedPacket[8+5] = clientState.registers[clientState.lastSentRegister][1]
				simulatedPacket[8+6] = clientState.registers[clientState.lastSentRegister][2]
				simulatedPacket[8+7] = clientState.registers[clientState.lastSentRegister][3]

				clientState.lastSentRegister++
				// fill register
				for ; clientState.lastSentRegister < 255 && !clientState.knownregs[clientState.lastSentRegister]; clientState.lastSentRegister++ {
				}
				if clientState.lastSentRegister >= 255 {
					clientState.lastSentRegister = 0
				}

				// fill in registers, in loop
				simulatedPacket[520+0] = 0x7F
				simulatedPacket[520+1] = 0x7F
				simulatedPacket[520+2] = 0x7F
				simulatedPacket[520+3] = byte(clientState.lastSentRegister<<3) | (clientState.last83 & 0x7)
				simulatedPacket[520+4] = clientState.registers[clientState.lastSentRegister][0]
				simulatedPacket[520+5] = clientState.registers[clientState.lastSentRegister][1]
				simulatedPacket[520+6] = clientState.registers[clientState.lastSentRegister][2]
				simulatedPacket[520+7] = clientState.registers[clientState.lastSentRegister][3]

				sendToTrx(simulatedPacket, 1032) // std pkt size
				clientState.lastReceived = ctm   // kinda received
				if verboseSimu {
					//logg.Println("Simulated client packet, newseq=", newSeq, " register=", clientState.lastSentRegister, ": error?=", err)
					logg.PrintfStd("[A] ")
				}
			}

		}
	}()

}

func isSameAddress(addr netip.AddrPort, addr2 netip.AddrPort) bool {
	if addr.Port() != addr2.Port() {
		return false
	}
	if addr.Addr().Is4() && addr2.Addr().Is4() {
		return addr.Addr().Compare(addr2.Addr()) == 0
	}
	return addr.Addr().As4() == addr2.Addr().As4()
}

var verboseTxTiming = flag.Bool("verbosetx", false, "Verbose log tx packets timings")
var fillLevel = flag.Int("fill", 4, "fill level to start simulate filling")
var doCpuProfile = flag.Bool("prof", false, "cpu profile few seconds of TRX->APP traffic")
var doSuspendClientTX = flag.Bool("testsct", false, "test suspend clent tx")
var doRawTest = flag.Bool("rawtest", false, "raw socket test")

func main() {
	// C.ctest()
	//log.Println("C loop exited..")

	logg.pipe = make(chan LogMessage, 10000)
	go func() {
		lastTime := time.Now()
		for {
			ch := <-logg.pipe
			if ch.when.IsZero() {
				str := ch.what
				fmt.Print(str)
			} else {
				pref := ""
				if strings.HasPrefix(ch.what, "\n") {
					pref = "\n"
					ch.what = ch.what[1:]
				}
				str := fmt.Sprintf("%s%02d:%02d:%02d.%03d (+%0.3f) %s",
					pref,
					ch.when.Hour(), ch.when.Minute(), ch.when.Second(),
					ch.when.Nanosecond()/1000000, float64(ch.when.Sub(lastTime).Nanoseconds())/1000000.0, ch.what)
				fmt.Print(str)
				lastTime = ch.when
			}
			if ch.fatal {
				os.Exit(1)
			}
		}
	}()
	go func() {
		for {
			prevServer, prevClient := serverCount, clientCount
			time.Sleep(1 * time.Second)
			logg.Println("\nPacket counts/1sec received: from TRX:", serverCount-prevServer, "  from APP:", clientCount-prevClient)
		}
	}()
	serverAddress := flag.String("hermes", "192.168.8.130:1024", "Hermes server address")
	clientAddress := flag.String("bind", ":1024", "Listen port for client connection")
	flag.Parse()
	if !flag.Parsed() {
		flag.Usage()
		logg.Fatal("ERROR: unable to parse flags")
	}
	if *doRawTest {
		go doRawSocketText()
	}

	var wg sync.WaitGroup

	// Resolve server and client addresses
	backendAddrx, err := net.ResolveUDPAddr("udp", *serverAddress)
	if err != nil {
		logg.Fatal(err)
	}
	backendAddr = backendAddrx.AddrPort()
	backendAddr = netip.MustParseAddrPort(*serverAddress)
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

func doRawSocketText() {
	conn, err := net.ListenPacket("ip4:udp", "0.0.0.0") // Listening for ICMP for demonstration
	if err != nil {
		fmt.Println("Error opening raw socket:", err)
		os.Exit(1)
	}
	defer conn.Close()

	// Buffer to read into
	buffer := make([]byte, 1024000)

	// Channel to signal every second
	ticker := time.NewTicker(time.Second)
	defer ticker.Stop()

	byteCount := 0
	packetCount := 0

	// Go routine to read from socket
	go func() {
		for {
			n, _, err := conn.ReadFrom(buffer)
			if err != nil {
				fmt.Println("Error reading from raw socket:", err)
				continue
			}
			// Process n bytes here
			//fmt.Printf("Received %d bytes\n", n)
			packetCount++
			byteCount += n
		}
	}()

	// Main loop
	for {
		select {
		case <-ticker.C:
			// Reset or display byte count here
			fmt.Println("RAWSOCKET: One second passed: ", packetCount, byteCount)
		}
	}
}
