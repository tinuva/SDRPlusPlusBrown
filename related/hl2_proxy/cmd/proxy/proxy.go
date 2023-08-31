package main

import (
	"flag"
	"log"
	"net"
	"sync"
	"time"
)

var incomingAddr net.UDPAddr
var backendAddr net.UDPAddr
var clientCount = 0
var serverCount = 0
var lastClientReport = int64(0)
var lastServerReport = int64(0)

func handleFrontend(frontend *net.UDPConn, backend *net.UDPAddr, wg *sync.WaitGroup) {
	defer wg.Done()

	buffer := make([]byte, 1024)

	for {
		n, addr, err := frontend.ReadFromUDP(buffer)
		if err != nil {
			log.Fatal(err)
		}
		if addr.IP.String() == backendAddr.IP.String() {
			// message from backend
			if incomingAddr.Port != 0 {
				_, err = frontend.WriteToUDP(buffer[:n], &incomingAddr)
				tm := time.Now().UnixMilli()
				if tm-lastServerReport > 1000 {
					log.Printf("[%d] Sent message (%d) to client %s\n", serverCount, n, incomingAddr.String())
					lastServerReport = tm
				}
				serverCount++
			}
			continue
		}
		incomingAddr = *addr
		clientCount++
		_, err = frontend.WriteToUDP(buffer[:n], backend)
		if err != nil {
			log.Fatal(err)
		}
		tm := time.Now().UnixMilli()
		if tm-lastClientReport > 1000 {
			log.Printf("[%d] Sent message (%d) to server from client %s\n", clientCount, n, addr.String())
			lastClientReport = tm
		}
	}
}

func main() {
	serverAddress := flag.String("hermes", "192.168.8.130:1024", "Hermes server address")
	clientAddress := flag.String("bind", ":1024", "Listen port for client connection")
	flag.Parse()
	if !flag.Parsed() {
		flag.Usage()
		log.Fatal("ERROR: unable to parse flags")
	}

	var wg sync.WaitGroup

	// Resolve server and client addresses
	backendAddrx, err := net.ResolveUDPAddr("udp", *serverAddress)
	if err != nil {
		log.Fatal(err)
	}
	backendAddr = *backendAddrx
	frontendAddr, err := net.ResolveUDPAddr("udp", *clientAddress)
	if err != nil {
		log.Fatal(err)
	}

	// Create the server and client connections
	frontendConn, err := net.ListenUDP("udp", frontendAddr)
	if err != nil {
		log.Fatal(err)
	}
	log.Println("Listening on frontend " + frontendAddr.String() + " (wanted: " + *clientAddress + ")")
	defer frontendConn.Close()

	backendConn, err := net.DialUDP("udp", nil, backendAddrx)
	if err != nil {
		log.Fatal(err)
	}
	defer backendConn.Close()

	log.Println("Proxy started...")

	// Handle incoming connections in separate goroutines
	wg.Add(2)
	go handleFrontend(frontendConn, backendAddrx, &wg)

	wg.Wait()
}
