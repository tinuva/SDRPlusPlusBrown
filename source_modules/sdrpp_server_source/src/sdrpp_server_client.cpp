#include "sdrpp_server_client.h"
#include <volk/volk.h>
#include <cstring>
#include <signal_path/signal_path.h>
#include <utils/flog.h>
#include <core.h>

using namespace std::chrono_literals;

namespace server {


    struct RemoteTransmitter : public Transmitter {
        Client* client;

        RemoteTransmitter(Client* client, const std::string someState) : client(client) {
            parseState(someState);
        }

        void sendTransmitAction(const json &action) {
            auto str = action.dump();
            strcpy((char*)client->s_cmd_data, str.c_str());
            client->sendCommand(COMMAND_TRANSMIT_ACTION, str.length()+1); // including zero, for conv.
        }

        bool txStatus;

        void setTransmitStatus(bool status) override {
            auto rv = json({});
            rv["transmitStatus"] = status;
            sendTransmitAction(rv);
            txStatus = status;
        }

        dsp::stream<dsp::complex_t> *txstream;

        static const int  TX_SEND_BUFFER_SAMPLES = 2400;
        void setTransmitStream(dsp::stream<dsp::complex_t> *astream) override {
            std::thread([this, astream]() {
                SetThreadName("sdrpp_server_client.txstream");
                auto debug = true;
                std::vector<dsp::complex_t> buffer;
                int addedBlocks = 0;
                int readSamples = 0;
                int nreads = 0;
                while (true) {
                    int rd = astream->read();
                    if (rd < 0) {
                        printf("End iq stream for tx");
                        break;
                    }
                    readSamples += rd;
                    nreads++;
                    for (int q = 0; q < rd; q++) {
                        buffer.push_back(astream->readBuf[q]);
                        if (buffer.size() == TX_SEND_BUFFER_SAMPLES) {
                            sendBuffer(buffer);
                            buffer.clear();
                        }
                    }
                    astream->flush();
                }
                buffer.clear();
                sendBuffer(buffer);     // empty buffer = end of transmission.
            }).detach();
        }

        uint8_t txStreamBuffer[TX_SEND_BUFFER_SAMPLES * sizeof(dsp::complex_t) + 1024];

        void sendBuffer(std::vector<dsp::complex_t> buffer) {
            if (client->sock->isOpen()) {
                PacketHeader* s_pkt_hdr = (PacketHeader*)&txStreamBuffer[0];
                uint8_t* s_pkt_data = txStreamBuffer + sizeof(PacketHeader);
                s_pkt_hdr->type = PACKET_TYPE_TRANSMIT_DATA;
                s_pkt_hdr->size = sizeof(PacketHeader) + buffer.size() * sizeof(dsp::complex_t);
                memcpy(s_pkt_data, buffer.data(), buffer.size() * sizeof(dsp::complex_t));
                client->sock->send(txStreamBuffer, s_pkt_hdr->size);
            }
        }

        unsigned char softwareGain = 0;
        unsigned char hardwareGain = 0;

        void setTransmitSoftwareGain(unsigned char gain) override {
            auto rv = json({});
            rv["transmitSoftwareGain"] = gain;
            sendTransmitAction(rv);
            softwareGain = gain;
        }
        void setTransmitHardwareGain(unsigned char gain) override {
            auto rv = json({});
            rv["transmitHardwareGain"] = gain;
            sendTransmitAction(rv);
            hardwareGain = gain;
        }
        unsigned char getTransmitHardwareGain() override {
            return hardwareGain;
        }
        void setTransmitFrequency(int freq) override {
            auto rv = json({});
            rv["transmitFrequency"] = freq;
            sendTransmitAction(rv);
        };

        void setPAEnabled(bool enabled) override {
            auto rv = json({});
            rv["paEnabled"] = enabled;
            sendTransmitAction(rv);
        }

        int getTXStatus() override {
            return txStatus;
        }

        float transmitPower = 0;
        float reflectedPower = 0;
        float swr = 0;
        float getTransmitPower() override {
            return transmitPower;
        }
        float getReflectedPower() override {
            return reflectedPower;
        }
        float getTransmitSWR() override {
            return swr;
        }

        int normalZone = 5;
        int redZone = 10;
        int getNormalZone() override {
            return normalZone;
        }
        int getRedZone() override {
            return redZone;
        }

        std::string transmitterName = "not set";
        std::string &getTransmitterName() override {
            return transmitterName;
        }

        void parseState(const std::string &state) {
            try {
                auto s = json::parse(state);
                normalZone = s["normalZone"];
                redZone = s["redZone"];
                reflectedPower = s["reflectedPower"];
                transmitPower = s["transmitPower"];
                swr = s["swr"];
                hardwareGain = s["hardwareGain"];
                transmitterName = s["transmitterName"];
            } catch (std::exception &e) {
                return;
            }
        }

        virtual ~RemoteTransmitter() {
//            if (txstream) {
//                txstream->stopReader();
//            }
        }



    };

    Client::Client(std::shared_ptr<net::Socket> sock, dsp::stream<dsp::complex_t>* out) {
        this->sock = sock;
        output = out;

        // Allocate buffers
        rbuffer = new uint8_t[SERVER_MAX_PACKET_SIZE];
        sbuffer = new uint8_t[SERVER_MAX_PACKET_SIZE];

        // Initialize headers
        r_pkt_hdr = (PacketHeader*)rbuffer;
        r_pkt_data = &rbuffer[sizeof(PacketHeader)];
        r_cmd_hdr = (CommandHeader*)r_pkt_data;
        r_cmd_data = &rbuffer[sizeof(PacketHeader) + sizeof(CommandHeader)];

        s_pkt_hdr = (PacketHeader*)sbuffer;
        s_pkt_data = &sbuffer[sizeof(PacketHeader)];
        s_cmd_hdr = (CommandHeader*)s_pkt_data;
        s_cmd_data = &sbuffer[sizeof(PacketHeader) + sizeof(CommandHeader)];

        // Initialize decompressor
        dctx = ZSTD_createDCtx();

        // Initialize DSP
        decompIn.setBufferSize(STREAM_BUFFER_SIZE*sizeof(dsp::complex_t) + 8);
        decompIn.clearWriteStop();
        decomp.init(&decompIn);
        prebufferer.init(&decomp.out);
        link.init(&prebufferer.out, output);
        prebufferer.start();
        decomp.start();
        link.start();

        // Start worker thread
        workerThread = std::thread(&Client::worker, this);

        // Ask for a UI
        int res = getUI();
        if (res < 0) {
            // Close client
            close();

            // Throw error
            switch (res) {
            case CONN_ERR_TIMEOUT:
                throw std::runtime_error("Timed out");
            case CONN_ERR_BUSY:
                throw std::runtime_error("Server busy");
            default:
                throw std::runtime_error("Unknown error");
            }
        }
    }

    Client::~Client() {
        close();
        ZSTD_freeDCtx(dctx);
        delete[] rbuffer;
        delete[] sbuffer;
    }

    void Client::showMenu() {
        std::string diffId = "";
        SmGui::DrawListElem diffValue;
        bool syncRequired = false;
        {
            std::lock_guard<std::mutex> lck(dlMtx);
            dl.draw(diffId, diffValue, syncRequired);
        }

        if (!diffId.empty()) {
            // Save ID
            SmGui::DrawListElem elemId;
            elemId.type = SmGui::DRAW_LIST_ELEM_TYPE_STRING;
            elemId.str = diffId;

            // Encore packet
            int size = 0;
            s_cmd_data[size++] = syncRequired;
            size += SmGui::DrawList::storeItem(elemId, &s_cmd_data[size], SERVER_MAX_PACKET_SIZE - size);
            size += SmGui::DrawList::storeItem(diffValue, &s_cmd_data[size], SERVER_MAX_PACKET_SIZE - size);

            // Send
            if (syncRequired) {
                flog::warn("Action requires resync");
                auto waiter = awaitCommandAck(COMMAND_UI_ACTION);
                sendCommand(COMMAND_UI_ACTION, size);
                if (waiter->await(PROTOCOL_TIMEOUT_MS)) {
                    std::lock_guard lck(dlMtx);
                    dl.load(r_cmd_data, r_pkt_hdr->size - sizeof(PacketHeader) - sizeof(CommandHeader));
                }
                else {
                    flog::error("Timeout out after asking for UI");
                }
                waiter->handled();
                flog::warn("Resync done");
            }
            else {
                flog::warn("Action does not require resync");
                sendCommand(COMMAND_UI_ACTION, size);
            }
        }
    }

    void Client::setFrequency(double freq) {
        if (!isOpen()) { return; }
        *(double*)s_cmd_data = freq;
        sendCommand(COMMAND_SET_FREQUENCY, sizeof(double));
        auto waiter = awaitCommandAck(COMMAND_SET_FREQUENCY);
        waiter->await(PROTOCOL_TIMEOUT_MS);
        waiter->handled();
    }

    double Client::getSampleRate() {
        return currentSampleRate;
    }

    void Client::setSampleType(dsp::compression::PCMType type) {
        if (!isOpen()) { return; }
        s_cmd_data[0] = type;
        sendCommand(COMMAND_SET_SAMPLE_TYPE, 1);
    }

    void Client::setCompression(bool enabled) {
        if (!isOpen()) { return; }
        s_cmd_data[0] = enabled;
        sendCommand(COMMAND_SET_COMPRESSION, 1);
    }

    void Client::start() {
        if (!isOpen()) { return; }
        prebufferer.setPrebufferMsec(rxPrebufferMsec);
        prebufferer.setSampleRate(currentSampleRate);
        prebufferer.clear();
        int32_t *sr = (int32_t *)&s_cmd_data[0];
        *sr = requestedSampleRate;
        sendCommand(COMMAND_SET_SAMPLERATE, sizeof(int32_t));
        sendCommand(COMMAND_START, 0);
        getUI();
    }

    void Client::stop() {
        if (!isOpen()) { return; }
        sendCommand(COMMAND_STOP, 0);
        getUI();
        prebufferer.clear();
    }

    void Client::close() {
        // Stop worker
        decompIn.stopWriter();
        if (sock) { sock->close(); }
        if (workerThread.joinable()) { workerThread.join(); }
        decompIn.clearWriteStop();

        // Stop DSP
        decomp.stop();
        prebufferer.stop();
        link.stop();
        if (sigpath::transmitter) {
            delete sigpath::transmitter;
            sigpath::transmitter = nullptr;
        }
    }

    bool Client::isOpen() {
        return sock && sock->isOpen();
    }

    long long lastReportedTime = 0;

    void updateStreamTime(Client *client) {
        long long currentTime = currentTimeMillis() - client->getBufferTimeDelay();
        if (currentTime > lastReportedTime) {
            sigpath::iqFrontEnd.setCurrentStreamTime(currentTime);
            lastReportedTime = currentTime;
        }

    }

    void Client::worker() {
        while (true) {
            // Receive header
            if (sock->recv(rbuffer, sizeof(PacketHeader), true) <= 0) {
                break;
            }

            // Receive remaining data
            if (sock->recv(&rbuffer[sizeof(PacketHeader)], r_pkt_hdr->size - sizeof(PacketHeader), true, PROTOCOL_TIMEOUT_MS) <= 0) {
                break;
            }

            // Increment data counter
            bytes += r_pkt_hdr->size;

            // Decode packet
            if (r_pkt_hdr->type == PACKET_TYPE_COMMAND) {
                // TODO: Move to command handler
                if (r_cmd_hdr->cmd == COMMAND_SET_SAMPLERATE && r_pkt_hdr->size == sizeof(PacketHeader) + sizeof(CommandHeader) + sizeof(double)) {
                    currentSampleRate = *(double*)r_cmd_data;
                    core::setInputSampleRate(currentSampleRate);
                    prebufferer.setSampleRate(currentSampleRate);
                } else if (r_cmd_hdr->cmd == COMMAND_SET_TRANSMITTER_SUPPORTED) {
                    if (!sigpath::transmitter) {
                        std::string str = std::string((char*)r_cmd_data, r_pkt_hdr->size - sizeof(PacketHeader) - sizeof(CommandHeader));
                        sigpath::transmitter = new RemoteTransmitter(this, str);
                    }
                } else if (r_cmd_hdr->cmd == COMMAND_SET_TRANSMITTER_NOT_SUPPORTED) {
                    if (sigpath::transmitter) {
                        delete sigpath::transmitter;
                        sigpath::transmitter = nullptr;
                    }
                }
                else if (r_cmd_hdr->cmd == COMMAND_DISCONNECT) {
                    flog::error("Asked to disconnect by the server");
                    serverBusy = true;

                    // Cancel waiters
                    std::vector<PacketWaiter*> toBeRemoved;
                    for (auto& [waiter, cmd] : commandAckWaiters) {
                        waiter->cancel();
                        toBeRemoved.push_back(waiter);
                    }

                    // Remove handled waiters
                    for (auto& waiter : toBeRemoved) {
                        commandAckWaiters.erase(waiter);
                        delete waiter;
                    }
                }
            }
            else if (r_pkt_hdr->type == PACKET_TYPE_COMMAND_ACK) {
                // Notify waiters
                std::vector<PacketWaiter*> toBeRemoved;
                for (auto& [waiter, cmd] : commandAckWaiters) {
                    if (cmd != r_cmd_hdr->cmd) { continue; }
                    waiter->notify();
                    toBeRemoved.push_back(waiter);
                }

                // Remove handled waiters
                for (auto& waiter : toBeRemoved) {
                    commandAckWaiters.erase(waiter);
                    delete waiter;
                }
            }
            else if (r_pkt_hdr->type == PACKET_TYPE_BASEBAND) {
                memcpy(decompIn.writeBuf, &rbuffer[sizeof(PacketHeader)], r_pkt_hdr->size - sizeof(PacketHeader));
                if (!decompIn.swap(r_pkt_hdr->size - sizeof(PacketHeader))) { break; }
                updateStreamTime(this);
            }
            else if (r_pkt_hdr->type == PACKET_TYPE_BASEBAND_COMPRESSED) {
                size_t outCount = ZSTD_decompressDCtx(dctx, decompIn.writeBuf, STREAM_BUFFER_SIZE*sizeof(dsp::complex_t)+8, r_pkt_data, r_pkt_hdr->size - sizeof(PacketHeader));
                if (outCount) {
                    if (!decompIn.swap(outCount)) { break; }
                };
                updateStreamTime(this);
            }
            else if (r_pkt_hdr->type == PACKET_TYPE_ERROR) {
                flog::error("SDR++ Server Error: {0}", rbuffer[sizeof(PacketHeader)]);
            }
            else {
                flog::error("Invalid packet type: {0}", r_pkt_hdr->type);
            }

        }
    }

    int Client::getUI() {
        if (!isOpen()) { return -1; }
        auto waiter = awaitCommandAck(COMMAND_GET_UI);
        sendCommand(COMMAND_GET_UI, 0);
        if (waiter->await(PROTOCOL_TIMEOUT_MS)) {
            std::lock_guard lck(dlMtx);
            dl.load(r_cmd_data, r_pkt_hdr->size - sizeof(PacketHeader) - sizeof(CommandHeader));
        }
        else {
            if (!serverBusy) { flog::error("Timeout out after asking for UI"); };
            waiter->handled();
            return serverBusy ? CONN_ERR_BUSY : CONN_ERR_TIMEOUT;
        }
        waiter->handled();
        return 0;
    }

    void Client::sendPacket(PacketType type, int len) {
        s_pkt_hdr->type = type;
        s_pkt_hdr->size = sizeof(PacketHeader) + len;
        sock->send(sbuffer, s_pkt_hdr->size);
    }

    void Client::sendCommand(Command cmd, int len) {
        s_cmd_hdr->cmd = cmd;
        sendPacket(PACKET_TYPE_COMMAND, sizeof(CommandHeader) + len);
    }

    void Client::sendCommandAck(Command cmd, int len) {
        s_cmd_hdr->cmd = cmd;
        sendPacket(PACKET_TYPE_COMMAND_ACK, sizeof(CommandHeader) + len);
    }

    PacketWaiter* Client::awaitCommandAck(Command cmd) {
        PacketWaiter* waiter = new PacketWaiter;
        commandAckWaiters[waiter] = cmd;
        return waiter;
    }

    void Client::dHandler(dsp::complex_t *data, int count, void *ctx) {
        Client* _this = (Client*)ctx;
        memcpy(_this->output->writeBuf, data, count * sizeof(dsp::complex_t));
        _this->output->swap(count);
    }

    std::shared_ptr<Client> connect(std::string host, uint16_t port, dsp::stream<dsp::complex_t>* out) {
        return std::make_shared<Client>(net::connect(host, port), out);
    }
}
