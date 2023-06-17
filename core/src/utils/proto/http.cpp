#include "http.h"
#include <inttypes.h>
#include <regex>
#include <utils/flog.h>

namespace net::http {

    std::string userAgent = "sdr++brown";

    void setUserAgent(const std::string& agent) {
        userAgent = agent;
    }

    ParsedUrl parseUrl(const std::string& url) {
        std::regex urlRegex(R"(^(https?|ftp):\/\/([^\/:]+)(?::(\d+))?(\/[^?]*)(?:\?(.*))?$)");
        std::smatch urlMatch;

        if (std::regex_match(url, urlMatch, urlRegex)) {
            ParsedUrl parsedUrl;
            parsedUrl.protocol = urlMatch[1].str();
            parsedUrl.host = urlMatch[2].str();
            parsedUrl.port = urlMatch[3].str().empty() ? 0 : std::stoi(urlMatch[3].str());
            parsedUrl.path = urlMatch[4].str();
            parsedUrl.query = urlMatch[5].str();
            if (parsedUrl.port == 0 && parsedUrl.protocol == "http") parsedUrl.port = 80;
            if (parsedUrl.port == 0 && parsedUrl.protocol == "https") parsedUrl.port = 443;
            return parsedUrl;
        } else {
            throw std::runtime_error("Invalid URL");
        }

    }

    std::string receiveResponse(net::http::Client &http, net::http::ResponseHeader &rshdr, const std::shared_ptr<Socket> &sock) {
        bool chunked = false;
        if (rshdr.hasField("Transfer-Encoding")) {
            chunked = rshdr.getField("Transfer-Encoding").find("chunked") != std::string::npos;
        }
        std::string s(3000000, ' ');
        if (chunked) {
            s.resize(0);
            while(true) {
                net::http::ChunkHeader chdr;
                int err = http.recvChunkHeader(chdr, 25000);
                if (err < 0) {
                    throw std::runtime_error("Couldn't read chunk header");
                }

                size_t clen = chdr.getLength();
                if (!clen) {
                    break;  // end of stream
                }

                // Read JSON
                std::string partial(clen+1, ' ');
                auto rd = sock->recv((uint8_t *) partial.data(), clen, true, 25000);
                if (rd != clen) {
                    throw std::runtime_error("Couldn't read chunk, short read");
                }
                s.append(partial.data(), clen);
                auto rd2 = sock->recv((uint8_t *) partial.data(), 2, true, 25000); // \r\n after block
                if (rd2 != 2) {
                    throw std::runtime_error("Couldn't read chunk, short read");
                }
            }
            return std::string(s.data(), s.length());
        } else {
            int rd = sock->recv((uint8_t *) s.data(), s.size());
            if (rd >= 0) {
                s.data()[rd] = 0;
            }
            sock->close();
            return std::string(s.data());
        }

    }
    std::string get(const std::string &url) {
        auto parsed = parseUrl(url);
        if (parsed.protocol != "http") {
            throw std::runtime_error("Only HTTP is supported");
        }
        auto sock = net::connect(parsed.host, parsed.port);
        auto http = net::http::Client(sock);
        net::http::RequestHeader rqhdr(net::http::METHOD_GET, parsed.path+"?"+parsed.query, parsed.host);
        rqhdr.setField("User-Agent", userAgent);
        http.sendRequestHeader(rqhdr);
        net::http::ResponseHeader rshdr;
        http.recvResponseHeader(rshdr, 5000);
        if (rshdr.getStatusCode() != net::http::STATUS_CODE_OK) {
            throw std::runtime_error("HTTP status code: "+std::to_string(rshdr.getStatusCode()) + " : " + rshdr.getStatusString());
        }
        return receiveResponse(http, rshdr, sock);
    }

    std::string post(const std::string &url, const std::string &formData) {
        auto parsed = parseUrl(url);
        if (parsed.protocol != "http") {
            throw std::runtime_error("Only HTTP is supported");
        }
        auto sock = net::connect(parsed.host, parsed.port);
        auto http = net::http::Client(sock);
        net::http::RequestHeader rqhdr(net::http::METHOD_POST, parsed.path+"?"+parsed.query, parsed.host);

        char lenBuf[16];
        sprintf(lenBuf, "%zu", formData.size());
        rqhdr.setField("Content-Length", lenBuf);
        rqhdr.setField("Content-Type", "application/x-www-form-urlencoded");
//        rqhdr.setField("Origin", "https://www.wsprnet.org");
        rqhdr.setField("User-Agent", userAgent);
        http.sendRequestHeader(rqhdr);
        sock->sendstr(formData.c_str());
        net::http::ResponseHeader rshdr;
        http.recvResponseHeader(rshdr, 5000);
        if (rshdr.getStatusCode() == net::http::STATUS_CODE_FOUND) {
            if (rshdr.hasField("Location")) {
                std::string loc = rshdr.getField("Location");
                if (loc.find("https://") == 0) {
                    loc = "http://" + loc.substr(8);
                }
                return get(loc);
            }
        }
        if (rshdr.getStatusCode() != net::http::STATUS_CODE_OK) {
            throw std::runtime_error("HTTP status code: "+std::to_string(rshdr.getStatusCode()) + " : " + rshdr.getStatusString());
        }
        const std::string result = receiveResponse(http, rshdr, sock);
        return result;
    }

    std::string MessageHeader::serialize() {
        std::string data;

        // Add start line
        data += serializeStartLine() + "\r\n";

        // Add fields
        for (const auto& [key, value] : fields) {
            data += key + ": " + value + "\r\n";
        }

        // Add terminator
        data += "\r\n";

        return data;
    }

    void MessageHeader::deserialize(const std::string& data) {
        // Clear existing fields
        fields.clear();

        // Parse first line
        std::string line;
        int offset = readLine(data, line);
        deserializeStartLine(line);

        // Parse fields
        while (offset < data.size()) {
            // Read line
            offset = readLine(data, line, offset);

            // If empty line, the header is done
            if (line.empty()) { break; }

            // Read until first ':' for the key
            int klen = 0;
            for (; klen < line.size(); klen++) {
                if (line[klen] == ':') { break; }
            }
            
            // Find offset of value
            int voff = klen + 1;
            for (; voff < line.size(); voff++) {
                if (line[voff] != ' ' && line[voff] != '\t') { break; }
            }

            // Save field
            fields[line.substr(0, klen)] = line.substr(voff);
        }
    }

    std::map<std::string, std::string>& MessageHeader::getFields() {
        return fields;
    }

    bool MessageHeader::hasField(const std::string& name) {
        return fields.find(name) != fields.end();
    }

    std::string MessageHeader::getField(const std::string name) {
        // TODO: Check if exists
        return fields[name];
    }

    void MessageHeader::setField(const std::string& name, const std::string& value) {
        fields[name] = value;
    }

    void MessageHeader::clearField(const std::string& name) {
        // TODO: Check if exists (but maybe no error?)
        fields.erase(name);
    }

    int MessageHeader::readLine(const std::string& str, std::string& line, int start) {
        // Get line length
        int len = 0;
        bool cr = false;
        for (int i = start; i < str.size(); i++) {
            if (str[i] == '\n') {
                if (len && str[i-1] == '\r') { cr = true; }
                break;
            }
            len++;
        }

        // Copy line
        line = str.substr(start, len - (cr ? 1:0));
        return start + len + 1;
    }

    RequestHeader::RequestHeader(Method method, std::string uri, std::string host) {
        this->method = method;
        this->uri = uri;
        setField("Host", host);
    }

    RequestHeader::RequestHeader(const std::string& data) {
        deserialize(data);
    }

    Method RequestHeader::getMethod() {
        return method;
    }

    void RequestHeader::setMethod(Method method) {
        this->method = method;
    }

    std::string RequestHeader::getURI() {
        return uri;
    }

    void RequestHeader::setURI(const std::string& uri) {
        this->uri = uri;
    }

    void RequestHeader::deserializeStartLine(const std::string& data) {
        // TODO
    }

    std::string RequestHeader::serializeStartLine() {
        // TODO: Allow to specify version
        return MethodStrings[method] + " " + uri + " HTTP/1.1";
    }

    ResponseHeader::ResponseHeader(StatusCode statusCode) {
        this->statusCode = statusCode;
        if (StatusCodeStrings.find(statusCode) != StatusCodeStrings.end()) {
            this->statusString = StatusCodeStrings[statusCode];
        }
        else {
            this->statusString = "UNKNOWN";
        }
    }

    ResponseHeader::ResponseHeader(StatusCode statusCode, const std::string& statusString) {
        this->statusCode = statusCode;
        this->statusString = statusString;
    }

    ResponseHeader::ResponseHeader(const std::string& data) {
        deserialize(data);
    }

    StatusCode ResponseHeader::getStatusCode() {
        return statusCode;
    }

    void ResponseHeader::setStatusCode(StatusCode statusCode) {
        this->statusCode = statusCode;
    }

    std::string ResponseHeader::getStatusString() {
        return statusString;
    }

    void ResponseHeader::setStatusString(const std::string& statusString) {
        this->statusString = statusString;
    }

    void ResponseHeader::deserializeStartLine(const std::string& data) {
        // Parse version
        int offset = 0;
        for (; offset < data.size(); offset++) {
            if (data[offset] == ' ') { break; }
        }
        // TODO: Error if null length
        // TODO: Parse version

        // Skip spaces
        for (; offset < data.size(); offset++) {
            if (data[offset] != ' ' && data[offset] != '\t') { break; }
        }
        
        // Parse status code
        int codeOffset = offset;
        for (; offset < data.size(); offset++) {
            if (data[offset] == ' ') { break; }
        }
        // TODO: Error if null length
        statusCode = (StatusCode)std::stoi(data.substr(codeOffset, codeOffset - offset));
    
        // Skip spaces
        for (; offset < data.size(); offset++) {
            if (data[offset] != ' ' && data[offset] != '\t') { break; }
        }
        
        // Parse status string
        int stringOffset = offset;
        for (; offset < data.size(); offset++) {
            if (data[offset] == ' ') { break; }
        }
        // TODO: Error if null length (maybe?)
        statusString = data.substr(stringOffset, stringOffset - offset);
    }

    std::string ResponseHeader::serializeStartLine() {
        char buf[1024];
        sprintf(buf, "%d %s", (int)statusCode, statusString.c_str());
        return buf;
    }

    ChunkHeader::ChunkHeader(size_t length) {
        this->length = length;
    }

    ChunkHeader::ChunkHeader(const std::string& data) {
        deserialize(data);
    }

    std::string ChunkHeader::serialize() {
        char buf[64];
        sprintf(buf, "%" PRIX64 "\r\n", length);
        return buf;
    }

    void ChunkHeader::deserialize(const std::string& data) {
        // Parse length
        int offset = 0;
        for (; offset < data.size(); offset++) {
            if (data[offset] == ' ') { break; }
        }
        // TODO: Error if null length
        length = (StatusCode)std::stoull(data.substr(0, offset), nullptr, 16);

        // TODO: Parse rest
    }

    size_t ChunkHeader::getLength() {
        return length;
    }

    void ChunkHeader::setLength(size_t length) {
        this->length = length;
    }

    Client::Client(std::shared_ptr<Socket> sock) {
        this->sock = sock;
    }

    int Client::sendRequestHeader(RequestHeader& req) {
        return sock->sendstr(req.serialize());
    }

    int Client::recvRequestHeader(RequestHeader& req, int timeout) {
        // Non-blocking mode not alloowed
        if (!timeout) { return -1; }

        // Read response
        std::string respData;
        int err = recvHeader(respData, timeout);
        if (err) { return err; }

        // Deserialize
        req.deserialize(respData);
        return 0;
    }

    int Client::sendResponseHeader(ResponseHeader& resp) {
        return sock->sendstr(resp.serialize());
    }

    int Client::recvResponseHeader(ResponseHeader& resp, int timeout) {
        // Non-blocking mode not alloowed
        if (!timeout) { return -1; }

        // Read response
        std::string respData;
        int err = recvHeader(respData, timeout);
        if (err) { return err; }

        // Deserialize
        resp.deserialize(respData);
        return 0;
    }

    int Client::sendChunkHeader(ChunkHeader& chdr) {
        return sock->sendstr(chdr.serialize());
    }

    int Client::recvChunkHeader(ChunkHeader& chdr, int timeout) {
        std::string respData;
        int err = sock->recvline(respData, 0, timeout);
        if (err <= 0) { return err; }
        if (respData[respData.size()-1] == '\r') {
            respData.pop_back();
        }
        chdr.deserialize(respData);
        return 0;
    }

    int Client::recvHeader(std::string& data, int timeout) {
        while (sock->isOpen()) {
            std::string line;
            int ret = sock->recvline(line);
            if (line == "\r") { break; }
            if (ret <= 0) { return ret; }
            data += line + "\n";
        }
        return 0;
    }
}