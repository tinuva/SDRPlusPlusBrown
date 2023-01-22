
#pragma once

#include <vector>
#include <string>

#include <complex.h>
//#define complex		_Complex
#include <fftw3.h>
#include <memory>
#include <cstring>

#ifdef I
#undef I
#endif


inline int toInt(const std::string &s, bool &ok, int radix) {
    int rv;
    if (s.size() == 0) {
        ok = false;
        return 0;
    }
    if (radix == 16) {
        ok = 1 == sscanf(s.c_str(), "%x", &rv);
    } else if (radix == 10) {
        ok = 1 == sscanf(s.c_str(), "%d", &rv);
    } else {
        abort();
    }
    return rv;
}


struct QString;

struct QStringList {
    std::shared_ptr<std::vector<std::string>> list;

    QStringList() {
        list = std::make_shared<std::vector<std::string>>();
    }

    QStringList &operator << (const QString &s);

    int count() {
        return (int)list->size();
    }

    void clear() {
        list->clear();
    }



    [[nodiscard]] QString operator[](int index) const;
    [[nodiscard]] QString at(int i) const;

    void replace(int i, QString qString);
};


struct QRegExp;


struct QChar {
    char c;

    QChar() {
        c = 0;
    }

    QChar(char c) {
        this->c = c;
    }

    operator char() const {
        return c;
    }

    bool isLetter() {
        return ::isalpha(c);
    }
    bool isDigit() {
        return ::isdigit(c);
    }
    char toLatin1() {
        return c;
    }
};

struct QString {
    std::shared_ptr<std::string> str;

    QString() {
        str = std::make_shared<std::string>();
    }

    QString(const char *init) {
        str = std::make_shared<std::string>(init);
    }

    QString(char init) {
        char buf[2] = {0, 0};
        buf[0] = init;
        str = std::make_shared<std::string>(buf);
    }

    QString(const std::string &init) {
        str = std::make_shared<std::string>(init);
    }

    int count() const {
        return str->length();
    }

    int length() {
        return str->length();
    }

    QChar at(int index) const {
        return str->at(index);
    }
    bool contains(const QRegExp &);
    bool contains(const char* s) {
        return str->find(s) != std::string::npos;
    }

    auto begin() {
        return str->begin();
    }
    auto end() {
        return str->end();
    }

    int indexOf(const QString &x, int start = 0) const {
        auto rv = str->find(*x.str, start);
        if (rv == std::string::npos) {
            return -1;
        }
        return rv;
    }

    int indexOf(char x, int start = 0) const {
        auto rv = str->find(x, start);
        if (rv == std::string::npos) {
            return -1;
        }
        return rv;
    }

    int indexOf(const char *x, int start = 0) const {
        auto rv = str->find(x, start);
        if (rv == std::string::npos) {
            return -1;
        }
        return rv;
    }

    QString operator+(const char *s) {
        return (*this->str) + s;
    }

    QString operator+(const char s) {
        return (*this->str) + s;
    }

    [[nodiscard]] QString arg(int) const {
        abort(); //
        return *this;
    }

    [[nodiscard]] QString arg(int, int, int, QChar = ' ') const {
        abort(); //
        return *this;
    }

    void remove(const char *x) {
        if (strlen(x) == 0) {
            return;
        }
        return this->replace(x, "");
    }

    bool operator ==(const char *x) const {
        return *str == x;
    }

    bool operator >=(const char *x) const {
        return *str >= x;
    }

    bool operator <=(const char *x) const {
        return *str >= x;
    }

    bool operator ==(const QString &x) const {
        return *str == *x.str;
    }

    bool operator !=(const QString &x) const {
        return !(*this == x);
    }

    QString operator +(const QString &other) const {
        return *str + *other.str;
    }

    [[nodiscard]] QString rightJustified(int w, QChar filler) const {
        unsigned long myLen = str->length();
        if (myLen >= w) {
            return *this;
        }
        std::string rv(w - myLen, filler.c);
        rv.append(*str);
        return rv;
        
    }

    [[nodiscard]] QString leftJustified(int w, QChar filler) const {
        unsigned long myLen = str->length();
        if (myLen >= w) {
            return *this;
        }
        std::string padding(w - myLen, filler.c);
        return *str + padding;
    }

    [[nodiscard]] QString trimmed() const {
        if (str->empty()) {
            return *this;
        }
        if (isspace((*str)[0]) || isspace((*str)[str->length() - 1])) {
            auto ns = *str;
            while(!ns.empty() && std::isspace(ns[0])) {
                ns = ns.substr(1);
            }
            while(!ns.empty() && std::isspace(ns[ns.length()-1])) {
                ns.resize(ns.length()-1);
            }
            return ns;
        } else {
            return *this;
        }
    }
    [[nodiscard]] QString toUpper() const {
        auto nv = *str;
        for(auto &q : nv) {
            q = (char)std::toupper(q);
        }
        return nv;
    }

    void append(const char *s) {
        str = std::make_shared<std::string>(*str + s);
    }

    void append(const QString &s) {
        str = std::make_shared<std::string>(*str + *s.str);
    }

    int lastIndexOf(const QString &s, int trimFirst) const {
        return this->mid(0, trimFirst).lastIndexOf(s);
    }

    void insert(int pos, const QString &s) {
        replaceWith(str->substr(0, pos) + *s.str + str->substr(pos));
    }

    bool isEmpty() const {
        return str->empty();
    }

    int lastIndexOf(const QString &s) const {
        auto rv = str->rfind(*s.str);
        if (rv == std::string::npos) {
            return -1;
        }
        return rv;
    }

    void replaceWith(const std::string &s) {
        str = std::make_shared<std::string>(s);
    }

    void prepend(const QString &s) {
        replaceWith(*s.str + *str);
    }

    void prepend(const char *s) {
        replaceWith(s + *str);
    }

    void append(char s) {
        replaceWith(*str + s);
    }

    void prepend(char s) {
        replaceWith(s + *str);
    }

    QChar operator[](int index) const {
        return str->at(index);
    }

    int toInt(bool *ok = nullptr, int radix = 10) const {
        bool ok0;
        if (!ok) {
            ok = &ok0;
        }
        return ::toInt(*str, *ok, radix);
    }


    [[nodiscard]] QString mid(int i, int len) const {
        if (i+len > str->size()) {
            len = str->size()-i;
        }
        if (len < 0) {
            len = 0;
        }
        if (len == 0) {
            return QString("");
        }
        return str->substr(i, len);
    }

    [[nodiscard]] QString midRef(int i, int len) const {
        return this->mid(i, len);
    }

    QStringList split(const char* separ) const {
        if (strlen(separ) != 1) {
            abort();
        }
        QStringList rv;

        std::istringstream f(*str);
        std::string t;
        while (getline(f, t, separ[0])) {
            rv.list->emplace_back(t);
        }
        return rv;
    }

    void replace(int start, int len, QString ns) {
        str->replace(start, start + len, ns.str->c_str());
    }

    void replace(const char* string, const char* string1) {
        auto pos = str->find(string);
        if (pos == std::string::npos) {
            return;
        }
        str->replace(pos, strlen(string), string1);
        return replace(string, string1);
    }
    bool startsWith(const char* string) {
        return str->find(string) == 0;
    }
};

struct QRegExp {
    QRegExp(const char *init) {
        abort();
    }
    static const int CaseInsensitive = 0;
    int CaseSensitive = 1;
    int caseSensitivity = 1;
    void setCaseSensitivity(int i) {    // 1 = sensitive, 0 = insensitive
        this->caseSensitivity = CaseInsensitive;
    }

    bool exactMatch(const QString &s) {
        abort();
    }
};


inline QString operator + (const char *a, const QString &s) {
    return std::string(a) + *s.str;
}

inline std::ostream &operator << (std::ostream &os, const QString &s) {
    os << *s.str;
    return os;
}

struct fftw_complexW {
    fftw_complex c;
    constexpr fftw_complexW() : c() {
        c[0] = 0;
        c[1] = 0;
    }
};

constexpr fftw_complexW mk_fftw_complex(double re, double im) {
    fftw_complexW w;
    w.c[0] = re;
    w.c[1] = im;
    return w;
}

constexpr auto mk_complex(double re, double im) {
    return std::complex<double>(re, im);
}

constexpr auto complex_zero = mk_complex(0, 0);
constexpr auto complex_i = mk_complex(0, 1);

inline double creal(std::complex<double> c) {
    return c.real();
}

inline double cimag(std::complex<double> c) {
    return c.imag();
}

inline double cabs(std::complex<double> c) {
    return abs(c);
}

