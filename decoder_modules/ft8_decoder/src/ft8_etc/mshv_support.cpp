

#include "mshv_support.h"
#include "pfx_sfx.h"
#include "decoderpom.h"
#include <functional>

char mshv_chars[256];
char mshv_chars_m1[256];

QStringList& QStringList::operator<<(const QString& s) {
    list->emplace_back(*s.str);
    return *this;
}
QString QStringList::at(int i) const {
    return list->at(i);
}
QString QStringList::operator[](int index) const {
    return at(index);
}
void QStringList::replace(int i, QString qString) {
    (*list)[i] = *qString.str;
}

bool QString::contains(const QRegExp&) {
    abort();
    return false;
}
QString::QString(const QString& other) {
    initWithConstChar(other.str->data(), other.str->length());
    verify();
}
QString::QString(QString&& other) noexcept {
    str = std::move(other.str);
    verify();
}
QString& QString::operator=(const QString& other) {
    if (&other == this) {
        return *this;
    }
    initWithConstChar(other.str->data(), other.str->length());
    verify();
    return *this;
}
void QString::verify() {
#if defined(__has_feature)
#  if __has_feature(memory_sanitizer)
    const char* x = "Z";
    for (int z = 0; z < str->length(); z++) {
        if (memcmp(&(*str)[z], x, 1) == 0) {
            printf("");
        }
    }
#  endif
#endif
}

void QString::initWithConstChar(const char* init, int len) {
    if (len == 1) {
        // So you're looking at this and wondering why?? Because -fsanitize=memory will claim that the data is uninitialized if you don't do this tweak.
        // I tried other things, yes.
        char cpy[3];
        int z = (int)*init;
        memset(cpy, z - 1, 3);
        cpy[0]++;
        str = std::make_shared<std::string>(cpy, 2);
        str->resize(1);
    }
    else {
        str = std::make_shared<std::string>(init, len);
    }
}
QString::QString(const char* init) {
    initWithConstChar(init, strlen(init));
    verify();
}
QString::QString(char init) {
    initWithConstChar("", 0);
    append(init);
    mutated();
    verify();
}
QString& QString::operator=(const char* ptr) {
    initWithConstChar(ptr, strlen(ptr));
    verify();
    return *this;
}
QString::QString(const std::string& init) {
    initWithConstChar(init.data(), init.length());
    verify();
}
void QString::mutated() {
    if (str->length() == 1) {
        auto os = str;
        initWithConstChar(os->data(), os->length());
    }
    for (int i = 0; i < str->length(); i++) {
        (*str)[i] = mshv_chars[(unsigned int)(*str)[i]];
    }
}
void QString::append(const char* s) {
    char buf[1000];
    if (str->size() + strlen(s) > sizeof(buf) - 1) {
        abort();
    }
    sprintf(buf, "%s%s", str->c_str(), s);
    str = std::make_shared<std::string>(buf);

    mutated();
    verify();
}
void QString::append(const QString& s) {
    str = std::make_shared<std::string>(*str + *s.str);
    mutated();
    verify();
}
void QString::insert(int pos, const QString& s) {
    replaceWith(str->substr(0, pos) + *s.str + str->substr(pos));
    mutated();
    verify();
}
void QString::replaceWith(const std::string& s) {
    str = std::make_shared<std::string>(s);
    mutated();
    verify();
}
void QString::prepend(const QString& s) {
    replaceWith(*s.str + *str);
    mutated();
    verify();
}
void QString::prepend(const char* s) {
    char buf[1000];
    if (str->size() + strlen(s) > sizeof(buf) - 1) {
        abort();
    }
    sprintf(buf, "%s%s", s, str->c_str());
    str = std::make_shared<std::string>(buf);
    mutated();
    verify();
}
void QString::append(char s) {
    char buf[1000];
    if (str->size() + 1 > sizeof(buf) - 1) {
        abort();
    }
    sprintf(buf, "%s%c", str->c_str(), s);
    //    str->resize(str->length() + 1);
    //    str->data()[str->length()-1] = mshv_chars_m1[(unsigned char)s];
    //    str->data()[str->length()-1]++;
    str = std::make_shared<std::string>(buf);
    mutated();
    verify();
}
void QString::prepend(char s) {
    replaceWith(s + *str);
    mutated();
    verify();
}
void QString::replace(int start, int len, QString ns) {
    str->replace(start, start + len, ns.str->c_str());
    mutated();
    verify();
}
void QString::replace(const char* string, const char* string1) {
    auto pos = str->find(string);
    if (pos == std::string::npos) {
        mutated();
        verify();
        return;
    }
    str->replace(pos, strlen(string), string1);
    return replace(string, string1);
}

static void splitString(const std::string & str, char sep, const std::function<void(const std::string&)> &callback) {
    const char *c = str.data();
    size_t limit = str.length();
    if (limit == 0) {
        return;
    }
    long long start = 0;
    for (long long i = 0; i < limit; i++) {
        if (i == limit - 1) {
            if (c[i] == sep) {
                callback(std::string(c + start, i - start));
            } else {
                callback(std::string(c + start, i - start + 1));
            }
            break;
        }
        if (c[i] == sep) {
            callback(std::string(c + start, i - start));
            start = i + 1;
        }
    }
}


QStringList QString::split(const char* separ) const {
    if (strlen(separ) != 1) {
        abort();
    }
    QStringList rv;
    auto ns = *str;
    while(!ns.empty() && ns[0] == separ[0]) {
        ns = ns.substr(1);
    }
    while(!ns.empty() && ns[ns.length()-1] == separ[0]) {
        ns = ns.substr(0, ns.length()-1);
    }
    splitString(ns, separ[0], [&](const std::string& s) {
        rv.list->emplace_back(s);
    });
    return rv;
}
bool QString::operator!=(const char* x) const {
    if (strlen(x) != str->length()) {
        return true;
    }
    if (str->length() == 1) {
        return (*str)[0] != x[0];
    }
    if (0 == memcmp(str->c_str(), x, str->length())) {
        return false;
    }
    return true;
}
bool QString::operator!=(const QString& x) const {
    return !(*this == x);
}
bool QString::operator==(const QString& x) const {
    if (x.str->length() != str->length()) {
        return false;
    }
    return *str == *x.str;
}
bool QString::operator<=(const char* x) const {
    return *str >= x;
}
bool QString::operator>=(const char* x) const {
    return *str >= x;
}
bool QString::operator==(const char* x) const {
    return !(*this != x);
}
void QString::remove(const char* x) {
    if (strlen(x) == 0) {
        return;
    }
    this->replace(x, "");
    mutated();
    verify();
}
QString QString::operator+(const char s) {
    return (*this->str) + s;
}
QString QString::operator+(const char* s) {
    QString s2(*str);
    s2.append(s);
    return s2;
}


/*
// stubs for using float fftw lib as double
extern "C" {
void* fftw_malloc(size_t n) {
    return nullptr;
}
void fftw_free(void *ptr) {
    return;
}
void fftw_execute(const fftw_plan plan) {

}

void fftw_destroy_plan(fftw_plan plan) {

}

fftw_plan fftw_plan_dft_1d(int n, fftw_complex *in, fftw_complex *out,
                           int sign, unsigned flags) {
    return nullptr;
}

fftw_plan fftw_plan_dft_c2r_1d(int n0,
                               fftw_complex *in, double *out,
                               unsigned flags) {
    return nullptr;
}

fftw_plan fftw_plan_dft_r2c_1d(int n0,
                               double *in, fftw_complex *out,
                               unsigned flags) {
    return nullptr;
}


}

 */


void mshv_init() {
    static int initialized = 0;
    if (initialized++) return;
    for (int i = 0; i < 256; i++) {
        mshv_chars[i] = i;
        mshv_chars_m1[i] = i;
        mshv_chars_m1[i]--;
    }
    init_pfx_sfx();
    initDecoderPom();
}