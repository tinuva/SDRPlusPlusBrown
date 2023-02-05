

#include "mshv_support.h"
#include "pfx_sfx.h"

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
    const char *x = "Z";
    for(int z=0; z<str->length(); z++) {
        if (memcmp(&(*str)[z], x, 1) == 0) {
            printf("");
        }
    }

}
void QString::initWithConstChar(const char* init, int len) {
    if (len == 1) {
        // So you're looking at this and wondering why?? Because -fsanitize=memory will claim that the data is uninitialized if you don't do this tweak.
        // I tried other things, yes.
        char cpy[3];
        int z = (int)*init;
        memset(cpy, z-1, 3);
        cpy[0]++;
        str = std::make_shared<std::string>(cpy, 2);
        str->resize(1);
    } else {
        str = std::make_shared<std::string>(init, len);
    }

}
QString::QString(const char* init) {
    initWithConstChar(init, strlen(init));
    verify();
}
QString::QString(char init) {
    char buf[2] = {0, 0};
    buf[0] = init;
    str = std::make_shared<std::string>(buf);
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
}
void QString::append(const char* s) {
    if (strlen(s) == 1) {
        append((*s)+1); // char
    } else {
        append(QString(s));
    }
    if (strlen(s) == 1) {
        (*str)[str->length()-1]--;
    }
//    int oldlen = str->size();       // <- this is oll to disable optimizer
//    if (strlen(s) == 1) {
//        (*str)[str->length()-1]++;
//    }
//    if (str->length() - oldlen == 1) {
//    }
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
    replaceWith(s + *str);
    mutated();
    verify();
}
void QString::append(char s) {
    replaceWith(*str + (char)(s+1));
    (*str)[str->length()-1]--;
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
    init_pfx_sfx();
}