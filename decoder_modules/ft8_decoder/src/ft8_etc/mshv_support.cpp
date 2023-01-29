

#include "mshv_support.h"

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