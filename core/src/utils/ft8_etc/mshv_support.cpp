

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
