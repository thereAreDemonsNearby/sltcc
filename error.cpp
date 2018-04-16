#include "error.h"
#include <algorithm>
#include "token.h"



void ErrorLog::add(Error e)
{
    errors_.insert(e);
}

bool ErrorLog::KeyCompare::operator()(const Error& t1, const Error& t2) const
{
    return t1.token()->context().pos < t2.token()->context().pos;
}
