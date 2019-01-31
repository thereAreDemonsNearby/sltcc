#ifndef UTILS_H
#define UTILS_H

#include <type_traits>

template<typename EnumType>
auto enumToInt(EnumType e)
{
    return static_cast<std::underlying_type_t<EnumType>>(e);
}

template<typename EnumType>
bool enumBetween(EnumType left, EnumType elm, EnumType right)
{
    return enumToInt(elm) >= enumToInt(left) && enumToInt(elm) <= enumToInt(right);
}


#endif //UTILS_H
