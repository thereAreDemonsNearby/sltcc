#ifndef UTILS_H
#define UTILS_H

#include <type_traits>

template<typename EnumType>
constexpr auto enumToInt(EnumType e)
{
    return static_cast<std::underlying_type_t<EnumType>>(e);
}

template<typename EnumType>
constexpr bool enumBetween(EnumType left, EnumType elm, EnumType right)
{
    return enumToInt(elm) >= enumToInt(left) && enumToInt(elm) <= enumToInt(right);
}

template<typename... Fs>
struct overload : Fs...
{
    using Fs::operator()...;
    explicit overload(Fs&&... fs) : Fs(std::forward<Fs>(fs))... {}
};

template<typename... Fs>
overload(Fs...) -> overload<Fs...>;

inline size_t alignUp(size_t v, size_t a) {
    // TODO performance can be improved
    while (v % a != 0)
        ++v;
    return v;
}

template<typename T, typename... Ts>
constexpr bool equalsAny(T const& e, Ts const&... cases)
{
    return ((e == cases) || ...);
}


#endif //UTILS_H
