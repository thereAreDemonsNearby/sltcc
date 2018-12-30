#ifndef PLATFORM_H__
#define PLATFORM_H__

#include <cstddef>

// measurement: byte
constexpr std::size_t WORDSIZE = 2;
constexpr std::size_t CHARSIZE = 1;
constexpr std::size_t SHORTSIZE = 2;
constexpr std::size_t PTRSIZE = 4;
constexpr std::size_t INTSIZE = 4;
constexpr std::size_t SINGLESIZE = 4;
constexpr std::size_t DOUBLESIZE = 8;

constexpr int PARAMREGNUM = 4;

#endif //PLATFORM_H__
