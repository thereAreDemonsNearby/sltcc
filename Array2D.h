#ifndef ARRAY2D_H
#define ARRAY2D_H

#include <vector>

template<typename T>
class Array2D
{
private:
    T* data_;
    size_t nrows_;
    size_t ncols_;
public:
    Array2D() = default;

    Array2D(size_t nrows, size_t ncols)
        : data_(new T[nrows * ncols]), nrows_(nrows), ncols_(ncols)
    {
    }

    ~Array2D() { delete [] data_; }

    T* operator[](size_t row)
    {
        return data_ + ncols_ * row;
    }

    T const* operator[](size_t row) const
    {
        return data_ + ncols_ * row;
    }

    void fill(const T& v)
    {
        for (auto& e : data_) { e = v; }
    }
};

#endif //ARRAY2D_H
