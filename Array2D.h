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

    Array2D(size_t nrows, size_t ncols)
        : data_(new T[nrows * ncols]), nrows_(nrows), ncols_(ncols)
    {
    }

    Array2D(size_t nrows, size_t ncols, const T& init)
            : data_(new T[nrows * ncols]), nrows_(nrows), ncols_(ncols)
    {
        for (size_t i = 0; i < nrows_ * ncols_; ++i) {
            data_[i] = init;
        }
    }

    Array2D(const Array2D& rhs)
        : data_(new T[rhs.nrows_ * rhs.ncols_]), nrows_(rhs.nrows_), ncols_(rhs.ncols_)
    {
        for (size_t i = 0; i < nrows_ * ncols_; ++i) {
            data_[i] = rhs.data_[i];
        }
    }

    Array2D& operator=(const Array2D& rhs)
    {
        if (nrows_ * ncols_ != rhs.nrows_ * rhs.ncols_) {
            delete[] data_;
            data_ = new T[rhs.nrows_ * rhs.ncols_];
        }
        nrows_ = rhs.nrows_;
        ncols_ = rhs.ncols_;
        for (size_t i = 0; i < nrows_ * ncols_; ++i) {
            data_[i] = rhs.data_[i];
        }
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
};

#endif //ARRAY2D_H
