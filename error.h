#ifndef SHITCC_ERROR_H
#define SHITCC_ERROR_H

#include <set>
#include <string>

class Token;
class Error;

class ErrorLog
{
public:
    struct KeyCompare
    {
        bool operator()(const Error& t1, const Error& t2) const;
    };

    using container = std::set<Error, KeyCompare>;
    container::iterator begin() { return errors_.begin(); }
    container::iterator end() { return errors_.end(); }
    size_t count() const { return errors_.size(); }
    void add(Error);
private:
    std::set<Error, KeyCompare> errors_;
};

/** throwable */
class Error
{
public:
    Error(const Token* t, std::string w) : tok_(t), what_(std::move(w)) {}
    const std::string& what() const { return what_; }
    Token const* token() const { return tok_; }
    void what(std::string e) { what_ = std::move(e); }
    int lineNumber() const;
private:
    Token const* tok_;
    std::string what_;
};

#endif //SHITCC_ERROR_H
