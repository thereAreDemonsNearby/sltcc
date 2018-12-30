#ifndef SCOPEDSYMBOLTABLE_H_
#define SCOPEDSYMBOLTABLE_H_

#include <string>
#include <memory>
#include <vector>
#include <cassert>
#include <iostream>
#include <unordered_map>
#include "tacdef.h"

class Type;
class Token;

struct SymtabEntry
{
    std::string const* pname;
    std::shared_ptr<Type> type;
    int level; /// level 0 means global scope

    bool ambiguous = false;
    size_t offset;
    Tac::Reg boundTo;

    bool isParam = false;
    int seq;

    Token const* position;
    SymtabEntry(std::shared_ptr<Type> ty, Token const* p, int lv)
            : type(std::move(ty)), position(p), level(lv) {}
};

class SymbolTable
{
public:
    explicit SymbolTable(SymbolTable* outer);
    virtual ~SymbolTable() = default;
    // empty shared_ptr means not found
    virtual SymtabEntry* find(const std::string& name) = 0;
    virtual SymtabEntry* findInCurr(const std::string& name) = 0;
    virtual bool insert(const std::string&, const std::shared_ptr<Type>& type, Token const*) = 0;
    virtual bool insertOrAssign(const std::string&, const std::shared_ptr<Type>& type, Token const*) = 0;
    virtual size_t erase(const std::string&) = 0;
    template<typename Func>
    bool insertOrConditionalAssign(const std::string&, const std::shared_ptr<Type>&,
                                   Token const*, Func );
    int level() const { return level_; }
    void level(int l) { level_ = l; }
protected:
    SymbolTable* outer_;
    int level_; // level 0 means global scope
};

class HashSymtab : public SymbolTable
{
public:
    explicit HashSymtab(SymbolTable* outer, bool deeper = false);
    // empty shared_ptr means not found
    SymtabEntry* find(const std::string& name) override;
    SymtabEntry* findInCurr(const std::string& name) override;
    bool insert(const std::string&, const std::shared_ptr<Type>& type, Token const*) override;
    bool insertOrAssign(const std::string&, const std::shared_ptr<Type>&, Token const*) override {
        // TODO imple
        return false;
    };
    size_t erase(const std::string&) override;

    using iterator = std::unordered_map<std::string, SymtabEntry>::iterator;
    iterator begin();
    iterator end();
private:
    std::unordered_map<std::string, SymtabEntry> table_;
    bool deeper_;
};

class ListSymtab : public SymbolTable
{
public:
    using value_type = std::pair<std::string, SymtabEntry>;
    explicit ListSymtab(SymbolTable* outer = nullptr);

    SymtabEntry* find(const std::string& name) override;
    SymtabEntry* findInCurr(const std::string& name) override;

    bool insert(const std::string&, const std::shared_ptr<Type>&, Token const*) override;
    bool insertOrAssign(const std::string&, const std::shared_ptr<Type>&, Token const*) override {
        // TODO imple
        return false;
    }
    size_t erase(const std::string&) override;

    using iterator = std::vector<value_type>::iterator;
    using citerator = std::vector<value_type>::const_iterator;

    iterator begin() { return table_.begin(); }
    iterator end() { return table_.end(); }
    citerator cbegin() const { return table_.cbegin(); }
    citerator cend() const { return table_.cend(); }
    // simply push back and without any check:
    void push_back(const std::string&, const std::shared_ptr<Type>&, Token const*);
private:
    std::vector<value_type> table_;
};

template<typename Func>
bool SymbolTable::insertOrConditionalAssign(const std::string& name,
                                            const std::shared_ptr<Type>& type, Token const* pos,
                                            Func P)
{
    auto oldTy = findInCurr(name);
    if (!oldTy) {
        // not found. insert.
        return insert(name, type, pos); // always true
    } else {
        // found.
        if (P(*oldTy->type)) {
            // satisfy, replace
            // std::cout << "replace take place" << std::endl;
            auto n = erase(name);
            assert(n == 1);
            return insert(name, type, pos); // always true
        } else {
            return false;
        }
    }
}

#endif