#include "SymbolTable.h"
#include <algorithm>

SymbolTable::SymbolTable(SymbolTable* outer)
    : outer_(outer)
{
    if (outer) {
        level_ = outer->level_ + 1;
    } else {
        level_ = 0;
    }
}


HashSymtab::HashSymtab(SymbolTable* outer, bool deeper)
    : SymbolTable(outer), deeper_(deeper)
{}

Entry* HashSymtab::findInCurr(const std::string& name)
{
    auto res = table_.find(name);
    if (res != table_.end()) {
        // found
        return &res->second;
    } else {
        return nullptr;
    }
}

Entry* HashSymtab::find(const std::string& name)
{
    auto ptr = findInCurr(name);
    if (ptr) {
        return ptr;
    } else {
        if (level_ == 0) {
            return nullptr;
        } else {
            return outer_->find(name);
        }
    }
}

bool HashSymtab::insert(const std::string& name,
                        const std::shared_ptr<Type>& type, Token const* tok)
{
    if (deeper_) {
        auto res = outer_->findInCurr(name);
        if (res) {
            return false;
        }
    }
    auto pr = table_.insert({name, {type, tok, level_}});
    if (pr.second) {
        pr.first->second.pname = &pr.first->first;
    }
    return pr.second; // denote whether the insertion took place

}

size_t HashSymtab::erase(const std::string& n)
{
    return table_.erase(n);
}

HashSymtab::iterator HashSymtab::begin()
{
    return table_.begin();
}

HashSymtab::iterator HashSymtab::end()
{
    return table_.end();
}

ListSymtab::ListSymtab(SymbolTable* outer)
    : SymbolTable(outer)
{}



Entry* ListSymtab::findInCurr(const std::string& name)
{
    auto found = std::find_if(table_.begin(), table_.end(), [&name](const value_type& v) {
        return v.first == name;
    });
    if (found != table_.end()) {
        return &found->second;
    } else {
        return nullptr;
    }
}

Entry* ListSymtab::find(const std::string& name)
{
    auto ptr = findInCurr(name);
    if (ptr) {
        return ptr;
    } else {
        if (level_ == 0) {
            return nullptr;
        } else {
            return outer_->find(name);
        }
    }
}

bool ListSymtab::insert(const std::string& name,
                        const std::shared_ptr<Type>& type, Token const* tok)
{
    // auto found = find(name);
    if (findInCurr(name)) {
        return false;
    } else {
        table_.push_back({name, {type, tok, level_}});
        table_.back().second.pname = &table_.back().first;
        return true;
    }
}

void ListSymtab::push_back(const std::string& n, const std::shared_ptr<Type>& t,
                           Token const* tok)
{
    table_.push_back({n, {t, tok, level_}});
}

size_t ListSymtab::erase(const std::string& name)
{
    auto it = std::find_if(table_.begin(), table_.end(), [&name](const value_type& v) {
        return v.first == name;
    });
    if (it == table_.end()) {
        return 0;
    }

    table_.erase(it);
    return 1;
}
