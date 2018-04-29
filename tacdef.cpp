#include "tacdef.h"
#include "SymbolTable.h"
#include <cassert>

namespace Tac
{

static const char* strfy[] = {
        "nop", "labelline", "loadr", "loadrc", "loadrr", "loadi",
        "loadru", "loadrcu", "loadrru", "loadiu", "storer", "storerc", "storerr",
        "add", "sub", "mul", "divu", "divs", "modu", "mods", "shl", "shrl", "shra",
        "band", "bor", "bxor", "binv", "movrr", "extu", "exts", "cmp", "jmp",
        "jeq", "jne", "jgs", "jges", "jls", "jles", "jgu", "jgeu", "jlu", "jleu",
        "call", "retval", "getarg", "getargp", "putarg", "putargstk",
};

Var::Var(Reg r) : tag(TReg), uvar(r)
{
}

Var::Var(int64_t imm) : tag(TImmi), uvar(imm)
{
}

Var::Var(Entry* s) : tag(TVar), uvar(s)
{
}

Var::Var(Label l) : tag(TLbl), uvar(l)
{
}

const Var Var::empty(TNone);

std::string Var::toString() const
{
    switch (tag) {
    case TReg:
        return std::get<Reg>(uvar).toString();
    case TImmi:
        return std::to_string(std::get<int>(uvar));
    case TVar:
        return *std::get<Entry*>(uvar)->pname;
    case TLbl:
        return std::get<Label>(uvar).toString();
    case TPool:
        return ".STRCONST";
    case TNone:
        return " ";
    default:
        assert(false);
    }
}

bool Var::operator==(const Var& rhs) const
{
    if (tag == rhs.tag) {
        return uvar == rhs.uvar;
    } else {
        return false;
    }
}

Var::Var(StringPoolEntry* e)
    : uvar(e)
{

}

std::string Label::toString() const
{
    return std::string(".label") + std::to_string(n);
}

bool Label::operator==(Label rhs) const
{
    return n == rhs.n;
}


std::string Reg::toString() const
{
    std::string ret{"%r"};
    switch (n) {
    case PArg:
        ret += "arg";
        break;
    case PFrame:
        ret += "f";
        break;
    case PStack:
        ret += "s";
        break;
    default:
        assert(false);
    }
    return ret;
}

bool Reg::operator==(Reg rhs) const
{
    return n == rhs.n;
}


std::string Quad::toString() const
{
    if (op == LabelLine)
        return res.toString();

    std::string ret = strfy[op];
    ret += std::to_string(width);
    ret += "\t";
    ret += opnd1.toString() + "\t";
    ret += opnd2.toString() + "\t";
    ret += res.toString();

    /*while (ret.back() == ' ' || ret.back() == '\t')
        ret.pop_back();*/
    return ret;
}

bool Quad::operator==(const Quad& rhs) const
{
    return op == rhs.op && opnd1 == rhs.opnd1
            && opnd2 == rhs.opnd2 && res == rhs.res
            && width == rhs.width;
}

std::string Function::toString() const
{
    std::string ret{"def func "};
    ret.append(name).append("\n");
    for (const auto& q : quads) {
        if (q.op == LabelLine) {
            ret.append(q.toString()).append("\n");
        } else {
            ret.append("\t").append(q.toString()).append("\n");
        }
    }
    return ret;
}

std::string TacIR::toString() const
{
    std::string ret;
    for (const auto& f : funcs) {
        ret.append(f.toString()).append("\n"); // one more new line
    }
    return ret;
}


StringPoolEntry* StringPool::findOrInsert(const std::string& str)
{
    auto iter = pool_.find(str);
    if (iter == pool_.end()) {
        auto [pos, inserted] = pool_.insert({std::move(str), StringPoolEntry()});
        assert(inserted);
        return &pos->second;
    } else {
        return &iter->second;
    }
}

} // end namespace tac