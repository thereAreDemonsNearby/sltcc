#include "tacdef.h"
#include "SymbolTable.h"
#include <cassert>
#include <sstream>

namespace Tac
{

static const char* strfy[] = {
        "nop", "labelline", "loadr", "loadrc", "loadi", "loadru", "loadrcu",
        "loadiu", "storer", "storerc", "add", "sub", "mul", "divu",
        "divs", "modu", "mods", "shl", "shrl", "shra", "band",
        "bor", "bxor", "binv", "movrr", "extu", "exts", "jmp",
        "jeq", "jne", "jgs", "jges", "jls", "jles", "jgu",
        "jgeu", "jlu", "jleu", "call", "ret", "loadvarptr", "stkalloc",
        "flushstkalloc", "getparamval", "getparamptr",
};

Var::Var(Reg r) : uvar(r)
{
}

Var::Var(const StackObject& o) : uvar(o)
{
}

Var::Var(Var::ImmType imm) : uvar(imm)
{
}

Var::Var(Label l) : uvar(l)
{
}

Var::Var(SymtabEntry* s) :  uvar(s)
{
}

Var::Var(const std::string& s) : uvar(s)
{

}


const Var Var::empty{};

std::string Var::toString() const
{
    struct ToStringVisitor
    {
        std::string operator()(Reg r) { return r.toString(); }
        std::string operator()(ImmType i) { return std::to_string(i); }
        std::string operator()(const StackObject& s) {
            std::ostringstream os;
            os << "Stack(" << s.n << "," << s.size << "," << s.alignAt << ")";
            return os.str();
        }
        std::string operator()(SymtabEntry* s) {
            return "function("s + s->pname + ")";
        }
        std::string operator()(const std::string& s) {
            return "globl("s + s + ")";
        }
        std::string operator()(Label l) {
            return l.toString();
        }
        std::string operator()(std::monostate m) {
            return "Tac::Var(invalid)";
        }
    };

    ToStringVisitor visitor;
    return std::visit(visitor, uvar);
}

bool Var::operator==(const Var& rhs) const
{
    return uvar == rhs.uvar;
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
    ret += std::to_string(n);
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

std::string LinearTacIR::toString() const
{
    std::string ret;
    for (const auto& f : funcs) {
        ret.append(f.toString()).append("\n"); // one more new line
    }
    return ret;
}

int LinearTacIR::addLiteral(const StaticObject& so)
{
    for (size_t i = 0; i < literalPool.size(); ++i) {
        if (literalPool[i] == so) {
            return i;
        }
    }
    literalPool.push_back(so);
    return literalPool.size() - 1;
}

void LinearTacIR::addGlobalVar(const std::string& name, const StaticObject& so)
{
    globalVars.emplace_back(name, so);
}

StaticObject::StaticObject(const std::string& ascii)
    : data(ascii), size(ascii.size()+1), align(sizeof(ascii[0]))
{
}

StaticObject::StaticObject(size_t s, size_t a, std::vector<StaticObject::BinData>&& d)
    : data(d), size(s), align(a)
{
}

StaticObject::StaticObject(size_t size, size_t align)
    : size(s), align(a)
{
}

} // end namespace tac