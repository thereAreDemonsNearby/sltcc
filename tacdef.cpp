#include "tacdef.h"
#include "SymbolTable.h"
#include <cassert>
#include <sstream>
#include <iomanip>

namespace Tac
{

using namespace std::literals;

static const char* opToString[] = {
        "Nop", "LabelLine", "Loadr", "Loadrc", "Loadi", "Loadru", "Loadrcu",
        "Loadiu", "Storer", "Storerc", "Add", "Sub", "Mul", "Divu",
        "Divs", "Modu", "Mods", "Shl", "Shrl", "Shra", "BAnd",
        "BOr", "BXor", "BInv", "Movrr", "Extu", "Exts", "Jmp",
        "Jeq", "Jne", "Jgs", "Jges", "Jls", "Jles", "Jgu",
        "Jgeu", "Jlu", "Jleu", "Call", "Ret", "LoadVarPtr", "Memcpy",
        "GetParamVal", "GetParamPtr", "Alloca", "Dealloca",
        "LoadGlobalPtr", "LoadConstantPtr",
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

Var::Var(FuncLabel f) :  uvar(std::move(f))
{
}

Var::Var(VarLabel v) : uvar(std::move(v))
{

}


const Var Var::empty{};

std::string Var::toString() const
{
    struct ToStringVisitor
    {
        std::string operator()(Reg r) { return r.toString(); }
        std::string operator()(ImmType i) { return std::to_string(i.val); }
        std::string operator()(StackObject const& s) {
            std::ostringstream os;
            os << "Stack(" << s.n << "," << s.size << "," << s.alignAt << ")";
            return os.str();
        }
        std::string operator()(FuncLabel const& f) {
            return "function("s + f.name + ")";
        }
        std::string operator()(VarLabel const& v) {
            return "global("s + v.name + ")";
        }
        std::string operator()(Label l) {
            return l.toString();
        }
        std::string operator()(std::monostate m) {
            return " ";
        }
    };

    ToStringVisitor visitor;
    return std::visit(visitor, uvar);
}


std::string Label::toString() const
{
    return ".label"s + std::to_string(n);
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
        return opnd1.toString();

    std::string ret = opToString[op];
    ret += std::to_string(width);
    ret += "\t";
    ret += opnd1.toString() + "\t";
    ret += opnd2.toString() + "\t";
    ret += res.toString();

    if (op == Call) {
        assert(passingSpec);
        for (auto const& s : *passingSpec) {
            ret += "\t{"s + s.reg.toString() + " " + std::to_string(s.size) + " " +
                    std::to_string(s.align) + (s.scalar ? " scalar" : " compound") +
                    (s.regMeans == ArgInfo::Value ? " v" : " p") + "}";
        }
    }
    return ret;
}

std::string Function::toString() const
{
    std::string ret{"def func "};
    ret.append(name).append("\n");
    for (const auto& b : basicBlocks) {
        for (const auto& q : b.quads) {
            if (q.op == LabelLine) {
                ret.append(q.toString()).append(":\n");
            } else {
                ret.append("\t").append(q.toString()).append("\n");
            }
        }
        ret.append("    ******\n");
    }
    return ret;
}

void Function::addBasicBlock(BasicBlock bb)
{
    auto n = basicBlocks.size();
    bb.n = static_cast<int>(n);
    basicBlocks.push_back(std::move(bb));
}

std::string stringifyBinData(StaticObject::BinData const& d)
{
    struct Visitor
    {
        std::string operator()(int8_t i) {
            return "B1 "s + std::to_string(i);
        }

        std::string operator()(int16_t i) {
            return "B2 "s + std::to_string(i);
        }

        std::string operator()(int32_t i) {
            return "B4 "s + std::to_string(i);
        }

        std::string operator()(int64_t i) {
            return "B8 "s + std::to_string(i);
        }

        std::string operator()(StaticObject::Padding p) {
            return "space " + std::to_string(p.size);
        }
    };

    return std::visit(Visitor{}, d);
}

std::string TacIR::toString() const
{
    std::string ret;

    auto quotedString = [] (std::string const& str) {
        std::ostringstream oss;
        oss << std::quoted(str);
        return oss.str();
    };

    for (auto const& p : globalVars) {
        ret += "global variable "s + p.first + ":\n";
        ret += "size: "s + std::to_string(p.second.size) + "  align: "
                + std::to_string(p.second.align) + "\n";
        if (p.second.initialized()) {
            if (std::holds_alternative<std::string>(p.second.data)) {
                ret += "\tasciiz "s + quotedString(std::get<std::string>(p.second.data)) + "\n";
            } else {
                assert(std::holds_alternative<std::vector<StaticObject::BinData>>(p.second.data));
                for (auto const& d : std::get<std::vector<StaticObject::BinData>>(p.second.data)) {
                    ret += "\t"s + stringifyBinData(d) + "\n";
                }
            }
        } else {
            ret += "not initialized.\n";
        }
        ret += "\n";
    }

    for (size_t i = 0; i < literalPool.size(); ++i) {
        ret += "$LC"s + std::to_string(i) + ":\n";
        assert(literalPool[i].initialized());
        if (std::holds_alternative<std::string>(literalPool[i].data)) {
            ret += "\tasciiz "s + quotedString(std::get<std::string>(literalPool[i].data)) + "\n";
        } else {
            assert(std::holds_alternative<std::vector<StaticObject::BinData>>(literalPool[i].data));
            for (auto const& d : std::get<std::vector<StaticObject::BinData>>(literalPool[i].data)) {
                ret += "\t"s + stringifyBinData(d) + "\n";
            }
        }
        ret += "\n";
    }

    for (const auto& f : funcs) {
        ret.append(f.toString()).append("\n"); // one more new line
    }
    return ret;
}

int TacIR::addLiteral(const StaticObject& so)
{
    for (size_t i = 0; i < literalPool.size(); ++i) {
        if (literalPool[i] == so) {
            return i;
        }
    }
    literalPool.push_back(so);
    return literalPool.size() - 1;
}

void TacIR::addGlobalVar(const std::string& name, const StaticObject& so)
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

StaticObject::StaticObject(size_t s, size_t a)
    : size(s), align(a)
{
}

} // end namespace tac