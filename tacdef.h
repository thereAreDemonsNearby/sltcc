
#ifndef TACDEF_H__
#define TACDEF_H__

#include <variant>
#include <string>
#include <list>
#include <memory>
// #include <boost/variant.hpp>
#include <vector>
#include <map>
#include <cinttypes>
#include "platform.h"

struct Entry;
class ListSymtab;
class Type;


namespace Tac
{
class StringPool;
struct StringPoolEntry;

enum Opcode
{
    Nop,
    LabelLine,
    Loadr, Loadrc, Loadrr, Loadi,
    Loadru, Loadrcu, Loadrru, Loadiu,
    Storer, Storerc, Storerr,
    Add, Sub, Mul, Divu, Divs, Modu, Mods,
    Shl, Shrl, Shra, BAnd, BOr, BXor, BInv,
    Movrr,
    Extu, Exts,
    Cmp, Jmp, Jeq, Jne, Jgs, Jges, Jls, Jles,
    Jgu, Jgeu, Jlu, Jleu, Call, RetVal, /** return a value in register : retval %r1*/
    GetArg, GetArgP,
    PutArg, PutArgStk,
};

struct Reg
{
    enum
    {
        PArg = -1, PFrame = -2, PStack = -3
    };
    int n; // if name >= 0, general register
    explicit Reg(int r = 0) : n(r) {}
    std::string toString() const;
    bool operator==(Reg rhs) const;
};

struct Label
{
    int n;
    explicit Label(int i = -1) : n(i) {}
    std::string toString() const;
    bool operator==(Label rhs) const;
};

struct Var
{
    enum Tag
    {
        TReg, TImmi, TVar, TLbl, TPool, TNone
    } tag;

    /// int64 : big enough
    std::variant<Reg, int64_t, Entry*, Label> uvar;

    Var() : tag(TNone) {}
    Var(Reg r);
    Var(int64_t imm);
    Var(Entry* s);
    Var(Label l);
    Var(StringPoolEntry* e);
    std::string toString() const;
    bool operator==(const Var&) const;
    static const Var empty;
};

struct Quad
{
    Opcode op;
    int width;
    Var opnd1;
    Var opnd2;
    Var res;

    Quad()
    {
        op = Nop;
    }

    Quad(Opcode optor, Var v1 = Var::empty,
         Var v2 = Var::empty, Var r = Var::empty, int sw = PTRSIZE)
            : op(optor), opnd1(v1), opnd2(v2), res(r), width(sw)
    {}
    std::string toString() const;
    bool operator==(const Quad& rhs) const;
};

/*struct BasicBlock
{
    std::list<Quad>::iterator head;
    std::vector<std::list<Quad>::iterator> successors;
};*/

struct Function
{

    // TODO global variable holder
    std::string name;
    ListSymtab* params;
    std::shared_ptr<Type> retType;
    std::list<Quad> quads;
    std::vector<Entry*> local;

    explicit Function(std::string n, ListSymtab* a)
            : name(std::move(n)), params(a) {}
    std::string toString() const;
};

struct StringPoolEntry
{
    /// more fields may be added in the future
};

class StringPool
{
private:
    std::map<std::string, StringPoolEntry> pool_;
public:
    StringPoolEntry* findOrInsert(const std::string&);
};

struct TacIR
{
    StringPool strPool;
    std::vector<Function> funcs;
    std::string toString() const;
};

}

#endif //TACDEF_H__
