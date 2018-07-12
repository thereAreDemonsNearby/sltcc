
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
#include <optional>
#include "platform.h"

struct SymtabEntry;
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
    Loadr, Loadrc, Loadi,
    Loadru, Loadrcu, Loadiu,
    Storer, Storerc,
    Add, Sub, Mul, Divu, Divs, Modu, Mods,
    Shl, Shrl, Shra, BAnd, BOr, BXor, BInv,
    Movrr,
    Extu, Exts,
    Jmp, Jeq, Jne, Jgs, Jges, Jls, Jles,
    Jgu, Jgeu, Jlu, Jleu,
    Call, /** call funcent empty reg */
    Ret, /** return a value in register : retval %r1*/
    LoadVarPtr, /// LoadVarAddr ent empty reg
    StkAlloc,    /// Alloca emp emp reg(ptr to allocated mem)
    FlushStkAlloc, /// Disalloca  lastnum
    /** 用于从函数体内获得参数的值或地址 */
    /** num 算上返回值变成的参数 */
    GetParamVal, GetParamPtr, /// GetParam* num empty reg
};

struct Reg
{
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
    std::variant<Reg, int64_t, SymtabEntry*,
            Label, StringPoolEntry*> uvar;

    Var() : tag(TNone) {}
    Var(Reg r);
    Var(int64_t imm);
    Var(SymtabEntry* s);
    Var(Label l);
    Var(StringPoolEntry* e);
    std::string toString() const;
    bool operator==(const Var&) const;
    static const Var empty;
};

enum class PassBy
{
    Value, ValuePtr
};

struct ArgPassingSpec
{
    std::vector<std::pair<Reg, PassBy>> list;
};

struct Quad
{
    Opcode op;
    std::size_t width;
    Var opnd1;
    Var opnd2;
    Var res;
    // only available when op is "Call"
    std::optional<ArgPassingSpec> passingSpec;

    Quad()
    {
        op = Nop;
    }

    Quad(Opcode optor, Var v1 = Var::empty,
         Var v2 = Var::empty, Var r = Var::empty, std::size_t sw = PTRSIZE)
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


struct StkAllocUnit
{
    std::shared_ptr<Type> type;
    bool expired = false;

    StkAllocUnit(std::shared_ptr<Type> t) : type(std::move(t)) {}
};

struct StkAllocManager
{
    std::vector<StkAllocUnit> pool;
    std::size_t liveBegin = 0;
    std::size_t size = 0;
};

struct Function
{
private:
    StkAllocManager tempPool_;
public:
    // TODO global variable holder
    std::string name;
    ListSymtab* params;
    std::shared_ptr<Type> retType;

    std::list<Quad> quads;
    std::vector<SymtabEntry*> local;

    explicit Function(std::string n, ListSymtab* a, std::shared_ptr<Type> ret)
            : name(std::move(n)), params(a), retType(std::move(ret)) {}
    std::string toString() const;
    std::size_t stkAlloc(std::shared_ptr<Type> ty);
    void stkDisalloc(std::size_t last);
};

struct StringPoolEntry
{
    /// more fields may be added in the future
    const std::string* sptr = nullptr;
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
