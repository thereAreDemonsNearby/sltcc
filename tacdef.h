
#ifndef TACDEF_H__
#define TACDEF_H__

#include <variant>
#include <string>
#include <list>
#include <memory>
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
    Loadr /** load value. The address is specified by a register.*/,
    Loadrc /** load value. The address is specified by a register and an offset */,
    Loadi /** load an immidiate number */,
    Loadru, Loadrcu, Loadiu,
    Storer, Storerc,
    Add, Sub, Mul, Divu, Divs, Modu, Mods,
    Shl, Shrl, Shra, BAnd, BOr, BXor, BInv,
    Movrr,
    Extu, Exts, /// 扩展指令代表将一定长度（由指令的width字段指出）的半字或字节扩展到字长
    Jmp, Jeq, Jne, Jgs, Jges, Jls, Jles,
    Jgu, Jgeu, Jlu, Jleu,
    Call, /** call funcent empty reg */
    Ret, /** return a value in register : retval %r1*/
    LoadVarPtr, /// LoadVarPtr ent empty reg

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

/**
  * PassBy的意义：
  * Value：针对标量，直接寄存器传递就可以
  * ValuePtr: 针对struct，Call指令里的寄存器只表示对象的地址。
  *           因为在目前的 ir 架构中，struct只能放在内存里，只能用其地址去标记它。*/

enum class PassBy
{
    Value, ValuePtr
};

using ArgPassingSpec = std::vector<std::pair<Reg, PassBy>>;

struct Quad
{
    Opcode op;
    std::size_t width;

    Var opnd1;
    Var opnd2;
    Var res;
    // only available when op is "Call"
    std::unique_ptr<ArgPassingSpec> passingSpec;

    Quad() : op(Nop)
    {
    }

    Quad(Opcode optor, Var v1 = Var::empty,
         Var v2 = Var::empty, Var r = Var::empty, std::size_t sw = PTRSIZE)
            : op(optor), opnd1(v1), opnd2(v2), res(r), width(sw)
    {}

    void setPassingSpec(ArgPassingSpec&& args) {
        passingSpec = std::make_unique<ArgPassingSpec>(std::move(args));
    }

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

struct LinearTacIR
{
    StringPool strPool;
    std::vector<Function> funcs;
    std::string toString() const;
};

}

#endif //TACDEF_H__
