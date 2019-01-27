
#ifndef TACDEF_H__
#define TACDEF_H__

#include <variant>
#include <string>
#include <list>
#include <memory>
#include <vector>
#include <map>
#include <unordered_set>
#include <cinttypes>
#include <type_traits>
#include "platform.h"

struct SymtabEntry;
class ListSymtab;
class Type;


namespace Tac
{

enum Opcode
{
    Nop,
    LabelLine,
    Loadr /** load value. The address is specified by a register.*/,
    Loadrc /** load value. The address is specified by a register and an offset */,
    Loadi /** load an immidiate number */,
    Loadru, Loadrcu, Loadiu,
    Storer, /// Storer <valueToBeStored> <empty> <destAddr>
    Storerc, /// Storerc <valueToBeStored> <destAddrOffset> <destAddrBase>
    Add, Sub, Mul, Divu, Divs, Modu, Mods,
    Shl, Shrl, Shra, BAnd, BOr, BXor, BInv,
    Movrr, /// copy between reigisters
    Extu, Exts, /// 扩展指令表示将一定长度（由指令的width字段指出）的半字或字节扩展到字长
    Jmp, Jeq, Jne, Jgs, Jges, Jls, Jles,
    Jgu, Jgeu, Jlu, Jleu,
    Call, /** call <funcent> <empty> <reg(return value)> */
    Ret, /** return a value in register : retval %r1*/
    LoadVarPtr, /// LoadVarPtr <StackObject> <empty> <reg>
    Memcpy, /// Memcpy <rhsAddr> <size> <lhsAddr>

    /** 用于从函数体内获得参数的值或地址 */
    /** num 算上返回值变成的参数 */
    GetParamVal, /// maybe from memory!!! should not be optimized out
    GetParamPtr, /// GetParam* num empty reg

    Alloca, /// Alloca <StackObject> <empty> <reg(save the address)> allocate memory on stack
    Dealloca, /// Dealloca <StackObject>

    LoadGlobalPtr, /// LoadGlobalPtr <string>
    LoadConstantPtr, /// LoadConstantPtr <seq number>
};

struct Reg
{
    int n; /// exclusive for a particular function
    explicit Reg(int r = -1) : n(r)
    {}

    std::string toString() const;

    bool operator==(Reg rhs) const;
};

struct Label
{
    int n; /// exclusive for a particular file
    explicit Label(int i = -1) : n(i)
    {}

    std::string toString() const;

    bool operator==(Label rhs) const;

    /// label must be unique in file scope.
};

/// represent a piece of stack memory
struct StackObject
{
    int n; /// exclusive for a particular function
    size_t alignAt; /// 直接按字节计。值得注意的是，在汇编里，是按照2的次方字节计的
    uint64_t size;

    StackObject(int i, uint64_t s, size_t align)
            : n(i), alignAt(align), size(s)
    {}
};

/// In asm, some constants are too big to fit in a instruction
/// In this situation, they are labeled and made to be part of
/// the object file.
/// 为了两种情况而设计：1.（初始化或未初始化的）全局变量，
///                  2. 尺寸比较大的字面量(字符串字面量、大于字长的字面量如32位架构中的double)
struct StaticObject
{
    /// padding
    struct Padding
    {
        size_t size;

        bool operator==(const Padding& rhs) const
        { return size == rhs.size; }
    };

    /// global variables use variable name as their id,
    /// while readonly data like string literal and double literal
    /// use a unique sequence number as their id


    using BinData = std::variant<int8_t, int16_t, int32_t, int64_t, Padding>; /// bin data like .word .half
    using DataType = std::variant<std::monostate,
                                  std::string, /// character data, null terminated ascii
                                  std::vector<BinData>// bin data
                                 >;
    DataType data;

    size_t size;
    size_t align;

    explicit StaticObject(const std::string& ascii);

    StaticObject(size_t size, size_t align,
                 std::vector<BinData>&& data);

    StaticObject(size_t size, size_t align);

    bool operator==(const StaticObject& rhs) const
    { return data == rhs.data; }

    bool initialized() const {
        return data.index() != 0;
    }
};

struct Var
{
    /// int64 : big enough

    struct ImmType
    {
        int64_t val;

        /// not necessary to be explicit
        template<typename IntType>
        ImmType(IntType v) : val(static_cast<int64_t>(v)) {
            static_assert(std::is_integral_v<IntType>);
        }
    };

    struct FuncLabel
    {
        std::string name;
    };

    struct VarLabel
    {
        std::string name;
    };

    std::variant<std::monostate,
                 Reg, ImmType, StackObject, Label,
                 FuncLabel /* for functions only */,
                 VarLabel /* for global variables only */
                 > uvar;

    Var() {}
    Var(Reg r);
    Var(const StackObject& o);
    Var(FuncLabel f); /// for functions only
    Var(VarLabel v);
    Var(ImmType imm);
    Var(Label l);
    std::string toString() const;
    static const Var empty;
};

/**
  * PassBy的意义：
  * Value：针对标量，直接寄存器传递就可以
  * Ptr: 针对struct，Call指令里的寄存器只表示对象的地址。
  *           因为在目前的 ir 架构中，struct只能放在内存里，只能用其地址去标记它。
  */

struct ArgInfo
{
    enum { Value, Ptr };

    Reg reg;
    int regMeans; /// reg is a value or a ptr
    size_t size;
    size_t align;
    bool scalar; /// only scalars are possible to be passed by registers
};

using ArgPassingSpec = std::vector<ArgInfo>;

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
};

struct Function
{
    std::string name;
    ListSymtab* params;
    std::shared_ptr<Type> retType;

    std::list<Quad> quads;

    explicit Function(std::string n, ListSymtab* a, std::shared_ptr<Type> ret)
            : name(std::move(n)), params(a), retType(std::move(ret)) {}
    std::string toString() const;
};


struct TacIR
{
    std::vector<std::pair<std::string, StaticObject>> globalVars;
    std::vector<StaticObject> literalPool;
    std::vector<Function> funcs;
    std::string toString() const;

    void addGlobalVar(const std::string& name, const StaticObject& so);
    int addLiteral(const StaticObject& so);

};

} // end namespace tac

#endif //TACDEF_H__
