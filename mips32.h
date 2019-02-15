#ifndef MIPS32_H
#define MIPS32_H

#include <variant>
#include <cinttypes>
#include <string>
#include <vector>
#include <list>

namespace Tac
{
struct TacIR;
}

namespace mips32
{
/// actually designed for spim
enum class InstName {
    Nop, LabelLine,
    /// all arithmetic instructions are without overflow
    Addu, /// pseudo inst. addu rdest rsrc1 src2
    Subu, /// subu rd rs rt
    Mul, /// mul rd rs rt
    Div, Divu, /// pseudo inst. div/divu rdest rsrc1 src2
    Rem, Remu, /// pseudo inst. rem/remu rdest rsrc1 rsrc2
    Nor, /// nor rd rs rt
    Not, /// pseudo inst. not rdest rsrc
    Or, /// or rd rs rt
    Ori, /// ori rd rs imm
    And, Andi, Xor, Xori,
    Sllv, /// sllv rd, rt, rs
    Srav, Srlv,
    Slt, Sltu, Seq, Sge, Sgeu, Sgt, Sgtu, Sle, Sleu, Sne, /// inst rdest, rsrc1, rsrc2

    Move, /// pseudo inst. move rdest rsrc

    La, /// pseudo inst. la rdest Label
    Lb, Lbu, Lh, Lhu, Lw, /// load rt imm(rs)

    Sb, Sh, Sw, /// store rt imm(rs)

    Lui, /// lui rt, imm
    Li, /// pseudo inst, li rdest, imm

    Beq, Bne, Bge, Bgeu, Bgt, Bgtu, Ble, Bleu, Blt, Bltu, /// beq rs rt label
    J, /** j label */ Jal, /** jal label(function) */

    Phi, /// for SSA
};

/// Virtual register used before register allocation
struct VReg
{
    int n; /// virtual register number
    int v; /// version number. used in SSA form

    explicit VReg(int n_) : n(n_), v(0) {}
};

struct LiveRange
{
    int n;
};

struct Reg
{
    enum Tag {
        GP, SP, FP, S /** s0 ~ s7 , callee saved  */, T /** t0 ~ t9 , caller saved */,
        A /** a0 ~ a3 , arguments */, RA /** return address */,
    };
    Tag tag;
    int n;

    explicit Reg(Tag t, int number = 0) : tag(t), n(number) {}
};

using Var = std::variant<std::monostate, VReg, LiveRange, Reg, int32_t>;


struct LabelInst
{
    std::string label;
};
/// include move
struct ArithInst
{
    Var rdest;
    Var rsrc1;
    Var src2; /// maybe register, imm or empty
};

struct LaInst
{
    Var rdest;
    std::string label;
};

struct LoadStoreInst
{
    Var rt;
    Var rs;
    int32_t offset;
};

struct LuiLiInst
{
    Var rdest;
    int32_t imm;
};

/// beq, bne, b*...
struct BranchInst
{
    Var rs;
    Var rt;
    std::string label;
};

/// j jal
struct JumpInst
{
    std::string label;
};

struct PhiInst
{
    std::vector<VReg> args;
};

using InstBody = std::variant<std::monostate, /** for 'nop' */
                              LabelInst,
                              ArithInst,
                              LaInst, LoadStoreInst, LuiLiInst,
                              BranchInst, JumpInst>;

struct Instruction
{
    InstName inst;
    InstBody body;
};

struct BasicBlock;
using BasicBlockPtr = std::list<BasicBlock>::iterator;

struct BasicBlock
{
    std::list<Instruction> instructions;
    std::vector<BasicBlockPtr> succs;
    std::vector<BasicBlockPtr> preds;
};

struct Function
{
    std::string funcName;
    std::list<BasicBlock> basicBlocks;
};

struct AsmFile
{
    std::string staticData; /// directly
    std::vector<Function> funcs;
};

} // namespace mips32

#endif //MIPS32_H
