#ifndef MIPS32_H
#define MIPS32_H

namespace mips32
{
/// actually designed for spim
enum class InstName {
    Nop,
    /// all arithmetic instructions are without overflow
    Addu, /// pseudo inst. addu rdest rsrc1 src2
    Subu, /// subu rd rs rt
    Mul, /// mul rd rs rt
    Div, Divu, /// pseudo inst. div/divu rdest rsrc1 src2
    Rem, Remu, /// pseudo inst. rem/remu rdest rsrc1 rsrc2
    Nor,
    Not, /// pseudo inst. not rdest rsrc
    Or, /// or rd rs rt
    Ori, /// ori rd rs imm
    And, Andi, Xor, Xori,
    Sllv, /// sllv rd, rt, rs
    Srav, Srlv,

    Move,

    La, /// pseudo inst. la rdest Label
    Lb, Lbu, Lh, Lhu, Lw, /// load rt imm(rs)

    Sb, Sh, Sw, /// store rt imm(rs)

    Lui, /// lui rt, imm
    Li, /// pseudo inst, li rdest, imm
    Slt, Sltu, Seq, Sge, Sgeu, Sgt, Sgtu, Sle, Sleu, Sne, /// inst rdest, rsrc1, rsrc2
    Beq, Bne, Bge, Bgeu, Bgt, Bgtu, Ble, Bleu, Blt, Bltu, /// beq rs rt label
    J, Jal,
};

/// Virtual register used before register allocation
struct VReg
{

};

struct Instruction
{
    InstName inst;
};



} // namespace mips32

#endif //MIPS32_H
