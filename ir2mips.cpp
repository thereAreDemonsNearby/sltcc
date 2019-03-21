#include <cassert>
#include "mips32.h"
#include "tacdef.h"
#include "ir2mips.h"
#include "utils.h"

using namespace std::literals;

namespace
{

std::size_t log2p1(std::size_t v)
{
    /// assume v is power of 2
    std::size_t mask = 1;
    for (std::size_t i = 0; i < sizeof(std::size_t); ++i) {
        if (mask == v) {
            assert((v & (~mask)) == 0);
            return i;
        }
    }
    assert(false);
}

std::string dump(Tac::StaticObject const& so);
void translateStaticData(mips32::AsmFile& asmFile, Tac::TacIR const& ir);
mips32::Function translateFunction(Tac::Function const&);

} // end namespace anonymous

struct ParamInfo
{
    enum PassThrough { Register, Stack };
    size_t size;
    size_t align;
    PassThrough passThrough;

    /// 当passThrough等于Register时，offset表示
    /// $a* 的寄存器编号，取值为0～3
    /// 当passThrough等于Stack时，offset表示
    /// 参数在内存中相对于$fp的位置，即 mem[$fp+offset]
    /// 另外，为了 #简化实现#，在每个函数的开头，将$a0~$a3先
    /// 保存进栈中，作为栈的地址最高处的头四个元素，从高到低分别
    /// 是a0~a3
    size_t offset;
};

struct StackStructure
{
    struct Chunk
    {
        int n; /// corresponding Tac::StackObject.n
        std::size_t size;
        int64_t offset; /// offset == n means the actual offset is $sp + n
        /// can be negative, which means the chunk is under $sp
    };

    std::vector<Chunk> perm; /// from $sp to higher
    std::vector<Chunk> temp; /// from $sp to lower
    size_t permSize = 0;
    size_t tempSize = 0;
};

class FunctionTranslater
{
private:
    Tac::Function const& tacFunc_;
    StackStructure stackStructure_;
    std::map<Tac::Reg, mips32::Reg> regBinding;
    std::vector<ParamInfo> params;

    int virtualRegSeq = 0;

    mips32::Reg nextVirtualReg() { return mips32::Reg(mips32::Reg::Virtual, virtualRegSeq++); };
    mips32::BasicBlock translateBasicBlock(Tac::BasicBlock const&);
    mips32::Instruction translateInstruction(Tac::Quad const&);
    void collectStackObject();
    void decideParamPosition();

    mips32::InstName properLoadInst(Tac::Quad const&);
    mips32::InstName properArithInst(Tac::Quad const&);
    mips32::Reg toMipsReg(Tac::Var const&);
    int32_t toInt32(Tac::Var const& v);
public:
    explicit FunctionTranslater(Tac::Function const& tacFunc)
        : tacFunc_(tacFunc) {}
    
    mips32::Function translate();
};


mips32::AsmFile ir2mips32(Tac::TacIR const& ir)
{
    mips32::AsmFile asmFile;
    translateStaticData(asmFile, ir);
    for (auto const& irFunc : ir.funcs) {
        asmFile.funcs.push_back(translateFunction(irFunc));
    }

    return asmFile;
}

mips32::Function FunctionTranslater::translate()
{
    decideParamPosition();
    collectStackObject();
    // TODO 插入函数初始化代码

    mips32::Function func;
    func.name = tacFunc_.name;
    std::unordered_map<Tac::ConstBasicBlockPtr,
            mips32::BasicBlockPtr> basicBlockPtrMap;
    for (Tac::ConstBasicBlockPtr irIter = tacFunc_.basicBlocks.begin();
         irIter != tacFunc_.basicBlocks.end(); ++irIter) {
        func.basicBlocks.push_back(translateBasicBlock(*irIter));
        auto iter = std::prev(func.basicBlocks.end());
        basicBlockPtrMap.insert({irIter, iter});
    }

    Tac::ConstBasicBlockPtr irIter = tacFunc_.basicBlocks.begin();
    mips32::BasicBlockPtr mipsIter = func.basicBlocks.begin();
    for ( ; irIter != tacFunc_.basicBlocks.end() && mipsIter != func.basicBlocks.end();
            ++irIter, ++mipsIter) {
        for (Tac::BasicBlockPtr iter : irIter->succs) {
            auto mapIt = basicBlockPtrMap.find(Tac::ConstBasicBlockPtr(iter));
            assert(mapIt != basicBlockPtrMap.end());
            mipsIter->succs.push_back(mapIt->second);
        }
        for (Tac::BasicBlockPtr iter : irIter->preds) {
            auto mapIt = basicBlockPtrMap.find(Tac::ConstBasicBlockPtr(iter));
            assert(mapIt != basicBlockPtrMap.end());
            mipsIter->preds.push_back(mapIt->second);
        }
    }
    assert(irIter == tacFunc_.basicBlocks.end() && mipsIter == func.basicBlocks.end());

    // TODO 关于栈和参数的一些标记，可能需要为mips32::Function添加一些成员变量。

    return func;
}

void FunctionTranslater::collectStackObject()
{
    for (auto& bb : tacFunc_.basicBlocks) {
        for (auto& quad : bb.quads) {
            if (quad.op == Tac::Alloca) {
                auto& so = std::get<Tac::StackObject>(quad.opnd1.uvar);
                std::size_t offset = alignUp(stackStructure_.permSize, so.alignAt);
                stackStructure_.perm.push_back(
                        StackStructure::Chunk{so.n, so.size, static_cast<int64_t>(offset)}
                        );
                stackStructure_.permSize = offset + so.size;
            }
        }
    }
}

mips32::BasicBlock FunctionTranslater::translateBasicBlock(Tac::BasicBlock const& irBasicBlock)
{
    mips32::BasicBlock mipsBasicBlock;

    for (auto& irInst : irBasicBlock.quads) {

    }
}

mips32::Instruction FunctionTranslater::translateInstruction(Tac::Quad const& quad)
{
    int32_t offset;
    if (quad.op == Tac::Nop) {
        return {mips32::InstName::Nop, {}};
    } else if (quad.op == Tac::LabelLine) {
        return {mips32::InstName::LabelLine,
                mips32::LabelInst{std::get<Tac::Label>(quad.opnd1.uvar).n}};
    } else if (equalsAny(quad.op,
               Tac::Loadrc, Tac::Loadrcu, Tac::Loadr, Tac::Loadru)){
        mips32::InstName inst = properLoadInst(quad);
        int32_t off;
        if (equalsAny(quad.op, Tac::Loadrc, Tac::Loadrcu)) {
            off = toInt32(quad.opnd2);
        } else {
            off = 0;
        }
        return {inst, mips32::LoadStoreInst{/*rt rs off*/
                toMipsReg(quad.res), toMipsReg(quad.opnd1), off}};
    } else if (equalsAny(quad.op, Tac::Loadi, Tac::Loadiu)) {
        return {mips32::InstName::Li, mips32::LuiLiInst{
                toMipsReg(quad.res),
                toInt32(quad.opnd1)}};
    } else if (equalsAny(quad.op, Tac::Storer, Tac::Storerc)) {
        int32_t off;
        if (quad.op == Tac::Storerc) {
            off = toInt32(quad.opnd2);
        } else {
            off = 0;
        }
        mips32::InstName inst;
        switch (quad.width) {
        case 1:
            inst = mips32::InstName::Sb;
            break;
        case 2:
            inst = mips32::InstName::Sh;
            break;
        case 4:
            inst = mips32::InstName::Sw;
            break;
        default:
            assert(false);
        }

        return {inst, mips32::LoadStoreInst{
                    toMipsReg(quad.res), toMipsReg(quad.opnd1), off}};
    } else if (enumBetween(Tac::Add, quad.op, Tac::BXor)) {
        /// only addu/div/divu may have a imm or reg for opnd2
        /// or and xor: ori andi xori
        auto inst = properArithInst(quad);
        if (std::holds_alternative<Tac::Reg>(quad.opnd2.uvar)) {
            return {inst, mips32::ArithInst{
                toMipsReg(quad.res), toMipsReg(quad.opnd1), toMipsReg(quad.opnd2)}
            };
        } else {
            assert(std::holds_alternative<Tac::Var::ImmType>(quad.opnd2.uvar));
            return {inst, mips32::ArithInst_Imm{
                toMipsReg(quad.res), toMipsReg(quad.opnd1), toInt32(quad.opnd2)
            }};
        }
    } else if (quad.op == Tac::BInv) {
        return {mips32::InstName::Not, mips32::ArithInst{
            toMipsReg(quad.res), toMipsReg(quad.opnd1), mips32::Reg{mips32::Reg::Invalid}
        }};
    } else if (quad.op == Tac::Movrr) {
        return {mips32::InstName::Move, mips32::ArithInst{
                toMipsReg(quad.res), toMipsReg(quad.opnd1), mips32::Reg{mips32::Reg::Invalid}
        }};
    } else if (equalsAny(quad.op, Tac::Extu, Tac::Exts)) {
        /// will be translated to multiple mips instructions
    } // TODO

}

mips32::InstName FunctionTranslater::properLoadInst(Tac::Quad const& quad)
{
    if (quad.width == 4)
        return mips32::InstName::Lw;

    switch (quad.op) {
    case Tac::Loadr:
    case Tac::Loadrc:
        switch (quad.width) {
        case 1: return mips32::InstName::Lb;
        case 2: return mips32::InstName::Lh;
        default: assert(false);
        }
        break;
    case Tac::Loadru:
    case Tac::Loadrcu:
        switch (quad.width) {
        case 1: return mips32::InstName::Lbu;
        case 2: return mips32::InstName::Lhu;
        default: assert(false);
        }
    default:
        assert(false);
    }
}

mips32::Reg FunctionTranslater::toMipsReg(Tac::Var const& v)
{
    Tac::Reg tr = std::get<Tac::Reg>(v.uvar);

    if (auto iter = regBinding.find(tr);
        iter != regBinding.end()) {
        return iter->second;
    } else {
        auto mr = nextVirtualReg();
        regBinding.emplace(tr, mr);
        return mr;
    }
}

int32_t FunctionTranslater::toInt32(Tac::Var const& v)
{
    return static_cast<int32_t>(std::get<Tac::Var::ImmType>(v.uvar).val);
}

mips32::InstName FunctionTranslater::properArithInst(Tac::Quad const& quad)
{
    using namespace mips32;
    static std::map<Tac::Opcode, mips32::InstName> map = {
            {Tac::Add, InstName::Addu}, {Tac::Sub, InstName::Subu}, {Tac::Mul, InstName::Mul},
            {Tac::Divu, InstName::Divu}, {Tac::Divs, InstName::Div}, {Tac::Modu, InstName::Remu},
            {Tac::Mods, InstName::Rem}, {Tac::Shl, InstName::Sllv}, {Tac::Shra, InstName::Srav},
            {Tac::Shrl, InstName::Srlv}, {Tac::BAnd, InstName::And}, {Tac::BOr, InstName::Or},
            {Tac::BXor, InstName::Xor},
    };

    auto iter = map.find(quad.op);
    assert(iter != map.end());
    auto inst = iter->second;
    if (equalsAny(inst, InstName::And, InstName::Or, InstName::Xor)) {
        if (std::holds_alternative<Tac::Var::ImmType>(quad.opnd2.uvar)) {
            switch (inst) {
            case InstName::And: return InstName::Andi;
            case InstName::Or: return InstName::Ori;
            case InstName::Xor: return InstName::Xori;
            default: assert(false);
            }
        }
    }
    return inst;
}

void FunctionTranslater::decideParamPosition()
{
    size_t stackOffset = 0; // relative to $fp
    size_t aIndex = 0; // index of $a
    for (Tac::ParamInfo const& pi : tacFunc_.params) {
        if (pi.scalar && pi.size <= PTRSIZE && aIndex <= 3) {
            // size align passThrough offset
            params.emplace_back(pi.size, pi.align, ParamInfo::Register, aIndex++);
        } else {
            stackOffset = alignUp(stackOffset, pi.align);
            params.emplace_back(pi.size, pi.align, ParamInfo::Stack, stackOffset);
            stackOffset += pi.size;
        }
    }
}

namespace
{
std::string dump(Tac::StaticObject const& so)
{
    std::string ret;
    if (!so.initialized()) {
        ret.append("\t.space "s + std::to_string(so.size) + "\n");
    } else {
        std::visit(overload {

                [](std::monostate) { assert(false); },

                [&ret](std::string const& ascii) {
                    ret.append("\t.asciiz "s + ascii + "\n");
                },

                [&ret](std::vector<Tac::StaticObject::BinData> const& vec) {
                    for (auto const& data : vec) {
                        std::string str = std::visit(overload {
                                [](int8_t v) { return ".byte "s + std::to_string((int32_t)v); },
                                [](int16_t v) { return ".half "s + std::to_string(v); },
                                [](int32_t v) { return ".word "s + std::to_string(v); },
                                [](int64_t v) { return "TODO int64_t"s; },
                                [](double d) { return ".double "s + std::to_string(d); },
                                [](Tac::StaticObject::Padding p) {
                                    return ".space "s + std::to_string(p.size);
                                },
                        }, data);

                        ret.append("\t"s + str + "\n");
                    }
                },
        }, so.data);
    }

    return ret;
}




void translateStaticData(mips32::AsmFile& asmFile, Tac::TacIR const& ir)
{
    for (auto const& pair : ir.globalVars) {
        asmFile.staticData.append("\t.data\n")
                .append("\t.align ")
                .append(std::to_string(log2p1(pair.second.align)))
                .append("\n")
                .append(pair.first).append(":\n"); /// variable name

        asmFile.staticData.append(dump(pair.second));
    }


    for (std::size_t i = 0; i < ir.literalPool.size(); ++i) {
        asmFile.staticData.append("\t.data\n")
                .append("\t.align ")
                .append(std::to_string(log2p1(ir.literalPool[i].align)))
                .append("\n");
        asmFile.staticData.append("$LC"s + std::to_string(i) + ":\n");
        asmFile.staticData.append(dump(ir.literalPool[i]));
    }
}

mips32::Function translateFunction(Tac::Function const& irFunc)
{
    FunctionTranslater translater(irFunc);
    return translater.translate();
}


} // end namespace anonymous