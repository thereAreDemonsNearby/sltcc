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
void convertStaticData(mips32::AsmFile& asmFile, Tac::TacIR const& ir);
mips32::Function convertFunction(Tac::Function const&);

} // end namespace anonymous

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

class FunctionConverter
{
private:
    Tac::Function const& tacFunc_;
    StackStructure stackStructure_;
    std::map<Tac::Reg, mips32::Reg> regBinding;

    int virtualRegSeq = 0;

    mips32::Reg nextVirtualReg() { return mips32::Reg(mips32::Reg::Virtual, virtualRegSeq++); };
    mips32::BasicBlock convertBasicBlock(Tac::BasicBlock const&);
    mips32::Instruction convertInstruction(Tac::Quad const&);
    void collectStackObject();

    mips32::InstName genProperLoadInst(Tac::Quad const&);
    mips32::Reg toMipsReg(Tac::Var const& r);
    int32_t toInt32(Tac::Var const& v);
public:
    explicit FunctionConverter(Tac::Function const& tacFunc)
        : tacFunc_(tacFunc) {}
    
    mips32::Function convert();
};


mips32::AsmFile ir2mips32(Tac::TacIR const& ir)
{
    mips32::AsmFile asmFile;
    convertStaticData(asmFile, ir);
    for (auto const& irFunc : ir.funcs) {
        asmFile.funcs.push_back(convertFunction(irFunc));
    }

    return asmFile;
}

mips32::Function FunctionConverter::convert()
{
    collectStackObject();

    mips32::Function func;
    func.name = tacFunc_.name;
    std::unordered_map<Tac::ConstBasicBlockPtr,
            mips32::BasicBlockPtr> basicBlockPtrMap;
    for (Tac::ConstBasicBlockPtr irIter = tacFunc_.basicBlocks.begin();
         irIter != tacFunc_.basicBlocks.end(); ++irIter) {
        func.basicBlocks.push_back(convertBasicBlock(*irIter));
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

void FunctionConverter::collectStackObject()
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

mips32::BasicBlock FunctionConverter::convertBasicBlock(Tac::BasicBlock const& irBasicBlock)
{
    mips32::BasicBlock mipsBasicBlock;

    for (auto& irInst : irBasicBlock.quads) {
        auto opcode = irInst.op;
        switch (opcode) {
        case Tac::Nop:
            mipsBasicBlock.addInstruction({mips32::InstName::Nop, {}});
            break;
        case Tac::LabelLine:
            mipsBasicBlock.addInstruction({mips32::InstName::LabelLine,
                                           mips32::LabelInst{std::get<std::string>(irInst.opnd1.uvar)}});
            break;
        case Tac::Loadr:

            break;
        }
    }
}

mips32::Instruction FunctionConverter::convertInstruction(Tac::Quad const& quad)
{
    int32_t offset;

    // todo may be an if chain will be better
    switch (quad.op) {
    case Tac::Nop:
        return {mips32::InstName::Nop, {}};

    case Tac::LabelLine:
        return {mips32::InstName::LabelLine,
                mips32::LabelInst{std::get<std::string const>(quad.opnd1.uvar)}};

    case Tac::Loadrc:
    case Tac::Loadrcu: {
        mips32::InstName inst = genProperLoadInst(quad);
        return {inst, mips32::LoadStoreInst{/*rt rs off*/
                toMipsReg(quad.res), toMipsReg(quad.opnd1),
                toInt32(quad.opnd2)}};
    }

    case Tac::Loadr:
    case Tac::Loadru: {
        mips32::InstName inst = genProperLoadInst(quad);
        return {inst, mips32::LoadStoreInst{/*rt rs off*/
                            toMipsReg(quad.res), toMipsReg(quad.opnd1), 0}};
    }

    case Tac::Loadi:
    case Tac::Loadiu:
        return {mips32::InstName::Li, mips32::LuiLiInst{
            toMipsReg(quad.res),
            toInt32(quad.opnd1)}};

    case Tac::Storerc: {
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

        return {
                inst, mips32::LoadStoreInst{toMipsReg(quad.res),
                                            toMipsReg(quad.opnd1),
                                            toInt32(quad.opnd2)}
        };
    }

    case Tac::Storer: {
        mips32::InstName inst;
        switch (quad.width) {
        case 1: inst = mips32::InstName::Sb; break;
        case 2: inst = mips32::InstName::Sh; break;
        case 4: inst = mips32::InstName::Sw; break;
        default: assert(false);
        }

        return {
                inst, mips32::LoadStoreInst{toMipsReg(quad.res),
                                            toMipsReg(quad.opnd1),
                                            0}
        };
    }

    case Tac::Add:
    case Tac::Sub:

    }


}

mips32::InstName FunctionConverter::genProperLoadInst(Tac::Quad const& quad)
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

mips32::Reg FunctionConverter::toMipsReg(Tac::Var const& v)
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

int32_t FunctionConverter::toInt32(Tac::Var const& v)
{
    return static_cast<int32_t>(std::get<Tac::Var::ImmType>(v.uvar).val);
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




void convertStaticData(mips32::AsmFile& asmFile, Tac::TacIR const& ir)
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

mips32::Function convertFunction(Tac::Function const& irFunc)
{
    FunctionConverter converter(irFunc);
    return converter.convert();
}


} // end namespace anonymous