#include "tacgenerator.h"
#include <iterator>

namespace
{
/// 传入一个有符号版本，然后撅定用有符号还是无符号版本
Tac::Opcode properLoadInst(const std::shared_ptr<Type>& ty, Tac::Opcode op)
{
    auto toUnsignedVersion = [](Tac::Opcode top) -> Tac::Opcode {
        switch (top) {
        case Tac::Loadi: return Tac::Loadiu;
        case Tac::Loadr: return Tac::Loadru;
        case Tac::Loadrc: return Tac::Loadrcu;
        default: assert(false);
        }
    };

    if (Type::isInteger(ty)) {
        auto intTy = static_cast<BuiltInType*>(ty.get());
        if (intTy->isUnsigned()) {
            return toUnsignedVersion(op);
        } else {
            return op;
        }
    } else {
        return toUnsignedVersion(op);
    }
}

int paramSeq(SymtabEntry* ent, const std::shared_ptr<Type>& retType)
{
    assert(ent->isParam);
    if (!Type::isScalar(retType) && !Type::isVoid(retType)) {
        return ent->seq + 1;
    } else {
        return ent->seq;
    }
}

}

#define ExprType(expr) ((expr)->promotedTo ? (expr)->promotedTo : (expr)->evalType)


void TacGenerator::visit(ASTRoot* node)
{
    // TODO some global information
    pushScope(&node->symtab_);
    for (auto& n : node->contents_) {
        n->accept(*this);
    }
    popScope();
    assert(scopeStack_.empty());
}

void TacGenerator::visit(FuncDef* node)
{
    FuncGenerator funcGen(*this, node->name_, &node->params_, node->type_->retType());
    funcGen.visit(node);
    ir_.funcs.push_back(std::move(funcGen.func()));
}

void TacGenerator::visit(VarDef* node)
{
    /// this function collects variables in the outest scope(global).
    auto size = node->type_->width();
    auto align = Type::alignAt(node->type_);
    if (node->init_) {
        if (Type::isArithmetic(node->type_)) {
            /// so far only literal init expression is permitted
            /// 这么写的话，要求全局变量定义中一定没有发生类型转换。
            if (node->init_->tag() == ExprTag::Literal) {
                auto literalExpr = static_cast<LiteralExpr*>(node->init_.get());
                auto literalTok = literalExpr->tok();
                std::vector<Tac::StaticObject::BinData> data;
                switch (literalTok->type()) {
                case Token::IntLiteral:
                    data.emplace_back(literalTok->intLiteral());
                    break;
                case Token::UnsignedLiteral:
                    data.emplace_back(static_cast<int>(literalTok->uintLiteral()));
                    break;
                case Token::DoubleLiteral: {
                    double v = literalTok->doubleLiteral();
                    data.emplace_back(*reinterpret_cast<int64_t*>(&v));
                    break;
                }
                case Token::CharLiteral:
                    data.emplace_back((int8_t)literalTok->charLiteral());
                    break;
                default:
                    assert(false);
                }

                Tac::StaticObject so(node->type_->width(),
                        Type::alignAt(node->type_), std::move(data));
                addGlobalVar(node->name_, so);
            } else {
                assert(false && "not implemented");
            }
        } else {
            // TODO not implemented
            assert(false);
        }
    } else {
        /// no init
        addGlobalVar(node->name_, Tac::StaticObject(node->type_->width(),
                Type::alignAt(node->type_)));
    }
}

void FuncGenerator::visit(IfStmt* node)
{
    Tac::Label falseLabel = nextLabel();
    BranchGenerator bg(*this, false, falseLabel);
    node->cond_->accept(bg);

    if (node->else_) {
        /// has else
        node->then_->accept(*this);
        auto iter = emit({Tac::LabelLine, falseLabel});
        node->else_->accept(*this);
        auto outLabel = nextLabel();
        emit({Tac::LabelLine, outLabel});
        currFunction_.quads.insert(iter, {Tac::Jmp, outLabel});
    } else {
        node->then_->accept(*this);
        emit({Tac::LabelLine, falseLabel});
    }
}

void FuncGenerator::visit(WhileStmt* node)
{
    auto beginLabel = nextLabel();
    auto outLabel = nextLabel();
    BranchGenerator bg(*this, false, outLabel);
    emit({Tac::LabelLine, beginLabel});
    node->cond_->accept(bg);
    node->body_->accept(*this);
    emit({Tac::Jmp, beginLabel});
    emit({Tac::LabelLine, outLabel});
}

void FuncGenerator::visit(ForStmt* node)
{
    auto beginLabel = nextLabel();
    auto outLabel = nextLabel();
    BranchGenerator bg(*this, false, outLabel);
    if (node->init_)
        node->init_->accept(*this);
    emit({Tac::LabelLine, beginLabel});
    if (node->cond_)
        node->cond_->accept(bg);
    else {
        // nothing need to do
    }
    node->body_->accept(*this);
    if (node->stepby_)
        node->stepby_->accept(*this);
    emit({Tac::Jmp, beginLabel});
    emit({Tac::LabelLine, outLabel});
}

void FuncGenerator::visit(DoWhileStmt* node)
{
    auto beginLabel = nextLabel();
    BranchGenerator bg(*this, true, beginLabel);
    node->body_->accept(*this);
    node->cond_->accept(bg);
}

Tac::Reg FuncGenerator::nextReg()
{
    // count from 0

    return Tac::Reg(regNo_++);
}

std::list<Tac::Quad>::iterator
FuncGenerator::emit(Tac::Quad&& quad)
{
    currFunction_.quads.push_back(std::move(quad));
    return std::prev(currFunction_.quads.end());;
}

std::list<Tac::Quad>::iterator FuncGenerator::lastQuad()
{
    return std::prev(currFunction_.quads.end());
}

Tac::Label FuncGenerator::nextLabel()
{
    return tacGen_.nextLabel();
}

Tac::StackObject FuncGenerator::nextStackObject(uint64_t size, size_t align)
{
    static int stackObjectNo = 1;
    return Tac::StackObject(stackObjectNo++, size, align);
}


void FuncGenerator::visit(FuncDef* node)
{
    node->body_->accept(*this);
}

void FuncGenerator::visit(VarDef* node)
{
    // TODO compute offset
    auto ent = currScope().find(node->name_);
    if (ent->ambiguous) {
        auto size = node->type_->width();
        auto align = Type::alignAt(node->type_);
        auto stackMem = nextStackObject(size, align);
        ent->irBinding = stackMem;
        emit({Tac::Alloca, stackMem});
    } else {
        // struct union array should be in memory
        if (Type::isScalar(ent->type)) {
            ent->irBinding = nextReg();
        } else {
            auto size = node->type_->width();
            auto align = Type::alignAt(node->type_);
            auto stackMem = nextStackObject(size, align);
            ent->irBinding = stackMem;
            emit({Tac::Alloca, stackMem});
        }
    }

    if (node->init_) {
        // use a temporary ast node to emulate initialization
        auto tempAssignExpr =
            std::make_shared<BinaryOpExpr>(node->tok_,
                                       std::make_shared<VarExpr>(node->tok_, node->name_),
                                       node->init_, Token::Assign);
        tempAssignExpr->lhs_->evalType = node->type_;
        tempAssignExpr->rhs_->evalType = node->init_->evalType;
        tempAssignExpr->rhs_->promotedTo = node->init_->promotedTo;
        tempAssignExpr->accept(*this);
    }
}

void FuncGenerator::visit(Block* node)
{
    pushScope(&node->symtab_);
    for (auto& n : node->stmts_) {
        n->accept(*this);
    }
    popScope();
}

void FuncGenerator::visit(UnaryOpExpr* node)
{
    TempStackObjectDeallocGuard guard(*this);

    ValueGenerator vg(*this);
    vg.visit(node);
}

void FuncGenerator::visit(BinaryOpExpr* node)
{
    TempStackObjectDeallocGuard guard(*this);

    auto op = node->operator_;
    switch (op) {
    case Token::Assign: {
        if (Type::isScalar(node->lhs_->evalType)) {
            // ValueGenerator rhsGen(*this);
            LValueGenerator lhsGen(*this);
            // node->rhs_->accept(rhsGen);
            node->lhs_->accept(lhsGen);
            auto width = ExprType(node->lhs_)->width();
            /// 右值和左值之间的类型差异已经由checkAssignE2T在标记promotedTo中指出。
            /// 我们假定这种隐式转换由ValueGenerator妥善解决。
            if (lhsGen.inMemory()) {
                ValueGenerator rhsGen(*this);
                node->rhs_->accept(rhsGen);
                emit({Tac::Storer, rhsGen.value(), Tac::Var::empty, lhsGen.addr(), width});
            } else {
                ValueGenerator rhsGen(*this, lhsGen.addr());
                node->rhs_->accept(rhsGen);
                // emit({Tac::Movrr, rhsGen.value(), Tac::Var::empty, lhsGen.addr()});
            }
        } else {
            /// assignment between variables of compound type
            auto ty = Type::derefIfIsUserDefinedType(node->lhs_->evalType);
            assert(ty->tag() == Type::Compound);
            assert(node->lhs_->evalType->equalUnqual(node->rhs_->evalType));

            LValueGenerator rhsGen(*this);
            LValueGenerator lhsGen(*this);
            node->rhs_->accept(rhsGen);
            node->lhs_->accept(lhsGen);
            assert(lhsGen.inMemory() && rhsGen.inMemory());
            auto compoundTy = static_cast<CompoundType*>(ty.get());
            compoundAssignment(compoundTy, lhsGen.addr(), rhsGen.addr());
        }

        break;
    }
    default: {
        ValueGenerator vg(*this);
        vg.visit(node);
    }
    }
}

void FuncGenerator::compoundAssignment(CompoundType* type, Tac::Reg lhsAddr, Tac::Reg rhsAddr)
{
    /**
      * 还有许多需要做compound类型变量之间复制的操作，在现阶段的ir中无法反映，
      * 所以我认为，目前没有必要将这一操作展开，而应该采用一个指令取代替
      */
    auto wholeSize = type->width();
    /*auto tempReg = nextReg();
    std::size_t pos, offset;
    for (pos = 0, offset = 0;
         pos < wholeSize / PTRSIZE;
         pos += 1, offset += PTRSIZE) {
        emit({Tac::Loadrc, rhsAddr, offset, tempReg});
        emit({Tac::Storerc, tempReg, lhsAddr, offset});
    }

    for ( ; offset < wholeSize; ++offset) {
        emit({Tac::Loadrcu, rhsAddr, offset, tempReg, 1});
        emit({Tac::Storerc, tempReg, lhsAddr, offset, 1});
    }*/
    emit({Tac::Memcpy, rhsAddr, Tac::Var::ImmType(wholeSize), lhsAddr});
}

/// trivial visit functions:
void FuncGenerator::visit(FuncCallExpr* node)
{
    TempStackObjectDeallocGuard guard(*this);

    ValueGenerator vg(*this);
    vg.visit(node);
}

void FuncGenerator::visit(MemberExpr* node)
{
    TempStackObjectDeallocGuard guard(*this);

    ValueGenerator vg(*this);
    vg.visit(node);
}

void FuncGenerator::visit(ArrayRefExpr* node)
{
    TempStackObjectDeallocGuard guard(*this);

    ValueGenerator vg(*this);
    vg.visit(node);
}

void FuncGenerator::visit(VarExpr* node)
{
    TempStackObjectDeallocGuard guard(*this);

    ValueGenerator vg(*this);
    vg.visit(node);
}

void FuncGenerator::visit(CastExpr* node)
{
    TempStackObjectDeallocGuard guard(*this);

    ValueGenerator vg(*this);
    vg.visit(node);
}

void FuncGenerator::visit(LiteralExpr* node)
{
    TempStackObjectDeallocGuard guard(*this);

    ValueGenerator vg(*this);
    vg.visit(node);
}

void FuncGenerator::visit(SizeofExpr* node)
{
    TempStackObjectDeallocGuard guard(*this);

    ValueGenerator vg(*this);
    vg.visit(node);
}

void FuncGenerator::visit(ReturnStmt* node)
{
    TempStackObjectDeallocGuard guard(*this);

    if (Type::isScalar(node->func_->type_->retType())) {
        ValueGenerator vg(*this);
        node->retExpr_->accept(vg);
        emit({Tac::Ret, vg.value()});
    } else {
        auto ty = Type::derefIfIsUserDefinedType(node->func_->type_->retType());
        auto compoundTy = static_cast<CompoundType*>(ty.get());
        assert(ty->tag() == Type::Compound);
        LValueGenerator retGen(*this);
        node->retExpr_->accept(retGen);
        assert(retGen.inMemory());
        auto ptr = nextReg();
        /// 返回非标量是通过在第一个参数的位置传指针实现的
        emit({Tac::GetParamVal, Tac::Var::ImmType(1), Tac::Var::empty, ptr});
        compoundAssignment(compoundTy, ptr, retGen.addr());
    }
}



void BranchGenerator::visit(BinaryOpExpr* node)
{
    assert(node->isCond);
    switch (node->operator_) {
    case Token::And: {
        if (!when_) {
            /// when cond is false goto label else fall through
            // 虽然这两个Generator和*this是一个东西，但是为了清晰，还是创建他们
            BranchGenerator lhs(funcGenerator_, false, goto_);
            BranchGenerator rhs(funcGenerator_, false, goto_);
            node->lhs_->accept(lhs);
            node->rhs_->accept(rhs);
        } else {
            assert(when_);
            auto outLabel = nextLabel();
            BranchGenerator lhs(funcGenerator_, false, outLabel);
            BranchGenerator rhs(funcGenerator_, true, goto_);
            node->lhs_->accept(lhs);
            node->rhs_->accept(rhs);
            emit({Tac::LabelLine, outLabel});
        }
        break;
    }
    case Token::Or: {
        if (!when_) {
            auto outLabel = nextLabel();
            BranchGenerator lhs(funcGenerator_, true, outLabel);
            BranchGenerator rhs(funcGenerator_, false, goto_);
            node->lhs_->accept(lhs);
            node->rhs_->accept(rhs);
            emit({Tac::LabelLine, outLabel});
        } else {
            assert(when_);
            BranchGenerator lhs(funcGenerator_, true, goto_);
            BranchGenerator rhs(funcGenerator_, true, goto_);
            node->lhs_->accept(lhs);
            node->rhs_->accept(rhs);
        }
        break;
    }

    case Token::Grtr:
    case Token::Ge:
    case Token::Smlr:
    case Token::Se:
    case Token::Eq:
    case Token::Ne: {
        ValueGenerator vgl(funcGenerator_);
        ValueGenerator vgr(funcGenerator_);
        node->lhs_->accept(vgl);
        node->rhs_->accept(vgr);
        Tac::Reg lhs = vgl.value(),
                rhs = vgr.value();
        Tac::Opcode op = genJumpInst(node->operator_, node->lhs_, node->rhs_);
        emit({op, lhs, rhs, goto_});
        break;
    }

    default: {
        ValueGenerator vg(funcGenerator_);
        vg.visit(node);
        implicitCond(vg.value(), ExprType(node));
        break;
    }
    }
}

void BranchGenerator::visit(UnaryOpExpr* node)
{
    switch (node->operator_) {
    case Token::Not: {
        BranchGenerator bg(funcGenerator_, !when_, goto_);
        node->operand_->accept(bg);
        break;
    }
    default: {
        ValueGenerator vg(funcGenerator_);
        vg.visit(node);
        implicitCond(vg.value(), ExprType(node));
        break;
    }
    }
}

void BranchGenerator::visit(FuncCallExpr* node)
{
    ValueGenerator vg(funcGenerator_);
    vg.visit(node);
    implicitCond(vg.value(), ExprType(node));
}

void BranchGenerator::visit(MemberExpr* node)
{
    ValueGenerator vg(funcGenerator_);
    vg.visit(node);
    implicitCond(vg.value(), ExprType(node));
}

void BranchGenerator::visit(ArrayRefExpr* node)
{
    ValueGenerator vg(funcGenerator_);
    vg.visit(node);
    implicitCond(vg.value(), ExprType(node));
}

void BranchGenerator::visit(VarExpr* node)
{
    ValueGenerator vg(funcGenerator_);
    vg.visit(node);
    implicitCond(vg.value(), ExprType(node));
}

void BranchGenerator::visit(CastExpr* node)
{
    ValueGenerator vg(funcGenerator_);
    vg.visit(node);
    implicitCond(vg.value(), ExprType(node));
}

void BranchGenerator::visit(LiteralExpr* node)
{
    ValueGenerator vg(funcGenerator_);
    vg.visit(node);
    implicitCond(vg.value(), ExprType(node));
}

void BranchGenerator::visit(SizeofExpr* node)
{
    ValueGenerator vg(funcGenerator_);
    vg.visit(node);
    implicitCond(vg.value(), ExprType(node));
}


Tac::Opcode
BranchGenerator::genJumpInst(Token::OperatorType op,
                             const std::shared_ptr<Expr>& lhs, const std::shared_ptr<Expr>& rhs)
{
    auto& lhsTy = ExprType(lhs);
    auto& rhsTy = ExprType(rhs);
    auto sign = false;
    if (Type::isFloating(lhsTy)) {
        assert(Type::isFloating(rhsTy));
        assert(false && "floating operations are not implemented");
    }
    if (Type::isInteger(lhsTy)) {
        assert(Type::isInteger(rhsTy));
        sign = not static_cast<BuiltInType*>(lhsTy.get())->isUnsigned();
        assert(sign == !static_cast<BuiltInType*>(rhsTy.get())->isUnsigned());
    }

    Tac::Opcode ret;
    switch (op) {
    case Token::Grtr:
        if (when_ == true) {
            if (sign) {
                return Tac::Jgs;
            } else {
                return Tac::Jgu;
            }
        } else {
            if (sign) {
                return Tac::Jles;
            } else {
                return Tac::Jleu;
            }
        }
    case Token::Smlr:
        if (when_ == true) {
            if (sign) {
                return Tac::Jls;
            } else {
                return Tac::Jlu;
            }
        } else {
            if (sign) {
                return Tac::Jges;
            } else {
                return Tac::Jgeu;
            }
        }
    case Token::Ge:
        if (when_ == true) {
            if (sign) {
                return Tac::Jgeu;
            } else {
                return Tac::Jges;
            }
        } else {
            if (sign) {
                return Tac::Jls;
            } else {
                return Tac::Jlu;
            }
        }
    case Token::Se:
        if (when_ == true) {
            if (sign) {
                return Tac::Jles;
            } else {
                return Tac::Jleu;
            }
        } else {
            if (sign) {
                return Tac::Jgs;
            } else {
                return Tac::Jgu;
            }
        }
    case Token::Eq:
        if (when_ == true) {
            return Tac::Jeq;
        } else {
            return Tac::Jne;
        }
    case Token::Ne:
        if (when_ == true) {
            return Tac::Jne;
        } else {
            return Tac::Jeq;
        }
    default:
        assert(false);
    }

    assert(false);
}

void BranchGenerator::implicitCond(Tac::Reg val, const std::shared_ptr<Type>& ty)
{
    Tac::Reg zero = nextReg();
    emit({Tac::Loadi, Tac::Var::ImmType(0), Tac::Var::empty, zero}); // load immidiate 0
    if (Type::isInteger(ty) || Type::isPointer(ty)) {
        if (when_) {
            emit({Tac::Jne, val, zero, goto_});
        } else {
            emit({Tac::Jeq, val, zero, goto_});
        }
    } else {
        assert(false && "not implemented");
    }
}


Tac::Reg ValueGenerator::cast(Tac::Reg reg,
                              const std::shared_ptr<Type>& from, const std::shared_ptr<Type>& to,
                              bool inplace)
{
    /** return reg; is a kind of optimization of:
      * auto dest = nextReg();
      * emit({Tac::Movrr, reg, Tac::Var::empty, dest});
      * return dest; """
      */
    if (from->equalUnqual(to)) {
        return reg;
    }

    if (Type::isArithmetic(from) && Type::isArithmetic(to)) {
        auto fromTy = static_cast<BuiltInType*>(from.get());
        auto toTy = static_cast<BuiltInType*>(to.get());
        if (fromTy->isInteger() && toTy->isInteger()) {
            if (toTy->width() == PTRSIZE) {
                return reg;
            }
            Tac::Reg dest;
            if (inplace)
                dest = reg;
            else
                dest = regToWrite();
            auto op = toTy->isUnsigned() ? Tac::Extu : Tac::Exts;
            emit({op, reg, Tac::Var::empty, dest, toTy->width()});
            return dest;
        } else {
            //TODO
            assert(false && "floating number not implemented");
        }
    } else if (Type::isPointer(from) && Type::isInteger(to)) {
        auto fromTy = static_cast<PointerType*>(from.get());
        auto toTy = static_cast<BuiltInType*>(to.get());
        if (toTy->width() == PTRSIZE) {
            return reg;
        } else {
            Tac::Reg dest;
            if (inplace)
                dest = reg;
            else
                dest = regToWrite();
            auto op = toTy->isUnsigned() ? Tac::Extu : Tac::Exts;
            emit({op, reg, Tac::Var::empty, dest, toTy->width()});
            return dest;
        }
    } else if (Type::isInteger(from) && Type::isPointer(to)) {
        return reg;
    } else if (Type::isPointer(from) && Type::isPointer(to)) {
        return reg;
    } else if (from->tag() == Type::Array && Type::isPointer(to)) {
        return reg;
    } else {
        assert(false);
    }
}

void ValueGenerator::visit(UnaryOpExpr* node)
{
    auto op = node->operator_;
    if (op == Token::BitAnd) {
        /// & 取地址
        if (Type::isArray(node->operand_->evalType)) {
            assert(!node->operand_->promotedTo);
            ValueGenerator vg(funcGenerator_);
            node->operand_->accept(vg);
            reg_ = vg.value();
        } else {
            LValueGenerator lg(funcGenerator_);
            node->operand_->accept(lg);
            assert(lg.inMemory());
            reg_ = lg.addr();
        }
    } else if (op == Token::Not) {
        auto falseLabel = nextLabel();
        BranchGenerator bg(funcGenerator_, false, falseLabel);
        bg.visit(node);
        reg_ = regToWrite();
        auto outLabel = nextLabel();
        emit({Tac::Loadi, Tac::Var::ImmType(1), Tac::Var::empty, reg_});
        emit({Tac::Jmp, outLabel});
        emit({Tac::LabelLine, falseLabel});
        emit({Tac::Loadi, Tac::Var::ImmType(0), Tac::Var::empty, reg_});
        emit({Tac::LabelLine, outLabel});
    } else {
        ValueGenerator opndGen(funcGenerator_);
        node->operand_->accept(opndGen);
        auto res = opndGen.value();

        switch (op) {
        case Token::Add:
            reg_ = res;
            break;
        case Token::Sub: {
            reg_ = regToWrite();
            Tac::Reg zero = nextReg();
            emit({Tac::Loadi, Tac::Var::ImmType(0), Tac::Var::empty, zero}); // load immidiate 0
            emit({Tac::Sub, zero, res, reg_});
            break;
        }
        case Token::BitInv:
            reg_ = regToWrite();
            emit({Tac::BInv, res, Tac::Var::empty, reg_});
            break;

        case Token::Mult: {
            /// * dereference
            assert(Type::isPointer(node->operand_->evalType));
            auto ptrTy = static_cast<PointerType*>(node->operand_->evalType.get());
            if (Type::isArray(ptrTy->base())) {
                /// 数组的地址即是数组名的值
                reg_ = res;
            } else {
                auto width = node->evalType->width();
                reg_ = regToWrite();
                Tac::Opcode inst = properLoadInst(node->evalType, Tac::Loadr);
                emit({inst, res, Tac::Var::empty, reg_, width});
            }

            break;
        }
        default:
            assert(false);
        }
    }

    if (node->promotedTo) {
        // need cast
        reg_ = cast(reg_, node->evalType, node->promotedTo, true);
    }

}

void ValueGenerator::visit(BinaryOpExpr* node)
{
    if (Type::isFloating(ExprType(node->lhs_))) {
        assert(false && "floating number not implemented");
    }
    
    auto op = node->operator_;
    /// not assignment:
    assert((int)op < (int)Token::Assign || (int)op > (int)Token::SftRAssign);
    if ((int)Token::Eq <= (int)op && (int)op <= (int)Token::Or) {
        auto falseLabel = nextLabel();
        BranchGenerator bg(funcGenerator_, false, falseLabel);
        bg.visit(node);
        reg_ = regToWrite();
        auto outLabel = nextLabel();
        emit({Tac::Loadi, Tac::Var::ImmType(1), Tac::Var::empty, reg_});
        emit({Tac::Jmp, outLabel});
        emit({Tac::LabelLine, falseLabel});
        emit({Tac::Loadi, Tac::Var::ImmType(0), Tac::Var::empty, reg_});
        emit({Tac::LabelLine, outLabel});
    } else {
        ValueGenerator lhsGen(funcGenerator_),
                rhsGen(funcGenerator_);
        node->lhs_->accept(lhsGen);
        node->rhs_->accept(rhsGen);
        auto lhs = lhsGen.value();
        auto rhs = rhsGen.value();
        reg_ = regToWrite();
        auto inst = Tac::Nop;
        switch(op) {
        case Token::Add:
            inst = Tac::Add;
            break;
        case Token::Sub:
            inst = Tac::Sub;
            break;
        case Token::Mult:
            inst = Tac::Mul;
            break;
        case Token::Div: {
            auto p = static_cast<BuiltInType*>(ExprType(node->lhs_).get())->isUnsigned();
            inst = p ? Tac::Divu : Tac::Divs;
            break;
        }
        case Token::Mod: {
            auto p = static_cast<BuiltInType*>(ExprType(node->lhs_).get())->isUnsigned();
            inst = p ? Tac::Modu : Tac::Mods;
            break;
        }
        case Token::SftL:
            inst = Tac::Shl;
            break;
        case Token::SftR: {
            auto p = static_cast<BuiltInType*>(ExprType(node->lhs_).get())->isUnsigned();
            inst = p ? Tac::Shrl : Tac::Shra;
            break;
        }
        case Token::BitAnd:
            inst = Tac::BAnd;
            break;
        case Token::BitXor:
            inst = Tac::BXor;
            break;
        case Token::BitOr:
            inst = Tac::BOr;
            break;
        default:
            assert(false);
        }

        auto opnd1 = lhs;
        auto opnd2 = rhs;
        if (op == Token::Add || op == Token::Sub) {
            if (Type::isPointer(node->lhs_->evalType)) {
                assert(Type::isInteger(ExprType(node->rhs_)));
                auto timed = nextReg();
                emit({Tac::Mul,
                      rhs,
                      Tac::Var::ImmType(
                              static_cast<PointerType*>(node->lhs_->evalType.get())->base()->width()),
                      timed});
                opnd2 = timed;
            } else if (Type::isPointer(node->rhs_->evalType)) {
                assert(Type::isInteger(ExprType(node->lhs_)));
                auto timed = nextReg();
                emit({Tac::Mul,
                      lhs,
                      Tac::Var::ImmType(
                              static_cast<PointerType*>(node->rhs_->evalType.get())->base()->width()),
                      timed});
                opnd1 = timed;
            }
        }

        emit({inst, opnd1, opnd2, reg_});
    }

    if (node->promotedTo) {
        // need cast
        reg_ = cast(reg_, node->evalType, node->promotedTo, true);
    }
}

void ValueGenerator::visit(MemberExpr* node)
{
    LValueGenerator addrGen(funcGenerator_);
    node->compound_->accept(addrGen);
    assert(addrGen.inMemory());
    auto base = addrGen.addr();

    assert(node->compound_->evalType->tag() == Type::UserDefined);
    auto ty = static_cast<CompoundType*>(
            Type::derefIfIsUserDefinedType(node->compound_->evalType).get());
    auto memberEnt = ty->members().find(node->memberName_);
    auto offset = memberEnt->offset;

    reg_ = regToWrite();
    if (node->evalType->tag() == Type::Array) {
        /// 对数组，值即其地址
        emit({Tac::Add, base, Tac::Var::ImmType(offset), reg_});
    } else {
        assert(Type::isScalar(node->evalType));
        auto width = node->evalType->width();
        Tac::Opcode inst = properLoadInst(node->evalType, Tac::Loadrc);
        emit({inst, base, Tac::Var::ImmType(offset), reg_, width});
    }

    if (node->promotedTo) {
        // need cast
        reg_ = cast(reg_, node->evalType, node->promotedTo, true);
    }
}

void ValueGenerator::visit(ArrayRefExpr* node)
{
    assert(not node->head_->promotedTo);

    ValueGenerator idxGen(funcGenerator_);
    node->index_->accept(idxGen);
    auto index = idxGen.value();

    reg_ = regToWrite();
    if (node->head_->evalType->tag() == Type::Pointer) {
        ValueGenerator vg(funcGenerator_);
        node->head_->accept(vg);
        auto ptrType = static_cast<PointerType*>(node->head_->evalType.get());
        auto width = ptrType->base()->width();
        auto offset = nextReg();
        emit({Tac::Mul, index, Tac::Var::ImmType(width), offset});
        if (ptrType->base()->tag() == Type::Array) {
            emit({Tac::Add, vg.value(), offset, reg_});
        } else {
            auto addr = nextReg();
            emit({Tac::Add, vg.value(), offset, addr});
            emit({properLoadInst(node->evalType, Tac::Loadr),
                  addr, Tac::Var::empty, reg_});
        }
    } else {
        assert(node->head_->evalType->tag() == Type::Array);
        ValueGenerator vg(funcGenerator_);
        node->head_->accept(vg);
        auto arrType = static_cast<ArrayType*>(node->head_->evalType.get());
        auto width = arrType->base()->width();
        auto offset = nextReg();
        emit({Tac::Mul, index, Tac::Var::ImmType(width), offset});
        if (arrType->base()->tag() == Type::Array) {
            emit({Tac::Add, vg.value(), offset, reg_});
        } else {
            auto addr = nextReg();
            emit({Tac::Add, vg.value(), offset, addr});
            emit({properLoadInst(node->evalType, Tac::Loadr),
                  addr, Tac::Var::empty, reg_});
        }
    }

    if (node->promotedTo) {
        // need cast
        reg_ = cast(reg_, node->evalType, node->promotedTo, true);
    }
}

void ValueGenerator::visit(VarExpr* node)
{
    /// assume scalar type or array
    bool inplace = true;
    auto varEnt = currScope().find(node->varName());
    auto& type = varEnt->type;
    if (Type::isArray(type)) {
        // array should be in memory
        reg_ = regToWrite();
        if (varEnt->level == 0) {
            /// global variable
            emit({Tac::LoadGlobalPtr, Tac::Var::VarLabel{node->varName()}, Tac::Var::empty, reg_});
        } else {
            /// auto variable
            emit({Tac::LoadVarPtr, std::get<Tac::StackObject>(varEnt->irBinding),
                  Tac::Var::empty, reg_});
        }
    } else {
        assert(Type::isScalar(type));
        if (varEnt->isParam) {
            reg_ = regToWrite();
            int seq = paramSeq(varEnt, funcGenerator_.func().retType);
            emit({Tac::GetParamVal, Tac::Var::ImmType(seq), Tac::Var::empty, reg_});
        } else {
            if (varEnt->level == 0) {
                /// global variable
                reg_ = regToWrite();
                emit({Tac::LoadGlobalPtr, Tac::Var::VarLabel{node->varName()}, Tac::Var::empty, reg_});
                emit({properLoadInst(varEnt->type, Tac::Loadr), reg_,
                      Tac::Var::empty, reg_});
            } else {
                if (varEnt->ambiguous) {
                    reg_ = regToWrite();
                    emit({Tac::LoadVarPtr, std::get<Tac::StackObject>(varEnt->irBinding),
                          Tac::Var::empty, reg_});
                    emit({properLoadInst(node->evalType, Tac::Loadr),
                          reg_, Tac::Var::empty, reg_});
                } else {
                    reg_ = std::get<Tac::Reg>(varEnt->irBinding);
                    inplace = false;
                }
            }

        }
    }

    if (node->promotedTo) {
        // need cast
        /// reg_ may be bound to a variable,
        /// in this case the following call should be done with inplace = false
        reg_ = cast(reg_, node->evalType, node->promotedTo, inplace);
    }
}

void ValueGenerator::visit(CastExpr* node)
{
    ValueGenerator casteeGen(funcGenerator_);
    node->expr_->accept(casteeGen);
    auto val = casteeGen.value();
    // allocation of new register is in function "cast"
    /// 这里为了偷懒，没有处理一些本来可以inplace = true的情况
    reg_ = cast(val, ExprType(node->expr_), node->castTo_, false);

    if (node->promotedTo) {
        // need cast
        reg_ = cast(reg_, node->evalType, node->promotedTo, true);
    }
}

void ValueGenerator::visit(SizeofExpr* node)
{
    reg_ = regToWrite();
    emit({Tac::Loadiu, Tac::Var::ImmType(node->type_->width()), Tac::Var::empty, reg_});

    if (node->promotedTo) {
        // need cast
        reg_ = cast(reg_, node->evalType, node->promotedTo, true);
    }
}

void ValueGenerator::visit(LiteralExpr* node)
{
    auto literalTok = node->tok_;
    auto literalType = literalTok->type();
    int64_t imm;
    if (Type::isInteger(node->evalType)) {
        switch (literalType) {
        case Token::IntLiteral:
            imm = literalTok->intLiteral();
            break;
        case Token::UnsignedLiteral:
            imm = literalTok->uintLiteral();
            break;
        case Token::CharLiteral:
            imm = literalTok->charLiteral();
            break;
        default:
            assert(false && "other literal types are not implemented");
        }
        reg_ = regToWrite();
        emit({Tac::Loadi, Tac::Var::ImmType(imm), Tac::Var::empty, reg_});

    } else if (Type::isFloating(node->evalType)) {
        assert(false && "floating literal types are not implemented");
    } else if (literalType == Token::StringLiteral) {
        assert(node->evalType->equal(PointerType::strLiteralType()));
        /// value of char* is the address it point to
        int n = addLiteral(Tac::StaticObject(literalTok->stringLiteral()));
        reg_ = regToWrite();
        emit({Tac::LoadConstantPtr, Tac::Var::ImmType(n), Tac::Var::empty, reg_});
    } else {
        assert(false);
    }

    if (node->promotedTo) {
        // need cast
        reg_ = cast(reg_, node->evalType, node->promotedTo, true);
    }
}

void ValueGenerator::visit(FuncCallExpr* node)
{
    auto funcEnt = currScope().find(node->funcName_);

    auto argNum = node->args_.size();
    Tac::ArgPassingSpec args;

    auto scalarRet = Type::isScalar(node->evalType);
    if (!scalarRet && !Type::isVoid(node->evalType)) {
        /// return type is struct or union
        reg_ = regToWrite();
        auto ty = Type::derefIfIsUserDefinedType(node->evalType);
        assert(ty->tag() == Type::Compound);
        auto compoundTy = static_cast<CompoundType*>(ty.get());
        auto width = compoundTy->width();

        /// and let addr be the first parameter
        auto stackMem = nextStackObject(compoundTy->width(), compoundTy->alignAt());
        pushTempStackObject(stackMem);
        emit({Tac::Alloca, stackMem, Tac::Var::empty, reg_});
        args.push_back(Tac::ArgInfo{reg_, Tac::ArgInfo::Value, PTRSIZE, PTRSIZE, true});
    }

    for (auto& arg: node->args_) {
        auto type = ExprType(arg);
        if (Type::isScalar(type)){
            ValueGenerator valGen(funcGenerator_);
            arg->accept(valGen);
            args.push_back({valGen.value(), Tac::ArgInfo::Value, type->width(), Type::alignAt(type), true});
        } else if (Type::isArray(type)) {
            ValueGenerator valGen(funcGenerator_);
            arg->accept(valGen);
            args.push_back({valGen.value(), Tac::ArgInfo::Value, PTRSIZE, PTRSIZE, true});
        } else {
            // is struct or union. pass struct or union by value.
            LValueGenerator ptrGen(funcGenerator_);
            arg->accept(ptrGen);
            args.push_back({ptrGen.addr(), Tac::ArgInfo::Ptr, type->width(), Type::alignAt(type), false});
        }
    }

    if (scalarRet) {
        reg_ = regToWrite();
        emit({Tac::Call, Tac::Var::FuncLabel{node->funcName_}, Tac::Var::empty, reg_});
    } else {
        // return value is compound type or void.
        emit({Tac::Call, Tac::Var::FuncLabel{node->funcName_}});
    }
    lastQuad()->setPassingSpec(std::move(args));
}

ValueGenerator::ValueGenerator(FuncGenerator& t, Tac::Reg aim)
    : FuncUtil(t), target_(aim)
{

}

void LValueGenerator::visit(ArrayRefExpr* node)
{
    assert(not node->head_->promotedTo);

    ValueGenerator idxGen(funcGenerator_);
    node->index_->accept(idxGen);
    auto index = idxGen.value();

    if (node->head_->evalType->tag() == Type::Pointer) {
        // get pointer value
        ValueGenerator vg(funcGenerator_);
        node->head_->accept(vg);
        auto width = static_cast<PointerType*>(node->head_->evalType.get())->base()->width();
        auto offset = nextReg();
        addr_ = nextReg();
        emit({Tac::Mul, index, Tac::Var::ImmType(width), offset});
        emit({Tac::Add, vg.value(), offset, addr_});
    } else {
        assert(node->head_->evalType->tag() == Type::Array);
        ValueGenerator vg(funcGenerator_);
        node->head_->accept(vg);
        auto width = static_cast<ArrayType*>(node->head_->evalType.get())->base()->width();
        auto offset = nextReg();
        addr_ = nextReg();
        emit({Tac::Mul, index, Tac::Var::ImmType(width), offset});
        emit({Tac::Add, vg.value(), offset, addr_});
    }
}

void LValueGenerator::visit(VarExpr* node)
{
    auto ent = currScope().find(node->varName());
    if (ent->isParam) {
        /// 如果标量，而且没有被取地址，但是还是调用了LValueGenerator，
        /// 真相只有一个，那就是该变量作为赋值的左边
        /// 怕麻烦，暂时用这种愚蠢的方法解决：即强制使其进入内存
        ent->ambiguous = true;
        int seq = paramSeq(ent, funcGenerator_.func().retType);
        addr_ = nextReg();
        emit({Tac::GetParamPtr, Tac::Var::ImmType(seq), Tac::Var::empty, addr_});

    } else {
        if (ent->level == 0) {
            /// global variable
            addr_ = nextReg();
            emit({Tac::LoadGlobalPtr, Tac::Var::VarLabel{node->varName()}, Tac::Var::empty, addr_});
        } else {
            if (ent->ambiguous || !Type::isScalar(ent->type)) {
                addr_ = nextReg();
                emit({Tac::LoadVarPtr, std::get<Tac::StackObject>(ent->irBinding), Tac::Var::empty, addr_});
            } else {
                inMemory_ = false;
                addr_ = std::get<Tac::Reg>(ent->irBinding);
            }
        }
    }
}

void LValueGenerator::visit(MemberExpr* node)
{
    LValueGenerator baseGen(funcGenerator_);
    node->compound_->accept(baseGen);
    auto base = baseGen.addr(); assert(baseGen.inMemory());
    assert(node->compound_->evalType->tag() == Type::UserDefined);
    auto ty = static_cast<CompoundType*>(
            Type::derefIfIsUserDefinedType(node->compound_->evalType).get());
    auto memberEnt = ty->members().find(node->memberName_);
    auto offset = memberEnt->offset;

    addr_ = nextReg();
    emit({Tac::Add, base, Tac::Var::ImmType(offset), addr_});
}

void LValueGenerator::visit(UnaryOpExpr* node)
{
    switch (node->operator_) {
    case Token::Mult: {
        /// *
        ValueGenerator ptrValGen(funcGenerator_);
        node->operand_->accept(ptrValGen);
        addr_ = ptrValGen.value();
        break;
    }
    default:
        assert(false && "not a lvalue");
    }
}

void LValueGenerator::visit(FuncCallExpr* node)
{
    assert(!Type::isScalar(node->evalType));
    ValueGenerator vg(funcGenerator_);
    vg.visit(node);
    addr_ = vg.value();
}