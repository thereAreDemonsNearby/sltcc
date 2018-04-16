#include "tacgenerator.h"
#include <iterator>


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
    FuncGenerator funcGen(*this, node->name_, &node->params_);
    funcGen.visit(node);
    ir_.funcs.push_back(std::move(funcGen.func()));
}

void TacGenerator::visit(VarDef* node)
{
    // TODO : collect global variables
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
    node->init_->accept(*this);
    emit({Tac::LabelLine, beginLabel});
    node->cond_->accept(bg);
    node->body_->accept(*this);
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
FuncGenerator::emit(const Tac::Quad& quad)
{
    currFunction_.quads.push_back(quad);
    return std::prev(currFunction_.quads.end());;
}

std::list<Tac::Quad>::iterator FuncGenerator::lastQuad()
{
    return std::prev(currFunction_.quads.end());
}

Tac::Label FuncGenerator::nextLabel()
{
    // count from 1
    static int labelNo = 1;
    int ret = labelNo++;
    return Tac::Label(ret);
}

void FuncGenerator::visit(FuncDef* node)
{
    // TODO see the parameters
    node->body_->accept(*this);
}

void FuncGenerator::visit(VarDef* node)
{
    // TODO compute offset
    auto ent = currScope().find(node->name_);
    if (ent->ambiguous) {
        currFunction_.local.push_back(ent);
    } else {
        // struct union array should be in memory
        auto tag = Type::derefIfUserDefined(ent->type)->tag();
        if (tag == Type::Array) {
            currFunction_.local.push_back(ent);
        } else if (tag == Type::Compound) {
            currFunction_.local.push_back(ent);
        } else {
            /// bind a unambiguous scalar type to a register
            ent->boundTo = nextReg();
        }
    }

    if (node->init_) {
        // use a temporary ast node to emulate initialization
        auto tempAssignExpr =
            std::make_shared<BinaryOpExpr>(node->tok_,
                                       std::make_shared<VarExpr>(node->tok_, node->name_),
                                       node->init_, Token::Assign);
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
    ValueGenerator vg(*this);
    vg.visit(node);
}

void FuncGenerator::visit(BinaryOpExpr* node)
{
    auto op = node->operator_;
    switch (op) {
    case Token::Assign: {
        // TODO struct type assignment
        ValueGenerator rhsGen(*this);
        LValueGenerator lhsGen(*this);
        node->rhs_->accept(rhsGen);
        node->lhs_->accept(lhsGen);
        auto width = ExprType(node->lhs_)->width();
        /// 右值和左值之间的类型差异已经由checkAssignE2T在标记promotedTo中指出。
        /// 我们假定这种隐式转换由ValueGenerator妥善解决。
        if (lhsGen.inMemory()) {
            emit({Tac::Storer, rhsGen.value(), lhsGen.addr(), Tac::Var::empty, width});
        } else {
            emit({Tac::Movrr, rhsGen.value(), Tac::Var::empty, lhsGen.addr()});
        }
        break;
    }
    /* TODO compound assignment */
    default: {
        ValueGenerator vg(*this);
        vg.visit(node);
    }
    }
}


/// trivial visit functions:
void FuncGenerator::visit(FuncCall* node)
{
    ValueGenerator vg(*this);
    vg.visit(node);
}

void FuncGenerator::visit(MemberExpr* node)
{
    ValueGenerator vg(*this);
    vg.visit(node);
}

void FuncGenerator::visit(ArrayRefExpr* node)
{
    ValueGenerator vg(*this);
    vg.visit(node);
}

void FuncGenerator::visit(VarExpr* node)
{
    ValueGenerator vg(*this);
    vg.visit(node);
}

void FuncGenerator::visit(CastExpr* node)
{
    ValueGenerator vg(*this);
    vg.visit(node);
}

void FuncGenerator::visit(LiteralExpr* node)
{
    ValueGenerator vg(*this);
    vg.visit(node);
}

void FuncGenerator::visit(SizeofExpr* node)
{
    ValueGenerator vg(*this);
    vg.visit(node);
}

void FuncGenerator::visit(ReturnStmt* node)
{
    // TODO  struct type
    ValueGenerator vg(*this);
    node->ret_->accept(vg);
    emit({Tac::RetVal, vg.value()});
}


void BranchGenerator::visit(BinaryOpExpr* node)
{
    assert(node->isCond);
    switch (node->operator_) {
    case Token::And: {
        if (!when_) {
            /// when cond is false goto label else fall through
            // 虽然这两个Generator和*this是一个东西，但是为了清晰，还是创建他们
            BranchGenerator lhs(mainGenerator_, false, goto_);
            BranchGenerator rhs(mainGenerator_, false, goto_);
            node->lhs_->accept(lhs);
            node->rhs_->accept(rhs);
        } else {
            assert(when_);
            auto outLabel = nextLabel();
            BranchGenerator lhs(mainGenerator_, false, outLabel);
            BranchGenerator rhs(mainGenerator_, true, goto_);
            node->lhs_->accept(lhs);
            node->rhs_->accept(rhs);
            emit({Tac::LabelLine, outLabel});
        }
        break;
    }
    case Token::Or: {
        if (!when_) {
            auto outLabel = nextLabel();
            BranchGenerator lhs(mainGenerator_, true, outLabel);
            BranchGenerator rhs(mainGenerator_, false, goto_);
            node->lhs_->accept(lhs);
            node->rhs_->accept(rhs);
            emit({Tac::LabelLine, outLabel});
        } else {
            assert(when_);
            BranchGenerator lhs(mainGenerator_, true, goto_);
            BranchGenerator rhs(mainGenerator_, true, goto_);
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
        ValueGenerator vgl(mainGenerator_);
        ValueGenerator vgr(mainGenerator_);
        node->lhs_->accept(vgl);
        node->rhs_->accept(vgr);
        Tac::Reg lhs = vgl.value(),
                rhs = vgr.value();
        Tac::Opcode op = comparator(node->operator_, node->lhs_, node->rhs_);
        emit({op, lhs, rhs, goto_});
        break;
    }

    default: {
        ValueGenerator vg(mainGenerator_);
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
        BranchGenerator bg(mainGenerator_, !when_, goto_);
        node->operand_->accept(bg);
        break;
    }
    default: {
        ValueGenerator vg(mainGenerator_);
        vg.visit(node);
        implicitCond(vg.value(), ExprType(node));
        break;
    }
    }
}

void BranchGenerator::visit(FuncCall* node)
{
    ValueGenerator vg(mainGenerator_);
    vg.visit(node);
    implicitCond(vg.value(), ExprType(node));
}

void BranchGenerator::visit(MemberExpr* node)
{
    ValueGenerator vg(mainGenerator_);
    vg.visit(node);
    implicitCond(vg.value(), ExprType(node));
}

void BranchGenerator::visit(ArrayRefExpr* node)
{
    ValueGenerator vg(mainGenerator_);
    vg.visit(node);
    implicitCond(vg.value(), ExprType(node));
}

void BranchGenerator::visit(VarExpr* node)
{
    ValueGenerator vg(mainGenerator_);
    vg.visit(node);
    implicitCond(vg.value(), ExprType(node));
}

void BranchGenerator::visit(CastExpr* node)
{
    ValueGenerator vg(mainGenerator_);
    vg.visit(node);
    implicitCond(vg.value(), ExprType(node));
}

void BranchGenerator::visit(LiteralExpr* node)
{
    ValueGenerator vg(mainGenerator_);
    vg.visit(node);
    implicitCond(vg.value(), ExprType(node));
}

void BranchGenerator::visit(SizeofExpr* node)
{
    ValueGenerator vg(mainGenerator_);
    vg.visit(node);
    implicitCond(vg.value(), ExprType(node));
}


Tac::Opcode
BranchGenerator::comparator(Token::OperatorType op,
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
    emit({Tac::Loadi, 0L, Tac::Var::empty, zero}); // load immidiate 0
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
    /** return reg; is a kind of optimization of
      * auto dest = nextReg();
      * emit({Tac::Movrr, reg, Tac::Var::empty, dest});
      * return dest;
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
                dest = nextReg();
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
                dest = nextReg();
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
        // &
        if (node->operand_->evalType->tag() == Type::Array) {
            assert(!node->operand_->promotedTo);
            ValueGenerator vg(mainGenerator_);
            node->operand_->accept(vg);
            reg_ = vg.value();
        } else {
            LValueGenerator lg(mainGenerator_);
            node->operand_->accept(lg);
            assert(lg.inMemory());
            reg_ = lg.addr();
        }
    } else if (op == Token::Not) {
        auto falseLabel = nextLabel();
        BranchGenerator bg(mainGenerator_, false, falseLabel);
        bg.visit(node);
        reg_ = nextReg();
        auto outLabel = nextLabel();
        emit({Tac::Loadi, 1, Tac::Var::empty, reg_});
        emit({Tac::Jmp, outLabel});
        emit({Tac::LabelLine, falseLabel});
        emit({Tac::Loadi, 0L, Tac::Var::empty, reg_});
        emit({Tac::LabelLine, outLabel});
    } else {
        ValueGenerator opndGen(mainGenerator_);
        node->operand_->accept(opndGen);
        auto res = opndGen.value();

        switch (op) {
        case Token::Add:
            reg_ = res;
            break;
        case Token::Sub: {
            reg_ = nextReg();
            Tac::Reg zero = nextReg();
            emit({Tac::Loadi, 0L, Tac::Var::empty, zero}); // load immidiate 0
            emit({Tac::Sub, zero, res, reg_});
            break;
        }
        case Token::BitInv:
            reg_ = nextReg();
            emit({Tac::BInv, res, Tac::Var::empty, reg_});
            break;

        case Token::Mult: {
            /// *
            // TODO struct type loading
            assert(Type::isPointer(node->operand_->evalType));
            auto ptrTy = static_cast<PointerType*>(node->operand_->evalType.get());
            if (ptrTy->base()->tag() == Type::Array) {
                reg_ = res;
            } else {
                reg_ = nextReg();
                emit({Tac::Loadr, res, Tac::Var::empty, reg_});
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
        BranchGenerator bg(mainGenerator_, false, falseLabel);
        bg.visit(node);
        reg_ = nextReg();
        auto outLabel = nextLabel();
        emit({Tac::Loadi, 1, Tac::Var::empty, reg_});
        emit({Tac::Jmp, outLabel});
        emit({Tac::LabelLine, falseLabel});
        emit({Tac::Loadi, (int64_t)0, Tac::Var::empty, reg_});
        emit({Tac::LabelLine, outLabel});
    } else {
        ValueGenerator lhsGen(mainGenerator_),
                rhsGen(mainGenerator_);
        node->lhs_->accept(lhsGen);
        node->rhs_->accept(rhsGen);
        auto lhs = lhsGen.value();
        auto rhs = rhsGen.value();
        reg_ = nextReg();
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
                      static_cast<PointerType*>(node->lhs_->evalType.get())->base()->width(),
                      timed});
                opnd2 = timed;
            } else if (Type::isPointer(node->rhs_->evalType)) {
                assert(Type::isInteger(ExprType(node->lhs_)));
                auto timed = nextReg();
                emit({Tac::Mul,
                      lhs,
                      static_cast<PointerType*>(node->rhs_->evalType.get())->base()->width(),
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
    LValueGenerator addrGen(mainGenerator_);
    node->compound_->accept(addrGen);
    assert(addrGen.inMemory());
    auto base = addrGen.addr();

    assert(node->compound_->evalType->tag() == Type::UserDefined);
    auto ty = static_cast<CompoundType*>(
                Type::derefIfUserDefined(node->compound_->evalType).get());
    auto memberEnt = ty->members().find(node->memberName_);
    auto offset = memberEnt->offset;

    emit({Tac::Loadrc, base, offset, reg_});
    if (node->promotedTo) {
        // need cast
        reg_ = cast(reg_, node->evalType, node->promotedTo, true);
    }
}

void ValueGenerator::visit(ArrayRefExpr* node)
{
    assert(not node->head_->promotedTo);

    ValueGenerator idxGen(mainGenerator_);
    node->index_->accept(idxGen);
    auto index = idxGen.value();

    if (node->head_->evalType->tag() == Type::Pointer) {
        ValueGenerator vg(mainGenerator_);
        node->head_->accept(vg);
        auto ptrType = static_cast<PointerType*>(node->head_->evalType);
        auto biase = ptrType->base()->width();
        auto offset = nextReg();
        reg_ = nextReg();
        emit({Tac::Mul, index, biase, offset});
        if (ptrType->base()->tag() == Type::Array) {
            emit({Tac::Add, vg.value(), offset, reg_});
        } else {
            emit({Tac::Loadrr, vg.value(), offset, reg_});
        }
    } else {
        assert(node->head_->evalType->tag() == Type::Array);
        ValueGenerator vg(mainGenerator_);
        node->head_->accept(vg);
        auto arrType = static_cast<ArrayType*>(node->head_->evalType);
        auto biase = arrType->base()->width();
        auto offset = nextReg();
        reg_ = nextReg();
        emit({Tac::Mul, index, biase, offset});
        if (arrType->base()->tag() == Type::Array) {
            emit({Tac::Add, vg.value(), offset, reg_});
        } else {
            emit({Tac::Loadrr, vg.value(), offset, reg_});
        }
    }
}

void ValueGenerator::visit(VarExpr* node)
{
    /// assume scalar type
    bool inplace = false;
    auto varEnt = currScope().find(node->varName());
    auto& type = varEnt->type;
    if (type->tag() == Type::Array) {
        // array should be in memory
        reg_ = nextReg();
        emit({Tac::Loadi, varEnt, Tac::Var::empty, reg_});
        inplace = true;
    } else {
        assert(Type::isArithmetic(type) || Type::isPointer(type));
        if (varEnt->ambiguous) {
            reg_ = nextReg();
            emit({Tac::Loadr, varEnt, Tac::Var::empty, reg_});
            inplace = true;
        } else {
            reg_ = varEnt->boundTo;
        }
    }

    if (node->promotedTo) {
        // need cast
        // reg_ may be bound to a variable, in this case the following call should not in-place
        reg_ = cast(reg_, node->evalType, node->promotedTo, inplace);
    }
}

void ValueGenerator::visit(CastExpr* node)
{
    ValueGenerator casteeGen(mainGenerator_);
    node->expr_->accept(casteeGen);
    auto val = casteeGen.value();
    // allocation of new register is in function "cast"
    reg_ = cast(val, ExprType(node->expr_), node->castTo_, false);

    if (node->promotedTo) {
        // need cast
        reg_ = cast(reg_, node->evalType, node->promotedTo, true);
    }
}

void ValueGenerator::visit(SizeofExpr* node)
{
    reg_ = nextReg();
    emit({Tac::Loadi, node->type_->width(), Tac::Var::empty, reg_});

    if (node->promotedTo) {
        // need cast
        reg_ = cast(reg_, node->evalType, node->promotedTo, true);
    }
}

void ValueGenerator::visit(LiteralExpr* node)
{
    auto literalTok = node->tok_;
    auto literalType = literalTok->type();
    int64_t immi;
    if (Type::isInteger(node->evalType)) {
        switch (literalType) {
        case Token::IntLiteral:
            immi = literalTok->intLiteral();
            break;
        case Token::CharLiteral:
            immi = literalTok->charLiteral();
            break;
        default:
            assert("other literal types are not implemented");
        }
        reg_ = nextReg();
        emit({Tac::Loadi, immi, Tac::Var::empty, reg_});

    } else if (Type::isFloating(node->evalType)) {
        assert("other literal types are not implemented");
    } else if (literalType == Token::StringLiteral) {
        // TODO pool to store string literals
        assert(node->evalType->equal(PointerType::strLiteralType()));
    } else {
        assert(false);
    }

    if (node->promotedTo) {
        // need cast
        reg_ = cast(reg_, node->evalType, node->promotedTo, true);
    }
}

void LValueGenerator::visit(ArrayRefExpr* node)
{
    assert(not node->head_->promotedTo);

    ValueGenerator idxGen(mainGenerator_);
    node->index_->accept(idxGen);
    auto index = idxGen.value();

    if (node->head_->evalType->tag() == Type::Pointer) {
        // get pointer value
        ValueGenerator vg(mainGenerator_);
        node->head_->accept(vg);
        auto biase = static_cast<PointerType*>(node->head_->evalType)->base()->width();
        auto offset = nextReg();
        addr_ = nextReg();
        emit({Tac::Mul, index, biase, offset});
        emit({Tac::Add, vg.value(), offset, addr_});
    } else {
        assert(node->head_->evalType->tag() == Type::Array);
        ValueGenerator vg(mainGenerator_);
        node->head_->accept(vg);
        auto biase = static_cast<ArrayType*>(node->head_->evalType)->base()->width();
        auto offset = nextReg();
        addr_ = nextReg();
        emit({Tac::Mul, index, biase, offset});
        emit({Tac::Add, vg.value(), offset, addr_});
    }
}
