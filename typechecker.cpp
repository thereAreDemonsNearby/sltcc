#include "typechecker.h"
#include "SymbolTable.h"
#include "error.h"
#include "type.h"
#include "platform.h"

TypeChecker::TypeChecker(ErrorLog& err)
        : errors_(err)
{
}

void TypeChecker::visit(IfStmt* node)
{
    node->cond_->accept(*this);
    checkAsCond(node->cond_);
    node->then_->accept(*this);
    if (node->else_)
        node->else_->accept(*this);
}

void TypeChecker::visit(WhileStmt* node)
{
    node->cond_->accept(*this);
    checkAsCond(node->cond_);
    node->body_->accept(*this);
}

void TypeChecker::visit(ForStmt* node)
{
    if (node->init_)
        node->init_->accept(*this);
    if (node->cond_) {
        node->cond_->accept(*this);
        checkAsCond(node->cond_);
    }
    if (node->stepby_)
        node->stepby_->accept(*this);
    node->body_->accept(*this);
}

void TypeChecker::visit(DoWhileStmt* node)
{
    node->cond_->accept(*this);
    checkAsCond(node->cond_);
    node->body_->accept(*this);
}

void TypeChecker::visit(LabelStmt* node)
{
    // not implemented
}

void TypeChecker::visit(CaseStmt* node)
{
    // not implemented
}

void TypeChecker::visit(SwitchStmt* node)
{
    // not implemented
}

void TypeChecker::visit(ReturnStmt* node)
{
    node->ret_->accept(*this);
    node->func_ = currFunc_;
    checkAssignE2T(node->ret_, node->func_->type_->retType(), true);
}

void TypeChecker::visit(BreakStmt* node)
{
    // not implemented
}

void TypeChecker::visit(CompoundDecl* node)
{
    // nothing to do
}

void TypeChecker::visit(VarDef* node)
{
    auto ty = node->type_;
    checkVarDef(ty, node->tok());

    if (node->init_) {
        node->init_->accept(*this);
        checkAssignE2T(node->init_, ty, true);
    }
}

void TypeChecker::visit(FuncDecl* node)
{
    // nothing to do
}

void TypeChecker::visit(Block* node)
{
    scopeStack_.push(&node->scope());
    for (auto& item : node->stmts_) {
        try {
            item->accept(*this);
        } catch (Error& err) {
            errors_.add(err);
        }
    }
    scopeStack_.pop();
}

void TypeChecker::visit(UnaryOpExpr* node)
{
    // node->isLValue = false;
    std::shared_ptr<Expr>& operand = node->operand_;
    operand->accept(*this);
    auto op = node->operator_;
    switch (op) {
    case Token::Add:
    case Token::Sub: {
        // unary + or -
        if (!Type::isArithmetic(operand->evalType)) {
            throw Error(operand->tok(), "the value can't be an operand of unary operator+/-");
        } else {
            node->evalType = operand->evalType;
        }
        break;
    }
    case Token::Not: {
        checkAsCond(operand);
        node->evalType = BuiltInType::charType();
        break;
    }
    case Token::BitInv: {
        if (!Type::isInteger(operand->evalType)) {
            throw Error(operand->tok(), "the value can't be an operand of operator~");
        } else {
            node->evalType = operand->evalType;
        }
        break;
    }
    case Token::Mult: {
        // pointer deref *p
        if (operand->evalType->tag() == Type::Pointer) {
            node->evalType = static_cast<PointerType*>(operand->evalType.get())->base();
        } else if (operand->evalType->tag() == Type::Array) {
            node->evalType = static_cast<ArrayType*>(operand->evalType.get())->base();
        } else {
            throw Error(operand->tok(), "can't dereference such a value");
        }
        node->isLValue = true;

        break;
    }
    case Token::BitAnd: {
        // get address &a
        if (!operand->isLValue) {
            throw Error(operand->tok(), "rvalues do not have an addresses");
        }
        node->evalType = std::make_shared<PointerType>(operand->evalType, 0);

        // make sth dirty
        makeInMemory(operand);
        break;
    }
    default:
        assert("wobuzhidaode unary operator" && false);
    }
}

void TypeChecker::visit(BinaryOpExpr* node)
{
    node->lhs_->accept(*this);
    node->rhs_->accept(*this);

    auto& lhs = node->lhs_;
    auto& rhs = node->rhs_;
    auto op = node->operator_;

    switch (op) {
    case Token::Mult:
    case Token::Div:
    case Token::Mod: {
        if (!Type::isArithmetic(lhs->evalType) ||
            !Type::isArithmetic(rhs->evalType)) {
            throw Error(node->tok(), "invalid type for *, / or %");
        }
        checkArithmetic(node, lhs, rhs);
        break;
    }

    case Token::Add:
    case Token::Sub: {
        if (Type::isArithmetic(lhs->evalType)
            && Type::isArithmetic(rhs->evalType)) {
            checkArithmetic(node, lhs, rhs);
        } else if (lhs->evalType->tag() == Type::Pointer
                   && Type::isInteger(rhs->evalType)) {
            checkPointerArith(node, lhs, rhs);
        } else if (rhs->evalType->tag() == Type::Pointer
                   && Type::isInteger(lhs->evalType)) {
            checkPointerArith(node, rhs, lhs);
        } else {
            throw Error(node->tok(), "invalid type for +/-");
        }
        break;
    }
    case Token::SftL:
    case Token::SftR: {
        if (!Type::isInteger(lhs->evalType) || !Type::isInteger(rhs->evalType)) {
            throw Error(node->tok(), "only integer can be used to shift");
        }
        auto rhsTy = static_cast<BuiltInType*>(rhs->evalType.get());
        if (!rhsTy->isUnsigned()) {
            auto beUnsigned = rhsTy->builtinClone();
            beUnsigned->isUnsigned(true);
            rhs->promotedTo = beUnsigned;
        }
        node->evalType = lhs->evalType;
        break;
    }
    case Token::Grtr:
    case Token::Smlr:
    case Token::Ge:
    case Token::Se:
    case Token::Eq:
    case Token::Ne: {
        if (Type::isArithmetic(lhs->evalType) && Type::isArithmetic(rhs->evalType)) {
            checkArithmetic(node, lhs, rhs);
            node->evalType = BuiltInType::charType();
            /// so the type set for node->evalTyppe in chechArithmetic is discarded
        } else if (lhs->evalType->tag() == Type::Pointer
                   && rhs->evalType->tag() == Type::Pointer) {
            checkPointerCompare(node, lhs, rhs);
        } else {
            throw Error(node->tok(), "can't compare lhs and rhs");
        }
        node->evalType = BuiltInType::charType();
    }

    case Token::BitAnd:
    case Token::BitXor:
    case Token::BitOr: {
        if (Type::isInteger(lhs->evalType) && Type::isInteger(rhs->evalType)) {
            checkArithmetic(node, lhs, rhs);
        } else {
            throw Error(node->tok(), "bit opearations are only for integer types");
        }
        break;
    }

    case Token::And:
    case Token::Or: {
        checkAsCond(lhs);
        checkAsCond(rhs);
        node->evalType = BuiltInType::charType();
        break;
    }

    case Token::Assign: {
        // TODO: += -= *= .......
        checkAssign(rhs, lhs, false);
        node->evalType = BuiltInType::voidType(); // 我希望赋值操作不返回值
        break;
    }
        /*case Token::AddAssign: {
            break;
        }
        case Token::SubAssign: {
            break;
        }
        case */

    default: {
        throw Error(node->tok(), "unknown binary operator expression");
    }

    }
}

void TypeChecker::visit(ConditionExpr* node)
{
    // TODO not implemented
}

void TypeChecker::visit(FuncCallExpr* node)
{
    auto ent = currScope().find(node->funcName_);
    if (!ent || ent->type->tag() != Type::Tag::Func) {
        throw Error(node->tok(), std::string("there is no function called ") + node->funcName_);
    }
    auto funcType = static_cast<FuncType*>(ent->type.get());

    node->evalType = funcType->retType();

    auto argIt = node->args_.begin();
    auto paramIt = funcType->begin();
    for ( ; argIt != node->args_.end() && paramIt != funcType->end();
            ++argIt, ++paramIt) {
        (*argIt)->accept(*this);

        checkAssignE2T(*argIt, *paramIt, true);
    }

    if (argIt != node->args_.end() && paramIt == funcType->end()) {
        if (!funcType->hasVarArgs()) {
            throw Error(node->tok(), "too much arguments");
        } // else ok
    } else if (argIt == node->args_.end() && paramIt != funcType->end()) {
        throw Error(node->tok(), "too few arguments");
    }
}

void TypeChecker::visit(MemberExpr* node)
{
    node->compound_->accept(*this);

    auto& lhs = node->compound_;
    if (lhs->evalType->tag() != Type::UserDefined) {
        throw Error(node->tok(), "don't have any data member");
    }

    auto typeRef = static_cast<UserDefinedTypeRef*>(lhs->evalType.get());
    auto name = typeRef->typeName();
    auto& scope = typeRef->scope();

    auto typeEnt = scope.findInCurr(name);
    if (!typeEnt) {
        throw Error(node->tok(), "a nonexistent compound type????");
    }
    auto realType = typeEnt->type;
    if (realType->tag() != Type::Compound) {
        throw Error(node->tok(), "variables of this type don't have any member");
    }

    auto compound = static_cast<CompoundType*>(realType.get());
    auto memberEnt = compound->members().find(node->memberName_);
    if (!memberEnt) {
        throw Error(node->tok(),
                    std::string("variables of such type don't have a member called")
                    + node->memberName_);
    }

    node->isLValue = node->compound_->isLValue;
    if (!lhs->evalType->isConst())
        node->evalType = memberEnt->type;
    else {
        node->evalType = memberEnt->type->shallowCopy();
        node->evalType->isConst(true);
    }
}

void TypeChecker::visit(ArrayRefExpr* node)
{
    node->head_->accept(*this);
    auto& ty = node->head_->evalType;
    if (ty->tag() == Type::Array
        || ty->tag() == Type::Pointer) {

        if (ty->tag() == Type::Array) {
            auto arrTy = static_cast<ArrayType*>(ty.get());
            node->evalType = arrTy->base();
        } else {
            auto ptrTy = static_cast<PointerType*>(ty.get());
            node->evalType = ptrTy->base();
        }

        node->isLValue = true;

    } else {
        throw Error(node->tok(), "operator[]: expect array or pointer");
    }
}

void TypeChecker::visit(VarExpr* node)
{
    auto ent = currScope().find(node->name_);
    if (!ent) {
        throw Error(node->tok(), std::string("variable ") + node->name_ + " does not exist");
    }
    auto& type = ent->type;
    if (ent->position > node->tok()) {
        throw Error(node->tok(), "variable is used before declaration");
    }
    checkVoid(type, node->tok());
    node->evalType = type;
    node->isLValue = true;
}

void TypeChecker::visit(CastExpr* node)
{
    // TODO: not implemented
    node->expr_->accept(*this);
    node->evalType = node->castTo_;
    if (Type::isArithmetic(node->castTo_)
        && Type::isArithmetic(node->expr_->evalType)) {
        // 算术类型互转，ok
    } else if (Type::isPointer(node->castTo_)
               && Type::isPointer(node->expr_->evalType)) {
        // 指针互转, ok
    } else if (Type::isPointer(node->castTo_)
               && node->expr_->evalType->tag() == Type::Array) {
        // 数组转指针，ok
    } else if (Type::isInteger(node->castTo_)
               && Type::isPointer(node->expr_->evalType)) {
        auto ty = static_cast<BuiltInType*>(node->castTo_.get());
        if (ty->width() < PTRSIZE) {
            throw Error(node->tok(),
                        "integer of such type is not big enough to hold a pointer");
        }
    } else if (Type::isPointer(node->castTo_) && Type::isInteger(node->expr_->evalType)) {
        if (node->expr_->evalType->width() < PTRSIZE) {
            errors_.add(Error(node->tok(), "warning: integer not wide enough to be a ptr"));
        }
    } else {
        throw Error(node->tok(), "can't cast to such type");
    }
}

void TypeChecker::visit(LiteralExpr* node)
{
    auto literalType = node->tok_->type();
    switch (literalType) {
    case Token::IntLiteral:
        node->evalType = BuiltInType::intType();
        break;
    case Token::CharLiteral:
        node->evalType = BuiltInType::charType();
        break;
    case Token::DoubleLiteral:
        node->evalType = BuiltInType::doubleType();
        break;
    case Token::StringLiteral:
        node->evalType = PointerType::strLiteralType();
        break;
    default:
        assert("other literals are not implemented" && false);
        break;
    }
}

void TypeChecker::visit(CompoundDef* node)
{
    auto iter = node->type_->begin();
    auto end = node->type_->end();
    for (; iter != end; ++iter) {
        checkVarDef(iter->second.type, node->tok());
    }
}

void TypeChecker::visit(TypeAlias* node)
{
    // TODO not implemented
}

void TypeChecker::visit(EnumDef* node)
{
    // TODO not implemented
}

void TypeChecker::visit(FuncDef* node)
{
    currFunc_ = node;
    auto n = 1;
    for (auto& param : node->params_) {
        param.second.isParam = true;
        param.second.seq = n++;
    }
    node->body_->accept(*this);
}

void TypeChecker::visit(ASTRoot* node)
{
    scopeStack_.push(&node->scope());
    for(auto& item : node->contents_) {
        try {
            item->accept(*this);
        } catch (Error& err) {
            errors_.add(err);
        }
    }
}

void TypeChecker::visit(SizeofExpr* node)
{
    node->evalType = BuiltInType::maxUIntType();
}

void TypeChecker::visit(EmptyExpr* node)
{
}



void TypeChecker::checkArithmetic(Expr* parent,
                                  const std::shared_ptr<Expr>& lhs, const std::shared_ptr<Expr>& rhs)
{
    assert(Type::isArithmetic(lhs->evalType) && Type::isArithmetic(rhs->evalType));
    auto lhsTy = static_cast<BuiltInType*>(lhs->evalType.get());
    auto rhsTy = static_cast<BuiltInType*>(rhs->evalType.get());
    if (lhsTy->isInteger() && rhsTy->isInteger()) {
        bool isUnsigned = lhsTy->isUnsigned() || rhsTy->isUnsigned();
        size_t width = std::max(lhsTy->width(), rhsTy->width());
        auto bigger = std::make_shared<BuiltInType>(BuiltInType::Integer, isUnsigned, width, 0);
        if (!bigger->equalUnqual(lhs->evalType)) {
            lhs->promotedTo = bigger;
        }
        if (!bigger->equalUnqual(rhs->evalType)) {
            rhs->promotedTo = bigger;
        }
        parent->evalType = bigger;
    } else if (lhsTy->isInteger() && rhsTy->isFloating()) {
        lhs->promotedTo = rhs->evalType;
        parent->evalType = rhs->evalType;
    } else if (lhsTy->isFloating() && rhsTy->isInteger()) {
        rhs->promotedTo = lhs->evalType;
        parent->evalType = lhs->evalType;
    } else {
        assert(lhsTy->isFloating() && rhsTy->isFloating());
        if (lhsTy->width() > rhsTy->width()) {
            rhs->promotedTo = lhs->evalType;
            parent->evalType = lhs->evalType;
        } else if (lhsTy->width() < rhsTy->width()) {
            lhs->promotedTo = rhs->evalType;
            parent->evalType = rhs->evalType;
        }
    }
}

void
TypeChecker::checkPointerArith(Expr* parent,
                               const std::shared_ptr<Expr>& pointer, const std::shared_ptr<Expr>& arith)
{
    parent->evalType = pointer->evalType;
    auto expanded = static_cast<BuiltInType*>(arith->evalType.get())->builtinClone();
    expanded->width(PTRSIZE);
    if (!arith->evalType->equalUnqual(expanded))
        arith->promotedTo = std::move(expanded);
}

void TypeChecker::checkPointerCompare(Expr* parent,
                                      const std::shared_ptr<Expr>& lhs, const std::shared_ptr<Expr>& rhs)
{
    assert(lhs->evalType->tag() == Type::Pointer
           && rhs->evalType->tag() == Type::Pointer);
    auto lhsTy = static_cast<PointerType*>(lhs->evalType.get());
    auto rhsTy = static_cast<PointerType*>(rhs->evalType.get());
    if (!lhsTy->base()->equalUnqual(rhsTy->base())) {
        throw Error(parent->tok(), "cannot compare between incompatible pointer types");
    }
}

void TypeChecker::checkAsCond(const std::shared_ptr<Expr>& expr)
{
    if (Type::isInteger(expr->evalType) || expr->evalType->tag() == Type::Pointer) {
        // if (expr->evalType->width() < INTS)
        expr->isCond = true;
        /*auto prom = std::make_shared<BuiltInType>
                (BuiltInType::Integer, true, PTRSIZE, 0);
        if (!expr->evalType->equalUnqual(prom)) {
            expr->promotedTo = std::move(prom);
        }*/
    } else {
        throw Error(expr->tok(), "expression cannot be used in a boolean expression");
    }
}

void TypeChecker::checkAssign(const std::shared_ptr<Expr>& from, const std::shared_ptr<Expr>& to, bool init)
{
    if (!to->isLValue) {
        throw Error(to->tok(), "can't assign to rvalue");
    }
    checkAssignE2T(from, to->evalType, init);
    /*if (!init) {
        if (to->evalType->isConst()) {
            throw Error(to->tok(), "can't assign to const variable");
        }
    }

    checkVoid(from->evalType, to->tok());
    checkVoid(to->evalType, to->tok());

    if (from->evalType->tag() == Type::Pointer && to->evalType->tag() == Type::Pointer) {
        PointerType* fromTy = static_cast<PointerType*>(from->evalType.get());
        PointerType* toTy = static_cast<PointerType*>(to->evalType.get());
        if (Type::isVoid(fromTy->base())) {
            // void* can be casted to any type of pointer
            return;
        }

        // TODO 详细思考某些指针赋值,也许根本就大脚步呢。
        if (fromTy->base()->equalUnqual(toTy->base())) {
            if (fromTy->base()->isConst() && !toTy->base()->isConst()) {
                throw Error(to->tok(), "can't assign between incompatible pointers");
            }
        } else {
            throw Error(to->tok(), "can't assign between incompatible pointers");
        }


    } else if (from->evalType->tag() == Type::BuiltIn
            && to->evalType->tag() == Type::BuiltIn) {

        // builtin but not void(checked before)
        BuiltInType* fromTy = static_cast<BuiltInType*>(from->evalType.get());
        BuiltInType* toTy = static_cast<BuiltInType*>(to->evalType.get());
        if (fromTy->cat() == toTy->cat()) {
            // 假设符号情况不同可以直接夏季把转型
            if (fromTy->width() > toTy->width()) {
                throw Error(to->tok(), "can't assign big type to small type (size)");
            } else if (fromTy->width() < toTy->width()) {
                from->promotedTo = to->evalType;
            }
        } else if (fromTy->isInteger() && toTy->isFloating()) {
            from->promotedTo = to->evalType;
        } else {
            throw Error(to->tok(), "can't assign big type to small type (floating to integer)");
        }

    } else if (from->evalType->tag() == Type::UserDefined
               && to->evalType->tag() == Type::UserDefined) {
        if (!from->evalType->equalUnqual(to->evalType)) {
            throw Error(to->tok(), "can't assign between incompatible types");
        }
    } else if (from->evalType->tag() == Type::Array && to->evalType->tag() == Type::Pointer) {
        auto fromTy = static_cast<ArrayType*>(from->evalType.get());
        auto toTy = static_cast<PointerType*>(to->evalType.get());
        if (fromTy->base()->equalUnqual(toTy->base())) {
            if (!toTy->base()->isConst()) {
                throw Error(to->tok(), "cannot assign array name to non-const pointer");
            }
            from->promotedTo = to->evalType;
        } else {
            throw Error(to->tok(), "cannot assign between incompatible array and pointer types");
        }
    } else {
        throw Error(to->tok(), "cannot assign between incompatible types");
    }*/
}


/** used by initialization */
void TypeChecker::checkAssignE2T(const std::shared_ptr<Expr>& from,
                                 const std::shared_ptr<Type>& aimTy, bool init)
{
    if (!init) {
        if (aimTy->isConst()) {
            throw Error(from->tok(), "can't assign to const variable");
        }
    }

    checkVoid(from->evalType, from->tok());
    checkVoid(aimTy, from->tok());

    if (from->evalType->tag() == Type::Pointer && aimTy->tag() == Type::Pointer) {
        PointerType* fromTy = static_cast<PointerType*>(from->evalType.get());
        PointerType* toTy = static_cast<PointerType*>(aimTy.get());
        if (Type::isVoid(fromTy->base())) {
            // void* can be casted to any type of pointer
            return;
        }

        // TODO 详细思考某些指针赋值,也许根本就大脚步呢。
        if (fromTy->base()->equalUnqual(toTy->base())) {
            if (fromTy->base()->isConst() && !toTy->base()->isConst()) {
                throw Error(from->tok(), "can't assign between incompatible pointers");
            }
        } else {
            throw Error(from->tok(), "can't assign between incompatible pointers");
        }


    } else if (from->evalType->tag() == Type::BuiltIn
               && aimTy->tag() == Type::BuiltIn) {

        // builtin but not void(checked before)
        BuiltInType* fromTy = static_cast<BuiltInType*>(from->evalType.get());
        BuiltInType* toTy = static_cast<BuiltInType*>(aimTy.get());
        if (fromTy->cat() == toTy->cat()) {
            // 假设符号情况不同可以直接夏季把转型
            if (fromTy->width() > toTy->width()) {
                throw Error(from->tok(), "can't assign big type to small type (size)");
            } else if (fromTy->width() < toTy->width()) {
                from->promotedTo = aimTy;
            }
        } else if (fromTy->isInteger() && toTy->isFloating()) {
            from->promotedTo = aimTy;
        } else {
            throw Error(from->tok(), "can't assign big type to small type (floating to integer)");
        }

    } else if (from->evalType->tag() == Type::UserDefined
               && aimTy->tag() == Type::UserDefined) {
        if (!from->evalType->equalUnqual(aimTy)) {
            throw Error(from->tok(), "can't assign between incompatible types");
        }
    } else if (from->evalType->tag() == Type::Array && aimTy->tag() == Type::Pointer) {
        auto fromTy = static_cast<ArrayType*>(from->evalType.get());
        auto toTy = static_cast<PointerType*>(aimTy.get());
        if (fromTy->base()->equalUnqual(toTy->base())
            && (int)toTy->base()->isConst() >= (int)fromTy->base()->isConst()) {
            // TODO this promotion may be not needed.
            // from->promotedTo = aimTy;
        } else {
            throw Error(from->tok(), "cannot assign between incompatible array and pointer types");
        }
    } else if (Type::isInteger(from->evalType) && from->isLiteral() && aimTy->tag() == Type::Pointer) {
        assert(from->tok()->type() == Token::IntLiteral);
        int value = from->tok()->intLiteral();
        if (value == 0) {
            /// 0 is null, ok
        } else {
            throw Error(from->tok(), "can't assign non-zero integer to pointer");
        }
    } else {
        throw Error(from->tok(), "cannot assign between incompatible types");
    }
}

std::shared_ptr<Type>
TypeChecker::checkArrayRef(Token const* pos,
                           const std::shared_ptr<Type>& ty,
                           TypeChecker::VEiter curr, TypeChecker::VEiter end)
{
    std::shared_ptr<Type> pty = ty;
    while (curr != end) {
        (*curr)->accept(*this);
        if (!Type::isInteger((*curr)->evalType)) {
            throw Error(pos, "array subscript should be integer");
        }
        (*curr)->promotedTo = BuiltInType::maxUIntType();

        if (pty->tag() == Type::Array) {
            pty = static_cast<ArrayType*>(pty.get())->base();
        } else if (pty->tag() == Type::Pointer) {
            pty = static_cast<PointerType*>(pty.get())->base();
        } else {
            // 还有维度，但是类型不是指针或者数组了。
            throw Error(pos, "expecting array or pointer type");
        }
        ++curr;
    }
    return pty;
    /*if (curr != end) {
        auto& currExpr = *curr;
        currExpr->accept(*this);

        if (!isInteger(currExpr->evalType)) {
            throw Error(pos, "array subscript should be integer");
        }
        currExpr->promotedTo = Type::ULong;

        if (ty->tag() == Type::Pointer) {

            auto ptrType = static_cast<PointerType*>(ty.get());
            return checkArrayRef(pos, ptrType->base(), curr + 1, end);

        } else if (ty->tag() == Type::Array) {

            auto arrType = static_cast<ArrayType*>(ty.get());
            return checkArrayRef(pos, arrType->base(), curr + 1, end);

        } else {
            // 还有维度，但是类型不是指针或者数组了。
            throw Error(pos, "expecting array or pointer type");
        }
    } else {
        return ty;
    }*/
}

void TypeChecker::checkVarDef(const std::shared_ptr<Type>& ty, Token const* pos)
{
    if (Type::isVoid(ty)) {
        throw Error(pos, "cannot declare a variable of type void");
    } else if (ty->tag() == Type::UserDefined) {
        auto cast = static_cast<UserDefinedTypeRef*>(ty.get());
        auto realType = cast->scope().findInCurr(cast->typeName())->type;
        if (realType->tag() == Type::Tag::Incomplete) {
            throw Error(pos, "cannot declare a variable of incomplete type");
        }
    }
}


void TypeChecker::checkVoid(const std::shared_ptr<Type>& ty, Token const* pos)
{
    if (Type::isVoid(ty)) {
        throw Error(pos, "value of type void is not permitted");
    }
}

void TypeChecker::makeInMemory(const std::shared_ptr<Expr>& expr)
{
    assert(expr->isLValue);
    switch (expr->tag()) {
    case ExprTag::Var: {
        auto varExpr = static_cast<VarExpr*>(expr.get());
        auto ent = currScope().find(varExpr->name_);
        if (!ent)
            assert(false);
        ent->ambiguous = true;
        break;
    }
    case ExprTag::ArrayRef: {
        auto arrRef = static_cast<ArrayRefExpr*>(expr.get());
        if (arrRef->head_->tag() == ExprTag::Var) {
            makeInMemory(arrRef->head_);
        }
        break;
    }
    case ExprTag::Member: {
        auto memberExpr = static_cast<MemberExpr*>(expr.get());
        makeInMemory(memberExpr->compound_);
        break;
    }
    case ExprTag::Unary: {
        auto unaryExpr = static_cast<UnaryOpExpr*>(expr.get());
        if (unaryExpr->operator_ == Token::OperatorType::Mult) {
            // should do nothing
        } else
            assert(false);
        break;
    }
    default:
        assert(false);
    }
}





