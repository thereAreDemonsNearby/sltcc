#ifndef TYPECHECKER_H__
#define TYPECHECKER_H__

#include <stack>
#include "visitor.h"

class ErrorLog;
class SymbolTable;

// 在typechecker中做了什么：
// 1. 检查类型的匹配程度
// 2. 为了生成代码而进行的其他工作如：
//      将return关联到对应的FuncDef,
//      检查必须留在内存里的变量，填进符号表
//      检查变量是否是函数参数，是第几个参数，填进符号表

class TypeChecker : public Visitor
{
public:
    TypeChecker(ErrorLog& err);
    ~TypeChecker() override = default;

    void visit(IfStmt* node) override;
    void visit(WhileStmt* node) override;
    void visit(ForStmt* node) override;
    void visit(DoWhileStmt* node) override;
    void visit(LabelStmt* node) override;
    void visit(CaseStmt* node) override;
    void visit(SwitchStmt* node) override;
    void visit(ReturnStmt* node) override;
    void visit(BreakStmt* node) override;
    void visit(CompoundDecl* node) override;
    void visit(VarDef* node) override;
    void visit(FuncDecl* node) override;
    void visit(Block* node) override;
    void visit(UnaryOpExpr* node) override;
    void visit(BinaryOpExpr* node) override;
    void visit(ConditionExpr* node) override;
    void visit(FuncCall* node) override;
    void visit(MemberExpr* node) override;
    void visit(ArrayRefExpr* node) override;
    void visit(VarExpr* node) override;
    void visit(CastExpr* node) override;
    void visit(LiteralExpr* node) override;
    void visit(SizeofExpr* node) override;
    void visit(EmptyExpr* node) override;
    void visit(CompoundDef* node) override;
    void visit(TypeAlias* node) override;
    void visit(EnumDef* node) override;
    void visit(FuncDef* node) override;
    void visit(ASTRoot* node) override;


private:
    ErrorLog& errors_;
    std::stack<SymbolTable*> scopeStack_;
    FuncDef* currFunc_ = nullptr;

    SymbolTable& currScope() { return *scopeStack_.top(); }


    void checkArithmetic(Expr* parent, const std::shared_ptr<Expr>& lhs, const std::shared_ptr<Expr>& rhs);
    void checkPointerArith(Expr* parent,
                           const std::shared_ptr<Expr>& pointer, const std::shared_ptr<Expr>& arith);
    void checkPointerCompare(Expr* parent,
                             const std::shared_ptr<Expr>& lhs, const std::shared_ptr<Expr>& rhs);
    void checkAsCond(const std::shared_ptr<Expr>& expr);
    void checkAssign(const std::shared_ptr<Expr>& from, const std::shared_ptr<Expr>& to, bool init);
    void checkVoid(const std::shared_ptr<Type>& ty, Token const*);
    void checkAssignE2T(const std::shared_ptr<Expr>& from, const std::shared_ptr<Type>& aimTy, bool init);

    using VEiter = std::vector<std::shared_ptr<Expr>>::iterator;
    std::shared_ptr<Type> checkArrayRef(Token const* pos,
                                        const std::shared_ptr<Type>&, VEiter curr, VEiter end);
    void checkVarDef(const std::shared_ptr<Type>& ty, Token const* pos);

    void makeInMemory(const std::shared_ptr<Expr>& expr);
};

#endif
