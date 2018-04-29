#ifndef PARENFULPRINTER_H__
#define PARENFULPRINTER_H__

#include "visitor.h"
#include "ast.h"

class ParenfulPrinter : public Visitor
{
public:
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
    void visit(FuncCallExpr* node) override;
    void visit(MemberExpr* node) override;
    void visit(ArrayRefExpr* node) override;
    void visit(VarExpr* node) override;
    void visit(CastExpr* node) override;
    void visit(LiteralExpr* node) override;
    void visit(CompoundDef* node) override;
    void visit(TypeAlias* node) override;
    void visit(EnumDef* node) override;
    void visit(FuncDef* node) override;
    void visit(ASTRoot* node) override;
    void visit(SizeofExpr* node) override;
    void visit(EmptyExpr* node) override;

    ~ParenfulPrinter() override = default;
};

#endif //PARENFULPRINTER_H__
