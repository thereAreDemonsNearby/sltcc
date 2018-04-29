#ifndef VISITOR_H__
#define VISITOR_H__


#include "ast.h"

class IfStmt;
class WhileStmt;
class ForStmt;
class DoWhileStmt;
class LabelStmt;
class CaseStmt;
class SwitchStmt;
class ReturnStmt;
class BreakStmt;
class CompoundDecl;
class VarDef;
class FuncDecl;
class Block;
class UnaryOpExpr;
class BinaryOpExpr;
class ConditionExpr;
class FuncCallExpr;
class MemberExpr;
class ArrayRefExpr;
class VarExpr;
class CastExpr;
class LiteralExpr;
class CompoundDef;
class TypeAlias;
class EnumDef;
class FuncDef;
class ASTRoot;

class Visitor
{
public:
    virtual void visit(IfStmt*) { assert(false); }
    virtual void visit(WhileStmt*) { assert(false); }
    virtual void visit(ForStmt*) { assert(false); }
    virtual void visit(DoWhileStmt*) { assert(false); }
    virtual void visit(LabelStmt*) { assert(false); }
    virtual void visit(CaseStmt*) { assert(false); }
    virtual void visit(SwitchStmt*) { assert(false); }
    virtual void visit(ReturnStmt*) { assert(false); }
    virtual void visit(BreakStmt*) { assert(false); }
    virtual void visit(CompoundDecl*) { assert(false); }
    virtual void visit(VarDef*) { assert(false); }
    virtual void visit(FuncDecl*) { assert(false); }
    virtual void visit(Block*) { assert(false); }
    virtual void visit(UnaryOpExpr*) { assert(false); }
    virtual void visit(BinaryOpExpr*) { assert(false); }
    virtual void visit(ConditionExpr*) { assert(false); }
    virtual void visit(FuncCallExpr*) { assert(false); }
    virtual void visit(MemberExpr*) { assert(false); }
    virtual void visit(ArrayRefExpr*) { assert(false); }
    virtual void visit(VarExpr*) { assert(false); }
    virtual void visit(CastExpr*) { assert(false); }
    virtual void visit(LiteralExpr*) { assert(false); }
    virtual void visit(CompoundDef*) { assert(false); }
    virtual void visit(TypeAlias*) { assert(false); }
    virtual void visit(EnumDef*) { assert(false); }
    virtual void visit(FuncDef*) { assert(false); }
    virtual void visit(ASTRoot*) { assert(false); }
    virtual void visit(SizeofExpr*) { assert(false); }
    virtual void visit(EmptyExpr*) { assert(false); }
    virtual ~Visitor() = default;
};

#endif //VISITOR_H__
