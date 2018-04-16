//
// Created by shiletong on 18-3-18.
//

#ifndef TACGENERATOR_H__
#define TACGENERATOR_H__

#include "visitor.h"
#include "tacdef.h"
#include <list>
#include <stack>

class TacGenerator : public Visitor
{
public:
    Tac::TacIR& ir() { return ir_; }

    void visit(ASTRoot* node) override;
    void visit(FuncDef* node) override;
    void visit(VarDef* node) override;

    void visit(CompoundDecl*) override {}
    void visit(CompoundDef*) override {}
    void visit(EnumDef*) override {}
    void visit(TypeAlias*) override {}
    void visit(FuncDecl*) override {}


    SymbolTable& currScope() { return *scopeStack_.top(); }
    void pushScope(SymbolTable* s) { scopeStack_.push(s); }
    void popScope() { scopeStack_.pop(); }
private:
    std::stack<SymbolTable*> scopeStack_;
    Tac::TacIR ir_;
};

class FuncGenerator : public Visitor
{
public:
    FuncGenerator(TacGenerator& tacgen,
                  std::string funcName, ListSymtab* params)
            : tacGen_(tacgen), currFunction_(std::move(funcName), params) {}
    Tac::Function& func() { return currFunction_; }

    void visit(FuncDef* node) override;
    void visit(Block* node) override;

    void visit(IfStmt* node) override;
    void visit(WhileStmt* node) override;
    void visit(ForStmt* node) override;
    void visit(DoWhileStmt* node) override;

    void visit(ReturnStmt* node) override;

    void visit(VarDef* node) override;

    void visit(UnaryOpExpr* node) override;
    void visit(BinaryOpExpr* node) override;
    void visit(FuncCall* node) override;
    void visit(MemberExpr* node) override;
    void visit(ArrayRefExpr* node) override;
    void visit(VarExpr* node) override;
    void visit(CastExpr* node) override;
    void visit(LiteralExpr* node) override;
    void visit(SizeofExpr* node) override;
    void visit(EmptyExpr* node) override;


    void visit(CompoundDecl*) override {}
    void visit(CompoundDef*) override {}
    void visit(EnumDef*) override {}
    void visit(TypeAlias*) override {}
    void visit(FuncDecl*) override {}

    // not implemented:
    // void visit(LabelStmt* node) override;
    // void visit(CaseStmt* node) override;
    // void visit(SwitchStmt* node) override;

    Tac::Reg nextReg();
    Tac::Label nextLabel();
    SymbolTable& currScope() { return tacGen_.currScope(); }
    void pushScope(SymbolTable* s) { tacGen_.pushScope(s); }
    void popScope() { tacGen_.popScope(); }
    std::list<Tac::Quad>::iterator emit(const Tac::Quad& quad);
    std::list<Tac::Quad>::iterator lastQuad();
private:
    TacGenerator& tacGen_;
    Tac::Function currFunction_;

    int regNo_ = 0;
};

class BranchGenerator : public Visitor
{
public:
    BranchGenerator(FuncGenerator& tg, bool when, Tac::Label lbl)
        : mainGenerator_(tg), when_(when), goto_(lbl)
    {}

    void visit(UnaryOpExpr* node) override; // !
    void visit(BinaryOpExpr* node) override; // < > && || ...
    void visit(FuncCall* node) override;
    void visit(MemberExpr* node) override;
    void visit(ArrayRefExpr* node) override;
    void visit(VarExpr* node) override;
    void visit(CastExpr* node) override;
    void visit(LiteralExpr* node) override;
    void visit(SizeofExpr* node) override;

    Tac::Reg nextReg() { return mainGenerator_.nextReg(); }
    Tac::Label nextLabel() { return mainGenerator_.nextLabel(); }
    SymbolTable& currScope() { return mainGenerator_.currScope(); }
    std::list<Tac::Quad>::iterator emit(const Tac::Quad& quad) { mainGenerator_.emit(quad); }
    std::list<Tac::Quad>::iterator lastQuad() { return mainGenerator_.lastQuad(); }
private:
    FuncGenerator& mainGenerator_;
    bool when_;
    Tac::Label goto_;

    Tac::Opcode comparator(Token::OperatorType op,
                           const std::shared_ptr<Expr>& lhs,
                           const std::shared_ptr<Expr>& rhs);
    void implicitCond(Tac::Reg, const std::shared_ptr<Type>&);
};

class ValueGenerator : public Visitor
{
public:
    explicit ValueGenerator(FuncGenerator& t)
            : mainGenerator_(t) {}

    Tac::Reg value() { return reg_; };

    void visit(UnaryOpExpr* node) override;
    void visit(BinaryOpExpr* node) override;
    void visit(FuncCall* node) override;
    void visit(MemberExpr* node) override;
    void visit(ArrayRefExpr* node) override;
    void visit(VarExpr* node) override;
    void visit(CastExpr* node) override;
    void visit(LiteralExpr* node) override;
    void visit(SizeofExpr* node) override;
    // void visit(EmptyExpr* node) override;

    Tac::Reg nextReg() { return mainGenerator_.nextReg(); }
    Tac::Label nextLabel() { return mainGenerator_.nextLabel(); }
    SymbolTable& currScope() { return mainGenerator_.currScope(); }
    std::list<Tac::Quad>::iterator emit(const Tac::Quad& quad) { mainGenerator_.emit(quad); }
    std::list<Tac::Quad>::iterator lastQuad() { return mainGenerator_.lastQuad(); }
private:
    Tac::Reg reg_;
    FuncGenerator& mainGenerator_;

    Tac::Reg cast(Tac::Reg reg,
                  const std::shared_ptr<Type>& from, const std::shared_ptr<Type>& to,
                  bool inplace);
};

class LValueGenerator: public Visitor
{
public:
    LValueGenerator(FuncGenerator& f) : mainGenerator_(f) {}
    Tac::Reg addr() { return addr_; }
    bool inMemory() { return inMemory_; }

    void visit(UnaryOpExpr* node) override;
    void visit(MemberExpr* node) override;
    void visit(ArrayRefExpr* node) override;
    void visit(VarExpr* node) override;

    Tac::Reg nextReg() { return mainGenerator_.nextReg(); }
    Tac::Label nextLabel() { return mainGenerator_.nextLabel(); }
    SymbolTable& currScope() { return mainGenerator_.currScope(); }
    std::list<Tac::Quad>::iterator emit(const Tac::Quad& quad) { mainGenerator_.emit(quad); }
    std::list<Tac::Quad>::iterator lastQuad() { return mainGenerator_.lastQuad(); }

private:
    FuncGenerator& mainGenerator_;
    Tac::Reg addr_;
    bool inMemory_ = true;
    // true if this lvalue is not kept in register
    // which means addr() returns the address of the lvalue
    // otherwise the value is in register, addr() returns the bounded register
};

#endif //TACGENERATOR_H__
