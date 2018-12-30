#ifndef TACGENERATOR_H__
#define TACGENERATOR_H__

#include "visitor.h"
#include "tacdef.h"
#include <list>
#include <stack>

class TacGenerator : public Visitor
{
public:
    Tac::LinearTacIR& ir() { return ir_; }

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

    Tac::StringPool& strPool() { return ir_.strPool; }
private:
    std::stack<SymbolTable*> scopeStack_;
    Tac::LinearTacIR ir_;
};

class FuncGenerator : public Visitor
{
public:
    FuncGenerator(TacGenerator& tacgen,
                  std::string funcName, ListSymtab* params,
                  std::shared_ptr<Type> retType)
            : tacGen_(tacgen),
              currFunction_(std::move(funcName), params, std::move(retType))
    {}
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
    void visit(FuncCallExpr* node) override;
    void visit(MemberExpr* node) override;
    void visit(ArrayRefExpr* node) override;
    void visit(VarExpr* node) override;
    void visit(CastExpr* node) override;
    void visit(LiteralExpr* node) override;
    void visit(SizeofExpr* node) override;

    void visit(EmptyExpr* node) override {};


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
    Tac::StringPool& strPool() { return tacGen_.strPool(); }

    TacGenerator& tacGenerator() { return tacGen_; }
private:
    TacGenerator& tacGen_;
    Tac::Function currFunction_;

    int regNo_ = 0;

    void compoundAssignment(CompoundType* type, Tac::Reg lhsAddr, Tac::Reg rhsAddr);
};

class FuncUtil
{
protected:
    FuncGenerator& funcGenerator_;

    FuncUtil(FuncGenerator& f) : funcGenerator_(f) {}
    Tac::Reg nextReg() { return funcGenerator_.nextReg(); }
    Tac::Label nextLabel() { return funcGenerator_.nextLabel(); }
    SymbolTable& currScope() { return funcGenerator_.currScope(); }
    std::list<Tac::Quad>::iterator emit(const Tac::Quad& quad) { return funcGenerator_.emit(quad); }
    std::list<Tac::Quad>::iterator lastQuad() { return funcGenerator_.lastQuad(); }
};

class BranchGenerator : public Visitor, public FuncUtil
{
public:
    BranchGenerator(FuncGenerator& tg, bool when, Tac::Label lbl)
        : FuncUtil(tg), when_(when), goto_(lbl)
    {}

    void visit(UnaryOpExpr* node) override; // !
    void visit(BinaryOpExpr* node) override; // < > && || ...
    void visit(FuncCallExpr* node) override;
    void visit(MemberExpr* node) override;
    void visit(ArrayRefExpr* node) override;
    void visit(VarExpr* node) override;
    void visit(CastExpr* node) override;
    void visit(LiteralExpr* node) override;
    void visit(SizeofExpr* node) override;

private:
    bool when_;
    Tac::Label goto_;

    Tac::Opcode genJumpInst(Token::OperatorType op,
                            const std::shared_ptr<Expr>& lhs,
                            const std::shared_ptr<Expr>& rhs);
    void implicitCond(Tac::Reg, const std::shared_ptr<Type>&);
};

/* 要是函数返回了一个结构体，本来讲道理它是个值类型，
 * 但是没办法，我们得为它分配内存，所以扔在LValueGenerator里？
 */

/**
 * 对于标量，返回一份其拷贝
 * 对于非标量：
 *     对于数组，其值即其地址，返回其地址
 *     对于struct & union：这种情况不由ValueGenerator处理，假设不可能遇到。
 * */
class ValueGenerator : public Visitor, public FuncUtil
{
public:
    explicit ValueGenerator(FuncGenerator& t)
            : FuncUtil(t) {}

    Tac::Reg value() { return reg_; };

    void visit(UnaryOpExpr* node) override;
    void visit(BinaryOpExpr* node) override;
    void visit(FuncCallExpr* node) override;
    void visit(MemberExpr* node) override;
    void visit(ArrayRefExpr* node) override;
    void visit(VarExpr* node) override;
    void visit(CastExpr* node) override;
    void visit(LiteralExpr* node) override;
    void visit(SizeofExpr* node) override;
    // void visit(EmptyExpr* node) override;

private:
    Tac::Reg reg_;

    Tac::Reg cast(Tac::Reg reg,
                  const std::shared_ptr<Type>& from, const std::shared_ptr<Type>& to,
                  bool inplace);


};

class LValueGenerator: public Visitor, public FuncUtil
{
public:
    explicit LValueGenerator(FuncGenerator& f) : FuncUtil(f) {}
    Tac::Reg addr() { return addr_; }
    bool inMemory() { return inMemory_; }

    void visit(UnaryOpExpr* node) override;
    void visit(MemberExpr* node) override;
    void visit(ArrayRefExpr* node) override;
    void visit(VarExpr* node) override;

    void visit(FuncCallExpr* node) override;

private:

    Tac::Reg addr_;
    bool inMemory_ = true;
    // true if this lvalue is not kept in register
    // which means addr() returns the address of the lvalue
    // otherwise the value is in register, addr() returns the bounded register
};

#endif //TACGENERATOR_H__
