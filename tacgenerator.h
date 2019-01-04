#ifndef TACGENERATOR_H__
#define TACGENERATOR_H__

#include "visitor.h"
#include "tacdef.h"
#include <list>
#include <stack>
#include <optional>

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

    int addLiteral(Tac::StaticObject const& so) { return ir_.addLiteral(so); }
    void addGlobalVar(std::string const& name, Tac::StaticObject const& so)
    {
        ir_.addGlobalVar(name, so);
    }

    Tac::Label nextLabel()
    {
        return Tac::Label(labelNo_++);
    }

private:
    std::stack<SymbolTable*> scopeStack_;
    Tac::LinearTacIR ir_;

    int labelNo_ = 1;
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

    struct TempStackObjectDeallocGuard
    {
        FuncGenerator& fg_;
        TempStackObjectDeallocGuard(FuncGenerator& fg) : fg_(fg) {
            assert(fg_.tempStackObjects_.empty());
        }

        ~TempStackObjectDeallocGuard() {
            while (!fg_.tempStackObjects_.empty()) {
                auto so = fg_.tempStackObjects_.top();
                fg_.tempStackObjects_.pop();
                fg_.emit({Tac::Dealloca, so});
            }
        }
    };

    Tac::Function& func() { return currFunction_; }

    void visit(FuncDef* node) override;
    void visit(Block* node) override;

    void visit(IfStmt* node) override;
    void visit(WhileStmt* node) override;
    void visit(ForStmt* node) override;
    void visit(DoWhileStmt* node) override;

    void visit(VarDef* node) override;

    void visit(ReturnStmt* node) override;
    void visit(BinaryOpExpr* node) override;
    /// trivial. forward to ValueGenerator
    void visit(UnaryOpExpr* node) override;
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

    ///  not implemented:
    // void visit(LabelStmt* node) override;
    // void visit(CaseStmt* node) override;
    // void visit(SwitchStmt* node) override;

    Tac::Reg nextReg();
    Tac::Label nextLabel();
    Tac::StackObject nextStackObject(uint64_t size, size_t align);
    SymbolTable& currScope() { return tacGen_.currScope(); }
    void pushScope(SymbolTable* s) { tacGen_.pushScope(s); }
    void popScope() { tacGen_.popScope(); }

    std::list<Tac::Quad>::iterator emit(Tac::Quad&& quad);
    std::list<Tac::Quad>::iterator lastQuad();
    void pushTempStackObject(const Tac::StackObject& s) { tempStackObjects_.push(s); }
    int addLiteral(Tac::StaticObject const& o) { return tacGen_.addLiteral(o); }
    void addGlobalVar(std::string const& s, Tac::StaticObject const& o)
    {
        tacGen_.addGlobalVar(s, o);
    }

    TacGenerator& tacGenerator() { return tacGen_; }
private:
    TacGenerator& tacGen_;
    Tac::Function currFunction_;
    std::stack<Tac::StackObject> tempStackObjects_;

    int regNo_ = 0;
    int stackObjectNo_ = 1;

    void compoundAssignment(CompoundType* type, Tac::Reg lhsAddr, Tac::Reg rhsAddr);
};

class FuncUtil
{
protected:
    FuncGenerator& funcGenerator_;

    FuncUtil(FuncGenerator& f) : funcGenerator_(f) {}
    Tac::Reg nextReg() { return funcGenerator_.nextReg(); }
    Tac::Label nextLabel() { return funcGenerator_.nextLabel(); }
    Tac::StackObject nextStackObject(uint64_t size, size_t align)
    {
        return funcGenerator_.nextStackObject(size, align);
    }
    SymbolTable& currScope() { return funcGenerator_.currScope(); }
    std::list<Tac::Quad>::iterator emit(Tac::Quad&& quad) { return funcGenerator_.emit(std::move(quad)); }
    std::list<Tac::Quad>::iterator lastQuad() { return funcGenerator_.lastQuad(); }
    void pushTempStackObject(const Tac::StackObject& s) { funcGenerator_.pushTempStackObject(s); }
    int addLiteral(Tac::StaticObject const& o) { return funcGenerator_.addLiteral(o); }
    void addGlobalVar(std::string const& n, Tac::StaticObject const& o)
    {
        funcGenerator_.addGlobalVar(n, o);
    }
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
    explicit ValueGenerator(FuncGenerator& t) : FuncUtil(t) {}
    ValueGenerator(FuncGenerator& t, Tac::Reg aim);

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
    std::optional<Tac::Reg> target_;
    Tac::Reg cast(Tac::Reg reg,
                  const std::shared_ptr<Type>& from, const std::shared_ptr<Type>& to,
                  bool inplace);

    /// an substitution of 'nextReg()'
    Tac::Reg regToWrite() {
        if (target_.has_value()) {
            return target_.value();
        } else {
            return nextReg();
        }
    }

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
