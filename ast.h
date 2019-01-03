#ifndef AST_H_
#define AST_H_

#include <vector>
#include <memory>
#include "token.h"
#include "SymbolTable.h"
#include "type.h"

// forward declaration
class Visitor;

class ParenfulPrinter;
class TypeChecker;

class Type;

class ASTNode;

class Declaration;

class Definition;

class Stmt;

class IfStmt;

class WhileStmt;

class ForStmt;

class DoWhileStmt;

class TypeDecl;

class VarDef;

class FuncDecl;

class FuncDef;

class Block;

class Expr;

class UnaryOpExpr;

class BinaryOpExpr;

class FuncCallExpr;

class Definition;

class CompoundDef;

class UnionDef;

class TypeAlias;

class EnumDef;

class ASTree;

#define FRIEND_LIST \
    friend class Visitor; \
    friend class ParenfulPrinter; \
    friend class TypeChecker; \
    friend class TacGenerator; \
    friend class FuncGenerator; \
    friend class BranchGenerator; \
    friend class ValueGenerator; \
    friend class LValueGenerator;


class ASTNode
{
    FRIEND_LIST

public:
    virtual ~ASTNode() = default;

    virtual void accept(Visitor&) = 0;

    explicit ASTNode(const Token*); // TODO : maybe a better constructor is needed
    Token const* tok() const
    { return tok_; }

protected:
    const Token* tok_;
};

class Declaration : public ASTNode
{
    FRIEND_LIST

public:
    explicit Declaration(const Token*);

    ~Declaration() override = default;

    void accept(Visitor&) override = 0;
};

class Definition : public ASTNode
{
    FRIEND_LIST

public:
    explicit Definition(const Token*);

    ~Definition() override = default;

    void accept(Visitor&) override = 0;
};

class Stmt : public ASTNode
{
    FRIEND_LIST

public:
    explicit Stmt(const Token*);

    ~Stmt() override = default;

    void accept(Visitor&) override = 0;

};

class IfStmt final : public Stmt
{
    FRIEND_LIST

public:
    IfStmt(const Token*, const std::shared_ptr<Expr>&,
           const std::shared_ptr<Stmt>&, const std::shared_ptr<Stmt>&);

    void accept(Visitor&) override;

private:
    std::shared_ptr<Expr> cond_;
    std::shared_ptr<Stmt> then_;
    std::shared_ptr<Stmt> else_;
};

class WhileStmt final : public Stmt
{
    FRIEND_LIST

public:
    WhileStmt(const Token*, const std::shared_ptr<Expr>&,
              const std::shared_ptr<Stmt>&);

    void accept(Visitor&) override;

private:
    std::shared_ptr<Expr> cond_;
    std::shared_ptr<Stmt> body_;
};

class ForStmt final : public Stmt
{
    FRIEND_LIST

public:
    ForStmt(const Token*,
            const std::shared_ptr<Expr>&,
            const std::shared_ptr<Expr>&,
            const std::shared_ptr<Expr>&,
            const std::shared_ptr<Stmt>&);

    void accept(Visitor&) override;

private:
    std::shared_ptr<Expr> init_; // I don't support 'for(int i=1;...;...)'
    std::shared_ptr<Expr> cond_;
    std::shared_ptr<Expr> stepby_;
    std::shared_ptr<Stmt> body_;
};

class DoWhileStmt final : public Stmt
{
    FRIEND_LIST

public:
    DoWhileStmt(const Token*, const std::shared_ptr<Expr>&,
                const std::shared_ptr<Stmt>&);

    void accept(Visitor&) override;

private:
    std::shared_ptr<Expr> cond_;
    std::shared_ptr<Stmt> body_;
};

class LabelStmt final : public Stmt
{
    FRIEND_LIST

public:
    LabelStmt(const Token*, const std::string&, const std::shared_ptr<Stmt>&);

    void accept(Visitor& v);

private:
    std::string label_;
    std::shared_ptr<Stmt> follow_;
};

class CaseStmt final : public Stmt
{
    FRIEND_LIST

public:
    CaseStmt(const Token*, int, std::vector<std::shared_ptr<Stmt>>&&);

    void accept(Visitor& v) override;

private:
    int caseNum_;
    std::vector<std::shared_ptr<Stmt>> follows_;
};

class SwitchStmt final : public Stmt
{
    FRIEND_LIST

public:
    SwitchStmt(const Token*, std::vector<std::shared_ptr<CaseStmt>>&&,
               const std::shared_ptr<Stmt>& d);

    void accept(Visitor& v) override;

private:
    std::vector<std::shared_ptr<CaseStmt>> cases_;
    std::shared_ptr<Stmt> default_;
};

class ReturnStmt final : public Stmt
{
    FRIEND_LIST

public:
    ReturnStmt(const Token*, const std::shared_ptr<Expr>&);

    void accept(Visitor& v) override;

private:
    std::shared_ptr<Expr> retExpr_;
    FuncDef* func_ = nullptr;
};

class BreakStmt final : public Stmt
{
    FRIEND_LIST

public:
    BreakStmt(const Token*, const std::shared_ptr<Stmt>&);

    void accept(Visitor&) override;

private:
    std::weak_ptr<Stmt> outer_;
};



class CompoundDecl final : public Declaration
{
    FRIEND_LIST

public:
    CompoundDecl(const Token*, const std::string&, const std::shared_ptr<IncompleteType>&);

//    CompoundType::MemModel model() const { return cat_->model(); }
    void accept(Visitor& v) override;

private:
    std::string name_;
    std::shared_ptr<IncompleteType> type_;
};

class VarDef final : public Definition
{
    FRIEND_LIST

public:
    VarDef(const Token*, std::string name, const std::shared_ptr<Type>&);

    VarDef(const Token*, std::string name, const std::shared_ptr<Type>&,
            const std::shared_ptr<Expr>& init);

    void accept(Visitor&) override;

private:
    std::string name_;
    std::shared_ptr<Type> type_;
    std::shared_ptr<Expr> init_;
};

class FuncDecl final : public Declaration
{
    FRIEND_LIST

public:
    FuncDecl(const Token* tok, const std::string& name,
             const std::shared_ptr<FuncType>& type);

    void accept(Visitor&) override;

private:
    std::string name_;
    std::shared_ptr<FuncType> type_;
};

class Block final : public Stmt
{
    FRIEND_LIST

public:
    void accept(Visitor&) override;

    Block(const Token*, std::vector<std::shared_ptr<ASTNode>>&&,
          SymbolTable*, bool isFuncBody = false);

    Block(const Token*, SymbolTable* outer, bool isFuncBody = false);

    void add(const std::shared_ptr<ASTNode>&);

    SymbolTable& scope()
    { return symtab_; }

private:
    std::vector<std::shared_ptr<ASTNode>> stmts_;
    HashSymtab symtab_;
};

enum class ExprTag {
    Empty, Sizeof, Unary, Binary, Condition,
    FuncCall, Member, ArrayRef, Var, Cast, Literal,
};

class Expr : public Stmt
{
    FRIEND_LIST

public:


    ~Expr() override
    {};

    void accept(Visitor&) override = 0;

    explicit Expr(const Token* t) : Stmt(t)
    {}

    virtual bool isLiteral() { return false; }
    ExprTag tag() { return tag_; }

protected:
    std::shared_ptr<Type> evalType;
    std::shared_ptr<Type> promotedTo;
    bool isLValue = false;
    bool isCond = false;
    ExprTag tag_;
};

class EmptyExpr final : public Expr
{
    FRIEND_LIST

public:
    explicit EmptyExpr(const Token*);
    void accept(Visitor&) override;
};

class SizeofExpr final : public Expr
{
    FRIEND_LIST

public:
    SizeofExpr(const Token*, const std::shared_ptr<Type>&);

    void accept(Visitor&) override;

private:
    std::shared_ptr<Type> type_;
    size_t val_;
};

class UnaryOpExpr final : public Expr
{
    FRIEND_LIST

public:
    void accept(Visitor&) override;

    UnaryOpExpr(const Token*, const std::shared_ptr<Expr>&, Token::OperatorType);

private:
    std::shared_ptr<Expr> operand_;
    Token::OperatorType operator_;
};

class BinaryOpExpr final : public Expr
{
    FRIEND_LIST

public:
    void accept(Visitor&) override;

    BinaryOpExpr(const Token*, const std::shared_ptr<Expr>&,
                 const std::shared_ptr<Expr>&, Token::OperatorType);

private:
    std::shared_ptr<Expr> lhs_;
    std::shared_ptr<Expr> rhs_;
    Token::OperatorType operator_;     // TODO : maybe duplicate
};

// ? :
class ConditionExpr final : public Expr
{
    FRIEND_LIST

public:
    void accept(Visitor&) override;

    ConditionExpr(const Token*, const std::shared_ptr<Expr>&,
                  const std::shared_ptr<Expr>&, const std::shared_ptr<Expr>&);

private:
    std::shared_ptr<Expr> cond_;
    std::shared_ptr<Expr> yes_;
    std::shared_ptr<Expr> no_;
};

class FuncCallExpr final : public Expr
{
    FRIEND_LIST

public:
    void accept(Visitor&) override;

    FuncCallExpr(const Token*, const std::string& name,
             std::vector<std::shared_ptr<Expr>>&&);

    FuncCallExpr(const Token*, std::string);

    void addArg(const std::shared_ptr<Expr>&);

private:
    std::string funcName_;
    std::vector<std::shared_ptr<Expr>> args_;
};

// point.x
class MemberExpr final : public Expr
{
    FRIEND_LIST

public:
    MemberExpr(const Token*, const std::shared_ptr<Expr>&, const std::string&);

    void accept(Visitor&) override;

private:
    std::shared_ptr<Expr> compound_;
    std::string memberName_;
};


// array ref expr
class ArrayRefExpr final : public Expr
{
    FRIEND_LIST

public:
    void accept(Visitor&) override;

    ArrayRefExpr(const Token*, std::shared_ptr<Expr>, std::shared_ptr<Expr>);

private:
    std::shared_ptr<Expr> head_;
    std::shared_ptr<Expr> index_;
};

// single variable
class VarExpr final : public Expr
{
    FRIEND_LIST

public:
    void accept(Visitor&) override;

    VarExpr(const Token*, const std::string&);

    std::string varName() { return name_; }
private:
    std::string name_;
};

// (int)3.1415926
class CastExpr final : public Expr
{
    FRIEND_LIST

public:
    CastExpr(const Token*, const std::shared_ptr<Type>&,
             const std::shared_ptr<Expr>&);

    void accept(Visitor&) override;

private:
    std::shared_ptr<Type> castTo_;
    std::shared_ptr<Expr> expr_;
};


class LiteralExpr final : public Expr
{
    FRIEND_LIST

public:
    explicit LiteralExpr(const Token*);

    void accept(Visitor&) override;

    bool isLiteral() override { return true; }
private:
    // because base class ASTNode has member Token
    // so here empty
};


class CompoundDef final : public Definition
{
    FRIEND_LIST

public:
    CompoundDef(const Token* tok, const std::string& n,
                const std::shared_ptr<CompoundType>& ty);

    void accept(Visitor& v) override;

    // for test:
    std::shared_ptr<CompoundType> type() { return type_; }
private:
    std::string name_;
    std::shared_ptr<CompoundType> type_;
};

class TypeAlias final : public Definition // really typedef
{
    FRIEND_LIST

public:
    TypeAlias(const Token* tok, const std::string& n,
              const std::shared_ptr<AliasType>& ty);

    void accept(Visitor& v) override;

private:
    std::string name_;
    std::shared_ptr<AliasType> type_;
};

class EnumDef final : public Definition
{
    FRIEND_LIST

public:
    EnumDef(const Token* tok, const std::string& n,
            const std::shared_ptr<EnumType>& ty);

    void accept(Visitor& v) override;

private:
    std::string name_;
    std::shared_ptr<Type> type_;
};

class FuncDef final : public Definition
{
    FRIEND_LIST

public:
    FuncDef(const Token* tok, const std::string& name,
            const std::shared_ptr<FuncType>& ty,
            ListSymtab&& p);

    FuncDef(const Token* tok, const std::string& name,
            const std::shared_ptr<FuncType>& ty);

    void accept(Visitor& v) override;

    SymbolTable& params()
    { return params_; }

    void body(const std::shared_ptr<Block>& b)
    {
        body_ = b;
    }

private:
    std::string name_;
    std::shared_ptr<FuncType> type_;
    ListSymtab params_;
    std::shared_ptr<Block> body_;
};


class ASTRoot final : public ASTNode
{
    FRIEND_LIST

public:
    ASTRoot(const Token* tok)
            : ASTNode(tok), symtab_(nullptr)
    {}

    void accept(Visitor&) override;

    SymbolTable& scope()
    { return symtab_; }

    void add(const std::shared_ptr<ASTNode>& n)
    { contents_.push_back(n); }

private:
    std::vector<std::shared_ptr<ASTNode>> contents_;
    HashSymtab symtab_;
};

#endif