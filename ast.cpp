#include "ast.h"
#include "visitor.h"
#include "token.h"

ASTNode::ASTNode(const Token* tok)
        : tok_(tok)
{
}

Declaration::Declaration(const Token* tok)
        : ASTNode(tok)
{
}

Definition::Definition(const Token* tok)
        : ASTNode(tok)
{
}

Stmt::Stmt(const Token* tok)
    : ASTNode(tok)
{
}

IfStmt::IfStmt(const Token* tok, const std::shared_ptr<Expr>& cond,
               const std::shared_ptr<Stmt>& t, const std::shared_ptr<Stmt>& e)
: Stmt(tok), cond_(cond), then_(t), else_(e)
{}

void IfStmt::accept(Visitor& v)
{
    v.visit(this);
}

WhileStmt::WhileStmt(const Token* tok, const std::shared_ptr<Expr>& cond,
                     const std::shared_ptr<Stmt>& body)
        : Stmt(tok), cond_(cond), body_(body)
{}

void WhileStmt::accept(Visitor& v)
{
    v.visit(this);
}

ForStmt::ForStmt(const Token* tok,
        const std::shared_ptr<Expr>& init,
        const std::shared_ptr<Expr>& cond,
        const std::shared_ptr<Expr>& stepby,
        const std::shared_ptr<Stmt>& body)
        : Stmt(tok), init_(init), cond_(cond), stepby_(stepby), body_(body)
{}

void ForStmt::accept(Visitor& v)
{
    v.visit(this);
}


DoWhileStmt::DoWhileStmt(const Token* tok, const std::shared_ptr<Expr>& cond,
                     const std::shared_ptr<Stmt>& body)
        : Stmt(tok), cond_(cond), body_(body)
{}

void DoWhileStmt::accept(Visitor& v)
{
     v.visit(this);
}

LabelStmt::LabelStmt(const Token* tok, const std::string& name, const std::shared_ptr<Stmt>& stmt)
    : Stmt(tok), label_(name), follow_(stmt)
{}

void LabelStmt::accept(Visitor& v)
{
    v.visit(this);
}

CaseStmt::CaseStmt(const Token* tok, int n, std::vector<std::shared_ptr<Stmt>>&& v)
        : Stmt(tok), caseNum_(n), follows_(std::move(v))
{
}

void CaseStmt::accept(Visitor& v)
{
    v.visit(this);
}

SwitchStmt::SwitchStmt(const Token* tok, std::vector<std::shared_ptr<CaseStmt>>&& cases,
                        const std::shared_ptr<Stmt>& dft)
    : Stmt(tok), cases_(std::move(cases)), default_(dft)
{
}

void SwitchStmt::accept(Visitor& v)
{
    v.visit(this);
}

ReturnStmt::ReturnStmt(const Token* tok, const std::shared_ptr<Expr>& expr)
    : Stmt(tok), ret_(expr)
{
}

void ReturnStmt::accept(Visitor& v)
{
    v.visit(this);
}

BreakStmt::BreakStmt(const Token* tok, const std::shared_ptr<Stmt>& outer)
    : Stmt(tok), outer_(outer)
{}

void BreakStmt::accept(Visitor& v)
{
    v.visit(this);
}

Block::Block(const Token* tok, std::vector<std::shared_ptr<ASTNode>>&& vec,
             SymbolTable* outer, bool isFuncBody)
        : Stmt(tok), stmts_(std::move(vec)), symtab_(outer, isFuncBody)
{
}

Block::Block(const Token* tok, SymbolTable* outer, bool isFuncBody)
        : Stmt(tok), symtab_(outer, isFuncBody)
{
}

void Block::accept(Visitor& v)
{
    v.visit(this);
}

void Block::add(const std::shared_ptr<ASTNode>& n)
{
    stmts_.push_back(n);
}

CompoundDecl::CompoundDecl(const Token* tok, const std::string& name,
                       const std::shared_ptr<IncompleteType>& type)
    : Declaration(tok), name_(name), type_(type)
{
}

void CompoundDecl::accept(Visitor& v)
{
    v.visit(this);
}

FuncDecl::FuncDecl(const Token* tok, const std::string& name, const std::shared_ptr<FuncType>& type)
     : Declaration(tok), name_(name), type_(type)
{
}

void FuncDecl::accept(Visitor& v)
{
    v.visit(this);
}

UnaryOpExpr::UnaryOpExpr(const Token* t, const std::shared_ptr<Expr>& e, Token::OperatorType op)
    : Expr(t), operand_(e), operator_(op)
{
    tag_ = ExprTag::Unary;
}

void UnaryOpExpr::accept(Visitor& v)
{
    v.visit(this);
}

BinaryOpExpr::BinaryOpExpr(const Token* tok, const std::shared_ptr<Expr>& lhs,
                           const std::shared_ptr<Expr>& rhs, Token::OperatorType op)
    : Expr(tok), lhs_(lhs), rhs_(rhs), operator_(op)
{
    tag_ = ExprTag::Binary;
}

void BinaryOpExpr::accept(Visitor& v)
{
    v.visit(this);
}

ConditionExpr::ConditionExpr(const Token* tok, const std::shared_ptr<Expr>& cond,
                             const std::shared_ptr<Expr>& yes, const std::shared_ptr<Expr>& no)
    : Expr(tok), cond_(cond), yes_(yes), no_(no)
{
    tag_ = ExprTag::Condition;
}

void ConditionExpr::accept(Visitor& v)
{
    v.visit(this);
}

FuncCallExpr::FuncCallExpr(const Token* tok, const std::string& name,
                   std::vector<std::shared_ptr<Expr>>&& argli)
    : Expr(tok), funcName_(name), args_(argli)
{
    tag_ = ExprTag::FuncCall;
}

void FuncCallExpr::accept(Visitor& v)
{
    v.visit(this);
}

FuncCallExpr::FuncCallExpr(const Token* tok, std::string n)
    : Expr(tok), funcName_(std::move(n))
{
    tag_ = ExprTag::FuncCall;
}

void FuncCallExpr::addArg(const std::shared_ptr<Expr>& arg)
{
    args_.push_back(arg);
}

MemberExpr::MemberExpr(const Token* tok, const std::shared_ptr<Expr>& lhs, const std::string& n)
    : Expr(tok), compound_(lhs), memberName_(n)
{
    tag_ = ExprTag::Member;
}

void MemberExpr::accept(Visitor& v)
{
    v.visit(this);
}

VarExpr::VarExpr(const Token* t, const std::string& n)
    : Expr(t), name_(n)
{
    tag_ = ExprTag::Var;
}

void VarExpr::accept(Visitor& v)
{
    v.visit(this);
}

CastExpr::CastExpr(const Token* tok, const std::shared_ptr<Type>& ty, const std::shared_ptr<Expr>& n)
    : Expr(tok), castTo_(ty), expr_(n)
{
    tag_ = ExprTag::Cast;
}

void CastExpr::accept(Visitor& v)
{
    v.visit(this);
}

LiteralExpr::LiteralExpr(const Token* tok)
    : Expr(tok)
{
    tag_ = ExprTag::Literal;
}

void LiteralExpr::accept(Visitor& v)
{
    v.visit(this);
}

CompoundDef::CompoundDef(const Token* tok, const std::string& n,
                         const std::shared_ptr<CompoundType>& ty)
    : Definition(tok), name_(n), type_(ty)
{
}

void CompoundDef::accept(Visitor& v)
{
    v.visit(this);
}

TypeAlias::TypeAlias(const Token* tok, const std::string& n,
                     const std::shared_ptr<AliasType>& ty)
    : Definition(tok), name_(n), type_(ty)
{}

void TypeAlias::accept(Visitor& v)
{
    v.visit(this);
}

EnumDef::EnumDef(const Token* tok, const std::string& n, const std::shared_ptr<EnumType>& ty)
        : Definition(tok), name_(n), type_(ty)
{}

void EnumDef::accept(Visitor& v)
{
    v.visit(this);
}

void FuncDef::accept(Visitor& v)
{
    v.visit(this);
}

FuncDef::FuncDef(const Token* tok, const std::string& name, const std::shared_ptr<FuncType>& ty,
                 ListSymtab&& p)
        : Definition(tok), name_(name), type_(ty), params_(std::move(p))
{
}


FuncDef::FuncDef(const Token* tok, const std::string& name, const std::shared_ptr<FuncType>& ty)
        : Definition(tok), name_(std::move(name)), type_(ty)
{
}


void ASTRoot::accept(Visitor& v)
{
    v.visit(this);
}


VarDef::VarDef(const Token* tok, std::string name, const std::shared_ptr<Type>& ty)
        : Definition(tok), name_(std::move(name)), type_(ty)
{
}

VarDef::VarDef(const Token* tok, std::string name, const std::shared_ptr<Type>& ty,
                 const std::shared_ptr<Expr>& init)
        : Definition(tok), name_(std::move(name)), type_(ty), init_(init)
{

}

void VarDef::accept(Visitor& v)
{
    v.visit(this);
}



SizeofExpr::SizeofExpr(const Token* tok, const std::shared_ptr<Type>& ty)
        : Expr(tok), type_(ty), val_(ty->width())
{
    tag_ = ExprTag::Sizeof;
}

void SizeofExpr::accept(Visitor& v)
{
    v.visit(this);
}

void ArrayRefExpr::accept(Visitor& v)
{
    v.visit(this);
}

ArrayRefExpr::ArrayRefExpr(const Token* tok, std::shared_ptr<Expr> head, std::shared_ptr<Expr> index)
        : Expr(tok), head_(std::move(head)), index_(std::move(index))
{
    tag_ = ExprTag::ArrayRef;
}

EmptyExpr::EmptyExpr(const Token* tok)
        : Expr(tok)
{
    tag_ = ExprTag::Empty;
}

void EmptyExpr::accept(Visitor& v)
{
    v.visit(this);
}