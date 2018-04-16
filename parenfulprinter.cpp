#include <iostream>
#include "parenfulprinter.h"

using namespace std;

void ParenfulPrinter::visit(IfStmt* node)
{
    cout << "(if ";
    node->cond_->accept(*this);
    cout << "\n(then\n";
    node->then_->accept(*this);
    cout << ")\n(else ";
    if (node->else_)
        node->else_->accept(*this);
    else
        cout << "(empty)";
    cout << "))\n";
}

void ParenfulPrinter::visit(WhileStmt* node)
{
    cout << "(while ";
    node->cond_->accept(*this);
    cout << "\n(";
    node->body_->accept(*this);
    cout << "))\n";
}

void ParenfulPrinter::visit(ForStmt* node)
{
    cout << "(for ";
    node->init_->accept(*this);
    cout << " ";
    node->cond_->accept(*this);
    cout << " ";
    node->stepby_->accept(*this);
    cout << "\n(";
    node->body_->accept(*this);
    cout << "))\n";
}

void ParenfulPrinter::visit(DoWhileStmt* node)
{
    cout << "(do-while ";
    node->cond_->accept(*this);
    cout << "\n(";
    node->body_->accept(*this);
    cout << "))\n";
}

void ParenfulPrinter::visit(LabelStmt* node)
{
    // TODO not implemented
}

void ParenfulPrinter::visit(CaseStmt* node)
{
    // TODO
}

void ParenfulPrinter::visit(SwitchStmt* node)
{
    // TODO
}

void ParenfulPrinter::visit(ReturnStmt* node)
{
    cout << "(return ";
    node->ret_->accept(*this);
    cout << ")\n";
}

void ParenfulPrinter::visit(BreakStmt* node)
{
    // TODO
}

void ParenfulPrinter::visit(CompoundDecl* node)
{
    cout << node->type_->toString() << endl;
}

void ParenfulPrinter::visit(VarDef* node)
{
    cout << "(vardef " << node->name_ << " " << node->type_->toString();
    if (node->init_) {
        cout << "\n  (init: ";
        node->init_->accept(*this);
        cout << ")\n";
    }
    cout << ")\n";
}

void ParenfulPrinter::visit(FuncDecl* node)
{
    cout << "(funcdecl " << node->type_->toString();
    cout << ")\n";
}

void ParenfulPrinter::visit(Block* node)
{
    cout << "(block\n";
    for (auto stmt : node->stmts_) {
        stmt->accept(*this);
    }
    cout << ")" << endl;
}

void ParenfulPrinter::visit(UnaryOpExpr* node)
{
    cout << "(" << Token::optorToStr[node->operator_] << " ";
    node->operand_->accept(*this);
    cout << ")";
}

void ParenfulPrinter::visit(BinaryOpExpr* node)
{
    cout << "(" << Token::optorToStr[node->operator_] << " ";
    node->lhs_->accept(*this);
    cout << " ";
    node->rhs_->accept(*this);
    cout << ")";
}

void ParenfulPrinter::visit(ConditionExpr* node)
{
    cout << "(?: ";
    node->cond_->accept(*this);
    cout << " ";
    node->yes_->accept(*this);
    cout << " ";
    node->no_->accept(*this);
    cout << ")";
}

void ParenfulPrinter::visit(FuncCall* node)
{
    cout << "(" << node->funcName_ << " ";
    for (auto& item : node->args_) {
        item->accept(*this);
        cout << " ";
    }
    cout << ")\n";
}

void ParenfulPrinter::visit(MemberExpr* node)
{
    cout << "(. ";
    node->compound_->accept(*this);
    cout << " ";
    cout << node->memberName_;
    cout << ")\n";
}

void ParenfulPrinter::visit(ArrayRefExpr* node)
{
    cout << "(arrref: ";
    node->head_->accept(*this);
    cout << " ";
    node->index_->accept(*this);
    cout << ")\n";
}

void ParenfulPrinter::visit(VarExpr* node)
{
    cout << "var:" << node->name_ << "";
}

void ParenfulPrinter::visit(CastExpr* node)
{
    cout << "(cast " << node->castTo_->toString() << " ";
    node->expr_->accept(*this);
    cout << ")";
}

void ParenfulPrinter::visit(LiteralExpr* node)
{
    switch (node->tok_->type()) {
    case Token::IntLiteral: cout << node->tok_->intLiteral(); break;
    case Token::DoubleLiteral: cout << node->tok_->doubleLiteral(); break;
    case Token::CharLiteral: cout << node->tok_->charLiteral(); break;
    case Token::StringLiteral: cout << "\"" << node->tok_->stringLiteral() << "\""; break;
    default: cout << "wrongLiteral"; break;
    }
}

void ParenfulPrinter::visit(CompoundDef* node)
{
    cout << "(compound-def ";
    cout << node->type_->toString() << ")\n";
}

void ParenfulPrinter::visit(TypeAlias* node)
{
    // TODO
}

void ParenfulPrinter::visit(EnumDef* node)
{
    // TODO
}

void ParenfulPrinter::visit(FuncDef* node)
{
    cout << "(funcdef " << node->type_->toString();
    cout << "\n(params: ";
    for (auto it = node->params_.begin(); it != node->params_.end(); ++it) {
        cout << it->first << " ";
    }
    cout << ")\n";
    node->body_->accept(*this);
    cout << ")\n";
}

void ParenfulPrinter::visit(ASTRoot* node)
{
    for (auto& item : node->contents_) {
        item->accept(*this);
    }
}

void ParenfulPrinter::visit(SizeofExpr* node)
{
    cout << "(sizeof ";
    cout << node->type_->toString() << ")";
}

void ParenfulPrinter::visit(EmptyExpr* node)
{
    cout << "(empty)\n";
}
