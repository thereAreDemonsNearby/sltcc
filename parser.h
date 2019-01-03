#ifndef SHITCC_PARSER_H
#define SHITCC_PARSER_H

#include <stack>
#include <algorithm>
#include "ast.h"
#include "lex.h"


class ErrorLog;
class TokenStream;

class Parser
{
public:
    static constexpr int stopErrorNum = 10;

    explicit Parser(TokenStream&, ErrorLog&);

    std::shared_ptr<ASTRoot> goal();

    // declarations and definitions
    std::shared_ptr<ASTNode> parseFuncDeclOrDef(std::shared_ptr<Type> retType);
    std::shared_ptr<FuncDef> parseFuncDef(
            std::pair<std::shared_ptr<FuncType>, std::vector<std::string>>&, const Token*);
    std::shared_ptr<CompoundDecl> parseCompoundDecl();
    std::shared_ptr<CompoundDef> parseCompoundDef();
    std::shared_ptr<ASTNode> parseTypeDeclOrDef();
    std::shared_ptr<VarDef> parseVarDef(std::shared_ptr<Type> typeHead);
    std::shared_ptr<EnumDef> parseEnumDef();

    // statements and expressions:
    // statements:
    std::shared_ptr<Stmt> parseStmt();
    std::shared_ptr<DoWhileStmt> parseDoWhileStmt();
    std::shared_ptr<WhileStmt> parseWhileStmt();
    std::shared_ptr<ForStmt> parseForStmt();
    std::shared_ptr<IfStmt> parseIfStmt();
    std::shared_ptr<SwitchStmt> parseSwitchStmt();
    std::shared_ptr<BreakStmt> parseBreakStmt();
    std::shared_ptr<ReturnStmt> parseReturnStmt();
    std::shared_ptr<Block> parseBlock(SymbolTable* outer, bool isFuncBody = false);

    // expressions:
    std::shared_ptr<Expr> parseExpr();
    std::shared_ptr<Expr> parsePostfix(std::shared_ptr<Expr> prev);
    std::shared_ptr<Expr> parsePrimary();
    std::shared_ptr<MemberExpr> parseMemberExtract(std::shared_ptr<Expr>, Token const*);
    std::shared_ptr<Expr> parsePrefix();
    std::shared_ptr<Expr> parseMultiplicative();
    std::shared_ptr<Expr> parseAdditive();
    std::shared_ptr<Expr> parseShift();
    std::shared_ptr<Expr> parseRelational(); //< > <= >=
    std::shared_ptr<Expr> parseEquality(); // == !=
    std::shared_ptr<Expr> parseBitAnd();
    std::shared_ptr<Expr> parseBitXor();
    std::shared_ptr<Expr> parseBitOr();
    std::shared_ptr<Expr> parseLogicalAnd();
    std::shared_ptr<Expr> parseLogicalOr();
    std::shared_ptr<Expr> parseConditional();
    std::shared_ptr<Expr> parseAssignment(); // = += -= ...

    std::shared_ptr<Expr> parseExprInForHead(bool last = false);

    template<typename Func>
    std::shared_ptr<Expr>
    parseBinaryOp(std::vector<Token::OperatorType> li, const Func& fn);


    // about type:
    std::pair<std::shared_ptr<FuncType>, std::vector<std::string>>
        parseFuncHead(std::shared_ptr<Type> retType);
    // a simple wrapper for parseTypeHead and parseTypeRest
    std::pair<std::shared_ptr<Type>, std::string> parseCompleteType();
    // return (type, name) pair
    // don't process semicolon(;), because of such case: void fun(int a, float b);
    std::pair<std::shared_ptr<Type>, std::string>
        parseTypeRest(std::shared_ptr<Type> typeHead);
    std::shared_ptr<Type> parseTypeHead();
    std::shared_ptr<Type> parseBasicType();
    BuiltInType::SpecifierHolder parseSpecifierList();
private:
    TokenStream& tokens_;
    ErrorLog& errors_;
    std::stack<SymbolTable*> scopeStack_;
    bool funcArgMode = false;

    SymbolTable& currScope() {
        //assert(!scopeStack_.empty());
        return *scopeStack_.top();
    }

    bool isAboutType(const Token&);
    void skipErrorParts();
    void skipUntil(Token::TokenType);
    bool isTypeSpecifier(const Token&);
    bool isUserDefinedType(Type::Tag);
};

template<typename Func>
std::shared_ptr<Expr>
Parser::parseBinaryOp(std::vector<Token::OperatorType> li, const Func& fn)
{
    auto lhs = fn(*this);
    auto p2peek = &tokens_.peek();
    while (p2peek->type() == Token::Operator &&
           std::find(li.begin(), li.end(), p2peek->getOperator()) != li.end()) {
        auto op = p2peek->getOperator();
        tokens_.next();
        auto rhs = fn(*this);
        lhs = std::make_shared<BinaryOpExpr>(p2peek, lhs, rhs, op);

        p2peek = &tokens_.peek();
    }

    return lhs;
}

#endif //SHITCC_PARSER_H
