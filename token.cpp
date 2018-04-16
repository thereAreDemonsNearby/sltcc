//
// Created by shiletong OperatorType 17-10-14.
//
#include <sstream>
#include <cassert>
#include "token.h"


std::map<Token::KeywordType, std::string> Token::kwdToStr = {{KeywordType::Int,      "int"},
                                                             {KeywordType::Unsigned, "unsigned"},
                                                             {KeywordType::Long,     "long"},
                                                             {KeywordType::Double,   "double"},
                                                             {KeywordType::Float,    "float"},
                                                             {KeywordType::Short,    "short"},
                                                             {KeywordType::Char,     "char"},
                                                             {KeywordType::Void,     "void"},
                                                             {KeywordType::Do,       "do"},
                                                             {KeywordType::While,    "while"},
                                                             {KeywordType::For,      "for"},
                                                             {KeywordType::Continue, "continue"},
                                                             {KeywordType::Break,    "break"},
                                                             {KeywordType::Switch,   "switch"},
                                                             {KeywordType::Case,     "case"},
                                                             {KeywordType::Default,  "default"},
                                                             {KeywordType::If,       "if"},
                                                             {KeywordType::Else,     "else"},
                                                             {KeywordType::Return,   "return"},
                                                             {KeywordType::Goto,     "goto"},
                                                             {KeywordType::Const,    "const"},
                                                             {KeywordType::Extern,   "extern"},
                                                             {KeywordType::Inline,   "inline"},
                                                             {KeywordType::Static,   "static"},
                                                             {KeywordType::Struct,   "struct"},
                                                             {KeywordType::Union,    "union"},
                                                             {KeywordType::Enum,     "enum"},
                                                             {KeywordType::Typedef,  "typedef"},
                                                             {KeywordType::Sizeof,   "sizeof"}};

std::map<Token::OperatorType, std::string> Token::optorToStr = {{OperatorType::And,          "&&"},
                                                                {OperatorType::Arrow,        "->"},
                                                                {OperatorType::BitAnd,       "&"},
                                                                {OperatorType::BitOr,        "|"},
                                                                {OperatorType::BitXor,       "^"},
                                                                {OperatorType::Div,          "/"},
                                                                {OperatorType::Dot,          "."},
                                                                {OperatorType::Eq,           "=="},
                                                                {OperatorType::Ge,           ">="},
                                                                {OperatorType::Grtr,         ">"},
                                                                {OperatorType::Mult,         "*"},
                                                                {OperatorType::Ne,           "!="},
                                                                {OperatorType::Not,          "!"},
                                                                {OperatorType::Or,           "||"},
                                                                {OperatorType::Add,          "+"},
                                                                {OperatorType::Se,           "<="},
                                                                {OperatorType::SftL,         "<<"},
                                                                {OperatorType::SftR,         ">>"},
                                                                {OperatorType::Smlr,         "<"},
                                                                {OperatorType::Sub,          "-"},
                                                                {OperatorType::Assign,       "="},
                                                                {OperatorType::Mod,          "%"},
                                                                {OperatorType::Question,     "?"},
                                                                {OperatorType::Colon,        ":"},
                                                                {OperatorType::BitInv,       "~"},
                                                                {OperatorType::Incr,         "++"},
                                                                {OperatorType::Decr,         "--"},
                                                                {OperatorType::AddAssign,    "+="},
                                                                {OperatorType::SubAssign,    "-="},
                                                                {OperatorType::MultAssign,   "*="},
                                                                {OperatorType::DivAssign,    "/="},
                                                                {OperatorType::ModAssign,    "%="},
                                                                {OperatorType::BitAndAssign, "&="},
                                                                {OperatorType::BitOrAssign,  "|="},
                                                                {OperatorType::BitXorAssign, "^="},
                                                                {OperatorType::SftLAssign,   "<<="},
                                                                {OperatorType::SftRAssign,   ">>="}};

const Token Token::invalidToken;

// implement of class Token
Token::Token(const Token &rhs)
{
    type_ = rhs.type_;
    if (rhs.hasString()) {
        // string must be constructed
        new(&str_) std::string(rhs.str_);
    } else {
        copyUnionTrivial(rhs);
    }
    context_ = rhs.context_;
}

Token::~Token()
{
    if (hasString()) {
        str_.~basic_string();
    }
}

Token &Token::operator=(const Token &rhs)
{
    bool lhsStr = hasString();
    bool rhsStr = rhs.hasString();
    type_ = rhs.type_;
    if (lhsStr) {
        if (rhsStr) {
            str_ = rhs.str_;
        } else {
            str_.~basic_string();
            copyUnionTrivial(rhs);
        }
    } else {
        if (rhsStr) {
            new(&str_) std::string(rhs.str_);
        } else {
            copyUnionTrivial(rhs);
        }
    }
    context_ = rhs.context_;
	return *this;
}


void Token::setInvalid()
{
    checkAndDestroy();
    type_ = TokenType::Invalid;
}

void Token::setLiteral(int val)
{
    checkAndDestroy();
    type_ = TokenType::IntLiteral;
    ival_ = val;
}

void Token::setLiteral(double val)
{
    checkAndDestroy();
    type_ = TokenType::DoubleLiteral;
    dval_ = val;
}

void Token::setLiteral(const std::string &s)
{
    checkAndSetStr(s, TokenType::StringLiteral);
}

void Token::setLiteral(char val)
{
    checkAndDestroy();
    type_ = TokenType::CharLiteral;
    cval_ = val;
}

void Token::setKeyword(KeywordType n)
{
    checkAndDestroy();
    kwd_ = n;
    type_ = TokenType::Keyword;
}

void Token::setName(const std::string &s)
{
    checkAndSetStr(s, TokenType::Name);
}

void Token::setSingleChar(char c)
{
    checkAndDestroy();
    switch (c) {
    case ';':
        type_ = TokenType::Semicolon;
        break;
    case ',':
        type_ = TokenType::Comma;
        break;
    case '{':
        type_ = TokenType::LBrace;
        break;
    case '}':
        type_ = TokenType::RBrace;
        break;
    case '[':
        type_ = TokenType::LBracket;
        break;
    case ']':
        type_ = TokenType::RBracket;
        break;
    case '(':
        type_ = TokenType::LParen;
        break;
    case ')':
        type_ = TokenType::RParen;
        break;

    default:
        assert(false);
    }
}

void Token::setOperator(OperatorType op)
{
    checkAndDestroy();
    optor_ = op;
    type_ = TokenType::Operator;
}

auto Token::type() const -> TokenType
{
    return type_;
}

auto Token::keyword() const -> KeywordType
{
    assert(type_ == TokenType::Keyword);
    return kwd_;
}

std::string Token::name() const
{
    assert(type_ == TokenType::Name);
    return str_;
}

std::string Token::stringLiteral() const
{
    assert(type_ == TokenType::StringLiteral);
    return str_;
}

int Token::intLiteral() const
{
    assert(type_ == TokenType::IntLiteral);
    return ival_;
}

double Token::doubleLiteral() const
{
    assert(type_ == TokenType::DoubleLiteral);
    return dval_;
}

char Token::charLiteral() const
{
    assert(type_ == TokenType::CharLiteral);
    return cval_;
}

auto Token::getOperator() const -> OperatorType
{
    assert(type_ == TokenType::Operator);
    return optor_;
}

bool Token::notEofOrInvalid() const
{
    return type_ != TokenType::Invalid && type_ != TokenType::Eof;
}

std::string Token::toString() const
{
    std::ostringstream os;
    // os << "line" << context().linum << " ";
    using tt = TokenType;
    switch (type_) {
    case tt::Invalid:
        os << "Invalid!";
        break;
    case tt::IntLiteral:
        os << "IntLiteral(" << ival_ << ")";
        break;
    case tt::DoubleLiteral:
        os << "DoubleLiteral(" << dval_ << ")";
        break;
    case tt::StringLiteral:
        os << "StringLiteral(" << str_ << ")";
        break;
    case tt::CharLiteral:
        os << "CharLiteral(" << cval_ << ")";
        break;
    case tt::Operator:
        os << "Operator" << optorToStr[optor_];
        break;
    case tt::LBrace:
        os << "LeftBrace";
        break;
    case tt::RBrace:
        os << "RightBrace";
        break;
    case tt::LBracket:
        os << "LeftBracket";
        break;
    case tt::RBracket:
        os << "RightBracket";
        break;
    case tt::LParen:
        os << "LeftParen";
        break;
    case tt::RParen:
        os << "RightParen";
        break;
    case tt::Semicolon:
        os << "Semicolon";
        break;
    case tt::Comma:
        os << "Comma";
        break;
    case tt::Name:
        os << "Name(" << str_ << ")";
        break;
    case tt::Keyword:
        os << "Keyword(" << kwdToStr[kwd_] << ")";
        break;
    case tt::Eof:
        os << "EndOfFile";
        break;
    case tt::VarArgs:
        os << "VarArgs(...)";
        break;
    default:
        assert(false);
    }
    return os.str();
}

void Token::setVarArgs()
{
    checkAndDestroy();
    type_ = TokenType::VarArgs;
}
