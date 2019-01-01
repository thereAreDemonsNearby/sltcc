//
// Created by shiletong OperatorType 17-10-14.
//

#ifndef C_LEXER_TOKEN_H
#define C_LEXER_TOKEN_H

#include <string>
#include <cassert>
#include <map>


struct TokenContext
{
    int pos = -1;
    int linum = -1;
};

class Token
{

public:
    enum TokenType
    {
        Eof,
        Invalid,
        IntLiteral, DoubleLiteral, StringLiteral, CharLiteral,
        BoolLiteral, FloatLiteral, LongDoubleLiteral, LongLiteral, LongLongLiteral,
        UnsignedLiteral, UnsignedLongLiteral, UnsignedLongLongLiteral,// literals
        Operator,
        Keyword,
        Semicolon, Comma,
        LParen, RParen, LBracket, RBracket, LBrace, RBrace,
        VarArgs, /* ... */
        Name // variable name or function name or user-defined type name
    };

    enum OperatorType
    {
        Add, Sub, Mult, Div, Mod, Eq, Ne, Smlr, Grtr, Se, Ge, // arith and compare
        And, Or, Not,                                     // logic
        BitAnd, BitOr, BitXor, BitInv, SftL, SftR,                // bit operation
        Dot, Arrow,                                        // deref . ->
        Assign, AddAssign, SubAssign, MultAssign, DivAssign, ModAssign,
        BitAndAssign, BitOrAssign, BitXorAssign, SftLAssign, SftRAssign,
        Incr, Decr, // ++ --
        Question, Colon, // ? :
        Condition // use in ast
    };

    enum KeywordType
    {
        Unsigned, Long, Short, Int, Char, Double, Float, Void, Const, Static, Extern, Inline,
        Do, While, For, Continue, Break,
        Switch, Case, Default, If, Else, Goto, Return,
        Struct, Union, Enum, Typedef, Sizeof
    };

    static std::map<Token::KeywordType, std::string> kwdToStr;
    static std::map<Token::OperatorType, std::string> optorToStr;

    Token()
    {
        type_ = TokenType::Invalid;
    }

    Token(const Token &);
    ~Token();
    Token &operator=(const Token &);

    // modifier
    void setInvalid();
    void setLiteral(const std::string &);
    void setLiteral(int);
    void setLiteral(unsigned int);
    void setLiteral(double);
    void setLiteral(char);
    void setKeyword(KeywordType);
    void setName(const std::string &);
    void setOperator(OperatorType);
    void setSingleChar(char);
    void setVarArgs();
    void setEof()
    {
        checkAndDestroy();
        type_ = TokenType::Eof;
    }

    void setContext(int pos, int linum)
    { context_.pos = pos; context_.linum = linum; }

    // observer
    TokenType type() const;
    KeywordType keyword() const;
    std::string name() const;
    OperatorType getOperator() const;
    int intLiteral() const;
    unsigned uintLiteral() const;
    double doubleLiteral() const;
    std::string stringLiteral() const;
    char charLiteral() const;
    bool notEofOrInvalid() const;
    bool invalid() const {
        return type_ == TokenType::Invalid;
    }
    bool eof() const
    {
        return type_ == TokenType::Eof;
    }
    std::string toString() const;
    TokenContext context() const { return context_; }

	static const Token invalidToken;

private:
    TokenType type_;
	TokenContext context_;
    union
    {
        std::string str_;
        double dval_;
        int ival_;
        unsigned uival_;
        char cval_;
        OperatorType optor_;
        KeywordType kwd_;
    };

    bool hasString() const
    {
        return type_ == TokenType::StringLiteral ||
               type_ == TokenType::Name;
    }

    void checkAndDestroy()
    {
        if (hasString()) {
            str_.~basic_string();
        }
    }

    void checkAndSetStr(const std::string &s, TokenType t)
    {
        if (hasString()) {
            str_ = s;
        } else {
            new(&str_) std::string(s);
        }
        type_ = t;
    }

    void copyUnionTrivial(const Token& rhs)
    {
        assert(!rhs.hasString());
        switch (rhs.type_) {
        case TokenType::IntLiteral:
            ival_ = rhs.ival_;
            break;
        case TokenType::DoubleLiteral:
            dval_ = rhs.dval_;
            break;
        case TokenType::CharLiteral:
            cval_ = rhs.cval_;
            break;
        case TokenType::Operator:
            optor_ = rhs.optor_;
            break;
        case TokenType::Keyword:
            kwd_ = rhs.kwd_;
            break;
        }
    }
};


#endif //C_LEXER_TOKEN_H
