//
// Created by shiletong OperatorType 17-10-13.
//
#include <unordered_set>
#include <unordered_map>
#include <cassert>
#include <cctype>
#include "lex.h"
#include "error.h"

namespace
{
std::unordered_set<std::string> bltinTypeSet = {"long", "int", "short", "char",
                                                "unsigned", "double", "float", "void"};
using kn = Token::KeywordType;
std::unordered_map<std::string, Token::KeywordType> keywordMap = {{"long",     kn::Long},
                                                           {"int",      kn::Int},
                                                           {"short",    kn::Short},
                                                           {"char",     kn::Char},
                                                           {"unsigned", kn::Unsigned},
                                                           {"double",   kn::Double},
                                                           {"float",    kn::Float},
                                                           {"void",     kn::Void},
                                                           {"do",       kn::Do},
                                                           {"while",    kn::While},
                                                           {"continue", kn::Continue},
                                                           {"break",    kn::Break},
                                                           {"for",      kn::For},
                                                           {"switch",   kn::Switch},
                                                           {"case",     kn::Case},
                                                           {"default",  kn::Default},
                                                           {"if",       kn::If},
                                                           {"else",     kn::Else},
                                                           {"goto",     kn::Goto},
                                                           {"return",   kn::Return},
                                                           {"extern",   kn::Extern},
                                                           {"inline",   kn::Inline},
                                                           {"const",    kn::Const},
                                                           {"static",   kn::Static},
                                                           {"struct",   kn::Struct},
                                                           {"union",    kn::Union},
                                                           {"enum",     kn::Enum},
                                                           {"typedef",  kn::Typedef},
                                                           {"sizeof",   kn::Sizeof}};
using on = Token::OperatorType;
std::unordered_map<std::string, Token::OperatorType> operatorMap = {{"&&", on::And},
                                                             {"->", on::Arrow},
                                                             {"&",  on::BitAnd},
                                                             {"|",  on::BitOr},
                                                             {"^",  on::BitXor},
                                                             {"/",  on::Div},
                                                             {".",  on::Dot},
                                                             {"==", on::Eq},
                                                             {">=", on::Ge},
                                                             {">",  on::Grtr},
                                                             {"*",  on::Mult},
                                                             {"!=", on::Ne},
                                                             {"!",  on::Not},
                                                             {"||", on::Or},
                                                             {"+",  on::Add},
                                                             {"<=", on::Se},
                                                             {"<<", on::SftL},
                                                             {">>", on::SftR},
                                                             {"<",  on::Smlr},
                                                             {"-",  on::Sub},
                                                             {"=",  on::Assign},
                                                             {"%",  on::Mod},
                                                             {"?",  on::Question},
                                                             {":",  on::Colon},
                                                             {"~",  on::BitInv},
                                                             {"++", on::Incr},
                                                             {"--", on::Decr},
                                                             {"+=", on::AddAssign},
                                                             {"-=", on::SubAssign},
                                                             {"*=", on::MultAssign},
                                                             {"/=", on::DivAssign},
                                                             {"%=", on::ModAssign},
                                                             {"&=", on::BitAndAssign},
                                                             {"|=", on::BitOrAssign},
                                                             {"^=", on::BitXorAssign},
                                                             {"<<=", on::SftLAssign},
                                                             {">>=", on::SftRAssign}};




}

std::vector<Token> TokenStream::lex()
{
    StrIter beg = text_.cbegin();
    StrIter end = text_.cend();
    std::vector<Token> tokList;
    Token tok = nextToken(beg, end); //beg is passed by reference
    while (!tok.eof()) {
        tokList.push_back(tok);
        if (tok.invalid()) {
            errors_.add(Error{&tokList.back(), "invalid token"});
        }

        tok = nextToken(beg, end);
    }

	tokList.push_back(tok); // push back eof
	return tokList;
};

Token TokenStream::nextToken(StrIter& beg, StrIter end)
{
    Token ret;
    skipSpace(beg, end);
    if (beg == end) {
        Token t;
        t.setEof();
        t.setContext(beg - text_.begin(), currline_);
        return t;
    }

    auto pos = beg - text_.begin();
    auto line = currline_;
    char curr = *beg;
    if (std::isdigit(curr)) {
        // int or float literal
        // TODO: deal with root not singular
        Token tok = lexNumber(beg, end);
        if (tok.notEofOrInvalid()) {
            ret = tok;
        } else {
            ret.setInvalid();
        }
    } else if (curr == '\'') {
        // char literal
        auto res = lexChar(beg, end);
        if (res.second) {
            ret.setLiteral(res.first);
        } else {
            ret.setInvalid();
        }
    } else if (curr == '\"') {
        // string literal
        auto res = lexString(beg, end);
        if (res.second) {
            ret.setLiteral(res.first);
        } else {
            ret.setInvalid();
        }
    } else if (isAlphaOr_(curr)) {
        Token tok = lexSymbol(beg, end);
        if (tok.notEofOrInvalid()) {
            ret = tok;
        } else {
            ret.setInvalid();
        }
    } else {
        // !"#$%&'()*+,-./:;<=>?@[\]^_`{|}~
        Token tok = lexPunctAndOperator(beg, end);
        if (tok.notEofOrInvalid()) {
            ret = tok;
        } else {
            ret.setInvalid();
        }
    }

    ret.setContext((int)pos, (int)line);
    return ret;
}

std::pair<char, bool> TokenStream::lexCharRaw(StrIter& it, StrIter end)
{
    char ret;
    if (it == end) {
        return {0, false};
    }

    if (*it == '\\') {
        // escape char
        ++it;
        if (it == end) { return {0, false}; };
        switch (*it) {
        case '\\':
            ret = '\\';
            break;
        case '\'':
            ret = '\'';
            break;
        case '\"':
            ret = '\"';
            break;
        case 'a':
            ret = '\a';
            break;
        case 'b':
            ret = '\b';
            break;
        case 'f':
            ret = '\f';
            break;
        case 'n':
            ret = '\n';
            break;
        case 'r':
            ret = '\r';
            break;
        case 't':
            ret = '\t';
            break;
        case 'v':
            ret = '\v';
            break;
        case '0':
            ret = '\0';
            break;
        default:
            assert(false);
        }
        ++it;
        return {ret, true};
    } else {
        // normal char
        ret = *it;
        ++it;
        return {ret, true};
    }
};

std::pair<char, bool> TokenStream::lexChar(StrIter& it, StrIter end)
{
    assert(*it == '\'');
    char c;
    ++it;
    auto res = lexCharRaw(it, end);
    if (res.second) {
        c = res.first;
    } else {
        return {0, false};
    }

    if (it != end && *it == '\'') {
        ++it;
        return {c, true};
    } else {
        return {0, false};
    }
};

std::pair<std::string, bool> TokenStream::lexString(StrIter& it, StrIter end)
{
    assert(*it == '\"');
    ++it;
    std::string str;
    for (;;) {
        if (it == end) {
            // miss right "
            return {"", false};
        }

        if (*it == '\"') {
            // end.
            ++it;
            return {str, true};
        } else {
            auto res = lexCharRaw(it, end);
            if (res.second) {
                str.push_back(res.first);
            } else {
                return {"", false};
            }
        }
    }
}


Token TokenStream::lexNumber(StrIter& it, StrIter end)
{
    // positive numbers only. negative sign will be treated as operator-
    assert(std::isdigit(*it));

    Token tok;
    bool afterDot = false;
    std::string lexeme;
    size_t base = 10;
    // integer part
    if (isDigit1to9(*it)) {
        lexeme.push_back(*it++);
        while (it != end && std::isdigit(*it)) {
            lexeme.push_back(*it++);
        }
        if (it != end ) {
            if (*it == '.') {
                if (!lexNumberAfterDot(it, end, lexeme)) {
					return Token::invalidToken;
                }
                afterDot = true;
            }
            if (it != end && *it == 'e') {
                lexeme.push_back(*it++);
                if (it != end && std::isdigit(*it)) {
                    do {
                        lexeme.push_back(*it++);
                    } while (it != end && std::isdigit(*it));
                } else {
					return Token::invalidToken;
                }
                afterDot = true;
            }
        }
    } else if (*it == '0') {
        lexeme.push_back(*it++);
        if (it != end) {
            if (std::tolower(*it) == 'x') {
                // HEX
                lexeme.push_back(*it++);
                if (!lexHex(it, end, lexeme)) {
					return Token::invalidToken;
                }
                base = 16;

            } else if (isDigitInRange(*it, '1', '7')) {
                // OCT
                lexeme.push_back(*it++);
                if (!lexOct(it, end, lexeme)) {
					return Token::invalidToken;
                }
                base = 8;

            } else {
                if (*it == '.') {
                    if (!lexNumberAfterDot(it, end, lexeme)) {
                        tok.setInvalid();
                        return tok;
                    }
                    afterDot = true;
                }
                if (it != end && *it == 'e') {
                    if (std::isdigit(*it)) {
                        do {
                            lexeme.push_back(*it++);
                        } while (it != end && std::isdigit(*it));
                        afterDot = true;
                    }
                }
            }
        }
    }

	auto cat = lexNumberSuffix(it, end, lexeme, afterDot);

	if (it != end && std::isalnum(*it)) {
		// TODO: root not singular
		do {
			++it;
		} while (it != end && std::isalnum(*it));
		return Token::invalidToken;
	}

    switch (cat) {
    // TODO: lazy and annoying
    case NumCat::U:
        tok.setLiteral(static_cast<unsigned>(std::stoul(lexeme, nullptr, base)));
        break;
    case NumCat::I:
    case NumCat::L:
    case NumCat::UL:
    case NumCat::LL:
    case NumCat::ULL:
        tok.setLiteral(std::stoi(lexeme, nullptr, base));
        break;
    case NumCat::F:
    case NumCat::DB:
    case NumCat::LDB:
        tok.setLiteral(std::stod(lexeme));
        break;
	case NumCat::ERR:
		return Token::invalidToken;
	default:
		assert("unknown number catgory" && false);
    }

    return tok;
}

bool TokenStream::lexNumberAfterDot(StrIter& it, StrIter end, std::string& lexeme)
{
    lexeme.push_back(*it);
    ++it;
    if (it != end && std::isdigit(*it)) {
        do {
            lexeme.push_back(*it);
            ++it;
        } while (it != end && std::isdigit(*it));
        return true;
    } else {
        // error : miss digits after .
        return false;
    }
}

bool TokenStream::lexHex(StrIter& it, StrIter end, std::string& lexeme)
{
    // 0x was processed
    if (it != end) {
        if (*it == '0') {
            lexeme.push_back(*it++);
            return true;
        } else if (isDigitInRange(*it, '1', 'f')) {
            do {
                lexeme.push_back(*it++);
            } while (it != end && std::isxdigit(*it));
            return true;
        } else {
            return false;
        }
    } else {
        return false;
    }
}

bool TokenStream::lexOct(StrIter& it, StrIter end, std::string& lexeme)
{
    // 0 was processed
    if (it != end) {
        if (*it == '0') {
            lexeme.push_back(*it++);
            return true;
        } else if (isDigitInRange(*it, '1', '7')) {
            do {
                lexeme.push_back(*it++);
            } while (it != end && isDigitInRange(*it, '0', '7'));
            return true;
        } else {
            return false;
        }
    } else {
        return false;
    }
}

auto TokenStream::lexNumberSuffix(StrIter& it, StrIter end, std::string& lexeme, bool afterDot) -> NumCat
{
    if (!afterDot) {
        // integer
        if (it != end) {
            if (tolower(*it) == 'u') {
#ifdef MULTIPLE_LITERAL
                lexeme.push_back(*it); //uvar
#endif
				++it;
                if (it != end && tolower(*it) == 'l') {
#ifdef MULTIPLE_LITERAL
					lexeme.push_back(*it); //ul
#endif
					++it;
                    if (it != end && tolower(*it) == 'l') {
                        // unsigned long long
#ifdef MULTIPLE_LITERAL
						lexeme.push_back(*it); //uvar
#endif
						++it;
                        return NumCat::ULL;
                    } else {
                        // unsigned long
                        return NumCat::UL;
                    }
                } else {
                    // unsigned
                    return NumCat::U;
                }
            } else if (tolower(*it) == 'l') {
#ifdef MULTIPLE_LITERAL
				lexeme.push_back(*it); //l
#endif
				++it;
                if (it != end && tolower(*it) == 'l') {
                    // long long
#ifdef MULTIPLE_LITERAL
					lexeme.push_back(*it); //ull
#endif
					++it;
                    return NumCat::LL;
                } else {
                    // long
                    return NumCat::L;
                }
            } else {
                // int
                return NumCat::I;
            }
        } else {
            // int
            return NumCat::I;
        }
    } else {
        // float number
        if (it != end) {
            if (tolower(*it) == 'f') {
                // float
#ifdef MULTIPLE_LITERAL
				lexeme.push_back(*it); //
#endif
				++it;
                return NumCat::F;
            } else if (tolower(*it) == 'l') {
                // long double
#ifdef MULTIPLE_LITERAL
				lexeme.push_back(*it); 
#endif
				++it;
				if (it != end && tolower(*it) == 'd') {
#ifdef MULTIPLE_LITERAL
					lexeme.push_back(*it);
#endif
					++it;
					return NumCat::LDB;
				} else {
					return NumCat::ERR;
				}

            } else {
                // double
                return NumCat::DB;
            }
        } else {
            // double
            return NumCat::DB;
        }
    }
}

Token TokenStream::lexSymbol(StrIter& it, StrIter end)
{
    assert(isAlphaOr_(*it));

    Token tok;
    std::string lexeme;
    lexeme.push_back(*it);
    ++it;
    for (; it != end && isAlnumOr_(*it); ++it) {
        lexeme.push_back(*it);
    }

    if (lexeme == "true") {
        // treat true and false as a kind of literal
        tok.setLiteral(true);
    } else if (lexeme == "false") {
        tok.setLiteral(false);
    } else {
        auto mapIt = keywordMap.find(lexeme);
        if (mapIt != keywordMap.end()) {
            // keyword
            tok.setKeyword(mapIt->second);
        } else {
            // user defined
            tok.setName(lexeme);
        }
    }



    return tok;
}

Token TokenStream::lexPunctAndOperator(StrIter& it, StrIter end)
{
    Token tok;
    switch (*it) {
    case '(':
    case ')':
    case '{':
    case '}':
    case '[':
    case ']':
    case ';':
    case ',':
        tok.setSingleChar(*it++);
        break;
    default:
        tok = lexOperator(it, end);
        break;
    }

    return tok;
}

Token TokenStream::lexOperator(StrIter& it, StrIter end)
{
    Token tok;
    std::string lexeme;
    switch (*it) {
    case '&':
        lexeme.push_back(*it++);
        if (it != end && (*it == '&' || *it == '=')) {
            lexeme.push_back(*it++);
        }
        break;
    case '|':
        lexeme.push_back(*it++);
        if (it != end && (*it == '|' || *it == '=')) {
            lexeme.push_back(*it++);
        }
        break;
    case '+':
        lexeme.push_back(*it++);
        if (it != end && (*it == '+' || *it == '=')) {
            lexeme.push_back(*it++);
        }
        break;
    case '-':
        lexeme.push_back(*it++);
        if (it != end && (*it == '>' || *it == '-' || *it == '=')) {
            lexeme.push_back(*it++);
        }
        break;

    case '=':
        lexeme.push_back(*it++);
        if (it != end && *it == '=') {
            lexeme.push_back(*it++);
        }
        break;
    case '>':
        lexeme.push_back(*it++);
        if (it != end) {
            if (*it == '>') {
                lexeme.push_back(*it++);
                if (it != end && *it == '=') {
                    lexeme.push_back(*it++);
                }
            } else if (*it == '=') {
                lexeme.push_back(*it++);
            }
        }
        break;
    case '<':
        lexeme.push_back(*it++);
        if (it != end) {
            if (*it == '<') {
                lexeme.push_back(*it++);
                if (it != end && *it == '=') {
                    lexeme.push_back(*it++);
                }
            } else if (*it == '=') {
                lexeme.push_back(*it++);
            }
        }
        break;
    case '!':
        lexeme.push_back(*it++);
        if (it != end && *it == '=') {
            lexeme.push_back(*it++);
        }
        break;

    case '^':
    case '%':
    case '/':
    case '*':
        lexeme.push_back(*it++);
        if (it != end && *it == '=') {
            lexeme.push_back(*it++);
        }
        break;
    case '.':
        lexeme.push_back(*it++);
        if (it != end && *it == '.' &&
                it + 1 != end && *(it + 1) == '.') {
            // ATTENTION: corner case.
            it += 2;
            tok.setVarArgs();
            return tok;
        }
        break;
    case '?':
    case ':':
    case '~':
        lexeme.push_back(*it++);
        break;
    default:
        lexeme = "\\(*_*)/";
    }

    auto mapIt = operatorMap.find(lexeme);
    if (mapIt != operatorMap.end()) {
        tok.setOperator(mapIt->second);
    } else {
        tok.setInvalid();
    }

    return tok;
}

TokenStream::TokenStream(std::string from, ErrorLog& errli)
        : text_(from), pos_(0), errors_(errli)
{
    toks_ = lex();
}


