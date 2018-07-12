#ifndef C_LEXER_LEX_H
#define C_LEXER_LEX_H

#include <string>
#include <vector>
#include <utility>
#include <iostream>
#include "token.h"

class ErrorLog;

class TokenStream
{
public:
	TokenStream(std::string from, ErrorLog&);

	TokenStream(const TokenStream&) = delete;
	TokenStream& operator= (const TokenStream&) = delete;
	TokenStream(TokenStream&&) = default;
	TokenStream& operator= (TokenStream&&) = default;

	const Token& peek() const
	{
		return toks_[pos_];
	}

	const Token& next()
	{
		auto t = pos_++;
		return toks_[t];
	}

	void putBack() { --pos_; }

	bool hasNext() const { return pos_ < toks_.size(); }

    std::vector<Token>::iterator curr() { return toks_.begin() + pos_; }
    std::vector<Token>::iterator end() { return toks_.end(); }

	// for backtracing
	size_t pos() const { return pos_; }
	void pos(size_t p) { pos_ = p; }

	const std::string text() const { return text_; }

	void printAll() {
        for (auto& i : toks_) {
            std::cout << i.toString() << " ";
        }
    }
private:
	std::string text_;
	std::size_t pos_;
	ErrorLog& errors_;
	std::vector<Token> toks_;


	// from lexer
	std::size_t currline_ = 1;

	std::vector<Token> lex();
	Token nextToken(std::string::const_iterator&, std::string::const_iterator end);
	using StrIter = std::string::const_iterator;
	enum class NumCat {
		I, U, UL, ULL, LL, L, F, DB, LDB, ERR
	};
	bool isDigit1to9(char c)
	{
		return std::isdigit(c) && c != '0';
	}

	bool isDigitInRange(char ch, char begin, char end)
	{
		// no fault detection
		if (std::isdigit(end)) {
			return ch >= begin && ch <= end;
		} else {
			if (std::isdigit(begin)) {
				return (ch >= begin && ch <= '9')
					   || (ch >= std::tolower('a') && ch <= std::tolower(end));
			} else {
				return ch >= std::tolower(begin) && ch <= std::tolower(end);
			}
		}
	}

	bool isAlphaOr_(char c)
	{
		return std::isalpha(c) || c == '_';
	}

	bool isAlnumOr_(char c)
	{
		return std::isalnum(c) || c == '_';
	}

	void skipSpace(StrIter& it, StrIter end)
	{
		while (it != end && std::isspace(*it)) {
			if (*it == '\n') {
				++currline_;
			}
			++it;
		}
	}


	std::pair<std::string, bool> lexString(StrIter&, StrIter end);

	std::pair<char, bool> lexCharRaw(StrIter&, StrIter end);

	std::pair<char, bool> lexChar(StrIter&, StrIter end);

	Token lexNumber(StrIter&, StrIter end);
	bool lexNumberAfterDot(StrIter&, StrIter, std::string&);
	bool lexHex(StrIter&, StrIter, std::string&);
	bool lexOct(StrIter&, StrIter, std::string&);
	NumCat lexNumberSuffix(StrIter&, StrIter, std::string&, bool);

	Token lexSymbol(StrIter&, StrIter end); // keyword or user defined symbol or true | false
	Token lexPunctAndOperator(StrIter&, StrIter end);
	Token lexOperator(StrIter&, StrIter);
};

#endif //C_LEXER_LEX_H
