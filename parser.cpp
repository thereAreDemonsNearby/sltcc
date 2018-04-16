#include <cassert>
#include <iostream>
#include <stack>
#include <vector>
#include <functional>
#include "parser.h"
#include "error.h"
#include "SymbolTable.h"

// utils
void Parser::skipUntil(Token::TokenType t)
{
    while (tokens_.peek().type() != Token::Eof
           && tokens_.peek().type() != t)
        tokens_.next();
}

bool Parser::isAboutType(const Token& tok)
{
    SymbolTable& scope = currScope();
    if (tok.type() == Token::Keyword) {
        auto t = tok.keyword();
        return (int) t >= 0 && (int) t <= (int) Token::Static;
    } else if (tok.type() == Token::Name) {
        auto entry = scope.find(tok.name());
        if (entry) {
            auto tag = entry->type->tag();
            if (tag == Type::Incomplete || tag == Type::Compound
                || tag == Type::Enum) {
                // TODO maybe add type alias
                return true;
            } else {
                return false;
            }
        } else
            return false;
    } else
        return false;
}

bool Parser::isTypeSpecifier(const Token& tok)
{
    return tok.type() == Token::Keyword
           && (int) tok.keyword() <= (int) Token::Void;
}

void Parser::skipErrorParts()
{
    while (tokens_.peek().type() != Token::Eof) {
        const Token& n = tokens_.next();
        auto t = n.type();
        if (t == Token::Semicolon || t == Token::RBrace)
            break;
    }
}

bool Parser::isUserDefinedType(Type::Tag tag)
{
    // TODO maybe add type alias
    return tag == Type::Incomplete || tag == Type::Compound
           || tag == Type::Enum || tag == Type::Alias;
}
// end utils

Parser::Parser(TokenStream& ts, ErrorLog& el)
        : tokens_(ts), errors_(el)
{
    // TODO this is a work around for test. delete later.
    scopeStack_.push(new HashSymtab(nullptr));
}

std::shared_ptr<ASTRoot> Parser::goal()
{
    auto root = std::make_shared<ASTRoot>(&(tokens_.peek()));
    scopeStack_.push(&root->scope());

    // open hole for scan and print
    auto voidType = BuiltInType::make(BuiltInType::BI_VOID, 0);
    auto scanType = std::make_shared<FuncType>("scan", voidType);
    auto printType = std::make_shared<FuncType>("print", voidType);
    scanType->defined(true);
    root->scope().insert("scan", scanType, nullptr);
    root->scope().insert("print", printType, nullptr);

    while (tokens_.peek().type() == Token::Semicolon)
        tokens_.next();
    while (errors_.count() < stopErrorNum && tokens_.peek().type() != Token::Eof) {
        try {
            // skip unnecessary ';'s

            auto curr = tokens_.curr();
            if (curr->type() == Token::Keyword &&
                (curr->keyword() == Token::Struct ||
                 curr->keyword() == Token::Union ||
                 curr->keyword() == Token::Enum ||
                 curr->keyword() == Token::Typedef)) {
                // this branch matches struct/union declaration and definition
                root->add(parseTypeDeclOrDef());

            } else {
                if (isAboutType(*curr)) {
                    auto typeHead = parseTypeHead();
                    // if success then continue else exception
                    auto p2peek = tokens_.curr();
                    if (p2peek->type() == Token::Name
                        && (p2peek + 1)->type() == Token::LParen) {
                        // function declaration or definition
                        root->add(parseFuncDeclOrDef(typeHead));
                    } else {
                        // var, array, pointer to array
                        root->add(parseVarDef(typeHead));
                    }
                } else {
                    errors_.add(Error(&tokens_.peek(), "unrecognized token"));
                    skipErrorParts();
                }
            }
        } catch (const Error& err) {
            errors_.add(err);
            skipErrorParts();
        } catch (...) {
            std::cerr << "Unknown exception. Bye." << std::endl;
            std::exit(1);
        }
        while (tokens_.peek().type() == Token::Semicolon)
            tokens_.next();
    }

    return root;
}

std::shared_ptr<CompoundDecl> Parser::parseCompoundDecl()
{
    // struct/union name; has been parsed.
    // so yi ma ping chuan
    auto& startTok = tokens_.next();
    CompoundType::MemModel memModel;
    assert(startTok.type() == Token::Keyword);
    if (startTok.keyword() == Token::Struct) {
        memModel = CompoundType::Compound_Struct;
    } else {
        assert(startTok.keyword() == Token::Union);
        memModel = CompoundType::Compound_Union;
    }

    auto& nameTok = tokens_.next();
    assert(nameTok.type() == Token::Name);

    auto& endTok = tokens_.next();
    assert(endTok.type() == Token::Semicolon);

    auto type = std::make_shared<IncompleteType>(nameTok.name(), memModel,
                                                 &currScope());
    bool inserted = currScope().insert(nameTok.name(), type, &startTok);
    if (!inserted) {
        throw Error(&startTok, std::string("name ") + nameTok.name() + " has already been used.");
    }
    return std::make_shared<CompoundDecl>(&startTok, nameTok.name(), type);
}


std::shared_ptr<VarDef> Parser::parseVarDef(std::shared_ptr<Type> typeHead)
{
    auto& startTok = tokens_.peek();
    auto pair = parseTypeRest(std::move(typeHead));
    if (pair.second.empty()) {
        throw Error(&startTok, "an variable identifier is needed");
    }

    auto& type = pair.first;
    if (type->tag() == Type::BuiltIn
        && static_cast<BuiltInType*>(type.get())->cat() == BuiltInType::Void) {
        throw Error(&startTok, "variable of type void is not permitted");
    }

    auto inserted = currScope().insert(pair.second, pair.first, &startTok);
    if (!inserted) {
        errors_.add(Error(&startTok, std::string("name ") + pair.second + " has already been used"));
    }

    std::shared_ptr<Expr> initExpr;
    if (tokens_.peek().type() == Token::Operator
        && tokens_.peek().getOperator() == Token::Assign) {
        // init
        // sth like: int a = 1 + 2;
        tokens_.next();
        initExpr = parseExpr();
    }

    auto def = std::make_shared<VarDef>(&startTok, pair.second, pair.first, initExpr);

    if (tokens_.peek().type() != Token::Semicolon) {
        errors_.add(Error(&tokens_.peek(), "miss ';' after variable declaration"));
    } else {
        tokens_.next();
    }

    return def;
}


std::shared_ptr<EnumDef> Parser::parseEnumDef()
{
    throw Error(&tokens_.peek(), "enum not supported yet");
}

std::shared_ptr<CompoundDef> Parser::parseCompoundDef()
{
    // assume 'struct 'ident and  '{ have been checked
    auto& startTok = tokens_.next();
    assert(startTok.type() == Token::Keyword);
    CompoundType::MemModel memModel;
    if (startTok.keyword() == Token::Struct) {
        memModel = CompoundType::Compound_Struct;
    } else {
        assert(startTok.keyword() == Token::Union);
        memModel = CompoundType::Compound_Union;
    }

    auto& nameTok = tokens_.next();
    assert(nameTok.type() == Token::Name);
    const std::string& name = nameTok.name();

    auto& lbraceTok = tokens_.next();
    assert(lbraceTok.type() == Token::LBrace);

    // to support behavior like this:
    // struct node { int data; node* next; };
    auto ent = currScope().findInCurr(name);
    if (ent) {
        if (ent->type->tag() != Type::Incomplete
                || static_cast<IncompleteType*>(ent->type.get())->model() != memModel) {
            skipUntil(Token::RBracket);
            throw Error(&nameTok, std::string("name ") + name + " has already been used");
        }
    } else {
        auto fwdType = std::make_shared<IncompleteType>(name, memModel, &currScope());
        if (!currScope().insert(name, fwdType, &startTok)) {
            skipUntil(Token::RBracket);
            throw Error(&nameTok, std::string("name ") + name + " has already been used");
        }
    }


    ListSymtab members;
    // auto type = std::make_shared<CompoundType>(name, memModel, &currScope());
    while (true) {
        if (tokens_.peek().type() == Token::RBrace) {
            tokens_.next();
            if (tokens_.peek().type() == Token::Semicolon) {
                tokens_.next();
            } else {
                errors_.add(Error(&tokens_.peek(), "miss ; after struct/union definition"));
            }

            auto type = std::make_shared<CompoundType>(name, memModel, &currScope(), std::move(members));
            // insert into symbol table
            auto shouldReplace = [memModel](const Type& ty) -> bool {
                if (ty.tag() == Type::Incomplete) {
                    return static_cast<IncompleteType const*>(&ty)->model() == memModel;
                } else
                    return false;
            };
            auto inserted =
                    currScope().insertOrConditionalAssign(name, type, &startTok, shouldReplace);
            if (!inserted) {
                throw Error(&startTok, std::string("name ") + name + "has already been used");
            }

            auto def = std::make_shared<CompoundDef>(&startTok, name, type);
            return def;

        } else {
            try {
                auto& begTok = tokens_.peek();
                auto member = parseCompleteType();
                if (member.second.size() == 0) {
                    errors_.add(Error(&tokens_.peek(),
                                      "an identifier is needed in data member declaration"));
                }

                auto inserted = members.insert(member.second, member.first, &begTok);
                // auto inserted = type->members().insert(member.second, member.first, &begTok);
                if (!inserted) {
                    // duplicate
                    errors_.add(Error(&begTok,
                                      std::string("variable ") + member.second + " is already a data member"));
                }
                if (tokens_.peek().type() == Token::Semicolon) {
                    tokens_.next();
                } else {
                    errors_.add(Error(&tokens_.peek(), "miss ; after data member definition"));
                }
            } catch (Error& err) {
                skipUntil(Token::Semicolon);
                while (tokens_.peek().type() == Token::Semicolon) {
                    tokens_.next();
                }
                errors_.add(err);
            }
        }
    }
}

// when no name, string is ""
std::pair<std::shared_ptr<Type>, std::string>
Parser::parseCompleteType()
{
    auto head = parseTypeHead();
    return parseTypeRest(head);
}


std::pair<std::shared_ptr<Type>, std::string>
Parser::parseTypeRest(std::shared_ptr<Type> typeHead)
{
    // helper class
    struct Emptyable
    {
        int val;
        bool empty;

        Emptyable(bool e, int a) : empty(e), val(a)
        {}
    };

    auto& tokens = tokens_;

    auto getDimensions = [&tokens]() -> std::stack<Emptyable> {
        std::stack<Emptyable> dims;
        assert(tokens.peek().type() == Token::LBracket);
        do {
            tokens.next();
            if (tokens.peek().type() == Token::IntLiteral) {
                dims.push(Emptyable(false, tokens.peek().intLiteral()));
                tokens.next();
                if (tokens.peek().type() == Token::RBracket) {
                    tokens.next();
                } else {
                    // miss ']'
                    throw Error(&tokens.peek(), "miss ] in array declaration");
                }
            } else if (tokens.peek().type() == Token::RBracket) {
                // sth like int arr[]
                dims.push(Emptyable{true, 0});
                tokens.next();
            } else {
                throw Error(&tokens.peek(), "incompatible subscript type in array declaration");
            }
        } while (tokens.peek().type() == Token::LBracket);
        return dims;
    };

    auto& startTok = tokens_.peek();

    std::string name;
    std::shared_ptr<Type> ret = std::move(typeHead);
    if (tokens_.peek().type() == Token::Name) {
        name = tokens_.peek().name();
        tokens_.next();
        // TODO ...
    }

    if (tokens_.peek().type() == Token::LBracket) {
        // parse array
        auto dims = getDimensions();

        while (!dims.empty()) {
            Emptyable dim = dims.top();
            dims.pop();
            if (!dim.empty) {
                if (dim.val <= 0) {
                    errors_.add(Error(&startTok, "array dimension should not be 0 or negative"));
                }
                ret = std::make_shared<ArrayType>(ret, dim.val, 0);
            } else {
                if (dims.empty()) {
                    // first dimension
                    ret = std::make_shared<PointerType>(ret, 0);
                } else {
                    throw Error(&startTok, "array dimension should not be empty");
                }
            }
        }

        // return { ret, name };

    } else if (tokens_.peek().type() == Token::LParen) {
        // int (*a)[2];
        tokens_.next();
        if (tokens_.peek().type() == Token::Operator
            && tokens_.peek().getOperator() == Token::Mult) {
            tokens_.next();
            if (tokens_.peek().type() == Token::Name) {
                name = tokens_.peek().name();
                tokens_.next();
            }

            if (tokens_.peek().type() != Token::RParen) {
                throw Error(&tokens_.peek(), "miss ')'");
            } else
                tokens_.next();

            if (tokens_.peek().type() != Token::LBracket) {
                throw Error(&tokens_.peek(), "complex type decl is not supported");
            }

            auto dims = getDimensions();
            while (!dims.empty()) {
                auto dim = dims.top();
                dims.pop();
                if (!dim.empty) {
                    if (dim.val <= 0) {
                        errors_.add(Error(&startTok, "array dimension should not be 0 or negative"));
                    }
                    ret = std::make_shared<ArrayType>(ret, dim.val);
                } else {
                    throw Error(&startTok, "array dimension should not be empty");
                }
            }

            ret = std::make_shared<PointerType>(ret, 0);
            // return {ret, name};
        } else {
            throw Error(&startTok, "array dimension should not be empty");
        }
    }

//    auto& currTok = tokens_.peek();
//    if (currTok.type() != Token::Semicolon || currTok.type() != Token::Comma) {
//        throw Error(&currTok, "cannot parse token after type declaration");
//    }

    return {ret, name};
}


std::shared_ptr<Type> Parser::parseTypeHead()
{
    // type that is not array/function/...

    // bool isStatic = false; // WARNING : will not support static
    auto& p = tokens_.peek();
    if (p.type() == Token::Keyword && p.keyword() == Token::Static) {
        // isStatic = true;
        tokens_.next();
    }

    std::shared_ptr<Type> baseType = parseBasicType();

    // now for pointers
    std::shared_ptr<Type> ret = baseType;
    while (tokens_.peek().type() == Token::Operator
           && tokens_.peek().getOperator() == Token::Mult) {
        Type::QualifierHolder qh = 0;
        tokens_.next();
        if (tokens_.peek().type() == Token::Keyword
            && tokens_.peek().keyword() == Token::Const) {
            tokens_.next();
            qh |= Type::Const;
        }
        ret = std::make_shared<PointerType>(ret, qh);
    }

    return ret;
}

std::shared_ptr<Type> Parser::parseBasicType()
{
    // type that is not pointer/array/... and not static
    Type::QualifierHolder qh = 0;
    BuiltInType::SpecifierHolder sh = 0;

    std::shared_ptr<Type> baseType;
    auto& tmpTok = tokens_.peek();

    while (tokens_.peek().type() == Token::Keyword
           && tokens_.peek().keyword() == Token::Const) {
        qh |= Type::Const;
        tokens_.next();
    }

    if (isTypeSpecifier(tokens_.peek())) {

        sh |= parseSpecifierList(); // something like unsigned long int
        // sth like char const
        while (tokens_.peek().type() == Token::Keyword
               && tokens_.peek().keyword() == Token::Const) {
            qh |= Type::Const;
            tokens_.next();
        }

        auto ty = BuiltInType::make(sh, qh); // factor
        if (!ty) {
            throw Error(&tmpTok, "Invalid type specification");
        }

        baseType = ty;
    } else if (tokens_.peek().type() == Token::Name) {
        auto& tok = tokens_.peek();
        auto ent = currScope().find(tok.name());
        if (ent && isUserDefinedType(ent->type->tag())) {
            // type name
            tokens_.next();
            if (tokens_.peek().type() == Token::Keyword
                && tokens_.peek().keyword() == Token::Const) {
                qh |= Type::Const;
                tokens_.next();
            }

            SymbolTable* whereDefined;

            if (ent->type->tag() == Type::Incomplete) {
                whereDefined = static_cast<IncompleteType*>(ent->type.get())->whereDefined();
            } else if (ent->type->tag() == Type::Compound) {
                whereDefined = static_cast<CompoundType*>(ent->type.get())->whereDefined();
            } else assert(false);

            baseType = std::make_shared<UserDefinedTypeRef>(tok.name(),
                                                            whereDefined, qh, ent->type->width());
        } else {
            throw Error(&tok, "Invalid type specification");
        }
    } else {
        throw Error(&tmpTok, "Invalid type specification");
    }

    return baseType;
}

BuiltInType::SpecifierHolder Parser::parseSpecifierList()
{
    static constexpr BuiltInType::BuiltInSpecifier kwdToSpec[8] = {
            BuiltInType::BI_Unsigned, BuiltInType::BI_Long, BuiltInType::BI_Short,
            BuiltInType::BI_Int, BuiltInType::BI_Char, BuiltInType::BI_Double,
            BuiltInType::BI_Float, BuiltInType::BI_VOID
    };

    BuiltInType::SpecifierHolder sh = 0;
    while (tokens_.hasNext()) {
        auto& tok = tokens_.next();
        if (tok.type() == Token::Keyword) {
            auto kwd = tok.keyword();
            if ((int) kwd <= (int) Token::Void) {
                sh |= kwdToSpec[(int) kwd];
            } else {
                tokens_.putBack();
                break;
            }
        } else {
            tokens_.putBack();
            break;
        }
    }

    return sh;
}

std::pair<std::shared_ptr<FuncType>, std::vector<std::string>>
Parser::parseFuncHead(std::shared_ptr<Type> retType)
{
    // assume that return type has already been parsed
    assert(tokens_.peek().type() == Token::Name);
    std::string funcName = tokens_.next().name();
    auto& tmpTok = tokens_.peek();

    auto funcType = std::make_shared<FuncType>(funcName, retType);
    std::vector<std::string> names;
    // parse parameter list
    assert(tokens_.peek().type() == Token::LParen);
    tokens_.next();
    if (tokens_.peek().type() == Token::RParen) {
        tokens_.next();
    } else if (tokens_.peek().type() == Token::VarArgs) {
        funcType->hasVarArgs(true);
        tokens_.next();
        if (tokens_.peek().type() != Token::RParen) {
            throw Error(&tokens_.peek(), "the varargs indicator should be the last param");
        } else {
            tokens_.next();
        }
    } else if (isAboutType(tokens_.peek())) {
        auto param = parseCompleteType();
        funcType->addParam(param.first);
        names.push_back(param.second);

        while (true) {
            if (tokens_.peek().type() == Token::RParen) {
                tokens_.next();
                break;
            } else if (tokens_.peek().type() == Token::Comma) {
                tokens_.next();
                if (tokens_.peek().type() == Token::VarArgs) {
                    funcType->hasVarArgs(true);
                    tokens_.next();
                    if (tokens_.peek().type() != Token::RParen) {
                        throw Error(&tokens_.peek(), "the varargs indicator should be the last param");
                    } else {
                        tokens_.next();
                        break;
                    }
                } else {
                    auto param = parseCompleteType();
                    funcType->addParam(param.first);
                    names.push_back(param.second);
                }
            } else {
                throw Error(&tokens_.peek(), "expect ',' or ')'");
            }
        }
    } else {
        throw Error(&tokens_.peek(), "expect empty or parameter list");
    }


    return {funcType, names};
}


std::shared_ptr<FuncDef> Parser::parseFuncDef(
        std::pair<std::shared_ptr<FuncType>, std::vector<std::string>>& headInfo,
        const Token* startTok)
{
    assert(tokens_.peek().type() == Token::LBrace);

    auto& type = headInfo.first;
    auto& paramVec = headInfo.second;
    auto& funcName = type->name();
    assert(paramVec.size() == type->paramCount());
    auto nameIt = paramVec.begin();
    auto nameEnd = paramVec.end();
    auto typeIt = type->begin();
    auto typeEnd = type->end();
    ListSymtab params(&currScope()); // outer scope
    for (; nameIt != nameEnd; ++nameIt, ++typeIt) {
        if (!params.insert(*nameIt, *typeIt, nullptr)) { // function arg. nullptr is ok
            errors_.add(Error(startTok, "duplicate parameter name"));
        }
    }

    type->defined(true);
    auto def = std::make_shared<FuncDef>(startTok, funcName, type, std::move(params));
    auto funcBody = parseBlock(&def->params(), true);
    // TODO : baocuo
    def->body(funcBody);

    // refresh symbol table
    auto status = currScope().insertOrConditionalAssign(
            funcName, type, startTok, [&type](Type& ty) -> bool {
                if (ty.tag() == Type::Func) {
                    auto p = static_cast<FuncType*>(&ty);
                    return p->equal(type);
                } else return false;
            });
    if (!status) {
        throw Error(startTok, std::string("function name ") + funcName + " has already been used");
    }

    return def;
}


std::shared_ptr<ASTNode> Parser::parseFuncDeclOrDef(std::shared_ptr<Type> retType)
{
    // assume return type has been parsed.
    auto& tok = tokens_.peek();
    auto headInfo = parseFuncHead(std::move(retType));
    if (tokens_.peek().type() == Token::Semicolon) {
        // func decl
        auto& type = headInfo.first;
        if (tokens_.peek().type() == Token::Semicolon) {
            tokens_.next();
            auto inserted = currScope().insert(type->name(), type, &tok);
            if (!inserted) {
                errors_.add(Error(&tok, std::string("name ") + type->name() + " has already been used"));
            }
        } else {
            errors_.add(Error(&tokens_.peek(), "miss ';' after function declaration"));
        }

        auto decl = std::make_shared<FuncDecl>(&tok, type->name(), type);
        return decl;

    } else if (tokens_.peek().type() == Token::LBrace) {
        // function def {
        return parseFuncDef(headInfo, &tok);
    }
}

std::shared_ptr<Block> Parser::parseBlock(SymbolTable* outer, bool isFuncBody)
{
    // block : a new scope
    auto& startTok = tokens_.peek();
    assert(startTok.type() == Token::LBrace); // {
    tokens_.next();
    std::shared_ptr<Block> block = std::make_shared<Block>(&startTok, outer, isFuncBody);
    scopeStack_.push(&block->scope());

    while (tokens_.peek().type() == Token::Semicolon)
        tokens_.next();

    while (tokens_.peek().type() != Token::Eof &&
            tokens_.peek().type() != Token::RBrace) {
        // skip unnecessary ';'s
        try {
            auto curr = tokens_.curr();
            if (curr->type() == Token::Keyword &&
                (curr->keyword() == Token::Struct ||
                 curr->keyword() == Token::Union ||
                 curr->keyword() == Token::Enum ||
                 curr->keyword() == Token::Typedef)) {
                // this branch matches struct/union declaration and definition
                block->add(parseTypeDeclOrDef());

            } else if (isAboutType(*curr)) {
                auto typeHead = parseTypeHead();
                // if success then continue else exception
                auto p2peek = tokens_.curr();
                if (p2peek->type() == Token::Name
                    && (p2peek + 1)->type() == Token::LParen) {
                    // function declaration or definition
                    block->add(parseFuncDeclOrDef(typeHead));
                    errors_.add(Error(&(*p2peek),
                                      "function decl or def is not allowed in a local scope"));
                } else {
                    // var, array, pointer to array
                    block->add(parseVarDef(typeHead));
                }
            } else {
                block->add(parseStmt());
            }
        } catch (Error& err) {
            errors_.add(err);
            skipErrorParts();
        } // end catch
        while (tokens_.peek().type() == Token::Semicolon)
            tokens_.next();
    } // end while

    if (tokens_.peek().type() == Token::RBrace) {
        tokens_.next();
    } else {
        throw Error(&tokens_.peek(), "miss '}' after compound statements");
    }

    scopeStack_.pop();
    return block;
}

std::shared_ptr<Expr> Parser::parseExpr()
{
    // semicolon is processed in function parseBlock
    return parseAssignment();
}

std::shared_ptr<Expr> Parser::parsePostfix(std::shared_ptr<Expr> prev)
{
    std::shared_ptr<Expr> ret;
    if (tokens_.peek().type() == Token::LParen) {
        // func call
        auto callOpTok = &tokens_.peek();
        if (prev->tag() != ExprTag::Var) {
            throw Error(&tokens_.peek(), "callable object except function is not implemented");
        }

        std::vector<std::shared_ptr<Expr>> args;
        tokens_.next();
        while (true) {
            if (tokens_.peek().type() == Token::RParen) {
                tokens_.next();
                break;
            } else if (tokens_.peek().type() == Token::Comma) {
                tokens_.next();
            } else {
                args.emplace_back(parseExpr());
            }
        }

        /// return immidiately. no chance for recursion.
        return std::make_shared<FuncCall>(callOpTok,
                                          static_cast<VarExpr*>(prev.get())->varName(),
                                          std::move(args));

    } else if (tokens_.peek().type() == Token::LBracket) {
        // a[2][3]
        ret = prev;
        while (tokens_.peek().type() == Token::LBracket) {
            auto bracketTok = &tokens_.peek();
            tokens_.next();
            ret = std::make_shared<ArrayRefExpr>(bracketTok, std::move(ret), parseExpr());
            if (tokens_.peek().type() != Token::RBracket) {
                throw Error(&tokens_.peek(), "miss ']' in array reference");
            }
            tokens_.next();
        }
    } else if (tokens_.peek().type() == Token::Operator &&
                    (tokens_.peek().getOperator() == Token::Dot ||
                     tokens_.peek().getOperator() == Token::Arrow)) {
        ret = parseMemberExtract(prev, &tokens_.peek());
    } else {
        /// recursion base
        return prev;
    }

    return parsePostfix(ret);
}

std::shared_ptr<Expr> Parser::parsePrimary()
{
    auto& tok = tokens_.peek();
    if ((int)tok.type() >= (int)Token::IntLiteral
        && (int)tok.type() <= (int)Token::UnsignedLongLongLiteral) {
        // literal
        // unsigned long long not supported though
        tokens_.next();
        return std::make_shared<LiteralExpr>(&tok);
    } else if (tok.type() == Token::Name) {
        // may be variable
        auto nameExpr = std::make_shared<VarExpr>(&tok, tok.name());
        tokens_.next();
        return parsePostfix(nameExpr);

    } else if (tok.type() == Token::LParen) {
        // (expr)
        tokens_.next();
        auto inside = parseExpr();
        if (tokens_.peek().type() != Token::RParen) {
            throw Error(&tokens_.peek(), "miss ')' in expression");
        }
        tokens_.next();

        return parsePostfix(inside);
    } else {
        throw Error(&tok, "unrecognized primary expression");
    }
}


std::shared_ptr<MemberExpr>
Parser::parseMemberExtract(std::shared_ptr<Expr> suffix, Token const* first)
{
    assert(tokens_.peek().type() == Token::Operator);
    std::shared_ptr<MemberExpr> ret;
    if (tokens_.peek().getOperator() == Token::Dot) {
        tokens_.next();
        if (tokens_.peek().type() == Token::Name) {
            ret = std::make_shared<MemberExpr>(first, suffix, tokens_.peek().name());
            tokens_.next();
        } else {
            throw Error(&tokens_.peek(), "expect an identifier in member extract");
        }
    } else {
        assert(tokens_.peek().getOperator() == Token::Arrow);
        // foo->x to (*foo).x
        tokens_.next();
        if (tokens_.peek().type() == Token::Name) {
            auto data = std::make_shared<UnaryOpExpr>(first, suffix, Token::Mult);
            ret = std::make_shared<MemberExpr>(first, data, tokens_.peek().name());
            tokens_.next();
        } else {
            throw Error(&tokens_.peek(), "expect an identifier in member extract");
        }
    }

    auto& peek = tokens_.peek();
    if (peek.type() == Token::Operator &&
        (peek.getOperator() == Token::Dot || peek.getOperator() == Token::Arrow)) {
        ret = parseMemberExtract(ret, first);
    }

    return ret;
}

std::shared_ptr<Expr> Parser::parsePrefix()
{
    auto& startTok = tokens_.peek();
    if (startTok.type() == Token::Operator) {
        auto op = startTok.getOperator();
        switch(op) {
        case Token::Add:
        case Token::Sub:
        case Token::Not:
        case Token::BitInv: /* ~ */
        case Token::Mult:
        case Token::BitAnd: /* &, address of */
            tokens_.next();
            return std::make_shared<UnaryOpExpr>(&startTok, /** recursive */ parsePrefix(), op);
            break;
        default:
            throw Error(&startTok, "not a unary operator");
            break;
        }
    } else if (startTok.type() == Token::LParen) {
        // (type cast)
        auto next = tokens_.curr() + 1;
        if (isAboutType(*next)) {
            tokens_.next(); // skip (
            auto ty = parseTypeHead();
            if (tokens_.peek().type() != Token::RParen) {
                throw Error(&tokens_.peek(), "miss ')' in type cast");
            }
            tokens_.next();
            return std::make_shared<CastExpr>(&startTok, ty, /** recursive */ parsePrefix());
        } else {
            return parsePrimary();
        }
    } else if (startTok.type() == Token::Keyword &&
            startTok.keyword() == Token::Sizeof) {
        tokens_.next();
        if (tokens_.peek().type() != Token::LParen) {
            throw Error(&tokens_.peek(), "miss '(' after sizeof");
        }
        tokens_.next();
        auto ty_nm = parseCompleteType();
        if (!ty_nm.second.empty()) {
            throw Error(&startTok, "complete variable definition is not allowed in sizeof expr");
        }

        if (tokens_.peek().type() != Token::RParen) {
            throw Error(&tokens_.peek(), "miss ')' in sizeof expression");
        }

        return std::make_shared<SizeofExpr>(&startTok, ty_nm.first);

    } else {
        return parsePrimary();
    }
}

std::shared_ptr<Expr> Parser::parseMultiplicative()
{
    auto lhs = parsePrefix();
    auto peek = &tokens_.peek();
    while (peek->type() == Token::Operator &&
            (peek->getOperator() == Token::Mult
             || peek->getOperator() == Token::Div
             || peek->getOperator() == Token::Mod)) {
        auto op = peek->getOperator();
        tokens_.next();
        auto rhs = parsePrefix();
        lhs = std::make_shared<BinaryOpExpr>(peek, lhs, rhs, op);
        peek = &tokens_.peek();
    }

    return lhs;
}

std::shared_ptr<Expr> Parser::parseAdditive()
{
    auto lhs = parseMultiplicative();
    auto peek = &tokens_.peek();
    while (peek->type() == Token::Operator &&
           (peek->getOperator() == Token::Add
            || peek->getOperator() == Token::Sub)) {
        auto op = peek->getOperator();
        tokens_.next();
        auto rhs = parseMultiplicative();
        lhs = std::make_shared<BinaryOpExpr>(peek, lhs, rhs, op);

        peek = &tokens_.peek();
    }

    return lhs;
}

std::shared_ptr<Expr> Parser::parseShift()
{
    auto lhs = parseAdditive();
    auto peek = &tokens_.peek();
    while (peek->type() == Token::Operator &&
           (peek->getOperator() == Token::SftL
            || peek->getOperator() == Token::SftR)) {
        auto op = peek->getOperator();
        tokens_.next();
        auto rhs = parseAdditive();
        lhs = std::make_shared<BinaryOpExpr>(peek, lhs, rhs, op);

        peek = &tokens_.peek();
    }

    return lhs;
}

std::shared_ptr<Expr> Parser::parseRelational()
{
    auto lhs = parseShift();
    auto peek = &tokens_.peek();
    while (peek->type() == Token::Operator &&
           (peek->getOperator() == Token::Smlr
            || peek->getOperator() == Token::Grtr
           || peek->getOperator() == Token::Se
           || peek->getOperator() == Token::Ge)) {
        auto op = peek->getOperator();
        tokens_.next();
        auto rhs = parseShift();
        lhs = std::make_shared<BinaryOpExpr>(peek, lhs, rhs, op);

        peek = &tokens_.peek();
    }

    return lhs;
}

std::shared_ptr<Expr> Parser::parseEquality()
{
    return parseBinaryOp({Token::Eq, Token::Ne}, std::mem_fn(&Parser::parseRelational));
}

std::shared_ptr<Expr> Parser::parseBitAnd()
{
    return parseBinaryOp({Token::BitAnd}, std::mem_fn(&Parser::parseEquality));
}

std::shared_ptr<Expr> Parser::parseBitXor()
{
    return parseBinaryOp({Token::BitXor}, std::mem_fn(&Parser::parseBitAnd));
}

std::shared_ptr<Expr> Parser::parseBitOr()
{
    return parseBinaryOp({Token::BitOr}, std::mem_fn(&Parser::parseBitXor));
}

std::shared_ptr<Expr> Parser::parseLogicalAnd()
{
    return parseBinaryOp({Token::And}, std::mem_fn(&Parser::parseBitOr));
}

std::shared_ptr<Expr> Parser::parseLogicalOr()
{
    return parseBinaryOp({Token::Or}, std::mem_fn(&Parser::parseLogicalAnd));
}

std::shared_ptr<Expr> Parser::parseConditional()
{
    auto p = &tokens_.peek();
    auto cond = parseLogicalOr();
    if (tokens_.peek().type() == Token::Operator
            && tokens_.peek().getOperator() == Token::Question) {
        tokens_.next();
        auto yes = parseLogicalOr();
        if (tokens_.peek().type() == Token::Operator
                && tokens_.peek().getOperator() == Token::Colon) {
            tokens_.next();
            auto no = parseLogicalOr();
            return std::make_shared<ConditionExpr>(p, cond, yes, no);
        } else {
            throw Error(&tokens_.peek(), "miss ':' in conditional expression");
        }
    } else {
        return cond;
    }
}

std::shared_ptr<Expr> Parser::parseAssignment()
{
    auto isAssignOp = [](Token::OperatorType t) {
        return (int)t <= (int)Token::SftRAssign && (int)t >= (int)Token::Assign;
    };

    auto currPos = tokens_.pos();
    auto& startTok = tokens_.peek();

    // firstly try assignment expression
    auto lhs = parsePrefix();
    auto& opTok = tokens_.peek();
    if (opTok.type() == Token::Operator && isAssignOp(opTok.getOperator())) {
        // hopefully
        tokens_.next();
        auto rhs = parseExpr();
        return std::make_shared<BinaryOpExpr>(&startTok, lhs, rhs, opTok.getOperator());
    } else {
        // shit, not an assignment expression
        // backtrace and congtouzailai
        tokens_.pos(currPos);
        return parseConditional();
    }
}

std::shared_ptr<DoWhileStmt> Parser::parseDoWhileStmt()
{
    auto& doTok = tokens_.peek();
    assert(tokens_.peek().type() == Token::Keyword &&
        tokens_.peek().keyword() == Token::Do);
    tokens_.next();
    if (tokens_.peek().type() != Token::LBrace) {
        throw Error(&tokens_.peek(), "miss '{' in do-while statement");
    }
    auto body = parseBlock(&currScope());
    if (tokens_.peek().type() != Token::Keyword ||
            tokens_.peek().keyword() != Token::While) {
        throw Error(&tokens_.peek(), "miss 'while' in do-while statement");
    }
    tokens_.next();
    if (tokens_.peek().type() != Token::LParen) {
        throw Error(&tokens_.peek(), "miss '(' in do-while statement");
    }
    tokens_.next();

    auto cond = parseExpr();

    if (tokens_.peek().type() != Token::RParen) {
        throw Error(&tokens_.peek(), "miss ')' in do-while statement");
    }
    tokens_.next();
    if (tokens_.peek().type() != Token::Semicolon) {
        throw Error(&tokens_.peek(), "miss ';' in do-while statement");
    }
    tokens_.next();

    return std::make_shared<DoWhileStmt>(&doTok, cond, body);
}

std::shared_ptr<WhileStmt> Parser::parseWhileStmt()
{
    auto& whileTok = tokens_.peek();
    assert(tokens_.peek().type() == Token::Keyword &&
           tokens_.peek().keyword() == Token::While);
    tokens_.next();

    if (tokens_.peek().type() != Token::LParen) {
        throw Error(&tokens_.peek(), "miss '(' in while statement");
    }
    tokens_.next();

    auto cond = parseExpr();

    if (tokens_.peek().type() != Token::RParen) {
        throw Error(&tokens_.peek(), "miss ')' in while statement");
    }
    tokens_.next();

    auto body = parseStmt();
    return std::make_shared<WhileStmt>(&whileTok, cond, body);
}

std::shared_ptr<ForStmt> Parser::parseForStmt()
{
    auto& forTok = tokens_.peek();
    assert(tokens_.peek().type() == Token::Keyword &&
           tokens_.peek().keyword() == Token::For);
    tokens_.next();

    if (tokens_.peek().type() != Token::LParen) {
        throw Error(&tokens_.peek(), "miss '(' in for statement");
    }
    tokens_.next();

    auto init = parseExpr();
    if (tokens_.peek().type() != Token::Semicolon) {
        throw Error(&tokens_.peek(), "miss ';' in for statement");
    }
    tokens_.next();

    auto cond = parseExpr();
    if (tokens_.peek().type() != Token::Semicolon) {
        throw Error(&tokens_.peek(), "miss ';' in for statement");
    }
    tokens_.next();

    auto stepby = parseExpr();

    if (tokens_.peek().type() != Token::RParen) {
        throw Error(&tokens_.peek(), "miss ')' in for statement");
    }
    tokens_.next();

    auto body = parseStmt();
    return std::make_shared<ForStmt>(&forTok, init, cond, stepby, body);
}

std::shared_ptr<IfStmt> Parser::parseIfStmt()
{
    auto& ifTok = tokens_.peek();
    assert(tokens_.peek().type() == Token::Keyword &&
           tokens_.peek().keyword() == Token::If);
    tokens_.next();

    if (tokens_.peek().type() != Token::LParen) {
        throw Error(&tokens_.peek(), "miss '(' in if statement");
    }
    tokens_.next();

    auto cond = parseExpr();
    if (tokens_.peek().type() != Token::RParen) {
        throw Error(&tokens_.peek(), "miss '(' in if statement");
    }
    tokens_.next();

    auto thenClause = parseStmt();

    if (tokens_.peek().type() == Token::Keyword
            && tokens_.peek().keyword() == Token::Else) {
        tokens_.next();
        auto elseClause = parseStmt();
        return std::make_shared<IfStmt>(&ifTok, cond, thenClause, elseClause);
    } else {
        // no else
        return std::make_shared<IfStmt>(&ifTok, cond, thenClause,
                                        /** no use arg */nullptr);
    }
}

std::shared_ptr<SwitchStmt> Parser::parseSwitchStmt()
{
    throw Error(&tokens_.peek(), "switch not implemented");
}

std::shared_ptr<BreakStmt> Parser::parseBreakStmt()
{
    throw Error(&tokens_.peek(), "break not implemented");
}

std::shared_ptr<ReturnStmt> Parser::parseReturnStmt()
{
    auto& start = tokens_.peek();
    assert(start.type() == Token::Keyword && start.keyword() == Token::Return);
    tokens_.next();
    auto expr = parseExpr();
    if (tokens_.peek().type() != Token::Semicolon) {
        errors_.add(Error(&tokens_.peek(), "miss ';' after return statement"));
    } else {
        tokens_.next();
    }
    return std::make_shared<ReturnStmt>(&start, expr);
}

std::shared_ptr<Stmt> Parser::parseStmt()
{
    if (tokens_.peek().type() == Token::Semicolon) {
        // empty statement
        auto p = &tokens_.next();
        return std::make_shared<EmptyExpr>(p);
    }

    if (tokens_.peek().type() == Token::LBrace) {
        // single scope
        return parseBlock(&currScope(), false);
    } else {
        // normal expressions
        auto& startTok = tokens_.peek();
        if (startTok.type() == Token::Keyword) {
            switch (startTok.keyword()) {
            case Token::Do:
                return parseDoWhileStmt();
                break;
            case Token::While:
                return parseWhileStmt();
                break;
            case Token::For:
                return parseForStmt();
                break;
            case Token::If:
                return parseIfStmt();
                break;
            case Token::Switch:
                return parseSwitchStmt();
                break;
            case Token::Break:
                return parseBreakStmt();
                break;
            case Token::Return:
                return parseReturnStmt();
                break;
            default:
                throw Error(&tokens_.peek(), "keyword not leading a statement");
                break;
            }
        } else {
            auto expr = parseExpr();
            if (tokens_.peek().type() != Token::Semicolon) {
                errors_.add(Error(&tokens_.peek(), "miss ';' after expression"));
            } else {
                tokens_.next();
            }
            return expr;
        }
    }
}

std::shared_ptr<ASTNode> Parser::parseTypeDeclOrDef()
{
    auto curr = tokens_.curr();
    auto kwd = curr->keyword();
    if (kwd == Token::Union || kwd == Token::Struct) {
        if ((curr + 1)->type() == Token::Name) {
            if ((curr + 2)->type() == Token::LBrace) {
                // then it's time for struct/union definition

                return parseCompoundDef();

            } else if ((curr + 2)->type() == Token::Semicolon) {
                // struct Foo;

                return parseCompoundDecl();

            } else {
                throw Error(&(*(curr + 1)), "expect ; or { after struct/union identity");
            }
        } else {
            throw Error(&(*curr), "expect an identity after struct/union");
        }
    } else {
        assert(kwd == Token::Enum);
        throw Error(&(*curr), "enum not supported yet.");
        //parseEnumDef();
    }
}