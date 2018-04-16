#include <iostream>
#include <fstream>
#include <memory>
#include <iterator>
#include "lex.h"
#include "parser.h"
#include "error.h"
#include "type.h"
#include "parenfulprinter.h"
#include "typechecker.h"
#include "ast.h"
#include "tacdef.h"

void testSimpleType(std::string str)
{
    using namespace std;
    ErrorLog errli;
    TokenStream ts(std::move(str), errli);
    Parser parser(ts, errli);
    try {
        auto ty = parser.parseTypeHead();
        cout << ty->toString() << "  \n";
    } catch (Error& err) {
        errli.add(err);
    }
    for (auto it = errli.begin(); it != errli.end(); ++it) {
        cout << "exception: " << it->what() << endl;
    }
}

void testCompleteType(std::string str)
{
    using namespace std;
    ErrorLog errli;
    TokenStream ts(std::move(str), errli);
    Parser parser(ts, errli);
    try {
        auto p = parser.parseCompleteType();
        cout << p.first->toString() << " " << p.second << '\n';
    } catch (Error& err) {
        errli.add(err);
    }
    for (Error const& err : errli) {
        cout << "exception: " << err.what() << endl;
    }
}

void testFuncType(std::string str)
{
    using namespace std;
    ErrorLog errli;
    TokenStream ts(std::move(str), errli);
    Parser parser(ts, errli);
    try {
        auto retT = parser.parseTypeHead();
        auto p = parser.parseFuncHead(retT);
        cout << p.first->toString() << "\n param names:\n";
        for (auto& item : p.second) {
            cout << item << " ";
        }
        cout << endl;
    } catch (Error& err) {
        errli.add(err);
    }
    for (Error const& err : errli) {
        cout << "exception: " << err.what() << endl;
    }
}

void testType()
{
    testSimpleType("int");
    testSimpleType("unsigned short");
    testSimpleType("short int");
    testSimpleType("unsigned long const");
    testSimpleType("void* const");
    testSimpleType("char* const*");
    testSimpleType("const char* const");
    testSimpleType("float");
    testSimpleType("double const*");
    testSimpleType("int**");
    testSimpleType("short long int");

    testCompleteType("int a");
    testCompleteType("int abc");
    testCompleteType("const long int abc");
    testCompleteType("const char* const abcba");
    testCompleteType("int arr[2]");
    testCompleteType("int arr[2][3][5][7]");
    testCompleteType("int* arr[2][3][5][7]");
    testCompleteType("int arr[][2]");
    testCompleteType("const int (*a)[2]");
    testCompleteType("double []");
    testCompleteType("double* [][]"); // error
}

void testFunc()
{
    testFuncType("void menu(double, double ha)");
    testFuncType("int main(int argc, char** argv)");
    testFuncType("int sum(int arr[], unsigned sz)");
    testFuncType("char* strtod(char*, char** pp)");
    testFuncType("void main()");
}

void printSimpleExpr(std::string str)
{
    using namespace std;
    ErrorLog errli;
    TokenStream ts(std::move(str), errli);
    Parser parser(ts, errli);
    ParenfulPrinter printer;
    try {
        auto expr = parser.parseExpr();
        expr->accept(printer);
        cout << endl;
    } catch (Error& err) {
        errli.add(err);
    }
    for (Error const& err : errli) {
        cout << "exception: " << err.what() << endl;
    }
}

void testSimpleExpr()
{
    printSimpleExpr("1+-2");
    printSimpleExpr("+1+2*3");
    printSimpleExpr("\"asdfasdasdfasdfasdf\"");
    printSimpleExpr("sizeof(int)");
    printSimpleExpr("1 & 0");
    printSimpleExpr("~0");
    printSimpleExpr("!!0");
    printSimpleExpr("(1)");
    printSimpleExpr("!!(1+2*3)");
    printSimpleExpr("1 ? 123 : 456");
    printSimpleExpr("1 = 2*3+4%5");
    printSimpleExpr("1 <= 2");
    printSimpleExpr("!!(1 <= 2 || (1 > 2 && 1 == 2))");
    printSimpleExpr("1 += 2 || 3 && 4 | 5 ^ 6 & 7 == 8  < 9 >> 10 + 11 / 12");
    printSimpleExpr("1 + *2");
    printSimpleExpr("1 + **2");
    printSimpleExpr("1 + &2");
    printSimpleExpr("(int)4.555 <= (int) 4 + (int) 3.3");
}

void printSimpleProgram(std::string str)
{
    using namespace std;
    ErrorLog errli;
    TokenStream ts(std::move(str), errli);
    Parser parser(ts, errli);
    ParenfulPrinter printer;
    try {
        auto goal = parser.goal();
        goal->accept(printer);
    } catch (Error& err) {
        errli.add(err);
    }
    for (Error const& err : errli) {
        cout << "exception: " << err.what() << endl;
    }
}

void testSimpleProgram()
{
    printSimpleProgram("int i = 2; int b = 43;");
    printSimpleProgram("int main() { int i = 2; i = 2 + (int)3;}");
    printSimpleProgram("int sum(int arr[], int sz); int main() { int i = 2; i = 2 + (int)3;}");
    printSimpleProgram("struct foo; struct bar { int a; char* str; };");
    printSimpleProgram(std::string("struct node { int val_; node* next; };"));
    printSimpleProgram(std::string("struct node { int val_; node* next; };")
                       + "int main() {"
                       + "node p; node q;"
                       + "p.next = &q;"
                       + "}");
    printSimpleProgram(std::string("int func(int arr[], int sz) {")
                       + "arr[sz] = 2 + 3; }");
    printSimpleProgram("int foo(int); int main() {} int foo(int a) {}");
    printSimpleProgram("int foo(); int main() {} int foo(int a) {}");
    printSimpleProgram("struct node { double* b; };"
                               "int main() {"
                               "node* a; double b;"
                               "a->b = &b;"
                               "}");
    printSimpleProgram("int arrsum(int arr[], int size) {"
                               "return 100; ; }");
    printSimpleProgram("int main() {"
                               "if (1 < 2) if (3>4) 1+1; else 1+1;}");
    printSimpleProgram("int main() {"
                               "while (1) { int arr[100]; arr[1] += 1; }"
                               "}");
    printSimpleProgram("int main() {"
                               "int a;"
                               "for (a = 1; a <= 100; a += 1)"
                               "if (a % 2 == 0) "
                               "a = a / 3 % 4;"
                               "}");
    printSimpleProgram("int main() {"
                               "int i;"
                               "do {"
                               "i += 1;"
                               "} while(a != 0);"
                               "}");
    printSimpleProgram("void foo(); int main() {"
                               "foo = foo + 1;"
                               "int foo; }");
    printSimpleProgram("void main() { void a; int b; double c; c = (double)b; }");
    printSimpleProgram("struct a { int v;}; struct b { a v;}; int main() { b v; int i = v.v.v; }");
    printSimpleProgram("int a; double b; int main(){}");
    printSimpleProgram("struct A { int a; int b[20][18]; }; "
                               "int main() {"
                               "A x;  x.b[1][4] = 3; }");
    printSimpleProgram("struct A { int x; }; "
                               "int main() { "
                               "A* p; int b = (*p).x;"
                               "}");
}

void printSema(std::string str)
{
    static int count = 0;
    ++count;
    std::cout << "Program " << count << ":\n";

    using namespace std;
    ErrorLog errli;
    TokenStream ts(std::move(str), errli);
    Parser parser(ts, errli);
    ParenfulPrinter printer;
    TypeChecker checker(errli);
    try {
        auto goal = parser.goal();
        goal->accept(checker);
    } catch (Error& err) {
        errli.add(err);
    }

    if (errli.begin() == errli.end()) {
        std::cout << "Ok" << std::endl;
    }
    for (auto it = errli.begin(); it != errli.end(); ++it) {
        int line = it->token()->context().linum;
        std::cout << "line " << line << ": " << it->what() << std::endl;
    }
}

void testSema()
{
    printSema("struct Rec\n"
                      "{ int a; float b; double c; };\n"
                      "int main() {\n"
                      "Rec r;\n"
                      "int a;\n"
                      "a = r.b;\n"
                      "}");
    printSema("int main() {\n"
                      "int const cv = 32;\n"
                      "int* ptr = &cv;\n}");
    printSema("int main() {\n"
                      "int const cv = 32;\n"
                      "int const* cp = &cv;\n}");
    printSema("int main() {\n"
                      "if (2.2) {} \n}");
    printSema("int main() {\n"
                      "if (2) {} }");
    printSema("int main() { while (\"hahaha\") {} }");
    printSema("int main() { char ch; const char* p; p = &ch; *p = 'a'; }");
    printSema("int main() { char ch; char* const p; p = &ch; }");
    printSema("int main() { char ch; char* const p = &ch; *p = 'a'; }");
    printSema("int main() { int arr[32]; arr[1] = 32; }");
    printSema("int main() { const int arr[32]; arr[1] = 32; }");
    printSema("int main() { const int arr[32][43]; arr[1] = 32; }");
    printSema("int main() { const int arr[32][43]; arr[1][2] = 44; }");
    printSema("char func() { return 2; }");
    printSema("char func() { return (char)2; }");
    printSema("char* func() { char ch; return &ch; }");
    printSema("int main() { void a; }");
    printSema("struct Point;"
                      "int main() { Point p; }");
    printSema("struct Point;"
                      "int main() { Point* p; }");
    printSema("int main() { unsigned int a = 2; }");
    printSema("int main() { int (*ap)[33]; int (*bp)[22]; bp = ap; }");
    printSema("int main() { int (*ap)[33]; int (*bp)[33]; bp = ap; }");
    printSema("int main() { const int (*ap)[33]; int (*bp)[33]; bp = ap; }");
    printSema("struct a { }; struct a { };");
    printSema("struct a { int a; }; struct b { int a; }; "
                      "int main() { a x; b y; x = y; }");
    printSema("struct a { int a; }; struct b { int a; }; "
                      "int main() { a x; a y; x = y; }");
    printSema("struct S; S* sp; struct S {}; int main() { S s; sp = &s; }");//
    printSema("int main() { const int arr[32]; int* const p = arr; }");
    printSema("int main() { const int arr[32]; const int* p = arr; }");
    printSema("int main() { int arr[32]; int* p = arr; }");
    printSema("int main() { int* p = 1; }");
    printSema("int main() { int* p = 0; }");
    printSema("int main() { int arr[32][32]; arr[1][2][3]; }");
    printSema("int main() { int arr[32][32]; arr[1][2]; }");
    printSema("int main() { int** pp; *pp = 32; }");
    printSema("int main() { int** pp; **pp = 32; }");
    printSema("int main() { int* const* pp; int b; *pp = &b; }");
    printSema("int main() { int* const* pp; int b; **pp = b; }");
    printSema("int main() { int (*arrp)[32][32]; const int arr[32][32]; arrp = &arr;}");

    printSema("int main() { const int (*arrp)[32][32]; int arr[32][32]; arrp = &arr;}");
    printSema("int main() { int (*arrp)[32][32]; const int arr[32][32]; arrp = &arr;}");

    printSema("int main() { int* p; *(p + 1) = 3; }");
    printSema("int main() { int** p; *(*(p+1)+2) = 3; }");
    printSema("int main() { int** p; p[1][2] = 3; }");
    printSema("int main() { int a; double b; int c = a + b; }");
    printSema("int main() { int a; double b; double c = a + b; }");
    printSema("int fn() {} int main() { fn() = 3; }");
    printSema("int* fn() {} int main() { *fn() = 3; }");
    printSema("int main() { int a; unsigned char b; while (a > b) {} }");
    printSema("int main() { int a; unsigned char b; while (a != b) {} }");
    printSema("int main() { int a = 2.2 << 3; }");
    printSema("int main() { int a = 2 << 3; }");
    printSema("void func(char* str); int main() { func(\"woaini\"); }");
    printSema("void func(char const* str); int main() { func(\"woaini\"); }");
    printSema("int printf(const char*); int main() { printf(\"%d%c\", 21, 'a'); }");
    printSema("int printf(const char*, ...); int main() { printf(\"%d%c\", 21, 'a'); }");
    printSema("int haha(int a, int b); int main() { haha(1); }");
    printSema("int haha(int a, int b); int main() { haha(1, 2+3); }");
    printSema("struct A { int val; A* next; };\n"
                      "struct B { A a; double b; };\n"
                      "int main() { A x; B y; y.b = 2.2; y.a.val = 3; y.a.next = &x; }");
    printSema("int main() { int const* const* p; int* const* q; p = q; }"); // TODO this is wrong too(maybe not wrong)
}

void printCompoundLayout(std::string text, int cnt)
{
    ErrorLog errs;
    TokenStream toks(std::move(text), errs);
    Parser parser(toks, errs);
    try {
        while (cnt--) {
            auto def = parser.parseCompoundDef();
            auto type = def->type();
            std::cout << type->toString() << '\n';
        }
    } catch (Error& err) {
        errs.add(err);
    }
    std::cout << "errors: \n";
    for (const auto& e : errs) {
        std::cout << e.what() << std::endl;
    }
}

void testCompoundLayout()
{
    printCompoundLayout("struct A { char a; double b; char c; };", 1);
    printCompoundLayout("struct A { char a; char b; double c; char d; };", 1);
    printCompoundLayout("union A { char a; double b; int c; };", 1);
    printCompoundLayout("struct A { char a; double b; char c; };"
                                "struct B { short a; A b; };", 2);
    printCompoundLayout("struct A { int arr[10]; char a; };", 1);
}


void testSemaFile()
{
    std::ifstream strm("/home/shiletong/forcheck.c", std::ios_base::in);
    std::string str{std::istreambuf_iterator<char>(strm),
                    std::istreambuf_iterator<char>()};
    ErrorLog errli;
    TokenStream ts(std::move(str), errli);
    Parser parser(ts, errli);
    TypeChecker checker(errli);
    ParenfulPrinter printer;
    try {
        auto goal = parser.goal();
        goal->accept(printer);
        goal->accept(checker);
    } catch (Error& err) {
        errli.add(err);
    } catch (...) {
        std::cerr << "Unknown exception" << std::endl;
        std::exit(0);
    }

    for (auto it = errli.begin(); it != errli.end(); ++it) {
        int line = it->token()->context().linum;
        std::cout << "line " << line << ": " << it->what() << std::endl;
    }
}

void testFile()
{
    using namespace std;
    std::ifstream srcfile("/home/shiletong/fortest.c");
    if (!srcfile) {
        cout << "file not open" << endl;
        std::exit(1);
    }

    std::string text {std::istreambuf_iterator<char>(srcfile), std::istreambuf_iterator<char>()};
    ErrorLog errli;
    TokenStream ts(std::move(text), errli);
    Parser parser(ts, errli);
    TypeChecker checker(errli);
    ParenfulPrinter printer;
    std::shared_ptr<ASTRoot> root;


    if (errli.count() == 0) {
        root = parser.goal();
        root->accept(printer);
    } else {
        cout << "didn't pass lexer." << endl;
    }



    if (errli.count() == 0) {
        root->accept(checker);
    } else {
        cout << "didn't pass parser." << endl;
    }


    cout << errli.count() << " errors found." << endl;
    for (auto it = errli.begin(); it != errli.end(); ++it) {
        cout << "line " <<  it->token()->context().linum << ": " << it->what() << endl;
    }
}

int main(int argc, char* argv[])
{
    // testCompoundLayout();
    // testSimpleProgram();
    testSema();
}