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
#include "tacgenerator.h"

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

void printAst(std::string str)
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
    printAst("int i = 2; int b = 43;");
    printAst("int main() { int i = 2; i = 2 + (int)3;}");
    printAst("int sum(int arr[], int sz); int main() { int i = 2; i = 2 + (int)3;}");
    printAst("struct foo; struct bar { int a; char* str; };");
    printAst(std::string("struct node { int val_; node* next; };"));
    printAst(std::string("struct node { int val_; node* next; };")
             + "int main() {"
             + "node p; node q;"
             + "p.next = &q;"
             + "}");
    printAst(std::string("int func(int arr[], int sz) {")
             + "arr[sz] = 2 + 3; }");
    printAst("int foo(int); int main() {} int foo(int a) {}");
    printAst("int foo(); int main() {} int foo(int a) {}");
    printAst("struct node { double* b; };"
             "int main() {"
             "node* a; double b;"
             "a->b = &b;"
             "}");
    printAst("int arrsum(int arr[], int size) {"
             "return 100; ; }");
    printAst("int main() {"
             "if (1 < 2) if (3>4) 1+1; else 1+1;}");
    printAst("int main() {"
             "while (1) { int arr[100]; arr[1] += 1; }"
             "}");
    printAst("int main() {"
             "int a;"
             "for (a = 1; a <= 100; a += 1)"
             "if (a % 2 == 0) "
             "a = a / 3 % 4;"
             "}");
    printAst("int main() {"
             "int i;"
             "do {"
             "i += 1;"
             "} while(a != 0);"
             "}");
    printAst("void foo(); int main() {"
             "foo = foo + 1;"
             "int foo; }");
    printAst("void main() { void a; int b; double c; c = (double)b; }");
    printAst("struct a { int v;}; struct b { a v;}; int main() { b v; int i = v.v.v; }");
    printAst("int a; double b; int main(){}");
    printAst("struct A { int a; int b[20][18]; }; "
             "int main() {"
             "A x;  x.b[1][4] = 3; }");
    printAst("struct A { int x; }; "
             "int main() { "
             "A* p; int b = (*p).x;"
             "}");
}

void printSema(std::string str, bool correct)
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

    assert(correct == (errli.begin() == errli.end()));

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
                          "}", false);
    printSema("int main() {\n"
                          "int const cv = 32;\n"
                          "int* ptr = &cv;\n}", false);
    printSema("int main() {\n"
                          "int const cv = 32;\n"
                          "int const* cp = &cv;\n}", true);
    printSema("int main() {\n"
                          "if (2.2) {} \n}", false);
    printSema("int main() {\n"
                          "if (2) {} }", true);
    printSema("int main() { while (\"hahaha\") {} }", true);
    printSema("int main() { char ch; const char* p; p = &ch; *p = 'a'; }", false);
    printSema("int main() { char ch; char* const p; p = &ch; }", false);
    printSema("int main() { char ch; char* const p = &ch; *p = 'a'; }", true);
    printSema("int main() { int arr[32]; arr[1] = 32; }", true);
    printSema("int main() { const int arr[32]; arr[1] = 32; }", false);
    printSema("int main() { const int arr[32][43]; arr[1] = 32; }", false);
    printSema("int main() { const int arr[32][43]; arr[1][2] = 44; }", false);
    printSema("char func() { return 2; }", false);
    printSema("char func() { return (char)2; }", true);
    printSema("char* func() { char ch; return &ch; }", true);
    printSema("int main() { void a; }", false);
    printSema("struct Point;"
                          "int main() { Point p; }", false);
    printSema("struct Point;"
                          "int main() { Point* p; }", true);
    printSema("int main() { unsigned int a = 2; }", true);
    printSema("int main() { int (*ap)[33]; int (*bp)[22]; bp = ap; }", false);
    printSema("int main() { int (*ap)[33]; int (*bp)[33]; bp = ap; }", true);
    printSema("int main() { const int (*ap)[33]; int (*bp)[33]; bp = ap; }", false);
    printSema("struct a { }; struct a { };", false);
    printSema("struct a { int a; }; struct b { int a; }; "
                          "int main() { a x; b y; x = y; }", false);
    printSema("struct S; S* sp; struct S {}; int main() { S s; sp = &s; }", true);//
    printSema("int main() { const int arr[32]; int* const p = arr; }", false);
    printSema("int main() { const int arr[32]; const int* p = arr; }", true);
    printSema("int main() { int arr[32]; int* p = arr; }", true);
    printSema("int main() { int* p = 1; }", false);
    printSema("int main() { int* p = 0; }", true);
    printSema("int main() { int arr[32][32]; arr[1][2][3]; }", false);
    printSema("int main() { int arr[32][32]; arr[1][2]; }", true);
    printSema("int main() { int** pp; *pp = 32; }", false);
    printSema("int main() { int** pp; **pp = 32; }", true);
    printSema("int main() { int* const* pp; int b; *pp = &b; }", false);
    printSema("int main() { int* const* pp; int b; **pp = b; }", true);
    printSema("int main() { int (*arrp)[32][32]; const int arr[32][32]; arrp = &arr;}", false);

    printSema("int main() { const int (*arrp)[32][32]; int arr[32][32]; arrp = &arr;}", true);
    printSema("int main() { int* p; *(p + 1) = 3; }", true);
    printSema("int main() { int** p; *(*(p+1)+2) = 3; }", true);
    printSema("int main() { int** p; p[1][2] = 3; }", true);
    printSema("int main() { int a; double b; int c = a + b; }", false);
    printSema("int main() { int a; double b; double c = a + b; }", true);
    printSema("int fn() {} int main() { fn() = 3; }", false);
    printSema("int* fn() {} int main() { *fn() = 3; }", true);
    printSema("int main() { int a; unsigned char b; while (a > b) {} }", true);
    printSema("int main() { int a; unsigned char b; while (a != b) {} }", true);
    printSema("int main() { int a = 2.2 << 3; }", false);
    printSema("int main() { int a = 2 << 3; }", true);
    printSema("void func(char* str); int main() { func(\"woaini\"); }", false);
    printSema("void func(char const* str); int main() { func(\"woaini\"); }", true);
    printSema("int printf(const char*); int main() { printf(\"%d%c\", 21, 'a'); }", false);
    printSema("int printf(const char*, ...); int main() { printf(\"%d%c\", 21, 'a'); }", true);
    printSema("int haha(int a, int b); int main() { haha(1); }", false);
    printSema("int haha(int a, int b); int main() { haha(1, 2+3); }", true);
    printSema("struct A { int val; A* next; };\n"
                          "struct B { A a; double b; };\n"
                          "int main() { A x; B y; y.b = 2.2; y.a.val = 3; y.a.next = &x; }", true);
    printSema("int main() { int const* const* p; int* const* q; p = q; }", false);
    printSema("struct A { int a; }; int main() { const A x; x.a = 3; }", false);
    printSema("struct A { int a; }; int main() { A const* p; p->a = 3; }", false);
    printSema("struct A { int a; }; int main() { A* p; p->a = 3; }", true);
    printSema("struct A { int a; }; int main() { A x; x.a = 3; }", true);
    printSema("struct A { int a; }; struct B { A x; };"
              "int main() { const B b;  b.x.a = 3; }", false);
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
    for (const Error& e : errs) {
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

void printTacGen_(std::string str, int lineno)
{
    ErrorLog errs;
    TokenStream tokens(std::move(str), errs);
    Parser parser(tokens, errs);
    TypeChecker sema(errs);
    TacGenerator tacGen;
    std::cout << "Line " << lineno << ":\n";
    auto root = parser.goal();
    if (errs.count() != 0) {
        std::cout << "syntax error:\n";
        for (auto const& err: errs) {
            std::cout << "line" << err.token()->context().linum << ": " << err.what() << "\n";
        }
        return;
    }
    root->accept(sema);
    if (errs.count() != 0) {
        std::cout << "semantic error:\n";
        for (auto const& err: errs) {
            std::cout << err.what() << "\n";
        }
        return;
    }
    root->accept(tacGen);
    std::cout << tacGen.ir().toString() << '\n';

    return;
}
#define printTacGen(str) printTacGen_((str), __LINE__)

void testTacGen()
{
    /*printTacGen("int sum() { int a; a = 1; int b = 2; int c = a + b; return c; }");
    printTacGen("void nothing() { unsigned char a; short b; b = a; }");
    printTacGen("int sum(int a, int b) { return a + b + 55; }");
    printTacGen("int sum(int a, int b) { return a + b + 2147483647; }");
    printTacGen("int sum(char a, unsigned b) { return a + b; }");
    printTacGen("int sum(char a, unsigned short b) { return a + b; }");
    printTacGen("int sum(unsigned char a, short b) { return a + b; }");
    printTacGen("int diff(unsigned a, unsigned b) { return a - b; }");
    printTacGen("int diff(short a) { return a - 0xffffffffu; }");
    printTacGen("int mult(short a, int b) { return a * (unsigned short)b; }");
    printTacGen("int* advancePtr(int* p, int n) { return p + n; }");
    printTacGen("int* advancePtr(int (*parr)[10], int n) { return (int*)(parr + n); }");
    printTacGen("struct S { char a; int b; }; S* prevPtr(S* p, int n) { return p - n; }");

    printTacGen("int max(int a, int b) {"
                "  if (a > b) { return a; }"
                "  else { return b; }"
                "}");
    printTacGen("int max(int a, int b) {"
                "  int m;"
                "  if (a > b) { m = a; }"
                "  else { m = b; }"
                "  return m;"
                "}");
    printTacGen("int max3(int a, int b, int c) {"
                "  if (a > b) {"
                "    if (a > c) return a;"
                "    else return c;"
                "  } else {"
                "    if (b > c) return b;"
                "    else return c;"
                "  }"
                "}");
    printTacGen("int isNull(int* p) {"
                "  if (p) {"
                "    return 1;"
                "  } else {"
                "    return 0;"
                "  }"
                "}");
    printTacGen("int not5(int a) {"
                "  if (a - 5) {"
                "    return 1;"
                "  } else {"
                "    return 0;"
                "  }"
                "}");

    printTacGen("int sum() {"
                "  int i;"
                "  int sum = 0;"
                "  for (i = 1; i <= 100; i = i + 1) {"
                "    sum = sum + i;"
                "  }"
                "  return sum;"
                "}");
    printTacGen("void sum(int* result) {"
                "  int i; int s = 0;"
                "  for (i = 1; i <= 100; i = i + 1) {"
                "    s = s + i;"
                "  }"
                "  *result = s;"
                "}");
    printTacGen("struct Stack; "
                "int stack_empty(Stack* s);"
                "void stack_pop(Stack* s);"
                "void makeItEmpty(Stack* s) {"
                "  while (!stack_empty(s)) {"
                "    stack_pop(s);"
                "  }"
                "}");
    printTacGen("int parse(char* str)"
                "{"
                "  int sum = 0;"
                "  while (str != (char*)0 && *str != '\\0') {"
                "    sum = sum + *str;"
                "  }"
                "  return sum;"
                "}");
    printTacGen("int parse(char* pstr[], int size)"
                "{"
                "  int sum = 0;"
                "  int i;"
                "  while (*pstr != (void*)0) {"
                "    for (i = 0; (*pstr)[i] != '\\0'; i=i+1) {"
                "      sum = sum + (*pstr)[i];"
                "    }"
                "    pstr = pstr + 1;"
                "  }"
                "  return sum;"
                "}");
    printTacGen("void* address() { int arr[10][10]; return (void*) &arr[1][2]; }");
    printTacGen("int value() { int arr[10][10]; return arr[1][2]; }");
    printTacGen("int chCount(char const* str) {"
                "  int cnt = 0;"
                "  int i = 0;"
                "  while (!(str[i] == '\\n' || str[i] == '\\0')) {"
                "    cnt = cnt + 1;"
                "    i = i + 1;"
                "  }"
                "  return cnt;"
                "}");
    printTacGen("unsigned countBlank(char const* str) {"
                "  unsigned cnt = 0;"
                "  while (*str != '\\0') {"
                "    if (*str == '\\t' || *str == '\\n' || *str == ' ') {"
                "      cnt = cnt + 1;"
                "    }"
                "  }"
                "  return cnt;"
                "}");
    printTacGen("struct Node { int val; Node* next; };"
                "int func() {"
                "  Node head;"
                "  head.val = 345; "
                "  Node tail;"
                "  tail.val = 678;"
                "  tail.next = (Node*) 0;"
                "  head.next = &tail;"
                "  return head.next->val;"
                "}");
    printTacGen("int testUnaryOps(int param)"
                "{"
                "  int a = param;"
                "  int* p = &a;"
                "  int b = ~a;"
                "  int c = -a;"
                "  return c;"
                "}");
    printTacGen("int testBinaryOps(int a, int b)"
                "{"
                "  int c = a / b;"
                "  int d = b % c;"
                "  int e = c * d;"
                "  int f = d >> 4;"
                "  unsigned g = f;"
                "  unsigned h = g >> 4;"
                "  int i = e ^ f;"
                "  int j = e | f;"
                "  int k = e & f;"
                "}");

    printTacGen("int add(int a, int b) {return a + b; }"
                "int add3(int a, int b, int c) "
                "{"
                "  return add(add(a, b), c);"
                "}");
    printTacGen("int reduce(int arr[], unsigned size)"
                "{"
                "  int i;"
                "  int sum = 0;"
                "  for (i = 0; i < size; i = i + 1) {"
                "    sum = sum + arr[i];"
                "  }"
                "  return sum;"
                "}"
                ""
                "int nonsense() "
                "{"
                "  int arr[3];"
                "  arr[0] = 1; arr[1] = 2; arr[2] = 3;"
                "  return reduce(arr, 3);"
                "}"); */

    printTacGen("struct Info "
                "{"
                "  char name[2];"
                "  int salary;"
                "}; \n"
                ""
                "Info createPerson(char const* n, int s)"
                "{"
                "  Info info;"
                "  info.name[0] = n[0]; info.name[1] = n[1];"
                "  info.salary = s;"
                "  return info;"
                "}\n"
                ""
                "Info createXiaoming()"
                "{"
                "  return createPerson(\"xm\", 10000);"
                "}\n"
                ""
                "int getSalary(Info info) { return info.salary; }\n"
                "int getSalaryP(Info const* info) { return info->salary; }\n"
                ""
                "int nonsense()\n"
                "{\n"
                "  Info xiaoming = createXiaoming();\n"
                "  int a = getSalary(xiaoming);\n"
                "  int b = getSalaryP(&xiaoming);\n"
                "  int c = createXiaoming().salary;\n"
                "  return a + b + c;\n"
                "}\n");

    printTacGen("int a = 10000;"
                "unsigned b = 4294967295u;"
                "char c = 'a';"
                "double d = 3.14;"
                "int func() {"
                "  int e = a;"
                "}");

    printTacGen("int func() {"
                "  int a = 10;"
                "  int b = a;"
                "  int c = b + 55;"
                "}");

    printTacGen("int func() {"
                "  int a = 10;"
                "  int b = a + 1;"
                "  int c = b + 55;"
                "}");

    printTacGen("int func() {"
                "  int a = 10;"
                "  unsigned b = (unsigned)a;"
                "  unsigned c = b + 100;"
                "}");
}

int main(int argc, char* argv[])
{
    // testCompoundLayout();
    // testSimpleProgram();
    // testSema();
    testTacGen();
}