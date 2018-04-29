cmake_minimum_required(VERSION 3.8)
project(sltcc)

set(CMAKE_CXX_STANDARD 17)

set(SOURCE_FILES main.cpp ast.cpp lex.cpp SymbolTable.cpp token.cpp
        type.cpp visitor.h parser.h parser.cpp error.h error.cpp parenfulprinter.h
        parenfulprinter.cpp typechecker.h typechecker.cpp platform.h
        tacdef.h tacdef.cpp tacgenerator.h tacgenerator.cpp
        )
add_executable(sltcc ${SOURCE_FILES})