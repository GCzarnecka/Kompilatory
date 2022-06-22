cmake_minimum_required(VERSION 3.1)
project(compiler)
find_package(BISON)
find_package(FLEX)
bison_target(parser
        ${CMAKE_CURRENT_SOURCE_DIR}/parser.y
        ${CMAKE_CURRENT_BINARY_DIR}/parser.cpp
        COMPILE_FLAGS "-d")
flex_target(scanner
        ${CMAKE_CURRENT_SOURCE_DIR}/scanner.l
        ${CMAKE_CURRENT_BINARY_DIR}/scanner.cpp)
add_flex_bison_dependency(scanner parser)
add_executable(compiler
        ast.cpp ast.hpp definition.cpp
        env.cpp env.hpp
        type.cpp type.hpp
        error.cpp error.hpp
        ${BISON_parser_OUTPUTS}
        ${FLEX_scanner_OUTPUTS}
        main.cpp
        )
target_include_directories(compiler PUBLIC ${CMAKE_CURRENT_SOURCE_DIR})
target_include_directories(compiler PUBLIC ${CMAKE_CURRENT_BINARY_DIR})