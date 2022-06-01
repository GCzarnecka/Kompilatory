bison -o parser.cpp -d parser.y
flex scanner.l -o scanner.cpp
g++ -c -o scanner.o scanner.cpp
g++ -c -o parser.o parser.cpp
g++ main.cpp parser.o scanner.o
