bison -o parser.cpp -d parser.y
flex -o scanner.cpp scanner.l
g++ -c -o scanner.o scanner.cpp
g++ -c -o parser.o parser.cpp
g++ main.cpp parser.o scanner.o
# create an .png image of the AST
bison parser.y --graph
dot parser.dot -T png -o parser.png
