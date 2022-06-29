#include "ast.hpp"
#include "parser.hpp"
#include <string>

void yy::parser::error(const std::string &msg)
{
    std::cout << "An error occured: " << msg << std::endl;
}

extern std::vector<definition_ptr> program;

int main(int argc, char *argv[])
{
    // haskell file extention
    std::string haskell_file_extention = "hs";
    // no input filepath initialy provided
    std::string input_file_path = "";
    // create output file in current folder
    std::string output_file_path = "./";

    // check how many parameters were passed to the function
    switch (argc - 1) // first param is the current file name
    {
    case 0:
        // no parameters passed to the program
        std::cout << "No parameters passed to the program.\n"
                     "Expected:\n"
                     "[0] path to the input file\n"
                     "[1] (optional) path to the output file"
                  << std::endl;
        // exit showing error
        return 1;
        break;
    case 2:
        // two parameters detected
        // asign input file path
        input_file_path = argv[1];
        // asign output file path
        output_file_path = argv[2];
        break;
    default:
        // there is at least one parameter
        // asume it is the path to input file
        // asign input file path
        input_file_path = argv[1];
        // asign output file path
        output_file_path = std::string("./default_out") + std::string(".") + std::string("hs");
    }

    // DEBUGING
    std::cout << "First parameter: " << input_file_path << std::endl;
    std::cout << "Second parameter: " << output_file_path << std::endl;

    // parse the input given to the program
    yy::parser parser;
    parser.parse();
    std::cout << "Program size: " << program.size() << std::endl;
}