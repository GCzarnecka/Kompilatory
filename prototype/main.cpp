#include "ast.hpp"
#include "parser.hpp"
#include <string>
#include <fstream>
#include <cstdio>
#include <cstring>

void yy::parser::error(const std::string &msg)
{
    std::cout << "An error occured: " << msg << std::endl;
}

// input file
extern FILE *yyin;
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
    default:
        // there is at least one parameter
        // asume it is the path to input file

        // asign input file path
        input_file_path = argv[1];
        // read from file
        yyin = fopen(input_file_path.c_str(), "r");

        // if the file does not exist
        if (yyin == NULL)
        {
            // std::cout << "DEBUG | Input file is null after reading!" << std::endl;
            std::cout << "Failed to read from input file!" << std::endl;
            // close the file
            fclose(yyin);
            return 1;
        }

        // asign output file path
        if (argc == 3)
        {
            output_file_path = argv[2];
        }
        else
        {
            output_file_path = std::string("..//haskell_out_files//default_out") + std::string(".") + std::string("hs");
        }
    }
    // DEBUGING
    std::cout << "DEBUG | First parameter: " << input_file_path << std::endl;
    std::cout << "DEBUG | Second parameter: " << output_file_path << std::endl;

    // parse the input given to the program
    yy::parser parser;
    parser.parse();
    std::cout << "Valid definitions found in program: " << program.size() << std::endl;

    // close input file
    fclose(yyin);

    // if the parsed program containes valid program
    if (program.size() > 0)
    {
        std::cout << "DEBUG | The program is valid and is not empty." << std::endl;

        // Write the compiled program to the output file
        FILE *output_file = fopen(output_file_path.c_str(), "w");

        // prepend module definition at the beggining of the file
        std::string module_definition = "module Main where";
        fputs(module_definition.c_str(), output_file);
        // move writing head to new line
        fputs("\n", output_file);

        // add IO monad for printing main function output
        std::string io_monad = "main :: IO ()";
        fputs(io_monad.c_str(), output_file);
        // move writing head to new line
        fputs("\n", output_file);

        // append the main class call with print
        fputs("main = print ( 0 )", output_file);
        // move writing head to new line
        fputs("\n", output_file);

        // close the output file
        fclose(output_file);
    }
}