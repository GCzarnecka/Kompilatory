#include "ast.hpp"
#include "parser.hpp"
#include <string>
#include <fstream>
#include <cstdio>
#include <cstring>
#include <algorithm>

int name_idx = 0;

std::string new_type_name()
{
    int temp = name_idx++;
    std::string str = "";

    while (temp != -1)
    {
        str += (char)('a' + (temp % 26));
        temp = temp / 26 - 1;
    }

    std::reverse(str.begin(), str.end());
    return str;
}

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

    // Display number of valid definitions parsed
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
        fputs("\n\n", output_file);

        // add IO monad for printing main function output
        std::string io_monad = "main :: IO ()";
        fputs(io_monad.c_str(), output_file);
        // move writing head to new line
        fputs("\n\n", output_file);

        // main function body
        std::string main_function_body = "";

        // Here is where the magic happenes
        for (auto &curr_definition : program)
        {
            // try casting the current definition dynamicly
            if (dynamic_cast<definition_comment *>(curr_definition.get()))
            {
                std::cout << "DEBUG | Comment." << std::endl;

                definition_comment *comment = dynamic_cast<definition_comment *>(curr_definition.get());
                if (!comment)
                    continue;

                std::string comment_body = comment->text.substr(2, comment->text.size() - 4);
                std::string compiled_comment = "";

                // check if this is a single or a multiline comment
                if (comment->text[1] == '/')
                {
                    // it is a single line comment
                    compiled_comment = "--" + comment_body;
                }
                else
                {
                    // it is a multiline comment
                    compiled_comment = "{-" + comment_body + "-}";
                }

                // write compiled comment to file
                fputs(compiled_comment.c_str(), output_file);
                // move writing head to new line
                fputs("\n\n", output_file);
            }
            else if (dynamic_cast<definition_def *>(curr_definition.get()))
            {
                std::cout << "DEBUG | Def." << std::endl;

                definition_def *def = dynamic_cast<definition_def *>(curr_definition.get());
                if (!def)
                    continue;

                // if this is the main function
                if (def->name == "main")
                {
                    // save the function body for later in main function body
                    main_function_body = "";
                    std::cout << "DEBUG | Main function encountered! Saving body." << std::endl;
                    // move to the next definition
                    continue;
                }

                std::string compiled_def = def->name;

                for (auto &param : def->params)
                    compiled_def = compiled_def + " " + param;
                compiled_def = compiled_def + " = ";
                // def->body->print(1, std::cout);

                // write compiled comment to file
                fputs(compiled_def.c_str(), output_file);
                // move writing head to new line
                fputs("\n\n", output_file);
            }
            else if (dynamic_cast<definition_data *>(curr_definition.get()))
            {
                std::cout << "DEBUG | Data" << std::endl;
                definition_data *data = dynamic_cast<definition_data *>(curr_definition.get());
                if (!data)
                    continue;

                std::string compiled_data = "data ";
                compiled_data = compiled_data + data->name + " = { ";
                //
                int is_first_constructor = 0;
                // compile each constructor based on type
                for (auto &constr : data->constructors)
                {
                    // only follow constructor with a comma
                    // when not first or last of constructors
                    if (is_first_constructor > 0 && is_first_constructor < data->constructors.size())
                    {
                        compiled_data = compiled_data + ", ";
                    }

                    // Is constructor only type Integer
                    if (constr->name == "Integer")
                    {
                        // constructor is of basic type Integer
                        compiled_data = compiled_data + new_type_name() + " :: Integer";
                        is_first_constructor = false;
                    }
                    else
                    {
                        // constructor is of non basic type
                        // it's a data structure -> should be of type Maybe
                        compiled_data = compiled_data + new_type_name() + " :: Maybe " + constr->name;
                        is_first_constructor = false;
                    }
                    // set index to next constructor
                    is_first_constructor++;
                }
                // remeber to close the bracket
                compiled_data = compiled_data + " }";

                // write compiled comment to file
                fputs(compiled_data.c_str(), output_file);
                // move writing head to new line
                fputs("\n\n", output_file);
            }
            else
            {
                std::cout << "DEBUG | This should never happen! Definition has no known type!" << std::endl;
            }
        }
        // end off all magic (it newer was to begin with)

        // append the main class call with print
        fputs((std::string("main = print ( ") + main_function_body + std::string(" )")).c_str(), output_file);
        // move writing head to new line
        fputs("\n\n", output_file);

        // close the output file
        fclose(output_file);
    }
}
