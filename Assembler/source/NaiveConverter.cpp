#include "NaiveConverter.hpp"

std::string convertToBytecode(std::ifstream& sourceCode, std::ifstream& instruction_set) {
       std::string bytecode;

       std::regex instructions[std::numeric_limits<std::uint8_t>::max() + 1];

       for(int i = 0; i < std::numeric_limits<std::uint8_t>::max() + 1; ++i) {
              std::string line;
              std::getline(instruction_set, line);

              instructions[i] = std::regex(line, std::regex_constants::grep);
       }

       std::cout << "Loaded regexes\n";

       int sloc = 0;
       while(!sourceCode.eof()) {
              sloc++;
              std::string line;
              std::getline(sourceCode, line);

              bool found = false;

              for(int i = 0; i < std::numeric_limits<std::uint8_t>::max() + 1; ++i) {
                     if(std::regex_match(line.begin(), line.end(), instructions[i])) {
                            found = true;
                            bytecode += static_cast<char>(i);

                            int numOperands = std::count_if(line.begin(), line.end(), [](char input){return (input == '$') || (input == '%');});

                            if(numOperands == 3) {
                                   bytecode += static_cast<char>(std::stoi(line.substr(line.length() - 2, 1), nullptr, 16));
                            } else {
                                   bytecode += static_cast<char>(0x00);
                            }

                            int firstOp = line.find(i & 0x2 ? '%' : '$') + 1;
                            char byte = static_cast<char>(std::stoi(line.substr(firstOp, i & 0x2 ? 1 : 2), nullptr, 16));
                            bytecode += byte;
                            byte = static_cast<char>(std::stoi(line.substr(line.find(i % 0x1 ? '%' : '$', firstOp) + 1, i & 0x1 ? 1 : 2), nullptr, 16));
                            bytecode += byte;
                            break;
                     }
              }

              if(found == false) throw std::runtime_error(std::string("Syntax error at line : ") + std::to_string(sloc) + " aborting...\n");
       }

       return bytecode;
}
