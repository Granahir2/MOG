#include <iostream>
#include <fstream>
#include <string>
#include <cstdint>
#include <stdexcept>
#include "NaiveConverter.hpp"

int main(int argc, char** argv) {
       // For now, we keep it in main. But in the future, a parsing function / module will probably be required
       if(argc < 3) {
              std::cout << "This utility must be launched with 2 arguments\n"; return 1;
       } else if(argc == 3) {
              std::ifstream source(argv[1]);
              std::ifstream instruction_set(std::string("specs/IS/") + argv[2] + ".mogISA");

              if(!source.is_open()){std::cout << "Unable to load source file\n"; return 1;}
              if(!instruction_set.is_open()){std::cout << "Unable to load ISA file\n"; return 1;}

	      std::string bytecode;

	       try {
                     bytecode = convertToBytecode(source, instruction_set);
              } catch(std::runtime_error &e) {
                     std::cout << e.what() << '\n';
                     return 2;
	      }

             std::ofstream compiled_file(std::string(argv[1]).append(".hex"));
              compiled_file.write(bytecode.c_str(), bytecode.length());
              compiled_file.close();
       }
}
