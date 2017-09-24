#pragma once

#include <cstdint>
#include <fstream>
#include <string>
#include <regex>
#include <iostream>
#include <limits>
#include <stdexcept>

std::string convertToBytecode(std::ifstream& sourceCode, std::ifstream& instruction_set);
