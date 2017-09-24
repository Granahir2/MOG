CXXFLAGS := -O3 -Wall -Wextra -Wpedantic -I"include" -std=c++0x
SRCDIR := source
OBJECTS := $(patsubst %.cpp, %.o, $(wildcard $(SRCDIR)/*.cpp))
BINDIR := bin
SPECSDIR := Specifications
SPECS := $(wildcard $(SPECSDIR)/*.mogISA)

.PHONY: all
all: $(OBJECTS) | DIRS
	$(CXX) $(OBJECTS) -o $(BINDIR)/Assembler.out

.PHONY: DIRS
DIRS:
	mkdir -p $(BINDIR)
	mkdir -p $(BINDIR)/specs/IS
	cp -r $(SPECSDIR)/* $(BINDIR)/specs/IS

.PHONY: clean
clean:
	rm -f $(OBJECTS)
