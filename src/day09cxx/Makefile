INPUT=../../data/09.txt

CXX=g++-13
CXXFLAGS=-std=c++23 -Wall -Wextra -Wpedantic -Werror -Ofast

.phony: run clean

run: day09
	INPUT=$(INPUT) ./day09

day09:
	$(CXX) $(CXXFLAGS) -o day09 src.cc

clean:
	rm -f day09
