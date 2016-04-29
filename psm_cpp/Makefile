all: psm

psm: psm.cpp
	$(CXX) $(CXXFLAGS) -o $@ $^

CXXFLAGS := -Wall -O2 -pthread -std=c++11
.PHONY: all clean

clean:
	rm -rf *.o psm
