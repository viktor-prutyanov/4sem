all: a.out

a.out: main.o heapq.o
	gcc -o $@ $^

CFLAGS := -Wall -O2 -MD
-include *.d
.PHONY: all clean

clean:
	rm -rf *.o *.d a.out