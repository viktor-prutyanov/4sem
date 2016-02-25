all: a.out

a.out: 1.o 2.o
	gcc -o $@ $^

CFLAGS := -Wall -O2 -MD
-include *.d
.PHONY: all clean

clean:
	rm -rf *.o a.out