all: psm

psm: main.o psm.o 
	gcc -O1 -std=c99 -lm -lpthread -o $@ $^

CFLAGS := -Wall -MD -O1 -std=c99
-include *.d
.PHONY: all clean

clean:
	rm -rf *.o *.d psm
