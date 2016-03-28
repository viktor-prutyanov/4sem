all: psm

psm: main.o psm.o 
	gcc -lm -lpthread -o $@ $^

CFLAGS := -Wall -MD
-include *.d
.PHONY: all clean

clean:
	rm -rf *.o *.d psm
