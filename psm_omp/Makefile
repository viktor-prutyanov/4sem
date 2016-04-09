CFLAGS:=-Wall -O1 -std=c99 -fopenmp
LDLIBS:=-lm
PROJECT_NAME:=psm
SOURCE:=main.c

.PHONY: clean

$(PROJECT_NAME): $(SOURCE)
	$(CC) $(CFLAGS) $(LDLIBS) -o $@ $<

clean:
	rm -rf *.d *.o $(PROJECT_NAME)
