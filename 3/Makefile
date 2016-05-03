all: supervisor worker

supervisor: supervisor.c 
	$(CC) $(CFLAGS) -o $@ $^

worker: worker.c
	$(CC) $(CFLAGS) -o $@ $^

CFLAGS := -Wall -O2 -std=c11
.PHONY: all clean

clean:
	rm -rf *.o dsm
