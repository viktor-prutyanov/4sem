all: supervisor worker

supervisor: supervisor.c 
	$(CC) $(CFLAGS) -o $@ $^

worker: worker.c
	$(CC) $(CFLAGS) -lm -o $@ $^

CFLAGS := -Wall -O2 -std=c99 -pthread
.PHONY: all clean

clean:
	rm -rf *.o supervisor worker
