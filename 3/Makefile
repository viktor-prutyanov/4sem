all: supervisor worker

supervisor: supervisor.c 
	$(CC) $(CFLAGS) -o $@ $^

worker: worker.c
	$(CC) $(CFLAGS) -lm -pthread -o $@ $^

CFLAGS := -Wall -O2 -std=c99
.PHONY: all clean

clean:
	rm -rf *.o supervisor worker
