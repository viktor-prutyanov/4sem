fox: fox.c 
	$(MPICC) $(CFLAGS) -o $@ $^ $(LDFLAGS)

MPICC := mpicc
CFLAGS := -Wall -O2 -std=c99
LDFLAGS := -lm
.PHONY: clean

clean:
	rm -rf *.o fox
