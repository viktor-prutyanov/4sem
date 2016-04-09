MODULE:=sm
TEST_MODULE:=sm_test
ENTRY:=sm_test

all: sm_test.beam
	erl -noshell -s $(TEST_MODULE) $(ENTRY) -s init stop

sm_test.beam: sm.beam
	erlc $(TEST_MODULE).erl

sm.beam: 
	erlc $(MODULE).erl

.PHONY: clean

clean:
	rm -rf *.beam *.dump
