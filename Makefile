ERLS=$(wildcard src/*.erl)
BEAMS=$(ERLS:src/%.erl=ebin/%.beam)

all: $(BEAMS)

ebin/%.beam: src/%.erl
	erlc -o ebin "$<"
