ERL_SOURCES := $(wildcard *.erl)
ERL_OBJECTS := $(ERL_SOURCES:.erl=.beam)

all: $(ERL_OBJECTS)

%.beam: %.erl
	erlc $<

clean:
	rm -f *.beam