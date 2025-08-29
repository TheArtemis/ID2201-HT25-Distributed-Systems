ERL_SOURCES := $(wildcard src/*.erl)
BIN_DIR := bin
ERL_OBJECTS := $(patsubst src/%.erl,$(BIN_DIR)/%.beam,$(ERL_SOURCES))

all: $(BIN_DIR) $(ERL_OBJECTS)

$(BIN_DIR):
	mkdir -p $(BIN_DIR)

$(BIN_DIR)/%.beam: src/%.erl
	erlc -o $(BIN_DIR) $<

clean:
	rm -f $(BIN_DIR)/*.beam