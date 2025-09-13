ERL_SOURCES := $(wildcard src/*.erl)
BIN_DIR := bin
DUMP_DIR := dumps
ERL_OBJECTS := $(patsubst src/%.erl,$(BIN_DIR)/%.beam,$(ERL_SOURCES))

# Set crash dump file path
export ERL_CRASH_DUMP := $(DUMP_DIR)/erl_crash.dump

all: $(BIN_DIR) $(DUMP_DIR) $(ERL_OBJECTS)

$(BIN_DIR):
	mkdir -p $(BIN_DIR)

$(DUMP_DIR):
	mkdir -p $(DUMP_DIR)

$(BIN_DIR)/%.beam: src/%.erl
	erlc -o $(BIN_DIR) $<

run:
	erl -pa $(BIN_DIR)

clean:
	rm -f $(BIN_DIR)/*.beam
	rm -f $(DUMP_DIR)/erl_crash.dump
