#!/bin/bash

# Color definitions
RED='\033[0;31m'
GREEN='\033[0;32m'
YELLOW='\033[1;33m'
BLUE='\033[0;34m'
PURPLE='\033[0;35m'
CYAN='\033[0;36m'
WHITE='\033[1;37m'
BOLD='\033[1m'
NC='\033[0m' # No Color

CURRENT_DIR="$(pwd)"
echo -e "${BLUE}📂 Working directory: ${CYAN}$CURRENT_DIR${NC}"

echo -e "${RED}🔪 Killing existing Erlang nodes...${NC}"
kill_node() {
    local name="$1"
    local pids
    pids=$(pgrep -af 'erl|beam.smp' 2>/dev/null | grep -E -- "-sname[[:space:]]+$name\\b|-name[[:space:]]+$name\\b" | awk '{print $1}')
    if [ -n "$pids" ]; then
        echo -e "${YELLOW}Found processes for node '${name}': ${pids}${NC}"
        kill $pids 2>/dev/null || kill -9 $pids 2>/dev/null
        sleep 1
    else
        pids=$(pgrep -af 'erl|beam.smp' 2>/dev/null | grep -i -- "$name" | awk '{print $1}')
        if [ -n "$pids" ]; then
            echo -e "${YELLOW}Fallback: found processes for '${name}': ${pids}${NC}"
            kill $pids 2>/dev/null || kill -9 $pids 2>/dev/null
            sleep 1
        else
            echo -e "${GREEN}No running Erlang process found for node '${name}'.${NC}"
        fi
    fi
}

kill_node "groupy"

pgrep -af 'erl|beam.smp' 2>/dev/null | grep -- "$CURRENT_DIR" >/dev/null 2>&1
if [ $? -eq 0 ]; then
    echo -e "${YELLOW}Killing Erlang processes started from ${CURRENT_DIR}${NC}"
    pgrep -af 'erl|beam.smp' 2>/dev/null | grep -- "$CURRENT_DIR" | awk '{print $1}' | xargs -r kill 2>/dev/null || true
    sleep 1
fi

echo -e "${YELLOW}⚙️  Compiling project...${NC}"
cd "$CURRENT_DIR" && make

if [ $? -ne 0 ]; then
    echo -e "${RED}❌ Compilation failed! Exiting...${NC}"
    exit 1
fi

echo -e "${GREEN}✅ Compilation successful!${NC}"

cd ..
EVAL="-eval 'test_stabilization:watch_ring_formation().'"
BEAM_DIR="$CURRENT_DIR/bin"
CMD="cd '$CURRENT_DIR' && erl -pa '$BEAM_DIR' -setcookie 1234 -sname groupy $EVAL"

echo ""
echo -e "${PURPLE}${BOLD}=== 🚀 Starting Erlang node GROUPY ===${NC}"
echo ""


# Start LOGGY node in the current shell
eval "$CMD"

echo -e "${CYAN}${BOLD}Manual command (if needed):${NC}"
echo -e "${WHITE}GROUPY: ${YELLOW}$CMD${NC}"
