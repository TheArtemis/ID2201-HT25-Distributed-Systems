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

# Get current working directory early for safer matching
CURRENT_DIR="$(pwd)"
echo -e "${BLUE}📂 Working directory: ${CYAN}$CURRENT_DIR${NC}"

# Kill any existing Erlang nodes (robust: matches erl/beam.smp and -sname/-name args)
echo -e "${RED}🔪 Killing existing Erlang nodes...${NC}"
kill_node() {
    local name="$1"
    # Look for Erlang VM processes (erl or beam.smp) whose args contain -sname or -name with the node name
    local pids
    pids=$(pgrep -af 'erl|beam.smp' 2>/dev/null | grep -E -- "-sname[[:space:]]+$name\\b|-name[[:space:]]+$name\\b" | awk '{print $1}')
    if [ -n "$pids" ]; then
        echo -e "${YELLOW}Found processes for node '${name}': ${pids}${NC}"
        kill $pids 2>/dev/null || kill -9 $pids 2>/dev/null
        sleep 1
    else
        # Fallback: fuzzy match the name in process arguments (case-insensitive)
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

# Try to kill common node names used by this project
kill_node "apple"
kill_node "banana"
kill_node "cherry"
kill_node "pear"

# Also attempt to kill any leftover erl/beam.smp processes that were started from this working dir
pgrep -af 'erl|beam.smp' 2>/dev/null | grep -- "$CURRENT_DIR" >/dev/null 2>&1
if [ $? -eq 0 ]; then
    echo -e "${YELLOW}Killing Erlang processes started from ${CURRENT_DIR}${NC}"
    pgrep -af 'erl|beam.smp' 2>/dev/null | grep -- "$CURRENT_DIR" | awk '{print $1}' | xargs -r kill 2>/dev/null || true
    sleep 1
fi

# Compile project
echo -e "${YELLOW}⚙️  Compiling project...${NC}"
cd "$CURRENT_DIR" && make

if [ $? -ne 0 ]; then
    echo -e "${RED}❌ Compilation failed! Exiting...${NC}"
    exit 1
fi

echo -e "${GREEN}✅ Compilation successful!${NC}"

BEAM_DIR="$CURRENT_DIR/bin"
EVAL_ITALY=""
EVAL_SPAIN=""

APPLE_CMD="cd '$CURRENT_DIR' && erl -pa '$BEAM_DIR' -setcookie 1234 -sname apple"
BANANA_CMD="cd '$CURRENT_DIR' && erl -pa '$BEAM_DIR' -setcookie 1234 -sname banana"
CHERRY_CMD="cd '$CURRENT_DIR' && erl -pa '$BEAM_DIR' -setcookie 1234 -sname cherry"
PEAR_CMD="cd '$CURRENT_DIR' && erl -pa '$BEAM_DIR' -setcookie 1234 -sname pear"

echo ""
echo -e "${PURPLE}${BOLD}=== 🚀 Starting Erlang nodes automatically ===${NC}"
echo -e "${WHITE}🍎 APPLE will start in ${GREEN}top-left${WHITE}, 🍌 BANANA in ${BLUE}top-right${WHITE}, 🍒 CHERRY in ${YELLOW}bottom-left${WHITE}, 🍐 PEAR in ${CYAN}bottom-right${NC}"
echo -e "${PURPLE}${BOLD}===========================================${NC}"
echo ""

# Windows Terminal profile to use (set to `happylemon` to use your profile)
WT_PROFILE="Happy Lemon"

# Launch Windows Terminal with split panes and auto-start nodes (2x2 grid using horizontal/vertical splits)
wt.exe new-tab --profile "$WT_PROFILE" --title "🍎 APPLE - Erlang" bash -c "$APPLE_CMD" \; split-pane --profile "$WT_PROFILE" --horizontal --title "🍌 BANANA" bash -c "$BANANA_CMD" \; split-pane --profile "$WT_PROFILE" --vertical --title "🍒 CHERRY" bash -c "$CHERRY_CMD" \; move-focus up \; split-pane --profile "$WT_PROFILE" --vertical --title "🍐 PEAR" bash -c "$PEAR_CMD"

echo -e "${GREEN}🎉 Terminal launched with auto-started nodes!${NC}"
echo ""
echo -e "${CYAN}${BOLD}Manual commands (if needed):${NC}"
echo -e "${WHITE}🍎 APPLE: ${YELLOW}$APPLE_CMD${NC}"
echo -e "${WHITE}🍌 BANANA: ${YELLOW}$BANANA_CMD${NC}"
echo -e "${WHITE}🍒 CHERRY: ${YELLOW}$CHERRY_CMD${NC}"
echo -e "${WHITE}🍐 PEAR: ${YELLOW}$PEAR_CMD${NC}"
