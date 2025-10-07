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
kill_node "italy"
kill_node "spain"
kill_node "france"
kill_node "sweden"

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

ITALY_CMD="cd '$CURRENT_DIR' && erl -pa '$BEAM_DIR' -setcookie 1234 -sname italy"
SPAIN_CMD="cd '$CURRENT_DIR' && erl -pa '$BEAM_DIR' -setcookie 1234 -sname spain"
FRANCE_CMD="cd '$CURRENT_DIR' && erl -pa '$BEAM_DIR' -setcookie 1234 -sname france"
SWEDEN_CMD="cd '$CURRENT_DIR' && erl -pa '$BEAM_DIR' -setcookie 1234 -sname sweden"

echo ""
echo -e "${PURPLE}${BOLD}=== 🚀 Starting Erlang nodes automatically ===${NC}"
echo -e "${WHITE}🇮🇹 ITALY will start in ${GREEN}top-left${WHITE}, 🇪🇸 SPAIN in ${BLUE}top-right${WHITE}, 🇫🇷 FRANCE in ${YELLOW}bottom-left${WHITE}, 🇸🇪 SWEDEN in ${CYAN}bottom-right${NC}"
echo -e "${PURPLE}${BOLD}===========================================${NC}"
echo ""

# Windows Terminal profile to use (set to `Happy Lemon` to use your profile)
WT_PROFILE="Happy Lemon"

# Launch Windows Terminal with split panes and auto-start nodes (2x2 grid using horizontal/vertical splits)
# Layout: new-tab (Italy) -> split horizontal (Spain) -> split vertical (France) -> move up -> split vertical (Sweden)
wt.exe new-tab --profile "$WT_PROFILE" --title "ITALY" bash -c "$ITALY_CMD" \; split-pane --profile "$WT_PROFILE" --horizontal --title "SPAIN" bash -c "$SPAIN_CMD" \; split-pane --profile "$WT_PROFILE" --vertical --title "FRANCE" bash -c "$FRANCE_CMD" \; move-focus up \; split-pane --profile "$WT_PROFILE" --vertical --title "SWEDEN" bash -c "$SWEDEN_CMD"

echo -e "${GREEN}🎉 Terminal launched with auto-started nodes!${NC}"
echo ""
echo -e "${CYAN}${BOLD}Manual commands (if needed):${NC}"
echo -e "${WHITE}🇮🇹 ITALY: ${YELLOW}$ITALY_CMD${NC}"
echo -e "${WHITE}🇪🇸 SPAIN: ${YELLOW}$SPAIN_CMD${NC}"
echo -e "${WHITE}🇫🇷 FRANCE: ${YELLOW}$FRANCE_CMD${NC}"
echo -e "${WHITE}🇸🇪 SWEDEN: ${YELLOW}$SWEDEN_CMD${NC}"
