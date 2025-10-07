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

# Only kill test nodes (red, blue, green, yellow)
kill_node "red"
kill_node "blue"
kill_node "green"
kill_node "yellow"

# Compile project
echo -e "${YELLOW}⚙️  Compiling project...${NC}"
cd "$CURRENT_DIR" && make

if [ $? -ne 0 ]; then
    echo -e "${RED}❌ Compilation failed! Exiting...${NC}"
    exit 1
fi

echo -e "${GREEN}✅ Compilation successful!${NC}"

BEAM_DIR="$CURRENT_DIR/bin"

RED_CMD="cd '$CURRENT_DIR' && erl -pa '$BEAM_DIR' -setcookie 1234 -sname red"
BLUE_CMD="cd '$CURRENT_DIR' && erl -pa '$BEAM_DIR' -setcookie 1234 -sname blue"
GREEN_CMD="cd '$CURRENT_DIR' && erl -pa '$BEAM_DIR' -setcookie 1234 -sname green"
YELLOW_CMD="cd '$CURRENT_DIR' && erl -pa '$BEAM_DIR' -setcookie 1234 -sname yellow"

echo ""
echo -e "${PURPLE}${BOLD}=== 🚀 Starting Erlang nodes automatically ===${NC}"
echo -e "${WHITE}❤️  RED will start in ${RED}top-left${WHITE}, 💙 BLUE in ${BLUE}top-right${WHITE}, 💚 GREEN in ${GREEN}bottom-left${WHITE}, 💛 YELLOW in ${YELLOW}bottom-right${NC}"
echo -e "${PURPLE}${BOLD}===========================================${NC}"
echo ""

# Windows Terminal profile to use (set to `Happy Lemon` to use your profile)
WT_PROFILE="Happy Lemon"

# Launch Windows Terminal with split panes and auto-start nodes (2x2 grid using horizontal/vertical splits)
# Layout: new-tab (Red) -> split horizontal (Blue) -> split vertical (Green) -> move up -> split vertical (Yellow)
wt.exe new-tab --profile "$WT_PROFILE" --title "❤️  RED" bash -c "$RED_CMD" \; split-pane --profile "$WT_PROFILE" --horizontal --title "💙 BLUE" bash -c "$BLUE_CMD" \; split-pane --profile "$WT_PROFILE" --vertical --title "💚 GREEN" bash -c "$GREEN_CMD" \; move-focus up \; split-pane --profile "$WT_PROFILE" --vertical --title "💛 YELLOW" bash -c "$YELLOW_CMD"

echo -e "${GREEN}🎉 Terminal launched with auto-started nodes!${NC}"
echo ""
echo -e "${CYAN}${BOLD}Manual commands (if needed):${NC}"
echo -e "${WHITE}❤️  RED: ${YELLOW}$RED_CMD${NC}"
echo -e "${WHITE}💙 BLUE: ${YELLOW}$BLUE_CMD${NC}"
echo -e "${WHITE}💚 GREEN: ${YELLOW}$GREEN_CMD${NC}"
echo -e "${WHITE}💛 YELLOW: ${YELLOW}$YELLOW_CMD${NC}"
