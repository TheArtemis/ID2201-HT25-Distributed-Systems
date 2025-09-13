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

# Kill any existing Erlang nodes
echo -e "${RED}🔪 Killing existing Erlang nodes...${NC}"
pkill -f "erl.*sname.*client" 2>/dev/null
pkill -f "erl.*sname.*server" 2>/dev/null
sleep 1

# Get current working directory
CURRENT_DIR="$(pwd)"
echo -e "${BLUE}📂 Working directory: ${CYAN}$CURRENT_DIR${NC}"

# Compile project
echo -e "${YELLOW}⚙️  Compiling project...${NC}"
cd "$CURRENT_DIR" && make

if [ $? -ne 0 ]; then
    echo -e "${RED}❌ Compilation failed! Exiting...${NC}"
    exit 1
fi

echo -e "${GREEN}✅ Compilation successful!${NC}"

# Create startup commands for server and client
BEAM_DIR="$CURRENT_DIR/bin"
SERVER_CMD="cd '$CURRENT_DIR' && erl -pa '$BEAM_DIR' -setcookie 1234 -sname italy -connect_all_false -eval 'routy:start(rome, rome)'"
CLIENT_CMD="cd '$CURRENT_DIR' && erl -pa '$BEAM_DIR' -setcookie 1234 -sname spain -connect_all_false -eval 'routy:start(madrid, madrid)'"

echo ""
echo -e "${PURPLE}${BOLD}=== 🚀 Starting Erlang nodes automatically ===${NC}"
echo -e "${WHITE}Server will start in ${GREEN}left pane${WHITE}, Client in ${BLUE}right pane${NC}"
echo -e "${PURPLE}${BOLD}===========================================${NC}"
echo ""

# Launch Windows Terminal with split panes and auto-start nodes
wt.exe new-tab --title "Erlang Development" bash -c "$SERVER_CMD" \; split-pane --title "Client" bash -c "$CLIENT_CMD"

echo -e "${GREEN}🎉 Terminal launched with auto-started nodes!${NC}"
echo ""
echo -e "${CYAN}${BOLD}Manual commands (if needed):${NC}"
echo -e "${WHITE}Server: ${YELLOW}$SERVER_CMD${NC}"

echo -e "${WHITE}Client: ${YELLOW}$CLIENT_CMD${NC}"

