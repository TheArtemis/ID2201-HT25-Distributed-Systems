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
CURRENT_DIR=$(dirname "$(pwd)")
echo -e "${BLUE}📂 Working directory: ${CYAN}$CURRENT_DIR${NC}"

# Compile project
echo -e "${YELLOW}⚙️  Compiling project...${NC}"
cd "$CURRENT_DIR" && make

if [ $? -ne 0 ]; then
    echo -e "${RED}❌ Compilation failed! Exiting...${NC}"
    exit 1
fi

echo -e "${GREEN}✅ Compilation successful!${NC}"

# Create startup commands for server and clients
SERVER_CMD="cd '$CURRENT_DIR' && erl -pa '$CURRENT_DIR/bin' -setcookie 1234 -sname server -eval 'rudy:start(8080).' "
CLIENT1_CMD="cd '$CURRENT_DIR' && erl -pa '$CURRENT_DIR/bin' -setcookie 1234 -sname client1"
CLIENT2_CMD="cd '$CURRENT_DIR' && erl -pa '$CURRENT_DIR/bin' -setcookie 1234 -sname client2"
CLIENT3_CMD="cd '$CURRENT_DIR' && erl -pa '$CURRENT_DIR/bin' -setcookie 1234 -sname client3"

echo ""
echo -e "${PURPLE}${BOLD}=== 🚀 Starting Erlang nodes automatically ===${NC}"
echo -e "${WHITE}Server will start in ${GREEN}top-left${WHITE}, Clients in ${BLUE}other panes${NC}"
echo -e "${PURPLE}${BOLD}===========================================${NC}"
echo ""

# Launch Windows Terminal with a 2x2 grid: server, client1, client2, client3
wt.exe new-tab --title "Erlang Development" bash -c "$SERVER_CMD" \; split-pane --horizontal --title "Client1" bash -c "$CLIENT1_CMD" \; split-pane --vertical --title "Client2" bash -c "$CLIENT2_CMD" \; move-focus up \; split-pane --vertical --title "Client3" bash -c "$CLIENT3_CMD"

echo -e "${GREEN}🎉 Windows Terminal launched with server and 3 clients in a 2x2 grid!${NC}"
echo ""
echo -e "${CYAN}${BOLD}Manual commands (if needed):${NC}"
echo -e "${WHITE}Server: ${YELLOW}$SERVER_CMD${NC}"
echo -e "${WHITE}Client1: ${YELLOW}$CLIENT1_CMD${NC}"
echo -e "${WHITE}Client2: ${YELLOW}$CLIENT2_CMD${NC}"
echo -e "${WHITE}Client3: ${YELLOW}$CLIENT3_CMD${NC}"

# Copy test command to clipboard
echo "test:bench_live(localhost, 8080)." | xclip -selection clipboard
echo -e "${GREEN}📋 Copied 'test:bench_live(localhost, 8080).' to clipboard!${NC}"
