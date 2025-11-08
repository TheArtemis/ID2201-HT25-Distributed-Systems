#!/bin/bash

# Color codes
GREEN='\033[0;32m'
YELLOW='\033[1;33m'
CYAN='\033[0;36m'
NC='\033[0m' # No Color

echo -e "${YELLOW}🔨 Building the project with make...${NC}"
make

echo -e "${CYAN}🚀 Entering the bin directory...${NC}"
cd bin

echo -e "${GREEN}🐇 Launching Erlang shell.${NC}"
erl