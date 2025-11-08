#!/bin/bash

# Kill all Erlang processes owned by the current user
pkill -u "$USER" beam.smp

# Output the timestamp in milliseconds since epoch
# echo "Erlang processes killed at: $(date +%s)" # MacOS
echo "Erlang processes killed at: $(date +%s%3N)" 