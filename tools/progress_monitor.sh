#!/bin/bash
# Monitor CmdStan progress by counting lines in chain CSV files
# Assumes 4 chains, 2000 iterations each (1000 warmup + 1000 samples)
CHAINS=4
ITER_PER_CHAIN=2000
TOTAL_EXPECTED=$((CHAINS * ITER_PER_CHAIN))

echo "Monitoring CmdStan progress (4 chains x 2000 iter each). Press Ctrl-C to stop."
while true; do
  TOTAL_LINES=0
  CHAIN_INFO=""
  for i in {1..4}; do
    FILE=$(find /tmp -name "*model_*${i}-*.csv" -type f 2>/dev/null | head -1)
    if [ -f "$FILE" ]; then
      LINES=$(awk 'NR>1 && !/^#/' "$FILE" | wc -l)
      TOTAL_LINES=$((TOTAL_LINES + LINES))
      CHAIN_INFO="${CHAIN_INFO}Chain $i: $LINES/$ITER_PER_CHAIN "
    fi
  done
  PERCENT=$(awk "BEGIN { printf \"%.1f\", ($TOTAL_LINES / $TOTAL_EXPECTED) * 100 }")
  echo "$(date +'%H:%M:%S') - Overall: $PERCENT% ($TOTAL_LINES/$TOTAL_EXPECTED samples) | $CHAIN_INFO"
  sleep 10
done
