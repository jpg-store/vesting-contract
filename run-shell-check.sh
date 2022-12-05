#!/bin/sh
echo "> Running shell check on all the '.sh' scripts."
find ./scripts \
  | grep -E ".*.sh" \
  | xargs shellcheck \
  > ./docs/audit/shell-check-report.txt
echo "> Done."
