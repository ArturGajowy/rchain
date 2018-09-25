#!/bin/bash

START=$(date +%s.%N)
START=${START%??????} #trunc to millis

_DIR=$( cd "$( dirname "${BASH_SOURCE[0]}" )" && pwd )
DIR=$( echo $_DIR | sed 's/\/.git\/hooks$//' )

scalafmt --test --diff-branch HEAD &> /dev/null
FORMATTED=$?

if [ ${FORMATTED} -eq 0 ]
then
  echo "All files formatted properly."
else
  echo "Reformatting. Please check the results and commit again."
  time scalafmt --diff-branch HEAD
  STATUS=1
fi

END=$(date +%s.%N)
END=${END%??????} #trunc to millis
echo "Total time taken by hook: $(echo "$END - $START" | bc) s."
exit ${STATUS}
