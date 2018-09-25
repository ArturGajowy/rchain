#!/bin/bash
set -e
SCRIPT_DIR="$(cd "$(dirname "${BASH_SOURCE[0]}")" && pwd)"

echo "Checking for scalafmt availability... "
which scalafmt

PRE_COMMIT_HOOK_PATH=${SCRIPT_DIR}/../.git/hooks/pre-commit
touch ${PRE_COMMIT_HOOK_PATH} || true #make sure the file exists so we can delete it next, and then link it
rm ${PRE_COMMIT_HOOK_PATH}
ln -s ${SCRIPT_DIR}/pre-commit-hook.sh ${PRE_COMMIT_HOOK_PATH}
cp ${SCRIPT_DIR}/pre-commit-hook.sh ${PRE_COMMIT_HOOK_PATH}-hook.sh

echo "Pre-commit-hook successfully installed."
