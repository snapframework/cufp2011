#!/bin/sh

set -e

if [ -z "$DEBUG" ]; then
    export DEBUG="testsuite"
fi

SUITE=./dist/build/testsuite-sample/testsuite-sample

export LC_ALL=C
export LANG=C

rm -f testsuite-sample.tix

if [ ! -f $SUITE ]; then
    cat <<EOF
Testsuite executable not found, please run:
    cabal configure -ftest
then
    cabal build
EOF
    exit;
fi

${SUITE} -j4 -a1000 $*

DIR=dist/hpc

rm -Rf $DIR
mkdir -p $DIR

EXCLUDES='Main
Snap.Chat.ChatRoom.Tests
Snap.Chat.Test.Common
Snap.Chat.Types.Tests
'

EXCL=""

for m in $EXCLUDES; do
    EXCL="$EXCL --exclude=$m"
done

hpc markup $EXCL --destdir=$DIR testsuite-sample >/dev/null 2>&1

rm -f testsuite-sample.tix

cat <<EOF

Test coverage report written to $DIR.
EOF
