#!/bin/sh
run_test() {
    FILE="$1"
    if [ ! -e "lib.o" ]; then
        gcc -c -o lib.o ../lib64/lib.c
    fi

    MSG=`../target/debug/rvml -o main.o "$FILE"`
    if [ $? != 0 ]; then
        echo "$MSG"
        echo "\033[35mError\033[0m: $FILE"
        return
    fi
    gcc -o main main.o lib.o
    OUTPUT=`./main`
    EXPECT=`ocaml $FILE`
    echo "$OUTPUT"
    if [ "$EXPECT" = "$OUTPUT" ]; then
        echo "\033[1;32mOK\033[0m: $FILE"
    else
        echo "\033[33mEXPECT\033[0m: $EXPECT"
        echo "\033[1;31mFail\033[0m: $FILE"
    fi
}

if [ ! -e "../target/debug/rvml" ]; then
    echo "You need to build rvml"
    exit 1
fi

for f in *.ml
do
    run_test "$f"
done

rm main main.o lib.o
