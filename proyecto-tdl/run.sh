#/bin/sh

if [ -z "$1" ]; then
    echo "Must specify a file to run"
    exit 1
fi

ghc -outputdir bin Main.hs && ./Main $1
