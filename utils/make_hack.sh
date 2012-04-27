#! /bin/bash

firstdir="$(pwd)"
hackdir="$(dirname $0)"

cd /

coffeenode="$(echo $(which coffee) | sed s/coffee$/node_modules\\/coffee-script\\/bin\\/\\0/)"

formatted="$(echo \"$coffeenode\" | sed s/\\/// | sed s/\\//:\\// | sed s/\\//\\\\\\\\/g)"

echo "$formatted"

cd "$firstdir"

cat > "$hackdir/coffee_hack.hs" << EOF

-- if windows can't find coffeescript, set the correct path below and compile
-- this to "coffee.exe" in the project's main folder.

module Main where

import System.Cmd (rawSystem)
import System.Environment (getArgs)

coffeeNode = $formatted

main = do args <- getArgs 
          rawSystem "node" $ coffeeNode : args

EOF

ghc -o coffee "$hackdir/coffee_hack.hs" 
