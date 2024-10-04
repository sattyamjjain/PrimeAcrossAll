#!/bin/bash

# Check if an argument is provided
if [ -z "$1" ]
then
  echo "Usage: $0 <number>"
  exit 1
fi

NUMBER=$1

# Python implementation
echo "Running Python implementation..."
python3 languages/python/prime.py << EOF
$NUMBER
EOF

# Java implementation
echo "Running Java implementation..."
javac languages/java/Prime.java
java -cp languages/java Prime << EOF
$NUMBER
EOF

# C implementation
echo "Running C implementation..."
gcc languages/c/prime.c -o languages/c/prime -lm
./languages/c/prime << EOF
$NUMBER
EOF

# JavaScript implementation
echo "Running JavaScript implementation..."
node languages/javascript/prime.js << EOF
$NUMBER
EOF

# Ruby implementation
echo "Running Ruby implementation..."
ruby languages/ruby/prime.rb << EOF
$NUMBER
EOF

# PHP implementation
echo "Running PHP implementation..."
php languages/php/prime.php << EOF
$NUMBER
EOF

# Go implementation
echo "Running Go implementation..."
go run languages/go/prime.go << EOF
$NUMBER
EOF

# Perl implementation
echo "Running Perl implementation..."
perl languages/perl/prime.pl << EOF
$NUMBER
EOF

# Swift implementation
echo "Running Swift implementation..."
swift languages/swift/prime.swift << EOF
$NUMBER
EOF

# R implementation
echo "Running R implementation..."
Rscript languages/r/prime.R << EOF
$NUMBER
EOF

# Kotlin implementation
echo "Running Kotlin implementation..."
kotlinc languages/kotlin/Prime.kt -include-runtime -d languages/kotlin/Prime.jar
java -jar languages/kotlin/Prime.jar << EOF
$NUMBER
EOF

# Rust implementation
echo "Running Rust implementation..."
rustc languages/rust/prime.rs -o languages/rust/prime
./languages/rust/prime << EOF
$NUMBER
EOF

# Haskell implementation
echo "Running Haskell implementation..."
ghc languages/haskell/prime.hs -o languages/haskell/prime
./languages/haskell/prime << EOF
$NUMBER
EOF

# Scala implementation
echo "Running Scala implementation..."
scalac languages/scala/Prime.scala -d languages/scala
scala -cp languages/scala Prime << EOF
$NUMBER
EOF

# Lua implementation
echo "Running Lua implementation..."
lua languages/lua/prime.lua << EOF
$NUMBER
EOF

# TypeScript implementation
echo "Running TypeScript implementation..."
tsc languages/typescript/prime.ts
node languages/typescript/prime.js << EOF
$NUMBER
EOF

# F# implementation
echo "Running F# implementation..."
fsharpi languages/fsharp/prime.fsx << EOF
$NUMBER
EOF

# Dart implementation
echo "Running Dart implementation..."
dart languages/dart/prime.dart << EOF
$NUMBER
EOF

# Elixir implementation
echo "Running Elixir implementation..."
elixir languages/elixir/prime.exs << EOF
$NUMBER
EOF

# Julia implementation
echo "Running Julia implementation..."
julia languages/julia/prime.jl << EOF
$NUMBER
EOF

# Objective-C implementation
echo "Running Objective-C implementation..."
gcc languages/objc/prime.m -o languages/objc/prime -lm -std=c99
./languages/objc/prime << EOF
$NUMBER
EOF

# V implementation
echo "Running V implementation..."
v languages/v/prime.v
./languages/v/prime << EOF
$NUMBER
EOF

echo "All implementations executed."
