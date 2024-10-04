#!/bin/bash

# Python implementation
echo "Running Python implementation..."
python3 languages/python/prime.py << EOF
17
EOF

# Java implementation
echo "Running Java implementation..."
javac languages/java/Prime.java
java -cp languages/java Prime << EOF
17
EOF

# C implementation
echo "Running C implementation..."
gcc languages/c/prime.c -o languages/c/prime -lm
./languages/c/prime << EOF
17
EOF

# JavaScript implementation
echo "Running JavaScript implementation..."
node languages/javascript/prime.js << EOF
17
EOF

# Ruby implementation
echo "Running Ruby implementation..."
ruby languages/ruby/prime.rb << EOF
17
EOF

# PHP implementation
echo "Running PHP implementation..."
php languages/php/prime.php << EOF
17
EOF

# Go implementation
echo "Running Go implementation..."
go run languages/go/prime.go << EOF
17
EOF

# Perl implementation
echo "Running Perl implementation..."
perl languages/perl/prime.pl << EOF
17
EOF

# Swift implementation
echo "Running Swift implementation..."
swift languages/swift/prime.swift << EOF
17
EOF

# R implementation
echo "Running R implementation..."
Rscript languages/r/prime.R << EOF
17
EOF

# Kotlin implementation
echo "Running Kotlin implementation..."
kotlinc languages/kotlin/Prime.kt -include-runtime -d languages/kotlin/Prime.jar
java -jar languages/kotlin/Prime.jar << EOF
17
EOF

# Rust implementation
echo "Running Rust implementation..."
rustc languages/rust/prime.rs -o languages/rust/prime
languages/rust/prime << EOF
17
EOF

# Haskell implementation
echo "Running Haskell implementation..."
ghc languages/haskell/prime.hs -o languages/haskell/prime
languages/haskell/prime << EOF
17
EOF

# Scala implementation
echo "Running Scala implementation..."
scalac languages/scala/Prime.scala -d languages/scala
scala -cp languages/scala Prime << EOF
17
EOF

# Lua implementation
echo "Running Lua implementation..."
lua languages/lua/prime.lua << EOF
17
EOF

# TypeScript implementation
echo "Running TypeScript implementation..."
tsc languages/typescript/prime.ts
node languages/typescript/prime.js << EOF
17
EOF

# F# implementation
echo "Running F# implementation..."
fsharpi languages/fsharp/prime.fsx << EOF
17
EOF

# Dart implementation
echo "Running Dart implementation..."
dart languages/dart/prime.dart << EOF
17
EOF

# Elixir implementation
echo "Running Elixir implementation..."
elixir languages/elixir/prime.exs << EOF
17
EOF

# Julia implementation
echo "Running Julia implementation..."
julia languages/julia/prime.jl << EOF
17
EOF

# Objective-C implementation
echo "Running Objective-C implementation..."
gcc languages/objc/prime.m -o languages/objc/prime -I /usr/include/GNUstep -L /usr/lib/GNUstep -lgnustep-base -lobjc -std=c99
./languages/objc/prime << EOF
17
EOF

# V implementation
echo "Running V implementation..."
v languages/v/prime.v
./languages/v/prime << EOF
17
EOF

echo "All implementations executed."
