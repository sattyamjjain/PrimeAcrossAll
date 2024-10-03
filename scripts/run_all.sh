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

echo "All implementations executed."
