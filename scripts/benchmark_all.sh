#!/bin/bash

# Number to check if prime (passed as argument or defaults to 1000000)
NUMBER=${1:-1000000}
OUTPUT_FILE="benchmark_results.json"

# Ensure the output file is created in the script's directory
SCRIPT_DIR=$(dirname "$0")
OUTPUT_FILE="$SCRIPT_DIR/$OUTPUT_FILE"

# Initialize the output JSON file
echo "{" > $OUTPUT_FILE
echo "  \"benchmarks\": [" >> $OUTPUT_FILE

# Function to benchmark a language and capture performance data
benchmark_language() {
    local language=$1
    local command=$2
    local loc=$3
    local lang_type=$4
    local concurrency=$5
    local dependencies=$6

    echo "Benchmarking $language implementation..."

    # Run the command with time and capture all output
    OUTPUT=$(/usr/bin/time -f "\nElapsed: %e\nUser: %U\nSystem: %S\nMaxMemory: %M" $command $NUMBER 2>&1)

    # Extract the key metrics from the output
    REAL_TIME=$(echo "$OUTPUT" | grep -oP 'Elapsed: \K[0-9.]+')
    if [[ -n "$REAL_TIME" ]]; then
        REAL_TIME=$(awk "BEGIN {printf \"%.0f\", $REAL_TIME * 1000}")
    else
        REAL_TIME="Failed"
    fi

    USER_TIME=$(echo "$OUTPUT" | grep -oP 'User: \K[0-9.]+')
    if [[ -n "$USER_TIME" ]]; then
        USER_TIME=$(awk "BEGIN {printf \"%.0f\", $USER_TIME * 1000}")
    else
        USER_TIME="Failed"
    fi

    SYS_TIME=$(echo "$OUTPUT" | grep -oP 'System: \K[0-9.]+')
    if [[ -n "$SYS_TIME" ]]; then
        SYS_TIME=$(awk "BEGIN {printf \"%.0f\", $SYS_TIME * 1000}")
    else
        SYS_TIME="Failed"
    fi

    MAX_MEMORY=$(echo "$OUTPUT" | grep -oP 'MaxMemory: \K[0-9]+')
    if [[ -z "$MAX_MEMORY" ]]; then
        MAX_MEMORY="N/A"
    fi

    # Append results to the output JSON file with additional details (LOC, language type, concurrency support, and dependencies)
    echo "    {" >> $OUTPUT_FILE
    echo "      \"language\": \"$language\"," >> $OUTPUT_FILE
    echo "      \"real_time_ms\": \"$REAL_TIME\"," >> $OUTPUT_FILE
    echo "      \"user_time_ms\": \"$USER_TIME\"," >> $OUTPUT_FILE
    echo "      \"sys_time_ms\": \"$SYS_TIME\"," >> $OUTPUT_FILE
    echo "      \"max_memory_kb\": \"$MAX_MEMORY\"," >> $OUTPUT_FILE
    echo "      \"loc\": \"$loc\"," >> $OUTPUT_FILE
    echo "      \"language_type\": \"$lang_type\"," >> $OUTPUT_FILE
    echo "      \"concurrency_support\": \"$concurrency\"," >> $OUTPUT_FILE
    echo "      \"dependencies\": \"$dependencies\"" >> $OUTPUT_FILE
    echo "    }," >> $OUTPUT_FILE
}

# Ensure compiled languages are built before benchmarking

# C: Compiled
if [[ ! -f languages/c/prime ]]; then
    gcc -o languages/c/prime languages/c/prime.c || { echo "C compilation failed"; exit 1; }
fi

# Rust: Compiled
if [[ ! -f languages/rust/prime ]]; then
    rustc languages/rust/prime.rs -o languages/rust/prime || { echo "Rust compilation failed"; exit 1; }
fi

# Haskell: Compiled
if [[ ! -f languages/haskell/prime ]]; then
    ghc -o languages/haskell/prime languages/haskell/prime.hs || { echo "Haskell compilation failed"; exit 1; }
fi

# Run benchmarks for each language

# Python: Interpreted
benchmark_language "Python" "python3 languages/python/prime.py" "15" "Interpreted" "Async/Await" "Minimal (Python3)"

# Java: Compiled
benchmark_language "Java" "java -cp languages/java Prime" "25" "Compiled" "Threads" "Requires JVM"

# C: Compiled
benchmark_language "C" "./languages/c/prime" "30" "Compiled" "Pthreads" "Requires gcc compiler"

# JavaScript: Interpreted
benchmark_language "JavaScript" "node languages/javascript/prime.js" "20" "Interpreted" "Limited (event-driven)" "Requires Node.js"

# Ruby: Interpreted
benchmark_language "Ruby" "ruby languages/ruby/prime.rb" "20" "Interpreted" "Limited" "Minimal (Ruby interpreter)"

# PHP: Interpreted
benchmark_language "PHP" "php languages/php/prime.php" "15" "Interpreted" "No native concurrency" "Requires PHP runtime"

# Go: Compiled
benchmark_language "Go" "go run languages/go/prime.go" "25" "Compiled" "Goroutines" "Minimal (Go runtime)"

# Perl: Interpreted
benchmark_language "Perl" "perl languages/perl/prime.pl" "18" "Interpreted" "Limited" "Minimal (Perl interpreter)"

# Swift: Compiled
benchmark_language "Swift" "swift languages/swift/prime.swift" "20" "Compiled" "Grand Central Dispatch" "Requires Swift runtime"

# R: Interpreted
benchmark_language "R" "Rscript languages/r/prime.R" "10" "Interpreted" "No native concurrency" "Requires R environment"

# Kotlin: Compiled
benchmark_language "Kotlin" "java -jar languages/kotlin/Prime.jar" "25" "Compiled" "Coroutines" "Requires JVM"

# Rust: Compiled
benchmark_language "Rust" "./languages/rust/prime" "35" "Compiled" "Native Threads" "Requires Rust compiler"

# Haskell: Compiled
benchmark_language "Haskell" "./languages/haskell/prime" "20" "Compiled" "Green Threads" "Requires GHC (Haskell compiler)"

# Scala: Compiled
benchmark_language "Scala" "scala -cp languages/scala Prime" "30" "Compiled" "Akka (Concurrency)" "Requires Scala runtime"

# Lua: Interpreted
benchmark_language "Lua" "lua languages/lua/prime.lua" "12" "Interpreted" "No native concurrency" "Minimal (Lua interpreter)"

# TypeScript: Interpreted
benchmark_language "TypeScript" "node languages/typescript/prime.js" "20" "Interpreted" "Event-driven concurrency" "Requires Node.js"

# F#: Compiled
benchmark_language "F#" "fsharpi languages/fsharp/prime.fsx" "20" "Compiled" "Async/Await" ".NET Runtime"

# Dart: Compiled
benchmark_language "Dart" "dart languages/dart/prime.dart" "25" "Compiled" "Async/Await" "Requires Dart SDK"

# Elixir: Interpreted
benchmark_language "Elixir" "elixir languages/elixir/prime.exs" "15" "Interpreted" "Actor-based concurrency (Erlang VM)" "Requires Erlang/Elixir"

# Julia: Compiled
benchmark_language "Julia" "julia languages/julia/prime.jl" "15" "Compiled" "Parallelism" "Requires Julia runtime"

# Objective-C: Compiled
benchmark_language "Objective-C" "./languages/objc/prime" "35" "Compiled" "Grand Central Dispatch" "Requires GNUstep"

# V: Compiled
benchmark_language "V" "./languages/v/prime" "25" "Compiled" "Concurrency Support" "Requires V compiler"

# Finalize the JSON structure
sed -i '$ s/,$//' $OUTPUT_FILE  # Remove the trailing comma from the last element
echo "  ]" >> $OUTPUT_FILE
echo "}" >> $OUTPUT_FILE

echo "All benchmarks completed."

# Display the JSON file
echo ""
echo "==================== Benchmark Results (JSON) ===================="
cat $OUTPUT_FILE
echo "============================================================"