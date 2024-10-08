#!/bin/bash

# Define colors for better visualization
GREEN='\033[0;32m'
YELLOW='\033[1;33m'
CYAN='\033[1;36m'
RESET='\033[0m'
separator="========================================"

# Check if an argument is provided
if [ -z "$1" ]
then
  echo "Usage: $0 <number>"
  exit 1
fi

NUMBER=$1

run_language() {
  local lang_name=$1
  local command=$2
  echo -e "${CYAN}${separator}${RESET}"
  echo -e "${YELLOW}Running $lang_name implementation...${RESET}"
  echo -e "${CYAN}${separator}${RESET}"
  eval "$command"
  echo -e "${GREEN}$lang_name implementation completed.${RESET}"
  echo -e "${CYAN}${separator}${RESET}"
}

# Run all language implementations

run_language "Python" "python3 languages/python/prime.py <<< $NUMBER"
run_language "Java" "javac languages/java/Prime.java && java -cp languages/java Prime <<< $NUMBER"
run_language "C" "gcc languages/c/prime.c -o languages/c/prime -lm && ./languages/c/prime <<< $NUMBER"
run_language "JavaScript" "node languages/javascript/prime.js <<< $NUMBER"
run_language "Ruby" "ruby languages/ruby/prime.rb <<< $NUMBER"
run_language "PHP" "php languages/php/prime.php <<< $NUMBER"
run_language "Go" "go run languages/go/prime.go <<< $NUMBER"
run_language "Perl" "perl languages/perl/prime.pl <<< $NUMBER"
run_language "Swift" "swift languages/swift/prime.swift <<< $NUMBER"
run_language "R" "Rscript languages/r/prime.R <<< $NUMBER"
run_language "Kotlin" "kotlinc languages/kotlin/Prime.kt -include-runtime -d languages/kotlin/Prime.jar && java -jar languages/kotlin/Prime.jar <<< $NUMBER"
run_language "Rust" "rustc languages/rust/prime.rs -o languages/rust/prime && ./languages/rust/prime <<< $NUMBER"
run_language "Haskell" "ghc languages/haskell/prime.hs -o languages/haskell/prime && ./languages/haskell/prime <<< $NUMBER"
run_language "Scala" "scalac languages/scala/Prime.scala -d languages/scala && scala -cp languages/scala Prime <<< $NUMBER"
run_language "Lua" "lua languages/lua/prime.lua <<< $NUMBER"
run_language "TypeScript" "tsc languages/typescript/prime.ts && node languages/typescript/prime.js <<< $NUMBER"
run_language "F#" "fsharpi languages/fsharp/prime.fsx <<< $NUMBER"
run_language "Dart" "dart languages/dart/prime.dart <<< $NUMBER"
run_language "Elixir" "elixir languages/elixir/prime.exs <<< $NUMBER"
run_language "Julia" "julia languages/julia/prime.jl <<< $NUMBER"
run_language "Objective-C" "gcc languages/objc/prime.m -o languages/objc/prime -lm -std=c99 && ./languages/objc/prime <<< $NUMBER"
run_language "V" "v languages/v/prime.v && ./languages/v/prime <<< $NUMBER"
run_language "C#" "mcs languages/c#/prime.cs -out:languages/c#/prime.exe && mono languages/c#/prime.exe <<< $NUMBER"
run_language "Ada" "gnatmake -o languages/ada/prime languages/ada/prime.adb && ./languages/ada/prime <<< $NUMBER"
run_language "Prolog" "swipl -q -s languages/prolog/prime.pl $NUMBER"
run_language "Arduino" "echo 'Please upload and run languages/arduino/prime.ino on an Arduino board.'"
run_language "Assembly" "nasm -f elf64 -o languages/assembly/prime.o languages/assembly/prime.asm && ld -o languages/assembly/prime languages/assembly/prime.o && ./languages/assembly/prime <<< $NUMBER"
run_language "AWK" "awk -f languages/awk/prime.awk <<< $NUMBER"
run_language "Bash" "bash languages/bash/prime.sh $NUMBER"
run_language "C++" "g++ -o languages/c++/prime languages/c++/prime.cpp && ./languages/c++/prime <<< $NUMBER"
run_language "Clojure" "clojure languages/clojure/prime.clj <<< $NUMBER"
run_language "ColdFusion" "echo 'Please run languages/coldFusion/prime.cfm on a ColdFusion server.'"
run_language "OpenCL" "command -v clcc >/dev/null 2>&1 && clcc languages/opencl/prime.cl -o languages/opencl/prime && ./languages/opencl/prime $NUMBER || echo 'OpenCL not found, skipping.'"
run_language "OCaml" "ocamlc -o languages/ocaml/prime languages/ocaml/prime.ml && ./languages/ocaml/prime <<< $NUMBER"
run_language "Pascal" "fpc languages/pascal/prime.pas && ./languages/pascal/prime <<< $NUMBER"
run_language "Blockly (JavaScript)" "node languages/blockly/prime.js <<< $NUMBER"
run_language "Chaincode (Go)" "go run languages/chaincode/prime.go <<< $NUMBER"
run_language "Cirq (Quantum)" "python3 languages/cirq/prime.py <<< $NUMBER"
run_language "Fortran" "gfortran -o languages/fortran/prime languages/fortran/prime.f90 && ./languages/fortran/prime <<< $NUMBER"
run_language "Forth" "gforth languages/forth/prime.fs <<< $NUMBER"
run_language "GraphQL" "node languages/graphql/prime.js <<< $NUMBER"
# run_language "J" "echo 'prime_j_check $NUMBER' | jconsole languages/j/prime.ijs"
run_language "Lisp" "sbcl --script languages/lisp/prime.lisp <<< $NUMBER"
run_language "MATLAB" "octave --no-gui languages/matlab/prime.m <<< $NUMBER"
run_language "MicroPython" "micropython languages/micropython/prime.py <<< $NUMBER"
run_language "PowerShell" "pwsh languages/powershell/prime.ps1 <<< $NUMBER"
run_language "Q#" "cd languages/q#/PrimeChecker && dotnet run -- $NUMBER"
run_language "Qiskit" "python3 languages/qiskit/prime.py <<< $NUMBER"
run_language "Racket" "racket languages/racket/prime.rkt <<< $NUMBER"
run_language "SAS" "sas -nodms languages/sas/prime.sas -log languages/sas/prime.log -print languages/sas/prime.lst <<< $NUMBER"
run_language "Solidity" "echo 'Please deploy and run PrimeChecker.sol using a Solidity development environment (like Remix or Ganache).'"
run_language "SQL" "mysql -u root -p'yourpassword' -e 'source languages/sql/prime.sql; CALL isPrime($NUMBER, @result); SELECT @result;'"
run_language "SystemVerilog" "
verilator --cc languages/systemverilog/prime.sv --exe languages/systemverilog/testbench.cpp --build &&
./obj_dir/Vprime_checker $NUMBER
"
run_language "Tcl" "tclsh languages/tcl/prime.tcl $NUMBER"
run_language "Vala" "valac --cc=gcc languages/valas/prime.vala -o languages/valas/prime && ./languages/valas/prime $NUMBER"
run_language "Verilog" "iverilog -o languages/verilog/prime_checker languages/verilog/prime.v languages/verilog/testbench.v && vvp languages/verilog/prime_checker"
run_language "VHDL" "ghdl -a languages/vhdl/prime.vhdl && ghdl -a languages/vhdl/testbench.vhdl && ghdl -e testbench && ghdl -r testbench --stop-time=40ns --vcd=languages/vhdl/prime_checker.vcd"
run_language "Vyper" "vyper -f bytecode languages/vyper/prime.vy -o languages/vyper/prime.bytecode"
run_language "Eiffel" "ec -config languages/eiffel/prime.e && ./prime <<< $NUMBER" # Eiffel added back

# End of Script
echo -e "${CYAN}$separator${RESET}"
echo -e "${GREEN}All implementations executed.${RESET}"
echo -e "${CYAN}$separator${RESET}"
