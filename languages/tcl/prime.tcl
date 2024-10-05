# prime.tcl - Tcl script to check if a number is prime

proc is_prime {num} {
    if {$num <= 1} {
        return 0
    }
    for {set i 2} {$i <= [expr {$num / 2}]} {incr i} {
        if {[expr {$num % $i}] == 0} {
            return 0
        }
    }
    return 1
}

# Accept number from the command line
set num [lindex $argv 0]

if {[is_prime $num]} {
    puts "$num is a prime number."
} else {
    puts "$num is not a prime number."
}
