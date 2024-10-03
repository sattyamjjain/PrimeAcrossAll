#!/usr/bin/perl
use strict;
use warnings;
use POSIX qw(floor);

sub is_prime {
    my ($n) = @_;
    
    return 0 if $n <= 1;
    return 1 if $n == 2;
    return 0 if $n % 2 == 0;
    
    for (my $i = 3; $i <= floor(sqrt($n)); $i += 2) {
        return 0 if $n % $i == 0;
    }
    return 1;
}

print "Enter a number to check if it's prime: ";
my $number = <STDIN>;
chomp($number);

if (is_prime($number)) {
    print "$number is a prime number.\n";
} else {
    print "$number is not a prime number.\n";
}
