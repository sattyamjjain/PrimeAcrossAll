function Is-Prime($number) {
    if ($number -le 1) {
        return $false
    }
    for ($i = 2; $i -le [math]::Sqrt($number); $i++) {
        if ($number % $i -eq 0) {
            return $false
        }
    }
    return $true
}

# Get input from user
$n = Read-Host "Enter a number to check if it's prime"
if (Is-Prime -number $n) {
    Write-Host "$n is a prime number."
} else {
    Write-Host "$n is not a prime number."
}
