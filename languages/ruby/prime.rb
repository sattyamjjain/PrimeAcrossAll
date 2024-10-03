def is_prime(n)
    return false if n <= 1
    return true if n == 2
    return false if n % 2 == 0
  
    (3..Math.sqrt(n)).step(2).each do |i|
      return false if n % i == 0
    end
    true
  end
  
  puts "Enter a number to check if it's prime:"
  number = gets.to_i
  
  if is_prime(number)
    puts "#{number} is a prime number."
  else
    puts "#{number} is not a prime number."
  end
  