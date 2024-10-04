defmodule PrimeChecker do
  def is_prime(n) when n <= 1 do
    false
  end

  def is_prime(2), do: true

  def is_prime(n) do
    limit = trunc(:math.sqrt(n)) # Ensure integer range
    Enum.all?(2..limit, fn x -> rem(n, x) != 0 end)
  end
end

IO.write("Enter a number to check if it's prime: ")
input = IO.gets("") |> String.trim() |> String.to_integer()

if PrimeChecker.is_prime(input) do
  IO.puts("#{input} is a prime number.")
else
  IO.puts("#{input} is not a prime number.")
end
