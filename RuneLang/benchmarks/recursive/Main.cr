def fib(n : Int64) : Int64
    if n <= 1_i64
        return n
    end
    return fib(n: n - 1_i64) &+ fib(n: n - 2_i64)
end

puts fib(40_i64)
