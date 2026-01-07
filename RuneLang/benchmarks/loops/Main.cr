total_steps = 0_i64

(1_i64..1_000_000_i64).each do |i|
  n = i

  while n != 1
    if n.even?
      n = n // 2
    else
      n = n &* 3 &+ 1
    end
    total_steps &+= 1
  end
end

print total_steps
