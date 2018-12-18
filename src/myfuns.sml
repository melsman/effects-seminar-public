fun plus a b : real = a + b
fun swap1 (a,b) = (b,a)
fun swap2 (a,b) = if false then (a,b) else (b,a)
fun pairgen a b = (a,b)
fun apply f x = f x
fun op o (f,g) x = f(g x)
