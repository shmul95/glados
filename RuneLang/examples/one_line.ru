def f(n: i32)->i32 { if n <= 1 { n } else { f(n - 1) + f(n - 2) } }
struct Person { name: string; age: i32; def greet(self)->null{show("hello ");show(self.name);show("\n");} }
def main()->null { p = Person { name: "Alice", age: 30 }; p.greet(); show("Fibonacci(10) = "); show(f(10)); show("\n"); }
