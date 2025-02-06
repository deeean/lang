# lang
An experimental statically-typed language that compiles to LLVM

## Syntax

### Fibonacci
```lang
fn fibonacci(n: i32) -> i32 {
    if n <= 1 {
        return n;
    }

    return fibonacci(n - 1) + fibonacci(n - 2);
}

fn main() -> i32 {
    printf("%d\n", fibonacci(10));
    return 0;
}
```

### FizzBuzz
```lang
fn main() -> i32 {
    var i: i32 = 1;

    while i <= 100 {
        if i % 15 == 0 {
            printf("FizzBuzz\n");
        } else if i % 3 == 0 {
            printf("Fizz\n");
        } else if i % 5 == 0 {
            printf("Buzz\n");
        } else {
            printf("%d\n", i);
        }

        i = i + 1;
    }

    return 0;
}
```
