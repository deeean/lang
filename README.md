# lang
An experimental statically-typed language that compiles to LLVM

## Syntax
```lang
fn main() -> i32 {
    var i: i32 = 1;

    while (i <= 100) {
        if (i % 15 == 0) {
            printf("FizzBuzz\n");
        } else if (i % 3 == 0) {
            printf("Fizz\n");
        } else if (i % 5 == 0) {
            printf("Buzz\n");
        } else {
            printf("%d\n", i);
        }

        i = i + 1;
    }

    return 0;
}

```
