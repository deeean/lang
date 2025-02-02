# lang
An experimental statically-typed language that compiles to LLVM

## Syntax
```lang
fn main() -> i32 {
    var i: i32 = 0;
    
    while (i < 10) {
        i = i + 1;
        printf("%d\n", i);
    }
    
    return 0;
}
```
