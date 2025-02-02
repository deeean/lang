# lang
실험적인 언어로, 정적타입언어이며, LLVM으로 컴파일 됩니다.

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
