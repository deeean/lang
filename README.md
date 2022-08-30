# lang
Test programming language

## Syntax

```
let a = 1;
let b = "Hello, \"world\"";
let c = 3.141592;

println(a + b);
println(c);

let sum = 0;

for (let i = 0; i < 100; i += 1) {
  for (let j = 0; j < 100; j += 1) {
    for (let k = 0; k < 100; k += 1) {
      sum += 1;
    }
  }
}

println(sum);
```


## Builtin functions

`println` - write values to stdout

`typeof` - returns the type of the variable.

`time` - returns the current timestamp in milliseconds
