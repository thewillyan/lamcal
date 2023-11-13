# LamCal

An simple Lambda calculus expression evaluator. This project was made for fun
using zig `0.12.0-dev.1168+54a4f24ea`, but everything should work on 
zig `0.11` or above.

For now only the type evaluator is available, compile the binary
with:

```bash
$ zig build
```

After the compilation is complete the binary `get-type` is available at the
directory `zig-out/bin/`.

The `get-type` binary works processing a lambda calculus expression by the
stdin and printing one of the following:

1. `!`, if the expression has a syntax error.
2. `-`, if the expreession results in a type error.
3. The type T, if the expression is valid lambda calculus expression.
