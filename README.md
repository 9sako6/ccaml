# CCaml

A toy C compiler written in OCaml.

## Usage

Let's print `Hello, World!`.

```bash
$ dune exec ccaml examples/valid/hello_world.c | gcc -xassembler - && ./a.out
Hello, World!
```

The `examples/valid/hello_world.c` program is the following.

```c
int putchar(int c);

int main() {
    putchar(72);
    putchar(101);
    putchar(108);
    putchar(108);
    putchar(111);
    putchar(44);
    putchar(32);
    putchar(87);
    putchar(111);
    putchar(114);
    putchar(108);
    putchar(100);
    putchar(33);
    putchar(10);

    return 0;
}
```

```bash
# Generate assembly
$ dune exec ccaml source.c
# Execute a program with GCC assembler
$ dune exec ccaml source.c | gcc -xassembler - && ./a.out
# Run test
$ dune test
```

## References

1. [Writing a C Compiler, Part 1](https://norasandler.com/2017/11/29/Write-a-Compiler.html)
1. [Writing a C Compiler, Part 2](https://norasandler.com/2017/12/05/Write-a-Compiler-2.html)
1. [Writing a C Compiler, Part 3](https://norasandler.com/2017/12/15/Write-a-Compiler-3.html)
1. [Writing a C Compiler, Part 4](https://norasandler.com/2017/12/28/Write-a-Compiler-4.html)
1. [Writing a C Compiler, Part 5](https://norasandler.com/2018/01/08/Write-a-Compiler-5.html)
1. [Writing a C Compiler, Part 6](https://norasandler.com/2018/02/25/Write-a-Compiler-6.html)
1. [Writing a C Compiler, Part 7](https://norasandler.com/2018/03/14/Write-a-Compiler-7.html)
1. [C Compiler, Part 8: Loops](https://norasandler.com/2018/04/10/Write-a-Compiler-8.html)
1. [C Compiler, Part 9: Functions](https://norasandler.com/2018/06/27/Write-a-Compiler-9.html)
1. [nlsandler/nqcc: A compiler for a tiny (but growing!) subset of C, written in OCaml.](https://github.com/nlsandler/nqcc)
1. [低レイヤを知りたい人のための C コンパイラ作成入門](https://www.sigbus.info/compilerbook)
1. [x86 アセンブリ言語での関数コール](https://vanya.jp.net/os/x86call/)
1. [AT&T assembly syntax and IA-32 instructions](https://gist.github.com/mishurov/6bcf04df329973c15044)
1. [Online GCC Assembler - online editor](https://www.onlinegdb.com/online_gcc_assembler)
