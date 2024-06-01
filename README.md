# Compiler Tutorial in C

A really simple programming language meant to be used as a tutorial for compilers (really easy ones).

```text
term = <input> | variable | literal
expression = term | term + term | ...
rel = term < term | ...
instr = variable = expression | <if> rel <then> instr | <goto> :label | <output> term | :label
```

Example of a program in this language

```text
n = input
i = 0
:label
output i
i = i + 1
if i < n then goto :label
if i < 10 then goto :label2
output 69
:label2
```

## Quickstart

```console
gcc -g main.c -o main
cat example.txt | ./main > example.asm
fasm example.asm
./example
```
