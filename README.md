# smol
a smol lang

to compile the example:
```
python compiler.py exampleProgram.txt
```

to run a program you compile:
```
./program
```
or
```
.\program
```
if you're on Windows.

# syntax

(in smol pseudocode, <x> is the address of x)
all smol programs use these commands

```
let <x> y
set <x> <y>
add <x> <y> <z>
eq? <x> <y> <z>
out <x>
```
where anything after the first word on a line must be a non-negative integer

# details

addresses go from 0 to 255
address 0 holds the current operation number
