## Compilation

```bash
gfortran doskliq.f -o doskliq
```

## Usage

The program can be executed without arguments and will ask for user input.
Alternatively, the paths to the input files can be piped in like this:

```bash
{ echo "\"input.cmd\""; echo "\"input.list\"" } | ./doskliq
```
