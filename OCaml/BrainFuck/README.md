# Dependencies
* OCaml (I used 4.08.1)
* OCaml packages: extlib, oUnit, unix
* nasm (I used 2.14.02)
* clang (I used Apple clang version 11.0.0 (clang-1100.0.33.8), Target: x86_64-apple-darwin19.2.0)
# Usage
To compile and run a Brainfuck program, put it in the `input/` directory with the extension `.bf` and run `bash compile.sh file_name`. For example, `bash compile.sh hello_world` compiles and runs `input/hello_world.bf`. Its assembly and associated binaries will be stored in the `output/` directory sharing the same file name.  

Currently, I have only validated that the compiler works on x86 64-bit on OSX. It might work on x86 64-bit Linux, but it won't work on any 32-bit architectures or any windows system.