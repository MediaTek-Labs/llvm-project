# REQUIRES: x86

## New linker script directive, STARTUP, takes a file an adds it to the files
## that should be parsed list (like INPUT). It also specifies that that file
## (object or bitcode) is treated first, as if it were specified as the first
## file on the command line. This doesn't work for archives.

# RUN: llvm-mc -filetype=obj -triple=x86_64 %s -o %t1.o
# RUN: echo '.globl b; b:' | llvm-mc -filetype=obj -triple=x86_64 - -o %t2.o

# RUN: echo 'STARTUP(%t1.o)' > %t.script
# RUN: ld.lld %t2.o -T %t.script -o %t
# RUN: llvm-objdump -t %t | FileCheck %s

# RUN: echo 'STARTUP(%t2.o)' >> %t.script

# RUN: not ld.lld -T %t.script -o /dev/null 2>&1 | FileCheck %s \
# RUN: --check-prefix=MULTIPLE-STARTUPS

# RUN: llvm-ar rc %t2.a %t2.o
# RUN: echo 'STARTUP(%t2.a)' > %t.script

# RUN: not ld.lld -T %t.script -o /dev/null %t1.o 2>&1 | FileCheck %s \
# RUN: --check-prefix=ARCHIVE-ERROR

# CHECK: _start
# CHECK: b

# MULTIPLE-STARTUPS: multiple STARTUP directives seen

# ARCHIVE-ERROR: Adding archive files with STARTUP directive not supported

.globl _start
_start:
  call b