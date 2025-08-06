# RUN: llvm-mc -filetype=obj -triple nanomips-elf -mattr=+dsp < %s \
# RUN:     | llvm-objdump -d - | FileCheck %s

# CHECK:{{.*}}move $a0, $a0
	balign	$a0, $a1, 0
# CHECK:{{.*}}extw $a0, $a1,{{.*}}0x18
	balign	$a0, $a1, 1
# CHECK:{{.*}}packrl.ph $a0, $a0, $a1
	balign	$a0, $a1, 2
# CHECK:{{.*}}extw $a0, $a1,{{.*}}0x8
	balign	$a0, $a1, 3
# CHECK:{{.*}}move $a0, $a0
        prepend $a0, $a1, 0
# CHECK:{{.*}}extw $a0, $a0, $a1,{{.*}}0x1
        prepend $a0, $a1, 1
# CHECK:{{.*}}extw $a0, $a0, $a1,{{.*}}0x2
        prepend $a0, $a1, 2
# CHECK:{{.*}}extw $a0, $a0, $a1,{{.*}}0x3
        prepend $a0, $a1, 3
# CHECK:{{.*}}extw $a0, $a0, $a1,{{.*}}0x1f
        prepend $a0, $a1, 31
