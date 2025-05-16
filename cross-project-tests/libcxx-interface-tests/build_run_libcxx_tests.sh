#!/bin/bash

SRC_DIR=`dirname $0`

if [ $# -lt 1 ]; then
	echo "Usage: ./build_run_libcxx_tests <path_to_elf_toolchain>"
	exit 1
fi

toolchain=$1;

# Make libstubs object file
$toolchain/bin/nanomips-elf-clang -c $SRC_DIR/../../libcxx/test/support/libstubs.c -o libstubs.o -march=i7200 -Tuhi32.ld -lm  -Wl,--defsym,__memory_size=256M
if [ $? -ne 0 ]; then
	echo -e ">>>>>>>>>>>>>>>>>>>> FAILED: libstubs <<<<<<<<<<<<<<<<<<<<\n"
	return
fi
$toolchain/bin/nanomips-elf-clang++ -c $SRC_DIR/../../libcxx/test/support/libcxx_stubs.cpp -o libcxx_stubs.o -march=i7200 -Tuhi32.ld -lm  -Wl,--defsym,__memory_size=256M
if [ $? -ne 0 ]; then
	echo -e ">>>>>>>>>>>>>>>>>>>> FAILED: libstubs.cpp <<<<<<<<<<<<<<<<<<<<\n"
	return
fi
$toolchain/bin/nanomips-elf-ar cru libstubs.a libstubs.o libcxx_stubs.o


tests=(algorithm array chrono climits cstdbool ctype fenv inttypes limits map
       math memory queue random set stdarg stdbool stdint stdio stdlib
       stdlib_exit_1 stdlib_exit_2 stdlib_exit_3 stdlib_exit_4
       stdlib_exit_5 stdlib_exit_6 stdlib_exit_7
       string time type_traits unordered_map vector)

for test in ${tests[@]}; do 

	echo ">>>>>>>>>>>>>>>>>>>> $test <<<<<<<<<<<<<<<<<<<<"

	# Compile test
	$toolchain/bin/nanomips-elf-clang++ $SRC_DIR/$test.cpp -o $test.elf -march=i7200 -Tuhi32.ld -Wl,--whole-archive -lstubs -Wl,--no-whole-archive -lm -Wl,--defsym,__memory_size=256M

	if [ $? -ne 0 ]; then
		echo -e ">>>>>>>>>>>>>>>>>>>> FAILED: $test <<<<<<<<<<<<<<<<<<<<\n"
		continue
	fi

	if [[ $test == "stdio" ]]; then
		touch file_to_be_removed_by_std::remove.txt
		touch file_to_be_renamed_by_std::rename.txt
	fi

	# Run test
	$toolchain/bin/qemu-system-nanomips -cpu I7200 -m 256 -semihosting -nographic -kernel $test.elf &> $test.out

	if [ $? -ne 0 ]; then
		if [[ $test == "stdlib_exit_1" || $test == "stdlib_exit_3" || $test == "stdlib_exit_5" ]]; then
		# The exit code of these tests is supposed to be non-zero
			echo -e ">>>>>>>>>>>>>>>>>>>> PASSED: $test <<<<<<<<<<<<<<<<<<<<\n"
		else
			echo -e ">>>>>>>>>>>>>>>>>>>> FAILED: $test <<<<<<<<<<<<<<<<<<<<\n"
		fi
	else
		echo -e ">>>>>>>>>>>>>>>>>>>> PASSED: $test <<<<<<<<<<<<<<<<<<<<\n"
	fi
done
