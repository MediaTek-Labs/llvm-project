#!/bin/bash

if [ $# -lt 1 ]; then
	echo "Usage: ./build_run_libcxx_tests <path_to_elf_toolchain>"
	exit 1
fi

build_run_gettimeofday_dependent_test() {

        local test_name="$1"

        # Make test object file
        $toolchain/bin/nanomips-elf-g++ -c $test_name.cpp -o $test_name.o -march=i7200 -Tuhi32.ld -fno-exceptions -lm -Wl,--defsym,__memory_size=256M
        if [ $? -ne 0 ]; then
                echo -e ">>>>>>>>>>>>>>>>>>>> FAILED: $test_name <<<<<<<<<<<<<<<<<<<<\n"
                return
        fi

        # Make libstubs object file
        $toolchain/bin/nanomips-elf-gcc -c ../../libcxx/libstubs.c -o libstubs.o -march=i7200 -Tuhi32.ld -fno-exceptions -lm  -Wl,--defsym,__memory_size=256M
        if [ $? -ne 0 ]; then
                echo -e ">>>>>>>>>>>>>>>>>>>> FAILED: libstubs <<<<<<<<<<<<<<<<<<<<\n"
                return
        fi

        # Link test.o and libstubs.o
        $toolchain/bin/nanomips-elf-g++ $test_name.o libstubs.o -o $test_name.elf -march=i7200 -Tuhi32.ld -fno-exceptions -lm  -Wl,--defsym,__memory_size=256M
        if [ $? -ne 0 ]; then
                echo -e ">>>>>>>>>>>>>>>>>>>> FAILED: $test_name <<<<<<<<<<<<<<<<<<<<\n"
                return
        fi

        # Run linked test
        $toolchain/bin/qemu-system-nanomips -cpu I7200 -m 256 -semihosting -nographic -kernel $test_name.elf
        if [ $? -ne 0 ]; then
                echo -e ">>>>>>>>>>>>>>>>>>>> FAILED: $test_name <<<<<<<<<<<<<<<<<<<<\n"
                return
        fi

	echo -e ">>>>>>>>>>>>>>>>>>>> PASSED: $test_name <<<<<<<<<<<<<<<<<<<<\n"
}

toolchain=$1;

tests=(algorithm array chrono climits cstdbool ctype fenv inttypes limits map
       math memory queue random set stdarg stdbool stdint stdio stdlib
       stdlib_exit_1 stdlib_exit_2 stdlib_exit_3 stdlib_exit_4
       stdlib_exit_5 stdlib_exit_6 stdlib_exit_7
       string time type_traits unordered_map vector)

for test in ${tests[@]}; do 

	echo ">>>>>>>>>>>>>>>>>>>> $test <<<<<<<<<<<<<<<<<<<<"

	if [[ $test == "chrono" || $test == "time" ]]; then
		build_run_gettimeofday_dependent_test $test
	else
		# Compile test
		$toolchain/bin/nanomips-elf-g++ $test.cpp -o $test.elf -march=i7200 -Tuhi32.ld -fno-exceptions -lm -Wl,--defsym,__memory_size=256M

		if [ $? -ne 0 ]; then
			echo -e ">>>>>>>>>>>>>>>>>>>> FAILED: $test <<<<<<<<<<<<<<<<<<<<\n"
		else
			if [[ $test == "stdio" ]]; then
				touch file_to_be_removed_by_std::remove.txt
				touch file_to_be_renamed_by_std::rename.txt
			fi

			# Run test
			$toolchain/bin/qemu-system-nanomips -cpu I7200 -m 256 -semihosting -nographic -kernel $test.elf

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
		fi
	fi
done


