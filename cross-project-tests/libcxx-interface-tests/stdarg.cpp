#include <cstdarg>
#include <iostream>
#include <cassert>

int add_nums(int count...){
    int result = 0;
    std::va_list args;
    va_start(args, count);
    for (int i = 0; i < count; ++i)
        result += va_arg(args, int);
    va_end(args);
    return result;
}

int check_va_copy(int count...){
    std::va_list args;
    va_start(args, count);
    std::va_list args_copy;
    va_copy(args_copy ,args);           

    int are_the_same = 1;
    for (int i = 0; i < count; ++i){
        int arg = va_arg(args, int);
        int arg_copy = va_arg(args_copy, int);
        if(arg != arg_copy)
            are_the_same = 0;
    }
    va_end(args);
    va_end(args_copy);
    return are_the_same;
}

int main(){

    assert((add_nums(4, 25, 25, 50, 50) == 150) && "Summ of given numbers is not correctly calculated. Variadic function arguments are not correctly read.");    
    assert(check_va_copy(5, 1, 2, 3, 4, 5) && "va_copy failed.");

    return 0;
}
