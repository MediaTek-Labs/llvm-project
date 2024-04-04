#include <stdbool.h>
#include <iostream>
#include <cassert>

int main(){

    assert((__bool_true_false_are_defined == 1) && "__bool_true_false_are_defined should be defined to 1.");
    std::cout << "__bool_true_false_are_defined: " << __bool_true_false_are_defined << std::endl;
    return 0;
}
