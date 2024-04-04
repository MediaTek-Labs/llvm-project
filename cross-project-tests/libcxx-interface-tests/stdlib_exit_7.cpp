#include <cstdlib>
#include <iostream>

void atquickexit_handler()
{
    std::cout << "At quick exit\n";
}

int main(){
    std::at_quick_exit(atquickexit_handler);
    std::quick_exit(EXIT_SUCCESS);
}
