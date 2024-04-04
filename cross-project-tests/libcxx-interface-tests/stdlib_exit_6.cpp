#include <cstdlib>
#include <iostream>

void atexit_handler()
{
    std::cout << "At exit\n";
}

int main(){
    std::atexit(atexit_handler);
    std::exit(EXIT_SUCCESS);
}
