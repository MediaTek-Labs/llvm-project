#include <climits>
#include <iostream>
#include <cassert>

int main(){

    std::cout << "CHAR_BIT: " << CHAR_BIT << std::endl;
    std::cout << "SCHAR_MIN: " << SCHAR_MIN << std::endl;
    std::cout << "SCHAR_MAX: " << SCHAR_MAX << std::endl;
    std::cout << "UCHAR_MAX: " << UCHAR_MAX << std::endl;
    std::cout << "CHAR_MIN: " << CHAR_MIN << std::endl;
    std::cout << "CHAR_MAX: " << CHAR_MAX << std::endl;
    std::cout << "MB_LEN_MAX: " << MB_LEN_MAX << std::endl;
    std::cout << "SHRT_MIN: " << SHRT_MIN << std::endl;
    std::cout << "SHRT_MAX: " << SHRT_MAX << std::endl;
    std::cout << "USHRT_MAX: " << USHRT_MAX << std::endl;
    std::cout << "INT_MIN: " << INT_MIN << std::endl;
    std::cout << "INT_MAX: " << INT_MAX << std::endl;
    std::cout << "UINT_MAX: " << UINT_MAX << std::endl;
    std::cout << "LONG_MIN: " << LONG_MIN << std::endl;
    std::cout << "LONG_MAX: " << LONG_MAX << std::endl;
    std::cout << "ULONG_MAX: " << ULONG_MAX << std::endl;
    
    // These macros might not be defined!
    std::cout << "LLONG_MIN: "   << LLONG_MIN << std::endl;
    std::cout << "LLONG_MAX: "   << LLONG_MAX << std::endl;
    std::cout << "ULLONG_MAX: "  << ULLONG_MAX << std::endl;
    
    return 0;
}
