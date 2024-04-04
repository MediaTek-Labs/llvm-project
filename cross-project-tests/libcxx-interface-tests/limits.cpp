#include <limits>
#include <iostream>


template <typename T>
void print_limits(const std::string& typeName) {

    std::cout << "Limits for type " << typeName << ":" << std::endl;
    std::cout << "------------------------------------" << std::endl;
    std::cout << "Minimum value: " << std::numeric_limits<T>::min() << std::endl;
    std::cout << "Maximum value: " << std::numeric_limits<T>::max() << std::endl;
    std::cout << "Is signed: " << std::numeric_limits<T>::is_signed << std::endl;
    std::cout << "Is integer: " << std::numeric_limits<T>::is_integer << std::endl;
    std::cout << "Digits: " << std::numeric_limits<T>::digits << std::endl;
    std::cout << "Digits10: " << std::numeric_limits<T>::digits10 << std::endl;
    std::cout << "Radix: " << std::numeric_limits<T>::radix << std::endl;
    std::cout << "Epsilon: " << std::numeric_limits<T>::epsilon() << std::endl;
    std::cout << "Round error: " << std::numeric_limits<T>::round_error() << std::endl;
    std::cout << "Infinity: " << std::numeric_limits<T>::infinity() << std::endl;
    std::cout << "Quiet NaN: " << std::numeric_limits<T>::quiet_NaN() << std::endl;
    std::cout << "Signaling NaN: " << std::numeric_limits<T>::signaling_NaN() << std::endl;
    std::cout << "Has infinity: " << std::numeric_limits<T>::has_infinity << std::endl;
    std::cout << "Has NaN: " << std::numeric_limits<T>::has_quiet_NaN << std::endl;
    std::cout << "------------------------------------" << std::endl << std::endl;
}

int main(){ 
    
    print_limits<char>("char");
    print_limits<unsigned char>("unsigned char");
    print_limits<int>("int");
    print_limits<unsigned int>("unsigned int");
    print_limits<long>("long");
    print_limits<long long>("long long");
    print_limits<short>("short");
    print_limits<float>("float");
    print_limits<double>("double");
    print_limits<long double>("long double");
    print_limits<bool>("bool");
    
    return 0;
}
