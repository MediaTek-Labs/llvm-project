#include <cstdlib>
#include <iostream>
#include <cassert>
#include <cstring>
#include <clocale>
#include <cwchar>
#include <cinttypes>

void atexit_handler_1()
{
    std::cout << "At exit #1\n";
}

void atquickexit_handler_1()
{
    std::cout << "At quick exit #1\n";
}

int compare(const void *a, const void *b) {
    int int_a = *static_cast<const int*>(a);
    int int_b = *static_cast<const int*>(b);
    
    if (int_a == int_b) return 0;
    else if (int_a < int_b) return -1;
    else return 1;
}

int main(){

    // Types

    div_t div_var;
    ldiv_t ldiv_var;
    lldiv_t lldiv_var;
    size_t size_var;

    // Macros

    std::cout << "EXIT_SUCCESS: " << EXIT_SUCCESS << std::endl;
    std::cout << "EXIT_FAILURE: " << EXIT_FAILURE << std::endl;
    std::cout << "MB_CUR_MAX: " << MB_CUR_MAX << std::endl;
    std::cout << "NULL: " << NULL << std::endl;
    std::cout << "RAND_MAX: " << RAND_MAX << std::endl;

    // Process control

    std::system("ls -l > std_system_test.txt");

    // Memory management

    constexpr std::size_t size = 256;
    void* ptr = std::malloc(size);
    assert(ptr != NULL);
    std::free(ptr);

    // error: ‘aligned_alloc’ is not a member of ‘std’
    void* p222 = std::aligned_alloc(1024, 1024);
    std::free(p222);

    int* p1 = (int*)std::calloc(4, sizeof(int));
    assert(p1 != NULL);
    std::free(p1);

    int* p2 = (int*)std::malloc(size);
    assert(p2 != NULL);
    p2 = (int*)std::realloc(p2, size*2);
    assert(p2 != NULL);
    std::free(p2);

    // Numeric string conversion

    std::atof("0.0000000123");
    std::atoi("10");
    std::atol("10");
    std::atoll("10");
    
    char str[] = "12345abcd";
    char *endptr;
    long num;

    num = std::strtol(str, &endptr, 10);
    if (endptr == str) {
        std::cout << "No digits were found." << std::endl;
    } else if (*endptr != '\0') {
        std::cout << "Conversion stopped at character " << *endptr << std::endl;
        std::cout << "The number is: " << num << std::endl;
    } else {
        std::cout << "The number is: " << num << std::endl;
    }

    long long num2 = std::strtoll(str, &endptr, 10);
    unsigned long num3 = std::strtoul(str, &endptr, 10);
    unsigned long long num4 = std::strtoull(str, &endptr, 10);

    float num5 = std::strtof(str, &endptr);
    double num6 = std::strtod(str, &endptr);
    long double num7 = std::strtold(str, &endptr);

    // Wide string manipulation

    std::setlocale(LC_ALL, "");

    const char* mb_string = "こんにちは";
    std::mblen(nullptr, 0); // reset the conversion state
    int bytes = std::mblen(&mb_string[9], MB_CUR_MAX);
    assert(bytes != -1);

    wchar_t wideChar1;
    std::mbtowc(nullptr, 0, 0); // reset the conversion state
    int numConverted1 = std::mbtowc(&wideChar1, mb_string, MB_CUR_MAX);
    assert(numConverted1 != -1);

    wchar_t wideChar2 = 'B';
    char multibyteChar[MB_CUR_MAX] = {0};
    int numConverted2 = std::wctomb(multibyteChar, wideChar2);
    assert(numConverted2 != -1);

    size_t length = std::strlen(mb_string) + 1;
    wchar_t wideString[length];
    std::mbstowcs(wideString, mb_string, length);
    std::cout << "Wide character string: " << wideString << std::endl;

    const wchar_t* wideString1 = L"こんにちは";
    size_t length1 = std::wcslen(wideString1) + 1;
    char multibyteString[length * MB_CUR_MAX];
    std::wcstombs(multibyteString, wideString1, length1 * MB_CUR_MAX);
    std::cout << "Multibyte string: " << multibyteString << std::endl;


    // Miscellaneous algorithms and math

    std::srand(5);
    std::cout << "Random number: " << std::rand() << std::endl;

    int arr[] = {5, 2, 9, 1, 7};
    size_t num_elements = sizeof(arr) / sizeof(arr[0]);
    std::qsort(arr, num_elements, sizeof(arr[0]), compare);
    std::cout << "Sorted array: ";
    for (size_t i = 0; i < num_elements; ++i) {
        std::cout << arr[i] << " ";
    }
    std::cout << std::endl;

    const size_t num_elements1 = 10;
    int arr1[] = {2, 8, 7, 1, 6, 4, 3, 5, 9, 0};
    int key = 5;
    std::qsort(arr1, num_elements1, sizeof(arr1[0]), compare);
    int* result = static_cast<int*>(std::bsearch(&key, arr1, num_elements1, sizeof(int), compare));
    if (result != nullptr) {
        std::cout << "Value " << key << " found at index: " << (result - arr1) << std::endl;
    } else {
        std::cout << "Value " << key << " not found." << std::endl;
    }

    std::abs((int)-22);
    std::abs((long)-22);
    std::abs((long long)-22);
    std::labs((long)-22);
    std::llabs((long long)-22);
    
    std::intmax_t abs_num = -22;
    std::abs(abs_num);
    // error: ‘imaxabs’ is not a member of ‘std’
    std::imaxabs(abs_num);

    std::div((int)20,(int)3);
    std::div((long)20,(long)3);
    std::div((long long)20,(long long)3);
    std::ldiv((long)20,(long)3);
    std::lldiv((long long)20,(long long)3);
    std::intmax_t div_num1 = 20;
    std::intmax_t div_num2 = 3;
    std::div(div_num1, div_num2);
    //error: ‘imaxdiv’ is not a member of ‘std’
    std::imaxdiv(div_num1, div_num2);

    return 0;
}
