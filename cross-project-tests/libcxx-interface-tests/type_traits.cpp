#include <type_traits>
#include <iostream>
#include <vector>
#include <cassert>

template <typename T>
void print_type_info() {

    std::cout << "------------------------------------" << std::endl;
    std::cout << "Type: " << typeid(T).name() << std::endl << std::endl;

    // Primary type categories
    std::cout << "Is array: " << std::is_array<T>::value << std::endl;
    std::cout << "Is enum: " << std::is_enum<T>::value << std::endl;
    std::cout << "Is class: " << std::is_class<T>::value << std::endl;
    std::cout << "Is floating point: " << std::is_floating_point<T>::value << std::endl;
    std::cout << "Is integral: " << std::is_integral<T>::value << std::endl;
    std::cout << "Is function: " << std::is_function<T>::value << std::endl;
    std::cout << "Is void: " << std::is_void<T>::value << std::endl;
    std::cout << "Is pointer: " << std::is_pointer<T>::value << std::endl;
    std::cout << "Is null pointer: " << std::is_null_pointer<T>::value << std::endl;
    std::cout << "Is pointer to function: " << std::is_function<typename std::remove_pointer<T>::type>::value << std::endl;

    // Composite type categories
    std::cout << "Is reference: " << std::is_reference<T>::value << std::endl;
    std::cout << "Is arithmetic: " << std::is_arithmetic<T>::value << std::endl;
    std::cout << "Is fundamental: " << std::is_fundamental<T>::value << std::endl;
    std::cout << "Is scalar: " << std::is_scalar<T>::value << std::endl;
    std::cout << "Is object: " << std::is_object<T>::value << std::endl;

    // Type properties
    std::cout << "Is const: " << std::is_const<T>::value << std::endl;
    std::cout << "Is trivial: " << std::is_trivial<T>::value << std::endl;
    std::cout << "Is volatile: " << std::is_volatile<T>::value << std::endl;
    std::cout << "Is trivially copyable: " << std::is_trivially_copyable<T>::value << std::endl;
    std::cout << "Is abstract: " << std::is_abstract<T>::value << std::endl;
    std::cout << "Is signed: " << std::is_signed<T>::value << std::endl;
    std::cout << "Is unsigned: " << std::is_unsigned<T>::value << std::endl;

    // Supported operations
    std::cout << "Is constructible: " << std::is_constructible<T>::value << std::endl;
    std::cout << "Is default constructible: " << std::is_default_constructible<T>::value << std::endl;
    std::cout << "Is copy constructible: " << std::is_copy_constructible<T>::value << std::endl;
    std::cout << "Is move constructible: " << std::is_move_constructible<T>::value << std::endl;
    std::cout << "Is copy assignable: " << std::is_copy_assignable<T>::value << std::endl;
    std::cout << "Is move assignable: " << std::is_move_assignable<T>::value << std::endl;
    std::cout << "Is destructible: " << std::is_destructible<T>::value << std::endl;
    std::cout << "Has virtual destructor: " << std::has_virtual_destructor<T>::value << std::endl;

    std::cout << "------------------------------------" << std::endl << std::endl;
}

void test_type_info(){
    // Print type information for various types
    print_type_info<int>();
    print_type_info<float>();
    print_type_info<char>();
    print_type_info<double>();
    print_type_info<void>();
    print_type_info<int*>();
    print_type_info<const int*>();
    print_type_info<int&>();
    print_type_info<int&&>();
    print_type_info<std::string>();
    print_type_info<std::vector<int>>();
    print_type_info<int[5]>();
}

void test_is_same(){

    // Example of using std::is_same
    std::cout << "is_same<int, std::int32_t> (maybe true) : " << std::is_same<int, std::int32_t>::value << std::endl;
    std::cout << "is_same<int, std::int64_t> (maybe true): " << std::is_same<int, std::int64_t>::value << std::endl;
    long double num1 = 1.0;
    long double num2 = 2.0;
    static_assert( std::is_same<decltype(num1), decltype(num2)>::value == true );
    // 'float' is never an integral type
    static_assert( std::is_same<float, std::int32_t>::value == false );
    // 'int' is implicitly 'signed'
    static_assert( std::is_same<int, int>::value == true );
    static_assert( std::is_same<int, unsigned int>::value == false );
    static_assert( std::is_same<int, signed int>::value == true );
    // 'char' is neither 'unsigned' nor 'signed'
    static_assert( std::is_same<char, char>::value == true );
    static_assert( std::is_same<char, unsigned char>::value == false );
    static_assert( std::is_same<char, signed char>::value == false );
}

void test_is_base(){
    class A {};
    class B : A {};
    class C : B {};
    class D {};
    union E {};
    using I = int;
    
    static_assert
    (
        std::is_base_of<A, A>::value == true &&
        std::is_base_of<A, B>::value == true &&
        std::is_base_of<A, C>::value == true &&
        std::is_base_of<A, D>::value != true &&
        std::is_base_of<B, A>::value != true &&
        std::is_base_of<E, E>::value != true &&
        std::is_base_of<I, I>::value != true
    );
}

void test_is_convertible(){
    class A {};
    class B : public A {};
    class C {};
    class D { public: operator C() { return c; } C c; };

    static_assert
    (
        std::is_convertible<B*, A*>::value == true &&
        std::is_convertible<A*, B*>::value  == false &&
        std::is_convertible<D, C>::value == true &&
        std::is_convertible<B*, C*>::value == false
    );
}

template<typename U, typename V>
constexpr bool same = std::is_same<U, V>::value;

void test_remove_cv(){
 
    static_assert
    (
        same<std::remove_cv_t<int>, int> &&
        same<std::remove_cv_t<const int>, int> &&
        same<std::remove_cv_t<volatile int>, int> &&
        same<std::remove_cv_t<const volatile int>, int> &&
        // remove_cv only works on types, not on pointers
        not same<std::remove_cv_t<const volatile int*>, int*> &&
        same<std::remove_cv_t<const volatile int*>, const volatile int*> &&
        same<std::remove_cv_t<const int* volatile>, const int*> &&
        same<std::remove_cv_t<int* const volatile>, int*>
    );
}

void test_add_cv(){

    struct foo
    {
        std::string m() { return "Non-cv"; }
        std::string m() const { return "Const"; }
        std::string m() volatile { return "Volatile"; }
        std::string m() const volatile {return "Const-volatile"; }
    };

    assert
    (
        foo{}.m() == "Non-cv" &&
        std::add_const<foo>::type{}.m() == "Const" &&
        std::add_volatile<foo>::type{}.m() == "Volatile" &&
        std::add_cv<foo>::type{}.m() == "Const-volatile"
    );
}

void test_remove_reference(){
    static_assert
    (
        same<int, std::remove_reference<int>::type> &&
        same<int, std::remove_reference<int&>::type> &&
        same<int, std::remove_reference<int&&>::type> &&
        same<const int, std::remove_reference<const int&>::type>
    );
}

void test_add_reference(){
    using non_ref = int;
    using l_ref = typename std::add_lvalue_reference_t<non_ref>;
    using r_ref = typename std::add_rvalue_reference_t<non_ref>;
    using void_ref = std::add_lvalue_reference_t<void>;
 
    static_assert
    (  
        std::is_lvalue_reference<non_ref>::value == false &&
        std::is_lvalue_reference<l_ref>::value == true &&
        std::is_rvalue_reference<r_ref>::value == true &&
        std::is_reference<void_ref>::value == false
    );
}

void test_remove_pointer(){
    static_assert
    (
        same<int, int> == true &&
        same<int, int*> == false &&
        same<int, int**> == false &&
        same<int, std::remove_pointer_t<int>> == true &&
        same<int, std::remove_pointer_t<int*>> == true &&
        same<int, std::remove_pointer_t<int**>> == false &&
        same<int, std::remove_pointer_t<int* const>> == true &&
        same<int, std::remove_pointer_t<int* volatile>> == true &&
        same<int, std::remove_pointer_t<int* const volatile>> == true
    );
}

void test_add_pointer(){
    int i = 123;
    int& ri = i;
    typedef std::add_pointer<decltype(i)>::type IntPtr1;
    typedef std::add_pointer<decltype(ri)>::type IntPtr2;
    static_assert
    (
        std::is_pointer<IntPtr1>::value &&
        same<IntPtr1, int*> &&
        same<IntPtr2, IntPtr1>
    );
}

void test_make_signed(){
    static_assert
    (
        same<std::make_signed_t<unsigned char>, signed char> &&
        same<std::make_signed_t<unsigned int>, signed int> &&
        same<std::make_signed_t<volatile unsigned long>, volatile signed long>
    );
}

void test_make_unsigned(){
    static_assert
    (
        same<std::make_unsigned_t<char>, unsigned char> &&
        same<std::make_unsigned_t<int>, unsigned int> &&
        same<std::make_unsigned_t<volatile long>, volatile unsigned long>
    );
}

void test_decay(){
    static_assert
    (
        same<std::decay_t<int>, int> &&
        !same<std::decay_t<int>, float> &&
        same<std::decay_t<int&>, int> &&
        same<std::decay_t<int&&>, int> &&
        same<std::decay_t<const int&>, int> &&
        same<std::decay_t<int[2]>, int*> &&
        !same<std::decay_t<int[4][2]>, int*> &&
        !same<std::decay_t<int[4][2]>, int**> &&
        same<std::decay_t<int[4][2]>, int(*)[2]> &&
        same<std::decay_t<int(int)>, int(*)(int)>
    );
}

template <typename T>
typename std::enable_if<std::is_integral<T>::value, T>::type
foo(T number) {
    int ret_val = 5;
    return ret_val;
}

template <typename T>
typename std::enable_if<std::is_floating_point<T>::value, T>::type
foo(T number) {
    float ret_val = 0.5;
    return ret_val;
}

void test_enable_if(){
    static_assert
    (
        same<decltype(foo(3)), int> &&
        same<decltype(foo(0.5)), double>
    );
}

// Example of using std::conditional
template <typename T>
typename std::conditional<std::is_integral<T>::value, int, double>::type
conditional_type(T x) {
    return x;
}

void test_conditional(){

    // Integral types
    int a = 5;
    long long b = 10;
    short c = 15;
    char d = 'a';

    // Non-integral types
    double e = 0.5;
    float f = 1.5;
    long double g = 2.5;
    std::string h = "abc";
    std::vector<int> i = {1, 2, 3, 4};

    static_assert
    (
        same<decltype(conditional_type(a)), int> &&
        same<decltype(conditional_type(b)), int> &&
        same<decltype(conditional_type(c)), int> &&
        same<decltype(conditional_type(c)), int> &&
        same<decltype(conditional_type(e)), double> &&
        same<decltype(conditional_type(f)), double> &&
        same<decltype(conditional_type(g)), double> &&
        same<decltype(conditional_type(h)), double> &&
        same<decltype(conditional_type(i)), double>
    );
}

int main() {

    // Queries over different types
    test_type_info();

    // Type relationships
    test_is_same();
    test_is_base();
    test_is_convertible();

    // Const-volatility specifiers
    test_remove_cv();
    test_add_cv();

    // References
    test_remove_reference();
    test_add_reference();

    // Pointers
    test_remove_pointer();
    test_add_pointer();

    // Sign modifiers
    test_make_signed();
    test_make_unsigned();

    // Miscellaneous transformations
    test_decay();
    test_enable_if();
    test_conditional();

    return 0;
}
