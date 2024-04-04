#include <memory>
#include <iostream>
#include <cassert>
#include <vector>

class MyClass {
public:
    MyClass(int val) : value(val) {
        std::cout << "Constructor called with value: " << value << std::endl;
    }

    ~MyClass() {
        std::cout << "Destructor called with value: " << value << std::endl;
    }

    void display() {
        std::cout << "Value: " << value << std::endl;
    }

private:
    int value;
};

int test_unique_ptr(){

    std::unique_ptr<MyClass> myClassPtr = std::make_unique<MyClass>(10);

    if (myClassPtr) {
        myClassPtr->display();
    }

    // The unique_ptr will automatically release the memory when it goes out of scope
}

void test_basic_allocators(){


    // 1. Allocator for int
    std::allocator<int> alloc1;
    assert((std::is_same<int, decltype(alloc1)::value_type>::value && "Type check fail."));
    int* p1 = alloc1.allocate(1); // Space for one int
    alloc1.deallocate(p1, 1);     // Space is freed

    // 2. Allocator traits
    using traits_t1 = std::allocator_traits<decltype(alloc1)>; // The matching trait
    int* p2 = traits_t1::allocate(alloc1, 1);
    traits_t1::construct(alloc1, p2, 7);  // Construct the int
    std::cout << "Allocated int value is: " << *p2 << std::endl;
    traits_t1::deallocate(alloc1, p2, 1); // Deallocate space for one int

    // 3. Default allocator for strings
    std::allocator<std::string> alloc2;
    // Matching traits
    using traits_t2 = std::allocator_traits<decltype(alloc2)>;

    // Rebinding the allocator using the trait for strings gets the same type
    traits_t2::rebind_alloc<std::string> alloc_ = alloc2;

    std::string* p3 = traits_t2::allocate(alloc2, 2); // Space for 2 strings

    traits_t2::construct(alloc2, p3, "foo");
    traits_t2::construct(alloc2, p3 + 1, "bar");
    std::cout << "Allocated strins are: " << p3[0] << ' ' << p3[1] << std::endl;
}

// Custom deleter function
void customDeleter(MyClass* obj) {
    std::cout << "Custom deleter called for MyClass with value: ";
    obj->display();
    delete obj;
}

void test_memory_management_on_my_class(){

    // 1. Using std::unique_ptr
    std::unique_ptr<MyClass> uniquePtr = std::make_unique<MyClass>(10);
    uniquePtr->display(); // Accessing member function

    // 2. Transferring ownership to another std::unique_ptr
    std::unique_ptr<MyClass> transferredPtr = std::move(uniquePtr);
    std::cout << "Display value after ownership transfer: ";
    transferredPtr->display();

    // 3. Using std::shared_ptr with a custom deleter
    // note: transferredPtr.release(): 
    //       returns a pointer to the managed object and releases the ownership
    std::shared_ptr<MyClass> sharedPtrWithDeleter(transferredPtr.release(), customDeleter);

    //Error! We release ownership from transferredPtr!
    //std::cout << "This should cause an error!" << std::endl;
    //transferredPtr->display();

    // 4. Using std::weak_ptr
    std::weak_ptr<MyClass> weakPtr = sharedPtrWithDeleter;

    // 5. Creating std::shared_ptr from std::weak_ptr
    if (auto sharedPtrFromWeak = weakPtr.lock()) {
        std::cout << "Shared pointer from weak pointer: ";
        sharedPtrFromWeak->display();
    } else {
        std::cout << "Weak pointer expired!" << std::endl;
    }

    // 6. Using std::make_shared
    auto sharedPtrMake = std::make_shared<MyClass>(20);

    // 7. Allocating memory using std::allocator
    std::allocator<MyClass> myClassAllocator;
    MyClass* allocatedObject = myClassAllocator.allocate(1);
    myClassAllocator.construct(allocatedObject, 30);

    // 8. Cleaning up allocated memory
    myClassAllocator.destroy(allocatedObject);  // Destructs an object in allocated storage
    myClassAllocator.deallocate(allocatedObject, 1); // Deallocates storage 

    // 9. Using std::unique_ptr with custom deleter function
    std::unique_ptr<MyClass, decltype(&customDeleter)> uniquePtrWithCustomDeleter(
        new MyClass(40), customDeleter);

    // 10. Using std::shared_ptr with a lambda deleter
    std::shared_ptr<MyClass> sharedPtrWithLambda(new MyClass(50), [](MyClass* obj) {
        std::cout << "Lambda deleter called for MyClass with value: "; 
        obj->display(); 
        delete obj;
    });

    // 11. Using std::vector with std::shared_ptr
    std::vector<std::shared_ptr<MyClass>> sharedPtrVector;
    sharedPtrVector.push_back(std::make_shared<MyClass>(60));
    sharedPtrVector.push_back(std::make_shared<MyClass>(70));
    sharedPtrVector.push_back(std::make_shared<MyClass>(80));

    std::cout << "Vector of shared pointers:" << std::endl;
    for (const auto& ptr : sharedPtrVector) {
        ptr->display();
    }

    // 12. Making multiple std::shared_ptr on same object
    std::shared_ptr<MyClass> originalSharedPtr = std::make_shared<MyClass>(90);
    std::shared_ptr<MyClass> sharedPtr_copy1(originalSharedPtr);
    std::shared_ptr<MyClass> sharedPtr_copy2(originalSharedPtr);
    std::cout << "Number of shared ptrs on object is " << originalSharedPtr.use_count() << std::endl;
}

int main(){

    test_unique_ptr();

    test_basic_allocators();
    
    test_memory_management_on_my_class();

    return 0;
}
