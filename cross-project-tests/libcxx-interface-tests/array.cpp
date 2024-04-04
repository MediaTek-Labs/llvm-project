#include <array>
#include <iostream>
#include <cassert>

int main(){

    std::array<int, 4> a = {1, 2, 3, 4};

    // Element access
    int e;
    e = a[1];
    assert((e == 2) && "operator[]: Access to element at position 1 is not correct.");
    e = a.at(2);
    assert((e == 3) && "at(): Access to element at position 2 is not correct.");
    e = a.front();
    assert((e == 1) && "front(): Access to the first element is not correct.");
    e = a.back();
    assert((e == 4) && "back(): Access to the last element is not correct.");

    // Iterators
    std::array<int, 4>::const_iterator begin = a.begin();
    assert((*begin == 1) && "begin(): An iterator to the beginning of an array is not correct.");

    std::array<int, 4>::const_iterator end = a.end();
    assert((*(end-1) == 4) && "end(): An iterator to the end of an array is not correct.");

    // Capacity
    int size = a.size();
    assert((size == 4) && "size(): Array size is not correct.");
    
    bool is_empty = a.empty();
    assert((is_empty == false) &&  "empty():  Wrong result for emptiness check.");

    // Operations
    a.fill(2);
    assert((a.size() == 4) && "fill(): Array size is changed during fill.");
    assert((a[0] == 2) && "fill(): 1. Array is not correctly filled.");
    assert((a[1] == 2) && "fill(): 2. Array is not correctly filled.");
    assert((a[2] == 2) && "fill(): 3. Array is not correctly filled.");
    assert((a[3] == 2) && "fill(): 4. Array is not correctly filled.");
    
    return 0;
}
