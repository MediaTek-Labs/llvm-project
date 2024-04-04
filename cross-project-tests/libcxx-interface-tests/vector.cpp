#include <vector>
#include <iostream>
#include <cassert>

int main(){

    std::vector<int> v = {8, 4, 5, 9};

    // Element access
    int e;
    e = v[1];
    assert((e == 4) && "operator[]: Access to element at position 1 is not correct.");
    e = v.at(2);
    assert((e == 5) && "at(): Access to element at position 2 is not correct.");
    e = v.front();
    assert((e == 8) && "front(): Access to the first element is not correct.");
    e = v.back();
    assert((e == 9) && "back(): Access to the last element is not correct.");

    // Iterators
    std::vector<int>::const_iterator begin = v.begin();
    assert((*begin == 8) && "begin(): An iterator to the beginning of a vector is not correct.");

    std::vector<int>::const_iterator end = v.end();
    assert((*(end-1) == 9) && "end(): An iterator to the end of a vector is not correct.");

    // Capacity
    int size = v.size();
    assert((size == 4) && "size(): Vector size is not correct.");

    bool is_empty = v.empty();
    assert((is_empty == false) &&  "empty():  Wrong result for emptiness check.");

    // Modifiers
    v.push_back(7);
    assert((v[v.size()-1] == 7) && "push_back(): Element not inserted at the end correctly.");

    v.pop_back();
    assert((v[v.size()-1] == 9) && "pop_back(): Element not removed from the end correctly.");

    v.insert(v.begin(), 2 ,10);
    assert((v[0] == 10) && (v[1] == 10) && "insert(): Two elements are not inserted correctly at the beginning of a vector.");

    v.erase(v.begin(), v.begin()+2);
    assert((v[0] == 8) && (v[1] == 4) && "erase(): Two elements are not removed correctly from the beginning of a vector.");

    return 0;
}
