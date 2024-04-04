#include <vector>
#include <algorithm>
#include <cassert>
#include <iostream>

using namespace std;

int main(){
    vector<int> vect;
    vect.push_back(10);
    vect.push_back(20);
    vect.push_back(5);
    vect.push_back(23);
    vect.push_back(42);
    vect.push_back(15);

    sort(vect.begin(), vect.end());    
    assert((vect.at(0) == 5) && "sort(): The first element is not the smallest");
    assert((vect.at(5) == 42) && "sort(): The last element is not the largest");    
    
    reverse(vect.begin(), vect.end());
    assert((vect.at(0) == 42) && "reverse(): Vector is reversed but the first element is not the largest");
    assert((vect.at(5) == 5) && "reverse(): Vector is reversed but the last element is not the smallest");

    auto max = max_element(vect.begin(), vect.end());
    assert((max != vect.end()) && "max_element(): Invalid return value for non-empty array.");
    assert((*max == 42) && "max_element(): Max element is not 42");

    auto min = min_element(vect.begin(), vect.end());
    assert((min != vect.end()) && "min_element(): Invalid return value for non-empty array.");
    assert((*min == 5) && "min_element(): Min element is not 5");

    return 0;
}
