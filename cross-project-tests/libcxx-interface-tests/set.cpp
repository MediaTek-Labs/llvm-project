#include <set>
#include <cassert>
#include <iostream>

using namespace std;

int main(){
    set<char> s;
    s.insert('S');
    s.insert('F');
    s.insert('S');
    assert((s.size() == 2) && "Set does not have an adequate number of elements!");

    s.erase('S');
    assert((s.size() == 1) && "Set does not have an adequate number of elements after deletion!");

    int num_el = s.count('F');
    assert((num_el == 1) && "Set must have one 'F' element!");

    return 0;
}
