#include <queue>
#include <cassert>
#include <iostream>

using namespace std;

int main(){
    queue<int> q;
    q.push(10);
    q.push(20);
    q.push(30);
    assert((q.size()==3) && "Queue does not have an adequate number of elements");    
    
    q.pop();
    q.pop();
    assert((q.size() == 1) && "Queue does not have an adequate number of elements");
    
    return 0;
}
