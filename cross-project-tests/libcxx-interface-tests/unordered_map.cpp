#include <unordered_map>
#include <cassert>
#include <iostream>

using namespace std;

int main(){
    unordered_map<char, int> umap;
    umap['A'] = 10;
    umap['B'] = 20;
    umap['C'] = 30;
    umap['D'] = 100;
    assert((umap.size() == 4) && "Map size not equal to 4.");

    int num_erased = umap.erase('A');
    assert((num_erased == 1) && "Only one 'A' element should have been deleted");
    assert((umap.size() == 3) && "Map size not equal to 3.");
    
    return 0;
}
