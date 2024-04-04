#include <map>
#include <string>
#include <cassert>
#include <iostream>

using namespace std;

int main(){
    map<std::string, int> m{{"CPU", 10}, {"GPU", 15}, {"RAM", 20}};
    
    m["CPU"] = 25;
    assert((m["CPU"] == 25) && "Value for key 'CPU' is not changed!");
    
    m["SSD"] = 30;
    assert((m["SSD"] == 30) && "New element was not added successfully");

    auto found = m.find("GPU");
    assert((found->first == "GPU") && "Found element with invalid key");
    assert((found->second == 15) && "invalid value for element found");

    int num_erased = m.erase("GPU");
    assert((num_erased == 1) && "Erased wrong number of elements!");
    assert((m.find("GPU") == m.end()) && "Element is not erased!");

    return 0;
}
