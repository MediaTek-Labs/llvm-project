#include <chrono>
#include <vector>
#include <iostream>

int main(){

    constexpr int arraySize = 1000000;
    std::vector<int> numbers(arraySize, 1);
    int sum = 0;    

    auto start_time = std::chrono::high_resolution_clock::now();

    for(auto it = numbers.begin(); it != numbers.end(); it++){
        sum += *it;
    }

    auto end_time = std::chrono::high_resolution_clock::now();

    auto duration = std::chrono::duration_cast<std::chrono::microseconds>(end_time - start_time);

    std::cout << "Time taken by code block: " << duration.count() << " microseconds." << std::endl;
    
    auto time1 = std::chrono::steady_clock::now();
    auto time2 = std::chrono::system_clock::now();

    return 0;
}
