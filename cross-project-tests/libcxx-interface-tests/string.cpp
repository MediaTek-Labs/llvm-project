#include<string>
#include<iostream>
#include<cassert>


int main(){

    std::string str1 = "hello";
    std::string str2 = "world";

    // Element access
    const char c1 = str1[0];
    assert((c1 == 'h') && "operator[]: Character at position 0 is no correct.");

    const char c2 = str1.at(1);
    assert((c2 == 'e') && "at(): Character at position 1 is no correct.");

    const char c3 = str1.front();
    assert((c3 == 'h') && "front(): The first character is not correct.");

    const char c4 = str1.back();
    assert((c4 == 'o') && "back(): The last character is not correct.");

    // Iterators
    std::string::const_iterator begin = str1.begin();
    assert((*begin == 'h') && "begin(): An iterator to the beginning of a string is not correct.");

    std::string::const_iterator end = str1.end();
    assert((*(end-1) == 'o') && "end(): An iterator to the end of a string is not correct.");

    // Capacity
    std::size_t size = str1.size();
    assert((size == 5) && "size(): String size is not correct.");

    // Concatenation
    std::string str3 = str1 + " " + str2;
    assert((str3 == "hello world") && "operator+: String concatenation is not successful.");

    // Search
    std::string::size_type pos = str3.find(" ");
    assert((pos == 5) && "find(): White space not found.");
    
    // Operations
    std::string str4 = str3.substr(0, pos);
    std::string str5 = str3.substr(pos+1);
    assert((str4 == "hello") && "substr(): Substring is not correct.");
    assert((str5 == "world") && "substr(): Substring is not correct.");

    int comparison_result;
    comparison_result  = str4.compare(str1);
    assert((comparison_result == 0) && "compare(): 1. String comparison is not correct.");
    comparison_result = str4.compare(str2);
    assert((comparison_result < 0) && "compare(): 2. String comparison is not correct.");
    comparison_result = str5.compare(str1);
    assert((comparison_result > 0) && "compare(): 3. String comparison is not correct.");

    // Modifiers
    str5.erase(0,1);
    assert((str5 == "orld") && "erase(): Character not erased correctly.");
    
    str5.push_back('!');
    str5.push_back('?');
    assert((str5 == "orld!?") && "push_back(): Character not inserted at the end correctly.");

    str5.pop_back();
    assert((str5 == "orld!") && "pop_back(): Character not removed from the end correctly.");

    str5.insert(0, 1, 'w');
    assert((str5 == "world!") && "insert(): Character not inserted correctly.");
    
    str4.push_back(' ');
    str4.append(str5);
    assert((str4 == "hello world!") && "append(): String not appended correctly.");

    return 0;
}
