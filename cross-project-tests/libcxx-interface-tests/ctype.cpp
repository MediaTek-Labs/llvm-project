#include <cctype>
#include <iostream>
#include <cassert>

int main(){
    
    assert(std::isalnum('0'));
    assert(std::isalnum('A'));
    assert(std::isalnum('a'));
    assert(!std::isalnum('#'));
    assert(!std::isalnum(' '));

    assert(!std::isalpha('0'));
    assert(std::isalpha('A'));
    assert(std::isalpha('a'));
    assert(!std::isalpha('#'));
    assert(!std::isalpha(' '));

    assert(!std::islower('0'));
    assert(!std::islower('A'));
    assert(std::islower('a'));
    assert(!std::islower('#'));
    assert(!std::islower(' '));

    assert(!std::isupper('0'));
    assert(std::isupper('A'));
    assert(!std::isupper('a'));
    assert(!std::isupper('#'));
    assert(!std::isupper(' '));

    assert(std::isdigit('0'));
    assert(!std::isdigit('A'));
    assert(!std::isdigit('a'));
    assert(!std::isdigit('#'));
    assert(!std::isdigit(' '));

    assert(std::isxdigit('0'));
    assert(std::isxdigit('9'));
    assert(std::isxdigit('A'));
    assert(std::isxdigit('F'));
    assert(std::isxdigit('a'));
    assert(std::isxdigit('f'));
    assert(!std::isxdigit('S'));
    assert(!std::isxdigit('s'));
    assert(!std::isxdigit('#'));
    assert(!std::isxdigit(' '));

    assert(std::iscntrl(0x7F));
    assert(std::iscntrl(0x00));
    assert(std::iscntrl(0x1F));
    assert(!std::iscntrl('0'));
    assert(!std::iscntrl('A'));
    assert(!std::iscntrl('a'));
    assert(!std::iscntrl('#'));
    assert(!std::iscntrl(' '));

    assert(std::isgraph('0'));
    assert(std::isgraph('A'));
    assert(std::isgraph('a'));
    assert(std::isgraph('#'));
    assert(!std::isgraph(' '));

    assert(std::isspace(' '));
    assert(std::isspace('\f'));
    assert(std::isspace('\n'));
    assert(std::isspace('\r'));
    assert(std::isspace('\t'));
    assert(std::isspace('\v'));
    assert(!std::isspace('0'));
    assert(!std::isspace('A'));
    assert(!std::isspace('a'));
    assert(!std::isspace('#'));

    assert(std::isblank(' '));
    assert(std::isblank('\t'));
    assert(!std::isblank('\f'));
    assert(!std::isblank('\n'));
    assert(!std::isblank('\r'));
    assert(!std::isblank('\v'));
    assert(!std::isblank('0'));
    assert(!std::isblank('A'));
    assert(!std::isblank('a'));
    assert(!std::isblank('#'));

    assert(std::isprint('0'));
    assert(std::isprint('A'));
    assert(std::isprint('a'));
    assert(std::isprint('#'));
    assert(std::isprint(' '));
    assert(!std::isprint('\t'));
    assert(!std::isprint('\f'));
    assert(!std::isprint('\n'));
    assert(!std::isprint('\r'));
    assert(!std::isprint('\v'));

    assert(std::ispunct('#'));
    assert(!std::ispunct('0'));
    assert(!std::ispunct('A'));
    assert(!std::ispunct('a'));
    assert(!std::ispunct(' '));
    assert(!std::ispunct('\t'));
    assert(!std::ispunct('\n'));
    assert(!std::ispunct('\f'));
    assert(!std::ispunct('\r'));
    assert(!std::ispunct('\v'));


    assert(std::islower(std::tolower('A')));
    assert(std::islower(std::tolower('a')));
    assert(std::tolower('0') == '0');
    assert(std::tolower('#') == '#');
    assert(std::tolower(' ') == ' ');
    assert(std::tolower('\t') == '\t');
    assert(std::tolower('\n') == '\n');
    assert(std::tolower('\f') == '\f');
    assert(std::tolower('\r') == '\r');
    assert(std::tolower('\v') == '\v');

    assert(std::isupper(std::toupper('A')));
    assert(std::isupper(std::toupper('a')));
    assert(std::toupper('0') == '0');
    assert(std::toupper('#') == '#');
    assert(std::toupper(' ') == ' ');
    assert(std::toupper('\t') == '\t');
    assert(std::toupper('\n') == '\n');
    assert(std::toupper('\f') == '\f');
    assert(std::toupper('\r') == '\r');
    assert(std::toupper('\v') == '\v');

    return 0;
}
