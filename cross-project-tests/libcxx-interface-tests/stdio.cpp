#include <cstdio>
#include <iostream>
#include <cassert>
#include <cstdarg>
#include <cstring>


// Used to test vsscanf
int parseInput(const char* input, const char* format, ...) {
    std::va_list args;
    va_start(args, format);
    int result = std::vsscanf(input, format, args);
    va_end(args);
    return result;
}

// Used to test vsprintf
int formatString1(char* buffer, const char* format, ...) {
    std::va_list args;
    va_start(args, format);
    int chars_written = std::vsprintf(buffer, format, args);
    va_end(args);
    return chars_written;
}

// Used to test vsnprintf
int formatString2(char* buffer, size_t bufferSize, const char* format, ...) {
    va_list args;
    va_start(args, format);
    int chars_written = vsnprintf(buffer, bufferSize, format, args);
    va_end(args);
    return chars_written;
}

int main(){

    // Types

    // std::FILE var1
    // std::fpos_t var2;
    std::size_t var3;

    // Macros

    std::cout << "NULL: " << NULL << std::endl;
    // stdin
    // stdout
    // stderr
    std::cout << "EOF: " << EOF << std::endl;
    std::cout << "FOPEN_MAX: " << FOPEN_MAX << std::endl;
    std::cout << "FILENAME_MAX: " << FILENAME_MAX << std::endl;
    // BUFSIZ
    // _IOFBF
    // _IOLBF
    // _IONBF
    // SEEK_SET
    // SEEK_CUR
    // SEEK_END
    std::cout << "TMP_MAX: " << TMP_MAX << std::endl;
    std::cout << "L_tmpnam: " << L_tmpnam << std::endl;

    // Functions

    // File access
    
    // fopen
    // freopen
    // fclose
    // fflush
    // setbuf
    // setvbuf

    // Direct input/output

    // fread
    // fwrite

    // Unformatted input/output 

    // fgetc
    // getc
    // fgets
    // fputc
    // putc
    // fputs
    // getchar
    // gets
    // putchar
    // puts
    // ungetc

    // Formatted input/output 

    // scanf
    // fscanf

    char input[] = "John 25 180.5";
    char name[20];
    int age;
    float height;
    int ret_val = std::sscanf(input, "%s %d %f", name, &age, &height);
    assert(ret_val == 3);
    assert((std::strcmp(name, "John") == 0) && (age == 25) && (height == 180.5));
    std::cout << "(sscanf) Name: " << name << std::endl;
    std::cout << "(sscanf) Age: " << age << std::endl;
    std::cout << "(sscanf) Height: " << height << std::endl;

    // vscanf
    // vfscanf
    
    char name_v[20];
    int age_v;
    float height_v;
    int ret_val_v = parseInput(input, "%s %d %f", name_v, &age_v, &height_v);
    assert(ret_val_v == 3);
    assert((std::strcmp(name_v, "John") == 0) && (age_v == 25) && (height_v == 180.5));
    std::cout << "(vsscanf) Name: " << name_v << std::endl;
    std::cout << "(vsscanf) Age: " << age_v << std::endl;
    std::cout << "(vsscanf) Height: " << height_v << std::endl;

    // printf
    // fprintf

    char buffer[100];
    int num = 123;
    float pi = 3.14159;
    int chars_written = std::sprintf(buffer, "Number: %d, Pi: %.2f", num, pi);
    assert(std::strcmp(buffer, "Number: 123, Pi: 3.14") == 0);
    assert(chars_written == 21);
    std::cout << "(sprintf) Formatted string: " << buffer << std::endl;

    char buffer2[100];
    int chars_written2 = std::snprintf(buffer2, sizeof(buffer2), "Number: %d, Pi: %.2f", num, pi);
    assert((chars_written2 >= 0) && (chars_written2 < sizeof(buffer2)));
    assert(std::strcmp(buffer2, "Number: 123, Pi: 3.14") == 0);
    std::cout << "(snprintf) Formatted string: " << buffer2 << std::endl;

    // vprintf
    // vfprintf

    char buffer3[100];
    int chars_written3 = formatString1(buffer3, "Number: %d, Pi: %.2f", num, pi);
    assert(std::strcmp(buffer3, "Number: 123, Pi: 3.14") == 0);
    assert(chars_written3 == 21);
    std::cout << "(vsprintf) Formatted string: " << buffer3 << std::endl;

    char buffer4[100];
    int chars_written4 = formatString2(buffer4, sizeof(buffer4), "Number: %d, Pi: %.2f", num, pi);
    assert((chars_written4 >= 0) && (chars_written4 < sizeof(buffer4)));
    assert(std::strcmp(buffer4, "Number: 123, Pi: 3.14") == 0);
    std::cout << "(snprintf) Formatted string: " << buffer4 << std::endl;

    // File positioning

    // ftell
    // fgetpos
    // fseek
    // fsetpos
    // rewind

    // Error handling 

    // clearerr
    // feof
    // ferror
    // perror

    // Operations on files

    // tmpfile

    int remove_ret_val1 = std::remove("file_to_be_removed_by_std::remove.txt");
    assert(remove_ret_val1 == 0);

    int rename_ret_val = std::rename("file_to_be_renamed_by_std::rename.txt", "abcd.txt");
    assert(rename_ret_val == 0);

    int remove_ret_val2 = std::remove("abcd.txt");
    assert(remove_ret_val2 == 0);

    std::string name1 = std::tmpnam(nullptr);
    std::cout << "temporary file name: " << name1 << std::endl;
    char name2[L_tmpnam];
    std::tmpnam(name2);
    std::cout << "temporary file name: " << name2 << std::endl;

    return 0;
}
