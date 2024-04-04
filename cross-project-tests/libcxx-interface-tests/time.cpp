#include <ctime>
#include <iostream>
#include <cassert>

int main(){

    // Macro constants

    std::cout << "CLOCKS_PER_SEC: " << CLOCKS_PER_SEC << std::endl;
    const std::clock_t cps{CLOCKS_PER_SEC};

    std::cout << "NULL: " << NULL << std::endl;

    // Types

    const std::clock_t cps1{CLOCKS_PER_SEC};
    std::cout << "clock_t cps1: " << cps1 << std::endl;

    std::size_t s;
    std::cout << "size_t s: " << s << std::endl; 

    std::time_t t;
    std::cout << "time_t t: " << t << std::endl;

    std::tm tm;
    std::cout << "tm.tm_sec " << tm.tm_sec << std::endl;
    std::cout << "tm.tm_min " << tm.tm_min << std::endl;
    std::cout << "tm.tm_hour " << tm.tm_hour << std::endl;
    std::cout << "tm.tm_mday " << tm.tm_mday << std::endl;
    std::cout << "tm.tm_mon " << tm.tm_mon << std::endl;
    std::cout << "tm.tm_year " << tm.tm_year << std::endl;
    std::cout << "tm.tm_wday " << tm.tm_wday << std::endl;
    std::cout << "tm.tm_yday " << tm.tm_yday << std::endl;
    std::cout << "tm.tm_isdst " << tm.tm_isdst << std::endl;

    // Functions

    //  Time manipulation

    // In function `_times_r':
    // (.text._times_r+0x4): error: undefined reference to 'times'
    // collect2: error: ld returned 1 exit status
    const std::clock_t c_start = std::clock();
    std::cout << "clock(): " << c_start << std::endl;

    //In function `_gettimeofday_r':
    //(.text._gettimeofday_r+0x10): error: undefined reference to 'gettimeofday'
    //collect2: error: ld returned 1 exit status
    std::time_t time_result = std::time(nullptr);
    std::cout << "time(): " << time_result << std::endl;

    std::time_t t_start = 100;
    std::time_t t_end = 300;
    std::cout << "difftime(t_end, t_start): " << std::difftime(t_end, t_start) << std::endl;

    std::time_t start = std::time(nullptr);
    double d = 1.0;
    // some time-consuming operation
    for (int p = 0; p < 1000; ++p)
        for (int q = 0; q < 10000; ++q)
            d = d + p * d * q + d;
    std::time_t end = std::time(nullptr);
    std::cout << "difftime(end, start): " << std::difftime(end, start) << std::endl;

    // Format conversions

    const std::time_t some_time = 1710969900;
    std::cout << "ctime(&some_time): " << std::ctime(&some_time) << std::endl;
    
    std::tm some_time_tm{};
    some_time_tm.tm_year = 2024 - 1900; // 2024
    some_time_tm.tm_mon = 2; // March
    some_time_tm.tm_mday = 20; // 20th
    some_time_tm.tm_hour = 21;
    some_time_tm.tm_min = 25;
    some_time_tm.tm_wday = 3; // Wednesday
    some_time_tm.tm_isdst = 0; // Not daylight saving
    std::cout << "asctime(&some_time_tm): " << asctime(&some_time_tm) << std::endl;

    char strftime_out[100];
    std::strftime(strftime_out, sizeof(strftime_out), "%A %c", &some_time_tm);
    std::cout << "strftime(): " << strftime_out << std::endl;

    std::tm* gmtime_out = std::gmtime(&some_time);
    std::cout << "gmtime() output is below:" << std::endl;
    std::cout << "gmtime_out.tm_sec "   << gmtime_out->tm_sec << std::endl;
    std::cout << "gmtime_out.tm_min "   << gmtime_out->tm_min << std::endl;
    std::cout << "gmtime_out.tm_hour "  << gmtime_out->tm_hour << std::endl;
    std::cout << "gmtime_out.tm_mday "  << gmtime_out->tm_mday << std::endl;
    std::cout << "gmtime_out.tm_mon "   << gmtime_out->tm_mon << std::endl;
    std::cout << "gmtime_out.tm_year "  << gmtime_out->tm_year << std::endl;
    std::cout << "gmtime_out.tm_wday "  << gmtime_out->tm_wday << std::endl;
    std::cout << "gmtime_out.tm_yday "  << gmtime_out->tm_yday << std::endl;
    std::cout << "gmtime_out.tm_isdst " << gmtime_out->tm_isdst << std::endl;

    std::tm* localtime_out = std::localtime(&some_time);
    std::cout << "localtime() output is below:" << std::endl;
    std::cout << "localtime_out.tm_sec "   << localtime_out->tm_sec << std::endl;
    std::cout << "localtime_out.tm_min "   << localtime_out->tm_min << std::endl;
    std::cout << "localtime_out.tm_hour "  << localtime_out->tm_hour << std::endl;
    std::cout << "localtime_out.tm_mday "  << localtime_out->tm_mday << std::endl;
    std::cout << "localtime_out.tm_mon "   << localtime_out->tm_mon << std::endl;
    std::cout << "localtime_out.tm_year "  << localtime_out->tm_year << std::endl;
    std::cout << "localtime_out.tm_wday "  << localtime_out->tm_wday << std::endl;
    std::cout << "localtime_out.tm_yday "  << localtime_out->tm_yday << std::endl;
    std::cout << "localtime_out.tm_isdst " << localtime_out->tm_isdst << std::endl;


    std::time_t mktime_out = std::mktime(&some_time_tm); 
    std::cout << "mktime(&some_time_tm): " << mktime_out << std::endl;

    return 0;
}
