#include <cinttypes>
#include <iostream>
#include <cassert>

int main(){

    // Macros

    auto v1 = PRId8;
    auto v2 = PRId16;
    auto v3 = PRId32;
    auto v4 = PRId64;
    auto v5 = PRIdLEAST8;
    auto v6 = PRIdLEAST16;
    auto v7 = PRIdLEAST32;
    auto v8 = PRIdLEAST64;
    auto v9 = PRIdFAST8;
    auto v10 = PRIdFAST16;
    auto v11 = PRIdFAST32;
    auto v12 = PRIdFAST64;
    auto v13 = PRIdMAX;
    auto v14 = PRIdPTR;
    auto v15 = PRIi8;
    auto v16 = PRIi16;
    auto v17 = PRIi32;
    auto v18 = PRIi64;
    auto v19 = PRIiLEAST8;
    auto v20 = PRIiLEAST16;
    auto v21 = PRIiLEAST32;
    auto v22 = PRIiLEAST64;
    auto v23 = PRIiFAST8;
    auto v24 = PRIiFAST16;
    auto v25 = PRIiFAST32;
    auto v26 = PRIiFAST64;
    auto v27 = PRIiMAX;
    auto v28 = PRIiPTR;
    auto v29 = PRIu8;
    auto v30 = PRIu16;
    auto v31 = PRIu32;
    auto v32 = PRIu64;
    auto v33 = PRIuLEAST8;
    auto v34 = PRIuLEAST16;
    auto v35 = PRIuLEAST32;
    auto v36 = PRIuLEAST64;
    auto v37 = PRIuFAST8;
    auto v38 = PRIuFAST16;
    auto v39 = PRIuFAST32;
    auto v40 = PRIuFAST64;
    auto v41 = PRIuMAX;
    auto v42 = PRIuPTR;
    auto v43 = PRIo8;
    auto v44 = PRIo16;
    auto v45 = PRIo32;
    auto v46 = PRIo64;
    auto v47 = PRIoLEAST8;
    auto v48 = PRIoLEAST16;
    auto v49 = PRIoLEAST32;
    auto v50 = PRIoLEAST64;
    auto v51 = PRIoFAST8;
    auto v52 = PRIoFAST16;
    auto v53 = PRIoFAST32;
    auto v54 = PRIoFAST64;
    auto v55 = PRIoMAX;
    auto v56 = PRIoPTR;
    auto v57 = PRIx8;
    auto v58 = PRIx16;
    auto v59 = PRIx32;
    auto v60 = PRIx64;
    auto v61 = PRIxLEAST8;
    auto v62 = PRIxLEAST16;
    auto v63 = PRIxLEAST32;
    auto v64 = PRIxLEAST64;
    auto v65 = PRIxFAST8;
    auto v66 = PRIxFAST16;
    auto v67 = PRIxFAST32;
    auto v68 = PRIxFAST64;
    auto v69 = PRIxMAX;
    auto v70 = PRIxPTR;
    auto v71 = PRIX8;
    auto v72 = PRIX16;
    auto v73 = PRIX32;
    auto v74 = PRIX64;
    auto v75 = PRIXLEAST8;
    auto v76 = PRIXLEAST16;
    auto v77 = PRIXLEAST32;
    auto v78 = PRIXLEAST64;
    auto v79 = PRIXFAST8;
    auto v80 = PRIXFAST16;
    auto v81 = PRIXFAST32;
    auto v82 = PRIXFAST64;
    auto v83 = PRIXMAX;
    auto v84 = PRIXPTR;
    auto v85 = SCNd8;
    auto v86 = SCNd16;
    auto v87 = SCNd32;
    auto v88 = SCNd64;
    auto v89 = SCNdLEAST8;
    auto v90 = SCNdLEAST16;
    auto v91 = SCNdLEAST32;
    auto v92 = SCNdLEAST64;
    auto v93 = SCNdFAST8;
    auto v94 = SCNdFAST16;
    auto v95 = SCNdFAST32;
    auto v96 = SCNdFAST64;
    auto v97 = SCNdMAX;
    auto v98 = SCNdPTR;
    auto v99 = SCNi8;
    auto v100 = SCNi16;
    auto v101 = SCNi32;
    auto v102 = SCNi64;
    auto v103 = SCNiLEAST8;
    auto v104 = SCNiLEAST16;
    auto v105 = SCNiLEAST32;
    auto v106 = SCNiLEAST64;
    auto v107 = SCNiFAST8;
    auto v108 = SCNiFAST16;
    auto v109 = SCNiFAST32;
    auto v110 = SCNiFAST64;
    auto v111 = SCNiMAX;
    auto v112 = SCNiPTR;
    auto v113 = SCNu8;
    auto v114 = SCNu16;
    auto v115 = SCNu32;
    auto v116 = SCNu64;
    auto v117 = SCNuLEAST8;
    auto v118 = SCNuLEAST16;
    auto v119 = SCNuLEAST32;
    auto v120 = SCNuLEAST64;
    auto v121 = SCNuFAST8;
    auto v122 = SCNuFAST16;
    auto v123 = SCNuFAST32;
    auto v124 = SCNuFAST64;
    auto v125 = SCNuMAX;
    auto v126 = SCNuPTR;
    auto v127 = SCNo8;
    auto v128 = SCNo16;
    auto v129 = SCNo32;
    auto v130 = SCNo64;
    auto v131 = SCNoLEAST8;
    auto v132 = SCNoLEAST16;
    auto v133 = SCNoLEAST32;
    auto v134 = SCNoLEAST64;
    auto v135 = SCNoFAST8;
    auto v136 = SCNoFAST16;
    auto v137 = SCNoFAST32;
    auto v138 = SCNoFAST64;
    auto v139 = SCNoMAX;
    auto v140 = SCNoPTR;
    auto v141 = SCNx8;
    auto v142 = SCNx16;
    auto v143 = SCNx32;
    auto v144 = SCNx64;
    auto v145 = SCNxLEAST8;
    auto v146 = SCNxLEAST16;
    auto v147 = SCNxLEAST32;
    auto v148 = SCNxLEAST64;
    auto v149 = SCNxFAST8;
    auto v150 = SCNxFAST16;
    auto v151 = SCNxFAST32;
    auto v152 = SCNxFAST64;
    auto v153 = SCNxMAX;
    auto v154 = SCNxPTR;


    // Absolute value
    int num1 = 5;
    int num2 = -5;
    assert((std::abs(num1) == 5) && "Wrong std::abs result for int number 5.");
    assert((std::abs(num2) == 5) && "Wrong std::abs result for int number -5.");
    long num3 = 10;
    long num4 = -10;
    assert((std::abs(num3) == 10) && "Wrong std::abs result for long number 10.");
    assert((std::abs(num4) == 10) && "Wrong std::abs result for long number -10.");
    assert((std::labs(num3) == 10) && "Wrong std::labs result for long number 10.");
    assert((std::labs(num4) == 10) && "Wrong std::labs result for long number -10.");
    long long num5 = 15;
    long long num6 = -15;
    assert((std::abs(num5) == 15) && "Wrong std::abs result for long long number 15.");
    assert((std::abs(num6) == 15) && "Wrong std::abs result for long long number -15.");
    assert((std::llabs(num5) == 15) && "Wrong std::llabs result for long long number 15.");
    assert((std::llabs(num6) == 15) && "Wrong std::llabs result for long long number -15.");

    std::intmax_t num7 = 20;
    std::intmax_t num8 = -20;
    std::intmax_t res1 = std::imaxabs(num7);
    assert(res1 == 20);
    std::intmax_t res2 = std::imaxabs(num8);
    assert(res2 == 20);

    // Quotient and remainder
    int dividend1 = 20;
    int divisor1 = 3;
    std::div_t dv1 = std::div(dividend1, divisor1);
    assert(dividend1 == divisor1 * dv1.quot + dv1.rem);
    assert(dv1.quot == dividend1 / divisor1);
    assert(dv1.rem == dividend1 % divisor1);

    long dividend2 = 30;
    long divisor2 = 4;
    std::ldiv_t dv2 = std::div(dividend2, divisor2);
    assert(dividend2 == divisor2 * dv2.quot + dv2.rem);
    assert(dv2.quot == dividend2 / divisor2);
    assert(dv2.rem == dividend2 % divisor2);
    std::ldiv_t dv2_l = std::ldiv(dividend2, divisor2);
    assert(dividend2 == divisor2 * dv2_l.quot + dv2_l.rem);
    assert(dv2_l.quot == dividend2 / divisor2);
    assert(dv2_l.rem == dividend2 % divisor2);
    
    long long dividend3 = 40;
    long long divisor3 = 6;
    std::lldiv_t dv3 = std::div(dividend3, divisor3);
    assert(dividend3 == divisor3 * dv3.quot + dv3.rem);
    assert(dv3.quot == dividend3 / divisor3);
    assert(dv3.rem == dividend3 % divisor3);
    std::lldiv_t dv3_ll = std::lldiv(dividend3, divisor3);
    assert(dividend3 == divisor3 * dv3_ll.quot + dv3_ll.rem);
    assert(dv3_ll.quot == dividend3 / divisor3);
    assert(dv3_ll.rem == dividend3 % divisor3);


    //std::intmax_t dividend4 = 50;
    //std::intmax_t divisor4 = 7;
    // // error: conversion from ‘lldiv_t’ to non-scalar type ‘imaxdiv_t’ requested
    //std::imaxdiv_t dv4 = std::div(dividend4, divisor4);

     std::intmax_t dividend5 = 50;
     std::intmax_t divisor5 = 7;
    // // error: undefined reference to 'imaxdiv'
     std::imaxdiv_t dv5 = std::imaxdiv(dividend5, divisor5);

    // // strtoimax, strtoumax
    std::string str = "helloworld";

    // // error: undefined reference to 'strtoimax'
    std::intmax_t val = std::strtoimax(str.c_str(), nullptr, 36);
    std::cout << str << " in base 36 is " << val << " in base 10" << std::endl;
    
    // // error: undefined reference to 'strtoumax'
    std::uintmax_t val2 = std::strtoumax(str.c_str(), nullptr, 30);
    std::cout << str << " in base 30 is " << val2 << " in base 30" << std::endl;


    // // wcstoimax, wcstoumax
    std::wstring wstr = L"helloworld";

    // // error: undefined reference to 'wcstoimax'
    val = std::wcstoimax(wstr.c_str(), nullptr, 36);
    std::wcout << wstr << " in base 36 is " << val << " in base 10" << std::endl;

    // // error: undefined reference to 'wcstoumax'
    val2 = std::wcstoumax(wstr.c_str(), nullptr, 36);
    std::wcout << wstr << " in base 36 is " << val2 << " in base 10" << std::endl;

    return 0;
}
