#include <cstdint>
#include <iostream>
#include <cassert>

int main(){

    // Types

    std::int8_t v1;
    std::int16_t v2;
    std::int32_t v3;
    std::int64_t v4;
    std::int_fast8_t v5;
    std::int_fast16_t v6;
    std::int_fast32_t v7;
    std::int_fast64_t v8;
    std::int_least8_t v9;
    std::int_least16_t v10;
    std::int_least32_t v11;
    std::int_least64_t v12;
    std::intmax_t v13;
    std::intptr_t v14;
    std::uint8_t v15;
    std::uint16_t v16;
    std::uint32_t v17;
    std::uint64_t v18;
    std::uint_fast8_t v19;
    std::uint_fast16_t v20;
    std::uint_fast32_t v21;
    std::uint_fast64_t v22;
    std::uint_least8_t v23;
    std::uint_least16_t v24;
    std::uint_least32_t v25;
    std::uint_least64_t v26;
    std::uintmax_t v27;
    std::uintptr_t v28;

    // Macros

    // Signed integers : minimum value

    auto m1 = INT8_MIN;
    auto m2 = INT16_MIN;
    auto m3 = INT32_MIN;
    auto m4 = INT64_MIN;
    auto m5 = INT_FAST8_MIN;
    auto m6 = INT_FAST16_MIN;
    auto m7 = INT_FAST32_MIN;
    auto m8 = INT_FAST64_MIN;
    auto m9 = INT_LEAST8_MIN;
    auto m10 = INT_LEAST16_MIN;
    auto m11 = INT_LEAST32_MIN;
    auto m12 = INT_LEAST64_MIN;
    auto m13 = INTPTR_MIN;
    auto m14 = INTMAX_MIN;

    // Signed integers : maximum value

    auto m15 = INT8_MAX;
    auto m16 = INT16_MAX;
    auto m17 = INT32_MAX;
    auto m18 = INT64_MAX;
    auto m19 = INT_FAST8_MAX;
    auto m20 = INT_FAST16_MAX;
    auto m21 = INT_FAST32_MAX;
    auto m22 = INT_FAST64_MAX;
    auto m23 = INT_LEAST8_MAX;
    auto m24 = INT_LEAST16_MAX;
    auto m25 = INT_LEAST32_MAX;
    auto m26 = INT_LEAST64_MAX;
    auto m27 = INTPTR_MAX;
    auto m28 = INTMAX_MAX;

    // Unsigned integers : maximum value

    auto m29 = UINT8_MAX;
    auto m30 = UINT16_MAX;
    auto m31 = UINT32_MAX;
    auto m32 = UINT64_MAX;
    auto m33 = UINT_FAST8_MAX;
    auto m34 = UINT_FAST16_MAX;
    auto m35 = UINT_FAST32_MAX;
    auto m36 = UINT_FAST64_MAX;
    auto m37 = UINT_LEAST8_MAX;
    auto m38 = UINT_LEAST16_MAX;
    auto m39 = UINT_LEAST32_MAX;
    auto m40 = UINT_LEAST64_MAX;
    auto m41 = UINTPTR_MAX;
    auto m42 = UINTMAX_MAX;

    // Limits of other integer types

    auto m43 = PTRDIFF_MIN;
    auto m44 = PTRDIFF_MAX;
    auto m45 = SIZE_MAX;
    auto m46 = SIG_ATOMIC_MIN;
    auto m47 = SIG_ATOMIC_MAX;
    auto m48 = WCHAR_MIN;
    auto m49 = WCHAR_MAX;
    auto m50 = WINT_MIN;
    auto m51 = WINT_MAX;

    // Function macros for integer constants

    INT8_C(10);
    INT16_C(10);
    INT32_C(10);
    INT64_C(10);
    INTMAX_C(10);
    UINT8_C(10);
    UINT16_C(10);
    UINT32_C(10);
    UINT64_C(10);
    UINTMAX_C(10);

    return 0;
}
