#include <cmath>
#include <iostream>
#include <cassert>


int main(){

    // Types

    std::float_t v1;
    std::double_t v2;

    // Macros

    auto m1 = HUGE_VALF;
    auto m2 = HUGE_VAL;
    auto m3 = HUGE_VALL;
    auto m4 = INFINITY;
    auto m5 = NAN;
    auto m6 = math_errhandling;
    auto m7 = MATH_ERRNO;
    auto m8 = MATH_ERREXCEPT;

    // Functions

    // Basic operations

    std::abs((float)-20.0);
    std::abs((double)-20.0);
    std::abs((long double)-20.0);
    std::fabs((float)-20.0);
    std::fabs((long)-20.0);
    std::fabs((long double)-20.0);
    std::fabsf((float)-20.0);
    std::fabsl((long double)-20.0);

    std::fmod((float)20.0,(float)3.0);
    std::fmod((double)20.0,(double)3.0);
    std::fmod((long double)20.0,(long double)3.0);
    std::fmodf((float)20.0,(float)3.0);
    std::fmodl((long double)20.0,(long double)3.0);

    std::remainder((float)20.0,(float)3.0);
    std::remainder((double)20.0,(double)3.0);
    std::remainder((long double)20.0,(long double)3.0);
    std::remainderf((float)20.0,(float)3.0);
    std::remainderl((long double)20.0,(long double)3.0);

    int quo;
    std::remquo((float)20.0,(float)3.0, &quo);
    std::remquo((double)20.0,(double)3.0, &quo);
    std::remquo((long double)20.0,(long double)3.0, &quo);
    std::remquof((float)20.0,(float)3.0, &quo);
    std::remquol((long double)20.0,(long double)3.0, &quo);

    std::fma((float)20.0,(float)3.0,(float)5);
    std::fma((double)20.0,(double)3.0,(double)5);
    std::fma((long double)20.0,(long double)3.0,(long double)5);
    std::fmaf((float)20.0,(float)3.0,(float)5);
    std::fmal((long double)20.0,(long double)3.0,(long double)5);

    std::fmax((float)20.0,(float)3.0);
    std::fmax((double)20.0,(double)3.0);
    std::fmax((long double)20.0,(long double)3.0);
    std::fmaxf((float)20.0,(float)3.0);
    std::fmaxl((long double)20.0,(long double)3.0);

    std::fmin((float)20.0,(float)3.0);
    std::fmin((double)20.0,(double)3.0);
    std::fmin((long double)20.0,(long double)3.0);
    std::fminf((float)20.0,(float)3.0);
    std::fminl((long double)20.0,(long double)3.0);

    std::fdim((float)20.0,(float)3.0);
    std::fdim((double)20.0,(double)3.0);
    std::fdim((long double)20.0,(long double)3.0);
    std::fdimf((float)20.0,(float)3.0);
    std::fdiml((long double)20.0,(long double)3.0);
    
    std::nan("1");
    std::nanf("1");
    std::nanl("1");

    // Exponential functions

    std::exp((float)15.0);
    std::exp((double)15.0);
    std::exp((long double)15.0);
    std::expf((float)15.0);
    std::expl((long double)15.0);

    std::exp2((float)15.0);
    std::exp2((double)15.0);
    std::exp2((long double)15.0);
    std::exp2f((float)15.0);
    std::exp2l((long double)15.0);

    std::expm1((float)15.0);
    std::expm1((double)15.0);
    std::expm1((long double)15.0);
    std::expm1f((float)15.0);
    std::expm1l((long double)15.0);

    std::log((float)15.0);
    std::log((double)15.0);
    std::log((long double)15.0);
    std::logf((float)15.0);
    std::logl((long double)15.0);

    std::log10((float)15.0);
    std::log10((double)15.0);
    std::log10((long double)15.0);
    std::log10f((float)15.0);
    std::log10l((long double)15.0);

    std::log2((float)15.0);
    std::log2((double)15.0);
    std::log2((long double)15.0);
    std::log2f((float)15.0);
    std::log2l((long double)15.0);

    std::log1p((float)15.0);
    std::log1p((double)15.0);
    std::log1p((long double)15.0);
    std::log1pf((float)15.0);
    std::log1pl((long double)15.0);

    // Power functions

    std::pow((float)3.0,(float)5.0);
    std::pow((double)3.0,(double)5.0);
    std::pow((long double)3.0,(long double)5.0);
    std::pow((float)3.0,(int)5.0);
    std::pow((double)3.0,(int)5.0);
    std::pow((long double)3.0,(int)5.0);
    std::powf((float)3.0,(float)5.0);
    std::powl((long double)3.0,(long double)5.0);

    std::sqrt((float)9.0);
    std::sqrt((double)9.0);
    std::sqrt((long double)9.0);
    std::sqrtf((float)9.0);
    std::sqrtl((long double)9.0);

    std::cbrt((float)8.0);
    std::cbrt((double)8.0);
    std::cbrt((long double)8.0);
    std::cbrtf((float)8.0);
    std::cbrtl((long double)8.0);

    std::hypot((float)3.0,(float)4.0);
    std::hypot((double)3.0,(double)4.0);
    std::hypot((long double)3.0,(long double)4.0);
    std::hypotf((float)3.0,(float)4.0);
    std::hypotl((long double)3.0,(long double)4.0);

    // Trigonometric functions

    std::sin((float)25.0);
    std::sin((double)25.0);
    std::sin((long double)25.0);
    std::sinf((float)25.0);
    std::sinl((long double)25.0);

    std::cos((float)25.0);
    std::cos((double)25.0);
    std::cos((long double)25.0);
    std::cosf((float)25.0);
    std::cosl((long double)25.0);

    std::tan((float)0.5);
    std::tan((double)0.5);
    std::tan((long double)0.5);
    std::tanf((float)0.5);
    std::tanl((long double)0.5);

    std::asin((float)0.5);
    std::asin((double)0.5);
    std::asin((long double)0.5);
    std::asinf((float)0.5);
    std::asinl((long double)0.5);

    std::acos((float)0.5);
    std::acos((double)0.5);
    std::acos((long double)0.5);
    std::acosf((float)0.5);
    std::acosl((long double)0.5);

    std::atan((float)25.0);
    std::atan((double)25.0);
    std::atan((long double)25.0);
    std::atanf((float)25.0);
    std::atanl((long double)25.0);

    std::atan2((float)25.0,(float)10.0);
    std::atan2((double)25.0,(double)10.0);
    std::atan2((long double)25.0,(long double)10.0);
    std::atan2f((float)25.0,(float)10.0);
    std::atan2l((long double)25.0,(long double)10.0);

    // Hyperbolic functions

    std::sinh((float)25.0);
    std::sinh((double)25.0);
    std::sinh((long double)25.0);
    std::sinhf((float)25.0);
    std::sinhl((long double)25.0);

    std::cosh((float)25.0);
    std::cosh((double)25.0);
    std::cosh((long double)25.0);
    std::coshf((float)25.0);
    std::coshl((long double)25.0);

    std::tanh((float)0.5);
    std::tanh((double)0.5);
    std::tanh((long double)0.5);
    std::tanhf((float)0.5);
    std::tanhl((long double)0.5);

    std::asinh((float)0.5);
    std::asinh((double)0.5);
    std::asinh((long double)0.5);
    std::asinhf((float)0.5);
    std::asinhl((long double)0.5);

    std::acosh((float)0.5);
    std::acosh((double)0.5);
    std::acosh((long double)0.5);
    std::acoshf((float)0.5);
    std::acoshl((long double)0.5);

    std::atanh((float)25.0);
    std::atanh((double)25.0);
    std::atanh((long double)25.0);
    std::atanhf((float)25.0);
    std::atanhl((long double)25.0);

    // Error and gamma functions

    std::erf((float)25.0);
    std::erf((double)25.0);
    std::erf((long double)25.0);
    std::erff((float)25.0);
    std::erfl((long double)25.0);

    std::erfc((float)25.0);
    std::erfc((double)25.0);
    std::erfc((long double)25.0);
    std::erfcf((float)25.0);
    std::erfcl((long double)25.0);

    std::tgamma((float)10.0);
    std::tgamma((double)10.0);
    std::tgamma((long double)10.0);
    std::tgammaf((float)10.0);
    std::tgammal((long double)10.0);

    std::lgamma((float)10.0);
    std::lgamma((double)10.0);
    std::lgamma((long double)10.0);
    std::lgammaf((float)10.0);
    std::lgammal((long double)10.0);

    // Nearest integer floating-point operations


    std::ceil((float)10.6);
    std::ceil((double)10.6);
    std::ceil((long double)10.6);
    std::ceilf((float)10.6);
    std::ceill((long double)10.6);

    std::floor((float)10.6);
    std::floor((double)10.6);
    std::floor((long double)10.6);
    std::floorf((float)10.6);
    std::floorl((long double)10.6);

    // error:
    // inlining failed in call to always_inline ‘float trunc(float) noexcept’: function not considered for inlining
    std::trunc((float)10.6);
    std::trunc((double)10.6);
    std::trunc((long double)10.6);
    std::truncf((float)10.6);
    std::truncl((long double)10.6);

    // error:
    // inlining failed in call to always_inline ‘float round(float) noexcept’: function not considered for inlining
    std::round((float)10.6);
    std::round((double)10.6);
    std::round((long double)10.6);
    std::roundf((float)10.6);
    std::roundl((long double)10.6);

    std::lround((float)10.6);
    std::lround((double)10.6);
    std::lround((long double)10.6);
    std::lroundf((float)10.6);
    std::lroundl((long double)10.6);

    std::llround((float)10.6);
    std::llround((double)10.6);
    std::llround((long double)10.6);
    std::llroundf((float)10.6);
    std::llroundl((long double)10.6);

    std::nearbyint((float)10.6);
    std::nearbyint((double)10.6);
    std::nearbyint((long double)10.6);
    std::nearbyintf((float)10.6);
    std::nearbyintl((long double)10.6);

    std::rint((float)10.6);
    std::rint((double)10.6);
    std::rint((long double)10.6);
    std::rintf((float)10.6);
    std::rintl((long double)10.6);

    std::lrint((float)10.6);
    std::lrint((double)10.6);
    std::lrint((long double)10.6);
    std::lrintf((float)10.6);
    std::lrintl((long double)10.6);

    std::llrint((float)10.6);
    std::llrint((double)10.6);
    std::llrint((long double)10.6);
    std::llrintf((float)10.6);
    std::llrintl((long double)10.6);

    // Floating-point manipulation functions

    int exp;
    std::frexp((float)10.0,&exp);
    std::frexp((double)10.0,&exp);
    std::frexp((long double)10.0,&exp);
    std::frexpf((float)10.0,&exp);
    std::frexpl((long double)10.0,&exp);

    std::ldexp((float)10.0,2);
    std::ldexp((double)10.0,2);
    std::ldexp((long double)10.0,2);
    std::ldexpf((float)10.0,2);
    std::ldexpl((long double)10.0,2);

    float s1;
    double s2;
    long double s3;
    float s4;
    long double s5;
    std::modf((float)10.6,&s1);
    std::modf((double)10.6,&s2);
    std::modf((long double)10.6,&s3);
    std::modff((float)10.6,&s4);
    std::modfl((long double)10.6,&s5);

    std::scalbn((float)10.0,2);
    std::scalbn((double)10.0,2);
    std::scalbn((long double)10.0,2);
    std::scalbnf((float)10.0,2);
    std::scalbnl((long double)10.0,2);
    
    std::scalbln((float)10.0,2);
    std::scalbln((double)10.0,2);
    std::scalbln((long double)10.0,2);
    std::scalblnf((float)10.0,2);
    std::scalblnl((long double)10.0,2);

    std::ilogb((float)10.6);
    std::ilogb((double)10.6);
    std::ilogb((long double)10.6);
    std::ilogbf((float)10.6);
    std::ilogbl((long double)10.6);

    std::logb((float)10.6);
    std::logb((double)10.6);
    std::logb((long double)10.6);
    std::logbf((float)10.6);
    std::logbl((long double)10.6);

    std::nextafter((float)25.0,(float)10.0);
    std::nextafter((double)25.0,(double)10.0);
    std::nextafter((long double)25.0,(long double)10.0);
    std::nextafterf((float)25.0,(float)10.0);
    std::nextafterl((long double)25.0,(long double)10.0);

    // error: undefined reference to 'nexttowardf'
    // collect2: error: ld returned 1 exit status
    std::nexttoward((float)25.0,(long double)10.0);
    std::nexttoward((double)25.0,(long double)10.0);
    std::nexttoward((long double)25.0,(long double)10.0);
    // error: undefined reference to 'nexttowardf'
    // collect2: error: ld returned 1 exit status
    std::nexttowardf((float)25.0,(long double)10.0);
    std::nexttowardl((long double)25.0,(long double)10.0);

    std::copysign((float)25.0,(float)-10.0);
    std::copysign((double)25.0,(double)-10.0);
    std::copysign((long double)25.0,(long double)-10.0);
    std::copysignf((float)25.0,(float)-10.0);
    std::copysignl((long double)25.0,(long double)-10.0);
    
    // Classification and comparison

    std::fpclassify((float)10.6);
    std::fpclassify((double)10.6);
    std::fpclassify((long double)10.6);

    std::isfinite((float)10.6);
    std::isfinite((double)10.6);
    std::isfinite((long double)10.6);

    std::isinf((float)10.6);
    std::isinf((double)10.6);
    std::isinf((long double)10.6);

    std::isnan((float)10.6);
    std::isnan((double)10.6);
    std::isnan((long double)10.6);

    std::isnormal((float)10.6);
    std::isnormal((double)10.6);
    std::isnormal((long double)10.6);

    std::signbit((float)-10.6);
    std::signbit((double)-10.6);
    std::signbit((long double)-10.6);

    std::isgreater((float)25.0,(float)-10.0);
    std::isgreater((double)25.0,(double)-10.0);
    std::isgreater((long double)25.0,(long double)-10.0);

    std::isgreaterequal((float)25.0,(float)-10.0);
    std::isgreaterequal((double)25.0,(double)-10.0);
    std::isgreaterequal((long double)25.0,(long double)-10.0);

    std::isless((float)25.0,(float)-10.0);
    std::isless((double)25.0,(double)-10.0);
    std::isless((long double)25.0,(long double)-10.0);

    std::islessequal((float)25.0,(float)-10.0);
    std::islessequal((double)25.0,(double)-10.0);
    std::islessequal((long double)25.0,(long double)-10.0);

    std::islessgreater((float)25.0,(float)-10.0);
    std::islessgreater((double)25.0,(double)-10.0);
    std::islessgreater((long double)25.0,(long double)-10.0);

    std::isunordered((float)25.0,(float)-10.0);
    std::isunordered((double)25.0,(double)-10.0);
    std::isunordered((long double)25.0,(long double)-10.0);

    return 0;
}
