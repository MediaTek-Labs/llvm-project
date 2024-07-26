#include <cfenv>

int main(){

    // Macros
    int a1 = FE_ALL_EXCEPT;
    int a2 = FE_DIVBYZERO;
    int a3 = FE_INEXACT;
    int a4 = FE_INVALID;
    int a5 = FE_OVERFLOW;
    int a6 = FE_UNDERFLOW;
    int a7 = FE_DOWNWARD;
    int a8 = FE_TONEAREST;
    int a9 = FE_TOWARDZERO;
    int a10 = FE_UPWARD;
    const fenv_t* a11 = FE_DFL_ENV;

    // Functions
    std::feclearexcept(FE_ALL_EXCEPT);
    std::feraiseexcept(FE_UNDERFLOW);
    std::fetestexcept(FE_DIVBYZERO);

    std::fexcept_t e;
    std::fegetexceptflag(&e, FE_ALL_EXCEPT);
    std::fesetexceptflag(&e, FE_ALL_EXCEPT);

    std::fegetround();
    std::fesetround(FE_UPWARD);

    std::fenv_t env;
    std::fegetenv(&env);
    std::fesetenv(&env);

    std::feholdexcept(&env);
    std::feupdateenv(&env);

    return 0;
}
