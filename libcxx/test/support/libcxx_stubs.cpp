namespace std {
void __libcpp_mutex_unlock(void **){return;}
void __libcpp_mutex_lock(void **){return;}
}

struct __attribute__((__visibility__("hidden")))  __cxa_eh_globals {
    void *   caughtExceptions;
    unsigned int        uncaughtExceptions;
};

namespace __cxxabiv1 {
extern "C" {
    static void * eh_globals;
    void *cxa_get_globals() { return &eh_globals; }
} // extern "C"
} // namespace __cxxabiv1
