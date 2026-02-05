#include<cerrno>
#include<__chrono/duration.h>
#include<ctime>

namespace std {
  int __libcpp_recursive_mutex_init(void** __m) { return EINVAL; }
  int __libcpp_recursive_mutex_lock(void** __m) { return EINVAL; }
  bool __libcpp_recursive_mutex_trylock(void** __m) { return false; }
  int __libcpp_recursive_mutex_unlock(void** __m) { return EINVAL; }
  int __libcpp_recursive_mutex_destroy(void** __m) { return EINVAL; }
  int __libcpp_mutex_lock(void **) { return 0; }
  bool __libcpp_mutex_trylock(void **) { return false; }
  int __libcpp_mutex_unlock(void **) { return 0; }
  int __libcpp_mutex_destroy(void **) { return EINVAL; }
  int __libcpp_thread_get_current_id() { return ENOSYS; }
  int __libcpp_thread_get_id(void* __t) { return EINVAL; }
  int __libcpp_thread_create(void** __t, void* (*__func)(void*), void* __arg) { return EINVAL; }
  bool __libcpp_thread_isnull(void* const *__t) { return false; }
  bool __libcpp_thread_id_less(unsigned __t1, unsigned __t2) { return false; }
  bool __libcpp_thread_id_equal(long __t1, long __t2) { return true; }
  int __libcpp_thread_join(void** __t) { return EINVAL; }
  int __libcpp_thread_detach(void** __t) { return EINVAL; }
  int __libcpp_thread_yield() { return ENOSYS; }
  int __libcpp_condvar_broadcast(void**) { return ENOSYS; }
  int __libcpp_condvar_wait(void**,void**) { return ENOSYS; }
  int __libcpp_condvar_signal(void**) { return ENOSYS; }
  int __libcpp_condvar_timedwait(void**,void**,timespec*) { return ENOSYS; }
  int __libcpp_condvar_destroy(void**) { return ENOSYS; }
  int __libcpp_tls_create(long*, void(*)(void*)) { return ENOSYS; }
  int __libcpp_tls_get(long) { return ENOSYS; }
  void __libcpp_tls_set(long, void*) { return; }
  void __libcpp_thread_sleep_for(const chrono::nanoseconds& __ns) { return; }
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
