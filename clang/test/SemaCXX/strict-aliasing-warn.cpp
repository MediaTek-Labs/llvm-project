// RUN: %clang_cc1 %s -O0 -Wstrict-aliasing -S -o %t -verify=quiet
// RUN: %clang_cc1 %s -O2 -Wstrict-aliasing=0 -S -o %t -verify=quiet
// RUN: %clang_cc1 %s -O2 -Wno-strict-aliasing -S -o %t -verify=quiet
// RUN: %clang_cc1 %s -O2 -Wstrict-aliasing=1 -S -o %t -verify=level1,level12,level123
// RUN: %clang_cc1 %s -O2 -Wstrict-aliasing=2 -S -o %t -verify=level2,level23,level12,level123
// RUN: %clang_cc1 %s -O2 -Wstrict-aliasing=3 -S -o %t -verify=level23,level123
// RUN: %clang_cc1 %s -O2 -Wstrict-aliasing -S -o %t -verify=level23,level123
// RUN: %clang_cc1 %s -O2 -S -o %t -verify=level23,level123

// quiet-no-diagnostics

// There seems to be a diagnostic bug in that elaborated type names are
// sometimes printed with a nested-name and sometime not.

#if _LP64
typedef long SMALL;
typedef long long BIG;
typedef unsigned long USMALL;
#else
typedef int SMALL;
typedef long BIG;
typedef unsigned int USMALL;
#endif

SMALL Scalar;
SMALL Ary[2];
struct Struct_t {int m;} Struct;
struct Struct2_t {float f; int m;};

_Complex int CPLx;

template<typename T> void LValue(T &);
template<typename T> void RValue(T);

namespace scalar {
void VarPtr(SMALL *Ptr) {
  // GCC: 1
  // level1-warning@+1{{type-punned pointer might break}}
  RValue(reinterpret_cast<BIG *>(Ptr));
  // level1-note@-1{{are not alias compatible}}

  // GCC: 1
  // level1-warning@+1{{type-punned reference might break}}
  RValue(reinterpret_cast<BIG &>(*Ptr));
  // level1-note@-1{{are not alias compatible}}

  // GCC:
  RValue(static_cast<BIG *>(static_cast<void *>(Ptr)));

  // GCC: 1
  // level1-warning@+1{{type-punned pointer might break}}
  RValue(reinterpret_cast<BIG *>(static_cast<void *>(Ptr)));
  // level1-note@-1{{are not alias compatible}}

  // GCC:
  RValue(static_cast<BIG *>(reinterpret_cast<void *>(Ptr)));

  // GCC: 1
  // level1-warning@+1{{type-punned reference might break}}
  RValue(reinterpret_cast<BIG &>(*Ptr));
  // level1-note@-1{{are not alias compatible}}

  // GCC: 1
  // level1-warning@+1{{type-punned pointer might break}}
  RValue(*reinterpret_cast<BIG *>(Ptr));
  // level1-note@-1{{are not alias compatible}}

  // GCC: 1
  // level1-warning@+1{{type-punned reference might break}}
  LValue(reinterpret_cast<BIG &>(*Ptr));
  // level1-note@-1{{are not alias compatible}}

  // GCC: 1
  // level1-warning@+1{{type-punned pointer might break}}
  LValue(*reinterpret_cast<BIG *>(Ptr));
  // level1-note@-1{{are not alias compatible}}
}

void VarRef(SMALL &Ref) {
  // GCC: 1
  // level1-warning@+1{{type-punned pointer might break}}
  RValue(reinterpret_cast<BIG *>(&Ref));
  // level1-note@-1{{are not alias compatible}}

  // GCC: 1
  // level1-warning@+1{{type-punned reference might break}}
  RValue(reinterpret_cast<BIG &>(Ref));
  // level1-note@-1{{are not alias compatible}}

  // GCC:
  RValue(static_cast<BIG *>(static_cast<void *>(&Ref)));

  // GCC: 1
  // level1-warning@+1{{type-punned pointer might break}}
  RValue(reinterpret_cast<BIG *>(static_cast<void *>(&Ref)));
  // level1-note@-1{{are not alias compatible}}

  // GCC:
  RValue(static_cast<BIG *>(reinterpret_cast<void *>(&Ref)));

  // GCC: 1
  // level1-warning@+1{{type-punned reference might break}}
  RValue(reinterpret_cast<BIG &>(Ref));
  // level1-note@-1{{are not alias compatible}}

  // GCC: 1
  // level1-warning@+1{{type-punned pointer might break}}
  RValue(*reinterpret_cast<BIG *>(&Ref));
  // level1-note@-1{{are not alias compatible}}

  // GCC: 1
  // level1-warning@+1{{type-punned reference might break}}
  LValue(reinterpret_cast<BIG &>(Ref));
  // level1-note@-1{{are not alias compatible}}

  // GCC: 1
  // level1-warning@+1{{type-punned reference might break}}
  LValue(reinterpret_cast<BIG &>(Ref));
  // level1-note@-1{{are not alias compatible}}

  // GCC: 1
  // level1-warning@+1{{type-punned pointer might break}}
  LValue(*reinterpret_cast<BIG *>(&Ref));
  // level1-note@-1{{are not alias compatible}}
}

void Object() {
  // GCC: 1, 2
  // level2-warning@+2{{type-punned pointer breaks}}
  // level1-warning@+1{{type-punned pointer might break}}
  RValue(reinterpret_cast<BIG *>(&Scalar));
  // level12-note@-1{{are not alias compatible}}

  // GCC: 1, 2, 3
  // level23-warning@+2{{type-punned reference breaks}}
  // level1-warning@+1{{type-punned reference might break}}
  RValue(reinterpret_cast<BIG &>(Scalar));
  // level123-note@-1{{are not alias compatible}}

  // GCC:
  RValue(static_cast<BIG *>(static_cast<void *>(&Scalar)));

  // GCC: 1, 2
  // level2-warning@+2{{type-punned pointer breaks}}
  // level1-warning@+1{{type-punned pointer might break}}
  RValue(reinterpret_cast<BIG *>(static_cast<void *>(&Scalar)));
  // level12-note@-1{{are not alias compatible}}

  // GCC:
  RValue(static_cast<BIG *>(reinterpret_cast<void *>(&Scalar)));

  // GCC: 1, 2, 3
  // level23-warning@+2{{type-punned reference breaks}}
  // level1-warning@+1{{type-punned reference might break}}
  RValue(reinterpret_cast<BIG &>(Scalar));
  // level123-note@-1{{are not alias compatible}}

  // GCC: 1, 2, 3
  // level23-warning@+2{{type-punned pointer breaks}}
  // level1-warning@+1{{type-punned pointer might break}}
  RValue(*reinterpret_cast<BIG *>(&Scalar));
  // level123-note@-1{{are not alias compatible}}

  // GCC: 1, 2, 3
  // level2-warning@+2{{type-punned reference breaks}}
  // level1-warning@+1{{type-punned reference might break}}
  LValue(reinterpret_cast<BIG &>(Scalar));
  // level12-note@-1{{are not alias compatible}}
  
  // GCC: 1, 2, 3
  // level23-warning@+2{{type-punned pointer breaks}}
  // level1-warning@+1{{type-punned pointer might break}}
  LValue(*reinterpret_cast<BIG *>(&Scalar));
  // level123-note@-1{{are not alias compatible}}
}

// Level 1, 2, 3 - 1, 2, 3
void DetectedVariants() {
  // GCC: 1, 2, 3
  // level23-warning@+2{{type-punned reference breaks}}
  // level1-warning@+1{{type-punned reference might break}}
  RValue(reinterpret_cast<BIG &>(Ary[1]));
  // level123-note@-1{{are not alias compatible}}

  // GCC: 1, 2, 3
  // level23-warning@+2{{type-punned pointer breaks}}
  // level1-warning@+1{{type-punned pointer might break}}
  RValue(*reinterpret_cast<BIG *>(&Ary[1]));
  // level123-note@-1{{are not alias compatible}}

  // GCC: 1, 2, 3
  // level23-warning@+2{{type-punned reference breaks}}
  // level1-warning@+1{{type-punned reference might break}}
  RValue(reinterpret_cast<BIG &>(Struct.m));
  // level123-note@-1{{are not alias compatible}}

  // GCC: 1, 2, 3
  // level23-warning@+2{{type-punned pointer breaks}}
  // level1-warning@+1{{type-punned pointer might break}}
  RValue(*reinterpret_cast<BIG *>(&Struct.m));
  // level123-note@-1{{are not alias compatible}}

  // GCC: 1, 2, 3
  // level23-warning@+2{{type-punned reference breaks}}
  // level1-warning@+1{{type-punned reference might break}}
  RValue(reinterpret_cast<BIG &>((&Struct)->m));
  // level123-note@-1{{are not alias compatible}}

  // GCC: 1, 2, 3
  // level23-warning@+2{{type-punned pointer breaks}}
  // level1-warning@+1{{type-punned pointer might break}}
  RValue(*reinterpret_cast<BIG *>(&(&Struct)->m));
  // level123-note@-1{{are not alias compatible}}

  // GCC: 1, 2, 3
  // level23-warning@+2{{type-punned reference breaks}}
  // level1-warning@+1{{type-punned reference might break}}
  RValue(reinterpret_cast<BIG &>(__real__(CPLx)));
  // level123-note@-1{{are not alias compatible}}

  // GCC: 1, 2, 3
  // level23-warning@+2{{type-punned pointer breaks}}
  // level1-warning@+1{{type-punned pointer might break}}
  RValue(*reinterpret_cast<BIG *>(&__real__(CPLx)));
  // level123-note@-1{{are not alias compatible}}

  // GCC: 1, 2, 3
  // level23-warning@+2{{type-punned reference breaks}}
  // level1-warning@+1{{type-punned reference might break}}
  RValue(reinterpret_cast<BIG &>(__imag__(CPLx)));
  // level123-note@-1{{are not alias compatible}}

  // GCC: 1, 2, 3
  // level23-warning@+2{{type-punned pointer breaks}}
  // level1-warning@+1{{type-punned pointer might break}}
  RValue(*reinterpret_cast<BIG *>(&__imag__(CPLx)));
  // level123-note@-1{{are not alias compatible}}
}

void Ok() {
  // GCC:
  RValue(reinterpret_cast<USMALL *>(&Scalar));
  // GCC:
  RValue(reinterpret_cast<USMALL &>(Scalar));
  // GCC:
  RValue(static_cast<USMALL *>(static_cast<void *>(&Scalar)));
  // GCC:
  RValue(reinterpret_cast<USMALL *>(static_cast<void *>(&Scalar)));
  // GCC:
  RValue(static_cast<USMALL *>(reinterpret_cast<void *>(&Scalar)));
  // GCC:
  RValue(reinterpret_cast<USMALL &>(Scalar));
  // GCC:
  RValue(*reinterpret_cast<USMALL *>(&Scalar));

  // GCC:
  LValue(reinterpret_cast<USMALL &>(Scalar));
  // GCC:
  LValue(reinterpret_cast<USMALL &>(Scalar));
  // GCC:
  LValue(*reinterpret_cast<USMALL *>(&Scalar));
}

// Level 1, 2, 3 - 1, 2, 3
void Parens() {
  // GCC: 1, 2, 3
  // level23-warning@+2{{type-punned reference breaks}}
  // level1-warning@+1{{type-punned reference might break}}
  RValue(reinterpret_cast<BIG &>((Scalar)));
  // level123-note@-1{{are not alias compatible}}

  // GCC: 1, 2, 3
  // level23-warning@+2{{type-punned pointer breaks}}
  // level1-warning@+1{{type-punned pointer might break}}
  RValue(*(reinterpret_cast<BIG *>((&(Scalar)))));
  // level123-note@-1{{are not alias compatible}}

  // GCC: 1, 2, 3
  // level23-warning@+2{{type-punned pointer breaks}}
  // level1-warning@+1{{type-punned pointer might break}}
  RValue(*(reinterpret_cast<BIG *>((&(Ary[1])))));
  // level123-note@-1{{are not alias compatible}}
}

// Clang models may_alias as a decl attribute, not a type attribute.

using MA __attribute__((may_alias)) = int;

void Frob(MA *a) {
  RValue(reinterpret_cast<short *>(a));
  RValue(*reinterpret_cast<short *>(a));
  LValue(reinterpret_cast<short &>(*a));
  RValue(reinterpret_cast<short &>(*a));
}

}

namespace std {
enum byte : unsigned char {};
enum my_byte : unsigned char {};
}

namespace ubiquitous_char {
void Frob(int a) {
  // ubiquitous char
  RValue(*reinterpret_cast<char *>(&a));
  RValue(*reinterpret_cast<unsigned char *>(&a));
  RValue(*reinterpret_cast<signed char *>(&a));
  RValue(*reinterpret_cast<std::byte *>(&a));

  // Not ubiquitous char
  // level23-warning@+2{{type-punned pointer breaks}}
  // level1-warning@+1{{type-punned pointer might break}}
  RValue(*reinterpret_cast<std::my_byte *>(&a));
  // level123-note@-1{{are not alias compatible}}
}
}

namespace record {

struct A {
  char a[4];
};

struct NotA {
  float a;
};
union U {
  char a[2];
  int b;
};
union NotU {
  int b;
  char a[2];
};

struct B {
  int a;
};

struct C {
  float a;
};

struct D : B {};

struct E;

template<typename T>
struct Wrapper {
  T t;
};

int ScalarINT;
A a;
B b;
C c;
D d;

void Frob(A *aptr, B *bptr, C *cptr) {
  // GCC: 
  RValue(reinterpret_cast<B *>(aptr));

  // GCC: 
  LValue(reinterpret_cast<B &>(*aptr));

  // GCC: 
  RValue(reinterpret_cast<A *>(bptr));

  // GCC: 
  LValue(reinterpret_cast<A &>(*bptr));

  // GCC: 1
  // level1-warning@+1{{type-punned pointer might break}}
  RValue(reinterpret_cast<B *>(cptr));
  // level1-note@-1{{are not alias compatible}}

  // GCC: 1
  // level1-warning@+1{{type-punned pointer might break}}
  RValue(*reinterpret_cast<B *>(cptr));
  // level1-note@-1{{are not alias compatible}}

  // GCC: 
  RValue(*reinterpret_cast<B *>(aptr));

  // GCC: 
  RValue(reinterpret_cast<B &>(*aptr));

  // GCC: 
  RValue(*reinterpret_cast<A *>(bptr));

  // GCC: 
  RValue(reinterpret_cast<A &>(*bptr));

  // GCC: 1
  // level1-warning@+1{{type-punned reference might break}}
  LValue(reinterpret_cast<B &>(*cptr));
  // level1-note@-1{{are not alias compatible}}

  // GCC: 1
  // level1-warning@+1{{type-punned reference might break}}
  RValue(reinterpret_cast<B &>(*cptr));
  // level1-note@-1{{are not alias compatible}}
}

void Frob(Wrapper<A> *awptr, Wrapper<C> *cwptr)
{
  // GCC: 
  RValue(reinterpret_cast<B *>(&awptr->t));

  // GCC: 
  LValue(reinterpret_cast<B &>(awptr->t));

  // GCC: 
  RValue(*reinterpret_cast<B *>(&awptr->t));

  // GCC: 
  RValue(reinterpret_cast<B &>(awptr->t));

  // GCC: 1, 2
  // level12-warning@+1{{type-punned pointer might break}}
  RValue(reinterpret_cast<B *>(&cwptr->t));
  // level12-note@-1{{are not alias compatible}}

  // GCC: 1, 2, 3
  // level123-warning@+1{{type-punned pointer might break}}
  RValue(*reinterpret_cast<B *>(&cwptr->t));
  // level123-note@-1{{are not alias compatible}}

  // GCC: 1, 2, 3
  // level12-warning@+1{{type-punned reference might break}}
  LValue(reinterpret_cast<B &>(cwptr->t));
  // level12-note@-1{{are not alias compatible}}

  // GCC: 1, 2, 3
  // level123-warning@+1{{type-punned reference might break}}
  RValue(reinterpret_cast<B &>(cwptr->t));
  // level123-note@-1{{are not alias compatible}}
}

void Frob() {
  // GCC:
  RValue(reinterpret_cast<B *>(&a));

  // GCC:
  LValue(reinterpret_cast<B &>(a));

  // GCC:
  RValue(*reinterpret_cast<B *>(&a));

  // GCC:
  RValue(reinterpret_cast<B &>(a));

  // GCC: 1, 2
  // level12-warning@+1{{type-punned pointer might break}}
  RValue(reinterpret_cast<B *>(&c));
  // level12-note@-1{{are not alias compatible}}

  // GCC: 1, 2, 3
  // level123-warning@+1{{type-punned pointer might break}}
  RValue(*reinterpret_cast<B *>(&c));
  // level123-note@-1{{are not alias compatible}}

  // GCC: 1, 2, 3
  // level12-warning@+1{{type-punned reference might break}}
  LValue(reinterpret_cast<B &>(c));
  // level12-note@-1{{are not alias compatible}}

  // GCC: 1, 2, 3
  // level123-warning@+1{{type-punned reference might break}}
  RValue(reinterpret_cast<B &>(c));
  // level123-note@-1{{are not alias compatible}}
}

void Not(Wrapper<A> * a, Wrapper<NotA> *na, Wrapper<U> *u, Wrapper<NotU> *nu) {
  // GCC: 
  RValue(reinterpret_cast<B *>(&a->t));

  // GCC: 
  RValue(reinterpret_cast<B *>(&u->t));

  // The differences below are expected as GCC considers record types containing
  // at least one aliases-all field to be alias all.  Clang's heuristic requires
  // the first field of a union or the only field of a struct to be so.
  
  // GCC: 
  // level12-warning@+1{{type-punned pointer might break}}
  RValue(reinterpret_cast<B *>(&na->t));
  // level12-note@-1{{are not alias compatible}}

  // GCC: 
  RValue(reinterpret_cast<B *>(&nu->t));
}

void Base(D d, Wrapper<D> *dw) {
  // GCC: 
  RValue(*static_cast<B *>(&d));
  // GCC: 
  RValue(*static_cast<B *>(&dw->t));

  RValue(reinterpret_cast<D *>(dw));

  // We permit inheritance, whether GCC does depends on the member types.
  // GCC: 1, 2
  RValue(*reinterpret_cast<B *>(&d));

  // GCC: 1, 2
  RValue(*reinterpret_cast<B *>(&dw->t));
}

// Clang models may_alias as a decl attribute, not a type attribute.

struct __attribute__((may_alias)) MA {
  int m;
};

void Frob(MA *a) {
  RValue(reinterpret_cast<short *>(a));
  RValue(*reinterpret_cast<short *>(a));
  LValue(reinterpret_cast<short &>(*a));
  RValue(reinterpret_cast<short &>(*a));
}

struct MM {
  int __attribute__((may_alias)) m;
};

void Frob(MM *a) {
  RValue(reinterpret_cast<short *>(a));
  RValue(*reinterpret_cast<short *>(a));
  LValue(reinterpret_cast<short &>(*a));
  RValue(reinterpret_cast<short &>(*a));
}

void Inc(E *ep, B *bp) {
  // GCC: 1
  // level1-warning@+1{{type-punned pointer to incomplete type might}}
  RValue(reinterpret_cast<E *>(bp));
  // level1-note@-1{{is incomplete}}

  // GCC:
  RValue(reinterpret_cast<B *>(ep));

  // GCC: 1, 2
  // level12-warning@+1{{type-punned pointer to incomplete type might}}
  RValue(reinterpret_cast<E *>(&b));
  // level12-note@-1{{is incomplete}}
}

}

namespace ccast {

void VarPtr(SMALL *Ptr) {
  // GCC: 1
  // level1-warning@+1{{type-punned pointer might break}}
  RValue((BIG *)(Ptr));
  // level1-note@-1{{are not alias compatible}}

  // GCC: 1
  // level1-warning@+1{{type-punned reference might break}}
  RValue((BIG &)(*Ptr));
  // level1-note@-1{{are not alias compatible}}

  // GCC:
  RValue((BIG *)((void *)(Ptr)));

  // GCC: 1
  // level1-warning@+1{{type-punned reference might break}}
  RValue((BIG &)(*Ptr));
  // level1-note@-1{{are not alias compatible}}

  // GCC: 1
  // level1-warning@+1{{type-punned pointer might break}}
  RValue(*(BIG *)(Ptr));
  // level1-note@-1{{are not alias compatible}}

  // GCC: 1
  // level1-warning@+1{{type-punned reference might break}}
  LValue((BIG &)(*Ptr));
  // level1-note@-1{{are not alias compatible}}

  // GCC: 1
  // level1-warning@+1{{type-punned pointer might break}}
  LValue(*(BIG *)(Ptr));
  // level1-note@-1{{are not alias compatible}}
}

void VarRef(SMALL &Ref) {
  // GCC: 1
  // level1-warning@+1{{type-punned pointer might break}}
  RValue((BIG *)(&Ref));
  // level1-note@-1{{are not alias compatible}}

  // GCC: 1
  // level1-warning@+1{{type-punned reference might break}}
  RValue((BIG &)(Ref));
  // level1-note@-1{{are not alias compatible}}

  // GCC:
  RValue((BIG *)((void *)(&Ref)));

  // GCC: 1
  // level1-warning@+1{{type-punned reference might break}}
  RValue((BIG &)(Ref));
  // level1-note@-1{{are not alias compatible}}

  // GCC: 1
  // level1-warning@+1{{type-punned pointer might break}}
  RValue(*(BIG *)(&Ref));
  // level1-note@-1{{are not alias compatible}}

  // GCC: 1
  // level1-warning@+1{{type-punned reference might break}}
  LValue((BIG &)(Ref));
  // level1-note@-1{{are not alias compatible}}

  // GCC: 1
  // level1-warning@+1{{type-punned reference might break}}
  LValue((BIG &)(Ref));
  // level1-note@-1{{are not alias compatible}}

  // GCC: 1
  // level1-warning@+1{{type-punned pointer might break}}
  LValue(*(BIG *)(&Ref));
  // level1-note@-1{{are not alias compatible}}
}

void Object() {
  // GCC: 1, 2
  // level2-warning@+2{{type-punned pointer breaks}}
  // level1-warning@+1{{type-punned pointer might break}}
  RValue((BIG *)(&Scalar));
  // level12-note@-1{{are not alias compatible}}

  // GCC: 1, 2, 3
  // level23-warning@+2{{type-punned reference breaks}}
  // level1-warning@+1{{type-punned reference might break}}
  RValue((BIG &)(Scalar));
  // level123-note@-1{{are not alias compatible}}

  // GCC:
  RValue((BIG *)((void *)(&Scalar)));

  // GCC: 1, 2, 3
  // level23-warning@+2{{type-punned reference breaks}}
  // level1-warning@+1{{type-punned reference might break}}
  RValue((BIG &)(Scalar));
  // level123-note@-1{{are not alias compatible}}

  // GCC: 1, 2, 3
  // level23-warning@+2{{type-punned pointer breaks}}
  // level1-warning@+1{{type-punned pointer might break}}
  RValue(*(BIG *)(&Scalar));
  // level123-note@-1{{are not alias compatible}}

  // GCC: 1, 2, 3
  // level2-warning@+2{{type-punned reference breaks}}
  // level1-warning@+1{{type-punned reference might break}}
  LValue((BIG &)(Scalar));
  // level12-note@-1{{are not alias compatible}}
  
  // GCC: 1, 2, 3
  // level23-warning@+2{{type-punned pointer breaks}}
  // level1-warning@+1{{type-punned pointer might break}}
  LValue(*(BIG *)(&Scalar));
  // level123-note@-1{{are not alias compatible}}
}

// Level 1, 2, 3 - 1, 2, 3
void DetectedVariants() {
  // GCC: 1, 2, 3
  // level23-warning@+2{{type-punned reference breaks}}
  // level1-warning@+1{{type-punned reference might break}}
  RValue((BIG &)(Ary[1]));
  // level123-note@-1{{are not alias compatible}}

  // GCC: 1, 2, 3
  // level23-warning@+2{{type-punned pointer breaks}}
  // level1-warning@+1{{type-punned pointer might break}}
  RValue(*(BIG *)(&Ary[1]));
  // level123-note@-1{{are not alias compatible}}

  // GCC: 1, 2, 3
  // level23-warning@+2{{type-punned reference breaks}}
  // level1-warning@+1{{type-punned reference might break}}
  RValue((BIG &)(Struct.m));
  // level123-note@-1{{are not alias compatible}}

  // GCC: 1, 2, 3
  // level23-warning@+2{{type-punned pointer breaks}}
  // level1-warning@+1{{type-punned pointer might break}}
  RValue(*(BIG *)(&Struct.m));
  // level123-note@-1{{are not alias compatible}}

  // GCC: 1, 2, 3
  // level23-warning@+2{{type-punned reference breaks}}
  // level1-warning@+1{{type-punned reference might break}}
  RValue((BIG &)((&Struct)->m));
  // level123-note@-1{{are not alias compatible}}

  // GCC: 1, 2, 3
  // level23-warning@+2{{type-punned pointer breaks}}
  // level1-warning@+1{{type-punned pointer might break}}
  RValue(*(BIG *)(&(&Struct)->m));
  // level123-note@-1{{are not alias compatible}}

  // GCC: 1, 2, 3
  // level23-warning@+2{{type-punned reference breaks}}
  // level1-warning@+1{{type-punned reference might break}}
  RValue((BIG &)(__real__(CPLx)));
  // level123-note@-1{{are not alias compatible}}

  // GCC: 1, 2, 3
  // level23-warning@+2{{type-punned pointer breaks}}
  // level1-warning@+1{{type-punned pointer might break}}
  RValue(*(BIG *)(&__real__(CPLx)));
  // level123-note@-1{{are not alias compatible}}

  // GCC: 1, 2, 3
  // level23-warning@+2{{type-punned reference breaks}}
  // level1-warning@+1{{type-punned reference might break}}
  RValue((BIG &)(__imag__(CPLx)));
  // level123-note@-1{{are not alias compatible}}

  // GCC: 1, 2, 3
  // level23-warning@+2{{type-punned pointer breaks}}
  // level1-warning@+1{{type-punned pointer might break}}
  RValue(*(BIG *)(&__imag__(CPLx)));
  // level123-note@-1{{are not alias compatible}}
}

void Ok() {
  // GCC:
  RValue((USMALL *)(&Scalar));
  // GCC:
  RValue((USMALL &)(Scalar));
  // GCC:
  RValue((USMALL *)((void *)(&Scalar)));
  // GCC:
  RValue((USMALL &)(Scalar));
  // GCC:
  RValue(*(USMALL *)(&Scalar));

  // GCC:
  LValue((USMALL &)(Scalar));
  // GCC:
  LValue((USMALL &)(Scalar));
  // GCC:
  LValue(*(USMALL *)(&Scalar));
}

// Level 1, 2, 3 - 1, 2, 3
void Parens() {
  // GCC: 1, 2, 3
  // level23-warning@+2{{type-punned reference breaks}}
  // level1-warning@+1{{type-punned reference might break}}
  RValue((BIG &)((Scalar)));
  // level123-note@-1{{are not alias compatible}}

  // GCC: 1, 2, 3
  // level23-warning@+2{{type-punned pointer breaks}}
  // level1-warning@+1{{type-punned pointer might break}}
  RValue(*((BIG *)((&(Scalar)))));
  // level123-note@-1{{are not alias compatible}}

  // GCC: 1, 2, 3
  // level23-warning@+2{{type-punned pointer breaks}}
  // level1-warning@+1{{type-punned pointer might break}}
  RValue(*((BIG *)((&(Ary[1])))));
  // level123-note@-1{{are not alias compatible}}
}

// Clang models may_alias as a decl attribute, not a type attribute.

using MA __attribute__((may_alias)) = int;

void Frob(MA *a) {
  RValue((short *)(a));
  RValue(*(short *)(a));
  LValue((short &)(*a));
  RValue((short &)(*a));
}

}

namespace fcast {

// Functional casts, GCC does not get any of these -- comments are for
// c casts, which are functionally the same.

using BIGPtr = BIG *;
using BIGRef = BIG &;
using VoidPtr = void *;

void VarPtr(int *Ptr) {
  // GCC: 1
  // level1-warning@+1{{type-punned pointer might break}}
  RValue(BIGPtr(Ptr));
  // level1-note@-1{{are not alias compatible}}

  // GCC: 1
  // level1-warning@+1{{type-punned reference might break}}
  RValue(BIGRef(*Ptr));
  // level1-note@-1{{are not alias compatible}}

  // GCC:
  RValue(BIGPtr(VoidPtr(Ptr)));

  // GCC: 1
  // level1-warning@+1{{type-punned reference might break}}
  RValue(BIGRef(*Ptr));
  // level1-note@-1{{are not alias compatible}}

  // GCC: 1
  // level1-warning@+1{{type-punned pointer might break}}
  RValue(*BIGPtr(Ptr));
  // level1-note@-1{{are not alias compatible}}

  // GCC: 1
  // level1-warning@+1{{type-punned reference might break}}
  LValue(BIGRef(*Ptr));
  // level1-note@-1{{are not alias compatible}}

  // GCC: 1
  // level1-warning@+1{{type-punned pointer might break}}
  LValue(*BIGPtr(Ptr));
  // level1-note@-1{{are not alias compatible}}
}

void VarRef(int &Ref) {
  // GCC: 1
  // level1-warning@+1{{type-punned pointer might break}}
  RValue(BIGPtr(&Ref));
  // level1-note@-1{{are not alias compatible}}

  // GCC: 1
  // level1-warning@+1{{type-punned reference might break}}
  RValue(BIGRef(Ref));
  // level1-note@-1{{are not alias compatible}}

  // GCC:
  RValue(BIGPtr(VoidPtr(&Ref)));

  // GCC: 1
  // level1-warning@+1{{type-punned reference might break}}
  RValue(BIGRef(Ref));
  // level1-note@-1{{are not alias compatible}}

  // GCC: 1
  // level1-warning@+1{{type-punned pointer might break}}
  RValue(*BIGPtr(&Ref));
  // level1-note@-1{{are not alias compatible}}

  // GCC: 1
  // level1-warning@+1{{type-punned reference might break}}
  LValue(BIGRef(Ref));
  // level1-note@-1{{are not alias compatible}}

  // GCC: 1
  // level1-warning@+1{{type-punned reference might break}}
  LValue(BIGRef(Ref));
  // level1-note@-1{{are not alias compatible}}

  // GCC: 1
  // level1-warning@+1{{type-punned pointer might break}}
  LValue(*BIGPtr(&Ref));
  // level1-note@-1{{are not alias compatible}}
}

void Object() {
  // GCC: 1, 2
  // level2-warning@+2{{type-punned pointer breaks}}
  // level1-warning@+1{{type-punned pointer might break}}
  RValue(BIGPtr(&Scalar));
  // level12-note@-1{{are not alias compatible}}

  // GCC: 1, 2, 3
  // level23-warning@+2{{type-punned reference breaks}}
  // level1-warning@+1{{type-punned reference might break}}
  RValue(BIGRef(Scalar));
  // level123-note@-1{{are not alias compatible}}

  // GCC:
  RValue(BIGPtr(VoidPtr(&Scalar)));

  // GCC: 1, 2, 3
  // level23-warning@+2{{type-punned reference breaks}}
  // level1-warning@+1{{type-punned reference might break}}
  RValue(BIGRef(Scalar));
  // level123-note@-1{{are not alias compatible}}

  // GCC: 1, 2, 3
  // level23-warning@+2{{type-punned pointer breaks}}
  // level1-warning@+1{{type-punned pointer might break}}
  RValue(*BIGPtr(&Scalar));
  // level123-note@-1{{are not alias compatible}}

  // GCC: 1, 2, 3
  // level2-warning@+2{{type-punned reference breaks}}
  // level1-warning@+1{{type-punned reference might break}}
  LValue(BIGRef(Scalar));
  // level12-note@-1{{are not alias compatible}}

  // GCC: 1, 2, 3
  // level23-warning@+2{{type-punned pointer breaks}}
  // level1-warning@+1{{type-punned pointer might break}}
  LValue(*BIGPtr(&Scalar));
  // level123-note@-1{{are not alias compatible}}
}

// Level 1, 2, 3 - 1, 2, 3
void DetectedVariants() {
  // GCC: 1, 2, 3
  // level23-warning@+2{{type-punned reference breaks}}
  // level1-warning@+1{{type-punned reference might break}}
  RValue(BIGRef(Ary[1]));
  // level123-note@-1{{are not alias compatible}}

  // GCC: 1, 2, 3
  // level23-warning@+2{{type-punned pointer breaks}}
  // level1-warning@+1{{type-punned pointer might break}}
  RValue(*BIGPtr(&Ary[1]));
  // level123-note@-1{{are not alias compatible}}

  // GCC: 1, 2, 3
  // level23-warning@+2{{type-punned reference breaks}}
  // level1-warning@+1{{type-punned reference might break}}
  RValue(BIGRef(Struct.m));
  // level123-note@-1{{are not alias compatible}}

  // GCC: 1, 2, 3
  // level23-warning@+2{{type-punned pointer breaks}}
  // level1-warning@+1{{type-punned pointer might break}}
  RValue(*BIGPtr(&Struct.m));
  // level123-note@-1{{are not alias compatible}}

  // GCC: 1, 2, 3
  // level23-warning@+2{{type-punned reference breaks}}
  // level1-warning@+1{{type-punned reference might break}}
  RValue(BIGRef((&Struct)->m));
  // level123-note@-1{{are not alias compatible}}

  // GCC: 1, 2, 3
  // level23-warning@+2{{type-punned pointer breaks}}
  // level1-warning@+1{{type-punned pointer might break}}
  RValue(*BIGPtr(&(&Struct)->m));
  // level123-note@-1{{are not alias compatible}}

  // GCC: 1, 2, 3
  // level23-warning@+2{{type-punned reference breaks}}
  // level1-warning@+1{{type-punned reference might break}}
  RValue(BIGRef(__real__(CPLx)));
  // level123-note@-1{{are not alias compatible}}

  // GCC: 1, 2, 3
  // level23-warning@+2{{type-punned pointer breaks}}
  // level1-warning@+1{{type-punned pointer might break}}
  RValue(*BIGPtr(&__real__(CPLx)));
  // level123-note@-1{{are not alias compatible}}

  // GCC: 1, 2, 3
  // level23-warning@+2{{type-punned reference breaks}}
  // level1-warning@+1{{type-punned reference might break}}
  RValue(BIGRef(__imag__(CPLx)));
  // level123-note@-1{{are not alias compatible}}

  // GCC: 1, 2, 3
  // level23-warning@+2{{type-punned pointer breaks}}
  // level1-warning@+1{{type-punned pointer might break}}
  RValue(*BIGPtr(&__imag__(CPLx)));
  // level123-note@-1{{are not alias compatible}}
}

using UnsignedBIGPtr = USMALL *;
using UnsignedBIGRef = USMALL &;

void Ok() {
  // GCC:
  RValue(UnsignedBIGPtr(&Scalar));
  // GCC:
  RValue(UnsignedBIGRef(Scalar));
  // GCC:
  RValue(UnsignedBIGPtr(VoidPtr(&Scalar)));
  // GCC:
  RValue(UnsignedBIGRef(Scalar));
  // GCC:
  RValue(*UnsignedBIGPtr(&Scalar));

  // GCC:
  LValue(UnsignedBIGRef(Scalar));
  // GCC:
  LValue(UnsignedBIGRef(Scalar));
  // GCC:
  LValue(*UnsignedBIGPtr(&Scalar));
}

// Level 1, 2, 3 - 1, 2, 3
void Parens() {
  // GCC: 1, 2, 3
  // level23-warning@+2{{type-punned reference breaks}}
  // level1-warning@+1{{type-punned reference might break}}
  RValue(BIGRef((Scalar)));
  // level123-note@-1{{are not alias compatible}}

  // GCC: 1, 2, 3
  // level23-warning@+2{{type-punned pointer breaks}}
  // level1-warning@+1{{type-punned pointer might break}}
  RValue(*(BIGPtr((&(Scalar)))));
  // level123-note@-1{{are not alias compatible}}

  // GCC: 1, 2, 3
  // level23-warning@+2{{type-punned pointer breaks}}
  // level1-warning@+1{{type-punned pointer might break}}
  RValue(*(BIGPtr((&(Ary[1])))));
  // level123-note@-1{{are not alias compatible}}
}

// Clang models may_alias as a decl attribute, not a type attribute.

using MA __attribute__((may_alias)) = int;

using ShortPtr = short *;
using ShortRef = short &;

void Frob(MA *a) {
  RValue(ShortPtr(a));
  RValue(*ShortPtr(a));
  LValue(ShortRef(*a));
  RValue(ShortRef(*a));
}

}

namespace puns {

// quiet-no-diagnostics

void Foo () {
  float d = 0.0;
  int i[2];

  // level23-warning@+2{{type-punned pointer breaks}}
  // level1-warning@+1{{type-punned pointer might break}}
  i[0] = reinterpret_cast<int *>(&d)[0];
  // level123-note@-1{{are not alias compatible}}

  // level23-warning@+2{{type-punned reference breaks}}
  // level1-warning@+1{{type-punned reference might break}}
  i[0] = reinterpret_cast<int (&)[2]>(d)[0];
  // level123-note@-1{{are not alias compatible}}

  // level23-warning@+2{{type-punned reference breaks}}
  // level1-warning@+1{{type-punned reference might break}}
  i[0] = reinterpret_cast<int &>(d);
  // level123-note@-1{{are not alias compatible}}

  // level23-warning@+2{{type-punned pointer breaks}}
  // level1-warning@+1{{type-punned pointer might break}}
  reinterpret_cast<int *>(&d)[0] = 1;
  // level123-note@-1{{are not alias compatible}}

  // level23-warning@+2{{type-punned reference breaks}}
  // level1-warning@+1{{type-punned reference might break}}
  reinterpret_cast<int (&)[2]>(d)[0] = 1;
  // level123-note@-1{{are not alias compatible}}

  // level23-warning@+2{{type-punned reference breaks}}
  // level1-warning@+1{{type-punned reference might break}}
  reinterpret_cast<int &>(d) = 1;
  // level123-note@-1{{are not alias compatible}}

  // level23-warning@+2{{type-punned pointer breaks}}
  // level1-warning@+1{{type-punned pointer might break}}
  ++*reinterpret_cast<int *>(&d);
  // level123-note@-1{{are not alias compatible}}

  // level23-warning@+2{{type-punned pointer breaks}}
  // level1-warning@+1{{type-punned pointer might break}}
  --*reinterpret_cast<int *>(&d);
  // level123-note@-1{{are not alias compatible}}

  // level23-warning@+2{{type-punned pointer breaks}}
  // level1-warning@+1{{type-punned pointer might break}}
  (*reinterpret_cast<int *>(&d))++;
  // level123-note@-1{{are not alias compatible}}

  // level23-warning@+2{{type-punned pointer breaks}}
  // level1-warning@+1{{type-punned pointer might break}}
  (*reinterpret_cast<int *>(&d))--;
  // level123-note@-1{{are not alias compatible}}

  // level23-warning@+2{{type-punned pointer breaks}}
  // level1-warning@+1{{type-punned pointer might break}}
  *reinterpret_cast<int *>(&d) += 1;
  // level123-note@-1{{are not alias compatible}}

  // level23-warning@+2{{type-punned pointer breaks}}
  // level1-warning@+1{{type-punned pointer might break}}
  *reinterpret_cast<int *>(&d) -= 1;
  // level123-note@-1{{are not alias compatible}}
  
  // level23-warning@+2{{type-punned reference breaks}}
  // level1-warning@+1{{type-punned reference might break}}
  ++reinterpret_cast<int &>(d);
  // level123-note@-1{{are not alias compatible}}

  // level23-warning@+2{{type-punned reference breaks}}
  // level1-warning@+1{{type-punned reference might break}}
  --reinterpret_cast<int &>(d);
  // level123-note@-1{{are not alias compatible}}

  // level23-warning@+2{{type-punned reference breaks}}
  // level1-warning@+1{{type-punned reference might break}}
  reinterpret_cast<int &>(d)++;
  // level123-note@-1{{are not alias compatible}}

  // level23-warning@+2{{type-punned reference breaks}}
  // level1-warning@+1{{type-punned reference might break}}
  reinterpret_cast<int &>(d)--;
  // level123-note@-1{{are not alias compatible}}

  // level23-warning@+2{{type-punned reference breaks}}
  // level1-warning@+1{{type-punned reference might break}}
  reinterpret_cast<int &>(d) += 1;
  // level123-note@-1{{are not alias compatible}}

  // level23-warning@+2{{type-punned reference breaks}}
  // level1-warning@+1{{type-punned reference might break}}
  reinterpret_cast<int &>(d) -= 1;
  // level123-note@-1{{are not alias compatible}}

  // level23-warning@+2{{type-punned pointer breaks}}
  // level1-warning@+1{{type-punned pointer might break}}
  d = reinterpret_cast<float *>(&i)[0];
  // level123-note@-1{{are not alias compatible}}

  // level23-warning@+2{{type-punned pointer breaks}}
  // level1-warning@+1{{type-punned pointer might break}}
  d = *reinterpret_cast<float *>(&i[0]);
  // level123-note@-1{{are not alias compatible}}

  // level23-warning@+2{{type-punned pointer breaks}}
  // level1-warning@+1{{type-punned pointer might break}}
  d = *reinterpret_cast<float *>(i);
  // level123-note@-1{{are not alias compatible}}

  // level23-warning@+2{{type-punned pointer breaks}}
  // level1-warning@+1{{type-punned pointer might break}}
  d = *reinterpret_cast<float *>(&i[0]);
  // level123-note@-1{{are not alias compatible}}

  // level23-warning@+2{{type-punned reference breaks}}
  // level1-warning@+1{{type-punned reference might break}}
  d = reinterpret_cast<float &>(i);
  // level123-note@-1{{are not alias compatible}}

  // level23-warning@+2{{type-punned reference breaks}}
  // level1-warning@+1{{type-punned reference might break}}
  d = reinterpret_cast<float &>(i[0]);
  // level123-note@-1{{are not alias compatible}}

  // level23-warning@+2{{type-punned reference breaks}}
  // level1-warning@+1{{type-punned reference might break}}
  d = reinterpret_cast<float &>(i);
  // level123-note@-1{{are not alias compatible}}

  // level23-warning@+2{{type-punned reference breaks}}
  // level1-warning@+1{{type-punned reference might break}}
  d = reinterpret_cast<float &>(i[0]);
  // level123-note@-1{{are not alias compatible}}

  reinterpret_cast<Struct_t &>(i).m = 1;
  reinterpret_cast<Struct_t *>(&i)->m = 1;

  // level123-warning@+1{{type-punned reference might break}}
  reinterpret_cast<Struct2_t &>(i).m = 1;
  // level123-note@-1{{are not alias compatible}}

  // level123-warning@+1{{type-punned pointer might break}}
  reinterpret_cast<Struct2_t *>(&i)->m = 1;
  // level123-note@-1{{are not alias compatible}}
}


void pr50066() {
  double d = 2.34;
  int i[2];

  RValue(d);

  // GCC: 1, 2, 3
  // level23-warning@+2{{type-punned pointer breaks}}
  // level1-warning@+1{{type-punned pointer might break}}
  *(BIG *)i =
  // level123-note@-1{{are not alias compatible}}

      // GCC: 1, 2, 3
      // level23-warning@+2{{type-punned pointer breaks}}
      // level1-warning@+1{{type-punned pointer might break}}
      *(BIG *)&d;
      // level123-note@-1{{are not alias compatible}}
  
  // GCC: 1, 2, 3
  // level23-warning@+2{{type-punned pointer breaks}}
  // level1-warning@+1{{type-punned pointer might break}}
  ((int *)&d)[0] = i[0];
  // level123-note@-1{{are not alias compatible}}

  // GCC: 1, 2
  // GCC misses this at level 3, because it represents (cast)[0] as
  // *(cast), but (cast)[1] as *((cast) + 1), which doesn't trigger the
  // indirection case that levl 3 requires.

  // level23-warning@+2{{type-punned pointer breaks}}
  // level1-warning@+1{{type-punned pointer might break}}
  ((int *)&d)[1] = i[1];
  // level123-note@-1{{are not alias compatible}}

  RValue(d);

  ((char *)&d)[2] += 123;

  RValue(d);
}
}

namespace tpl {

int vi;
float vf;

template<typename T>
void Frob0(int *a) {
  // GCC 1
  // level1-warning@+1 2{{type-punned pointer might break}}
  RValue(*reinterpret_cast<float *>(a));
  // level1-note@-1 2{{are not alias compatible}}

  // GCC 1
  // level1-warning@+1 2{{type-punned pointer might break}}
  RValue(*reinterpret_cast<float *>(static_cast<void *>(a)));
  // level1-note@-1 2{{are not alias compatible}}
}

template<typename T>
void Frob0() {
  // GCC 1, 2, 3
  // level23-warning@+2{{type-punned pointer breaks}}
  // level1-warning@+1{{type-punned pointer might break}}
  RValue(*reinterpret_cast<int *>(&vf));
  // level123-note@-1{{are not alias compatible}}

  // GCC 1, 2, 3
  // level2-warning@+2 2{{type-punned pointer breaks}}
  // level1-warning@+1 2{{type-punned pointer might break}}
  RValue(*reinterpret_cast<int *>(static_cast<void *>(&vf)));
  // level12-note@-1 2{{are not alias compatible}}
}

template<typename T>
void Frob1(T *b) {
  // GCC 1
  // level1-warning@+1{{type-punned pointer might break}}
  RValue(*reinterpret_cast<float *>(b));
  // level1-note@-1{{are not alias compatible}}

  // GCC 1
  // level1-warning@+1{{type-punned pointer might break}}
  RValue(*reinterpret_cast<float *>(static_cast<void *>(b)));
  // level1-note@-1{{are not alias compatible}}
}

template<typename T>
void Frob1() {
  // GCC 1, 2, 3
  // level23-warning@+2{{type-punned pointer breaks}}
  // level1-warning@+1{{type-punned pointer might break}}
  RValue(*reinterpret_cast<T *>(&vf));
  // level123-note@-1{{are not alias compatible}}

  // GCC 1, 2, 3
  // level2-warning@+2{{type-punned pointer breaks}}
  // level1-warning@+1{{type-punned pointer might break}}
  RValue(*reinterpret_cast<T *>(static_cast<void *>(&vf)));
  // level12-note@-1{{are not alias compatible}}
}

void Call(){
  int i;
  // level1-note@+1{{'tpl::Frob0<float>' requested here}}
  Frob0<float>(&i);
  // level12-note@+1{{'tpl::Frob0<float>' requested here}}
  Frob0<float>();

  float f;
  Frob1(&f);
  Frob1<float>();

  SMALL l;
  // level1-note-re@+1{{'tpl::Frob1<{{.*}}>' requested here}}
  Frob1(&l);
  // level123-note-re@+1{{'tpl::Frob1<{{.*}}>' requested here}}
  Frob1<SMALL>();
}

}

namespace embedding {
// Check embedding as first member is accepted

struct Ints { int a, b; };
struct Floats { float a, b; };
struct Other { short a, b, c, d; };

template<typename T>
struct Wrapper { T m; void *other;};

template<typename T>
struct Allocator { alignas(T) char buffer[sizeof(T)]; };

union Union {
  void *random;
  Ints ints;
  Floats floats;
};

template<typename T>
struct NotWrapper { void *other; T m; };

void FromContainer (Wrapper<Ints> wrapint,
                    Allocator<Ints> allocint,
                    Union u,
                    NotWrapper<Ints> notwrapint) {

  RValue (*reinterpret_cast<Ints *> (&wrapint));
  RValue (*reinterpret_cast<Ints *> (&allocint));
  RValue (*reinterpret_cast<Ints *> (&u));
  RValue (*reinterpret_cast<Floats *> (&u));

  // level123-warning@+1{{type-punned pointer might break}}
  RValue (*reinterpret_cast<Ints *> (&notwrapint));
  // level123-note@-1{{are not alias compatible}}
}

void ToContainer (Ints ints, Floats floats, Other others) {
  RValue (*reinterpret_cast<Wrapper<Ints> *>(&ints));

  // level123-warning@+1{{type-punned pointer to incomplete type might break}}
  RValue (*reinterpret_cast<Wrapper<Floats> *>(&floats));
  // level123-note@-1{{is incomplete}}

  // This second cast occurs after Wrapper<Floats> has been instantiated, so no
  // warning.
  RValue (*reinterpret_cast<Wrapper<Floats> *>(&floats));

  RValue (*reinterpret_cast<Allocator<Ints> *>(&ints));
  RValue (*reinterpret_cast<Union *>(&ints));
  RValue (*reinterpret_cast<Union *>(&floats));

  // level123-warning@+1{{type-punned pointer might break}}
  RValue (*reinterpret_cast<Wrapper<Floats> *>(&ints));
  // level123-note@-1{{are not alias compatible}}

  // level123-warning@+1{{type-punned pointer might break}}
  RValue (*reinterpret_cast<NotWrapper<Ints> *>(&ints));
  // level123-note@-1{{are not alias compatible}}

  // level123-warning@+1{{type-punned pointer might break}}
  RValue (*reinterpret_cast<Union *>(&others));
  // level123-note@-1{{are not alias compatible}}
}

struct Inner { int m; };
struct Outer1 { Inner i; };
struct Outer2 { Outer1 o; };
struct Inherit1 : Inner {};
struct Inherit2 : Inherit1 {};

void Inherit() {
  // Check we see through multiple levels
  Inner i;
  Outer2 o;
  Inherit2 i2;
  int in;

  // Containership is ok
  RValue(*reinterpret_cast<Outer2 *>(&i));
  RValue(*reinterpret_cast<Inner *>(&o.o));
  RValue(*reinterpret_cast<Inner *>(&o));
  RValue(*reinterpret_cast<Outer2 *>(&in));
  RValue(*reinterpret_cast<int *>(&o));

  // Inheritance is also ok -- but why not static cast here?
  RValue(*reinterpret_cast<Inherit2 *>(&i));
  RValue(*reinterpret_cast<Inner *>(&i2));
  RValue(*reinterpret_cast<Inherit2 *>(&in));
  RValue(*reinterpret_cast<int *>(&i2));
}

}
