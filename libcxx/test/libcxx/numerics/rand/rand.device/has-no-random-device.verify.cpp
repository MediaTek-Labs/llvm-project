//===----------------------------------------------------------------------===//
//
// Part of the LLVM Project, under the Apache License v2.0 with LLVM Exceptions.
// See https://llvm.org/LICENSE.txt for license information.
// SPDX-License-Identifier: Apache-2.0 WITH LLVM-exception
//
//===----------------------------------------------------------------------===//

// Make sure that std::random_device is not available in namespace std:: when
// libc++ is built without support for random device.

// REQUIRES: no-random-device

// NanoMips does not have a functional random-device but declares a
// wrapper over rand(), so the type is indeed present in the namespace.
// XFAIL: target=nanomips-elf

#include <random>

void f() {
  std::random_device d; // expected-error {{no type named 'random_device' in namespace 'std'}}
}
