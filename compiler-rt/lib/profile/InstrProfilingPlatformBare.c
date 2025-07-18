/*===- InstrProfilingPlatformBare.c - Profile data default platform ------===*\
|*
|* Part of the LLVM Project, under the Apache License v2.0 with LLVM Exceptions.
|* See https://llvm.org/LICENSE.txt for license information.
|* SPDX-License-Identifier: Apache-2.0 WITH LLVM-exception
|*
|*===----------------------------------------------------------------------===*
|* This sets up interfaces for any bare-metal target without relying on
|* run-time registration or tracking. It requires __llvm_prf_* symbols to
|* be defined by the linker scatter file around profile-related sections.
\*===----------------------------------------------------------------------===*/


#if defined(__nanomips__)

#include <stdlib.h>
#include <stdio.h>

#include "InstrProfiling.h"
#include "InstrProfilingInternal.h"

extern const __llvm_profile_data __llvm_prf_data_start;
extern const __llvm_profile_data __llvm_prf_data_end;
extern const char __llvm_prf_names_start;
extern const char __llvm_prf_names_end;
extern char __llvm_prf_cnts_start;
extern char __llvm_prf_cnts_end;

static uint32_t *OrderFileFirst = NULL;

COMPILER_RT_VISIBILITY
const __llvm_profile_data *__llvm_profile_begin_data(void) { return &__llvm_prf_data_start; }
COMPILER_RT_VISIBILITY
const __llvm_profile_data *__llvm_profile_end_data(void) { return &__llvm_prf_data_end; }
COMPILER_RT_VISIBILITY
const char *__llvm_profile_begin_names(void) { return &__llvm_prf_names_start; }
COMPILER_RT_VISIBILITY
const char *__llvm_profile_end_names(void) { return &__llvm_prf_names_end; }
COMPILER_RT_VISIBILITY
char *__llvm_profile_begin_counters(void) { return &__llvm_prf_cnts_start; }
COMPILER_RT_VISIBILITY
char *__llvm_profile_end_counters(void) { return &__llvm_prf_cnts_end; }

COMPILER_RT_VISIBILITY
uint32_t *__llvm_profile_begin_orderfile(void) { return OrderFileFirst; }

COMPILER_RT_VISIBILITY
ValueProfNode *__llvm_profile_begin_vnodes(void) {
  return 0;
}
COMPILER_RT_VISIBILITY
ValueProfNode *__llvm_profile_end_vnodes(void) { return 0; }

COMPILER_RT_VISIBILITY ValueProfNode *CurrentVNode = 0;
COMPILER_RT_VISIBILITY ValueProfNode *EndVNode = 0;

COMPILER_RT_VISIBILITY int __llvm_write_binary_ids(ProfDataWriter *Writer) {
  return 0;
}

#endif /* __nanomips__ */
