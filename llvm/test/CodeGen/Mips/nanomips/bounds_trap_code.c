// RUN: clang -O %s -S -fsanitize=bounds -fsanitize-trap=all -o - | FileCheck --check-prefix=DEFAULT %s
// RUN: clang -O %s -S -fsanitize=bounds -fsanitize-trap=all -mllvm -nmips-ubsantrap-sankind-codes -o - | FileCheck --check-prefix=SANKIND %s
int x[10];
int get(int i) {
  // DEFAULT: break 0x1e
  // SANKIND: break 0x12
  return x[i];
}
