// RUN: clang %s -S -O3  -fsanitize-trap -fsanitize=signed-integer-overflow \
// RUN:   -mllvm -enable-nmips-addsubtrap=1
int generate_p(int max_counter, int n, unsigned buf_len) {
  int i, j, k;
  for (i = 0 ; i <= max_counter; i++){
    for (j =0; j <= n; j++) {
      for (k = (int)buf_len -1; k >= 0; k--) {
      }
    }
  }
  return -1;
}
