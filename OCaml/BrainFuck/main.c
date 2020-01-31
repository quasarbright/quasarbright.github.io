#include <stdio.h>
#include <inttypes.h>

extern int our_code_starts_here() asm("our_code_starts_here");

int main(int argc, char** argv) {
  // printf("\nvalue under head: %d\n", our_code_starts_here());
  our_code_starts_here();
  printf("\n");
  return 0;
}

extern void output(u_int64_t value) {
  int value_int = value;
  char value_char = value_int;
  // printf("%" PRId64 "", value);
  // printf("%d\n", value_int);
  printf("%c", value_char);
}

extern u_int64_t input() {
  char c;
  scanf(" %c", &c);
  int c_int = c;
  u_int64_t c_int64 = c_int;
  return c_int64;
}