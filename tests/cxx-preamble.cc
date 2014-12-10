#include <iostream>

#define MGL_API_FUNC static
#define MGL_PROTO static
#define MGL_FUNC static

static void put_int(int x) {
  std::cout << x << std::endl;
}

static int putting_int(int x) {
  put_int(x);
  return x;
}

static int inc(int x) {
  return x + 1;
}

static bool is_non_zero(int x) {
  return x != 0;
}
