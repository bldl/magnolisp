#include <iostream>

#define MGL_API_FUNC static
#define MGL_PROTO static
#define MGL_FUNC static

static void put_int(int const& x) {
  std::cout << x << std::endl;
}

static int putting_int(int const& x) {
  put_int(x);
  return x;
}

static void put_long(long const& x) {
  std::cout << x << std::endl;
}

static int inc(int const& x) {
  return x + 1;
}

static bool is_non_zero(int const& x) {
  return x != 0;
}

static long to_long(int const& x) {
  return x;
}
