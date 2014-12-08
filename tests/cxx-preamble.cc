#include <iostream>

#define MGL_API_FUNC static
#define MGL_PROTO static
#define MGL_FUNC static

static int put_int(int x) {
  std::cout << x << std::endl;
  return x;
}

static int inc(int x) {
  return x + 1;
}
