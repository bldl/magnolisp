#include <iostream>
#include <vector>

#define MGL_API_FUNC static
#define MGL_PROTO static
#define MGL_FUNC static

// --------------------------------------------------
// integers
// --------------------------------------------------

static void put_int(int x) {
  std::cout << x << std::endl;
}

static int putting_int(int x) {
  put_int(x);
  return x;
}

static void put_long(long x) {
  std::cout << x << std::endl;
}

static int inc(int x) {
  return x + 1;
}

static bool is_non_zero(int x) {
  return x != 0;
}

static long to_long(int x) {
  return x;
}

static int int_add(int x, int y) {
  return x + y;
}

// --------------------------------------------------
// lists
// --------------------------------------------------

typedef std::vector<int> IntList;

static bool is_IntList_empty(IntList const& lst) {
  return lst.empty();
}

static int IntList_head(IntList const& lst) {
  return lst.front();
}

static IntList IntList_tail(IntList const& lst) { 
  return IntList(lst.begin()+1, lst.end());
}

static IntList IntList_push_back(IntList const& lst, int x) {
  IntList tmp(lst);
  tmp.push_back(x);
  return tmp;
}

static IntList IntList_new() {
  return IntList();
}
