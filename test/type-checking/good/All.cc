int foo(double x, bool b) {
  int res = 0;
  while (x > 0.0) {
    x = x / 10.0;
    res = res + 1;
  }
  if (b) ++res; else --res;

  return res;
}

int main() {
  int x,y,z;

  bool b = (1 < 2) == true || false;

  if (b) {
    int x = 12;
    y = x;
    z = y;
  } else {
    y = 1000;
    while (y > 0) {
      z = y;
      --y;
    }

    return y;
  }

  z = foo(x*2.0, y != 0);

  return 0;
}
