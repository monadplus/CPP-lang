int fib(int n) {
  int a, b, c, i;
  a = 0;
  b = 1;
  if (n == 0)
    return a;
  i = 2;
  while (i <= n) {
    c = a + b;
    a = b;
    b = c;
    i++;
  }
  return b;
}

int main() {
  int n = readInt();
  printInt(fib(n));

  return 0;
}
