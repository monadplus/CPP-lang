bool foo() {
  bool b = true;
  (b == b);
}

int main() {
  bool b = foo();
  printBool(b);
}
