int main() {
  int x;
  {
    // Shadowing is ok in a different scope
    string x = "foo";
  }
  return 0;
}
