int max(int x, int y) {
  if (x < y) {
    return y;
  } else {
    return x;
  }
}


int main()
{
  double d = 2.0;
  // Implicit type cast not implemented
  int x = max(d);

  return 0;
}

