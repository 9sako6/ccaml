int main() {
    int a = 4;
    {
      int a = 3;
      return a;
    }
    return a;
}
