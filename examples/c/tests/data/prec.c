struct S {
  int member;
};

int main(void) {
  int a = 1, b = 2, c = 3, d = 4;
  int x = 5, y = 6, z = 7;
  int arr[10] = {0};
  int *p = arr;
  struct S s = {0};
  struct S *sp = &s;

  int result =
      a++ + --b * c / d % x + y - z << a + b >> c < d == x != y & z ^ a | b &&
                  c ||
              d
          ? x + y * z
          : a - b / c;
  result = a = b += c -= d *= x /= y %= z;
  result = a <<= b >>= c;
  result = a &= b ^= c |= d;
  result = (int)(float)~*p + sizeof arr + _Alignof(int);
  result = !a + +b - -c;
  result = (a, b, c) + arr[a + b * c] + s.member + sp->member;
  result = a ? b : c ? d : x;
  result = a + b, c * d, x << y;
  result = (a + b) * c / d % x + (y << z) - (a & b) ^ (c | d);
}
