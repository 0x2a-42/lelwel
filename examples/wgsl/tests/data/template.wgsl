fn f() {
  _ = a << b > c;
  _ = a < b >> c;
  _ = (a < b) > c;
  _ = (a < b < c) > d;
  _ = a < (b > c);
  _ = a<b && c>d;
  _ = a<b || c>d;
  _ = a<b + c>;
  a <<= b > c;
  a >>= b > c;
}
