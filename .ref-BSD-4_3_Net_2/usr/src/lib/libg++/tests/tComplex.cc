/*
 a test file for Complex
*/

#include <assert.h>

#define tassert(ex) {if ((ex)) cerr << #ex << "\n"; \
                       else _assert(#ex, __FILE__,__LINE__); }

#include <Complex.h>

// to test near-equality

const double eps = 0.000001;

static void close_enough(const Complex& a, const Complex& b)
{
  assert(abs(real(a) - real(b)) < eps &&
         abs(imag(a) - imag(b)) < eps);
}


void test3(Complex& a, Complex& b, Complex& c)
{

  close_enough(-(-a) , a);
  close_enough((a + b) ,  (b + a));
  close_enough((a + (-b)) ,  (a - b));
  close_enough((a * b) ,  (b * a));
  close_enough((a * (-b)) , -(a * b));
  close_enough((a / (-b)) , -(a / b));
  close_enough((a - b) ,  -(b - a));
  close_enough((a + (b + c)) , ((a + b) + c));
  close_enough((a * (b * c)) , ((a * b) * c));
  close_enough((a * (b + c)) , ((a * b) + (a * c)));
  close_enough(((a - b) + b) , a);
  close_enough(((a + b) - b) , a);
  close_enough(((a * b) / b) , a);
  close_enough(((a / b) * b) , a);


  Complex x = a;
  x *= b;
  close_enough(x , (a * b));
  x += c;
  close_enough(x , ((a * b) + c));
  x -= a;
  close_enough(x , (((a * b) + c) - a));
  x /= b;
  close_enough(x , ((((a * b) + c) - a) / b));

}

main()
{
  Complex one = 1.0;
  Complex i (0.0, 1.0);
  Complex neg_one = -1.0;

  cout << "Complex one = " << one << "\n";
  cout << "i = " << i << "\n";
  cout << "neg_one = " << neg_one << "\n";
  cout << "sqrt(neg_one) = " << sqrt(neg_one) << "\n";

  Complex a (2.0, 3.0);
  Complex b (4.0, 5.0);

  cout << "a = " << a << "\n";
  cout << "b = " << b << "\n";

  cout << "a + one = " << (a + one) << "\n";
  (close_enough((a+one), Complex(3.0, 3.0)));
  cout << "a - one = " << (a - one) << "\n";
  (close_enough((a-one), Complex(1.0, 3.0)));
  cout << "a * one = " << (a * one) << "\n";
  (close_enough((a*one), a));
  cout << "a / one = " << (a / one) << "\n";
  (close_enough((a/one), a));

  cout << "a + b = " << (a + b) << "\n";
  (close_enough((a+b), Complex(6.0, 8.0)));
  cout << "a - b = " << (a - b) << "\n";
  (close_enough((a-b), Complex(-2.0, -2.0)));
  cout << "a * b = " << (a * b) << "\n";
  (close_enough((a*b), Complex(-7.0, 22.0)));
  cout << "a / b = " << (a / b) << "\n";
  (close_enough((a/b), Complex(0.5609760976, 0.0487804878)));

  Complex c;

  c = a; cout << "c = a; c += b = " << (c += b) << "\n";
  c = a; cout << "c = a; c -= b = " << (c -= b) << "\n";
  c = a; cout << "c = a; c *= b = " << (c *= b) << "\n";
  c = a; cout << "c = a; c /= b = " << (c /= b) << "\n";

  cout << "-a = " << (-a) << "\n";
  cout << "real(a) = " << real(a) << "\n";
  assert(real(a) == 2.0);
  cout << "imag(a) = " << imag(a) << "\n";
  assert(imag(a) == 3.0);
  cout << "conj(a) = " << conj(a) << "\n";
  assert(conj(a) == Complex(2.0, -3.0));
  cout << "norm(a) = " << norm(a) << "\n";
  assert(norm(a) == 13.0);

  cout << "abs(a) = " << abs(a) << "\n";
  cout << "arg(a) = " << arg(a) << "\n";
  cout << "cos(a) = " << cos(a) << "\n";
  cout << "sin(a) = " << sin(a) << "\n";
  cout << "cosh(a) = " << cosh(a) << "\n";
  cout << "sinh(a) = " << sinh(a) << "\n";
  cout << "log(a) = " << log(a) << "\n";
  cout << "exp(a) = " << exp(a) << "\n";
  cout << "sqrt(a) = " << sqrt(a) << "\n";
  cout << "pow(a, 2) = " << pow(a, 2) << "\n";
  cout << "pow(a, b) = " << pow(a, b) << "\n";

  Complex d (10, 20);
  Complex e = pow(a, 2);
 
  test3(one, one, one);
  test3(a, a, a);
  test3(a, b, d);
  test3(e, i, b);
  test3(d, d, i);

  cout << "enter a Complex number in form (a, b) or (a) or a: ";
  cin >> c;
  cout << "number = " << c << "\n";

  cout << "\nEnd of test\n";
}
