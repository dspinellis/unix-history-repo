#include <stream.h>
#include <Integer.h>
#include <builtin.h>

// Fun and games with the Fibonacci function.
// Also demonstrates use of the `named return value' extension.

// fib1 and fib2 from Doug Lea, others from Doug Schmidt.

/**********************************************************************/

// Standard iterative version:

// Schmidt uses convention that fib(0) = 1, not fib(0) = 0;
// so I just increment n here.

Integer 
fib1(int n)
{
  ++n;
  if (n <= 0)
    return 0;
  else
    {
      Integer f = 1;
      Integer prev = 0;

      while (n > 1)
        {
          Integer tmp = f;
          f += prev;
          prev = tmp;
          --n;
        }

      return f;
    }
}

/**********************************************************************/

//                                                       n
// via transformed matrix multiplication  of     ( 0  1 )
//                                               ( 1  1 )
// simplified knowing that the matrix is always of 
// the form  (a b)
//           (b c)
// (where (a, b) and (b, c) are conseq pairs of conseq fib's;
// further simplified by realizing that c = a+b, so c is only
// calculated implicitly.
// Given all this, do the power via the standard russian
// peasant divide-and-conquer algorithm, with
// the current multiplier held in (p q)
//                                (q -)
//
// This example also shows that using procedure calls instead of operators
// can be faster for Integers


// operator version

Integer 
fib2(int n) 
{
  ++n;                          // 1-based for compatability; see above
  Integer a = 0;
  if (n <= 0)
    return a;
  else
  {
    Integer b = 1;
    Integer p = 0;
    Integer q = 1;
    
    for(;;)
    {
      if (n == 1)
        return a * p + b * q;
      else if (odd (n))
      {
        Integer aq = a * q;
        Integer bq = b * q;
        a = a * p + bq;
        b = b * p  + aq + bq;
      }
      Integer qq = q * q;
      q = (q * p) * 2 + qq;
      p = (p * p) + qq;
      n >>= 1;
    }
  }
}

// with all expressions named, including return value

Integer 
fib2a(int n) return a(0)
{
  ++n;
  if (n <= 0)
    return;
  else
  {
    Integer b = 1;
    Integer p = 0;
    Integer q = 1;
    
    for(;;)
    {
      if (n == 1)
      {
        Integer bq = b * q;
        a *= p ;
        a += bq;
        return;
      }
      else if (odd (n))
      {
        Integer aq = a * q;
        Integer bq = b * q;
        a *= p;
        a += bq;
        b *= p;
        b += bq;
        b += aq;
      }
      Integer qq = q * q;
      Integer pp = p * p;
      Integer pq = p * q;
      q = pq;
      q += pq;
      q += qq;
      p = pp;
      p += qq;
      n >>= 1;
    }
  }
}

// procedure call version

Integer 
fib2b(int n) return a(0)
{
  ++n; 

  if (n <= 0)
    return;
  else
  {
    Integer b = 1;
    Integer p = 0;
    Integer q = 1;
    
    for(;;)
    {
      if (n == 1)
      {
        Integer bq; mul(b, q, bq);
        mul(a, p, a);
        add(a, bq, a);
        return;
      }
      else if (odd (n))
      {
        Integer aq; mul(a, q, aq);
        Integer bq; mul(b, q, bq);
        mul(a, p, a);
        mul(b, p, b);
        add(a, bq, a);
        add(b, bq, b);
        add(b, aq, b);
      }
      Integer qq; mul(q, q, qq);
      Integer pp; mul(p, p, pp);
      Integer pq; mul(p, q, pq);
      add(pq, pq, q);
      add(q, qq, q);
      add(pp, qq, p);
      n >>= 1;
    }
  }
}

// procedure call version, reusing variables

Integer 
fib2c(int n) return a(0)
{
  ++n; 

  if (n <= 0)
    return;
  else
  {
    Integer b = 1;
    Integer p = 0;
    Integer q = 1;
    Integer bq, aq, qq, pp, pq;
    for(;;)
    {
      if (n == 1)
      {
        mul(b, q, bq);
        mul(a, p, a);
        add(a, bq, a);
        return;
      }
      else if (odd (n))
      {
        mul(a, q, aq);
        mul(b, q, bq);
        mul(a, p, a);
        mul(b, p, b);
        add(a, bq, a);
        add(b, bq, b);
        add(b, aq, b);
      }
      mul(q, q, qq);
      mul(p, p, pp);
      mul(p, q, pq);
      add(pq, pq, q);
      add(q, qq, q);
      add(pp, qq, p);
      n >>= 1;
    }
  }
}

/**********************************************************************/
// Ullman memoizers:

class Ullman_Array
{
  // Ullman's array implementation allows Initialization, Store, and Fetch
  // in O(1) time.  Although it takes O(n) space the time to initialize enables
  // us to compute a Fibonacci number in O(log n) time!
  // The basic concept of Ullman's array implementation is the use of a 
  // Hand_Shake_Array and an Index_Array.  An Index location in the array is 
  // only considered initialized if the contents in the Hand_Shake_Array and
  // Index_Array point to each other, i.e. they ``shake hands!''
  
private:
  int       max_array_range;
  int       max_set_size;
  int      *index_array;
  int      *hand_shake_array;
  Integer  *storage_array;
  int       current_max;
  
public:
  Integer uninitialized;

  Ullman_Array (int array_range, int set_size);
  void    store (int index, Integer &item);
  Integer fetch (int index);

};

inline
Ullman_Array::Ullman_Array (int array_range, int set_size)
{
  max_array_range  = array_range;
  max_set_size     = set_size;
  index_array      = new int[max_array_range + 1];
  hand_shake_array = new int[max_set_size];
  storage_array    = new Integer[max_set_size];
  current_max      = -1;
  uninitialized    = 0; // No fibonacci number has value of 0.
}

// Store Item at the proper Index of the Ullman array.
// The Hand_Shake_Array determines whether the Index has already
// been stored into.  If it has NOT, then a new location for it
// is set up in the Storage_Array and the Hand_Shake_Array and Index_Array 
// are set to point at each other.

inline void
Ullman_Array::store (int index, Integer &item)
{
  int  hand_shake_index = index_array[index];
  
  if (hand_shake_index > current_max || hand_shake_index < 0
      || index != hand_shake_array[hand_shake_index])
    {
      hand_shake_index                   = ++current_max;
      hand_shake_array[hand_shake_index] = index;
      index_array[index]                 = hand_shake_index;
    }
  storage_array[hand_shake_index] = item;
}

// Returns uninitialized if not initialized, else returns Item at Index.

inline Integer
Ullman_Array::fetch(int index) 
{
  int hand_shake_index = index_array[index];
  
  if (hand_shake_index > current_max || hand_shake_index < 0
      || index != hand_shake_array[hand_shake_index])
    return uninitialized;
  else 
    return storage_array[hand_shake_index];
}

/**********************************************************************/

class Memo_Fib : public Ullman_Array
{
public:
  Memo_Fib (int fib_num);
  Integer fib3 (int n);
  Integer fib3a (int n);
};


// The total number of Items computed by the Fib routine is bounded by
// 4 * ceiling of log base 2 of Fib_Num.  Of course, the basis of the
// recurrence is Fib(0) == 1 and Fib(1) == 1!

Memo_Fib::Memo_Fib (int fib_num) : Ullman_Array(fib_num, (4 * lg (fib_num)))
{
  store (0, 1);
  store (1, 1);
}

// Uses the memoization technique to reduce the time complexity to calculate
// the nth Fibonacci number in O(log n) time.  If the value of ``n'' is
// already in the table we return it in O(1) time.  Otherwise, we use the
// super-nifty divide-and-conquer algorithm to break the problem up into
// 4 pieces of roughly size n/2 and solve them recursively.  Although this
// looks like an O(n^2) recurrence the memoization reduces the number of
// recursive calls to O(log n)!

Integer
Memo_Fib::fib3 (int n) return fib (fetch (n));
{
  if (fib == uninitialized)
    {
      int m = n >> 1;
      
      fib = fib3 (m) * fib3 (n - m) + fib3 (m - 1) * fib3 (n - m - 1);
      store (n, fib);
    }   
}

// The same, with procedure calls instead of operators

Integer
Memo_Fib::fib3a (int n) return fib (fetch (n));
{
  if (fib == uninitialized)
    {
      int m = n >> 1;
      Integer tmp;
      mul(fib3a(m), fib3a(n - m), fib);
      mul(fib3a(m - 1), fib3a(n - m - 1), tmp);
      add(fib, tmp, fib);
      store (n, fib);
    }   
}

/**********************************************************************/

// Here's a linear-time dynamic programming solution to the same problem. 
// It makes use of G++/GCC dynamic arrays to build a table of ``n'' partial 
// solutions.  Naturally, there is an O(1) space solution, but this O(n) 
// approach is somewhat more intuitive and follows the ``original'' recurrence
// more closely.

Integer 
fib4 (int n)
{
  Integer table[n + 1];
  table[0] = 1;
  table[1] = 1;

  for (int i = 2; i <= n; i++) 
    table[i] = table[i - 1] + table[i - 2];
  
  return table[n];
}

/**********************************************************************/

// The extended integers provide numbers of the form:
// (base+sqrt(5)*Rad_5)/2^Pow_2
// These are used to find the solution to the closed form of fib(n). 

struct Extended_Int
{
  Integer base;
  Integer rad_5;
  int     pow_2;

  Extended_Int (Integer &param1, Integer &param2, int param3);
  Extended_Int (Extended_Int& param);
  Extended_Int (void) {}

  friend Extended_Int operator- (Extended_Int &param1, Extended_Int &param2);
  friend Extended_Int operator* (Extended_Int &param1, Extended_Int &param2);
  friend Extended_Int sqrt (Extended_Int &param1);
  friend Extended_Int pow  (Extended_Int &param1, int num);
};

inline
Extended_Int::Extended_Int (Integer &param1, Integer &param2, int param3)
{
  base  = param1;
  rad_5 = param2;
  pow_2 = param3;
}

inline
Extended_Int::Extended_Int (Extended_Int &param)
{
  base  = param.base;
  rad_5 = param.rad_5;
  pow_2 = param.pow_2;
}

inline Extended_Int 
operator- (Extended_Int &param1, Extended_Int &param2) return temp;
{
  temp.base  = param1.base - param2.base;
  temp.rad_5 = param1.rad_5 - param2.rad_5;
  temp.pow_2 = param1.pow_2;
}

Extended_Int 
operator* (Extended_Int &param1, Extended_Int &param2) return temp;
{
  temp.base  = param1.base * param2.base + 5 * param1.rad_5 * param2.rad_5;
  temp.rad_5 = param1.base * param2.rad_5 + param1.rad_5 * param2.base;
  temp.pow_2 = param1.pow_2 + param2.pow_2;

  while (temp.pow_2 > 0 && !(odd (temp.base) || odd (temp.rad_5)))
    {
      temp.base >>= 1;
      temp.rad_5 >>= 1;
      temp.pow_2--;
    }

}

inline Extended_Int 
sqrt (Extended_Int &param1)
{
  return param1 * param1;
}

Extended_Int 
pow (Extended_Int &param1, int num) 
{
  if (num > 1)
    return odd (num)
      ? param1 * sqrt (pow (param1, num >> 1)) : sqrt (pow (param1, num >> 1));
  else 
    return param1;
}

/**********************************************************************/

// Calculates fib (n) by solving the closed form of the recurrence. 

class Closed_Form : private Extended_Int
{
private:
  Extended_Int cons1;
  Extended_Int cons2;

public:
  Closed_Form (void): cons1 (1, 1, 1), cons2 (1, -1, 1) {}
  Integer fib5 (int n);
};

Integer
Closed_Form::fib5 (int num) 
{
  Extended_Int temp = pow (cons1, num + 1) - pow (cons2, num + 1);
  
  while (temp.pow_2 > 0 && !(odd (temp.base) || odd (temp.rad_5)))
    {
      temp.base  >>= 1;
      temp.rad_5 >>= 1;
      temp.pow_2--;
    }
  
  return temp.rad_5;
}

/**********************************************************************/

static const int DEFAULT_SIZE = 10000;


void dofib1(int fib_num)
{
  start_timer ();
  Integer result = fib1 (fib_num);
  double time = return_elapsed_time(0.0);
  cout << "fib1 = " << result << ". Time = " << time << "\n";
}

void dofib2(int fib_num)
{
  start_timer ();
  Integer result = fib2 (fib_num);
  double time = return_elapsed_time(0.0);
  cout << "fib2 = " << result << ". Time = " << time << "\n";
}

void dofib2a(int fib_num)
{
  start_timer ();
  Integer result = fib2a(fib_num);
  double time = return_elapsed_time(0.0);
  cout << "fib2a = " << result << ". Time = " << time << "\n";
}

void dofib2b(int fib_num)
{
  start_timer ();
  Integer result = fib2b(fib_num);
  double time = return_elapsed_time(0.0);
  cout << "fib2b = " << result << ". Time = " << time << "\n";
}

void dofib2c(int fib_num)
{
  start_timer ();
  Integer result = fib2c(fib_num);
  double time = return_elapsed_time(0.0);
  cout << "fib2c = " << result << ". Time = " << time << "\n";
}

void dofib3(int fib_num)
{
  start_timer ();
  Memo_Fib    Memo_Test (fib_num);
  Integer result = Memo_Test.fib3 (fib_num);
  double time = return_elapsed_time(0.0);
  cout << "fib3 = " << result << ". Time = " << time << "\n";
}

void dofib3a(int fib_num)
{
  start_timer ();
  Memo_Fib    Memo_Test (fib_num);
  Integer result = Memo_Test.fib3a (fib_num);
  double time = return_elapsed_time(0.0);
  cout << "fib3a = " << result << ". Time = " << time << "\n";
}

void dofib4(int fib_num)
{
  start_timer ();
  Integer result = fib4 (fib_num);
  double time = return_elapsed_time(0.0);
  cout << "fib4 = " << result << ". Time = " << time << "\n";
}

void dofib5(int fib_num)
{
  Closed_Form Rec_Test;
  Integer result = Rec_Test.fib5 (fib_num);
  double time = return_elapsed_time(0.0);
  cout << "fib5 = " << result << ". Time = " << time << "\n";
}


int
main (int argc, char *argv[])
{
  int         fib_num = (argc > 1) ? atoi (argv[1]) : DEFAULT_SIZE;

  cout << "Results for fib(" << fib_num << "):\n\n";

  dofib1(fib_num);
  dofib2(fib_num);
  dofib2a(fib_num);
  dofib2b(fib_num);
  dofib2c(fib_num);
  dofib3(fib_num);
  dofib3a(fib_num);
//  dofib4(fib_num);  // This uses too much mem for large n!
  dofib5(fib_num);
  return 0;
}

