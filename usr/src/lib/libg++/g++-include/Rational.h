// This may look like C code, but it is really -*- C++ -*-

/* 
Copyright (C) 1988 Free Software Foundation
    written by Doug Lea (dl@rocky.oswego.edu)

This file is part of GNU CC.

GNU CC is distributed in the hope that it will be useful,
but WITHOUT ANY WARRANTY.  No author or distributor
accepts responsibility to anyone for the consequences of using it
or for whether it serves any particular purpose or works at all,
unless he says so in writing.  Refer to the GNU CC General Public
License for full details.

Everyone is granted permission to copy, modify and redistribute
GNU CC, but only under the conditions described in the
GNU CC General Public License.   A copy of this license is
supposed to have been given to you along with GNU CC so you
can know your rights and responsibilities.  It should be in a
file named COPYING.  Among other things, the copyright notice
and this notice must be preserved on all copies.  
*/

#ifndef _Rational_h
#ifdef __GNUG__
#pragma once
#pragma interface
#endif
#define _Rational_h 1

#include <Integer.h>
#include <math.h>

class Rational
{
protected:
  Integer          num;
  Integer          den;

  void             normalize();

public:
                   Rational();
                   Rational(double);
                   Rational(long n, long d = 1);
                   Rational(const Integer& n);
                   Rational(const Integer& n, const Integer& d);
                   Rational(const Rational&);

                  ~Rational();

  void             operator =  (const Rational& y);

  friend int       operator == (const Rational& x, const Rational& y);
  friend int       operator != (const Rational& x, const Rational& y);
  friend int       operator <  (const Rational& x, const Rational& y);
  friend int       operator <= (const Rational& x, const Rational& y);
  friend int       operator >  (const Rational& x, const Rational& y);
  friend int       operator >= (const Rational& x, const Rational& y);

  friend Rational  operator +  (const Rational& x, const Rational& y);
  friend Rational  operator -  (const Rational& x, const Rational& y);
  friend Rational  operator *  (const Rational& x, const Rational& y);
  friend Rational  operator /  (const Rational& x, const Rational& y);

  void             operator += (const Rational& y);
  void             operator -= (const Rational& y);
  void             operator *= (const Rational& y);
  void             operator /= (const Rational& y);

#ifdef __GNUG__
  friend Rational  operator <? (const Rational& x, const Rational& y); // min
  friend Rational  operator >? (const Rational& x, const Rational& y); // max
#endif

  friend Rational  operator - (const Rational& x);


// builtin Rational functions


  void             negate();                      // x = -x
  void             invert();                      // x = 1/x

  friend int       sign(const Rational& x);             // -1, 0, or +1
  friend Rational  abs(const Rational& x);              // absolute value
  friend Rational  sqr(const Rational& x);              // square
  friend Rational  pow(const Rational& x, long y);
  friend Rational  pow(const Rational& x, Integer& y);
  const Integer&   numerator() const;
  const Integer&   denominator() const;

// coercion & conversion

                   operator double() const;
  friend Integer   floor(const Rational& x);
  friend Integer   ceil(const Rational& x);
  friend Integer   trunc(const Rational& x);
  friend Integer   round(const Rational& x);

  friend istream&  operator >> (istream& s, Rational& y);
  friend ostream&  operator << (ostream& s, const Rational& y);


// procedural versions of operators

  friend int       compare(const Rational& x, const Rational& y);
  friend void      add(const Rational& x, const Rational& y, Rational& dest);
  friend void      sub(const Rational& x, const Rational& y, Rational& dest);
  friend void      mul(const Rational& x, const Rational& y, Rational& dest);
  friend void      div(const Rational& x, const Rational& y, Rational& dest);

// error detection

  volatile void    error(const char* msg) const;
  int              OK() const;

};

typedef Rational RatTmp; // backwards compatibility

#if defined(__OPTIMIZE__) || defined(USE_LIBGXX_INLINES)

inline Rational::Rational()  {}
inline Rational::~Rational() {}

inline Rational::Rational(const Rational& y) :num(y.num), den(y.den) {}

inline Rational::Rational(const Integer& n) :num(n), den(1) {}

inline Rational::Rational(const Integer& n, const Integer& d) :num(n),den(d)
{
  normalize();
}

inline Rational::Rational(long n, long d) :num(n), den(d)
{
  normalize();
}

inline  void Rational::operator =  (const Rational& y)
{
  num = y.num;  den = y.den;
}

inline int operator == (const Rational& x, const Rational& y)
{
  return compare(x.num, y.num) == 0 && compare(x.den, y.den) == 0;
}

inline int operator != (const Rational& x, const Rational& y)
{
  return compare(x.num, y.num) != 0 || compare(x.den, y.den) != 0;
}

inline int operator <  (const Rational& x, const Rational& y)
{
  return compare(x, y) <  0; 
}

inline int operator <= (const Rational& x, const Rational& y)
{
  return compare(x, y) <= 0; 
}

inline int operator >  (const Rational& x, const Rational& y)
{
  return compare(x, y) >  0; 
}

inline int operator >= (const Rational& x, const Rational& y)
{
  return compare(x, y) >= 0; 
}

inline int sign(const Rational& x)
{
  return sign(x.num);
}

inline void Rational::negate()
{
  num.negate();
}


inline void Rational::operator += (const Rational& y) 
{
  add(*this, y, *this);
}

inline void Rational::operator -= (const Rational& y) 
{
  sub(*this, y, *this);
}

inline void Rational::operator *= (const Rational& y) 
{
  mul(*this, y, *this);
}

inline void Rational::operator /= (const Rational& y) 
{
  div(*this, y, *this);
}

inline const Integer& Rational::numerator() const { return num; }
inline const Integer& Rational::denominator() const { return den; }
inline Rational::operator double() const { return ratio(num, den); }

#ifdef __GNUG__
inline Rational operator <? (const Rational& x, const Rational& y)
{
  if (compare(x, y) <= 0) return x; else return y;
}

inline Rational operator >? (const Rational& x, const Rational& y)
{
  if (compare(x, y) >= 0) return x; else return y;
}
#endif

#if defined(__GNUG__) && !defined(NO_NRV)

inline Rational operator + (const Rational& x, const Rational& y) return r
{
  add(x, y, r);
}

inline Rational operator - (const Rational& x, const Rational& y) return r
{
  sub(x, y, r);
}

inline Rational operator * (const Rational& x, const Rational& y) return r
{
  mul(x, y, r);
}

inline Rational operator / (const Rational& x, const Rational& y) return r
{
  div(x, y, r);
}

#else /* NO_NRV */

inline Rational operator + (const Rational& x, const Rational& y) 
{
  Rational r; add(x, y, r); return r;
}

inline Rational operator - (const Rational& x, const Rational& y)
{
  Rational r; sub(x, y, r); return r;
}

inline Rational operator * (const Rational& x, const Rational& y)
{
  Rational r; mul(x, y, r); return r;
}

inline Rational operator / (const Rational& x, const Rational& y)
{
  Rational r; div(x, y, r); return r;
}
#endif
#endif

#endif
