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

#ifndef _Complex_h
#ifdef __GNUG__
#pragma once
#pragma interface
#endif
#define _Complex_h 1


#include <stream.h>
#include <math.h>

class Complex
{
#ifdef __ATT_complex__
public:
#else
protected:
#endif

  double           re;
  double           im;

public:

  double           real() const;
  double           imag() const;

                   Complex();
                   Complex(const Complex& y);
                   Complex(double r, double i=0);

                  ~Complex();

  Complex&         operator =  (const Complex& y);

  Complex&         operator += (const Complex& y);
  Complex&         operator += (double y);
  Complex&         operator -= (const Complex& y);
  Complex&         operator -= (double y);
  Complex&         operator *= (const Complex& y);
  Complex&         operator *= (double y);

  Complex&         operator /= (const Complex& y); 
  Complex&         operator /= (double y); 

  void             error(const char* msg) const;
};


// error handlers

extern  void default_Complex_error_handler(const char*);
extern  one_arg_error_handler_t Complex_error_handler;

extern  one_arg_error_handler_t 
        set_Complex_error_handler(one_arg_error_handler_t f);


// non-inline functions

Complex   operator /  (const Complex& x, const Complex& y);
Complex   operator /  (const Complex& x, double y);
Complex   operator /  (double   x, const Complex& y);

Complex   cos(const Complex& x);
Complex   sin(const Complex& x);

Complex   cosh(const Complex& x);
Complex   sinh(const Complex& x);

Complex   exp(const Complex& x);
Complex   log(const Complex& x);

Complex   pow(const Complex& x, long p);
Complex   pow(const Complex& x, const Complex& p);
Complex   pow(const Complex& x, double y);
Complex   sqrt(const Complex& x);
   
istream&  operator >> (istream& s, Complex& x);
ostream&  operator << (ostream& s, const Complex& x);

// other functions defined as inlines

int  operator == (const Complex& x, const Complex& y);
int  operator == (const Complex& x, double y);
int  operator != (const Complex& x, const Complex& y);
int  operator != (const Complex& x, double y);

Complex  operator - (const Complex& x);
Complex  conj(const Complex& x);
Complex  operator + (const Complex& x, const Complex& y);
Complex  operator + (const Complex& x, double y);
Complex  operator + (double x, const Complex& y);
Complex  operator - (const Complex& x, const Complex& y);
Complex  operator - (const Complex& x, double y);
Complex  operator - (double x, const Complex& y);
Complex  operator * (const Complex& x, const Complex& y);
Complex  operator * (const Complex& x, double y);
Complex  operator * (double x, const Complex& y);

double  real(const Complex& x);
double  imag(const Complex& x);
double  abs(const Complex& x);
double  norm(const Complex& x);
double  arg(const Complex& x);

Complex  polar(double r, double t = 0.0);

#if defined(__OPTIMIZE__) || defined(USE_LIBGXX_INLINES)

// inline members

inline double  Complex::real() const { return re; }
inline double  Complex::imag() const { return im; }

inline Complex::Complex() {}
inline Complex::Complex(const Complex& y) :re(y.real()), im(y.imag()) {}
inline Complex::Complex(double r, double i) :re(r), im(i) {}

inline Complex::~Complex() {}

inline Complex&  Complex::operator =  (const Complex& y) 
{ 
  re = y.real(); im = y.imag(); return *this; 
} 

inline Complex&  Complex::operator += (const Complex& y)
{ 
  re += y.real();  im += y.imag(); return *this; 
}

inline Complex&  Complex::operator += (double y)
{ 
  re += y; return *this; 
}

inline Complex&  Complex::operator -= (const Complex& y)
{ 
  re -= y.real();  im -= y.imag(); return *this; 
}

inline Complex&  Complex::operator -= (double y)
{ 
  re -= y; return *this; 
}

inline Complex&  Complex::operator *= (const Complex& y)
{  
  double r = re * y.real() - im * y.imag();
  im = re * y.imag() + im * y.real(); 
  re = r; 
  return *this; 
}

inline Complex&  Complex::operator *= (double y)
{  
  re *=  y; im *=  y; return *this; 
}


//  functions

inline int  operator == (const Complex& x, const Complex& y)
{
  return x.real() == y.real() && x.imag() == y.imag();
}

inline int  operator == (const Complex& x, double y)
{
  return x.imag() == 0.0 && x.real() == y;
}

inline int  operator != (const Complex& x, const Complex& y)
{
  return x.real() != y.real() || x.imag() != y.imag();
}

inline int  operator != (const Complex& x, double y)
{
  return x.imag() != 0.0 || x.real() != y;
}

inline Complex  operator - (const Complex& x)
{
  return Complex(-x.real(), -x.imag());
}

inline Complex  conj(const Complex& x)
{
  return Complex(x.real(), -x.imag());
}

inline Complex  operator + (const Complex& x, const Complex& y)
{
  return Complex(x.real() + y.real(), x.imag() + y.imag());
}

inline Complex  operator + (const Complex& x, double y)
{
  return Complex(x.real() + y, x.imag());
}

inline Complex  operator + (double x, const Complex& y)
{
  return Complex(x + y.real(), y.imag());
}

inline Complex  operator - (const Complex& x, const Complex& y)
{
  return Complex(x.real() - y.real(), x.imag() - y.imag());
}

inline Complex  operator - (const Complex& x, double y)
{
  return Complex(x.real() - y, x.imag());
}

inline Complex  operator - (double x, const Complex& y)
{
  return Complex(x - y.real(), -y.imag());
}

inline Complex  operator * (const Complex& x, const Complex& y)
{
  return Complex(x.real() * y.real() - x.imag() * y.imag(), 
                 x.real() * y.imag() + x.imag() * y.real());
}

inline Complex  operator * (const Complex& x, double y)
{
  return Complex(x.real() * y, x.imag() * y);
}

inline Complex  operator * (double x, const Complex& y)
{
  return Complex(x * y.real(), x * y.imag());
}

inline double  real(const Complex& x)
{
  return x.real();
}

inline double  imag(const Complex& x)
{
  return x.imag();
}

inline double  abs(const Complex& x)
{
  return hypot(x.real(), x.imag());
}

inline double  norm(const Complex& x)
{
  return (x.real() * x.real() + x.imag() * x.imag());
}

inline double  arg(const Complex& x)
{
  return atan2(x.imag(), x.real());
}

inline Complex  polar(double r, double t)
{
  return Complex(r * cos(t), r * sin(t));
}

#endif __OPTIMIZE__
#endif
