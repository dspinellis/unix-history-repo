// Header file for Vec3D class -*- C++ -*-
/* 
Copyright (C) 1990 Free Software Foundation
    written by J. Thomas Ngo, Harvard University

This file is part of the GNU C++ Library.

The GNU C++ Library is distributed in the hope that it will be useful,
but WITHOUT ANY WARRANTY.  No author or distributor accepts
responsibility to anyone for the consequences of using it or for
whether it serves any particular purpose or works at all, unless he
says so in writing.  Refer to the GNU General Public License for full
details.

Everyone is granted permission to copy, modify and redistribute The
GNU C++ Library, but only under the conditions described in the GNU
General Public License.  A copy of this license is supposed to have
been given to you along with The GNU C++ Library so you can know your
rights and responsibilities.  It should be in a file named COPYING.
Among other things, the copyright notice and this notice must be
preserved on all copies.  
*/


#ifndef _Vec3D_h
#define _Vec3D_h 1

#include <math.h>
#include <builtin.h>

//===========================================================================
// This class is used by PlotFile3D
//===========================================================================

class Vec3D 
{
protected:
  double          n[3];

public:

  // Constructors and destructor

                  Vec3D(); // initialize to (0,0,0)
                  Vec3D(const double x, const double y, const double z);
                  Vec3D(const Vec3D& that);

                 ~Vec3D();

  // Assignment operators

  Vec3D&          operator  = ( const Vec3D& that );
  Vec3D&          operator += ( const Vec3D& that );
  Vec3D&          operator -= ( const Vec3D& that );
  Vec3D&          operator *= ( const double that );
  Vec3D&          operator /= ( const double that );

  // You can read, but not set coordinates individually:

  double          operator [] (const int i) const;   // Vector notation

  double          x() const;   // Point notation
  double          y() const;
  double          z() const;

  // Error handling
  volatile void  error(const char* s) const;
};

// associated operators defined inline below

double  mod(const Vec3D& x); // sqrt of sum of squares

Vec3D   operator - (const Vec3D& a);
Vec3D   operator + (const Vec3D& a, const Vec3D& b);
Vec3D   operator - (const Vec3D& a, const Vec3D& b);
Vec3D   operator * (const Vec3D& a, const double sc); // scale
double  operator * (const Vec3D& a, const Vec3D& b);  // dot
Vec3D   operator ^ (const Vec3D& a, const Vec3D& b);  // cross prod
int     operator == (const Vec3D& a, const Vec3D& b);
int     operator != (const Vec3D& a, const Vec3D& b);


// Error handling

inline volatile void Vec3D::error(const char* s) const 
{
  (*lib_error_handler)("Vec3D",s);
}

inline Vec3D::Vec3D() 
{ 
  n[0] = n[1] = n[2] = 0; 
}

inline Vec3D::Vec3D(const double x, const double y, const double z)
{ 
  n[0]=x; n[1]=y; n[2]=z; 
}


inline Vec3D::Vec3D(const Vec3D& that) 
{ 
  n[0] = that.n[0]; n[1] = that.n[1]; n[2] = that.n[2]; 
}

inline Vec3D::~Vec3D() {}

inline Vec3D& Vec3D::operator = (const Vec3D& that) 
{ 
  n[0] = that.n[0]; n[1] = that.n[1]; n[2] = that.n[2]; 
  return *this; 
}

inline const double Vec3D::operator [] (const int i) const 
{
  if( i<0 || i>2 ) error("index out of range");
  return n[i]; 
}

inline const double Vec3D::x() const
{
  return n[0];
}

inline const double Vec3D::y() const
{
  return n[1];
}

inline const double Vec3D::z() const
{
  return n[2];
}


inline Vec3D operator - (const Vec3D& a)
{ 
  return Vec3D( -a.x(), -a.y(), -a.z() ); 
}

inline double mod(const Vec3D& p)
{ 
  return sqrt(sqr(p.x()) + sqr(p.y()) + sqr(p.z()));
}

inline Vec3D operator + (const Vec3D& a, const Vec3D& b)
{ 
  return Vec3D( a.x()+b.x(), a.y()+b.y(), a.z()+b.z() ); 
}

inline Vec3D operator - (const Vec3D& a, const Vec3D& b)
{ 
  return Vec3D( a.x()-b.x(), a.y()-b.y(), a.z()-b.z() ); 
}

inline Vec3D operator * (const Vec3D& a, const double sc) // scale
{ 
  return Vec3D( a.x()*sc, a.y()*sc, a.z()*sc ); 
}

inline double operator * (const Vec3D& a, const Vec3D& b) // dot
{ 
  return a.x()*b.x() + a.y()*b.y() + a.z()*b.z(); 
}

inline Vec3D operator ^ (const Vec3D& a, const Vec3D& b) // cross
{ 
  return Vec3D(a.y()*b.z() - a.z()*b.y(),                // UK style
               a.z()*b.x() - a.x()*b.z(),                // notation
               a.x()*b.y() - a.y()*b.x()); 
}

inline Vec3D& Vec3D::operator += ( const Vec3D& that ) 
{ 
  n[0] += that.x(); n[1] += that.y(); n[2] += that.z(); 
  return *this; 
}

inline Vec3D& Vec3D::operator -= ( const Vec3D& that ) 
{ 
  n[0] -= that.x(); n[1] -= that.y(); n[2] -= that.z(); 
  return *this; 
}

inline Vec3D& Vec3D::operator *= ( const double that ) 
{ 
  n[0] *= that; n[1] *= that; n[2] *= that; 
  return *this; 
}

inline Vec3D& Vec3D::operator /= ( const double that ) 
{ 
  n[0] /= that; n[1] /= that; n[2] /= that; 
  return *this; 
}

inline int operator == (const Vec3D& a, const Vec3D& b)
{
  return a.x() == b.x() && a.y() == b.y() && a.z() == b.z();
}

inline int operator != (const Vec3D& a, const Vec3D& b)
{
  return !(a == b);
}


#endif                          // _Vec3D_h
