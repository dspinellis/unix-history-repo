// This may look like C code, but it is really -*- C++ -*-
/* 
Copyright (C) 1989 Free Software Foundation
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

#ifndef _Int_h
#define _Int_h 1

/* compile with

    -DBUILTIN  - to get standard ints
    -DCALL     - to get calls instead of inlines 
                 (in which case don't use -finline-functions!)
    -DVIRT     - to make all members virtual
    -DBYVAL    - to use call-by-value, not by-reference
            (-DNO_GNU_CONST - to eliminate calling const functions const
              **** REMOVED, NO LONGER SUPPORTED IN g++ ***** )
    -DCONVERT  - to eliminate mixed mode fns that avoid constructors
    -DNO_NRV   - to eliminate use of named return values
    -DFAKEVPTR - to get one pointer per object padding
    -DRETREF   - to make =, +=, etc. return *this, not void
*/

#ifdef BUILTIN
typedef int Int;
#else

#ifdef CALL
#define INLINE
#else
#define INLINE inline
#endif

#ifndef VIRT
#define VIRTUAL
#else
#define VIRTUAL virtual
#endif

#ifdef BYVAL
#define REF 
#else
#define REF &
#endif

#ifndef  CONVERT
#define EXPLICIT
#endif

#ifndef RETREF
#define IntR void
#define ReturnIntR 
#else
#define IntR Int&
#define ReturnIntr  return *this
#endif

class Int 
{
protected:
  int          rep;
#ifdef FAKEVPTR
  void*        fake_vptr;
#endif
public: 
               Int ();
               Int (const int  b);
               Int (const Int& b);
  VIRTUAL      ~Int();

               operator int() const;

  VIRTUAL int   val() const;

  VIRTUAL IntR operator  = (const int);
  VIRTUAL IntR operator  = (const Int&);

  VIRTUAL IntR negate();
  VIRTUAL IntR complement();
  VIRTUAL IntR operator ++ ();
  VIRTUAL IntR operator -- ();

  VIRTUAL IntR operator += (const Int REF );
  VIRTUAL IntR operator -= (const Int REF );
  VIRTUAL IntR operator *= (const Int REF );
  VIRTUAL IntR operator /= (const Int REF );
  VIRTUAL IntR operator %= (const Int REF );
  VIRTUAL IntR operator |= (const Int REF );
  VIRTUAL IntR operator &= (const Int REF );
  VIRTUAL IntR operator ^= (const Int REF );
  VIRTUAL IntR operator <<=(const Int REF );
  VIRTUAL IntR operator >>=(const Int REF );

#ifdef EXPLICIT
  VIRTUAL IntR operator += (const int);
  VIRTUAL IntR operator -= (const int);
  VIRTUAL IntR operator *= (const int);
  VIRTUAL IntR operator /= (const int);
  VIRTUAL IntR operator %= (const int);
  VIRTUAL IntR operator |= (const int);
  VIRTUAL IntR operator &= (const int);
  VIRTUAL IntR operator ^= (const int);
  VIRTUAL IntR operator <<=(const int);
  VIRTUAL IntR operator >>=(const int);

#endif
};

INLINE int  Int::val() const { return rep; }
INLINE      Int::operator int() const { return val(); }

INLINE      Int::Int () :rep(0) {}
INLINE      Int::Int (const int  b) :rep(b) {}
INLINE      Int::Int (const Int& b) :rep(b.Int::val()) {}
INLINE      Int::~Int() {}

INLINE IntR Int::operator  = (const int  b) 
{ rep = b; ReturnIntR; }
INLINE IntR Int::operator  = (const Int&  b) 
{ rep = b.Int::val(); ReturnIntR; }
 
INLINE IntR Int::complement()
{ rep = ~rep; ReturnIntR; }
INLINE IntR Int::negate()
{ rep = -rep; ReturnIntR; }
INLINE IntR Int::operator ++ ()        
{ ++rep; ReturnIntR; }
INLINE IntR Int::operator -- ()        
{ --rep; ReturnIntR; }

INLINE IntR Int::operator += (const Int REF  b) 
{ rep += b.Int::val(); ReturnIntR; }
INLINE IntR Int::operator -= (const Int REF  b) 
{ rep -= b.Int::val(); ReturnIntR; }
INLINE IntR Int::operator *= (const Int REF  b) 
{ rep *= b.Int::val(); ReturnIntR; }
INLINE IntR Int::operator /= (const Int REF  b) 
{ rep /= b.Int::val(); ReturnIntR; }
INLINE IntR Int::operator %= (const Int REF  b) 
{ rep %= b.Int::val(); ReturnIntR; }
INLINE IntR Int::operator |= (const Int REF  b) 
{ rep |= b.Int::val(); ReturnIntR; }
INLINE IntR Int::operator &= (const Int REF  b) 
{ rep &= b.Int::val(); ReturnIntR; }
INLINE IntR Int::operator ^= (const Int REF  b) 
{ rep ^= b.Int::val(); ReturnIntR; }
INLINE IntR Int::operator <<=(const Int REF  b) 
{ rep <<= b.Int::val(); ReturnIntR; }
INLINE IntR Int::operator >>=(const Int REF  b) 
{ rep >>= b.Int::val(); ReturnIntR; }

#ifdef EXPLICIT

INLINE IntR Int::operator += (const int b) 
{ rep += b; ReturnIntR; }
INLINE IntR Int::operator -= (const int b) 
{ rep -= b; ReturnIntR; }
INLINE IntR Int::operator *= (const int b) 
{ rep *= b; ReturnIntR; }
INLINE IntR Int::operator /= (const int b) 
{ rep /= b; ReturnIntR; }
INLINE IntR Int::operator %= (const int b) 
{ rep %= b; ReturnIntR; }
INLINE IntR Int::operator |= (const int b) 
{ rep |= b; ReturnIntR; }
INLINE IntR Int::operator &= (const int b) 
{ rep &= b; ReturnIntR; }
INLINE IntR Int::operator ^= (const int b) 
{ rep ^= b; ReturnIntR; }
INLINE IntR Int::operator <<=(const int b) 
{ rep <<= b; ReturnIntR; }
INLINE IntR Int::operator >>=(const int b) 
{ rep >>= b; ReturnIntR; }


INLINE int& operator  = (int& a,  const Int REF  b) 
{ a = b.Int::val(); return a;}
INLINE int& operator += (int& a,  const Int REF  b) 
{ a += b.Int::val(); return a; }
INLINE int& operator -= (int& a,  const Int REF  b) 
{ a -= b.Int::val(); return a;}
INLINE int& operator *= (int& a,  const Int REF  b) 
{ a *= b.Int::val(); return a;}
INLINE int& operator /= (int& a,  const Int REF  b) 
{ a /= b.Int::val(); return a;}
INLINE int& operator %= (int& a,  const Int REF  b) 
{ a %= b.Int::val(); return a;}
INLINE int& operator |= (int& a,  const Int REF  b) 
{ a |= b.Int::val(); return a;}
INLINE int& operator &= (int& a,  const Int REF  b) 
{ a &= b.Int::val(); return a;}
INLINE int& operator ^= (int& a,  const Int REF  b) 
{ a ^= b.Int::val(); return a;}
INLINE int& operator <<=(int& a,  const Int REF  b) 
{ a <<= b.Int::val(); return a;}
INLINE int& operator >>=(int& a,  const Int REF  b) 
{ a >>= b.Int::val(); return a;}

#endif

#ifdef NO_NRV

INLINE Int  operator -  (const Int REF  a) 
{ Int r(a); r.negate(); return r; }
INLINE Int  operator ~  (const Int REF  a) 
{ Int r(a); r.complement(); return r; }

INLINE Int  operator +  (const Int REF  a, const Int REF  b) 
{ Int r(a); r += b.Int::val(); return r; }
INLINE Int  operator -  (const Int REF  a, const Int REF  b) 
{ Int r(a); r -= b.Int::val(); return r; }
INLINE Int  operator *  (const Int REF  a, const Int REF  b) 
{ Int r(a); r *= b.Int::val(); return r; }
INLINE Int  operator /  (const Int REF  a, const Int REF  b) 
{ Int r(a); r /= b.Int::val(); return r; }
INLINE Int  operator %  (const Int REF  a, const Int REF  b) 
{ Int r(a); r %= b.Int::val(); return r; }
INLINE Int  operator << (const Int REF  a, const Int REF  b) 
{ Int r(a); r <<= b.Int::val(); return r; }
INLINE Int  operator >> (const Int REF  a, const Int REF  b) 
{ Int r(a); r >>= b.Int::val(); return r; }
INLINE Int  operator &  (const Int REF  a, const Int REF  b) 
{ Int r(a); r &= b.Int::val(); return r; }
INLINE Int  operator |  (const Int REF  a, const Int REF  b) 
{ Int r(a); r |= b.Int::val(); return r; }
INLINE Int  operator ^  (const Int REF  a, const Int REF  b) 
{ Int r(a); r ^= b.Int::val(); return r; }

INLINE Int  operator +  (const Int REF  a, const int b) 
{ Int r(a); r += b; return r; }
INLINE Int  operator -  (const Int REF  a, const int b) 
{ Int r(a); r -= b; return r; }
INLINE Int  operator *  (const Int REF  a, const int b) 
{ Int r(a); r *= b; return r; }
INLINE Int  operator /  (const Int REF  a, const int b) 
{ Int r(a); r /= b; return r; }
INLINE Int  operator %  (const Int REF  a, const int b) 
{ Int r(a); r %= b; return r; }
INLINE Int  operator << (const Int REF  a, const int b) 
{ Int r(a); r <<= b; return r; }
INLINE Int  operator >> (const Int REF  a, const int b) 
{ Int r(a); r >>= b; return r; }
INLINE Int  operator &  (const Int REF  a, const int b) 
{ Int r(a); r &= b; return r; }
INLINE Int  operator |  (const Int REF  a, const int b) 
{ Int r(a); r |= b; return r; }
INLINE Int  operator ^  (const Int REF  a, const int b) 
{ Int r(a); r ^= b; return r; }

INLINE Int  operator +  (const int a, const Int REF  b) 
{ Int r(a); r += b.Int::val(); return r; }
INLINE Int  operator -  (const int a, const Int REF  b) 
{ Int r(a); r -= b.Int::val(); return r; }
INLINE Int  operator *  (const int a, const Int REF  b) 
{ Int r(a); r *= b.Int::val(); return r; }
INLINE Int  operator /  (const int a, const Int REF  b) 
{ Int r(a); r /= b.Int::val(); return r; }
INLINE Int  operator %  (const int a, const Int REF  b) 
{ Int r(a); r %= b.Int::val(); return r; }
INLINE Int  operator << (const int a, const Int REF  b) 
{ Int r(a); r <<= b.Int::val(); return r; }
INLINE Int  operator >> (const int a, const Int REF  b) 
{ Int r(a); r >>= b.Int::val(); return r; }
INLINE Int  operator &  (const int a, const Int REF  b) 
{ Int r(a); r &= b.Int::val(); return r; }
INLINE Int  operator |  (const int a, const Int REF  b) 
{ Int r(a); r |= b.Int::val(); return r; }
INLINE Int  operator ^  (const int a, const Int REF  b) 
{ Int r(a); r ^= b.Int::val(); return r; }

#else

INLINE Int  operator -  (const Int REF  a) return r(a)
{ r.negate();  }
INLINE Int  operator ~  (const Int REF  a) return r(a)
{ r.complement();  }

INLINE Int  operator +  (const Int REF  a, const Int REF  b) return r(a)
{ r += b.Int::val();  }
INLINE Int  operator -  (const Int REF  a, const Int REF  b) return r(a)
{ r -= b.Int::val();  }
INLINE Int  operator *  (const Int REF  a, const Int REF  b) return r(a)
{ r *= b.Int::val();  }
INLINE Int  operator /  (const Int REF  a, const Int REF  b) return r(a)
{ r /= b.Int::val();  }
INLINE Int  operator %  (const Int REF  a, const Int REF  b) return r(a)
{ r %= b.Int::val();  }
INLINE Int  operator << (const Int REF  a, const Int REF  b) return r(a)
{ r <<= b.Int::val();  }
INLINE Int  operator >> (const Int REF  a, const Int REF  b) return r(a)
{ r >>= b.Int::val();  }
INLINE Int  operator &  (const Int REF  a, const Int REF  b) return r(a)
{ r &= b.Int::val();  }
INLINE Int  operator |  (const Int REF  a, const Int REF  b) return r(a)
{ r |= b.Int::val();  }
INLINE Int  operator ^  (const Int REF  a, const Int REF  b) return r(a)
{ r ^= b.Int::val();  }

INLINE Int  operator +  (const Int REF  a, const int b) return r(a)
{ r += b;  }
INLINE Int  operator -  (const Int REF  a, const int b) return r(a)
{ r -= b;  }
INLINE Int  operator *  (const Int REF  a, const int b) return r(a)
{ r *= b;  }
INLINE Int  operator /  (const Int REF  a, const int b) return r(a)
{ r /= b;  }
INLINE Int  operator %  (const Int REF  a, const int b) return r(a)
{ r %= b;  }
INLINE Int  operator << (const Int REF  a, const int b) return r(a)
{ r <<= b;  }
INLINE Int  operator >> (const Int REF  a, const int b) return r(a)
{ r >>= b;  }
INLINE Int  operator &  (const Int REF  a, const int b) return r(a)
{ r &= b;  }
INLINE Int  operator |  (const Int REF  a, const int b) return r(a)
{ r |= b;  }
INLINE Int  operator ^  (const Int REF  a, const int b) return r(a)
{ r ^= b;  }

INLINE Int  operator +  (const int a, const Int REF  b) return r(a)
{ r += b.Int::val();  }
INLINE Int  operator -  (const int a, const Int REF  b) return r(a)
{ r -= b.Int::val();  }
INLINE Int  operator *  (const int a, const Int REF  b) return r(a)
{ r *= b.Int::val();  }
INLINE Int  operator /  (const int a, const Int REF  b) return r(a)
{ r /= b.Int::val();  }
INLINE Int  operator %  (const int a, const Int REF  b) return r(a)
{ r %= b.Int::val();  }
INLINE Int  operator << (const int a, const Int REF  b) return r(a)
{ r <<= b.Int::val();  }
INLINE Int  operator >> (const int a, const Int REF  b) return r(a)
{ r >>= b.Int::val();  }
INLINE Int  operator &  (const int a, const Int REF  b) return r(a)
{ r &= b.Int::val();  }
INLINE Int  operator |  (const int a, const Int REF  b) return r(a)
{ r |= b.Int::val();  }
INLINE Int  operator ^  (const int a, const Int REF  b) return r(a)
{ r ^= b.Int::val();  }

#endif

INLINE int  operator !  (const Int REF  a) { return !a.Int::val(); }

INLINE int  operator == (const Int REF  a, const Int REF  b) 
{ return a.Int::val() == b.Int::val(); }
INLINE int  operator != (const Int REF  a, const Int REF  b) 
{ return a.Int::val() != b.Int::val(); }
INLINE int  operator <  (const Int REF  a, const Int REF  b) 
{ return a.Int::val() <  b.Int::val(); }
INLINE int  operator <= (const Int REF  a, const Int REF  b) 
{ return a.Int::val() <= b.Int::val(); }
INLINE int  operator >  (const Int REF  a, const Int REF  b) 
{ return a.Int::val() >  b.Int::val(); }
INLINE int  operator >= (const Int REF  a, const Int REF  b) 
{ return a.Int::val() >= b.Int::val(); }

INLINE int  operator == (const Int REF  a, const int b) 
{ return a.Int::val() == b; }
INLINE int  operator != (const Int REF  a, const int b) 
{ return a.Int::val() != b; }
INLINE int  operator <  (const Int REF  a, const int b) 
{ return a.Int::val() <  b; }
INLINE int  operator <= (const Int REF  a, const int b) 
{ return a.Int::val() <= b; }
INLINE int  operator >  (const Int REF  a, const int b) 
{ return a.Int::val() >  b; }
INLINE int  operator >= (const Int REF  a, const int b) 
{ return a.Int::val() >= b; }

INLINE int  operator == (const int a, const Int REF  b) 
{ return a == b.Int::val(); }
INLINE int  operator != (const int a, const Int REF  b) 
{ return a != b.Int::val(); }
INLINE int  operator <  (const int a, const Int REF  b) 
{ return a <  b.Int::val(); }
INLINE int  operator <= (const int a, const Int REF  b) 
{ return a <= b.Int::val(); }
INLINE int  operator >  (const int a, const Int REF  b) 
{ return a >  b.Int::val(); }
INLINE int  operator >= (const int a, const Int REF  b) 
{ return a >= b.Int::val(); }

#endif
#endif
