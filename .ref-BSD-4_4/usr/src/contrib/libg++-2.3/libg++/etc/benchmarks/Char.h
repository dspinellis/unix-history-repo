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

#ifndef _Char_h
#define _Char_h 1

/* compile with

    -DBUILTIN  - to get standard chars
    -DCALL     - to get calls instead of inlines 
                 (in which case don't use -finline-functions!)
    -DVIRT     - to make all members virtual
    -DBYVAL    - to use call-by-value, not by-reference
          (-DNO_GNU_CONST - to eliminate calling const functions const
            **** REMOVED, NO LONGER SUPPORTED IN G++ *****)
    -DNO_GNU_CONST - to eliminate calling const functions const
    -DCONVERT  - to eliminate mixed mode fns that avoid constructors
    -DNO_NRV   - to eliminate use of named return values
    -DFAKEVPTR - to get one pocharer per object padding
    -DRETREF   - to make =, +=, etc. return *this, not void
*/

#ifdef BUILTIN
typedef char Char;
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
#define CharR void
#define ReturnCharR 
#else
#define CharR Char&
#define ReturnCharr  return *this
#endif

class Char 
{
protected:
  char          rep;
#ifdef FAKEVPTR
  void*        fake_vptr;
#endif
public: 
               Char ();
               Char (const char  b);
               Char (const Char& b);
  VIRTUAL      ~Char();

               operator char() const;

  VIRTUAL char   val() const;

  VIRTUAL CharR operator  = (const char);
  VIRTUAL CharR operator  = (const Char&);

  VIRTUAL CharR negate();
  VIRTUAL CharR complement();
  VIRTUAL CharR operator ++ ();
  VIRTUAL CharR operator -- ();

  VIRTUAL CharR operator += (const Char REF );
  VIRTUAL CharR operator -= (const Char REF );
  VIRTUAL CharR operator *= (const Char REF );
  VIRTUAL CharR operator /= (const Char REF );
  VIRTUAL CharR operator %= (const Char REF );
  VIRTUAL CharR operator |= (const Char REF );
  VIRTUAL CharR operator &= (const Char REF );
  VIRTUAL CharR operator ^= (const Char REF );
  VIRTUAL CharR operator <<=(const Char REF );
  VIRTUAL CharR operator >>=(const Char REF );

#ifdef EXPLICIT
  VIRTUAL CharR operator += (const char);
  VIRTUAL CharR operator -= (const char);
  VIRTUAL CharR operator *= (const char);
  VIRTUAL CharR operator /= (const char);
  VIRTUAL CharR operator %= (const char);
  VIRTUAL CharR operator |= (const char);
  VIRTUAL CharR operator &= (const char);
  VIRTUAL CharR operator ^= (const char);
  VIRTUAL CharR operator <<=(const char);
  VIRTUAL CharR operator >>=(const char);

#endif
};

INLINE char  Char::val() const { return rep; }
INLINE      Char::operator char() const { return val(); }

INLINE      Char::Char () :rep(0) {}
INLINE      Char::Char (const char  b) :rep(b) {}
INLINE      Char::Char (const Char& b) :rep(b.Char::val()) {}
INLINE      Char::~Char() {}

INLINE CharR Char::operator  = (const char  b) 
{ rep = b; ReturnCharR; }
INLINE CharR Char::operator  = (const Char&  b) 
{ rep = b.Char::val(); ReturnCharR; }
 
INLINE CharR Char::complement()
{ rep = ~rep; ReturnCharR; }
INLINE CharR Char::negate()
{ rep = -rep; ReturnCharR; }
INLINE CharR Char::operator ++ ()        
{ ++rep; ReturnCharR; }
INLINE CharR Char::operator -- ()        
{ --rep; ReturnCharR; }

INLINE CharR Char::operator += (const Char REF  b) 
{ rep += b.Char::val(); ReturnCharR; }
INLINE CharR Char::operator -= (const Char REF  b) 
{ rep -= b.Char::val(); ReturnCharR; }
INLINE CharR Char::operator *= (const Char REF  b) 
{ rep *= b.Char::val(); ReturnCharR; }
INLINE CharR Char::operator /= (const Char REF  b) 
{ rep /= b.Char::val(); ReturnCharR; }
INLINE CharR Char::operator %= (const Char REF  b) 
{ rep %= b.Char::val(); ReturnCharR; }
INLINE CharR Char::operator |= (const Char REF  b) 
{ rep |= b.Char::val(); ReturnCharR; }
INLINE CharR Char::operator &= (const Char REF  b) 
{ rep &= b.Char::val(); ReturnCharR; }
INLINE CharR Char::operator ^= (const Char REF  b) 
{ rep ^= b.Char::val(); ReturnCharR; }
INLINE CharR Char::operator <<=(const Char REF  b) 
{ rep <<= b.Char::val(); ReturnCharR; }
INLINE CharR Char::operator >>=(const Char REF  b) 
{ rep >>= b.Char::val(); ReturnCharR; }

#ifdef EXPLICIT

INLINE CharR Char::operator += (const char b) 
{ rep += b; ReturnCharR; }
INLINE CharR Char::operator -= (const char b) 
{ rep -= b; ReturnCharR; }
INLINE CharR Char::operator *= (const char b) 
{ rep *= b; ReturnCharR; }
INLINE CharR Char::operator /= (const char b) 
{ rep /= b; ReturnCharR; }
INLINE CharR Char::operator %= (const char b) 
{ rep %= b; ReturnCharR; }
INLINE CharR Char::operator |= (const char b) 
{ rep |= b; ReturnCharR; }
INLINE CharR Char::operator &= (const char b) 
{ rep &= b; ReturnCharR; }
INLINE CharR Char::operator ^= (const char b) 
{ rep ^= b; ReturnCharR; }
INLINE CharR Char::operator <<=(const char b) 
{ rep <<= b; ReturnCharR; }
INLINE CharR Char::operator >>=(const char b) 
{ rep >>= b; ReturnCharR; }


INLINE char& operator  = (char& a,  const Char REF  b) 
{ a = b.Char::val(); return a;}
INLINE char& operator += (char& a,  const Char REF  b) 
{ a += b.Char::val(); return a; }
INLINE char& operator -= (char& a,  const Char REF  b) 
{ a -= b.Char::val(); return a;}
INLINE char& operator *= (char& a,  const Char REF  b) 
{ a *= b.Char::val(); return a;}
INLINE char& operator /= (char& a,  const Char REF  b) 
{ a /= b.Char::val(); return a;}
INLINE char& operator %= (char& a,  const Char REF  b) 
{ a %= b.Char::val(); return a;}
INLINE char& operator |= (char& a,  const Char REF  b) 
{ a |= b.Char::val(); return a;}
INLINE char& operator &= (char& a,  const Char REF  b) 
{ a &= b.Char::val(); return a;}
INLINE char& operator ^= (char& a,  const Char REF  b) 
{ a ^= b.Char::val(); return a;}
INLINE char& operator <<=(char& a,  const Char REF  b) 
{ a <<= b.Char::val(); return a;}
INLINE char& operator >>=(char& a,  const Char REF  b) 
{ a >>= b.Char::val(); return a;}

#endif

#ifdef NO_NRV

INLINE Char  operator -  (const Char REF  a) 
{ Char r(a); r.negate(); return r; }
INLINE Char  operator ~  (const Char REF  a) 
{ Char r(a); r.complement(); return r; }

INLINE Char  operator +  (const Char REF  a, const Char REF  b) 
{ Char r(a); r += b.Char::val(); return r; }
INLINE Char  operator -  (const Char REF  a, const Char REF  b) 
{ Char r(a); r -= b.Char::val(); return r; }
INLINE Char  operator *  (const Char REF  a, const Char REF  b) 
{ Char r(a); r *= b.Char::val(); return r; }
INLINE Char  operator /  (const Char REF  a, const Char REF  b) 
{ Char r(a); r /= b.Char::val(); return r; }
INLINE Char  operator %  (const Char REF  a, const Char REF  b) 
{ Char r(a); r %= b.Char::val(); return r; }
INLINE Char  operator << (const Char REF  a, const Char REF  b) 
{ Char r(a); r <<= b.Char::val(); return r; }
INLINE Char  operator >> (const Char REF  a, const Char REF  b) 
{ Char r(a); r >>= b.Char::val(); return r; }
INLINE Char  operator &  (const Char REF  a, const Char REF  b) 
{ Char r(a); r &= b.Char::val(); return r; }
INLINE Char  operator |  (const Char REF  a, const Char REF  b) 
{ Char r(a); r |= b.Char::val(); return r; }
INLINE Char  operator ^  (const Char REF  a, const Char REF  b) 
{ Char r(a); r ^= b.Char::val(); return r; }

INLINE Char  operator +  (const Char REF  a, const char b) 
{ Char r(a); r += b; return r; }
INLINE Char  operator -  (const Char REF  a, const char b) 
{ Char r(a); r -= b; return r; }
INLINE Char  operator *  (const Char REF  a, const char b) 
{ Char r(a); r *= b; return r; }
INLINE Char  operator /  (const Char REF  a, const char b) 
{ Char r(a); r /= b; return r; }
INLINE Char  operator %  (const Char REF  a, const char b) 
{ Char r(a); r %= b; return r; }
INLINE Char  operator << (const Char REF  a, const char b) 
{ Char r(a); r <<= b; return r; }
INLINE Char  operator >> (const Char REF  a, const char b) 
{ Char r(a); r >>= b; return r; }
INLINE Char  operator &  (const Char REF  a, const char b) 
{ Char r(a); r &= b; return r; }
INLINE Char  operator |  (const Char REF  a, const char b) 
{ Char r(a); r |= b; return r; }
INLINE Char  operator ^  (const Char REF  a, const char b) 
{ Char r(a); r ^= b; return r; }

INLINE Char  operator +  (const char a, const Char REF  b) 
{ Char r(a); r += b.Char::val(); return r; }
INLINE Char  operator -  (const char a, const Char REF  b) 
{ Char r(a); r -= b.Char::val(); return r; }
INLINE Char  operator *  (const char a, const Char REF  b) 
{ Char r(a); r *= b.Char::val(); return r; }
INLINE Char  operator /  (const char a, const Char REF  b) 
{ Char r(a); r /= b.Char::val(); return r; }
INLINE Char  operator %  (const char a, const Char REF  b) 
{ Char r(a); r %= b.Char::val(); return r; }
INLINE Char  operator << (const char a, const Char REF  b) 
{ Char r(a); r <<= b.Char::val(); return r; }
INLINE Char  operator >> (const char a, const Char REF  b) 
{ Char r(a); r >>= b.Char::val(); return r; }
INLINE Char  operator &  (const char a, const Char REF  b) 
{ Char r(a); r &= b.Char::val(); return r; }
INLINE Char  operator |  (const char a, const Char REF  b) 
{ Char r(a); r |= b.Char::val(); return r; }
INLINE Char  operator ^  (const char a, const Char REF  b) 
{ Char r(a); r ^= b.Char::val(); return r; }

#else

INLINE Char  operator -  (const Char REF  a) return r(a)
{ r.negate();  }
INLINE Char  operator ~  (const Char REF  a) return r(a)
{ r.complement();  }

INLINE Char  operator +  (const Char REF  a, const Char REF  b) return r(a)
{ r += b.Char::val();  }
INLINE Char  operator -  (const Char REF  a, const Char REF  b) return r(a)
{ r -= b.Char::val();  }
INLINE Char  operator *  (const Char REF  a, const Char REF  b) return r(a)
{ r *= b.Char::val();  }
INLINE Char  operator /  (const Char REF  a, const Char REF  b) return r(a)
{ r /= b.Char::val();  }
INLINE Char  operator %  (const Char REF  a, const Char REF  b) return r(a)
{ r %= b.Char::val();  }
INLINE Char  operator << (const Char REF  a, const Char REF  b) return r(a)
{ r <<= b.Char::val();  }
INLINE Char  operator >> (const Char REF  a, const Char REF  b) return r(a)
{ r >>= b.Char::val();  }
INLINE Char  operator &  (const Char REF  a, const Char REF  b) return r(a)
{ r &= b.Char::val();  }
INLINE Char  operator |  (const Char REF  a, const Char REF  b) return r(a)
{ r |= b.Char::val();  }
INLINE Char  operator ^  (const Char REF  a, const Char REF  b) return r(a)
{ r ^= b.Char::val();  }

INLINE Char  operator +  (const Char REF  a, const char b) return r(a)
{ r += b;  }
INLINE Char  operator -  (const Char REF  a, const char b) return r(a)
{ r -= b;  }
INLINE Char  operator *  (const Char REF  a, const char b) return r(a)
{ r *= b;  }
INLINE Char  operator /  (const Char REF  a, const char b) return r(a)
{ r /= b;  }
INLINE Char  operator %  (const Char REF  a, const char b) return r(a)
{ r %= b;  }
INLINE Char  operator << (const Char REF  a, const char b) return r(a)
{ r <<= b;  }
INLINE Char  operator >> (const Char REF  a, const char b) return r(a)
{ r >>= b;  }
INLINE Char  operator &  (const Char REF  a, const char b) return r(a)
{ r &= b;  }
INLINE Char  operator |  (const Char REF  a, const char b) return r(a)
{ r |= b;  }
INLINE Char  operator ^  (const Char REF  a, const char b) return r(a)
{ r ^= b;  }

INLINE Char  operator +  (const char a, const Char REF  b) return r(a)
{ r += b.Char::val();  }
INLINE Char  operator -  (const char a, const Char REF  b) return r(a)
{ r -= b.Char::val();  }
INLINE Char  operator *  (const char a, const Char REF  b) return r(a)
{ r *= b.Char::val();  }
INLINE Char  operator /  (const char a, const Char REF  b) return r(a)
{ r /= b.Char::val();  }
INLINE Char  operator %  (const char a, const Char REF  b) return r(a)
{ r %= b.Char::val();  }
INLINE Char  operator << (const char a, const Char REF  b) return r(a)
{ r <<= b.Char::val();  }
INLINE Char  operator >> (const char a, const Char REF  b) return r(a)
{ r >>= b.Char::val();  }
INLINE Char  operator &  (const char a, const Char REF  b) return r(a)
{ r &= b.Char::val();  }
INLINE Char  operator |  (const char a, const Char REF  b) return r(a)
{ r |= b.Char::val();  }
INLINE Char  operator ^  (const char a, const Char REF  b) return r(a)
{ r ^= b.Char::val();  }

#endif

INLINE char  operator !  (const Char REF  a) { return !a.Char::val(); }

INLINE char  operator == (const Char REF  a, const Char REF  b) 
{ return a.Char::val() == b.Char::val(); }
INLINE char  operator != (const Char REF  a, const Char REF  b) 
{ return a.Char::val() != b.Char::val(); }
INLINE char  operator <  (const Char REF  a, const Char REF  b) 
{ return a.Char::val() <  b.Char::val(); }
INLINE char  operator <= (const Char REF  a, const Char REF  b) 
{ return a.Char::val() <= b.Char::val(); }
INLINE char  operator >  (const Char REF  a, const Char REF  b) 
{ return a.Char::val() >  b.Char::val(); }
INLINE char  operator >= (const Char REF  a, const Char REF  b) 
{ return a.Char::val() >= b.Char::val(); }

INLINE char  operator == (const Char REF  a, const char b) 
{ return a.Char::val() == b; }
INLINE char  operator != (const Char REF  a, const char b) 
{ return a.Char::val() != b; }
INLINE char  operator <  (const Char REF  a, const char b) 
{ return a.Char::val() <  b; }
INLINE char  operator <= (const Char REF  a, const char b) 
{ return a.Char::val() <= b; }
INLINE char  operator >  (const Char REF  a, const char b) 
{ return a.Char::val() >  b; }
INLINE char  operator >= (const Char REF  a, const char b) 
{ return a.Char::val() >= b; }

INLINE char  operator == (const char a, const Char REF  b) 
{ return a == b.Char::val(); }
INLINE char  operator != (const char a, const Char REF  b) 
{ return a != b.Char::val(); }
INLINE char  operator <  (const char a, const Char REF  b) 
{ return a <  b.Char::val(); }
INLINE char  operator <= (const char a, const Char REF  b) 
{ return a <= b.Char::val(); }
INLINE char  operator >  (const char a, const Char REF  b) 
{ return a >  b.Char::val(); }
INLINE char  operator >= (const char a, const Char REF  b) 
{ return a >= b.Char::val(); }

#endif
#endif
