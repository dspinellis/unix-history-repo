// This may look like C code, but it is really -*- C++ -*-
/* 
Copyright (C) 1988 Free Software Foundation
    written by Dirk Grunwald (grunwald@cs.uiuc.edu)

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
#ifndef _Binomial_h
#ifdef __GNUG__
#pragma once
#pragma interface
#endif
#define _Binomial_h 1

#include <Random.h>

class Binomial: public Random {
protected:
    int pN;
    double pU;
public:
    Binomial(int n, double u, RNG *gen);

    int n();
    int n(int xn);

    double u();
    double u(int xu);

    virtual double operator()();

};

#if defined(__OPTIMIZE__) || defined(USE_LIBGXX_INLINES)

inline Binomial::Binomial(int n, double u, RNG *gen)
: (gen){
  pN = n; pU = u;
}

inline int Binomial::n() { return pN; }
inline int Binomial::n(int xn) { int tmp = pN; pN = xn; return tmp; }

inline double Binomial::u() { return pU; }
inline double Binomial::u(int xu) { double tmp = pU; pU = xu; return tmp; }

#endif
#endif
