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
#ifndef _Weibull_h
#ifdef __GNUG__
#pragma once
#pragma interface
#endif
#define _Weibull_h 

#include <Random.h>

class Weibull: public Random {
protected:
    double pAlpha;
    double pInvAlpha;
    double pBeta;

    void setState();
    
public:
    Weibull(double alpha, double beta, RNG *gen);

    double alpha();
    double alpha(double x);

    double beta();
    double beta(double x);

    virtual double operator()();
};

#if defined(__OPTIMIZE__) || defined(USE_LIBGXX_INLINES)

inline void Weibull::setState() {
  pInvAlpha = 1.0 / pAlpha;
}
    
inline Weibull::Weibull(double alpha, double beta,  RNG *gen) : (gen)
{
  pAlpha = alpha;
  pBeta = beta;
  setState();
}

inline double Weibull::alpha() { return pAlpha; }

inline double Weibull::alpha(double x) {
  double tmp = pAlpha;
  pAlpha = x;
  setState();
  return tmp;
}

inline double Weibull::beta() { return pBeta; };
inline double Weibull::beta(double x) {
  double tmp = pBeta;
  pBeta = x;
  return tmp;
};


#endif
#endif
