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
#ifndef _HyperGeometric_h
#ifdef __GNUG__
#pragma once
#pragma interface
#endif
#define _HyperGeometric_h 

#include <Random.h>

class HyperGeometric: public Random {
protected:
    double pMean;
    double pVariance;
    double pP;
    void setState();

public:
    HyperGeometric(double mean, double variance, RNG *gen);

    double mean();
    double mean(double x);
    double variance();
    double variance(double x);

    virtual double operator()();
};

#if defined(__OPTIMIZE__) || defined(USE_LIBGXX_INLINES)

inline void HyperGeometric::setState() {
  double z = pVariance / (pMean * pMean);
  pP = 0.5 * (1.0 - sqrt((z - 1.0) / ( z + 1.0 )));
}

inline HyperGeometric::HyperGeometric(double mean, double variance, RNG *gen)
: (gen) {
  pMean = mean; pVariance = variance;
  setState();
}

inline double HyperGeometric::mean() { return pMean; };

inline double HyperGeometric::mean(double x) {
  double t = pMean; pMean = x;
  setState(); return t;
}

inline double HyperGeometric::variance() { return pVariance; }

inline double HyperGeometric::variance(double x) {
  double t = pVariance; pVariance = x;
  setState(); return t;
}

#endif
#endif
