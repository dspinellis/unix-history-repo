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
#ifndef _LogNormal_h
#ifdef __GNUG__
#pragma once
#pragma interface
#endif
#define _LogNormal_h 

#include "Normal.h"

class LogNormal: public Normal {
protected:
    double logMean;
    double logVariance;
    void setState();
public:
    LogNormal(double mean, double variance, RNG *gen);
    double mean();
    double mean(double x);
    double variance();
    double variance(double x);
    virtual double operator()();
};

#if defined(__OPTIMIZE__) || defined(USE_LIBGXX_INLINES)

inline void LogNormal::setState()
{
    double m2 = logMean * logMean;
    pMean = log(m2 / sqrt(logVariance + m2) );
    pVariance = log((sqrt(logVariance + m2)/m2 ));
}

inline LogNormal::LogNormal(double mean, double variance, RNG *gen)
    : (mean, variance, gen)
{
    logMean = mean;
    logVariance = variance;
    setState();
}

inline double LogNormal::mean() {
    return logMean;
}

inline double LogNormal::mean(double x)
{
    double t=logMean; logMean = x; setState();
    return t;
}

inline double LogNormal::variance() {
    return logVariance;
}

inline double LogNormal::variance(double x)
{
    double t=logVariance; logVariance = x; setState();
    return t;
}

#endif
#endif
