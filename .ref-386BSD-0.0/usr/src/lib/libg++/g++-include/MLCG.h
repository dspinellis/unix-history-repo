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
#ifndef _MLCG_h
#define _MLCG_h 1 
#ifdef __GNUG__
#pragma once
#pragma interface
#endif

#include <RNG.h>
#include <math.h>

//
//	Multiplicative Linear Conguential Generator
//

class MLCG : public RNG {
    long initialSeedOne;
    long initialSeedTwo;
    long seedOne;
    long seedTwo;

protected:

public:
    MLCG(long seed1 = 0, long seed2 = 1);
    //
    // Return a long-words word of random bits
    //
    virtual unsigned long asLong();
    virtual void reset();
    long seed1();
    void seed1(long);
    long seed2();
    void seed2(long);
    void reseed(long, long);
};

#if defined(__OPTIMIZE__) || defined(USE_LIBGXX_INLINES)

inline long
MLCG::seed1()
{
    return(seedOne);
}

inline void
MLCG::seed1(long s)
{
    initialSeedOne = s;
    reset();
}

inline long
MLCG::seed2()
{
    return(seedTwo);
}

inline void
MLCG::seed2(long s)
{
    initialSeedTwo = s;
    reset();
}

inline void
MLCG::reseed(long s1, long s2)
{
    initialSeedOne = s1;
    initialSeedTwo = s2;
    reset();
}

#endif

#endif
