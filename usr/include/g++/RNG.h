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
#ifndef _RNG_h
#define _RNG_h 1
#ifdef __GNUG__
#pragma once
#pragma interface
#endif

#include <assert.h>
#include <math.h>

union PrivateRNGSingleType {		   	// used to access floats as unsigneds
    float s;
    unsigned long u;
};

union PrivateRNGDoubleType {		   	// used to access doubles as unsigneds
    double d;
    unsigned long u[2];
};

//
// Base class for Random Number Generators. See ACG and MLCG for instances.
//
class RNG {
    static PrivateRNGSingleType singleMantissa;	// mantissa bit vector
    static PrivateRNGDoubleType doubleMantissa;	// mantissa bit vector
public:
    RNG();
    //
    // Return a long-words word of random bits
    //
    virtual unsigned long asLong() = 0;
    virtual void reset() = 0;
    //
    // Return random bits converted to either a float or a double
    //
    float asFloat();
    double asDouble();
};

#if defined(__OPTIMIZE__) || defined(USE_LIBGXX_INLINES)


inline float RNG::asFloat()
{
    PrivateRNGSingleType result;
    result.s = 1.0;
    result.u |= (asLong() & singleMantissa.u);
    result.s -= 1.0;
    assert( result.s < 1.0 && result.s >= 0);
    return( result.s );
}
	
inline double RNG::asDouble()
{
    PrivateRNGDoubleType result;
    result.d = 1.0;
    result.u[0] |= (asLong() & doubleMantissa.u[0]);
    result.u[1] |= (asLong() & doubleMantissa.u[1]);
    result.d -= 1.0;
    assert( result.d < 1.0 && result.d >= 0);
    return( result.d );
}

#endif
#endif
