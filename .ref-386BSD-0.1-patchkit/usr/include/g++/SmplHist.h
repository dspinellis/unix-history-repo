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

#ifndef SampleHistogram_h
#ifdef __GNUG__
#pragma once
#pragma interface
#endif
#define SampleHistogram_h 1

#include <stream.h>
#include <SmplStat.h>

extern const int SampleHistogramMinimum;
extern const int SampleHistogramMaximum;

class SampleHistogram : public SampleStatistic 
{
protected:
    short howManyBuckets;
    int *bucketCount;
    double *bucketLimit;

public:
    
    SampleHistogram(double low, double hi, double bucketWidth = -1.0);

    ~SampleHistogram();

    virtual void reset();
    virtual void operator+=(double);

    int similarSamples(double);

    int buckets();

    double bucketThreshold(int i);
    int inBucket(int i);
    void printBuckets(ostream&);

};

#if defined(__OPTIMIZE__) || defined(USE_LIBGXX_INLINES)

    inline int SampleHistogram:: buckets() { return(howManyBuckets); };

    inline double SampleHistogram:: bucketThreshold(int i) {
      if (i < 0 || i >= howManyBuckets)
        error("invalid bucket access");
	return(bucketLimit[i]);
    }

    inline int SampleHistogram:: inBucket(int i) {
      if (i < 0 || i >= howManyBuckets)
        error("invalid bucket access");
	return(bucketCount[i]);
    }


#endif

#endif
