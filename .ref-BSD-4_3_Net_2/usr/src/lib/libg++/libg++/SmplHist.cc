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
#ifdef __GNUG__
#pragma implementation
#endif
#include <stream.h>
#include <SmplHist.h>
#include <math.h>


const int SampleHistogramMinimum = -2;
const int SampleHistogramMaximum = -1;

SampleHistogram::SampleHistogram(double low, double high, double width)
{
    if (high < low) {
	double t = high;
	high = low;
	low = t;
    }

    if (width == -1) {
	width = (high - low) / 10;
    }

    howManyBuckets = int((high - low) / width) + 2;
    bucketCount = new int[howManyBuckets];
    bucketLimit = new double[howManyBuckets];
    double lim = low;
    for (int i = 0; i < howManyBuckets; i++) {
	bucketCount[i] = 0;
	bucketLimit[i] = lim;
	lim += width;
    }
    bucketLimit[howManyBuckets-1] = HUGE;	/* from math.h */
}

SampleHistogram::~SampleHistogram()
{
    if (howManyBuckets > 0) {
	delete bucketCount;
	delete bucketLimit;
    }
}

void
SampleHistogram::operator+=(double value)
{
    int i;
    for (i = 0; i < howManyBuckets; i++) {
	if (value < bucketLimit[i]) break;
    }
    bucketCount[i]++;
    this->SampleStatistic::operator+=(value);
}

int
SampleHistogram::similarSamples(double d)
{
    int i;
    for (i = 0; i < howManyBuckets; i++) {
	if (d < bucketLimit[i]) return(bucketCount[i]);
    }
    return(0);
}

void
SampleHistogram::printBuckets(ostream& s)
{
    for(int i = 0; i < howManyBuckets; i++) {
	if (bucketLimit[i] >= HUGE) {
	    s << "< max : " << bucketCount[i] << "\n";
	} else {
	    s << "< " << bucketLimit[i] << " : " << bucketCount[i] << "\n";
	}
    }
}

void
SampleHistogram::reset()
{
    this->SampleStatistic::reset();
    if (howManyBuckets > 0) {
	for (register int i = 0; i < howManyBuckets; i++) {
	    bucketCount[i] = 0;
	}
    }
}

