//	@(#)Sample.cc	6.2 (Berkeley) 2/25/91

// Modified for Berkeley Unix by Donn Seeley, donn@okeeffe.berkeley.edu

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
#include <SmplStat.h>
#include <math.h>

// error handling

void default_SampleStatistic_error_handler(const char* msg)
{
  cerr << "Fatal SampleStatistic error. " << msg << "\n";
  exit(1);
}

one_arg_error_handler_t SampleStatistic_error_handler = default_SampleStatistic_error_handler;

one_arg_error_handler_t set_SampleStatistic_error_handler(one_arg_error_handler_t f)
{
  one_arg_error_handler_t old = SampleStatistic_error_handler;
  SampleStatistic_error_handler = f;
  return old;
}

void SampleStatistic::error(const char* msg)
{
  (*SampleStatistic_error_handler)(msg);
}

// t-distribution: given p-value and degrees of freedom, return t-value
// adapted from Peizer & Pratt JASA, vol63, p1416

double tval(double p, int df) 
{
  double t;
  int positive = p >= 0.5;
  p = (positive)? 1.0 - p : p;
  if (p <= 0.0 || df <= 0)
    t = HUGE;
  else if (p == 0.5)
    t = 0.0;
  else if (df == 1)
    t = 1.0 / tan((p + p) * 1.57079633);
  else if (df == 2)
    t = sqrt(1.0 / ((p + p) * (1.0 - p)) - 2.0);
  else
  {	
    double ddf = df;
    double a = sqrt(log(1.0 / (p * p)));
    double aa = a * a;
    a = a - ((2.515517 + (0.802853 * a) + (0.010328 * aa)) /
             (1.0 + (1.432788 * a) + (0.189269 * aa) +
              (0.001308 * aa * a)));
    t = ddf - 0.666666667 + 1.0 / (10.0 * ddf);
    t = sqrt(ddf * (exp(a * a * (ddf - 0.833333333) / (t * t)) - 1.0));
  }
  return (positive)? t : -t;
}

void
SampleStatistic::reset()
{
    n = 0; x = x2 = 0.0;
    maxValue = -HUGE;
    minValue = HUGE;
}

void
SampleStatistic::operator+=(double value)
{
    n += 1;
    x += value;
    x2 += (value * value);
    if ( minValue > value) minValue = value;
    if ( maxValue < value) maxValue = value;
}

double
SampleStatistic::mean()
{
    if ( n > 0) {
	return (x / n);
    }
    else {
	return ( 0.0 );
    }
}

double
SampleStatistic::var()
{
    if ( n > 1) {
	return(( x2 - ((x * x) /  n)) / ( n - 1));
    }
    else {
	return ( 0.0 );
    }
}

double
SampleStatistic::stdDev()
{
    if ( n <= 0 || this -> var() <= 0) {
	return(0);
    } else {
	return( (double) sqrt( var() ) );
    }
}

double
SampleStatistic::confidence(int interval)
{
  int df = n - 1;
  if (df <= 0) return HUGE;
  double t = tval(double(100 + interval) * 0.005, df);
  if (t == HUGE)
    return t;
  else
    return (t * stdDev()) / sqrt(double(n));
}

double
SampleStatistic::confidence(double p_value)
{
  int df = n - 1;
  if (df <= 0) return HUGE;
  double t = tval((1.0 + p_value) * 0.5, df);
  if (t == HUGE)
    return t;
  else
    return (t * stdDev()) / sqrt(double(n));
}


#include <SmplHist.h>

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

