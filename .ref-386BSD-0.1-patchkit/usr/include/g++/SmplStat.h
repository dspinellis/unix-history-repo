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
#ifndef SampleStatistic_h
#ifdef __GNUG__
#pragma once
#pragma interface
#endif
#define SampleStatistic_h 1

#include <builtin.h>

class SampleStatistic {
protected:
    int	n;
    double x;
    double x2;
    double minValue, maxValue;

    public :

    SampleStatistic();
    virtual void reset(); 

    virtual void operator+=(double);
    int  samples();
    double mean();
    double stdDev();
    double var();
    double min();
    double max();
    double confidence(int p_percentage);
    double confidence(double p_value);

    void            error(const char* msg);
};

// error handlers

extern  void default_SampleStatistic_error_handler(const char*);
extern  one_arg_error_handler_t SampleStatistic_error_handler;

extern  one_arg_error_handler_t 
        set_SampleStatistic_error_handler(one_arg_error_handler_t f);

#if defined(__OPTIMIZE__) || defined(USE_LIBGXX_INLINES)

    inline SampleStatistic:: SampleStatistic(){ reset();}
    inline int SampleStatistic::  samples() {return(n);}
    inline double SampleStatistic:: min() {return(minValue);}
    inline double SampleStatistic:: max() {return(maxValue);}

#endif
#endif
