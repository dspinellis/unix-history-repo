// This may look like C code, but it is really -*- C++ -*-

/* 
Copyright (C) 1988 Free Software Foundation
    written by Doug Lea (dl@rocky.oswego.edu)

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

/*
  arithmetic, etc. functions on built in types
*/


#ifndef _builtin_h
#ifdef __GNUG__
#pragma once
#pragma interface
#endif
#define _builtin_h 1


typedef void (*one_arg_error_handler_t)(const char*);
typedef void (*two_arg_error_handler_t)(const char*, const char*);



#include <stddef.h>
#include <std.h>
#include <math.h>

long         gcd(long, long);
long         lg(unsigned long); 
double       pow(double, long);
long         pow(long, long);

double       start_timer();
double       return_elapsed_time(double last_time = 0.0);

char*        itoa(long x, int base = 10, int width = 0);
char*        itoa(unsigned long x, int base = 10, int width = 0);
#ifdef __GNUG__
char*        itoa(long long x, int base = 10, int width = 0);
char*        itoa(unsigned long long x, int base = 10, int width = 0);
#endif
char*        dtoa(double x, char cvt = 'g', int width = 0, int prec = 6);

char*        hex(long x, int width = 0);
char*        hex(unsigned long x, int width = 0);
char*        hex(int x, int width = 0);
char*        hex(short x, int width = 0);
char*        hex(unsigned int x, int width = 0);
char*        hex(unsigned short x, int width = 0);

char*        oct(long x, int width = 0);
char*        oct(unsigned long x, int width = 0);
char*        oct(int x, int width = 0);
char*        oct(short x, int width = 0);
char*        oct(unsigned int x, int width = 0) ;
char*        oct(unsigned short x, int width = 0); 

char*        dec(long x, int width = 0);
char*        dec(unsigned long x, int width = 0);
char*        dec(int x, int width = 0);
char*        dec(short x, int width = 0);
char*        dec(unsigned int x, int width = 0) ;
char*        dec(unsigned short x, int width = 0); 

char*        form(const char* fmt ...);
char*        chr(char ch, int width = 0);
char*        str(const char* s, int width = 0);

unsigned int hashpjw(const char*);
unsigned int multiplicativehash(int);
unsigned int foldhash(double);

extern void default_one_arg_error_handler(const char*);
extern void default_two_arg_error_handler(const char*, const char*);

extern two_arg_error_handler_t lib_error_handler;

extern two_arg_error_handler_t 
       set_lib_error_handler(two_arg_error_handler_t f);


double abs(double arg);
float abs(float arg);
short abs(short arg);
long abs(long arg);
int sign(long arg);
int sign(double arg);
long sqr(long arg);
double sqr(double arg);
int even(long arg);
int odd(long arg);
long lcm(long x, long y);
void setbit(long& x, long b);
void clearbit(long& x, long b);
int testbit(long x, long b);

signed char min(signed char a, signed char b);
unsigned char min(unsigned char a, unsigned char b);

signed short min(signed short a, signed short b);
unsigned short min(unsigned short a, unsigned short b);

signed int min(signed int a, signed int b);
unsigned int min(unsigned int a, unsigned int b);

signed long min(signed long a, signed long b);
unsigned long min(unsigned long a, unsigned long b);

float min(float a, float b);

double min(double a, double b);

signed char max(signed char a, signed char b);
unsigned char max(unsigned char a, unsigned char b);

signed short max(signed short a, signed short b);
unsigned short max(unsigned short a, unsigned short b);

signed int max(signed int a, signed int b);
unsigned int max(unsigned int a, unsigned int b);

signed long max(signed long a, signed long b);
unsigned long max(unsigned long a, unsigned long b);

float max(float a, float b);

double max(double a, double b);

#if defined(__OPTIMIZE__) || defined(USE_LIBGXX_INLINES)


inline double abs(double arg) 
{
  return (arg < 0.0)? -arg : arg;
}

inline float abs(float arg) 
{
  return (arg < 0.0)? -arg : arg;
}

inline short abs(short arg) 
{
  return (arg < 0)? -arg : arg;
}

inline long abs(long arg) 
{
  return (arg < 0)? -arg : arg;
}

inline int sign(long arg)
{
  return (arg == 0) ? 0 : ( (arg > 0) ? 1 : -1 );
}

inline int sign(double arg)
{
  return (arg == 0.0) ? 0 : ( (arg > 0.0) ? 1 : -1 );
}

inline long sqr(long arg)
{
  return arg * arg;
}

inline double sqr(double arg)
{
  return arg * arg;
}

inline int even(long arg)
{
  return !(arg & 1);
}

inline int odd(long arg)
{
  return (arg & 1);
}

inline long lcm(long x, long y)
{
  return x / gcd(x, y) * y;
}

inline void setbit(long& x, long b)
{
  x |= (1 << b);
}

inline void clearbit(long& x, long b)
{
  x &= ~(1 << b);
}

inline int testbit(long x, long b)
{
  return ((x & (1 << b)) != 0);
}

inline char* hex(int x, int width = 0) { return hex(long(x), width); }
inline char* hex(short x, int width = 0) { return hex(long(x), width); }
inline char* hex(unsigned int x, int width = 0) 
{ return hex((unsigned long)(x), width); }
inline char* hex(unsigned short x, int width = 0) 
{ return hex((unsigned long)(x), width); }

inline char* oct(int x, int width = 0) { return oct(long(x), width); }
inline char* oct(short x, int width = 0) { return oct(long(x), width); }
inline char* oct(unsigned int x, int width = 0) 
{ return oct((unsigned long)(x), width); }
inline char* oct(unsigned short x, int width = 0) 
{ return oct((unsigned long)(x), width); }

inline char* dec(int x, int width = 0) { return dec(long(x), width); }
inline char* dec(short x, int width = 0) { return dec(long(x), width); }
inline char* dec(unsigned int x, int width = 0) 
{ return dec((unsigned long)(x), width); }
inline char* dec(unsigned short x, int width = 0) 
{ return dec((unsigned long)(x), width); }

inline signed char min(signed char a, signed char b) { return (a < b)?a:b;}
inline unsigned char min(unsigned char a, unsigned char b) {return (a < b)?a:b;}

inline signed short min(signed short a, signed short b) {return (a < b) ?a:b;}
inline unsigned short min(unsigned short a, unsigned short b) {return (a < b)?a:b;}

inline signed int min(signed int a, signed int b) {return (a < b)?a:b;}
inline unsigned int min(unsigned int a, unsigned int b) {return (a < b)?a:b;}

inline signed long min(signed long a, signed long b) {return (a < b)?a:b;}
inline unsigned long min(unsigned long a, unsigned long b) {return (a < b)?a:b;}

inline float min(float a, float b) {return (a < b)?a:b;}

inline double min(double a, double b) {return (a < b)?a:b;}

inline signed char max(signed char a, signed char b) { return (a > b)?a:b;}
inline unsigned char max(unsigned char a, unsigned char b) {return (a > b)?a:b;}

inline signed short max(signed short a, signed short b) {return (a > b) ?a:b;}
inline unsigned short max(unsigned short a, unsigned short b) {return (a > b)?a:b;}

inline signed int max(signed int a, signed int b) {return (a > b)?a:b;}
inline unsigned int max(unsigned int a, unsigned int b) {return (a > b)?a:b;}

inline signed long max(signed long a, signed long b) {return (a > b)?a:b;}
inline unsigned long max(unsigned long a, unsigned long b) {return (a > b)?a:b;}

inline float max(float a, float b) {return (a > b)?a:b;}

inline double max(double a, double b) {return (a > b)?a:b;}

#endif

#endif
