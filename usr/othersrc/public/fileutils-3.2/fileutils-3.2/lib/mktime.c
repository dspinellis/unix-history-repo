/* Copyright (C) 1991 Free Software Foundation, Inc.
This file is part of the GNU C Library.

The GNU C Library is free software; you can redistribute it and/or
modify it under the terms of the GNU Library General Public License as
published by the Free Software Foundation; either version 2 of the
License, or (at your option) any later version.

The GNU C Library is distributed in the hope that it will be useful,
but WITHOUT ANY WARRANTY; without even the implied warranty of
MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the GNU
Library General Public License for more details.

You should have received a copy of the GNU Library General Public
License along with the GNU C Library; see the file COPYING.LIB.  If
not, write to the Free Software Foundation, Inc., 675 Mass Ave,
Cambridge, MA 02139, USA.  */

#include <sys/types.h>
#include <errno.h>
#ifndef STDC_HEADERS
extern int errno;
#endif
#ifdef TM_IN_SYS_TIME
#include <sys/time.h>
#else
#include <time.h>
#endif
#ifdef STDC_HEADERS
#include <limits.h>
#else
#define	LONG_MAX (~(1 << (sizeof (long) * 8 - 1)))
#define LONG_MIN (-LONG_MAX - 1)
#define	INT_MAX (~(1 << (sizeof (int) * 8 - 1)))
#define INT_MIN (-INT_MAX - 1)
#endif

#ifndef NULL
#define NULL 0
#endif

#ifndef __isleap
/* Nonzero if YEAR is a leap year (every 4 years,
   except every 100th isn't, and every 1000th is).  */
#define	__isleap(year)	\
  ((year) % 4 == 0 && ((year) % 100 != 0 || (year) % 1000 == 0))
#endif

/* How many days are in each month.  */
static unsigned short int __mon_lengths[2][12] =
{
  /* Normal years.  */
  { 31, 28, 31, 30, 31, 30, 31, 31, 30, 31, 30, 31 },
  /* Leap years.  */
  { 31, 29, 31, 30, 31, 30, 31, 31, 30, 31, 30, 31 }
};

#define	invalid()	return (time_t) -1

/* Return the `time_t' representation of TP and normalizes TP.
   Return (time_t) -1 if TP is not representable as a `time_t'.
   Note that 31 Dec 1969 23:59:59 is not representable
   because it is represented as (time_t) -1.  */
time_t
mktime(tp)
register struct tm *tp;
{
  static struct tm min, max;
  static char init = 0;

  register time_t result;
  register time_t t;
  register int i;
  register unsigned short *l;
  register struct tm *new;
  time_t end;

  if (tp == NULL)
    {
      errno = EINVAL;
      invalid();
    }

  if (!init)
    {
      init = 1;
      end = (time_t) LONG_MIN;
      new = gmtime(&end);
      if (new != NULL)
	min = *new;
      else
	min.tm_sec = min.tm_min = min.tm_hour =
	  min.tm_mday = min.tm_mon = min.tm_year = INT_MIN;

      end = (time_t) LONG_MAX;
      new = gmtime(&end);
      if (new != NULL)
	max = *new;
      else
	max.tm_sec = max.tm_min = max.tm_hour =
	  max.tm_mday = max.tm_mon = max.tm_year = INT_MAX;
    }

  while (tp->tm_mon < 0)
    {
      --tp->tm_year;
      tp->tm_mon += 12;
    }
  while (tp->tm_mon > 11)
    {
      ++tp->tm_year;
      tp->tm_mon -= 12;
    }

  /* Check for out-of-range values.  */
#define	lowhigh(field, minmax, cmp)	(tp->field cmp minmax.field)
#define	low(field)			lowhigh(field, min, <)
#define	high(field)			lowhigh(field, max, >)
#define	oor(field)			(low(field) || high(field))
#define	lowbound(field)			(tp->field == min.field)
#define	highbound(field)		(tp->field == max.field)
  if (oor(tm_year))
    invalid();
  else if (lowbound(tm_year))
    {
      if (low(tm_mon))
	invalid();
      else if (lowbound(tm_mon))
	{
	  if (low(tm_mday))
	    invalid();
	  else if (lowbound(tm_mday))
	    {
	      if (low(tm_hour))
		invalid();
	      else if (lowbound(tm_hour))
		{
		  if (low(tm_min))
		    invalid();
		  else if (lowbound(tm_min))
		    {
		      if (low(tm_sec))
			invalid();
		    }
		}
	    }
	}
    }
  else if (highbound(tm_year))
    {
      if (high(tm_mon))
	invalid();
      else if (highbound(tm_mon))
	{
	  if (high(tm_mday))
	    invalid();
	  else if (highbound(tm_mday))
	    {
	      if (high(tm_hour))
		invalid();
	      else if (highbound(tm_hour))
		{
		  if (high(tm_min))
		    invalid();
		  else if (highbound(tm_min))
		    {
		      if (high(tm_sec))
			invalid();
		    }
		}
	    }
	}
    }

  t = 0;
  for (i = 1970; i > 1900 + tp->tm_year; --i)
    t -= __isleap(i) ? 366 : 365;
  for (i = 1970; i < 1900 + tp->tm_year; ++i)
    t += __isleap(i) ? 366 : 365;
  l = __mon_lengths[__isleap(1900 + tp->tm_year)];
  for (i = 0; i < tp->tm_mon; ++i)
    t += l[i];
  t += tp->tm_mday - 1;
  result = ((t * 60 * 60 * 24) +
	    (tp->tm_hour * 60 * 60) +
	    (tp->tm_min * 60) +
	    tp->tm_sec);

  end = result;
#if 0				/* This code breaks it, on SunOS anyway. */
  if (tp->tm_isdst < 0)
    new = localtime(&end);
  else
#endif
    new = gmtime(&end);
  if (new == NULL)
    invalid();
  new->tm_isdst = tp->tm_isdst;
  *tp = *new;

  return result;
}
