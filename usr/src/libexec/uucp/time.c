/* time.c
   Routines to deal with UUCP time strings.

   Copyright (C) 1991, 1992 Ian Lance Taylor

   This file is part of the Taylor UUCP package.

   This program is free software; you can redistribute it and/or
   modify it under the terms of the GNU General Public License as
   published by the Free Software Foundation; either version 2 of the
   License, or (at your option) any later version.

   This program is distributed in the hope that it will be useful, but
   WITHOUT ANY WARRANTY; without even the implied warranty of
   MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the GNU
   General Public License for more details.

   You should have received a copy of the GNU General Public License
   along with this program; if not, write to the Free Software
   Foundation, Inc., 675 Mass Ave, Cambridge, MA 02139, USA.

   The author of the program may be contacted at ian@airs.com or
   c/o AIRS, P.O. Box 520, Waltham, MA 02254.

   $Log: time.c,v $
   Revision 1.12  1992/03/17  00:32:40  ian
   Cast argument to qttime_parse

   Revision 1.11  1992/03/09  05:08:16  ian
   Added status for wrong time to call, not used if system can't be called

   Revision 1.10  1992/03/08  01:52:34  ian
   Removed extraneous semicolons

   Revision 1.9  1992/03/07  02:56:30  ian
   Rewrote time routines

   Revision 1.8  1992/02/08  03:54:18  ian
   Include <string.h> only in <uucp.h>, added 1992 copyright

   Revision 1.7  1992/01/11  17:30:10  ian
   John Antypas: use memcpy instead of relying on structure assignment

   Revision 1.6  1991/12/29  04:04:18  ian
   Added a bunch of extern definitions

   Revision 1.5  1991/12/22  20:57:57  ian
   Added externs for strcasecmp or strncasecmp

   Revision 1.4  1991/09/19  02:22:44  ian
   Chip Salzenberg's patch to allow ";retrytime" at the end of a time string

   Revision 1.3  1991/09/12  05:04:44  ian
   Wrong sense of comparison in btime_low_grade

   Revision 1.2  1991/09/11  16:59:00  ian
   fcheck_time and btime_low_grade looped endlessly on unusual grades
  
   Revision 1.1  1991/09/10  19:40:31  ian
   Initial revision
  
   */

#include "uucp.h"

#if USE_RCS_ID
char time_rcsid[] = "$Id: time.c,v 1.12 1992/03/17 00:32:40 ian Rel $";
#endif

#include <ctype.h>

#if HAVE_TIME_H
#include <time.h>
#endif

#if ! HAVE_TIME_T
#if HAVE_SYS_TIME_T
#include <sys/types.h>
#endif /* HAVE_SYS_TIME_T */
#endif /* ! HAVE_TIME_T */

#include "uutime.h"

/* External functions.  */
extern int strncasecmp ();
extern time_t time ();
extern struct tm *localtime ();

/* Local functions.  */

static struct sspan *qtnew P((struct sspan *qnext, long ival,
			      int istart, int iend, int cretry));
static struct sspan *qtadd_span P((struct sspan *qlist, long ival,
				   int istart, int iend,
				   boolean (*picmp) P((long, long)),
				   int cretry));
static struct sspan *qttime_parse P((const char *ztime,
				     struct sspan *qlist, long ival,
				     boolean (*picmp) P((long, long)),
				     int cretry));

/* A helper function to create a new time span with the specified
   arguments.  */

static struct sspan *
qtnew (qnext, ival, istart, iend, cretry)
     struct sspan *qnext;
     long ival;
     int istart;
     int iend;
     int cretry;
{
  struct sspan *q;

  q = (struct sspan *) xmalloc (sizeof (struct sspan));
  q->qnext = qnext;
  q->ival = ival;
  q->istart = istart;
  q->iend = iend;
  q->cretry = cretry;
  return q;
}

/* A simple function to free a list of time spans.  */

void
utimespan_free (q)
     struct sspan *q;
{
  while (q != NULL)
    {
      struct sspan *qnext;

      qnext = q->qnext;
      xfree ((pointer) q);
      q = qnext;
    }
}

/* Add a time span to an existing list of time spans.  We keep the
   list sorted by time to make this operation easier.  This modifies
   the existing list, and returns the modified version.  It takes a
   comparison function which should return < 0 if the first argument
   should take precedence over the second argument and == 0 if they
   are the same (for grades this is igradecmp; for sizes it is minus
   (the binary operator)).  */

static struct sspan *
qtadd_span (qlist, ival, istart, iend, picmp, cretry)
     struct sspan *qlist;
     long ival;
     int istart;
     int iend;
     boolean (*picmp) P((long, long));
     int cretry;
{
  struct sspan **pq;

  /* istart < iend  */
  for (pq = &qlist; *pq != NULL; pq = &(*pq)->qnext)
    {
      int icmp;

      /* Invariant: PREV (*pq) == NULL || PREV (*pq)->iend <= istart  */
      /* istart < iend && (*pq)->istart < (*pq)->iend  */

      if (iend <= (*pq)->istart)
	{
	  /* istart < iend <= (*pq)->istart < (*pq)->iend  */
	  /* No overlap, and we're at the right spot.  See if we can
	     combine these spans.  */
	  if (iend == (*pq)->istart
	      && cretry == (*pq)->cretry
	      && (*picmp) (ival, (*pq)->ival) == 0)
	    {
	      (*pq)->istart = istart;
	      return qlist;
	    }
	  /* We couldn't combine them.  */
	  break;
	}

      if ((*pq)->iend <= istart)
	{
	  /* (*pq)->istart < (*pq)->iend <= istart < iend  */
	  /* No overlap.  Try attaching this span.  */
	  if ((*pq)->iend == istart
	      && (*pq)->cretry == cretry
	      && ((*pq)->qnext == NULL
		  || iend <= (*pq)->qnext->istart)
	      && (*picmp) (ival, (*pq)->ival) == 0)
	    {
	      (*pq)->iend = iend;
	      return qlist;
	    }
	  /* Couldn't attach; keep looking for the right spot.  We
	     might be able to combine part of the new span onto an
	     existing span, but it's probably not worth it.  */
	  continue;
	}

      /* istart < iend
	 && (*pq)->istart < (*pq)->iend
	 && istart < (*pq)->iend
	 && (*pq)->istart < iend  */
      /* Overlap.  */

      icmp = (*picmp) (ival, (*pq)->ival);

      if (icmp == 0)
	{
	  /* Just expand the old span to include the new span.  */
	  if (istart < (*pq)->istart)
	    (*pq)->istart = istart;
	  if ((*pq)->iend < iend)
	    (*pq)->iend = iend;
	}
      else if (icmp < 0)
	{
	  /* Replace the old span with the new span.  */
	  if ((*pq)->istart < istart)
	    {
	      /* Save the initial portion of the old span.  */
	      *pq = qtnew (*pq, (*pq)->ival, (*pq)->istart, istart,
			   (*pq)->cretry);
	      pq = &(*pq)->qnext;
	    }
	  if (iend < (*pq)->iend)
	    {
	      /* Save the final portion of the old span.  */
	      (*pq)->qnext = qtnew ((*pq)->qnext, (*pq)->ival, iend,
				    (*pq)->iend, (*pq)->cretry);
	    }
	  (*pq)->ival = ival;
	  (*pq)->istart = istart;
	  (*pq)->iend = iend;
	  (*pq)->cretry = cretry;
	}
      else
	{
	  /* Leave the old span untouched.  */
	  if (istart < (*pq)->istart)
	    {
	      /* Put in the initial portion of the new span.  */
	      *pq = qtnew (*pq, ival, istart, (*pq)->istart, cretry);
	      pq = &(*pq)->qnext;
	    }
	  if ((*pq)->iend < iend)
	    {
	      /* Put in the final portion of the new span.  */
	      (*pq)->qnext = qtnew ((*pq)->qnext, ival, (*pq)->iend,
				    iend, cretry);
	    }
	}

      return qlist;
    }

  /* This is the spot for the new span, and there's no overlap.  */

  *pq = qtnew (*pq, ival, istart, iend, cretry);

  return qlist;
}

/* An array of weekday abbreviations.  The code below assumes that
   each one starts with a lower case letter.  */

static const struct
{
  const char *zname;
  int imin;
  int imax;
} asTdays[] =
{
  { "any", 0, 6 },
  { "wk", 1, 5 },
  { "su", 0, 0 },
  { "mo", 1, 1 },
  { "tu", 2, 2 },
  { "we", 3, 3 },
  { "th", 4, 4 },
  { "fr", 5, 5 },
  { "sa", 6, 6 },
  { "never", -1, -2 },
  { NULL, 0, 0 }
};

/* Parse a time string and add it to a span list.  This function is
   given the value and comparison function to use.  The time string
   continues to a null byte, a space or a semicolon.  This returns the
   new span list, or NULL on error.  If no time matches, it will wind
   up returning qlist, which may itself be NULL.  */

static struct sspan *
qttime_parse (ztime, qlist, ival, picmp, cretry)
     const char *ztime;
     struct sspan *qlist;
     long ival;
     int (*picmp) P((long, long));
     int cretry;
{
  const char *zend;
  char bfirst;
  int i;

  zend = ztime + strcspn (ztime, "; ");

  if (pasTtable == NULL)
    uinittimetables ();

  /* Expand the string using a timetable.  */
  bfirst = tolower (BUCHAR (*ztime));
  for (i = 0; i < cTtable; i++)
    {
      if (bfirst == tolower (BUCHAR (pasTtable[i].zname[0]))
	  && strncasecmp (ztime, pasTtable[i].zname, zend - ztime) == 0)
	{
	  ztime = pasTtable[i].ztime;
	  zend = ztime + strlen (ztime);
	  /* Now search the table for this string.  */
	  i = -1;
	}
    }

  /* Look through the portions of the time string separated by a
     comma or a vertical bar.  */

  for (; ztime < zend; ztime += strcspn (ztime, ",|"))
    {
      int iday;
      boolean afday[7];
      const char *z;
      int istart, iend;

      if (*ztime == ',' || *ztime == '|')
	++ztime;

      for (iday = 0; iday < 7; iday++)
	afday[iday] = FALSE;

      /* Get the days.  */

      z = ztime;
      do
	{
	  bfirst = tolower (BUCHAR (*z));
	  for (iday = 0; asTdays[iday].zname != NULL; iday++)
	    {
	      int clen;

	      if (bfirst != asTdays[iday].zname[0])
		continue;

	      clen = strlen (asTdays[iday].zname);
	      if (strncasecmp (z, asTdays[iday].zname, clen) == 0)
		{
		  int iset;

		  for (iset = asTdays[iday].imin;
		       iset <= asTdays[iday].imax;
		       iset++)
		    afday[iset] = TRUE;
		  z += clen;
		  break;
		}
	    }
	  if (asTdays[iday].zname == NULL)
	    {
	      ulog (LOG_ERROR, "%s: unparseable time string", ztime);
	      return NULL;
	    }
	}
      while (isalpha (BUCHAR (*z)));

      /* Get the hours.  */

      if (! isdigit (BUCHAR (*z)))
	{
	  istart = 0;
	  iend = 24 * 60;
	}
      else
	{
	  char *zendnum;

	  istart = (int) strtol (z, &zendnum, 10);
	  if (*zendnum != '-' || ! isdigit (BUCHAR (zendnum[1])))
	    {
	      ulog (LOG_ERROR, "%s: unparseable time string", ztime);
	      return NULL;
	    }
	  z = zendnum + 1;
	  iend = (int) strtol (z, &zendnum, 10);
	  if (*zendnum != '\0'
	      && *zendnum != ' '
	      && *zendnum != ';'
	      && *zendnum != ','
	      && *zendnum != '|')
	    {
	      ulog (LOG_ERROR, "%s: unparseable time string", ztime);
	      return NULL;
	    }

	  istart = (istart / 100) * 60 + istart % 100;
	  iend = (iend / 100) * 60 + iend % 100;
	}

      /* Add the times we've found onto the list.  */

      for (iday = 0; iday < 7; iday++)
	{
	  if (afday[iday])
	    {
	      int iminute;

	      iminute = iday * 24 * 60;
	      if (istart < iend)
		qlist = qtadd_span (qlist, ival, iminute + istart,
				    iminute + iend, picmp, cretry);
	      else
		{
		  /* Wrap around midnight.  */
		  qlist = qtadd_span (qlist, ival, iminute,
				      iminute + iend, picmp, cretry);
		  qlist = qtadd_span (qlist, ival, iminute + istart,
				      iminute + 24 * 60, picmp,
				      cretry);
		}
	    }
	}
    }

  return qlist;
}

/* See if the current time matches a time span.  If it does, return
   TRUE, set *pival to the value for the matching span, and set
   *pcretry to the retry for the matching span.  Otherwise return
   FALSE.  */

boolean
ftimespan_match (qspan, pival, pcretry)
     const struct sspan *qspan;
     long *pival;
     int *pcretry;
{
  time_t inow;
  struct tm *qtm;
  int itm;
  const struct sspan *q;

  time (&inow);
  qtm = localtime (&inow);

  /* Get the number of minutes since Sunday for the time.  */
  itm = qtm->tm_wday * 24 * 60 + qtm->tm_hour * 60 + qtm->tm_min;

  for (q = qspan; q != NULL; q = q->qnext)
    {
      if (q->istart <= itm && itm <= q->iend)
	{
	  if (pival != NULL)
	    *pival = q->ival;
	  if (pcretry != NULL)
	    *pcretry = q->cretry;
	  return TRUE;
	}
    }

  return FALSE;
}

/* Compare two work grades.  */

static int itgradecmp P((long, long));

static int
itgradecmp (i1, i2)
     long i1;
     long i2;
{
  return igradecmp ((int) i1, (int) i2);
}

/* Parse a time grade string into a time span.  A time grade string is
   a series of single character work grades followed by time strings.
   The time string may end with a semicolon and a retry time.  Each
   grade/time/retry tuple is separated by a single space.  This
   function returns a time span, or NULL if no time matches or an
   error occurs. */

struct sspan *
qtimegrade_parse (ztimegrade)
     const char *ztimegrade;
{
  struct sspan *qret;

  if (ztimegrade == NULL)
    return NULL;

  qret = NULL;

  while (TRUE)
    {
      const char *zretry;
      int cretry;
      struct sspan *qnext;

      zretry = ztimegrade + strcspn (ztimegrade, "; ");
      if (*zretry == ';')
	cretry = atoi (zretry + 1);
      else
	cretry = 0;

      qnext = qttime_parse (ztimegrade + 1, qret, (long) *ztimegrade,
			    itgradecmp, cretry);
      if (qnext != NULL)
	qret = qnext;

      ztimegrade += strcspn (ztimegrade, " ");

      if (*ztimegrade == '\0')
	break;

      ++ztimegrade;
    }

  return qret;
}

/* Compare sizes when putting them into a timestring.  */

static int itsizecmp P((long, long));

static int
itsizecmp (i1, i2)
     long i1;
     long i2;
{
  /* We can't just return i1 - i2 because that would be a long.  */
  if (i1 < i2)
    return -1;
  else if (i1 == i2)
    return 0;
  else
    return 1;
}

/* Parse a time size string into a span.  A time size string is a
   size, a space, a time string, a space, repeated.  There is no retry
   time associated with a time size string.  */

struct sspan *
qtimesize_parse (ztimesize)
     const char *ztimesize;
{
  struct sspan *qret;

  if (ztimesize == NULL)
    return NULL;

  qret = NULL;

  while (TRUE)
    {
      long isize;
      char *zend;
      struct sspan *qnext;

      isize = strtol (ztimesize, &zend, 10);

#if DEBUG > 0
      if (*zend != ' ')
	ulog (LOG_FATAL, "qtimesize_parse: Can't happen");
#endif

      ++zend;

      qnext = qttime_parse (zend, qret, isize, itsizecmp, 0);
      if (qnext != NULL)
	qret = qnext;

      ztimesize = zend + strcspn (zend, " ");

      if (*ztimesize == '\0')
	break;

      ++ztimesize;
    }

  return qret;
}

/* Determine the grade of work we are permitted to do at the current
   time, given a time/grade string.  Return a null byte if no grades
   are legal.  */

char
btimegrade (ztimegrade)
     const char *ztimegrade;
{
  struct sspan *qspan;
  boolean fmatch;
  long ival;

  qspan = qtimegrade_parse (ztimegrade);
  if (qspan == NULL)
    return '\0';

  fmatch = ftimespan_match (qspan, &ival, (int *) NULL);

  utimespan_free (qspan);

  if (! fmatch)
    return '\0';

  return (int) ival;
}

/* Determine the maximum size that may be transferred at the present
   time, according to a time size string.  This returns -1 if there
   are no restrictions.  */

long
cmax_size_now (ztimesize)
     const char *ztimesize;
{
  struct sspan *qspan;
  boolean fmatch;
  long ival;

  qspan = qtimesize_parse (ztimesize);
  if (qspan == NULL)
    return -1;

  fmatch = ftimespan_match (qspan, &ival, (int *) NULL);

  utimespan_free (qspan);

  if (! fmatch)
    return -1;

  return ival;
}

/* Determine the maximum size that may ever be transferred, according
   to a time size string.  This returns -1 if there is no limit.  */

long
cmax_size_ever (ztimesize)
     const char *ztimesize;
{
  struct sspan *qspan;
  long imax;
  struct sspan *q;

  qspan = qtimesize_parse (ztimesize);
  if (qspan == NULL)
    return -1;

  /* Look through the list of spans.  If there is any gap larger than
     1 hour, we assume there are no restrictions.  Otherwise we keep
     track of the largest value we see.  I picked 1 hour arbitrarily,
     on the theory that a 1 hour span to transfer large files might
     actually occur, and is probably not an accident.  */

  if (qspan->istart >= 60)
    {
      utimespan_free (qspan);
      return -1;
    }

  imax = -1;

  for (q = qspan; q != NULL; q = q->qnext)
    {
      if (q->qnext == NULL)
	{
	  if (q->iend <= 6 * 24 * 60 + 23 * 60)
	    {
	      utimespan_free (qspan);
	      return -1;
	    }
	}
      else
	{
	  if (q->iend + 60 <= q->qnext->istart)
	    {
	      utimespan_free (qspan);
	      return -1;
	    }
	}

      if (imax < q->ival)
	imax = q->ival;
    }

  utimespan_free (qspan);

  return imax;
}
