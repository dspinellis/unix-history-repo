/* util.c
   Utilities for the UUCP package.

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

   $Log: util.c,v $
   Revision 1.11  1992/03/12  19:56:10  ian
   Debugging based on types rather than number

   Revision 1.10  1992/03/02  04:53:07  ian
   Marc Unangst: added HAVE_SCO_LOCKFILES configuration parameter

   Revision 1.9  1992/02/23  03:26:51  ian
   Overhaul to use automatic configure shell script

   Revision 1.8  1992/02/08  03:54:18  ian
   Include <string.h> only in <uucp.h>, added 1992 copyright

   Revision 1.7  1992/01/19  18:29:05  ian
   Added HAVE_BSEARCH configuration parameter

   Revision 1.6  1992/01/15  20:40:04  ian
   Mike Park: some systems don't have <limits.h>

   Revision 1.5  1991/12/28  06:10:50  ian
   Added HAVE_STRCHR and HAVE_INDEX to conf.h

   Revision 1.4  1991/12/28  03:49:23  ian
   Added HAVE_MEMFNS and HAVE_BFNS; changed uses of memset to bzero

   Revision 1.3  1991/12/11  19:35:48  ian
   Mark Powell: put in my own version of strtol

   Revision 1.2  1991/11/21  21:20:41  ian
   Brian Campbell: offer str{n}icmp as an alternative to str{n}casecmp

   Revision 1.1  1991/09/10  19:40:31  ian
   Initial revision

   */

#include "uucp.h"

#if USE_RCS_ID
char util_rcsid[] = "$Id: util.c,v 1.11 1992/03/12 19:56:10 ian Rel $";
#endif

#include <stdio.h>
#include <ctype.h>
#include <errno.h>
 
#if HAVE_LIMITS_H
#include <limits.h>
#endif

/* Allocate a block of memory without fail.  */

pointer
xmalloc (c)
     int c;
{
  pointer pret;

  pret = malloc (c);
  if (pret == NULL && c != 0)
    ulog (LOG_FATAL, "Out of memory");
  return pret;
}

/* Realloc a block of memory without fail.  Supposedly some versions of
   realloc can't handle a NULL first argument, so we check for that
   here.  */

pointer
xrealloc (p, c)
     pointer p;
     int c;
{
  pointer pret;

  if (p == NULL)
    return xmalloc (c);
  pret = realloc (p, c);
  if (pret == NULL && c != 0)
    ulog (LOG_FATAL, "Out of memory");
  return pret;
}

/* Some versions of free (like the one in SCO Unix 3.2.2) don't handle
   null pointers correctly, so we go through our own routine.  */

void
xfree (p)
     pointer p;
{
  if (p != NULL)
    free (p);
}

/* Read a string of arbitrary length from a stdio file, returning an
   malloced memory area.  */

#define CFGETSDEFAULT (63)

char *
zfgets (e, fbackslash)
     FILE *e;
     boolean fbackslash;
{
  char *zret, *z, *zend;
  int clen;
  int ichar;

  clen = CFGETSDEFAULT;

  /* Allocate one extra byte for the '\0'.  */
  zret = (char *) xmalloc (clen + 1);
  
  z = zret;
  zend = zret + clen;
  while ((ichar = getc (e)) != EOF)
    {
      if (z >= zend)
	{
	  char *znew;

	  clen += CFGETSDEFAULT;

	  /* Allocate one extra byte for the '\0'.  */
	  znew = (char *) xrealloc ((pointer) zret, clen + 1);
	  z = znew + (z - zret);
	  zret = znew;
	  zend = zret + clen;
	}

      *z++ = (char) ichar;
      if (ichar == '\n')
	{
	  if (! fbackslash || z - zret < 2 || z[-2] != '\\')
	    break;
	  z -= 2;
	}
    }

  if (z == zret)
    {
      xfree ((pointer) zret);
      return NULL;
    }

  *z = '\0';

  return zret;
}

#if ! HAVE_STRDUP

/* Duplicate a string in memory.  */

char *
strdup (z)
     const char *z;
{
  char *zret;

  zret = malloc (strlen (z) + 1);
  if (zret != NULL)
    strcpy (zret, z);
  return zret;
}

#endif /* ! HAVE_STRDUP */

/* Duplicate a string in memory with no errors.  */

char *
xstrdup (z)
     const char *z;
{
  char *zret;

  zret = strdup (z);
  if (zret == NULL)
    ulog (LOG_FATAL, "Out of memory");
  return zret;
}

#if ! HAVE_STRSTR

/* Look for one string inside another.  */

char *
strstr (zhold, zwithin)
     const char *zhold;
     const char *zwithin;
{
  register char b;
  register char bwithin;

  if ((bwithin = *zwithin++) == '\0')
    return (char *) zhold;

  while ((b = *zhold++) != '\0')
    {
      if (bwithin == b)
	{
	  register const char *zout, *zin;

	  zout = zhold;
	  zin = zwithin;
	  do
	    {
	      if (*zin == '\0')
		return (char *) (zhold - 1);
	    }
	  while (*zout++ == *zin++);
	}
    }

  return (char *) NULL;
}

#endif /* ! HAVE_STRSTR */

#if ! HAVE_STRCASECMP && ! HAVE_STRICMP

/* Do a case insensitive string comparison.  */

int
strcasecmp (z1, z2)
     const char *z1;
     const char *z2;
{
  char b1, b2;

  while ((b1 = *z1++) != '\0')
    {
      b2 = *z2++;
      if (b2 == '\0')
	return 1;
      if (b1 != b2)
	{
	  if (isupper (BUCHAR (b1)))
	    b1 = tolower (BUCHAR (b1));
	  if (isupper (BUCHAR (b2)))
	    b2 = tolower (BUCHAR (b2));
	  if (b1 != b2)
	    return b1 - b2;
	}
    }
  if (*z2 == '\0')
    return 0;
  else
    return -1;
}

int
strncasecmp (z1, z2, c)
     const char *z1;
     const char *z2;
     int c;
{
  char b1, b2;

  if (c <= 0)
    return 0;
  while ((b1 = *z1++) != '\0')
    {
      b2 = *z2++;
      if (b2 == '\0')
	return 1;
      if (b1 != b2)
	{
	  if (isupper (BUCHAR (b1)))
	    b1 = tolower (BUCHAR (b1));
	  if (isupper (BUCHAR (b2)))
	    b2 = tolower (BUCHAR (b2));
	  if (b1 != b2)
	    return b1 - b2;
	}
      --c;
      if (c <= 0)
	return 0;
    }
  if (*z2 == '\0')
    return 0;
  else
    return -1;
}

#endif /* ! HAVE_STRCASECMP && ! HAVE_STRICMP */

#if ! HAVE_MEMCHR

/* Find a single byte in a memory block.  */

pointer
memchr (parg, b, c)
     constpointer parg;
     int b;
     int c;
{
  const char *p = (const char *) parg;

  b = BUCHAR (b);
  while (c-- != 0)
    if (BUCHAR (*p++) == b)
      return (pointer) --p;
  return NULL;
}

#endif /* ! HAVE_MEMCHR */

#if ! HAVE_MEMCMP && ! HAVE_BCMP

/* Compare two memory blocks.  */

int
memcmp (p1arg, p2arg, c)
     constpointer p1arg;
     constpointer p2arg;
     int c;
{
  const char *p1 = (const char *) p1arg;
  const char *p2 = (const char *) p2arg;

  while (c-- != 0)
    if (*p1++ != *p2++)
      return BUCHAR (*--p1) - BUCHAR (*--p2);
  return 0;
}
	  
#endif /* ! HAVE_MEMCMP && ! HAVE_BCMP */

#if ! HAVE_MEMCPY && ! HAVE_BCOPY

/* Copy one memory block to another.  */

pointer
memcpy (ptoarg, pfromarg, c)
     pointer ptoarg;
     constpointer pfromarg;
     int c;
{
  char *pto = (char *) ptoarg;
  const char *pfrom = (const char *) pfromarg;

  while (c-- != 0)
    *pto++ = *pfrom++;
  return ptoarg;
}

#endif /* ! HAVE_MEMCPY && ! HAVE_BCOPY */

#if ! HAVE_BZERO && ! HAVE_MEMSET

/* Zero out a block of memory.  */

void
bzero (parg, c)
     pointer parg;
     int c;
{
  char *p = (char *) parg;

  while (c-- != 0)
    *p++ = 0;
}

#endif /* ! HAVE_BZERO && ! HAVE_MEMSET */

#if ! HAVE_MEMMOVE

/* Move a memory block safely despite overlap.  This function is
   almost impossible to write in strictly conforming C, because it
   wants to compare pointers to different objects, but this
   implementation will suffice for all normal systems.  I hope.  */

pointer
xmemmove (pto, pfrom, c)
     pointer pto;
     constpointer pfrom;
     int c;
{
  char *zto = (char *) pto;
  const char *zfrom = (const char *) pfrom;

  if (zto <= zfrom || zto >= zfrom + c)
    {
      while (c-- != 0)
	*zto++ = *zfrom++;
    }
  else
    {
      zto += c;
      zfrom += c;
      while (c-- != 0)
	*--zto = *--zfrom;
    }
    
  return pto;
}

#endif /* ! HAVE_MEMMOVE */

#if ! HAVE_STRCHR && ! HAVE_INDEX

/* I doubt there are any systems for which this is true, but who
   knows?  Provide my own version of strchr.  */

/* Look for a character in a string.  This is supposed to work for a
   null byte, although we never actually call it with one.  */

char *
strchr (z, b)
     const char *z;
     int b;
{
  b = (char) b;
  while (*z != b)
    if (*z++ == '\0')
      return NULL;
  return (char *) z;
}

#endif /* ! HAVE_STRCHR && ! HAVE_INDEX */

#if ! HAVE_STRRCHR && ! HAVE_RINDEX

/* Look for the last occurrence of a character in a string.  This is
   supposed to work for a null byte, although we never actually call
   it with one.  */

char *
strrchr (z, b)
     const char *z;
     int b;
{
  char *zret;

  b = (char) b;
  zret = NULL;
  do
    {
      if (*z == b)
	zret = (char *) z;
    }
  while (*z++ != '\0');
  return zret;
}

#endif /* ! HAVE_STRRCHR && ! HAVE_RINDEX */

#if ! HAVE_STRLWR

/* Convert a string to lower case.  */

char *
strlwr (zarg)
     char *zarg;
{
  char *z = zarg;

  while (*z != '\0')
    {
      if (isupper (*z))
	*z = tolower (*z);
      ++z;
    }

  return zarg;
}

#endif /* ! HAVE_STRLWR */

#if ! HAVE_STRTOL

/* My own version of strtol.  This assumes that the upper case
   characters appear in sequence and that the lower case characters
   appear in sequence.  It also assumes that unsigned arithmetic is
   performed correctly, but that's probably a safe assumption.  This
   code needs only a couple of changes to also work as strtoul.  */

/* We need definitions for LONG_MAX and LONG_MIN; the limits that
   appear here are those guaranteed by the C standard.  The value for
   LONG_MIN is one greater than that applicable to most computers.  */

#ifndef LONG_MAX
#define LONG_MAX (2147483647)
#endif
#ifndef LONG_MIN
#define LONG_MIN (- 2147483647)
#endif

long
strtol (zarg, pzend, ibase)
     const char *zarg;
     char **pzend;
     int ibase;
{
  const char *z, *zsubj;
  boolean fsign;
  unsigned long ival;
  boolean foverflow;

  z = zarg;

  while (isspace (BUCHAR (*z)))
    ++z;

  fsign = FALSE;
  if (*z == '+')
    ++z;
  else if (*z == '-')
    {
      ++z;
      fsign = TRUE;
    }

  if (ibase == 0)
    {
      if (*z == '0')
	{
	  if (z[1] == 'x' || z[1] == 'X')
	    {
	      z += 2;
	      ibase = 16;
	    }
	  else
	    ibase = 8;
	}
      else
	ibase = 10;
    }
  else
    {
      if (ibase == 16
	  && *z == '0'
	  && (z[1] == 'x' || z[1] == 'X'))
	z += 2;
    }

  ival = 0;
  foverflow = FALSE;
  zsubj = z;

  while (TRUE)
    {
      int inext;
      unsigned long itmp;

      if (isdigit (BUCHAR (*z)))
	inext = *z - '0';
      else if (isupper (BUCHAR (*z)))
	inext = *z - 'A' + 10;
      else if (islower (BUCHAR (*z)))
	inext = *z - 'a' + 10;
      else
	break;

      if (inext >= ibase)
	break;

      itmp = ival * ibase + inext;

      /* Operations on unsigned values are performed using modulos
	 arithmetic.  Therefore any overflow will result in a smaller
	 number.  Note that we can't simply return out on overflow,
	 because we still have to determine the end of the subject
	 sequence.  */
      if (itmp < ival)
	foverflow = TRUE;

      ival = itmp;
      ++z;
    }

  if (z == zsubj)
    {
      if (pzend != NULL)
	*pzend = (char *) zarg;
      return 0;
    }

  if (pzend != NULL)
    *pzend = (char *) z;

  /* Now checked for overflow as a signed type.  If this were strtoul,
     we would just leave out this check.  Converting LONG_MIN, a
     negative number, to unsigned long means adding it to ULONG_MAX +
     1, which can not overflow since long and unsigned long are the
     same size.  Negating an unsigned long, say i, means performing
     the operation (ULONG_MAX + 1) - i, which clearly can not
     overflow.  The result is thus (ULONG_MAX + 1) - ((ULONG_MAX + 1)
     + LONG_MIN) == - LONG_MIN, which is the magnitude we are looking
     for.  */
  if (fsign
      ? ival > (unsigned long) LONG_MAX
      : ival > - (unsigned long) LONG_MIN)
    foverflow = TRUE;

  /* If this were strtoul, we would return ULONG_MAX on overflow
     regardless of the value of fsign.  */
  if (foverflow)
    {
      errno = ERANGE;
      if (fsign)
	return LONG_MIN;
      else
	return LONG_MAX;
    }

  if (fsign)
    ival = - ival;

  /* If this were strtoul, we would not case the value before
     returning.  */
  return (long) ival;
}

#endif /* ! HAVE_STRTOL */

#if ! HAVE_BSEARCH

/* Search for a key in a sorted array.  The third and fourth arguments
   should be size_t, but int will suffice for my uses and spare me
   from defining size_t portably.  */

pointer
bsearch (pkey, parray, celes, cbytes, pficmp)
     constpointer pkey;
     constpointer parray;
     int celes;
     int cbytes;
     int (*pficmp) P((constpointer, constpointer));
{
  const char *zarray = (const char *) parray;
  int ilow, ihigh, itrial;

  ilow = 0;
  ihigh = celes;
  while (ilow < ihigh)
    {
      const char *zcheck;
      int icmp;

      itrial = (ilow + ihigh) >> 1;
      /* Here ilow <= itrial < ihigh */
      zcheck = zarray + itrial * cbytes;
      icmp = (*pficmp) (pkey, (constpointer) zcheck);
      if (icmp < 0)
	ihigh = itrial;
      else if (icmp > 0)
	ilow = itrial + 1;
      else
	return (pointer) zcheck;
    }

  return NULL;
}

#endif /* ! HAVE_BSEARCH */
