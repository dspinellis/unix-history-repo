/* alloca.c
   A very simplistic alloca routine.

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

   $Log: alloca.c,v $
   Revision 1.1  1992/02/23  03:26:51  ian
   Initial revision

   */

#include "uucp.h"

#if USE_RCS_ID
char alloca_rcsid[] = "$Id: alloca.c,v 1.1 1992/02/23 03:26:51 ian Rel $";
#endif

/* A simplistic implementation of alloca.  I could just include Doug
   Gwyn's alloca, but this is simpler for what I need and it's
   guaranteed to be portable.  It will only work for a simple program
   like this one.  Obviously a real alloca will be preferable.  */

static void uaclear_alloca P((void));

struct salloca
{
  struct salloca *qnext;
  pointer pbuf;
};

static struct salloca *qAlloca;

/* Allocate temporary storage.  */

pointer
alloca (isize)
     int isize;
{
  struct salloca *q;

  if (isize == 0)
    {
      uaclear_alloca ();
      return NULL;
    }
  q = (struct salloca *) malloc (sizeof (struct salloca));
  if (q == NULL)
    abort ();
  q->qnext = qAlloca;
  qAlloca = q;
  q->pbuf = malloc (isize);
  if (q->pbuf == NULL)
    abort ();
  return q->pbuf;
}

/* Free up all temporary storage.  */

static void
uaclear_alloca ()
{
  struct salloca *q;

  q = qAlloca;
  while (q != NULL)
    {
      struct salloca *qnext;

      free (q->pbuf);
      qnext = q->qnext;
      free ((pointer) q);
      q = qnext;
    }
  qAlloca = NULL;
}
