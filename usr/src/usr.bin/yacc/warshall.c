/*
 * Copyright (c) 1989 The Regents of the University of California.
 * All rights reserved.
 *
 * This code is derived from software contributed to Berkeley by
 * Robert Paul Corbett.
 *
 * Redistribution and use in source and binary forms are permitted
 * provided that the above copyright notice and this paragraph are
 * duplicated in all such forms and that any documentation,
 * advertising materials, and other materials related to such
 * distribution and use acknowledge that the software was developed
 * by the University of California, Berkeley.  The name of the
 * University may not be used to endorse or promote products derived
 * from this software without specific prior written permission.
 * THIS SOFTWARE IS PROVIDED ``AS IS'' AND WITHOUT ANY EXPRESS OR
 * IMPLIED WARRANTIES, INCLUDING, WITHOUT LIMITATION, THE IMPLIED
 * WARRANTIES OF MERCHANTABILITY AND FITNESS FOR A PARTICULAR PURPOSE.
 */

#ifndef lint
static char sccsid[] = "@(#)warshall.c	5.1 (Berkeley) %G%";
#endif /* not lint */

#include "defs.h"

transitive_closure(R, n)
unsigned *R;
int n;
{
  register int rowsize;
  register unsigned mask;
  register unsigned *rowj;
  register unsigned *rp;
  register unsigned *rend;
  register unsigned *ccol;

  unsigned *relend;
  unsigned *cword;
  unsigned *rowi;

  rowsize = ROWSIZE(n);
  relend = (unsigned *) ((unsigned) R + n*rowsize);

  cword = R;
  mask = 1;
  rowi = R;
  while (rowi < relend)
    {
      ccol = cword;
      rowj = R;

      while (rowj < relend)
	{
	  if (*ccol & mask)
	    {
	      rp = rowi;
	      rend = (unsigned *) ((unsigned) rowj + rowsize);

	      while (rowj < rend)
		*rowj++ |= *rp++;
	    }
	  else
	    {
	      rowj = (unsigned *) ((unsigned) rowj + rowsize);
	    }

	  ccol = (unsigned *) ((unsigned) ccol + rowsize);
	}

      mask <<= 1;
      if (mask == 0)
	{
	  mask = 1;
	  cword++;
	}

      rowi = (unsigned *) ((unsigned) rowi + rowsize);
    }
}

reflexive_transitive_closure(R, n)
unsigned *R;
int n;
{
  register int rowsize;
  register unsigned mask;
  register unsigned *rp;
  register unsigned *relend;

  transitive_closure(R, n);

  rowsize = ROWSIZE(n);
  relend = (unsigned *) ((unsigned) R + n*rowsize);

  mask = 1;
  rp = R;
  while (rp < relend)
    {
      *rp |= mask;

      mask <<= 1;
      if (mask == 0)
	{
	  mask = 1;
	  rp++;
	}

      rp = (unsigned *) ((unsigned) rp + rowsize);
    }
}
