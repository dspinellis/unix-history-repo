/* Cursor motion subroutines for GNU Emacs.
   Copyright (C) 1985 Free Software Foundation, Inc.
    based primarily on public domain code written by Chris Torek

This file is part of GNU Emacs.

GNU Emacs is free software; you can redistribute it and/or modify
it under the terms of the GNU General Public License as published by
the Free Software Foundation; either version 1, or (at your option)
any later version.

GNU Emacs is distributed in the hope that it will be useful,
but WITHOUT ANY WARRANTY; without even the implied warranty of
MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
GNU General Public License for more details.

You should have received a copy of the GNU General Public License
along with GNU Emacs; see the file COPYING.  If not, write to
the Free Software Foundation, 675 Mass Ave, Cambridge, MA 02139, USA.  */


#include "config.h"
#include <stdio.h>
#include "cm.h"
#include "termhooks.h"

#define	BIG	9999		/* 9999 good on VAXen.  For 16 bit machines
				   use about 2000.... */

char *tgoto ();

extern char *BC, *UP;

int cost;		/* sums up costs */

/* ARGSUSED */
evalcost (c)
     char c;
{
  cost++;
}

void
cmputc (c)
     char c;
{
  if (termscript)
    fputc (c & 0177, termscript);
  putchar (c & 0177);
}

/* NEXT TWO ARE DONE WITH MACROS */
#if 0
/*
 * Assume the cursor is at row row, column col.  Normally used only after
 * clearing the screen, when the cursor is at (0, 0), but what the heck,
 * let's let the guy put it anywhere.
 */

static
at (row, col) {
    curY = row;
    curX = col;
}

/*
 * Add n columns to the current cursor position.
 */

static
addcol (n) {
    curX += n;

    /*
     * If cursor hit edge of screen, what happened?
     * N.B.: DO NOT!! write past edge of screen.  If you do, you
     * deserve what you get.  Furthermore, on terminals with
     * autowrap (but not magicwrap), don't write in the last column
     * of the last line.
     */

    if (curX == Wcm.cm_cols) {
	/*
	 * Well, if magicwrap, still there, past the edge of the
	 * screen (!).  If autowrap, on the col 0 of the next line.
	 * Otherwise on last column.
	 */

	if (Wcm.cm_magicwrap)
	    ;			/* "limbo" */
	else if (Wcm.cm_autowrap) {
	    curX = 0;
	    curY++;		/* Beware end of screen! */
	}
	else
	    curX--;
    }
}
#endif

/*
 * (Re)Initialize the cost factors, given the output speed of the terminal
 * in the variable ospeed.  (Note: this holds B300, B9600, etc -- ie stuff
 * out of <sgtty.h>.)
 */

cmcostinit ()
{
    char *p;

#define	COST(x,e)	(x ? (cost = 0, tputs (x, 1, e), cost) : BIG)
#define CMCOST(x,e)	((x == 0) ? BIG : (p = tgoto(x, 0, 0), COST(p ,e)))

    Wcm.cc_up =		COST (Wcm.cm_up, evalcost);
    Wcm.cc_down =	COST (Wcm.cm_down, evalcost);
    Wcm.cc_left =	COST (Wcm.cm_left, evalcost);
    Wcm.cc_right =	COST (Wcm.cm_right, evalcost);
    Wcm.cc_home =	COST (Wcm.cm_home, evalcost);
    Wcm.cc_cr =		COST (Wcm.cm_cr, evalcost);
    Wcm.cc_ll =		COST (Wcm.cm_ll, evalcost);
    Wcm.cc_tab =	Wcm.cm_tabwidth ? COST (Wcm.cm_tab, evalcost) : BIG;

    /*
     * These last three are actually minimum costs.  When (if) they are
     * candidates for the least-cost motion, the real cost is computed.
     * (Note that "0" is the assumed to generate the minimum cost.
     * While this is not necessarily true, I have yet to see a terminal
     * for which is not; all the terminals that have variable-cost
     * cursor motion seem to take straight numeric values.  --ACT)
     */

    Wcm.cc_abs =  CMCOST (Wcm.cm_abs, evalcost);
    Wcm.cc_habs = CMCOST (Wcm.cm_habs, evalcost);
    Wcm.cc_vabs = CMCOST (Wcm.cm_vabs, evalcost);

#undef CMCOST
#undef COST
}

/*
 * Calculate the cost to move from (srcy, srcx) to (dsty, dstx) using
 * up and down, and left and right, motions, and tabs.  If doit is set
 * actually perform the motion.
 */

static
calccost (srcy, srcx, dsty, dstx, doit)
{
    register int    deltay,
                    deltax,
                    c,
                    totalcost;
    int     ntabs,
            n2tabs,
            tabx,
            tab2x,
            tabcost;
    register char  *p;

    /* If have just wrapped on a terminal with xn,
       don't believe the cursor position: give up here
       and force use of absolute positioning.  */

    if (curX == Wcm.cm_cols)
      goto fail;

    totalcost = 0;
    if ((deltay = dsty - srcy) == 0)
	goto x;
    if (deltay < 0)
	p = Wcm.cm_up, c = Wcm.cc_up, deltay = -deltay;
    else
	p = Wcm.cm_down, c = Wcm.cc_down;
    if (c == BIG) {		/* caint get thar from here */
	if (doit)
	    printf ("OOPS");
	return c;
    }
    totalcost = c * deltay;
    if (doit)
	while (--deltay >= 0)
	    tputs (p, 1, cmputc);
x: 
    if ((deltax = dstx - srcx) == 0)
	goto done;
    if (deltax < 0) {
	p = Wcm.cm_left, c = Wcm.cc_left, deltax = -deltax;
	goto dodelta;		/* skip all the tab junk */
    }
    /* Tabs (the toughie) */
    if (Wcm.cc_tab >= BIG || !Wcm.cm_usetabs)
	goto olddelta;		/* forget it! */

    /* 
     * ntabs is # tabs towards but not past dstx; n2tabs is one more
     * (ie past dstx), but this is only valid if that is not past the
     * right edge of the screen.  We can check that at the same time
     * as we figure out where we would be if we use the tabs (which
     * we will put into tabx (for ntabs) and tab2x (for n2tabs)).
     */

    ntabs = (deltax + srcx % Wcm.cm_tabwidth) / Wcm.cm_tabwidth;
    n2tabs = ntabs + 1;
    tabx = (srcx / Wcm.cm_tabwidth + ntabs) * Wcm.cm_tabwidth;
    tab2x = tabx + Wcm.cm_tabwidth;

    if (tab2x >= Wcm.cm_cols)	/* too far (past edge) */
	n2tabs = 0;

    /* 
     * Now set tabcost to the cost for using ntabs, and c to the cost
     * for using n2tabs, then pick the minimum.
     */

		   /* cost for ntabs     +    cost for right motion */
    tabcost = ntabs ? ntabs * Wcm.cc_tab + (dstx - tabx) * Wcm.cc_right
		    : BIG;

		   /* cost for n2tabs    +    cost for left motion */
    c = n2tabs  ?    n2tabs * Wcm.cc_tab + (tab2x - dstx) * Wcm.cc_left
		: BIG;

    if (c < tabcost)		/* then cheaper to overshoot & back up */
	ntabs = n2tabs, tabcost = c, tabx = tab2x;

    if (tabcost >= BIG)		/* caint use tabs */
	goto newdelta;

    /* 
     * See if tabcost is less than just moving right
     */

    if (tabcost < (deltax * Wcm.cc_right)) {
	totalcost += tabcost;	/* use the tabs */
	if (doit)
	    while (--ntabs >= 0)
		tputs (Wcm.cm_tab, 1, cmputc);
	srcx = tabx;
    }

    /* 
     * Now might as well just recompute the delta.
     */

newdelta: 
    if ((deltax = dstx - srcx) == 0)
	goto done;
olddelta: 
    if (deltax > 0)
	p = Wcm.cm_right, c = Wcm.cc_right;
    else
	p = Wcm.cm_left, c = Wcm.cc_left, deltax = -deltax;

dodelta: 
    if (c == BIG) {		/* caint get thar from here */
fail:
	if (doit)
	    printf ("OOPS");
	return BIG;
    }
    totalcost += c * deltax;
    if (doit)
	while (--deltax >= 0)
	    tputs (p, 1, cmputc);
done: 
    return totalcost;
}

losecursor ()
{
  curY = -1;
}

#define	USEREL	0
#define	USEHOME	1
#define	USELL	2
#define	USECR	3

cmgoto (row, col)
{
    int     homecost,
            crcost,
            llcost,
            relcost,
            directcost;
    int     use;
    char   *p,
           *dcm;

  /* First the degenerate case */
  if (row == curY && col == curX) /* already there */
    return;

  if (curY >= 0 && curX >= 0)
    {
      /* 
       * Pick least-cost motions
       */

      relcost = calccost (curY, curX, row, col, 0);
      use = USEREL;
      if ((homecost = Wcm.cc_home) < BIG)
	  homecost += calccost (0, 0, row, col, 0);
      if (homecost < relcost)
	  relcost = homecost, use = USEHOME;
      if ((llcost = Wcm.cc_ll) < BIG)
	  llcost += calccost (Wcm.cm_rows - 1, 0, row, col, 0);
      if (llcost < relcost)
	  relcost = llcost, use = USELL;
      if ((crcost = Wcm.cc_cr) < BIG) {
	  if (Wcm.cm_autolf)
	      if (curY + 1 >= Wcm.cm_rows)
		  crcost = BIG;
	      else
		  crcost += calccost (curY + 1, 0, row, col, 0);
	  else
	      crcost += calccost (curY, 0, row, col, 0);
      }
      if (crcost < relcost)
	  relcost = crcost, use = USECR;
      directcost = Wcm.cc_abs, dcm = Wcm.cm_abs;
      if (row == curY && Wcm.cc_habs < BIG)
	  directcost = Wcm.cc_habs, dcm = Wcm.cm_habs;
      else if (col == curX && Wcm.cc_vabs < BIG)
	  directcost = Wcm.cc_vabs, dcm = Wcm.cm_vabs;
    }
  else
    {
      directcost = 0, relcost = 100000;
      dcm = Wcm.cm_abs;
    }

  /* 
   * In the following comparison, the = in <= is because when the costs
   * are the same, it looks nicer (I think) to move directly there.
   */
  if (directcost <= relcost)
    {
      /* compute REAL direct cost */
      cost = 0;
      p = dcm == Wcm.cm_habs ? tgoto (dcm, row, col) :
			       tgoto (dcm, col, row);
      tputs (p, 1, evalcost);
      if (cost <= relcost)
	{	/* really is cheaper */
	  tputs (p, 1, cmputc);
	  curY = row, curX = col;
	  return;
	}
    }

  switch (use)
    {
    case USEHOME: 
      tputs (Wcm.cm_home, 1, cmputc);
      curY = 0, curX = 0;
      break;

    case USELL: 
      tputs (Wcm.cm_ll, 1, cmputc);
      curY = Wcm.cm_rows - 1, curX = 0;
      break;

    case USECR: 
      tputs (Wcm.cm_cr, 1, cmputc);
      if (Wcm.cm_autolf)
	curY++;
      curX = 0;
      break;
    }

  (void) calccost (curY, curX, row, col, 1);
  curY = row, curX = col;
}

/* Clear out all terminal info.
   Used before copying into it the info on the actual terminal.
 */

Wcm_clear ()
{
  bzero (&Wcm, sizeof Wcm);
  UP = 0;
  BC = 0;
}

/*
 * Initialized stuff
 * Return 0 if can do CM.
 */

Wcm_init ()
{
  /* Check that we know the size of the screen.... */
  if (Wcm.cm_rows <= 0 || Wcm.cm_cols <= 0)
    return - 1;
  if (Wcm.cm_abs && !Wcm.cm_ds)
    return 0;
  /* Require up and left, and, if no absolute, down and right */
  if (!Wcm.cm_up || !Wcm.cm_left)
    return - 1;
  if (!Wcm.cm_abs && (!Wcm.cm_down || !Wcm.cm_right))
    return - 1;
  return 0;
}
