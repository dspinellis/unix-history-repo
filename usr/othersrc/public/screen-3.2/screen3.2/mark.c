/* Copyright (c) 1991
 *      Juergen Weigert (jnweiger@immd4.informatik.uni-erlangen.de)
 *      Michael Schroeder (mlschroe@immd4.informatik.uni-erlangen.de)
 * Copyright (c) 1987 Oliver Laumann
 *
 * This program is free software; you can redistribute it and/or modify
 * it under the terms of the GNU General Public License as published by
 * the Free Software Foundation; either version 1, or (at your option)
 * any later version.
 *
 * This program is distributed in the hope that it will be useful,
 * but WITHOUT ANY WARRANTY; without even the implied warranty of
 * MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
 * GNU General Public License for more details.
 *
 * You should have received a copy of the GNU General Public License
 * along with this program (see the file COPYING); if not, write to the
 * Free Software Foundation, Inc., 675 Mass Ave, Cambridge, MA 02139, USA.
 *
 * Noteworthy contributors to screen's design and implementation:
 *	Wayne Davison (davison@borland.com)
 *	Patrick Wolfe (pat@kai.com, kailand!pat)
 *	Bart Schaefer (schaefer@cse.ogi.edu)
 *	Nathan Glasser (nathan@brokaw.lcs.mit.edu)
 *	Larry W. Virden (lwv27%cas.BITNET@CUNYVM.CUNY.Edu)
 *	Howard Chu (hyc@hanauma.jpl.nasa.gov)
 *	Tim MacKenzie (tym@dibbler.cs.monash.edu.au)
 *	Markku Jarvinen (mta@{cc,cs,ee}.tut.fi)
 *	Marc Boucher (marc@CAM.ORG)
 *
 ****************************************************************
 */

#ifndef lint
  static char rcs_id[] = "$Id: mark.c,v 1.2 1992/05/18 03:49:28 rich Exp $ FAU";
#endif

#include <sys/types.h>

#ifdef BSDI
# include <sys/signal.h>
#endif /* BSDI */

#include "config.h"
#include "screen.h"
#include "ansi.h"	/* here we find A_SO, ASCII, EXPENSIVE */
#include "extern.h"

static int is_letter __P((int));
static void nextword __P((int *, int *, int, int));
static int linestart __P((int));
static int lineend __P((int));
static int rem __P((int, int , int , int , int , char *, int));
static int eq __P((int, int ));
static void revto __P((int, int));
static void revto_line __P((int, int, int));
static void MarkRedisplayLine __P((int, int, int, int));
static int MarkRewrite __P((int, int, int, int));
static void process_mark_input __P((char **, int *));
static void AbortMarkRoutine __P((void));
static int MarkScrollDownDisplay __P((int));
static int MarkScrollUpDisplay __P((int));

int join_with_cr =  0;
extern struct win *fore, *wtab[];
extern int screenwidth, screenheight;
extern int screentop, screenbot;
extern char GlobalAttr, GlobalCharset;
extern int in_ovl;
extern int HS;
extern int LP;
extern char *null, *blank;

#ifdef NETHACK
extern nethackflag;
#endif

char *copybuffer = NULL;
int copylen = 0;
char mark_key_tab[256]; /* this array must be initialised first! */

static int in_mark;	/* mark routine active */
static int left_mar, right_mar, nonl;
static int x1, y1, second; /* y1 is in terms of WIN coordinates, not DISPLAY */
static int cx, cy;	/* Cursor Position in WIN coords*/
static rep_cnt;		/* no. of repeats are rep_cnt+1. jw. */
static int append_mode;	/* shall we overwrite or append to copybuffer */
static write_buffer;	/* shall we do a KEY_WRITE_EXCHANGE right away? */
static hist_offset;

static int is_letter(c)
char c;
{
  if ((c >= 'a' && c <= 'z') ||
      (c >= 'A' && c <= 'Z') ||
      (c >= '0' && c <= '9') ||
      c == '_' || c == '.' ||
      c == '@' || c == ':' ||
      c == '%' || c == '!' ||
      c == '-' || c == '+')
    /* thus we can catch email-addresses as a word :-) */
    return 1;
  else if (c != ' ')
    return 2;
  return 0;
}

/*
 * iWIN gives us a reference to line y of the *whole* image 
 * where line 0 is the oldest line in our history.
 * y must be in WIN coordinate system, not in display.
 */
#define iWIN(y) ((y < fore->histheight) ? fore->ihist[(fore->histidx + y)\
		% fore->histheight] : fore->image[y - fore->histheight])
#define aWIN(y) ((y < fore->histheight) ? fore->ahist[(fore->histidx + y)\
		% fore->histheight] : fore->attr[y - fore->histheight])
#define fWIN(y) ((y < fore->histheight) ? fore->fhist[(fore->histidx + y)\
		% fore->histheight] : fore->font[y - fore->histheight])
/*
 * hist_offset tells us, how many lines there are on top of the
 * visible screen.
 */

#define W2D(y) ((y)-hist_offset)
#define D2W(y) ((y)+hist_offset)

static int
linestart(y)
int y;
{
  register int x;
  register char *i;

  for (x = left_mar, i = iWIN(y) + x; x < screenwidth-1; x++)
    if (*i++ != ' ')
      break;
  if (x == screenwidth-1)
    x = left_mar;
  return(x);
}

static int
lineend(y)
int y;
{
  register int x;
  register char *i;

  for (x = right_mar, i = iWIN(y) + x; x >= 0; x--)
    if (*i-- != ' ')
      break;
  if (x < 0)
    x = left_mar;
  return(x);
}


/*
 *  nextword calculates the cursor position of the num'th word.
 *  If the cursor is on a word, it counts as the first.
 *  NW_BACK:		search backward
 *  NW_ENDOFWORD:	find the end of the word
 *  NW_MUSTMOVE:	move even if the position is correct.
 */

#define NW_BACK		1
#define NW_ENDOFWORD	2
#define NW_MUSTMOVE	4

static void
nextword(xp, yp, flags, num)
int *xp, *yp, flags, num;
{
  int xx = screenwidth, yy = fore->histheight + screenheight;
  register int sx, oq, q, x, y;

  x = *xp;
  y = *yp;
  sx = (flags & NW_BACK) ? -1 : 1;
  if ((flags & NW_ENDOFWORD) && (flags & NW_MUSTMOVE))
    x += sx;
  for (oq = -1; ; x += sx, oq = q)
    {
      if (x >= xx || x < 0)
	q = 0;
      else
        q = is_letter(iWIN(y)[x]);
      if (oq >= 0 && oq != q)
	{
	  if (oq == 0 || !(flags & NW_ENDOFWORD))
	    *xp = x;
	  else
	    *xp = x-sx;
	  *yp = y;
	  if ((!(flags & NW_ENDOFWORD) && q) ||
	      ((flags & NW_ENDOFWORD) && oq))
	    {
	      if (--num <= 0)
	        return;
	    }
	}
      if (x == xx)
	{
	  x = -1;
	  if (++y >= yy)
	    return;
	}
      else if (x < 0)
	{
	  x = xx;
	  if (--y < 0)
	    return;
	}
    }
}


/*
 * y1, y2 are WIN coordinates
 *
 * redisplay:	0  -  just copy
 * 		1  -  redisplay + copy
 *		2  -  count + copy, don't redisplay
 */

static int rem(x1, y1, x2, y2, redisplay, pt, yend)
int x1, y1, x2, y2, redisplay, yend;
char *pt;
{
  int i, j, from, to, ry;
  int l = 0;
  char *im;

  second = 0;
  if (y2 < y1 || ((y2 == y1) && (x2 < x1)))
    {
      i = y2;
      y2 = y1;
      y1 = i;
      i = x2;
      x2 = x1;
      x1 = i;
    }
  ry = y1 - hist_offset;
  
  i = y1;
  if (redisplay != 2 && pt == 0 && ry <0)
    {
      i -= ry;
      ry = 0;
    }
  for (; i <= y2; i++, ry++)
    {
      if (redisplay != 2 && pt == 0 && ry > yend)
	break;
      from = (i == y1) ? x1 : 0;
      if (from < left_mar)
	from = left_mar;
      for (to = screenwidth-1, im = iWIN(i)+to; to>=0; to--)
        if (*im-- != ' ')
	  break;
      if (i == y2 && x2 < to)
	to = x2;
      if (to > right_mar)
	to = right_mar;
      if (redisplay == 1 && from <= to && ry >=0 && ry <= yend)
	MarkRedisplayLine(ry, from, to, 0);
      if (redisplay != 2 && pt == 0)	/* don't count/copy */
	continue;
      for (j = from, im = iWIN(i)+from; j <= to; j++)
	{
	  if (pt)
	    *pt++ = *im++;
	  l++;
	}
      if (i != y2)
	{
	  /* 
	   * this code defines, what glues lines together
	   */
	  switch (nonl)
	    {
	    case 0:		/* lines separated by newlines */
	      if (join_with_cr)
		{
		  if (pt)
		    *pt++ = '\r';
		  l++;
		}
	      if (pt)
		*pt++ = '\n';
	      l++;
	      break;
	    case 1:		/* nothing to separate lines */
	      break;
	    case 2:		/* lines separated by blanks */
	      if (pt)
		*pt++ = ' ';
	      l++;
	      break;
	    }
	}
    }
  return(l);
}

static int eq(a, b)
int a, b;
{
  if (a == b)
    return 1;
  if (a == 0 || b == 0)
    return 1;
  if (a <= '9' && a >= '0' && b <= '9' && b >= '0')
    return 1;
  return 0;
}

static int crazychar = 0;
static int crazy_y = -1;
static int crazy_x = -1;

int MarkRoutine(flag)	/* return value 1 when copybuffer changed; */
int flag;
{
  int x, y, i;
 
  hist_offset = fore->histheight;
 
  if (!fore->active)
    {
      Msg(0, "Fore window is not active !!!");
      return 0;
    }

  second = 0;
  rep_cnt = 0;
  append_mode = 0;
  write_buffer = 0;
  nonl = left_mar = 0;
  right_mar = screenwidth-1;
  x = fore->x;
  y = D2W(fore->y);
  if (x >= screenwidth)
    x = screenwidth-1;

  if (flag == CRAZY && crazychar != 0 && crazy_x != -1 && crazy_y != -1)
    {
      Msg(0, "CRAZY mode not impl.\n");
    }
  crazychar = 0;
  crazy_y = -1;
  crazy_x = -1;
  if (flag == TRICKY)
    {
      int f, q = 0, xx, yy;
      char *linep;

      debug2("cursor is at x=%d, y=%d\n", fore->x, D2W(fore->y));
      for (xx = fore->x - 1, linep = iWIN(y) + xx; xx >= 0; xx--)
	if ((q = *linep--) != ' ' )
	  break;
      debug3("%c at (%d,%d)\n", q, xx, y);
      for (yy = D2W(fore->y) - 1; yy >= 0; yy--)
	if (xx < 0 || eq(iWIN(yy)[xx], q))
	  {		/* line is matching... */
	    f = 0;
	    for (i = fore->x; i < screenwidth-1; i++)
	      {
		if (iWIN(yy)[i] != ' ')
		  {
		    f = 1;
		    break;
		  }
	      }
	    if (f)
	      break;
	  }
      if (yy < 0)
	return 0;
      xx = 0;
      for (i = screenwidth-1, linep = iWIN(yy)+i; i>0; i--)
	if (*linep-- != ' ')
	  break;
      if (i < x)
	i = x;
      if (copybuffer != NULL)
	Free(copybuffer);
      if ((copybuffer = malloc((unsigned) (i - x + 2))) == NULL)
	{
	  Msg(0, "Not enough memoooh!... Sorry.");
	  return 0;
	}
      rem(x, yy, i, yy, 0, copybuffer, 0);
      copylen = i - x + 1;
      return 1;
    }
  InitOverlayPage(process_mark_input, MarkRedisplayLine, MarkRewrite, 1);
  GotoPos(x, W2D(y));
#ifdef NETHACK
  if (nethackflag)
    Msg(0, "Welcome to hacker's treasure zoo - Column %d Line %d(+%d) (%d,%d)",
	x+1, W2D(y+1), fore->histheight, fore->width, fore->height);
  else
#endif
  Msg(0, "Copy mode - Column %d Line %d(+%d) (%d,%d)",
      x+1, W2D(y+1), fore->histheight, fore->width, fore->height);
  fflush(stdout);
  cx = x1 = x;
  cy = y1 = y;
  in_mark = 1;
  return 0;
}

static void process_mark_input(inbufp,inlenp)
char **inbufp;
int *inlenp;
{
  char *inbuf, *pt;
  int inlen;
  int x2, y2, i, j, yend;
  int newcopylen = 0, od;
/*
  char *extrap = 0, extrabuf[100];
*/
      
  if (inbufp == 0)
    {
      AbortMarkRoutine();
      return;
    }
 
  inbuf= *inbufp;
  inlen= *inlenp;
  pt = inbuf;
  while (in_mark && (inlen /* || extrap */))
    {
      if (!HS)
	RemoveStatus();
/*
      if (extrap)
	{
	  od = *extrap++;
	  if (*extrap == 0)
	    extrap = 0;
	}
      else
*/
	{
          od = mark_key_tab[*pt++];
          inlen--;
	}
      if (od >= '0' && od <= '9')
        {
	  if (rep_cnt < 1001 && (od != '0' || rep_cnt != 0))
	    {
	      rep_cnt = 10 * rep_cnt + od - '0';
	      continue;
 	      /*
	       * Now what is that 1001 here? Well, we have a screen with
	       * 25 * 80 = 2000 characters. Movement is at most across the full
	       * screen. This we do with word by word movement, as character by
	       * character movement never steps over line boundaries. The most words
	       * we can place on the screen are 1000 single letter words. Thus 1001
	       * is sufficient. Users with bigger screens never write in single letter
	       * words, as they should be more advanced. jw.
	       * Oh, wrong. We still give even the experienced user a factor of ten.
	       */
	    }
	}
      switch (od)
	{
	case '\014':	/* CTRL-L Redisplay */
	  Redisplay(0);
	  GotoPos(cx, W2D(cy));
	  break;
	case '\010':	/* CTRL-H Backspace */
	case 'h':
	  if (rep_cnt == 0)
	    rep_cnt = 1;
	  revto(cx - rep_cnt, cy);
	  break;
	case '\016':	/* CTRL-N */
	case 'j':
	  if (rep_cnt == 0)
	    rep_cnt = 1;
	  revto(cx, cy + rep_cnt);
	  break;
	case '+':
	  if (rep_cnt == 0)
	    rep_cnt = 1;
	  j = cy + rep_cnt;
	  if (j > fore->histheight + screenheight - 1)
	    j = fore->histheight + screenheight - 1;
	  revto(linestart(j), j);
	  break;
	case '-':
	  if (rep_cnt == 0)
	    rep_cnt = 1;
	  j = cy - rep_cnt;
	  if (j < 0)
	    j = 0;
	  revto(linestart(j), j);
	  break;
	case '^':
	  revto(linestart(cy), cy);
	  break;
	case '\n':
	  revto(left_mar, cy + 1);
	  break;
	case 'k':
	case '\020':	/* CTRL-P */
	  if (rep_cnt == 0)
	    rep_cnt = 1;
	  revto(cx, cy - rep_cnt);
	  break;
	case 'l':
	  if (rep_cnt == 0)
	    rep_cnt = 1;
	  revto(cx + rep_cnt, cy);
	  break;
	case '\001':	/* CTRL-A from tcsh/emacs */
	case '0':
	  revto(left_mar, cy);
	  break;
	case '\004':    /* CTRL-D down half screen */
	  if (rep_cnt == 0)
	    rep_cnt = (screenheight+1) >> 1;
	  revto_line(cx, cy + rep_cnt, W2D(cy));
	  break;
	case '$':
	  revto(lineend(cy), cy);
	  break;
	case '\025':	/* CTRL-U up half screen */
	  if (rep_cnt == 0)
	    rep_cnt = (screenheight+1) >> 1;
	  revto_line(cx, cy - rep_cnt, W2D(cy));
	  break;
	case '?':
	  if (left_mar == 0 && right_mar == screenwidth - 1)
	    Msg(0, "Column %d Line %d(+%d)", cx+1, W2D(cy)+1,
		hist_offset);
	  else
	    Msg(0, "Column %d(%d..%d) Line %d(+%d)", cx+1,
		left_mar+1, right_mar+1, W2D(cy)+1, hist_offset);
	  break;
	case '\002':	/* CTRL-B  back one page */
	  if (rep_cnt == 0)
	    rep_cnt = 1;
	  rep_cnt *= (screenheight-1);
	  revto(cx, cy - rep_cnt);
	  break;
	case '\006':	/* CTRL-F  forward one page */
	  if (rep_cnt == 0)
	    rep_cnt = 1;
	  rep_cnt *= (screenheight-1);
	  revto(cx, cy + rep_cnt);
	  break;
	case '\005':	/* CTRL-E  scroll up */
	  if (rep_cnt == 0)
	    rep_cnt = 1;
	  rep_cnt = MarkScrollUpDisplay(rep_cnt);
	  if (cy < D2W(0))
            revto(cx, D2W(0));
	  else
            GotoPos(cx, W2D(cy));
	  break;
	case '\031': /* CTRL-Y  scroll down */
	  if (rep_cnt == 0)
	    rep_cnt = 1;
	  rep_cnt = MarkScrollDownDisplay(rep_cnt);
	  if (cy > D2W(screenheight-1))
            revto(cx, D2W(screenheight-1));
	  else
            GotoPos(cx, W2D(cy));
	  break;
	case '@':
	  /* it may be usefull to have a key that does nothing */
	  break;
	case '%':
	  rep_cnt--;
	  /* rep_cnt is a percentage for the history buffer */
	  if (rep_cnt < 0)
	    rep_cnt = 0;
	  if (rep_cnt > 100)
	    rep_cnt = 100;
	  revto_line(left_mar, (rep_cnt * (fore->histheight + screenheight)) / 100, (screenheight-1)/2);
	  break;
	case 'g':
	  rep_cnt = 1;
	  /* FALLTHROUGH */
	case 'G':
	  /* rep_cnt is here the WIN line number */
	  if (rep_cnt == 0)
	    rep_cnt = fore->histheight + screenheight;
	  revto_line(left_mar, --rep_cnt, (screenheight-1)/2);
	  break;
	case 'H':
	  revto(left_mar, D2W(0));
	  break;
	case 'M':
	  revto(left_mar, D2W((screenheight-1) / 2));
	  break;
	case 'L':
	  revto(left_mar, D2W(screenheight-1));
	  break;
	case '|':
	  revto(--rep_cnt, cy);
	  break;
	case 'w':
	  i = cx;
	  j = cy;
	  if (rep_cnt == 0)
	    rep_cnt = 1;
	  nextword(&i, &j, NW_MUSTMOVE, rep_cnt);
	  revto(i, j);
	  break;
	case 'e':
	  i = cx;
	  j = cy;
	  if (rep_cnt == 0)
	    rep_cnt = 1;
	  nextword(&i, &j, NW_ENDOFWORD|NW_MUSTMOVE, rep_cnt);
	  revto(i, j);
	  break;
	case 'b':
	  i = cx;
	  j = cy;
	  if (rep_cnt == 0)
	    rep_cnt = 1;
	  nextword(&i, &j, NW_BACK|NW_ENDOFWORD|NW_MUSTMOVE, rep_cnt);
	  revto(i, j);
	  break;
	case 'a':
	  append_mode = 1 - append_mode;
	  debug1("append mode %d--\n", append_mode);
	  Msg(0, (append_mode) ? ":set append" : ":set noappend");
	  break;
	case 'v':
	case 'V':
	  /* this sets start column to column 9 for VI :set nu users */
	  if (left_mar == 8)
	    rep_cnt = 1;
	  else
	    rep_cnt = 9;
	  /* FALLTHROUGH */
	case 'c':
	case 'C':
	  /* set start column (c) and end column (C) */
	  if (second)
	    {
	      rem(x1, y1, cx, cy, 1, (char *)0, screenheight-1); /* Hack */
	      second = 1;	/* rem turns off second */
	    }
	  rep_cnt--;
	  if (rep_cnt < 0)
	    rep_cnt = cx;
	  if (od != 'C')
	    {
	      left_mar = rep_cnt;
	      if (left_mar > right_mar)
		left_mar = right_mar;
	    }
	  else
	    {
	      right_mar = rep_cnt;
	      if (left_mar > right_mar)
		right_mar = left_mar;
	    }
	  if (second)
	    {
	      int x = cx, y = cy;
	      cx = x1; cy = y1;
	      revto(x, y);
	    }
	  if (od == 'v' || od == 'V')
	    Msg(0, (left_mar != 8) ? ":set nonu" : ":set nu");
	  break;
	case 'J':
	  /* how do you join lines in VI ? */
	  nonl = (nonl + 1) % 3;
	  switch (nonl)
	    {
	    case 0:
	      if (join_with_cr)
		Msg(0, "Multiple lines (CR/LF)");
	      else
		Msg(0, "Multiple lines (LF)");
	      break;
	    case 1:
	      Msg(0, "Lines joined");
	      break;
	    case 2:
	      Msg(0, "Lines joined with blanks");
	      break;
	    }
	  break;
	case 'y':
	case 'Y':
	  if (!second)
	    {
	      revto(linestart(cy), cy);
	      second++;
	      x1 = cx;
	      y1 = cy;
	    }
	  if (--rep_cnt > 0)
	    revto(cx, cy + rep_cnt);
	  revto(lineend(cy), cy);
	  if (od == 'y')
	    break;
	  /* FALLTHROUGH */
	case 'W':
	  if (od == 'W')
	    {
	      if (rep_cnt == 0)
		rep_cnt = 1;
	      if (!second)
		{
		  i = cx;
		  j = cy;
		  nextword(&i, &j, NW_BACK|NW_ENDOFWORD, 1);
		  revto(i, j);
		  second++;
		  x1 = cx;
		  y1 = cy;
		}
	      i = cx;
	      j = cy;
	      nextword(&i, &j, NW_ENDOFWORD, rep_cnt);
	      revto(i, j);
	    }
	  /* FALLTHROUGH */
	case 'A':
	  if (od == 'A')
	    append_mode = 1;
	  /* FALLTHROUGH */
	case '>':
	  if (od == '>')
	    write_buffer = 1;
	  /* FALLTHROUGH */
	case ' ':
	case '\r':
	  if (!second)
	    {
	      second++;
	      x1 = cx;
	      y1 = cy;
	      revto(x1, y1);
#ifdef NETHACK
	      if (nethackflag)
		Msg(0, "You drop a magic marker - Column %d Line %d",
	    	    cx+1, W2D(cy)+1, hist_offset);
	      else
#endif
	      Msg(0, "First mark set - Column %d Line %d", cx+1, cy+1);
	      break;
	    }
	  else
	    {
	      x2 = cx;
	      y2 = cy;
	      newcopylen = rem(x1, y1, x2, y2, 2, (char *)0, 0); /* count */
	      if (copybuffer != NULL && !append_mode)
		{
		  copylen = 0;
		  Free(copybuffer);
		}
	      if (newcopylen > 0)
		{
		  /* the +3 below is for : cr + lf + \0 */
		  if (copybuffer != NULL)
		    copybuffer = realloc(copybuffer,
			(unsigned) (copylen + newcopylen + 3));
		  else
		    {
		    copylen = 0;
		    copybuffer = malloc((unsigned) (newcopylen + 3));
		    }
		  if (copybuffer == NULL)
		    {
		      AbortMarkRoutine();
		      Msg(0, "Not enough memoooh!... Sorry.");
		      copylen = 0;
		      copybuffer = NULL;
		      break;
		    }
		  if (append_mode)
		    {
		      switch (nonl)
			/* 
			 * this code defines, what glues lines together
			 */
			{
			case 0:
			  if (join_with_cr)
			    {
			      copybuffer[copylen] = '\r';
			      copylen++;
			    }
			  copybuffer[copylen] = '\n';
			  copylen++;
			  break;
			case 1:
			  break;
			case 2:
			  copybuffer[copylen] = ' ';
			  copylen++;
			  break;
			}
		    }
		  yend = screenheight - 1;
		  if (fore->histheight - hist_offset < screenheight)
		    {
		      second = 0;
		      yend -= MarkScrollUpDisplay(fore->histheight - hist_offset);
		    }
		  copylen += rem(x1, y1, x2, y2, hist_offset == fore->histheight, copybuffer + copylen, yend);
		}
	      if (hist_offset != fore->histheight)
		{
		  in_ovl = 0;	/* So we can use Activate() */
		  Activate(0);
		}
	      ExitOverlayPage();
	      if (append_mode)
		Msg(0, "Appended %d characters to buffer",
		    newcopylen);
	      else
		Msg(0, "Copied %d characters into buffer", copylen);
	      if (write_buffer)
		WriteFile(DUMP_EXCHANGE);
	      in_mark = 0;
	      break;
	    }
	default:
	  AbortMarkRoutine();
#ifdef NETHACK
	  if (nethackflag)
	    Msg(0, "You escaped the dungeon.");
	  else
#endif
	  Msg(0, "Copy mode aborted");
	  break;
	}
      rep_cnt = 0;
    }
  fflush(stdout);
  *inbufp = pt;
  *inlenp = inlen;
}

static void revto(tx, ty)
int tx, ty;
{
  revto_line(tx, ty, -1);
}

/* tx, ty: WINDOW,  line: DISPLAY */
static void revto_line(tx, ty, line)
int tx, ty, line;
{
  int fx, fy;
  int x, y, t, revst, reven, qq, ff, tt, st, en, ce = 0;
  int ystart = 0, yend = screenheight-1;
  int i, ry;
 
  if (tx < 0)
    tx = 0;
  else if (tx > screenwidth - 1)
    tx = screenwidth -1;
  if (ty < 0)
    ty = 0;
  else if (ty > fore->histheight + screenheight - 1)
    ty = fore->histheight + screenheight - 1;
  
  fx = cx; fy = cy;
  cx = tx; cy = ty;
/*debug2("revto(%d, %d, ", x1, y1);
  debug2("%d, %d, ", fx, fy);
  debug2("%d, %d)\n", tx, ty);*/
 
  /*
   * if we go to a position that is currently offscreen 
   * then scroll the screen
   */
  i = 0;
  if (line >= 0 && line < screenheight)
    i = W2D(ty) - line;
  else if (ty < hist_offset)
    i = ty - hist_offset;
  else if (ty > hist_offset + (screenheight-1))
    i = ty-hist_offset-(screenheight-1);
  if (i > 0)
    yend -= MarkScrollUpDisplay(i);
  else if (i < 0)
    ystart += MarkScrollDownDisplay(-i);

  if (second == 0)
    {
      GotoPos(tx, W2D(cy));
      return;
    }
  
  qq = x1 + y1 * screenwidth;
  ff = fx + fy * screenwidth; /* "from" offset in WIN coords */
  tt = tx + ty * screenwidth; /* "to" offset  in WIN coords*/
 
  if (ff > tt)
    {
      st = tt; en = ff;
      x = tx; y = ty;
    }
  else
    {
      st = ff; en = tt;
      x = fx; y = fy;
    }
  if (st > qq)
    {
      st++;
      x++;
    }
  if (en < qq)
    en--;
  if (tt > qq)
    {
      revst = qq; reven = tt;
    }
  else
    {
      revst = tt; reven = qq;
    }
  ry = y - hist_offset;
  if (ry < ystart)
    {
      y += (ystart - ry);
      x = 0;
      st = y * screenwidth;
      ry = ystart;
    }
  for (t = st; t <= en; t++, x++)
    {
      if (x >= screenwidth)
	{
	  x = 0;
	  y++, ry++;
	}
      if (ry > yend)
	break;
      if (t == st || x == 0)
	{
	  for (ce = screenwidth-1; ce >= 0; ce--)
	    if (iWIN(y)[ce] != ' ')
	      break;
	}
      if (x <= ce && x >= left_mar && x <= right_mar
          && (LP || x < screenwidth-1 || ry < screenbot))
	{
	  GotoPos(x, W2D(y));
	  if (t >= revst && t <= reven)
	    SaveSetAttr(A_SO, ASCII);
	  else
	    SaveSetAttr(aWIN(y)[x], fWIN(y)[x]);
	  PUTCHAR(iWIN(y)[x]);
	}
    }
  GotoPos(tx, W2D(cy));
}

static void AbortMarkRoutine()
{
  int yend, redisp;

  yend = screenheight - 1;
  redisp = second;
  if (fore->histheight - hist_offset < screenheight)
    {
      second = 0;
      yend -= MarkScrollUpDisplay(fore->histheight - hist_offset);
    }
  if (hist_offset != fore->histheight)
    {
      in_ovl = 0;	/* So we can use Activate() */
      Activate(0);	/* to do a complete redisplay */
    }
  else
    {
      rem(x1, y1, cx, cy, redisp, (char *)0, yend);
    }
  ExitOverlayPage();
  in_mark = 0;
}


static void MarkRedisplayLine(y, xs, xe, isblank)
int y; /* NOTE: y is in DISPLAY coords system! */
int xs, xe;
int isblank;
{
  int x, i, rm;
  int sta, sto, cp; /* NOTE: these 3 are in WINDOW coords system */
  char *wi, *wa, *wf, *oldi;
 
  InsertMode(0); /* Not done in DisplayLine() */

  wi = iWIN(D2W(y));
  wa = aWIN(D2W(y));
  wf = fWIN(D2W(y));
  oldi = isblank ? blank : null;
 
  if (second == 0)
    {
      DisplayLine(oldi, null, null, wi, wa, wf, y, xs, xe);
      return;
    }
 
  sta = y1 * screenwidth + x1;
  sto = cy * screenwidth + cx;
  if (sta > sto)
    {
      i=sta; sta=sto; sto=i;
    }
  cp = D2W(y) * screenwidth + xs;
 
  rm = right_mar;
  for (x = screenwidth - 1; x >= 0; x--)
    if (wi[x] != ' ')
      break;
  if (x < rm)
    rm = x;
 
  for (x = xs; x <= xe; x++, cp++)
    if (cp >= sta && x >= left_mar)
      break;
  if (x > xs)
    DisplayLine(oldi, null, null, wi, wa, wf, y, xs, x-1);
  for (; x <= xe; x++, cp++)
    {
      if (cp > sto || x > rm || (!LP && x >= screenwidth-1 && y == screenbot))
	break;
      GotoPos(x, y);
      SaveSetAttr(A_SO, ASCII);
      PUTCHAR(wi[x]);
    }
  if (x<=xe)
    DisplayLine(oldi, null, null, wi, wa, wf, y, x, xe);
}


static int
MarkRewrite(ry, xs, xe, doit)
int ry, xs, xe, doit;
{
  int dx, x, y, st, en, t, rm;
  char *a, *f, *i;

  y = D2W(ry);
  dx = xe - xs;
  if (doit)
    {
      i = iWIN(y) + xs;
      while (dx--)
        PUTCHAR(*i++);
      return(0);
    }
  
  a = aWIN(y) + xs,
  f = fWIN(y) + xs;
  if (second == 0)
    st = en = -1;
  else
    {
      st = y1 * screenwidth + x1;
      en = cy * screenwidth + cx;
      if (st > en)
        {
          t = st; st = en; en = t;
        }
    }
  t = y * screenwidth + xs;
  for (rm=screenwidth-1, i=iWIN(y) + screenwidth-1; rm>=0; rm--)
    if (*i-- != ' ')
      break;
  if (rm > right_mar)
    rm = right_mar;
  x = xs;
  while (dx--)
    {
      if (t >= st && t <= en && x >= left_mar && x <= rm)
        {
	  if (GlobalAttr != A_SO || GlobalCharset != ASCII)
	    return(EXPENSIVE);
        }
      else
        {
	  if (GlobalAttr != *a || GlobalCharset != *f)
	    return(EXPENSIVE);
        }
      a++, f++, t++, x++;
    }
  return(xe - xs);
}


/*
 * scroll the screen contents up/down.
 */
static int MarkScrollUpDisplay(n)
int n;
{
  int i;
 
  debug1("MarkScrollUpDisplay(%d)\n", n);
  if (n <= 0)
    return 0;
  if (n > fore->histheight - hist_offset)
    n = fore->histheight - hist_offset;
  i = (n < screenheight) ? n : (screenheight);
  ScrollRegion(0, screenheight - 1, i);
  hist_offset += n;
  while (i-- > 0)
    MarkRedisplayLine(screenheight-i-1, 0, screenwidth-1, 1);
  return n;
}

static int MarkScrollDownDisplay(n)
int n;
{
  int i;
 
  debug1("MarkScrollDownDisplay(%d)\n", n);
  if (n <= 0)
    return 0;
  if (n > hist_offset)
    n = hist_offset;
  i = (n < screenheight) ? n : (screenheight);
  ScrollRegion(0, screenheight - 1, -i);
  hist_offset -= n;
  while (i-- > 0)
    MarkRedisplayLine(i, 0, screenwidth-1, 1);
  return n;
}

