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
  static char rcs_id[] = "$Id: window.c,v 1.2 92/02/03 02:28:20 jnweiger Exp $ FAU";
#endif

#include <stdio.h>
#include <sys/types.h>
#include <signal.h>
#ifndef sun
#include <sys/ioctl.h>
#endif
#ifdef BSDI
# include <string.h>
#endif /* BSDI */

#ifdef ISC
# include <sys/tty.h>
# include <sys/sioctl.h>
# include <sys/pty.h>
#endif

#ifdef MIPS
extern int errno;
#endif

#include "config.h"
#include "screen.h"
#include "extern.h"

static void FreeScrollback __P((struct win *));

static int ResizeHistArray __P((struct win *, char ***, int, int, int));
static int ResizeScreenArray __P((struct win *, char ***, int, int, int));
static void FreeArray __P((char ***, int));

extern TermcapCOLS, TermcapROWS;
extern int maxwidth;
extern int default_width, default_height, screenwidth, screenheight;
extern char *blank, *null, *OldImage, *OldAttr;
extern char *OldFont, *LastMsg;
extern struct win *wtab[], *fore;
extern int WinList, ForeNum;
extern char *Z0, *Z1, *WS;
extern int Z0width, Z1width;

extern int Detached;

#if defined(TIOCGWINSZ) || defined(TIOCSWINSZ)
  struct winsize glwz;
#endif

/*
 * ChangeFlag:   0: try to modify no window
 *               1: modify fore (and try to modify no other)
 *               2: modify all windows
 *
 * Note: Activate() is only called if change_flag == 1
 *       i.e. on a WINCH event
 */

void
CheckScreenSize(change_flag)
int change_flag;
{
  int width, height, n;
  struct win *p;

  if (Detached)
    {
      debug("CheckScreenSize: Detached -> No check.\n");
      return;
    }
#ifdef TIOCGWINSZ
  if (ioctl(0, TIOCGWINSZ, &glwz) != 0)
    {
      debug1("CheckScreenSize: ioctl(0, TIOCGWINSZ) errno %d\n", errno);
      width = TermcapCOLS;
      height = TermcapROWS;
    }
  else
    {
      width = glwz.ws_col;
      height = glwz.ws_row;
      if (width == 0)
        width = TermcapCOLS;
      if (height == 0)
        height = TermcapROWS;
    }
#else
  width = TermcapCOLS;
  height = TermcapROWS;
#endif
  
  debug2("CheckScreenSize: screen is (%d,%d)\n", width, height);

  if (change_flag == 2)
    {
      for (n = WinList; n != -1; n = p->WinLink)
        {
          p = wtab[n];
          ChangeWindowSize(p, width, height);
	}
    }
  if (screenwidth == width && screenheight == height)
    {
      debug("CheckScreenSize: No change -> return.\n");
      return;
    }
  ChangeScreenSize(width, height, change_flag);
  if (change_flag == 1 && WinList != -1)	/* was HasWindow */
    Activate(fore->norefresh);
}

void
ChangeScreenSize(width, height, change_fore)
int width, height;
int change_fore;
{
  struct win *p;
  int n, wwi;

  if (screenwidth == width && screenheight == height)
    {
      debug("ChangeScreenSize: no change\n");
      return;
    }
  debug3("ChangeScreenSize to (%d,%d) (change_fore: %d)\n",width, height, change_fore);
  screenwidth = width;
  screenheight = height;

  if (WS)
    {
      default_width = TermcapCOLS;
      default_height = TermcapROWS;
    }
  else
    {
      if (Z0 && (width == Z0width || width == Z1width) &&
          (TermcapCOLS == Z0width || TermcapCOLS == Z1width))
        default_width = TermcapCOLS;
      else
        default_width = width;
      default_height = height;
    }
  debug2("Default size: (%d,%d)\n",default_width, default_height);
  if (change_fore)
    {
      if (WinList != -1 && change_fore) /* was HasWindow */
        {
          debug("Trying to change fore.\n");
          ChangeWindowSize(fore, width, height);
        }
    }
  if (WS == NULL)
    {
      /* We have to adapt all windows */
      for (n = WinList; n != -1; n = p->WinLink)
        {
          p = wtab[n];
          debug1("Trying to change window %d.\n",n);
          wwi = width;
          if (Z0 && (width==Z0width || width==Z1width))
	    {
	      if (p->width > (Z0width + Z1width) / 2)
		wwi = Z0width;
	      else
		wwi = Z1width;
	    }
          ChangeWindowSize(p, wwi, height);
        }
    }
}

int
ChangeScrollback(p, histheight, histwidth)
struct win *p;
int histheight, histwidth;
{
  if (histheight > MAXHISTHEIGHT)
    histheight = MAXHISTHEIGHT;
  debug2("ChangeScrollback(..., %d, %d)\n", histheight, histwidth);
  debug2("  was %d, %d\n", p->histheight, p->width);

  if (histheight == 0)
    {
      FreeScrollback(p);
      return 0;
    }

  if (ResizeHistArray(p, &p->ihist, histwidth, histheight, 1)
      || ResizeHistArray(p, &p->ahist, histwidth, histheight, 0)
      || ResizeHistArray(p, &p->fhist, histwidth, histheight, 0))
    {
      debug("   failed, removing all histbuf\n");
      FreeScrollback(p);
      Msg_nomem;
      return (-1);
    }
  if (p->histheight != histheight)
    p->histidx = 0;
  p->histheight = histheight;

  return(0);
}

static void FreeScrollback(p)
struct win *p;
{
  FreeArray(&p->ihist, p->histheight);
  FreeArray(&p->ahist, p->histheight);
  FreeArray(&p->fhist, p->histheight);
  p->histheight = 0;
}

static int
ResizeHistArray(p, arr, wi, hi, fillblank)
struct win *p;
char ***arr;
int wi, hi, fillblank;
{
  char **narr, **np, **onp, **onpe;
  int t, x, first;

  if (p->width == wi && p->histheight == hi)
    return(0);
  if (p->histheight != hi)
    {
      if ((narr = (char **)calloc(sizeof(char *), hi)) == NULL)
	{
	  FreeArray(arr, p->histheight);
	  return(-1);
	}
      np = narr;
      onp = (*arr) + p->histidx;
      onpe = (*arr) + p->histheight;
      first = p->histheight - hi;
      if (first<0)
	 np-=first;
      for(t=0; t<p->histheight; t++)
	{
          if (t-first >=0 && t-first < hi)
	    *np++ = *onp;
	  else
	    Free(*onp);
	  if (++onp == onpe)
	    onp = *arr;
	}
      if (*arr)
	Free(*arr);
    }
  else
    narr = *arr;
 
  for (t=0, np=narr; t<hi; t++, np++)
    {
      x = p->width;
      if (*np == 0)
	{
	  *np = (char *)malloc(wi);
          x = 0;
	}
      else if (p->width != wi)
	{
	  *np = (char *)xrealloc(*np, wi);
	}
      if (*np == 0)
	{
	  FreeArray(&narr, hi);
	  return(-1);
	}
      if (x<wi)
	{
	  if (fillblank)
	    bclear(*np+x, wi-x);
	  else
	    bzero(*np+x, wi-x);
	}
    }
  *arr = narr;
  return(0);
}
      

static int
ResizeScreenArray(p, arr, wi, hi, fillblank)
struct win *p;
char ***arr;
int wi, hi, fillblank;
{
  int minr;
  char **cp;

  if (p->width == wi && p->height == hi)
    return(0);

  if (hi > p->height)
    minr = p->height;
  else
    minr = hi;

  if (p->height > hi)
    {
      for (cp = *arr; cp < *arr + (p->height - hi); cp++)
	Free(*cp);
      bcopy((char *)(*arr + (p->height - hi)), (char *)(*arr),
	    hi * sizeof(char *));
    }
  if (*arr && p->width != wi)
    for (cp = *arr; cp < *arr + minr; cp++)
      {
	if ((*cp = (char *)xrealloc(*cp, (unsigned) wi)) == 0)
	  {
	    FreeArray(arr, p->height);
	    return(-1);
	  }
	if (wi > p->width)
	  {
	    if (fillblank)
	      bclear(*cp + p->width, wi - p->width);
	    else
	      bzero(*cp + p->width, wi - p->width);
	  }
      }
  if (*arr)
    *arr = (char **) xrealloc((char *) *arr, (unsigned) hi * sizeof(char *));
  else
    *arr = (char **) malloc((unsigned) hi * sizeof(char *));
  if (*arr == 0)
    return(-1);
  for (cp = *arr + p->height; cp < *arr + hi; cp++)
    {
      if ((*cp = malloc((unsigned) wi)) == 0)
	{
	  while (--cp >= *arr)
	    Free(*cp);
	  Free(*arr);
          return(-1);
	}
      if (fillblank)
	bclear(*cp, wi);
      else
	bzero(*cp, wi);
    }
  return(0);
}

static void
FreeArray(arr, hi)
char ***arr;
int hi;
{
  register char **p;
  register int t;

  if (*arr == 0)
    return;
  for (t = hi, p = *arr; t--; p++)
    if (*p)
      Free(*p);
  Free(*arr);
}


int
ChangeWindowSize(p, width, height)
struct win *p;
int width, height;
{
  int t, scr;
  
  if (width > maxwidth)
    {
      maxwidth = width;
      debug1("New maxwidth: %d\n", maxwidth);
      if (blank == 0)
        blank = malloc((unsigned) maxwidth);
      else
        blank = xrealloc(blank, (unsigned) maxwidth);
      if (null == 0)
        null = malloc((unsigned) maxwidth);
      else
        null = xrealloc(null, (unsigned) maxwidth);
      if (OldImage == 0)
        OldImage = malloc((unsigned) maxwidth);
      else
        OldImage = xrealloc(OldImage, (unsigned) maxwidth);
      if (OldAttr == 0)
        OldAttr = malloc((unsigned) maxwidth);
      else
        OldAttr = xrealloc(OldAttr, (unsigned) maxwidth);
      if (OldFont == 0)
        OldFont = malloc((unsigned) maxwidth);
      else
        OldFont = xrealloc(OldFont, (unsigned) maxwidth);
      if (LastMsg == 0)
        {
          LastMsg = malloc((unsigned) maxwidth + 1);
          *LastMsg = 0;
        }
      else
        LastMsg = xrealloc(LastMsg, (unsigned) maxwidth + 1);
      LastMsg[maxwidth]=0;
      if (!(blank && null && OldImage && OldAttr && OldFont && LastMsg))
	{
nomem:	  for (t = WinList; t != -1 && wtab[t] != p; t = p->WinLink) 
	    ;
	  if (t >= 0)
	    KillWindow(t);
	  Msg(0, "Out of memory -> Window destroyed !!");
	  return(-1);
	}
      MakeBlankLine(blank, maxwidth);
      bzero(null, maxwidth);
    }
  
  if (width == p->width && height == p->height)
    {
      debug("ChangeWindowSize: No change.\n");
      return(0);
    }

  debug2("ChangeWindowSize from (%d,%d) to ", p->width, p->height);
  debug2("(%d,%d)\n", width, height);

  if (width == 0 && height == 0)
    {
      FreeArray(&p->image, p->height);
      FreeArray(&p->attr, p->height);
      FreeArray(&p->font, p->height);
      if (p->tabs)
	Free(p->tabs);
      p->width = 0;
      p->height = 0;
      FreeScrollback(p);
      return(0);
    }

  /* when window gets smaller, scr is the no. of lines we scroll up */
  scr = p->height - height;
  if (scr < 0)
    scr = 0;
  for (t = 0; t < scr; t++)
    AddLineToHist(p, p->image+t, p->attr+t, p->font+t); 
  if (ResizeScreenArray(p, &p->image, width, height, 1)
      || ResizeScreenArray(p, &p->attr, width, height, 0)
      || ResizeScreenArray(p, &p->font, width, height, 0))
    {
      goto nomem;
    }
  /* this won't change the height of the scrollback history buffer, but
   * it will check the width of the lines.
   */
  ChangeScrollback(p, p->histheight, width);

  if (p->tabs == 0)
    {
      /* tabs get width+1 because 0 <= x <= width */
      if ((p->tabs = malloc((unsigned) width + 1)) == 0)
        goto nomem;
      t = 8;
    }
  else
    {
      if ((p->tabs = xrealloc(p->tabs, (unsigned) width + 1)) == 0)
        goto nomem;
      t = p->width;
    }
  for (t = (t + 7) & 8; t < width; t += 8)
    p->tabs[t] = 1; 
  p->height = height;
  p->width = width;
  if (p->x >= width)
    p->x = width - 1;
  if ((p->y -= scr) < 0)
    p->y = 0;
  if (p->Saved_x >= width)
    p->Saved_x = width - 1;
  if ((p->Saved_y -= scr) < 0)
    p->Saved_y = 0;
  if (p->autoaka > 0) 
    if ((p->autoaka -= scr) < 1)
      p->autoaka = 1;
  p->top = 0;
  p->bot = height - 1;
#ifdef TIOCSWINSZ
  if (p->ptyfd && p->wpid)
    {
      glwz.ws_col = width;
      glwz.ws_row = height;
      debug("Setting pty winsize.\n");
      if (ioctl(p->ptyfd, TIOCSWINSZ, &glwz))
	debug2("SetPtySize: errno %d (fd:%d)\n", errno, p->ptyfd);
# if defined(STUPIDTIOCSWINSZ) && defined(SIGWINCH)
#  ifdef POSIX
      pgrp = tcgetpgrp(p->ptyfd);
#  else
      if (ioctl(p->ptyfd, TIOCGPGRP, &pgrp))
	pgrp = 0;
#  endif
      if (pgrp)
	{
	  debug1("Sending SIGWINCH to pgrp %d.\n", pgrp);
	  if (killpg(pgrp, SIGWINCH))
	    debug1("killpg: errno %d\n", errno);
	}
      else
	debug1("Could not get pgrp: errno %d\n", errno);
# endif /* STUPIDTIOCSWINSZ */
    }
#endif
  return(0);
}


void
ResizeScreen(wi)
struct win *wi;
{
  int width, height;

  if (wi)
    {
      width = wi->width;
      height = wi->height;
    }
  else
    {
      width = default_width;
      height = default_height;
    }
  if (screenwidth == width && screenheight == height)
    {
      debug("ResizeScreen: No change\n");
      return;
    }
  debug2("ResizeScreen: to (%d,%d).\n", width, height);
  if (WS)
    {
      debug("ResizeScreen: using WS\n");
      WSresize(width, height);
      ChangeScreenSize(width, height, 0);
    }
  else if (Z0 && (width == Z0width || width == Z1width))
    {
      debug("ResizeScreen: using Z0/Z1\n");
      PutStr(width == Z0width ? Z0 : Z1);
      ChangeScreenSize(width, screenheight, 0);
    }
  if (screenwidth != width || screenheight != height)
    {
      debug2("BUG: Cannot resize from (%d,%d)",screenwidth, screenheight);
      debug2(" to (%d,%d) !!\n", width, height);
      if (wi)
	ChangeWindowSize(wi, screenwidth, screenheight);
    }
}

char *
xrealloc(mem, len)
char *mem;
int len;
{
  register char *nmem;

  if (nmem = realloc(mem, len))
    return(nmem);
  free(mem);
  return((char *)0);
}
