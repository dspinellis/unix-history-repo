/* X Communication module for terminals which understand the X protocol.
   Copyright (C) 1985 Free Software Foundation, Inc.

This file is part of GNU Emacs.

GNU Emacs is distributed in the hope that it will be useful,
but WITHOUT ANY WARRANTY.  No author or distributor
accepts responsibility to anyone for the consequences of using it
or for whether it serves any particular purpose or works at all,
unless he says so in writing.  Refer to the GNU Emacs General Public
License for full details.

Everyone is granted permission to copy, modify and redistribute
GNU Emacs, but only under the conditions described in the
GNU Emacs General Public License.   A copy of this license is
supposed to have been given to you along with GNU Emacs so you
can know your rights and responsibilities.  It should be in a
file named COPYING.  Among other things, the copyright notice
and this notice must be preserved on all copies.  */

/* Written by Yakim Martillo.  */

/*
 *	$Source: /u1/third_party/gnuemacs.v17/src/RCS/xterm.c,v $
 *	$Author: rlk $
 *	$Locker:  $
 *	$Header: xterm.c,v 1.13 86/02/17 12:24:48 rlk Exp $
 */

#ifndef lint
static char *rcsid_TrmXTERM_c = "$Header: xterm.c,v 1.13 86/02/17 12:24:48 rlk Exp $";
#endif	lint

#include "config.h"

/* This includes sys/types.h, and that somehow loses
   if this is not done before the other system files.  */
#include "xterm.h"

#include <sys/time.h>
#include <sys/ioctl.h>
#include <stdio.h>
#include <ctype.h>
#include <errno.h>
#include <signal.h>
#include <strings.h>
#include <sys/stat.h>

#include "dispextern.h"
#include "termhooks.h"
#include "termopts.h"
#include "termchar.h"
#include "sink.h"
#include "sinkmask.h"
/*#include <X/Xproto.h>	*/

#define min(a,b) ((a)<(b) ? (a) : (b))
#define sigunblockx(sig) sigblock (0)
#define sigblockx(sig) sigblock (1 << ((sig) - 1))
XREPBUFFER Xxrepbuffer;
int pixelwidth;
int pixelheight;
int PendingExposure;
int PendingIconExposure;
#define MAXICID 80
char iconidentity[MAXICID];
#define ICONTAG "emacs@"
#define METABIT 0x80
Window XXIconWindow;
Bitmap XXIconMask;

char *XXcurrentfont;
char *default_window;
int informflag;
extern struct display_line *DesiredScreen[], *PhysScreen[];
extern int initialized;
int XXdebug;
int XXpid;
extern int screen_garbaged;
int XXxoffset, XXyoffset;
int IconWindow;

int WindowMapped;
int CurHL;

static int flexlines;		/* last line affect by dellines or */
				/* inslines functions */
extern int errno;
int VisibleX, VisibleY;	/* genuine location of cursor on screen */
				/* if it is there */
static int SavedX, SavedY;	/* Where the cursor was before update */
				/* started */

int bitblt;		/* Used to track bit blt events */
int CursorExists;	/* during updates cursor is turned off */
static int InUpdate;		/* many of functions here may be invoked */
				/* even if no update in progress, when */
				/* no update is in progress the action */
				/* can be slightly different */
char MouseCursor[33] ="\000\000\002\000\006\000\016\000\036\000\076\000\
\176\000\376\000\376\001\076\000\066\000\142\000\140\000\300\000\300\000\000\
\000";

char MouseMask[33] = "\003\000\007\000\017\000\037\000\077\000\177\000\
\377\000\377\001\377\003\377\003\177\000\367\000\363\000\340\001\340\001\
\300\000";

Display *XXdisplay;
FontInfo *fontinfo;
Window XXwindow;
Cursor EmacsCursor;

char *fore_color;	/* Variables to store colors */
char *back_color;
char *brdr_color;
char *curs_color;
char *mous_color;

int fore;
int back;
int brdr;
int curs;
int mous;

static WindowInfo windowinfo;
WindowInfo rootwindowinfo;



static XEvent XXEvent;    	/* as X messages are read in they are */
                                /* stored here */
static XREPBUFFER XXqueue;/* Used for storing up ExposeRegion */
				/* replies, so that the SIGIO inter- */
				/* rupt serving routines do almost */
				/* no writes to the X socket */
/*int CurHL;			/* Current Highlighting actually being */
				/* being used for bold font right now*/

int XXborder;


extern Display *XOpenDisplay ();
extern Window XCreateWindow ();
extern Cursor XDefineCursor ();
extern Cursor XCreateCursor ();
extern FontInfo *XOpenFont ();


/* HLmode -- Changes the GX function for output strings.  Could be used to
 * change font.  Check an XText library function call. 
 */

static
HLmode (new)
     int new;
{
  CurHL =  new;
}


/* External interface to control of standout mode.
   Call this when about to modify line at position VPOS
   and not change whether it is highlighted.  */

XTreassert_line_highlight (highlight, vpos)
     int highlight, vpos;
{
  HLmode (highlight);
}

/* Call this when about to modify line at position VPOS
   and change whether it is highlighted.  */

static
XTchange_line_highlight (new_highlight, vpos, first_unused_hpos)
     int new_highlight, vpos, first_unused_hpos;
{
  HLmode (new_highlight);
  XTtopos (vpos, 0);
  XTclear_end_of_line (0);
}


/* Used for starting or restarting (after suspension) the X window.  Puts the
 * cursor in a known place, update does not begin with this routine but only
 * with a call to DoDsp.  The mouse cursor is warped into the window and then
 * the cursor is turned on.
 */



static
XTset_terminal_modes ()
{
  int stuffpending;
#ifdef XDEBUG
  fprintf (stderr, "XTset_terminal_modes\n");
#endif
  InUpdate = 0;
  stuffpending = 0;
  if (!initialized)
    {
      CursorExists = 0;
      VisibleX = 0;
      VisibleY = 0;
    }
  XTclear_screen ();
  ioctl (0, FIONREAD, &stuffpending);
  if (stuffpending)
    kill (XXpid, SIGIO);
}

/* XTtopos moves the cursor to the correct location and checks whether an update
 * is in progress in order to toggle it on.
 */

static
XTtopos (row, col)
     register int row, col;
{
#ifdef XDEBUG
  fprintf (stderr, "XTtopos\n");
#endif
  cursX = col;
  cursY = row;
  if (InUpdate)
    {
      if (CursorExists)
	{
	  CursorToggle ();
	}
      return;		/* Generally, XTtopos will be invoked */
      /* when InUpdate with !CursorExists */
      /* so that wasteful XFlush is not called */
    }
  if ((row == VisibleY) && (col == VisibleX))
    {
      if (!CursorExists)
	{
	  CursorToggle ();
	}
      XFlush ();
      return;
    }
  if (CursorExists) CursorToggle ();
  VisibleX = col;
  VisibleY = row;
  if (!CursorExists) CursorToggle ();
  XFlush ();
}

/* Used to get the terminal back to a known state after resets.  Usually
 * used when restarting suspended or waiting emacs
 */

static
cleanup ()
{
  inverse_video = 0;
  HLmode (0);
}

/* wipes out numcols columns starting a current column on the current line */
  
static
XTclear_end_of_line (first_blank)
     register int first_blank;
{
  register int numcols;

#ifdef XDEBUG
  fprintf (stderr, "XTclear_end_of_line\n");

#endif
  if (cursY < 0 || cursY >= screen_height)
    return;
  if (first_blank >= screen_width)
    return;

  if (first_blank < 0)
    first_blank = 0;
  numcols = screen_width - first_blank;
  if (cursY == VisibleY && VisibleX >= first_blank)
    {
      if (CursorExists) CursorToggle ();
    }
  XPixSet (XXwindow, 
	   first_blank * fontinfo->width, 
	   cursY * fontinfo->height, 
	   fontinfo->width * numcols,
	   fontinfo->height,
	   back);
  XTtopos (cursY, first_blank);
}

static
XTreset_terminal_modes ()
{
#ifdef XDEBUG
  fprintf (stderr, "XTreset_terminal_modes\n");
#endif
  XTclear_screen ();
}

static
XTclear_screen ()
{
#ifdef XDEBUG
  fprintf (stderr, "XTclear_screen\n");
#endif
  HLmode (0);
  CursorExists = 0;
  
  cursX = 0;
  cursY = 0;
  SavedX = 0;
  SavedY = 0;
  VisibleX = 0;
  VisibleY = 0;
  XClear (XXwindow);
  CursorToggle ();
  if (!InUpdate)
    XFlush ();
}

/* used by dumprectangle which is usually invoked upon ExposeRegion
 * events which come from bit blt's or moving an obscuring opaque window
 */

static
dumpchars (ActiveScreen, numcols, tempX, tempY, tempHL)
     register struct display_line **ActiveScreen;
     register int numcols;
     register int tempX, tempY, tempHL;
{
  if (numcols <= 0) return;
  if (((numcols - 1) + tempX) > screen_width)
    {
      numcols = (screen_width - tempX) + 1;
    }
  if ((tempX < 0) || (tempX >= screen_width) ||
      (tempY < 0) || (tempY >= screen_height))
    {
      return;
    }
  XText (XXwindow,
	 (tempX * fontinfo->width),
	 (tempY * fontinfo->height),
	 &ActiveScreen[tempY + 1]->body[tempX],
	 numcols,
	 fontinfo->id,
	 (tempHL ? back : fore),
	 (tempHL ? fore : back));
}

/* When a line has been changed this function is called.  X is so fast
 * that the actual sequence is ignore.  Rather, the new version of the
 * line is simply output if this function is invoked while in UpDate.
 * Sometimes writechars can be invoked when not in update if text is to
 * be output at the end of the line.  In this case the whole line is not
 * output.  Simply the new text at the current cursor position given
 * by VisibleX,Y.  The cursor is moved to the end of the new text.
 */
static
writechars (start, end)
     register char *start, *end;
{
  register int temp_length;

  if ((cursY < 0) || (cursY >= screen_height))
    {
      return;
    }
  if (CursorExists)
    {
      CursorToggle ();
    }
  if (InUpdate)
    {
      if (DesiredScreen && DesiredScreen[cursY + 1])
	{
	  temp_length = DesiredScreen[cursY + 1]->length;
	}
      else
	temp_length = 0;
      if (temp_length > 0)
	{
	  XText (XXwindow,
		 0,
		 (cursY * fontinfo->height),
		 &DesiredScreen[cursY + 1]->body[0],
		 temp_length,
		 fontinfo->id,
		 (CurHL ? back : fore),
		 (CurHL ? fore : back));
	  if (temp_length < screen_width)
	    {
	      XTclear_end_of_line (temp_length);
	    }
	  XTtopos (cursY, temp_length);
	}
      else
	{
	  XTclear_end_of_line (0);
	  XTtopos (cursY, 0);
	}
    }
  else
    {
      if ((VisibleX < 0) || (VisibleX >= screen_width))
	{
	  return;
	}
      if ((VisibleY < 0) || (VisibleY >= screen_height))
	{
	  return;
	}
      if (((end - start) + VisibleX) >= screen_width)
	{
	  end = start + (screen_width - (VisibleX + 1));
	}
      if(end >= start)
	{
	   XText (XXwindow,
		 (VisibleX * fontinfo->width),
		 (VisibleY * fontinfo->height),
		 start,
		 ((end - start) + 1),
		 fontinfo->id,
		 (CurHL ? back : fore),
		 (CurHL ? fore : back));
	  VisibleX = VisibleX + (end - start) + 1;
	}
      if (!CursorExists) CursorToggle ();
    }
}


static 
XTwrite_chars (start, len)
     register char *start;
     register int len;
{
#ifdef XDEBUG
  fprintf (stderr, "XTwrite_chars\n");
#endif
  writechars (start, start + len - 1);
}

/* The following routine is for the deaf or for the pervert who prefers 
 * that his terminal flashes at him rather than beep at him.
 */

static int flashedback;

static
XTflash ()
{
  struct itimerval itimer;
  extern int flashback ();

#ifdef XDEBUG
  fprintf (stderr, "XTflash\n");
#endif

  signal (SIGALRM, flashback);
  getitimer (ITIMER_REAL, &itimer);
  itimer.it_value.tv_usec += 250000;
  itimer.it_interval.tv_sec = 0;
  itimer.it_interval.tv_usec = 0;
  flashedback = 0;
  setitimer (ITIMER_REAL, &itimer, 0);
  XPixFill (XXwindow, 0, 0, screen_width * fontinfo->width,
	    screen_height * fontinfo->height, WhitePixel, ClipModeClipped,
	    GXinvert, AllPlanes);
  XFlush ();
  while (!flashedback) pause ();
}

static
flashback ()
{
  signal (SIGALRM, SIG_IGN);
  XPixFill (XXwindow, 0, 0, screen_width * fontinfo->width,
	    screen_height * fontinfo->height, WhitePixel, ClipModeClipped,
	    GXinvert, AllPlanes);
  XFlush ();
  flashedback = 1;
}	

/* A kludge to get a bell */

static
XTfeep ()
{
#ifdef XDEBUG
  fprintf (stderr, "XTfeep\n");
#endif
  XFeep (0);
}

/* Artificially creating a cursor is hard, the actual position on the
 * screen (either where it is or last was) is tracked with VisibleX,Y.
 * Gnu Emacs code tends to assume a cursor exists in hardward at cursX,Y
 * and that output text will appear there.  During updates, the cursor is
 * supposed to be blinked out and will only reappear after the update 
 * finishes.
 */

CursorToggle ()
{
  register struct display_line **ActiveScreen;
  if (!WindowMapped) 
    {
      CursorExists = 0;
      return 0;
    }
  if ((VisibleX < 0) || (VisibleX >= screen_width) ||
      (VisibleY < 0) || (VisibleY >= screen_height))
    {			/* Current Cursor position trash */
      /* Not much can be done */
      XFlush ();
      CursorExists = 0;
      return 0;	/* Currently the return values are not */
      /* used, but I could anticipate using */
      /* them in the future. */
    }
  /*  if(InUpdate && DesiredScreen)
      ActiveScreen = DesiredScreen;
      else*/
  ActiveScreen = PhysScreen;
  if (ActiveScreen && ActiveScreen[VisibleY + 1] &&
      (VisibleX < ActiveScreen[VisibleY + 1]->length))
    {
      if (CursorExists)
	{
	  XText (XXwindow,
		 VisibleX * fontinfo->width,
		 VisibleY * fontinfo->height,
		 &ActiveScreen[VisibleY + 1]->body[VisibleX], 1, 
		 fontinfo->id,
		 fore, back);
	}
      else
	{
	    XText (XXwindow,
		   VisibleX * fontinfo->width,
		   VisibleY * fontinfo->height,
		   &ActiveScreen[VisibleY + 1]->body[VisibleX], 1, 
		   fontinfo->id,
		   back, curs);
	}
    }
  else if (CursorExists)
    {
      XPixSet (XXwindow,
	       VisibleX * fontinfo->width,
	       VisibleY * fontinfo->height,
	       fontinfo->width, fontinfo->height, back);
    }
  else
    {
    XPixSet (XXwindow,
	     VisibleX * fontinfo->width,
	     VisibleY * fontinfo->height,
	     fontinfo->width, fontinfo->height, curs);
    }
  CursorExists = !CursorExists;/* Cursor has either been blinked in */
     /* or out */
  if (!InUpdate)
    {
      XFlush ();
    }
  return 1;
}

/* This routine is used by routines which are called to paint regions */
/* designated by ExposeRegion events.  If the cursor may be in the exposed */
/* region, this routine makes sure it is gone so that dumprectangle can */
/* toggle it back into existance if dumprectangle is invoked when not in */
/* the midst of a screen update. */
static 
ClearCursor ()
{
  if (!WindowMapped)
    {
      CursorExists = 0;
      return;
    }
  if ((VisibleX < 0) || (VisibleX >= screen_width)
      || (VisibleY < 0) || (VisibleY >= screen_height))
    {			/* Current Cursor position trash */
      /* Not much can be done */
      CursorExists = 0;
      return;
    }
  XPixSet (XXwindow,
	  VisibleX * fontinfo->width,
	  VisibleY * fontinfo->height,
	  fontinfo->width, fontinfo->height,
	  back);
  CursorExists = 0;
}

static
XTupdate_begin ()
{	
#ifdef XDEBUG
  fprintf (stderr, "XTupdate_begin\n");
#endif

  InUpdate = 1;
  if (CursorExists) 
    {
      CursorToggle ();
    }
  SavedX = cursX;		/* The initial"hardware" cursor position is */
  /*  saved because that is where gnu emacs */
  /*  expects the cursor to be at the end of*/
  /* the update */
  SavedY = cursY;
  dumpqueue();
}


static
XTupdate_end ()
{	
#ifdef XDEBUG
  fprintf (stderr, "XTupdate_end\n");
#endif
  if (CursorExists)
    CursorToggle ();
  InUpdate = 0;
  dumpqueue ();
  XTtopos (SavedY, SavedX);	/* XTtopos invokes cursor toggle */
}

/* Used for expose region and expose copy events.  Have to get the text
 * back into the newly blank areas.
 */

dumprectangle (top, left, rows, cols)
     register int top, left, rows, cols;
{
  register struct display_line **ActiveScreen;
  register int index;
  int localX, localY, localHL;
  rows += top;
  cols += left;
  top /= fontinfo->height;	/* Get row and col containing up and */
  /* left borders of exposed region -- */
  /* round down here*/
  left /= fontinfo->width;
  rows += (fontinfo->height - 1);
  cols += (fontinfo->width - 1);
  rows /= fontinfo->height;/* Get row and col containing bottom and */
  /* right borders -- round up here */
  rows -= top;
  cols /= fontinfo->width;
  cols -= left;
  if (rows < 0) return;
  if (cols < 0) return;
  if (top > (screen_height - 1)) return;
  if (left > (screen_width - 1)) return;
  if ((VisibleX >= left) && (VisibleX < (left + cols)) &&
      (VisibleY >= top) && (VisibleY < (top + rows)))
    {
      ClearCursor ();
    }
  if (InUpdate && DesiredScreen)
    ActiveScreen = PhysScreen;
  else if (PhysScreen)
    ActiveScreen = PhysScreen;/* When cue is dumped in update this */
  else
    return;
  /* should perhaps be DesiredScreen */
  /* but PhysScreen is guaranteed to contain*/
  /* date which was good for every line on */
  /* screen. For desired screen only for */
  /* lines which are changing.  Emacs does */
  /* not consider a line within a newly */
  /* exposed region necessarily to have */
  /* been changed.  Emacs knows nothing */
  /* about ExposeRegion events.*/
  for (localY = top, index = 0;
       (index < rows) && (localY < screen_height);
       ++index, ++localY)
    {
      if ((localY < 0) || (localY >= screen_height)) continue;
      if (!ActiveScreen[localY + 1]) continue;
      if ((left + 1) > ActiveScreen[localY + 1]->length) continue;
      localX = left;
      localHL = ActiveScreen[localY + 1]->highlighted;
      dumpchars (ActiveScreen,
		 min (cols,
		      ActiveScreen[localY + 1]->length
			- localX),
		 localX, localY, localHL);
    }
  if (!InUpdate && !CursorExists) CursorToggle ();
  /* Routine usually called */
  /* when not in update */
}

/* What sections of the window will be modified from the UpdateDisplay
 * routine is totally under software control.  Any line with Y coordinate
 * greater than flexlines will not change during an update.  This is really
 * used only during dellines and inslines routines (scraplines and stufflines)
 */
static
XTset_terminal_window (n)
     register int n;
{
#ifdef XDEBUG
  fprintf (stderr, "XTset_terminal_window\n");
#endif
  if ((n <= 0) || (n > screen_height))
    flexlines = screen_height;
  else
    flexlines = n;
}

XTins_del_lines (vpos, n)
     int vpos, n;
{
#ifdef XDEBUG
  fprintf (stderr, "XTins_del_lines\n");
#endif
  XTtopos (vpos, 0);
  if (n >= 0) stufflines (n);
  else scraplines (-n);
		
}

static 
XTinsert_chars (start, len)
     register char *start;
     register int len;
{
#ifdef XDEBUG
  fprintf (stderr, "XTinsert_chars\n");
#endif
  writechars (start, start + len - 1);
}

static 
XTdelete_chars (n)
     register int n;
{
  char *msg = "***Delete Chars Called Outside of Update!!!***";
#ifdef XDEBUG
  fprintf (stderr, "XTdelete_chars\n");
#endif
  writechars (msg, msg + strlen (msg) - 1);
}

static
stufflines (n)
     register int n;
{
  register int topregion, bottomregion;
  register int length, newtop;

  if (cursY >= flexlines)
    return;
  
  if (!WindowMapped)
    {
      bitblt = 0;
      return;
    }
  if (CursorExists) CursorToggle ();
  dumpqueue ();
  topregion = cursY;
  bottomregion = flexlines - (n + 1);
  newtop = cursY + n;
  length = (bottomregion - topregion) + 1;
  if ((length > 0) && (newtop <= flexlines))
    {
      /* Should already have cleared */
      /* queue of events associated */
      /* with old bitblts */
      XMoveArea (XXwindow, 0,
		 topregion * fontinfo->height,
		 0, newtop * fontinfo->height,
		 screen_width * fontinfo->width,
		 length * fontinfo->height);
      if (WindowMapped)
	bitblt = 1;
      request_sigio ();
      XFlush ();
      while (bitblt) 
	{
	  kill (XXpid, SIGIO);
	}
      unrequest_sigio ();
      XFlush ();
    }
  newtop = min (newtop, (flexlines - 1));
  length = newtop - topregion;
  if (length > 0)
    {
      XPixSet (XXwindow,
	       0,
	       topregion * fontinfo->height,
	       screen_width * fontinfo->width,
	       n * fontinfo->height,
	       back);
    }
  /*  if (!InUpdate) CursorToggle (); */
}

static
scraplines (n)
     register int n;
{
  if (!WindowMapped)
    {
      bitblt = 0;
      return;
    }

  if (cursY >= flexlines)
    return;
  if (CursorExists) CursorToggle ();
  dumpqueue ();
  if ((cursY + n) >= flexlines)
    {
      if (flexlines >= (cursY + 1))
	{
	  XPixSet (XXwindow,
		   0, cursY * fontinfo->height,
		   screen_width * fontinfo->width,
		   (flexlines - cursY) * fontinfo->height,
		   back);
	}
    }
  else
    {
      XMoveArea (XXwindow,
		 0, (cursY + n) * fontinfo->height,
		 0, cursY * fontinfo->height,
		 screen_width * fontinfo->width,
		 (flexlines - (cursY + n)) * fontinfo->height);
      if (WindowMapped)
	bitblt = 1;
      request_sigio ();
      XFlush ();
      while (bitblt)
	{
	  kill (XXpid, SIGIO);
	}
      unrequest_sigio ();
      XFlush ();
      XPixSet (XXwindow, 0, (flexlines - n) * fontinfo->height,
	       screen_width * fontinfo->width,
	       n * fontinfo->height, back);
    }
  /* if (!InUpdate) CursorToggle (); */
}
	
/* Substitutes for standard read routine.  Under X not interested in individual
 * bytes but rather individual packets.
 */

XTread_socket (sd, bufp, numchars)
     register int sd;
     register char *bufp;
     register int numchars;
{

  int count;
  int stuffpending;
  int temp_width, temp_height;
/*  typedef struct reply {XEvent event; struct reply *next} Reply;
    Reply *replies = NULL;*/

  count = 0;
  if (numchars <= 0)
    {	/* To keep from overflowing read buffer */
      numchars = 1;
      --bufp;
    }
  while (bitblt || XPending () != 0)
    {
      /* while there are more events*/
      XNextEvent (&XXEvent);
      switch (XXEvent.type)
	{
/*	case X_Reply:
	{
	extern char *malloc();
	Reply *reply = (Reply *) malloc (sizeof (Reply));
	reply->next = replies;
	reply->event = XXEvent;
	replies = reply;
	break;
	}*/
	default:
	  break;
	case ExposeWindow:
	  if (((XExposeEvent *)&XXEvent)->window == XXIconWindow)
	    {
	      PendingIconExposure = 1;
	    }
	  else
	    PendingExposure = 1;/* No reason to repeat */
	  /* this if several */
	  /* ExposeWindow events */
	  /* come in quick succes-*/
	  /* ion */
	  break;
	case ExposeRegion:
	  if (PendingExposure)
	    {	/* Don't bother with */
	      /* region events when */
	      /* full window event */
	      /* is pending */
	      break;
	    }
	  loadxrepbuffer (&XXEvent, &XXqueue);
	  if (XXqueue.rindex == XXqueue.windex)
	    {
	      PendingExposure = 1;
	    }
	  if ((XXqueue.rindex > XXqueue.mindex) ||
	      (XXqueue.windex > XXqueue.mindex) ||
		(XXqueue.rindex < 0) ||
		  (XXqueue.windex < 0))
	    {
	      PendingExposure = 1;
	    }
	  break;
	case ExposeCopy:	/* For ExposeCopy sync */
	  /* will block all outgoing */
	  /* requests until this is */
	  /* decremented */
	  if (WindowMapped) bitblt = 0;
	  break;
	case KeyPressed:
	  if (Input (((XKeyPressedEvent *) &XXEvent)->detail, bufp))
	    {
	      ++bufp;
	      ++count;
	      --numchars;
	    }
	  break;
	case ButtonPressed:
	  switch (spacecheck (Xxrepbuffer.mindex, 
			      Xxrepbuffer.rindex,
			      Xxrepbuffer.windex, 0))
	    {
	    case 0:
	      loadxrepbuffer (&XXEvent,
			      &Xxrepbuffer);
	      if (informflag)
		{
		  *bufp++ = (char) 003; /* C-c */
		  ++count;
		  --numchars;
		  *bufp++ = (char) '\r';  /* C-m */
		  ++count;
		  --numchars;
		}
	      break;
	    case -1:
	      break;
	    case -2:
	    default:
	      fixxrepbuffer ();
	      break;
	    }
	  break;
	}
    }
/*  while (replies) {
    Reply *reply = replies;
    XPutBackEvent (&reply->event);
    replies = reply->next;
    free (reply);
    }*/
  if (count < 0)
    count = 0;
  if (CursorExists)
    xfixscreen ();
  return count;
}

/* refresh bitmap kitchen sink icon */
refreshicon ()
{
  if (XXIconWindow) 
    XBitmapBitsPut (XXIconWindow, 0,  0, sink_width, sink_height,
		    sink_bits, BlackPixel, WhitePixel, 
		    XXIconMask, GXcopy, AllPlanes);
  XFlush ();
}

XBitmapIcon () 
{
    if (!IconWindow)
      {
	  XSetIconWindow (XXwindow,XXIconWindow);
	  XSelectInput (XXIconWindow, ExposeWindow);
	  IconWindow = !IconWindow;
      }
}

XTextIcon () 
{
    if (IconWindow)
      {
	  XClearIconWindow (XXwindow);
	  XSelectInput (XXIconWindow, NoEvent);
	  IconWindow = !IconWindow;
      }
}

/* Interpreting incoming keycodes. Should have table modifiable as needed
 * from elisp.
 */

/* Exit gracefully from gnuemacs, doing an autosave and giving a status.
 */

XExitGracefully ()
{
  XAutoSave();
  exit(70);
}

xfixscreen ()
{
  register int temp_width, temp_height;
  register int (*func) ();
  register int temp_x, temp_y;
  dumpqueue ();
  func = signal (SIGIO, SIG_IGN);
  /* Check that the connection is in fact open.  This works by doing a nop */
  /* (well, almost) write operation.  If there is an XIOerror or a */
  /* SIGPIPE, exit gracefully.  This fixes the loop-on-logout bug.*/
  XIOErrorHandler (XExitGracefully);
  CursorToggle();
  CursorToggle();
  XFlush ();
  XIOErrorHandler (0);
  if (PendingIconExposure)
    {
      refreshicon ();
      PendingIconExposure = 0;
    }
  if (PendingExposure)
    {
      PendingExposure = 0;
      ClearCursor ();
      XXqueue.rindex = 0;
      XXqueue.windex = 0;
      XQueryWindow (XXwindow, &windowinfo); /* Dangerous to do */
      /* writes here but */
      /* otherwise would */
      /* have to alter */
      /* gnu emacs display */
      /* routines to query */
      /* when screen garbaged */
      temp_width = (windowinfo.width / fontinfo->width);
      temp_height = (windowinfo.height / fontinfo->height);
      if (temp_width != screen_width || temp_height != screen_height)
	change_screen_size (temp_height, temp_width);
      temp_x = windowinfo.x;
      temp_y = windowinfo.y;
      if (temp_x != XXxoffset || temp_y != XXyoffset)
	XSetOffset (temp_x, temp_y);
      dumprectangle (0, 0, screen_height * fontinfo->height,
		     screen_width * fontinfo->width);
    }
  if (!InUpdate)
    if (!CursorExists)
      CursorToggle ();
  (void) signal (SIGIO, func);
kill (XXpid, SIGIO);
}


static
Input (keycode, buffer)
     register int keycode;
     register char *buffer;
{
  register short c;
  register int offset;
  extern KeyMapEntry StdMap[];
  offset = KeyState (keycode);	/* set SHIFT, CONTROL, META */
  c = StdMap [keycode & ValueMask] [offset];
  if ((keycode & ShiftLockMask) && (c >= 'a') && (c <= 'z'))
    {
      c += 'A' - 'a';
    }
  keycode &= ValueMask;	/* no longer need shift bits for anything */
  if (! (c & ~377))
    {
      *buffer = c;
      return 1;
    }
  switch (c)
    {
/*    case '\007':
      kill(XXpid, SIGINT);
      break;*/
    case KEYPAD:	
    case CURSOR:
      switch (keycode & ValueMask) 
	{
	case 0247:	/* left-arrow maps to C-B */
	  c = 002 | ((keycode & MetaMask) ? METABIT : 0);
	  *buffer = c;
	  return(1);
	case 0250:	/* right-arrow maps to C-F */
	  c = 006 | ((keycode & MetaMask) ? METABIT : 0);
	  *buffer = c;
	  return(1);
	case 0252:	/* up-arrow maps to C-P */
	  c = 020 | ((keycode & MetaMask) ? METABIT : 0);
	  *buffer = c;
	  return(1);
	case 0251:	/* down-arrow maps to C-N */
	  c = 016 | ((keycode & MetaMask) ? METABIT : 0);
	  *buffer = c;
	  return(1);
	default:
	  return(0);
	}
    case PFX:
    case (short) -1:
    case SHFT:
    case CNTL:
    case SYMBOL:
    case LOCK:
    case FUNC1:
    case FUNC2:
    case FUNC3:
    case FUNC4:
    case FUNC5:
    case FUNC6:
    case FUNC7:
    case FUNC8:
    case FUNC9:
    case FUNC10:
    case FUNC11:
    case FUNC12:
    case FUNC13:
    case FUNC14:
    case FUNC15:
    case FUNC16:
    case FUNC17:
    case FUNC18:
    case FUNC19:
    case FUNC20:
    case E1:
    case E2:
    case E3:
    case E4:
    case E5:
    case E6:
      return 0;	
    default:
      *buffer = c;
      return 1;
    }
}


x_term_init ()
{
  register char *vardisplay;
  register char *colonpointer;
  register int status;
  extern char *getenv ();
  register int scratchindex;
  extern XTinterrupt_signal ();
  extern char *malloc ();

  vardisplay = getenv ("DISPLAY");
  if (!vardisplay)
    {
      fprintf (stderr, "DISPLAY environment variable must be set\n");
      exit (-200);
    }
  XXdisplay = XOpenDisplay (vardisplay);
  if (XXdisplay == (Display *) 0)
    {
      fprintf (stderr, "No X.\n");
      exit (-99);	
    }
  dup2 (dpyno (), 0);
  close (dpyno ());
  dpyno () = 0;		/* Looks a little strange? */
  /* check the def of the */
  /* macro, it is a genuine */
  /* lvalue */
  Xxrepbuffer.mindex = XREPBUFSIZE - 1;
  Xxrepbuffer.windex = 0;
  Xxrepbuffer.rindex = 0;
  XXqueue.mindex = XREPBUFSIZE - 1;
  XXqueue.windex = 0;
  XXqueue.rindex = 0;
  WindowMapped = 0;
  baud_rate = 9600;
  min_padding_speed = 10000;
  must_write_spaces = 1;
  informflag = 1;
  MetaFlag = 1;
  visible_bell = 1;
  interrupt_input = 1;
  inverse_video = 1;
  bitblt = 0;
  PendingExposure = 0;
  IconWindow = 0;

  fix_screen_hook = xfixscreen;
  clear_screen_hook = XTclear_screen;
  clear_end_of_line_hook = XTclear_end_of_line;
  ins_del_lines_hook = XTins_del_lines;
  change_line_highlight_hook = XTchange_line_highlight;
  insert_chars_hook = XTinsert_chars;
  write_chars_hook = XTwrite_chars;
  delete_chars_hook = XTdelete_chars;
  ring_bell_hook = XTfeep;
  reset_terminal_modes_hook = XTreset_terminal_modes;
  set_terminal_modes_hook = XTset_terminal_modes;
  update_begin_hook = XTupdate_begin;
  update_end_hook = XTupdate_end;
  set_terminal_window_hook = XTset_terminal_window;
  read_socket_hook = XTread_socket;
  topos_hook = XTtopos;
  /* raw_topos_hook = XTraw_topos;  */
  reassert_line_highlight_hook = XTreassert_line_highlight;
  scroll_region_ok = 1;        /* we'll scroll partial screens */
  char_ins_del_ok = 0;         /* just as fast to write the line */
  line_ins_del_ok = 1;         /* we'll just blt 'em */
  fast_clear_end_of_line = 1;  /* X does this well */
  memory_below_screen = 0;	/* we don't remember what scrolls 
				   off the bottom */
  dont_calculate_costs = 1;

  fore = BlackPixel;
  back = WhitePixel;
  brdr = BlackPixel;
  mous = BlackPixel;
  curs = BlackPixel;

  fore_color = "black";
  back_color = "white";
  brdr_color = "black";
  mous_color = "black";
  curs_color = "black";

  XXpid = getpid ();
  XXcurrentfont = malloc (sizeof ("vtsingle") + 1);
  default_window = "=80x24+1+1";
  signal (SIGIO, XTread_socket);
  signal (SIGPIPE, XExitGracefully);
  if (XXcurrentfont == (char *) 0)
    {
      fprintf (stderr, "Memory allocation failure.\n");
      exit (-150);
    }
  strcpy (&XXcurrentfont[0], "vtsingle");
  XQueryWindow (RootWindow, &rootwindowinfo);
  strncpy (iconidentity, ICONTAG, MAXICID);
  XXborder = 1;
  screen_width = 80;
  screen_height = 66;
  XXxoffset = 0;
  XXyoffset = 0;
  XXdebug = 0;
  fontinfo = XOpenFont (&XXcurrentfont[0]);
  if (fontinfo == (FontInfo *) 0)
    {
      fprintf (stderr, "No font\n");
      exit (-98);
    }
  pixelwidth = screen_width * fontinfo->width + 1;
  pixelheight = screen_height * fontinfo->height + 1;
  XXwindow = XCreateWindow (RootWindow,
			    XXxoffset /* Absolute horizontal offset */,
			    XXyoffset /* Absolute Vertical offset */,
			    pixelwidth, pixelheight,
			    XXborder, BlackPixmap, WhitePixmap);
  if (!XXwindow)
    {
      fprintf (stderr, "Unable to create window.\n");
      exit (-97);
    }

  XXIconWindow = XCreateWindow (RootWindow, 0, 0, sink_width, sink_height,
				2, WhitePixmap, (Pixmap) NULL);
  
  if (!XXIconWindow)
    {
      fprintf (stderr, "Unable to create icon window.\n");
      fflush (stderr);
      exit (-97);
    }
  XSelectInput (XXIconWindow, NoEvent);
  XXIconMask = XStoreBitmap(sink_mask_width, sink_mask_height, sink_mask_bits);

  XSelectInput (XXwindow, NoEvent);
  XSetResizeHint (XXwindow, fontinfo->width * 10, fontinfo->height *5, 
		  fontinfo->width, fontinfo->height);

  if (gethostname (&iconidentity[sizeof (ICONTAG) - 1],
		   (MAXICID - 1) - sizeof (ICONTAG)))
    {
      iconidentity[sizeof (ICONTAG) - 2] = '\0';
    }
  XStoreName (XXwindow, &iconidentity[0]);

  EmacsCursor = XCreateCursor (16, 16, MouseCursor, MouseMask,
			       0, 0, BlackPixel, WhitePixel, GXcopy);
  XDefineCursor (XXwindow, EmacsCursor);
  /* Dirty kluge so maybe things will work right */
  XBitmapIcon();
  XTextIcon();
  flexlines = screen_height;
  if (!initialized)
    XPopUpWindow ();
  setxterm ();
}


/* Process all queued ExposeRegion events. */
static
dumpqueue ()
{
  register int i;
  XExposeRegionEvent r;
  if ((XXqueue.rindex > XXqueue.mindex) ||
      (XXqueue.windex > XXqueue.mindex) ||
      (XXqueue.rindex < 0) ||
      (XXqueue.windex < 0))
    {
      PendingExposure = 1;
    }
  else
    while (XXqueue.rindex != XXqueue.windex)
      {
	if (CursorExists)
	  CursorToggle ();
	unloadxrepbuffer (&r, &XXqueue);
	dumprectangle (r.y, r.x, r.height, r.width);
      }
}

		

XSetFlash ()
{
  ring_bell_hook = XTflash;
}

XSetFeep ()
{
  ring_bell_hook = XTfeep;
}


XNewFont (newname)
     register char *newname;
{
  FontInfo *temp;
  int (*func) ();
  func = signal (SIGIO, SIG_IGN);
  XFlush ();
  if (XXdebug)
    fprintf (stderr, "Request id is %d\n", XXdisplay->request);
  temp = XOpenFont (newname);
  if (temp == (FontInfo *) 0)
    {
      (void) signal (SIGIO, func);
      if (QLength () > 0)
	{
	    kill (XXpid, SIGIO);
	}
      return -1;
    }
  XCloseFont (fontinfo);
  fontinfo = temp;
  (void) signal (SIGIO, func);
  XSetResizeHint (XXwindow, fontinfo->width * 10, fontinfo->height *5, 
		  fontinfo->width, fontinfo->height);
  XSetWindowSize (screen_height, screen_width);
  if (QLength () > 0)
    {
	kill (XXpid, SIGIO);
    }
  return 0;
}

XFlipColor ()
{
  Pixmap temp;
  int tempcolor;
  char *tempname;
  int (*func) ();
  Cursor temp_curs;
  CursorToggle ();
  func = signal (SIGIO, SIG_IGN);
  temp = XMakeTile(fore);
  XChangeBackground (XXwindow, temp);
  XFreePixmap (temp);
  temp = XMakeTile (back);
  if (XXborder)
    XChangeBorder (XXwindow, temp);
  XFreePixmap(temp);
  brdr = back;
  brdr_color = back_color;
  tempcolor = fore;
  fore = back;
  back = tempcolor;
  tempname = fore_color ;
  fore_color = back_color;
  back_color = tempname;
/*  XPixFill (XXwindow, 0, 0, screen_width * fontinfo->width,
	    screen_height * fontinfo->height, back, ClipModeClipped,
	    GXcopy, AllPlanes);
  dumprectangle(0, 0, screen_height * fontinfo->height,
  screen_width * fontinfo -> width);*/
  XRedrawDisplay();
  if (curs == WhitePixel)
    {
	curs = BlackPixel;
	curs_color = "black";
    }
  else if (curs == BlackPixel)
    {
	curs = WhitePixel;
	curs_color = "white";
    }
  if (mous == WhitePixel)
    {
	mous = BlackPixel;
	mous_color = "black";
    }
  else if (mous == BlackPixel)
    {
	mous = WhitePixel;
	mous_color = "white";
    }
  temp_curs = XCreateCursor(16, 16, MouseCursor, MouseMask, 0, 0,
			    mous, back, GXcopy);
  XUndefineCursor (XXwindow);
  XDefineCursor (XXwindow, temp_curs);
  XFreeCursor (EmacsCursor);
  (void) signal (SIGIO, func);
  bcopy (&temp_curs, &EmacsCursor, sizeof (Cursor));
  CursorToggle ();
  XFlush ();
}

XSetOffset (xoff, yoff)
     register int xoff, yoff;
{
  if (xoff < 0)
    {
      XXxoffset = rootwindowinfo.width + (++xoff) - pixelwidth - 4;
    }
  else
    {
      XXxoffset = xoff;
    }
  if (yoff < 0)
    {
      XXyoffset
	= rootwindowinfo.height + (++yoff) - pixelheight - 4;
    }
  else
    {
      XXyoffset = yoff;
    }
  XMoveWindow (XXwindow, XXxoffset, XXyoffset);
  /* XWarpMouse (XXwindow, pixelwidth >> 1, pixelheight >> 1); */
}

XSetWindowSize (rows, cols)
     register int rows, cols;
{
  if (rows < 5) rows = 66;
  if (cols < 5) cols = 80;
  pixelwidth = cols * fontinfo->width + 1;
  pixelheight = rows * fontinfo->height + 1;
  XChangeWindow (XXwindow, pixelwidth, pixelheight);
  XFlush ();
  change_screen_size (rows, cols);
  PendingExposure = 0;
}

XPopUpWindow ()
{
    if (WindowMapped)
      return;
    if(!x_edges_specified)
      Fx_rubber_band ();
    bitblt = 0;
    CursorExists = 0;
    VisibleX = 0;
    VisibleY = 0;
    WindowMapped = 1;
    XMapWindow (XXwindow);
    dumprectangle (0, 0, screen_height * fontinfo->height,
		   screen_width * fontinfo->width);
    XSelectInput (XXwindow, KeyPressed | ExposeWindow |
		  ButtonPressed | ExposeRegion | ExposeCopy);
    /*	XWarpMouse(XXwindow, pixelwidth >> 1, pixelheight >> 1);*/
    XTtopos (0, 0);
/*  XRedrawDisplay();*/
    XFlush ();
}

spacecheck (mindex, rindex, windex, minfreespace)
     register int mindex, rindex, windex, minfreespace;
{
  if ((rindex > mindex) || (windex > mindex))
    {
      fprintf (stderr, "Fatal Mouse Buffer Error.\n");
      fprintf (stderr, "%d = mindex, %d = rindex, %d = windex\n",
	       mindex, rindex, windex);
      return -2;
    }
  if (windex >= rindex)
    {
      if ((mindex - (windex - rindex)) > minfreespace)
	return 0;
    }
  else
    {
      if (((rindex - windex) - 1) > minfreespace)
	return 0;
    }
  return -1;
}

loadxrepbuffer (p_xrep, p_buffer)
     register XEvent *p_xrep;
     register XREPBUFFER *p_buffer;
{
  p_buffer->xrep[p_buffer->windex] = *p_xrep;
  if (p_buffer->windex == p_buffer->mindex)
    p_buffer->windex = 0;
  else
    p_buffer->windex++;
}

unloadxrepbuffer (p_xrep, p_buffer)
     register XEvent *p_xrep;
     register XREPBUFFER *p_buffer;
{
  if (p_buffer->windex == p_buffer->rindex)
    return -1;
  *p_xrep = p_buffer->xrep[p_buffer->rindex];
  if (p_buffer->rindex == p_buffer->mindex)
    p_buffer->rindex = 0;
  else
    p_buffer->rindex++;
  return 0;
}

fixxrepbuffer ()
{
  Xxrepbuffer.mindex = XREPBUFSIZE - 1;
  Xxrepbuffer.windex = 0;
  Xxrepbuffer.rindex = 0;
}
