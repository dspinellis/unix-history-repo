#include <X/mit-copyright.h>

/* Copyright 1984, 1985 Massachusetts Institute of Technology */
/* Tplot.c */

#ifndef lint
static char *rcsid_Tplot_c = "$Header: Tplot.c,v 10.8 86/02/01 16:05:23 tony Rel $";
#endif	lint

#include <X/Xlib.h>
#include "ptyx.h"
#undef CTRL
#include <stdio.h>
#include <sys/file.h>
#include <sgtty.h>
#include <pwd.h>
#undef CTRL

extern debug;

static int bplot[5];
static int nplot;
static int ipchr;
static int refresh;
static int LoYSet;

#define MAX_PTS 150
#define MAX_VTX 300
static Vertex Tline[MAX_VTX];
static Vertex *line_pt;

static FILE *rfile, *wfile;

static TekLink *TekBuf = NULL;
TekLink *tb_end_link = NULL;	  /* world accessible for TekBufPut */
int tb_end = TEK_LINK_BLOCK_SIZE; /* world accessible for TekBufPut */
static TekLink *tb_pt_link = NULL;
static int tb_pt = TEK_LINK_BLOCK_SIZE;

#include "../cursors/tcross.cursor"
#include "../cursors/tcross_mask.cursor"

TekErase (term)

/* Reset buffer pointer for display list to start of buffer */

Terminal *term;
{
	register Screen *screen = &term->screen;
	register struct _TekLink *pnt;
	register struct _TekLink *next;

	pnt = (struct _TekLink *) TekBuf;

	/* free stored list */
	while (pnt)
	{
		next = pnt->next;
		free (pnt);
		pnt = next;
	}
		
	/* reset pointers */
	TekBuf = tb_end_link = tb_pt_link = NULL;
	tb_end = tb_pt = TEK_LINK_BLOCK_SIZE;

	refresh = 0;
	screen->cur_x = 0;
	screen->cur_y = 0;
	screen->TekGMode = 0;
	screen->TekAMode = 0;
	LoYSet = 0;

	if (debug) wfile = fopen ("testbufw","w"); 

	return;
}


TekRefresh (term)

/* Redisplay graphics screen, including positioned alpha strings */

register Terminal *term;
{
	register Screen *screen = &term->screen;
	Window window = screen->window;
	Font fnt = screen->fnt_norm;
	int chr, nstr;
	register int ch;
	unsigned char str[257];
	int modeG = screen->TekGMode, modeA = screen->TekAMode,
	    modeI = screen->TekIMode, modeP = screen->TekPMode;

	TekBufResetPt ();

	if (TekBufEmpty ()) return;

	if (debug) rfile = fopen ("testbufr","w"); 

	refresh = 1;
	TekAlph (term,0);
	screen->cur_x = screen->cur_y = 0;

	while (TekBufGet (&chr) != -1)
	{
		ch = chr;	/* lets get the char into a register */
		if (screen->TekGMode)
		{
			if (ch > 31)
			{
				if (screen->TekIMode) TekPoint (term,ch);
				else TekPlot (term,ch);
			}
			else if (ch > 27 && ch < 31) TekInit (term,ch);
			else if (ch == 31 || ch < 28) TekAlph (term,0);
			continue;
		}

		if (ch > 27 && ch < 31)
		{
			TekInit (term, ch);
			continue;
		}

		if (ch > (unsigned) 128)
		{
			nstr = ch - (unsigned) 128;

			if (debug)
			{
				fprintf (rfile,"alpha mode %d chars\n",nstr);
				fflush (rfile);
			}

			if (nstr > 256) exit (77);
			TekBufGetString (str, nstr);
			*(str + nstr) = NULL;

			XTextMask ( window,
				TCursorX(screen), TCursorY(screen),
				str, nstr, fnt, screen->foreground);

			screen->cur_X += (nstr * screen->f_width);
			screen->cur_x +=
				(nstr * screen->f_width / screen->TekScale);
		}
	    }

	if (nplot > 0) TekFlush (term);
	refresh = 0;

	screen->TekGMode = modeG;
	screen->TekAMode = modeA;
	screen->TekIMode = modeI;
	screen->TekPMode = modeP;
}


TekString (term, str, nchr)

/* Display positioned alphanumeric string, saving it in display list */

register Terminal *term;
unsigned char *str;
int nchr;
{
	register Screen *screen = &term->screen;
	Window window = screen->window;
	Font fnt = screen->fnt_norm;

	TekBufPut (128 + nchr);
	if (debug)
	{
		fprintf (wfile,"alpha mode %d character string\n",nchr);
		fflush (wfile);
	}

	TekBufPutString (str, nchr);

	XTextMask (window,
		TCursorX(screen), TCursorY(screen),
		str, nchr, fnt, screen->foreground);

	screen->cur_X = screen->cur_X + (nchr * screen->f_width);
	screen->cur_x = screen->cur_x +
			(nchr * screen->f_width / screen->TekScale);
}

TekBufResetPt ()
/* set pt to point to beginning of buffer */
{
	tb_pt_link = TekBuf;
	tb_pt = 0;
}

TekBufEmpty ()
/* return nonzero iff TekBuf is empty */
{
	return (((tb_end & ~TEK_LINK_BLOCK_SIZE) == 0)
		&& (tb_end_link == TekBuf));
}


/* Extend display buffer */
TekBufExtend ()
{

	/* if TekBuf is NULL then must initialize */
	if (!TekBuf)
	{
		TekBuf = (TekLink *) malloc (sizeof (TekLink));
		TekBuf->next = NULL;
		tb_end_link = TekBuf;
		tb_end = 0;
	}
	else
	{
		tb_end_link->next =
		(struct _TekLink *) malloc (sizeof (TekLink));
		tb_end_link = (TekLink *) tb_end_link->next;
		tb_end_link->next = NULL;
		tb_end = 0;
	}
}

/*
   effect: puts len bytes at str into display buffer
 */
TekBufPutString (str, len)
register unsigned char *str;
register int len;
{
	register int amount;

	while (len)
	{
		if (tb_end & TEK_LINK_BLOCK_SIZE) TekBufExtend ();

		amount = TEK_LINK_BLOCK_SIZE - tb_end;
		if (amount > len) amount = len;

		bcopy (str, tb_end_link->data + tb_end, amount);
		str += amount;
		len -= amount;
		tb_end += amount;
	}
}


/* Read character from display list */

int TekBufGet (chr)
register unsigned int *chr;
{
	if (tb_pt & TEK_LINK_BLOCK_SIZE)

	/* if tb_pt_link is NULL then check for a TekBuf */
	    if (!tb_pt_link)
	    {
		if (!TekBuf) return (-1);
		tb_pt_link = TekBuf;
		tb_pt = 0;
	    }
	    else
		if (tb_pt_link->next == NULL) return (-1);
		else
		{
			tb_pt_link = (TekLink *) tb_pt_link->next;
			tb_pt = 0;
		}
		
	if ((tb_pt_link ==  tb_end_link) && (tb_pt >= tb_end)) return (-1);

	*chr = tb_pt_link->data [tb_pt++];
	return (0);
}

/* store next len characters of TekBuf in str, advancing tb_pt */
TekBufGetString (str, len)
register char *str;
register int len;
{
	register int n;

	while (len)
	{
		n = TEK_LINK_BLOCK_SIZE - tb_pt;
		n = (n < len) ? n : len;

		bcopy (tb_pt_link->data + tb_pt, str, n);
		tb_pt += n;
		len -= n;
		str += n;

		if (tb_pt == TEK_LINK_BLOCK_SIZE)
		{
			if (tb_pt_link->next == NULL) return;
			tb_pt_link = (TekLink *) tb_pt_link->next;
			tb_pt = 0;
		}
	}
}

/* Switch to graphics mode and initialize byte and point counters */

TekInit (term,chr)
Terminal *term;
register int chr;
{
	register Screen *screen = &term->screen;

	if (debug)
	    if (!refresh)
		{
		fprintf (wfile,"switching to graphics mode %d\n",chr);
		fflush (wfile);
		}
	    else
		{
		fprintf (rfile,"switching to graphics mode %d\n",chr);
		fflush (rfile);
		}

	if (!screen->TekGMode && nplot > 0) TekFlush (term);

	screen->pen = 0;
	ipchr = 0;
	screen->TekPMode = 0;
	screen->TekIMode = 0;
	if (chr == 28) screen->TekPMode = 1;
	if (chr == 30) screen->TekIMode = 1;

	if (!refresh) TekBufPut (chr);
	if (screen->TekGMode) return;

	nplot = 0;
	screen->TekGMode = 1;
	screen->TekAMode = 1;
/*
 *	screen->cur_row = 0;
 *	screen->cur_col = 0;
 */
	line_pt = Tline;
}

TekAlph (term,chr)

/* Switch back to alphanumeric mode from graphics mode */

Terminal *term;
int chr;
{
	register Screen *screen = &term->screen;

	if (!screen->TekGMode) return;

	if (debug)
	    if (!refresh)
		{
		fprintf (wfile,"switching to alpha mode %d\n",chr);
		fflush (wfile);
		}
	    else
		{
		fprintf (rfile,"switching to alpha mode %d\n",chr);
		fflush (rfile);
		}

	if (!refresh) TekBufPut (chr);
	if (nplot > 0) TekFlush (term);

	screen->TekGMode = 0;
	screen->TekPMode = 0;
	screen->TekIMode = 0;
	if (chr>0 && chr != 31) TekReset(term);
	return;
}


TekReset (term)

/* Switch back to vt100 alphanumeric mode from Tektronix alphanumeric mode
 *  This allows alphanumeric strings to be positioned by graphics commands,
 *  but reenables scrolling after CR or LF.  */

Terminal *term;
{
	register Screen *screen = &term->screen;
	int fh = screen->f_height;
	int fw = screen->f_width;

	screen->TekAMode = 0;

	if (debug)
	    if (!refresh)
		{
		fprintf (wfile,"resetting to alpha mode\n");
		fflush (wfile);
		}
	    else
		{
		fprintf (rfile,"resetting to alpha mode\n");
		fflush (rfile);
		}

	if (screen->cur_Y > 0)
	   {
	   screen->cur_row = (screen->cur_Y - (fh >> 1)) / fh;
	   if (screen->cur_row > screen->max_row)
	   	screen->cur_row = screen->max_row;
	   screen->cur_Y = 0;
	   }
	if (screen->cur_X > 0)
	   {
	   screen->cur_col = (screen->cur_X + (fw >> 1)) / fw;
	   if (screen->cur_col > screen->max_col)
	   	screen->cur_col = screen->max_col;
	   screen->cur_X = 0;
	   }
	screen->cur_x = 0;
	screen->cur_y = 0;

	return;
}


TekCursor (term)

/* Reads position of mouse cursor when a character is typed and returns
 *  it in Tektronix-format coordinates */

Terminal *term;
{
	register Screen *screen = &term->screen;
	Keyboard *keyboard = &term->keyboard;
	register Window window = screen->window;
	int MouseX, MouseY;
	XEvent reply;
	XEvent *rep = &reply;
	Window subw;
	int pty = screen->respond;
	int keycode, event, shifts;
	short c;
	char cplot [5];
	extern int wflush ();
	int x, y;
	Cursor cursor;
	char *string = " ";
	int nbytes;

	TekAlph (term, 0);
	TekReset (term);

	/* Set cross-hair cursor raster array */
	cursor = XCreateCursor(tcross_width, tcross_height, 
		tcross_bits, tcross_bits, 7, 7,
		screen->foreground, screen->background, GXxor);

	/* Display cross-hair cursor */
	XDefineCursor(window, cursor);
	
	/* reselect input.  Must be careful not to leave stray button
	 * events upon return since handle buttons would be invoked */

	XSelectInput (window, ButtonReleased | KeyPressed);

	/* Wait for keyboard entry */
	XWindowEvent (window, KeyPressed | ButtonReleased, &reply);

	keycode = ((XKeyOrButtonEvent *)rep)->detail;
	event = reply.type;

	/* Get current mouse position and translate to screen coordinates */
	XUpdateMouse (window, &MouseX, &MouseY, &subw);

	x = ((MouseX - screen->border) << 1);
	y = 782 - ((MouseY - screen->border) << 1);
	screen->cur_X = MouseX;
	screen->cur_Y = MouseY;

	/* Translate x and y to Tektronix code */
	cplot[1] = 32 + (x >> 5);
	cplot[2] = 32 + (x & 31);
	cplot[3] = 32 + (y >> 5);
	cplot[4] = 32 + (y & 31);

	/* Translate keyboard entry and return one byte from terminal */
	if (event == KeyPressed)
	{
		string = XLookupMapping ((XKeyPressedEvent *)& reply, &nbytes);
		c = *string;
	}
	else c = keycode + 48;

	cplot[0] = c;

	/* Return encode screen coordinates */
	write (pty, cplot, 5 * sizeof (char));

	/* Reset cursor and return */

	XFreeCursor(cursor);
	XDefineCursor(window,
		(term->flags & INVERSE) ? screen->rcurs : screen->curs);


	TekInit (term,0);

	XSelectInput (window, KeyPressed | ExposeWindow | ExposeRegion |
		      ExposeCopy | ButtonPressed | ButtonReleased);
}


TekPlot (term,chr)

/* Translates Tektronix byte-encoded screen coordinates to integer screen
 * coordinates each time the final byte of a coordinate set is encountered.  */

	Terminal *term;
	register int chr;
{
	register Screen *screen = &term->screen;
	register int *bp = bplot;
	register int x, y;

	ipchr++;
	if (ipchr > 5)
	{
		TekAlph (term,chr);
		return;
	}

	/* decode Tektronix byte position codes */

	if (chr < 64)
	{
		if (LoYSet)			/* 4st byte */
		{
			bp[3] = chr & ~32;
		}
		else				/* 1st byte */
		{
			bp[0] = chr & ~32;
			bp[1] = 0;
		}
	}
	else if (chr > 95)
	{
		if (LoYSet)			/* 2nd byte */
		{				/* 3rd if 2nd present */
			bp[1] = bp[2];
			bp[2] = chr & ~96;
		}
		else 				/* 3rd byte */
		{
			bp[2] = chr & ~96;
			bp[1] = 0;
			LoYSet = 1;
		}
	}
	else					/* 5th byte */
	{
		bp[4] = chr & ~64;
		ipchr = 0;
		LoYSet = 0;

		/* compute screen x and y coordinates ignoring extended
		 * precision byte */

		y = (bp[0] << 7) + (bp[2] << 2);
		x = (bp[3] << 7) + (bp[4] << 2);

		/* add in 4014 extended precision byte */
		if (bp[1] > 0)
		{
			x += (bp [1] & 3);
			y += ((bp [1] & 12) >> 2);
		}

		/* Check to make sure screen limits are not exceeded */
		if (y > 3127)  y = 3127;

		/* transfer this point to vertex array */
		if (screen->TekPMode)
		{
			TekDraw (0, x, y, term);
			TekDraw (1, x, y, term);
		}
		else
		{
			TekDraw (screen->pen, x, y, term);
			screen->pen = 1;
		}
	}
}


TekPoint (term,chr)

/* Translates Tektronix byte-encoded incremental plot commands to screen
 * coordinates and writes them to the vector buffer */

register Terminal *term;
register int chr;
{
	Screen *screen = &term->screen;
	register int pen,x,y;

	x = screen->cur_x;
	y = screen->cur_y;
	pen = screen->pen;

	if (nplot == 0) TekDraw (0,x,y,term);

	switch (chr)
	    {
	    case ' ':	screen->pen = 0;
			break;

	    case 'P':	screen->pen = 1;
			break;

	    case 'D':	TekDraw (pen, x, ++y, term);
			break;

	    case 'E':	TekDraw (pen, ++x, ++y, term);
			break;

	    case 'A':	TekDraw (pen, ++x, y, term);
			break;

	    case 'I':	TekDraw (pen, ++x, --y, term);
			break;

	    case 'H':	TekDraw (pen, x, --y, term);
			break;

	    case 'J':	TekDraw (pen, --x, --y, term);
			break;

	    case 'B':	TekDraw (pen, --x, y, term);
			break;

	    case 'F':	TekDraw (pen, --x, ++y, term);
			break;

	    }
}


TekDraw (pen, x, y, term)

/* Translates Tektronix screen coordinates to vs100 screen
 *  coordinates, drawing them to the screen when a buffer is filled */

int x,y,pen;
Terminal *term;
{
	register Screen *screen = &term->screen;
	register Vertex *lp = line_pt;

	screen->cur_x = x;
	screen->cur_y = y;

	/* convert to vs100 window coordinates */
	screen->cur_X = (x + 1) * screen->TekScale + screen->border;
	screen->cur_Y = (3128 - y) * screen->TekScale + screen->border;

	/* write to plot buffer */
	lp->x = screen->cur_X;
	lp->y = screen->cur_Y;

	if (pen) lp->flags = VertexDrawLastPoint;
	else lp->flags = VertexDontDraw;

	lp = ++line_pt;
	nplot++;

	if (debug)
	    if (refresh) fprintf (rfile,"%d: %d %d,%d -> %d,%d\n",
		nplot,pen,x,y,screen->cur_X,screen->cur_Y);
	    else fprintf (wfile,"%d: %d %d,%d -> %d,%d\n",
		nplot,pen,x,y,screen->cur_X,screen->cur_Y);
	if (debug)
	    if (refresh) fflush (rfile);
	    else fflush (wfile);

	/* draw line if buffer limit has been reached */
	if (nplot >= MAX_PTS)
	{
		TekFlush (term);
		lp = line_pt;
		lp->x = screen->cur_X;
		lp->y = screen->cur_Y;
		lp->flags = VertexDontDraw;
		line_pt++;
		nplot++;
	}
}


TekFlush (term)

register Terminal *term;
{
	Window window = term->screen.window;

	XDraw (window, Tline, nplot, 1, 1, term->screen.foreground, GXcopy, 
	       AllPlanes);
	nplot = 0;

	line_pt = Tline;
}

TekInq (term)
/* Reports position of cursor in Tektronix-format coordinates */
Terminal *term;
{
	register Screen *screen = &term->screen;
	int pty = screen->respond;
	char cplot [5];
	int x, y;
	x = ((screen->cur_X - screen->border) << 1);
	y = 782 - ((screen->cur_Y - screen->border) << 1);
	/* Translate x and y to Tektronix code */
	cplot[1] = 32 + (x >> 5);
	cplot[2] = 32 + (x & 31);
	cplot[3] = 32 + (y >> 5);
	cplot[4] = 32 + (y & 31);
	cplot[0] = '%';	/* standard hardware config */
	/* Return encoded screen coordinates */
	write (pty, cplot, 5 * sizeof (char));
}

#ifdef ENABLE_PRINT

#include <signal.h>

TekPrint ()

/* Dump graphics screen, including positioned alpha strings */

{
	int chr, pid, uid, i;
	char c;
	FILE *tekfile;
	char *uname, tekfile_name[40];
	struct passwd *upasswd;
	int temp_tb_pt = tb_pt;
	TekLink *temp_tb_pt_link = tb_pt_link;

	TekBufResetPt ();
	if (TekBufEmpty ()) return;

	pid = getpid ();
	uid = getuid ();
	upasswd = getpwuid (uid);
	sprintf (tekfile_name,"/tmp/vst%d\.%s\0",pid,upasswd->pw_name);

	tekfile = fopen (tekfile_name,"w"); 

	while (TekBufGet (&chr) != -1)
	{
		c = chr;
		if (chr < 128) putc (c,tekfile);
	}

	tb_pt = temp_tb_pt;
	tb_pt_link = temp_tb_pt_link;

	fclose (tekfile);

	if (debug) printf ("attempting fork\n");

	switch (fork ())
	{
		case -1:	Error ();
				break;

		case  0: 	for (i= 0; i < _NFILE; i++) close (i);
				signal(SIGCHLD, SIG_DFL);
				signal(SIGTERM, SIG_DFL);
				signal(SIGHUP,  SIG_IGN);
				execlp("imtek", "imtek", "-x",tekfile_name, 0);
				exit (1);
				break;

		default:
				break;
	}
}

#endif
