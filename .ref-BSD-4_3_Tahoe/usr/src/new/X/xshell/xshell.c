/*
 * Copyright 1985, Cognition Inc.
 * This software is not supported by Cognition Inc. and is
 * provided "as is".  To keep the X revolution growing, please forward 
 * enhancements and bug fixes to anyone who is interested.
 */

/*
 * XShell - a quick "shell" to allow you to cons up programs on the fly.  The
 * program looks at Button and Key Pressed events and looks up Xshell.name in
 * $HOME/.Xdefaults where name is:
 *
 *	LeftButton, MiddleButton, RightButton - on button events.
 *	ascii (e.g. [aB^9]) - on key pressed events.
 *	PF# and various special names - on special keys.
 *
 * The idea is that the user can define any set of key/button bindings which
 * can be invoked by simply pressing in the XShell window.  This can be very
 * useful for times when you have filled all of your windows with things that
 * you don't want to (or can't) suspend.
 *
 * I find it useful to have a large and small terminal window, a dc, and 
 * sometimes an editor that I can pop up on demand.  This program was written
 * because I was tired of getting into situations where I didn't have a window
 * handy and I needed to run some little calculator or editor.  Since I use
 * a small, terse window manager I didn't just stick a bag on the side of it,
 * but wrote a separate program instead.
 *
 * Apologies to anyone who has the scallop shell as a trademark.
 *
 * Author:  Jim Fulton, Cognition Inc.
 */
#ifndef lint
static char *rcsid_xshell_c = "$Header: xshell.c,v 10.7 86/11/19 19:55:45 jg Rel $";
#endif

#include <stdio.h>
#include <X/Xlib.h>
#include <X/Xkeyboard.h>
#include <signal.h>
#include <ctype.h>
#include <strings.h>
#include <sys/file.h>
#include <sys/ioctl.h>
#include <sys/wait.h>
#include <sys/time.h>
#include <sys/resource.h>
#include <sys/types.h>
#include "scallopshell.h"
#include "largescallopshell.h"

extern KeyMapEntry StdMap[];

extern char *malloc();

#define strneq(a,b) (strncmp ((a), (b), strlen(a)) == 0)
#define streq(a,b) (strcmp ((a), (b)) == 0)
#define isopt(optname) (strneq(*argv, optname))	/* does partial matching */
#define hasarg ((argv[1] != (char *) NULL) && (argv[1][0] != '-'))
#define NO 0
#define YES 1
#define DEFAULT_FONT "helv12b"
#define micro2centi (10000)		/* microsecond to 1/100 second */

char *ProgramName;

Window ShellWindow;
Display *dpy;
int fg = BlackPixel;	/* print bits in black */
int bg = WhitePixel;	/* print background in white */
int bd = BlackPixel;	/* print border in black */
int bw = 0;		/* no border */
int volume = 1;
WindowInfo RootWindowInfo;
Bitmap IconBitmap;
Pixmap IconPixmap;
XEvent inputevent;
int width, height;
unsigned long code;
int nflash = 3;
struct timeval delaytime = {0, 5*micro2centi}; 
int quiet = NO;
short *icon_bits;
int icon_width, icon_height;

char **actionvector[256];		/* command table */
char actionfound[256];		/* see flags below */
#define ACTION_NEW 		((char) 0)
#define ACTION_FOUND 		((char) 1)
#define ACTION_NOT_FOUND 	((char) 2)

char *yes[] = {"y", "yes", "YES", "on", "ON", "On", 
		      "t", "true", "TRUE", "True", (char *) NULL};

char *small[] = {"s", "S", "small", "SMALL", "Small", "sm", "SM", "Sm",
			(char *) NULL};

int IsMemberOf (list, string)
	register char *list[];
	register char *string;
{
	for (; *list; list++) {
	    if (streq (string, *list)) return (YES);
	}
	return (NO);
}

reapchild ()
{
	union wait status;

	while (wait3 (&status, WNOHANG, (struct rusage *) 0) > 0) ;
	return;
}

static void Error (msg, arg)
	char *msg, *arg;
{
	fprintf (stderr, "%s:  error with %s", msg);
	if (arg != (char *) NULL && *arg != '\0')
	    fprintf (stderr, ": %s\n", arg);
	else
	    fprintf (stderr, "\n");
	exit (1);
}

static void Usage (msg)
	char *msg;
{
	fprintf (stderr, "%s:  error with \"%s\".  Usage is\n", 
		ProgramName, msg);
	fprintf (stderr, "\n\t\t%s [-flags] [=WxH+X+Y] [host:displaynum]\n\n", 
		ProgramName);
	fprintf (stderr, "where -flags are:\n");
	fprintf (stderr, "\t-fg color       Foreground color.\n");
	fprintf (stderr, "\t-bg color       Background color.\n");
	fprintf (stderr, "\t-bd color       Border color.\n");
	fprintf (stderr, "\t-bw[idth]n      Border width in pixels.\n");
	fprintf (stderr, "\t-v[olume] n     Bell volume.\n");
	fprintf (stderr, "\t-fl[ash] n      Number of times to flash icon.\n");
	fprintf (stderr, "\t-d[elay] n      1/10ths of a second flashes.\n");
	fprintf (stderr, "\t-r[everse]      Reverse video.\n");
	fprintf (stderr, "\t-q[uiet]        Don\'t feep on errors.\n");
	fprintf (stderr, "\t-s[mall]        Use a small icon instead of a big one.\n");
	fprintf (stderr, "\n");
	exit (1);
}

quit ()
{
	XUnmapWindow (ShellWindow);
	XCloseDisplay (dpy);
	exit (0);
}

main (argc, argv)
	int argc;
	char *argv[];
{
	register int i;
	register char *cp;
	char *fgname, *bgname, *bdname;
	int reverse;
	char *displayname;
	Color colorstruct;
	int c;
	char cbuf[2];
	int xoff, yoff, xsign, ysign;

	(void) signal (SIGINT, quit);
	(void) signal (SIGHUP, quit);
	(void) signal (SIGCHLD, reapchild);

	setlinebuf (stderr);

			/* set program name for errors */
	ProgramName = argv[0];
	cp = rindex (ProgramName, '/');
	if (cp) ProgramName = ++cp;	/* strip off directory name */

			/* Initialize variables */
	fgname = bgname = bdname = (char *) NULL;
	reverse = NO;
	displayname = (char *) NULL;
	bzero (actionfound, sizeof (actionfound));
	icon_bits = largescallopshell_bits;
	width = -1;
	height = -1;
	icon_width = largescallopshell_width; 
	icon_height = largescallopshell_height;

			/* read in defaults from .Xdefaults */

	fgname = XGetDefault (ProgramName, "Foreground");

	bgname = XGetDefault (ProgramName, "Background");

	bdname = XGetDefault (ProgramName, "Border");

	cp = XGetDefault (ProgramName, "BorderWidth");
	if (cp) bw = atoi (cp);

	cp = XGetDefault (ProgramName, "Volume");
	if (cp) volume = atoi (cp);

	cp = XGetDefault (ProgramName, "Flash");
	if (cp) nflash = atoi (cp);

	cp = XGetDefault (ProgramName, "Delay");
	if (cp) delaytime.tv_usec = atoi (cp) * micro2centi;

	if ((cp = XGetDefault (ProgramName, "ReverseVideo")) != NULL)
		if (IsMemberOf (yes, cp)) reverse = YES;

	if ((cp = XGetDefault (ProgramName, "Quiet")) != NULL)
		if (IsMemberOf (yes, cp)) quiet = YES;

	if ((cp = XGetDefault (ProgramName, "IconSize")) != NULL) {
		if (IsMemberOf(small, cp)) {
		    icon_bits = scallopshell_bits;
		    icon_width = scallopshell_width; 
		    icon_height = scallopshell_height;
		}
	}

	cp = XGetDefault (ProgramName, "WindowGeometry");
	if (cp) {
	    if (!XParse_Window_Geometry (cp, &width, &height, 
		&xsign, &xoff, &ysign, &yoff)) Usage ("default =WxH+XOFF+YOFF");
	}

					/* read command arguments */
	argv++;				/* advance past command name */
	for (; *argv; argv++) {		/* iterate over arguments */
	  if (**argv == '-') {
	    if (isopt ("-fg")) {
		if (hasarg) {
		    fgname = *++argv;
		} else Usage ("-fg foreground");
	    } else
	    if (isopt ("-bg")) {
		if (hasarg) {
		    bgname = *++argv;
		} else Usage ("-bg background");
	    } else
	    if (isopt ("-bd")) {
		if (hasarg) {
		    bdname = *++argv;
		} else Usage ("-bd border color");
	    } else
	    if (isopt ("-bwidth")) {
		if (hasarg) {
		    bw = atoi (*++argv);
		} else Usage ("-bwidth borderwidth");
	    } else
	    if (isopt ("-volume")) {
		if (hasarg) {
		    volume = atoi (*++argv);
		} else Usage ("-volume volume");
	    } else
	    if (isopt ("-flash")) {
		if (hasarg) {
		    nflash = atoi (*++argv);
		} else Usage ("-flash n");
	    } else
	    if (isopt ("-delay")) {
		if (hasarg) {
		    delaytime.tv_usec = atoi (*++argv) * micro2centi;
		} else Usage ("-delay n");
	    } else
	    if (isopt ("-reverse")) {
		reverse = YES;
	    } else 
	    if (isopt ("-quiet")) {
		quiet = YES;
	    } else 
	    if (isopt ("-small")) {
		icon_bits = scallopshell_bits;
		icon_width = scallopshell_width;
		icon_height = scallopshell_height;
	    } else 
	    Usage ("-unknown flag");	/* end if command line options */
	  } else if (**argv == '=') {
	    if (!XParse_Window_Geometry (*argv, &width, &height, 
		&xsign, &xoff, &ysign, &yoff)) Usage ("=WxH+XOFF+YOFF");
	  } else 
	    displayname = *argv;
	} /*end for*/


	if (width == -1) width = icon_width;
	if (height == -1) height = icon_height;

				/* okay, now set things up */
	dpy = XOpenDisplay (displayname);
	if (!dpy) {
	        fprintf(stderr, "%s: Can't open display '%s'\n",
		    ProgramName, XDisplayName(displayname));
		exit(1);
	      }

	if (!XQueryWindow (RootWindow, &RootWindowInfo)) 
	    Error ("query root", "");

	if (xsign < 0) xoff = RootWindowInfo.width - xoff - width - 2*bw;
	if (ysign < 0) yoff = RootWindowInfo.height - yoff - height - 2*bw;

				/* set the colors for the various parts */

#define setcolor(colorname,colornum) 					\
	if (colorname && DisplayCells() > 2 &&				\
		XParseColor(colorname, &colorstruct) && 		\
		XGetHardwareColor(&colorstruct)) {			\
	    colornum = colorstruct.pixel;				\
	    reverse = NO;						\
	}

	setcolor (fgname, fg);
	setcolor (bgname, bg);
	setcolor (bdname, bd);

#undef setcolor

	if (reverse) {
	    i = fg;
	    fg = bg;
	    bg = i;
	}
			/* now, make up the icon pixmap */

	IconBitmap = XStoreBitmap (icon_width, icon_height, icon_bits);
	if (!IconBitmap) Error ("storing icon bitmap", "");

	IconPixmap = XMakePixmap (IconBitmap, fg, bg);
	if (!IconPixmap) Error ("storing icon pixmap", "");

	
			/* make the window */

	ShellWindow = XCreateWindow (RootWindow, xoff, yoff, width, height,
			bw, XMakeTile(bd), XMakeTile(bg));
	if (!ShellWindow) Error ("creating shell window", "");

			/* and store away the program name in the window */

	XStoreName (ShellWindow, ProgramName);

			/* select the window events */

	XSelectInput (ShellWindow, KeyPressed | ButtonPressed | ExposeWindow);
	
			/* and map it, this should generate an Expose event */
	XMapWindow (ShellWindow);

	while (1) {					/* loop forever */
	    XNextEvent (&inputevent);
	    code = ((XKeyOrButtonEvent *) &inputevent)->detail;
	    switch ((int) inputevent.type) {
		case ExposeWindow:			/* repaint the icon */
		    if (inputevent.window == ShellWindow)
			XPixmapPut (ShellWindow, 0, 0, 0, 0, 
				icon_width, icon_height, 
				IconPixmap, GXcopy, AllPlanes);
		    break;

		case KeyPressed:
		    c = StdMap [code & ValueMask] [KeyState(code)];
		    switch (c) {
			case -1:
			    feep ();
			    break;
			case SHFT:
			    perform ("SHIFT");
			    break;
			case CNTL:
			    perform ("CONTROL");
			    break;
			case LOCK:
			    perform ("LOCK");
			    break;
			case SYMBOL:
			    perform ("SYMBOL");
			    break;
			case KEYPAD:
			    switch (code & ValueMask) {
				case KC_KEYPAD_0:
				    perform ("KEYPAD0");
				    break;
				case KC_KEYPAD_PERIOD:
				    perform ("KEYPAD.");
				    break;
				case KC_ENTER:
				    perform ("ENTER");
				    break;
				case KC_KEYPAD_1:
				    perform ("KEYPAD1");
				    break;
				case KC_KEYPAD_2:
				    perform ("KEYPAD2");
				    break;
				case KC_KEYPAD_3:
				    perform ("KEYPAD3");
				    break;
				case KC_KEYPAD_4:
				    perform ("KEYPAD4");
				    break;
				case KC_KEYPAD_5:
				    perform ("KEYPAD5");
				    break;
				case KC_KEYPAD_6:
				    perform ("KEYPAD6");
				    break;
				case KC_KEYPAD_COMMA:
				    perform ("KEYPAD,");
				    break;
				case KC_KEYPAD_7:
				    perform ("KEYPAD7");
				    break;
				case KC_KEYPAD_8:
				    perform ("KEYPAD8");
				    break;
				case KC_KEYPAD_9:
				    perform ("KEYPAD9");
				    break;
				case KC_KEYPAD_MINUS:
				    perform ("KEYPAD-");
				    break;
				default:
				    feep ();
				    break;
			    } /*end switch*/
			    break;
			case CURSOR:
			    switch (code & ValueMask) {
				case KC_CURSOR_LEFT:
				    perform ("LEFTARROW");
				    break;
				case KC_CURSOR_RIGHT:
				    perform ("RIGHTARROW");
				    break;
				case KC_CURSOR_DOWN:
				    perform ("DOWNARROW");
				    break;
				case KC_CURSOR_UP:
				    perform ("UPARROW");
				    break;
				default:
				    feep ();
				    break;
			    }
			    break;
			case PFX:
			    switch (code & ValueMask) {
				case KC_PF1:
				    perform ("PF1");
				case KC_PF2:
				    perform ("PF2");
				case KC_PF3:
				    perform ("PF3");
				case KC_PF4:
				    perform ("PF4");
				default:
				    feep ();
				    break;
			    }
			    break;
			case FUNC1:
			    perform ("FUNC1");
			    break;
			case FUNC2:
			    perform ("FUNC2");
			    break;
			case FUNC3:
			    perform ("FUNC3");
			    break;
			case FUNC4:
			    perform ("FUNC4");
			    break;
			case FUNC5:
			    perform ("FUNC5");
			    break;
			case FUNC6:
			    perform ("FUNC6");
			    break;
			case FUNC7:
			    perform ("FUNC7");
			    break;
			case FUNC8:
			    perform ("FUNC8");
			    break;
			case FUNC9:
			    perform ("FUNC9");
			    break;
			case FUNC10:
			    perform ("FUNC10");
			    break;
			case FUNC11:
			    perform ("FUNC11");
			    break;
			case FUNC12:
			    perform ("FUNC12");
			    break;
			case FUNC13:
			    perform ("FUNC13");
			    break;
			case FUNC14:
			    perform ("FUNC14");
			    break;
			case FUNC15:
			    perform ("FUNC15");
			    break;
			case FUNC16:
			    perform ("FUNC16");
			    break;
			case FUNC17:
			    perform ("FUNC17");
			    break;
			case FUNC18:
			    perform ("FUNC18");
			    break;
			case FUNC19:
			    perform ("FUNC19");
			    break;
			case FUNC20:
			    perform ("FUNC20");
			    break;
			case E1:
			    perform ("E1");
			    break;
			case E2:
			    perform ("E2");
			    break;
			case E3:
			    perform ("E3");
			    break;
			case E4:
			    perform ("E4");
			    break;
			case E5:
			    perform ("E5");
			    break;
			case E6:
			    perform ("E6");
			    break;
			default:	/* must be ascii */
			    cbuf[0] = (char) c;
			    cbuf[1] = '\0';
			    perform (cbuf);
			    break;
		    } /*end switch on keycode*/
		    break;

		case ButtonPressed:
		    switch (code & ValueMask) {
			case LeftButton:
			    perform ("LEFTBUTTON");
			    break;
			case MiddleButton:
			    perform ("MIDDLEBUTTON");
			    break;
			case RightButton:
			    perform ("RIGHTBUTTON");
			    break;
			default:
			    feep ();
			    break;
		    }
		    break;

		default:
		    feep ();
		    break;
	    } /*end switch on event type*/
	} /*end while forever getting input events*/
} /*end main*/

/****************************************************************************
 * perform - This routine looks in its table to see if it already has a key
 * code, else it does an XGetDefault of the keyname.
 */

static perform (keyname)
	char *keyname;
{
	char buf[32];
	register char *cp;

	if (actionfound [code] == ACTION_NEW) {
	    (void) strcpy (buf, "action.");
	    (void) strcat (buf, keyname);
	    cp = XGetDefault (ProgramName, buf);
	    if (!cp) 
		actionfound [code] = ACTION_NOT_FOUND;
	    else {		/* else we have to parse the string */
		parseaction (cp);
	    } /*end if we have an action*/
	} /*end if we needed to look up an action*/

	if (actionfound [code] == ACTION_FOUND) {
	    if (vfork() == 0) 		/* in child, start program */
		execvp (actionvector [code] [0], actionvector [code]);
	    else 			/* in parent, flash icon */
		flash ();
	} else {
	    if (!quiet) feep ();
	}

	return;
}


static parseaction (actionstring)
	char *actionstring;
{
	register char *cp;
	register int wc = 0;		/* word count */
	register int inword;
	register char **actionlist;
	register int i;

	inword = 0;
	for (cp = actionstring; *cp; cp++) {	/* iterate over string */
	    if (isspace(*cp)) {
		if (inword) inword = 0;		/* no longer in a word */
	    } else {
		if (!inword) {			/* weren't in word */
		    inword = 1;			/* but now we are */
		    wc++;			/* so increment counter */
		}
	    }
	}
			/* wc now contains the number of separate words */
	actionlist = (char **) malloc ((unsigned)sizeof (char *) * (wc + 1));
	if (!actionlist) 
	    Error ("allocating memory for command list", actionstring);

	i = 0;
	inword = 0;
	for (cp = actionstring; *cp; cp++) {
	    if (isspace(*cp)) {
		if (inword) {			/* were in a word */
		    inword = 0;			/* but now we're not */
		}
		*cp = '\0';			/* and null out space */
	    } else {
		if (!inword) {			/* weren't in a word */
		    inword = 1;			/* but now we are */
		    actionlist [i++] = cp;	/* store pointer to start of word */
		}
	    }
	}
	actionlist [wc] = (char *) NULL;	/* execv wants this */

	actionfound [code] = ACTION_FOUND;
	actionvector [code] = actionlist;	/* store the action */
	return;
}


/****************************************************************************
 * feep - is designed to alert the user that something went wrong.  It could
 * put up a dialog box if it were smart....
 */

static feep ()
{
	XFeep (volume);
	XFlush ();
	return;
}


/****************************************************************************
 * flash - this just flashes the shell box a couple of times
 */

flash ()
{
	register int i, j;

	for (i = 0; i < nflash; i++) {
	    for (j = 0; j < 2; j++) {
		XPixFill (ShellWindow, 0, 0, width, height, BlackPixel,
				(Bitmap) 0, GXinvert, AllPlanes);
		XFlush ();
		(void) select (0, (fd_set *) 0, (fd_set *) 0, (fd_set *) 0, &delaytime);
	    }
	}
	return;
}

