#include <X/mit-copyright.h>

/* Copyright    Massachusetts Institute of Technology    1984, 1985	*/

/* ptyx.c */

#ifndef lint
static char *rcsid_ptyx_c = "$Header: main.c,v 10.44 86/05/16 10:17:20 jg Exp $";
#endif	lint

#include <pwd.h>
#include <sgtty.h>
#include <sys/wait.h>
#include <sys/time.h>
#include <sys/resource.h>
#include <stdio.h>
#include <sys/file.h>
#include <errno.h>
#include <signal.h>
#include <strings.h>
#include <X/Xlib.h>
#include "ptyx.h"
#include <grp.h>
#include <ttyent.h>
#include <utmp.h>

char *xterm_name;	/* argv[0] */
Terminal term;		/* master data structure for client */
int am_slave = 0;	/* set to 1 if running as a slave process */
int debug = 0; 		/* true causes error messages to be displayed */

static char *ptydev = "/dev/ptyxx";
static char *ttydev = "/dev/ttyxx";

#include "../cursors/xterm.cursor"
#include "../cursors/xterm_mask.cursor"
#include "icon.ic"
#include "icon_mask.ic"

static int reapchild ();

static char **command_to_exec = (char **) NULL;
static char *win_name = (char *) 0;


static struct  sgttyb d_sg = {
        0, 0, 0177, CKILL, EVENP|ODDP|ECHO|XTABS|CRMOD
};
static struct  tchars d_tc = {
        CINTR, CQUIT, CSTART,
        CSTOP, CEOF, CBRK,
};
static struct  ltchars d_ltc = {
        CSUSP, CDSUSP, CRPRNT,
        CFLUSH, CWERASE, CLNEXT
};
static int d_disipline = NTTYDISC;
static int d_lmode = LCRTBS|LCRTERA|LCRTKIL|LCTLECH;
static int no_dev_tty = 0;
static int loginflag = 0;
static int dologinflag = 0;
#ifdef sun
#ifdef TIOCCONS
static int SunConsole = 0;
#endif TIOCCONS
#endif sun


main (argc, argv)
int argc;
char **argv;
{
	int ind;
	char *strind, *strind1, *strscan();
	char *fn = "vtsingle";
	char *fb = "vtbold";
	short fnflag = 0;	/* True iff -fn option used */
	short fbflag = 0;	/* True iff -fb option used */
	char *getty = NULL;
	int reverse = 0, multiscroll = 0;
	int border = 1, tek = 0;
	int borderwidth = 2;
	int bitmapicon = 0;
#ifdef JUMPSCROLL
	int jumpscroll = 0;
#endif JUMPSCROLL
	int slave = 0;		/* if non-zero, run as slave */
	char passedPty[2];	/* name if pty if slave */
	char *fore_color;
	char *back_color;
	char *brdr_color;
	char *curs_color;
	char *mous_color;
	char display[256];
	char *getenv();
	char *option;
	char *geometry, *def = "=80x24+1+1";

	xterm_name = argv[0];

	display[0] = '\0';
	/*
	 * go get options out of default file
	 */
	if ((option = XGetDefault(argv[0], "BodyFont")) != NULL) {
		fn = option;
		fnflag = 1;
	}
	if ((option = XGetDefault(argv[0], "BoldFont")) != NULL) {
		fb = option;
		fbflag = 1;
	}
	if ((option = XGetDefault(argv[0], "InternalBorder")) != NULL) {
		border = atoi (option);
	}
	if ((option = XGetDefault(argv[0], "BorderWidth")) != NULL) {
		borderwidth = atoi (option);
	}

	if ((option = XGetDefault(argv[0], "BitmapIcon")) != NULL)
		if (strcmp (option, "on") == 0) 
			bitmapicon = 1;

	if ((option = XGetDefault(argv[0], "ReverseVideo")) != NULL)
		if (strcmp (option, "on") == 0)
			reverse = 1;

	if ((option = XGetDefault(argv[0], "Tektronix")) != NULL)
		if (strcmp (option, "on") == 0) 
			tek = 1;
#ifdef JUMPSCROLL
	if ((option = XGetDefault(argv[0], "JumpScroll")) != NULL)
		if (strcmp (option, "on") == 0) 
			jumpscroll = 1;
#endif JUMPSCROLL

	fore_color = XGetDefault(argv[0], "Foreground");
	back_color = XGetDefault(argv[0], "Background");
	brdr_color = XGetDefault(argv[0], "Border");
	curs_color = XGetDefault(argv[0], "Cursor");
	mous_color = XGetDefault(argv[0], "Mouse");

	/* parse command line */
	
	for (ind = 1; ind < argc; ind++) {
	    if (argv [ind] [0] == '=') {
		geometry = argv[ind];
		continue;
	    }

	    strind = index (argv[ind], ':');
	    if(strind != NULL) {
		strncpy(display,argv[ind],sizeof(display));
		continue;
	    }


	    strind = (char *) index (argv [ind], '-');

	    if (strind == NULL) Syntax ();

	    if (strcmp (argv [ind], "-L") == 0) {
		char tt[32];
		int mode = O_RDWR|O_NDELAY;
		loginflag = 1;
		getty = argv[argc-1];
		argc -= 1;
		strcpy(tt,"/dev/");
		strcat(tt, getty);
		chown(tt, 0, 0);
		chmod(tt, 0622);
		if (open(tt, mode, 0) < 0) {
			consolepr("open failed\n");
		}
		signal(SIGHUP, SIG_IGN);
		vhangup();
		setpgrp(0,0);
		signal(SIGHUP, SIG_DFL);
		open(tt, mode, 0);
		close(0);
		dup(1);
		dup(0);
		continue;
	    }
    
	    if (strncmp (argv [ind], "-S", 2) == 0) {
		sscanf(argv[ind] + 2, "%c%c%d", passedPty, passedPty+1,
			&slave);
		if (slave <= 0) Syntax();
		am_slave = 1;
		continue;
	    }

	    if (strcmp (argv [ind], "-e") == 0) {
		if (++ind >= argc) Syntax ();
		command_to_exec = argv + ind;
		break;
	    }
		
	    /* Switch to set up Tektronix-shaped (4096x3128 -> 512x390) window
	     * with characters sized to fit 39 lines of 85 characters each */

	    if (strcmp (argv [ind], "-t") == 0) {
		tek = 1;
		if (!fnflag) fn = "6x10";
		if (!fbflag) fb = "6x10";
		def = "85x39+1+1";
		continue;
	    }
		
	    if (strcmp (argv [ind], "-fn") == 0) {
		if (++ind >= argc) Syntax ();
		fn = argv [ind];
		fnflag = 1;
		continue;
	    }

	    if (strcmp (argv [ind], "-fb") == 0) {
		if (++ind >= argc) Syntax ();
		fb = argv [ind];
		fbflag = 1;
		continue;
	    }

	    if (strcmp (argv [ind], "-fg") == 0) {
		if (++ind >= argc) Syntax ();
		fore_color = argv [ind];
		continue;
	    }

	    if (strcmp (argv [ind], "-bg") == 0) {
		if (++ind >= argc) Syntax ();
		back_color = argv [ind];
		continue;
	    }

	    if (strcmp (argv [ind], "-bd") == 0) {
		if (++ind >= argc) Syntax ();
		brdr_color = argv [ind];
		continue;
	    }

	    if (strcmp (argv [ind], "-cr") == 0) {
		if (++ind >= argc) Syntax ();
		curs_color = argv [ind];
		continue;
	    }

	    if (strcmp (argv [ind], "-ms") == 0) {
		if (++ind >= argc) Syntax ();
		mous_color = argv [ind];
		continue;
	    }

	    if (strcmp (argv [ind], "-l") == 0) {
		dologinflag = 1;
		continue;
	    }

	    if (strcmp (argv [ind], "-d") == 0) {
		debug = 1;
		continue;
	    }

	    if (strcmp (argv [ind], "-b") == 0) {
		if (++ind >= argc) Syntax ();
		border = atoi (argv [ind]);
		continue;
	    }

	    if (strcmp (argv [ind], "-bw") == 0 ||
		strcmp (argv [ind], "-w") == 0) {
		if (++ind >= argc) Syntax ();
		borderwidth = atoi (argv [ind]);
		continue;
	    }

	    if (strcmp (argv [ind], "-rv") == 0 ||
		strcmp (argv [ind], "-r") == 0) {
		reverse = 1;	/* backwards from usual definition */
		continue;
	    }

	    if (strcmp (argv [ind], "-s") == 0) {
		multiscroll = 1;
		continue;
	    }

	    if (strcmp (argv [ind], "-i") == 0) {
		bitmapicon = 1;
		continue;
	    }

#ifdef JUMPSCROLL
	    if (strcmp (argv [ind], "-j") == 0) {
		jumpscroll = 1;
		continue;
	    }
#endif JUMPSCROLL

	    if (strcmp (argv [ind], "-n") == 0) {
		if (++ind >= argc) Syntax ();
		win_name = argv [ind];
		continue;
	    }
#ifdef sun
#ifdef TIOCCONS
	    if (strcmp (argv [ind], "-C") == 0) {
		SunConsole = 1;
		continue;
	    }
#endif TIOCCONS
#endif sun

	    Syntax ();
	}
	
	if (fnflag && !fbflag) fb = fn;
	if (!fnflag && fbflag) fn = fb;
#ifdef JUMPSCROLL
	if(tek)
		jumpscroll = 0;
#endif JUMPSCROLL
	Serve (display, fn, fb, geometry, def, getty, slave, passedPty,
	   border, borderwidth, tek, reverse, multiscroll, bitmapicon,
	   fore_color, back_color, brdr_color, curs_color, mous_color
#ifdef JUMPSCROLL
	   , jumpscroll
#endif JUMPSCROLL
	   );
}

Syntax ()
{
	static char *ustring[] = {
	"Usage: xterm [-rv] [-fn normal_font] [-fb bold_font]\n",
	"\t[=[width]x[height][[+-]xoff[[+-]yoff]]] [-bw bdr_width]\n",
	"\t[-fg color] [-bg color] [-bd color] [-cr color] [-ms color]\n",
	"\t[[[host]:vs]] [-d] [-s] [-t] [-i] [-j] [-e command_to_exec]\n",
	"\t[-n window_name]\n\n",
	"Fonts must be of fixed width and of same size;\n",
	"If only one font is specified, it will be used for normal and bold text\n",
#ifdef JUMPSCROLL
	"The -j option enables jump scroll\n",
#endif
	"The -s option enables asynchronous scrolling\n",
	"The -t option enables Tektronics 4010 emulation\n",
	"The -i option enables bitmap icons\n",
	"The -d option turns debugging on (error messages printed)\n",
	"The -n option sets the window name\n",
	"The -b option specifies the inner padding\n",
	"Default is: xterm -fn vtsingle -fb vtbold =80x24 :0\n",
	0};
	char **us = ustring;
	while (*us) fputs(*us++, stderr);
	exit (1);
}

char *strscan (search, what)
/*
   Returns pointer to first char ins search which is also in what, else NULL.
 */
char *search, *what;
{
	int i, len = strlen (what);
	char c;

	while ((c = *(search++)) != NULL)
	    for (i = 0; i < len; i++)
	    	if (c == what [i]) return (--search);

	return (NULL);
}

Serve (disp, fn, fb, geometry, def, getty, slave, passedPty,
       border, borderwidth, tek, reverse, multiscroll, bitmapicon,
       fore_color, back_color, brdr_color, curs_color, mous_color
#ifdef JUMPSCROLL
       , jumpscroll
#endif JUMPSCROLL
       )
char *disp;	 /* host of display to serve */
char *fn;	 /* fontname of normal characters */
char *fb;	 /* fontname of bold characters */
char *getty;	 /* true iff child should be getty */
int slave;	 /* true if should run as slave (contains file # of pty) */
char *passedPty; /* name of pty to use if slave */
char *geometry;	 /* user supplied geometry spec */
char *def;	 /* default geometry spec */
int border;	 /* inner border in pixels */
int borderwidth; /* outer border in pixels */
int tek;	 /* true ==> Tektronics emulation */
int reverse;	 /* true ==> black background, white characters */
int multiscroll; /* true ==> asynchronous full-screen scrolling */
int bitmapicon;  /* true ==> bitmap icons rather than text icon */
#ifdef JUMPSCROLL
int jumpscroll;  /* true ==> fast multi-line scrolling */
#endif JUMPSCROLL
char *fore_color;/* text color */
char *back_color;/* background color */
char *brdr_color;/* border color */
char *curs_color;/* text cursor color */
char *mous_color;/* mouse cursor color */
{
	int pty;	/* fildes for pty of client */
	XEvent reply;
	XEvent *rep = & reply;
	int aborted = 0;
	int Select_mask, select_mask = 0;
	int maxplus1;
	short toggled = NULL;
	Screen *screen = &term.screen;
	int Xsocket;
	int pty_mask, X_mask;
	extern int errno;
	int mode = 1;
#ifdef TIOCSWINSZ
	struct winsize ws;
#endif

	signal (SIGCHLD, reapchild);
	signal (SIGHUP, SIG_IGN);

	/* open a terminal for client */
	get_terminal (disp, &term, fn, fb, geometry, def, border,
	   borderwidth, (int)getty, slave, reverse, multiscroll, bitmapicon,
	   tek, fore_color, back_color, brdr_color, curs_color, mous_color
#ifdef JUMPSCROLL
	   , jumpscroll
#endif JUMPSCROLL
	   );

	Xsocket = screen->display->fd;

	spawn (disp, &pty, Xsocket, screen, getty, slave, passedPty);

	if (slave) {	/* Write window id so master end can read and use */
	    write(pty, &screen->window, sizeof(screen->window));
	    write(pty, "\n", 1);
	}

	screen->respond = term.buf.fildes = pty;

#ifdef TIOCSWINSZ
	/* tell tty how big window is */
	ws.ws_row = screen->max_row + 1;
	ws.ws_col = screen->max_col + 1;
	ws.ws_xpixel = screen->width;
	ws.ws_ypixel = screen->height;
	ioctl (screen->respond, TIOCSWINSZ, &ws);
#endif

	/* Initialize Tektronix graphics mode parameters */
	TekErase (&term);
	screen->TekEmu = tek;
	screen->cur_x = screen->cur_y = 0;
	screen->cur_X = screen->cur_Y = 0;
	screen->TekGMode = 0;
	screen->TekAMode = 0;
	screen->TekPMode = 0;

	if (ioctl (pty, FIONBIO, &mode) == -1) Error ();
	
	pty_mask = 1 << pty;
	X_mask = 1 << Xsocket;
	Select_mask = pty_mask | X_mask;
	maxplus1 = (pty < Xsocket) ? (1 + Xsocket) : (1 + pty);

	if (debug) printf ("debugging on\n");

	while (1)
	{
	   if (! aborted)
	   {
#ifdef JUMPSCROLL
		if(screen->scroll_amt)
			FlushScroll(screen);
#endif JUMPSCROLL
		if (toggled)
		{
		    CursorToggle (screen, toggled);
		    toggled = NULL;
		}

		select_mask = Select_mask;
		XFlush();
		while (select (maxplus1, &select_mask, NULL, NULL, 0) <= 0) {
			if (errno != EINTR) Error();
			}
	   }
	   else select_mask = NULL;

	   if (select_mask & pty_mask || aborted)
	   {
 		if (!toggled)
		{
		    CursorToggle (screen, toggled);
		    toggled = 1;
		}
		do {
			aborted = (*screen->mode)(&term);
		} while (screen->display->qlen==0 && term.buf.cnt>0);
	   }

	   if (select_mask & X_mask || aborted)
	   {
#ifdef JUMPSCROLL
		if(screen->scroll_amt)
			FlushScroll(screen);
#endif JUMPSCROLL
		XPending ();
		do {
		    XNextEvent (&reply);

		    switch ((int)reply.type)
		    {
			case KeyPressed:
				Input (&term.keyboard, &term.screen,
					(XKeyPressedEvent *)rep);
				break;

			case ExposeWindow:
				if (bitmapicon) {
					if (((XExposeWindowEvent *)rep)->window
					== screen->iconwindow) {
						RefreshIcon(screen);
						break;
						}
					}
				toggled = 1;
				if (ScreenResize (screen,
					((XExposeWindowEvent *)rep)->width,
					((XExposeWindowEvent *)rep)->height,
					&term.flags) != -1)
				{
				    TabReset (term.tabs);
				    XClear (screen->window);
				    ScrnRefresh (screen, 0, 0,
				    		 screen->max_row + 1,
						 screen->max_col + 1);

				    if (screen->TekEmu) TekRefresh (&term);
				}
				break;

			case ExposeRegion:
				if (((XExposeWindowEvent *)rep)->detail ==
				    	ExposeCopy &&
				    screen->incopy <= 0) {
					screen->incopy = 1;
					if (screen->scrolls > 0)
						screen->scrolls--;
				}
				if (HandleExposure (screen, &reply))
					toggled = 1;
				break;

			case ExposeCopy:
				if (screen->incopy <= 0 && screen->scrolls > 0)
					screen->scrolls--;
				if (screen->scrolls)
					screen->incopy = -1;
				else
					screen->incopy = 0;
				break;

			case ButtonPressed:
			case ButtonReleased:
				if (screen->incopy)
					CopyWait (screen);
				if (HandleButtons(&term,&reply,pty))
					toggled = 1;
				break;
		        /*
			 *  Enter window is being handled just to give xterm
			 *  a kick in the pants when the mouse gets in the
			 *  window in case it was swapped out.  Of course,
			 *  one might thrash...
			 */
			case EnterWindow:
				break;
			default:
				break;
			}
		} while (screen->display->qlen > 0);
	   }
	}
}

RefreshIcon(screen)
register Screen *screen;
{
	XBitmapBitsPut(screen->iconwindow, 0, 0, icon_width, icon_height,
		icon_bits, screen->foreground, screen->background,
		screen->iconmask, GXcopy, AllPlanes);
}

get_pty (pty, pty_name)
/*
   opens a pty, storing fildes in pty
   and it's identifying character in pty_name.
 */
int *pty;
char *pty_name;
{
	int devindex, letter = 0;
	int fd;
	extern errno;

	if (debug) {
	    fd = open ("xterm.debug.log", O_WRONLY | O_CREAT | O_TRUNC, 0666);
	    dup2 (fd, fileno (stderr));
	}

	while (letter < 4) {
	    ttydev [8] = ptydev [8] = "pqrs" [letter++];
	    devindex = 0;

	    while (devindex < 16) {
		ptydev [9] = *pty_name = "0123456789abcdef" [devindex++];
		if ((*pty = open (ptydev, O_RDWR, 0)) < 0)	{
			if (debug) fprintf (stderr, "pty %d code %d\n",
					devindex - 1, errno);
			continue;
		}
		goto got_pty;
	    }
	}
	
	fprintf (stderr,"Not enough available pty's\n");
	exit (11);

got_pty:
	if (debug) {
		close (fileno (stderr));
		close (fd);
	}
}

get_terminal (disp, term, fn, fb, geometry, def,
      border, borderwidth, do_warp, slave, reverse, multiscroll, bitmapicon,
      tek, fore_color, back_color, brdr_color, curs_color, mous_color
#ifdef JUMPSCROLL
      , jumpscroll
#endif JUMPSCROLL
      )
/* 
 * sets up X and initializes the terminal structure except for term.buf.fildes.
 */
char *disp;
register Terminal *term;
char *fn, *fb;
char *geometry, *def;
int border, borderwidth, do_warp, reverse, multiscroll, bitmapicon;
int slave;
int tek;
#ifdef JUMPSCROLL
int jumpscroll;
#endif JUMPSCROLL
char *fore_color, *back_color, *brdr_color, *curs_color, *mous_color;
{
	int width, height;
	FontInfo *fInfo;
	register Keyboard *keyboard;
	register Screen *screen;
	Buffer *buf;
	double scale_x, scale_y;
	Color cdef;
	int pixels[2];
	int try = 10;
	OpaqueFrame twindow;

	keyboard = &term->keyboard;
	screen = &term->screen;
	buf = &term->buf;

	term->flags = WRAPAROUND|SMOOTHSCROLL;

	keyboard->flags = NULL;
	keyboard->offset = 0;
	
	buf->cnt = 0;
	buf->ptr = &buf->buf[0];
	
	TabReset (term->tabs);

	while (try--)
	    if ((screen->display = XOpenDisplay(disp)) == NULL) {
		if (loginflag == 0) {
			fprintf(stderr,"No such display server %s\n", disp);
			exit(1);
		}
		sleep (5);
		continue;
	    }
	    else break;

	if (try <= 0)  {
		fprintf (stderr,"Can't connect to display server %s\n",
		disp);
		exit (111);
	}	    

	screen->foreground = BlackPixel;
	screen->background = WhitePixel;
	screen->cursorcolor = BlackPixel;
	screen->mousecolor = BlackPixel;
	screen->xorplane = 1;
	if (DisplayCells() > 2 && (fore_color || back_color || curs_color)) {
		if (tek) {
			if (curs_color && XParseColor(curs_color, &cdef)) {
				if (XGetColorCells(0, 2, 1, &screen->xorplane,
								pixels)) {
					screen->background = pixels[0];
					screen->foreground = pixels[1];
					screen->cursorcolor = screen->background |
					screen->xorplane;
					cdef.pixel = screen->cursorcolor;
					XStoreColor(&cdef);
				}
			} 
			else if (XGetColorCells(0, 1, 1, &screen->xorplane,
							&screen->background)) {
				screen->foreground = screen->background |
				screen->xorplane;
				screen->cursorcolor = screen->foreground;
			}
			if (screen->background != WhitePixel) {
				if (back_color == NULL ||
					!XParseColor(back_color, &cdef)) {
					cdef.pixel = WhitePixel;
					XQueryColor(&cdef);
				}
				cdef.pixel = screen->background;
				XStoreColor(&cdef);
				if(screen->cursorcolor != screen->foreground) {
					cdef.pixel = screen->foreground |
						screen->xorplane;
					XStoreColor(&cdef);
				}
				if (fore_color == NULL ||
					!XParseColor(fore_color, &cdef)) {
					cdef.pixel = BlackPixel;
					XQueryColor(&cdef);
				}
				cdef.pixel = screen->foreground;
				XStoreColor(&cdef);
			}
		}
		else {
			if (fore_color && XParseColor(fore_color, &cdef) &&
						XGetHardwareColor(&cdef)) {
				screen->foreground = 
					screen->foreground = cdef.pixel;
				reverse = 0;
			}
			if (back_color && XParseColor(back_color, &cdef) &&
						XGetHardwareColor(&cdef)) {
				screen->background = cdef.pixel;
				reverse = 0;
			}
			if (curs_color && XParseColor(curs_color, &cdef) &&
			XGetHardwareColor(&cdef))
			screen->cursorcolor = cdef.pixel;
			else
			screen->cursorcolor = screen->foreground;
		}
	}

	screen->border = border;
	screen->borderwidth = borderwidth;
	screen->fnt_norm = screen->fnt_bold = NULL;
	   
	if ((fInfo = XOpenFont(fn)) == NULL) {
		fprintf(stderr, "%s: Could not open font %s!\n",
			xterm_name, fn);
		exit(1);
	}
	if (fn) screen->fnt_norm = fInfo->id;
	if (fb) screen->fnt_bold = XOpenFont(fb)->id;
	screen->f_width = fInfo->width;
	screen->f_height = fInfo->height;

	if (brdr_color && DisplayCells() > 2 &&
	    XParseColor(brdr_color, &cdef) && XGetHardwareColor(&cdef))
	    screen->bordertile = XMakeTile(cdef.pixel);
	else
	    screen->bordertile = BlackPixmap;

	screen->cursor = XStoreBitmap(xterm_width, xterm_height, xterm_bits);
	screen->mask = 	XStoreBitmap(xterm_mask_width, xterm_mask_height, 
				    xterm_mask_bits);
	if (mous_color && DisplayCells() > 2 &&
	    XParseColor(mous_color, &cdef) && XGetHardwareColor(&cdef))
	    screen->mousecolor = cdef.pixel;
	else
	    screen->mousecolor = screen->cursorcolor;
	screen->curs = XStoreCursor(screen->cursor, screen->mask, 5, 8,
		screen->mousecolor, screen->background, GXcopy);
	screen->rcurs = XStoreCursor(screen->cursor, screen->mask, 5, 8,
		screen->background, screen->mousecolor, GXcopy);

	if (reverse) {	/* reverse is black background with white chars */
		term->flags |= REVERSE_VIDEO;
		screen->cursorcolor = screen->background;
		screen->background = screen->foreground;
		screen->foreground = screen->cursorcolor;
		if (screen->bordertile == BlackPixmap)
		    screen->bordertile = WhitePixmap;
	}
	screen->bgndtile = XMakeTile(screen->background);

	twindow.bdrwidth = screen->borderwidth;
	twindow.border = screen->bordertile;
	twindow.background = screen->bgndtile;

	screen->window = XCreateTerm ("Terminal Emulator", xterm_name,
		geometry, def, &twindow, 12, 8, 
		screen->border * 2, screen->border * 2,
		&width, &height, 
		fInfo, fInfo->width, fInfo->height);

	screen->width = twindow.width - border * 2;
	screen->height = twindow.height - border * 2;

	/* Reset variables used by ANSI emulation. */

	screen->ansi.a_type = 0;		/* New sequence.	*/
	screen->ansi.a_pintro = 0;		/* New sequence.	*/
	screen->ansi.a_final = 0;		/* New sequence.	*/
	screen->gsets[0] = 'B';			/* ASCII_G		*/
	screen->gsets[1] = 'B';
	screen->gsets[2] = '<';			/* DEC supplemental.	*/
	screen->gsets[3] = '<';
	screen->curgl = 0;			/* G0 => GL.		*/
	screen->curgr = 2;			/* G2 => GR.		*/
	screen->curss = 0;			/* No single shift.	*/
	screen->rx8bit = 0;			/* 7 bit.		*/
	screen->tx8bit = 0;			/* 7 bit.		*/
	screen->mode = ANSInormal;

	/* Reset Tektronix alpha mode */
	screen->TekGMode = 0;
	screen->TekAMode = 0;
	screen->cur_x = screen->cur_y = 0;
	screen->cur_X = screen->cur_Y = 0;

	scale_x = screen->width / 4096.0;
	scale_y = screen->height / 3128.0;
	screen->TekScale = scale_x;
	if (scale_y < scale_x) screen->TekScale = scale_y;

	if (bitmapicon) {
		screen->iconwindow = XCreateWindow (RootWindow,
			0, 0, icon_width, icon_height, 0, 0, 0);

		XTileRelative(screen->iconwindow);

		XSetIconWindow(screen->window, screen->iconwindow);

		screen->iconmask = XStoreBitmap(icon_mask_width,
			icon_mask_height, icon_mask_bits);
		
		XSelectInput (screen->iconwindow, ExposeWindow);
	}

	if (reverse)
		XDefineCursor(screen->window, screen->rcurs);
	else	XDefineCursor(screen->window, screen->curs);

	XStoreName (screen->window, (win_name != (char *) 0 ? win_name:
			(do_warp ? "login" :
			(slave ? "xtermslave" :
			(command_to_exec ? command_to_exec[0] : "xterm")))));

	XSetResizeHint (screen->window,
			2 * border, 2 * border, fInfo->width, fInfo->height);
		
	XMapWindow (screen->window);

	XSelectInput (screen->window, KeyPressed | ExposeWindow | EnterWindow |
		ButtonPressed | ButtonReleased | ExposeRegion | ExposeCopy);


	if (do_warp)
		XWarpMouse (screen->window,
			    screen->width >> 1, screen->height >>1);

	screen->cur_col = screen->cur_row = 0;
	screen->max_col = screen->width  / fInfo->width - 1;
	screen->top_marg = 0;
	screen->bot_marg = screen->max_row = screen->height/ fInfo->height - 1;

	screen->sc.row = screen->sc.col = screen->sc.flags = NULL;


	/* allocate memory for screen buffer */
	screen->buf = (ScrnBuf) Allocate (screen->max_row + 1,
					  screen->max_col +1);

	screen->do_wrap = NULL;
	screen->scrolls = screen->incopy = 0;
	screen->multiscroll = multiscroll;
#ifdef JUMPSCROLL
	if (screen->jumpscroll = jumpscroll)
		term->flags &= ~SMOOTHSCROLL;
#endif JUMPSCROLL


	/* display initial cursor */
	CursorToggle (screen, 1);
}

spawn (display, pty, Xsocket, screen, getty, slave, passedPty)
/* 
 *  Inits pty and tty and forks a login process. Returns fd for pty in pty.
 *  Does not close fd Xsocket.
 *  If getty,  execs getty rather than csh and uses std fd's rather
 *  than opening a pty/tty pair.
 *  If slave, the pty named in passedPty is already open for use
 */
int *pty, Xsocket;
Screen *screen;
char *getty;		/* if true execs /etc/getty - Xwindow */
int slave;
char *passedPty;
char *display;
{
	int index1, tty;
	char pty_name;
	int discipline;
	unsigned lmode;
	struct tchars tc;
	struct ltchars ltc;
	struct sgttyb sg;

	char termcap [1024];
	char newtc [1024];
	char prog [256];
	char numbuf[10];
	char *ptr;
	char *index (), *strindex ();
	int i = 0;
	char **envnew;		/* new environment */
	struct passwd *getpwuid();
	struct passwd *pw;
	char logindev[32];
	char *TermName = "xterm";
	int ldisc = 0;

	/* be real paranoid about getting some usable entry */
	if (tgetent (termcap, TermName)  == 1
		|| (TermName = "vt102", tgetent(termcap, TermName)) == 1
		|| (TermName = "ansi",  tgetent(termcap, TermName)) == 1) {
		/* update termcap string */
		/* first do columns */
		if ((ptr = strindex (termcap, "co#")) == NULL){
			fprintf(stderr,"Can't find co# in termcap string %s\n",
				TermName);
			exit (1);
		}
		strncpy (newtc, termcap, ptr - termcap + 3);
		newtc[ptr-termcap+3] = '\0';
		sprintf (numbuf, "%d\0", screen->max_col + 1);
		strcat (newtc, numbuf);
		ptr = index (ptr, ':');
		strcat (newtc, ptr);
		strncpy (termcap, newtc, sizeof(termcap));
		/* now do lines */
		if ((ptr = strindex (termcap, "li#")) == NULL) {
			fprintf(stderr,"Can't find li# in termcap string %s\n",
				TermName);
			exit (1);
		}
		strncpy (newtc, termcap, ptr - termcap + 3);
		newtc[ptr-termcap+3] = '\0';
		sprintf (numbuf, "%d\0", screen->max_row + 1);
		strcat (newtc, numbuf);
		ptr = index (ptr, ':');
		strcat (newtc, ptr);
		if (strcmp(TermName, "xterm") != 0)
		    fprintf(stderr, 
		    "xterm: can't find xterm termcap entry, using %s instead!\n", TermName);
	}
	else fprintf(stderr,"xterm: can't find any usable termcap entry!\n");

	if (getty) {
		strcpy(logindev,"/dev/");
		strcat(logindev,getty);
		logindev [5] = 'p';
		*pty = open (logindev, O_RDWR, 0);
	}
	else if (slave) {
	    *pty = slave;
	    ptydev[8] = ttydev[8] = passedPty[0];
	    ptydev[9] = ttydev[9] = passedPty[1];
	}
	else {
		if ((tty = open ("/dev/tty", O_RDWR, 0)) < 0) {
			if (errno != ENXIO) Error();
			else {
				no_dev_tty = 1;
				sg = d_sg;
				tc = d_tc;
				discipline = d_disipline;
				ltc = d_ltc;
				lmode = d_lmode;
				for (index1 = 0; index1 < 3; index1++)
				    close (index1);
			}
		}
		else {
			/* get a copy of the current terminal's state */

			if ((tty = open ("/dev/tty", O_RDWR, 0)) < 0) Error ();

			if (ioctl (tty, TIOCGETP, &sg) == -1) Error ();
			if (ioctl (tty, TIOCGETC, (int *)&tc) == -1) Error ();
			if (ioctl (tty, TIOCGETD, &discipline) == -1) Error ();
			if (ioctl (tty, TIOCGLTC, (int *)&ltc) == -1) Error ();
			if (ioctl (tty, TIOCLGET, &lmode) == -1) Error ();

			close (tty);

			/* close all std file descriptors */
			for (index1 = 0; index1 < 3; index1++)
			    close (index1);
			if ((tty = open ("/dev/tty", O_RDWR, 0)) < 0) Error ();

			if (ioctl (tty, TIOCNOTTY, 0) == -1) Error ();
			close (tty);
		}

		get_pty (pty, &pty_name);

		if (*pty != Xsocket + 1) {
			dup2 (*pty, Xsocket + 1);
			close (*pty);
			*pty = Xsocket + 1;
		}

		ttydev [9] = pty_name;
		if ((tty = open (ttydev, O_RDWR, 0)) < 0) Error ();

		/* change ownership of tty to real group and user id */
		chown (ttydev, getuid (), tty_gid (getgid()));

		/* change protection of tty */
		chmod (ttydev, 0620);

		if (tty != Xsocket + 2)	{
			dup2 (tty, Xsocket + 2);
			close (tty);
			tty = Xsocket + 2;
		}

		/* set the new terminal's state to be the old one's 
		   with minor modifications for efficiency */

		sg.sg_flags &= ~(ALLDELAY | XTABS | CBREAK | RAW);
		sg.sg_flags |= ECHO | CRMOD;
		/* make sure speed is set on pty so that editors work right*/
		sg.sg_ispeed = B9600;
		sg.sg_ospeed = B9600;

		if (ioctl (tty, TIOCSETP, &sg) == -1) Error ();
		if (ioctl (tty, TIOCSETC, (int *)&tc) == -1) Error ();
		if (ioctl (tty, TIOCSETD, &discipline) == -1) Error ();
		if (ioctl (tty, TIOCSLTC, (int *)&ltc) == -1) Error ();
		if (ioctl (tty, TIOCLSET, &lmode) == -1) Error ();
#ifdef sun
#ifdef TIOCCONS
		if (SunConsole) {
			int on = 1;
			if (ioctl (tty, TIOCCONS, &on) == -1) Error();
		}
#endif TIOCCONS
#endif sun

		close (open ("/dev/null", O_RDWR, 0));

		for (index1 = 0; index1 < 3; index1++)
			dup2 (tty, index1);
	}

	if (!slave && (screen->pid = fork ()) == -1) Error ();
		
	if (!slave && screen->pid == 0) {
		extern char **environ;
		int pgrp = getpid();

		close (Xsocket);
		close (*pty);

		if (getty == NULL) close (tty);

		signal (SIGCHLD, SIG_DFL);
		signal (SIGHUP, SIG_IGN);

		/* copy the environment before Setenving */
		while (environ [i] != NULL) i++;
		envnew = (char **) malloc (sizeof (char *) * (i + 4));
		for (; i >= 0; i--) envnew [i] = environ [i];
		environ = envnew;
		Setenv ("TERM=", TermName);
		Setenv ("TERMCAP=", newtc);
		/* put the display into the environment of the shell*/
		if (display[0] != '\0') 
		Setenv ("DISPLAY=", screen->display->displayname);

		pw = getpwuid (getuid ());
		signal(SIGTERM, SIG_DFL);
		ioctl(0, TIOCSPGRP, &pgrp);
		setpgrp (0, 0);
		close(open(ttyname(0), O_WRONLY, 0));
		setpgrp (0, pgrp);
		if (dologinflag)	/* login */
			utrelog(1, pw->pw_name, screen->display->displayname);

		setgid(getgid ());
		setuid(getuid ());

		if (command_to_exec) {
			execvp(command_to_exec[0], command_to_exec, 0);
		}
		signal(SIGHUP, SIG_IGN);
		if (getty) {
			ioctl (0, TIOCNOTTY, 0);
			execl ("/etc/getty", "+", "Xwindow", getty, 0);
		}
		signal(SIGHUP, SIG_DFL);

		if (*pw->pw_shell == '\0') pw->pw_shell = "/bin/sh";

		/* make sure line discipline gets set */
		ldisc = 0;
		ioctl(0, TIOCSETD, &ldisc);
		if (!strcmp(pw->pw_shell, "/bin/csh")) {
			ldisc = NTTYDISC;
			ioctl(0, TIOCSETD, &ldisc);
		}
		
		ptr = rindex(pw->pw_shell, '/');
		if (ptr == NULL)
			ptr = pw->pw_shell;
		else
			ptr++;
		if (dologinflag) {
			prog[0] = '-';
			strcpy(&prog[1], ptr);
		} else
			strcpy(prog, ptr);
		execlp (pw->pw_shell, prog, 0);
		fprintf (stderr,"Error: Could not exec %s!\n", pw->pw_shell);
		sleep(5);
		Cleanup(121);
	}

	close (tty);
	signal(SIGHUP,SIG_IGN);
	signal(SIGTTOU,SIG_IGN); /* so that TIOCSWINSZ doesn't block */

	if ((tty = open (no_dev_tty ? "/dev/null" : "/dev/tty",
	    O_RDWR, 0)) < 0) Error();
	for (index1 = 0; index1 < 3; index1++)
		dup2 (tty, index1);
	if (tty > 2) close (tty);

	/* set ids to user's */
	/*
	setgid (getgid ());
	setuid (getuid ());
		setregid (getegid (), getgid ());
		setreuid (geteuid (), getuid ());
	*/

}


static reapchild ()
{
	extern Terminal term;
	register long pgrp;
	union wait status;
	int pid;
	
	if (debug) printf ("Exiting\n");
	pid  = wait3 (&status, WNOHANG, NULL);
	if (!pid) return;
	if (pid != term.screen.pid) return;
	
	if (dologinflag)
		utrelog(0, "", "");
	Cleanup(0);
}

consolepr(string)
char *string;
{
	extern int errno;
	extern char *sys_errlist[];
	int oerrno;
	int f;
	oerrno = errno;
	f = open("/dev/console",O_WRONLY, 0);
	write(f, "xterm: ", 7);
	write(f, string, strlen(string));
	write(f, ": ", 2);
	write(f, sys_errlist[oerrno],strlen(sys_errlist[oerrno]));
	write(f, "\n", 1);
	close(f);
	if ((f = open("/dev/tty", 2, 0)) >= 0) {
		ioctl(f, TIOCNOTTY, 0);
		close(f);
	}
}

#define TTYGRPNAME	"tty"

tty_gid(default_gid)
	int default_gid;
{
	struct group *getgrnam(), *gr;
	int gid = default_gid;

	gr = getgrnam(TTYGRPNAME);
	if (gr != (struct group *) 0)
		gid = gr->gr_gid;

	endgrent();

	return (gid);
}

utrelog(io, user, display)
int io;
char *user;
char *display;
{
	struct utmp ut;
	struct ttyent *ty;
	register int s;
	long slot;
	int ufd;
	char *colon;
	
	if (io == 1) {
		strncpy(ut.ut_line, &ttydev[5], sizeof ut.ut_line);
		colon = index(display, ':');
		if (colon)
			*colon = '\0';
		strncpy(ut.ut_host, display, sizeof ut.ut_host);
		if (colon)
			*colon = ':';
		strncpy(ut.ut_name, user, sizeof ut.ut_name);
		(void) time(&ut.ut_time);
	}
	else {
		strcpy(ut.ut_line, "");
		strcpy(ut.ut_name, "");
		strcpy(ut.ut_host, "");
		ut.ut_time = 0;
		chown(ttydev, 0, 0);
		chmod(ttydev, 0666);
	}

	setttyent();
	slot = 0;
	s = 0;
	while ((ty = getttyent()) != NULL) {
		s++;
		if (strcmp(ty->ty_name, &ttydev[5]) == 0) {
			slot = s;
			break;
		}
	}
	endttyent();
	if (slot > 0 && (ufd = open("/etc/utmp", O_WRONLY, 0)) >= 0) {
		if (lseek(ufd, slot * sizeof ut, 0) < 0L ||
		    write(ufd, (char *)&ut, sizeof ut) != sizeof ut) {
			close(ufd);
			return(-1);
		}
		close(ufd);
	}
	return(0);
}
