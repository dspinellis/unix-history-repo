#ifndef lint
static char *rcsid = "$Header: initial.c,v 10.6 86/12/17 20:34:22 swick Exp $";
#endif lint
#ifdef	sun
/*
 * The Sun X drivers are a product of Sun Microsystems, Inc. and are provided
 * for unrestricted use provided that this legend is included on all tape
 * media and as a part of the software program in whole or part.  Users
 * may copy or modify these drivers without charge, but are not authorized
 * to license or distribute them to anyone else except as part of a product or
 * program developed by the user.
 * 
 * THE SUN X DRIVERS ARE PROVIDED AS IS WITH NO WARRANTIES OF ANY KIND
 * INCLUDING THE WARRANTIES OF DESIGN, MERCHANTIBILITY AND FITNESS FOR A
 * PARTICULAR PURPOSE, OR ARISING FROM A COURSE OF DEALING, USAGE OR TRADE
 * PRACTICE.
 *
 * The Sun X Drivers are provided with no support and without any obligation
 * on the part of Sun Microsystems, Inc. to assist in their use, correction,
 * modification or enhancement.
 * 
 * SUN MICROSYSTEMS, INC. SHALL HAVE NO LIABILITY WITH RESPECT TO THE
 * INFRINGEMENT OF COPYRIGHTS, TRADE SECRETS OR ANY PATENTS BY THE SUN X
 * DRIVERS OR ANY PART THEREOF.
 * 
 * In no event will Sun Microsystems, Inc. be liable for any lost revenue
 * or profits or other special, indirect and consequential damages, even if
 * Sun has been advised of the possibility of such damages.
 * 
 * Sun Microsystems, Inc.
 * 2550 Garcia Avenue
 * Mountain View, California  94043
 */

#ifndef	lint
static char sccsid[] = "@(#)initial.c 1.2 86/08/26 Copyright 1986 Sun Micro";
#endif

/*-
 * Copyright 1985, Massachusetts Institute of Technology
 * Copyright (c) 1986 by Sun Microsystems,  Inc.
 */

/* initial.c	Routines to open & close display
 *
 *	OpenDisplay		Open it
 *	InitDisplay		Download it
 *	DisplayDead		Check if dead
 *	Allocate_space		Allocate some temporary storage
 *
 */

/*
 *	ToDo:
 *		Look in environment/defaults for programs to start
 */


#include <stdio.h>
#include <fcntl.h>
#include <errno.h>
#include <sys/time.h>
#include <sys/ioctl.h>
#include <sys/signal.h>
#include <sun/fbio.h>
#include "Xsun.h"
#include <pixrect/pixrect_hs.h>
#include <sunwindow/rect.h>
#include <sunwindow/rectlist.h>
#include <sunwindow/pixwin.h>
#include <sunwindow/win_screen.h>
#include <sunwindow/win_struct.h>
#include <sunwindow/win_input.h>
#include <sundev/kbd.h>
#include <sundev/kbio.h>

extern int InputReader();

struct pixwin *Display;
struct pixrect *PixRect;
u_char InvPix[256];

short Sun_From_X_Op[] = {
    	PIX_CLR,			/* GXclear */
	PIX_SRC&PIX_DST,		/* GXand */
	PIX_SRC&PIX_NOT(PIX_DST),	/* GXandReverse */
	PIX_SRC,			/* GXcopy */
	PIX_NOT(PIX_SRC)&PIX_DST,	/* GXandInverted */
	PIX_DST,			/* GXnoop */
	PIX_SRC^PIX_DST,		/* GXxor */
	PIX_SRC|PIX_DST,		/* GXor */
	PIX_NOT(PIX_SRC)&PIX_NOT(PIX_DST), /* GXnor */
	PIX_NOT(PIX_SRC)^PIX_DST,	/* GXequiv */
	PIX_NOT(PIX_DST),		/* GXinvert */
	PIX_SRC|PIX_NOT(PIX_DST),	/* GXorReverse */
	PIX_NOT(PIX_SRC),		/* GXcopyInverted */
	PIX_NOT(PIX_SRC)|PIX_DST,		/* GXorInverted */
	PIX_NOT(PIX_SRC)|PIX_NOT(PIX_DST), /* GXnand */
	PIX_SET,			/* GXset */
};

int vsdev = -1;
extern int errno;
DEVICE *CurrentDevice;

#define PARENT "WINDOW_GFX"
/* Open the display */
static char *parent;

#ifdef	RAW_KBD
struct kiockey	sunkeymap[128*5];
#endif

/*ARGSUSED*/
OpenDisplay (devname)
	char *devname;
{
    struct screen sc;
    extern char *getenv();

    signal (SIGWINCH, SIG_IGN);
    parent = getenv(PARENT);
    bzero((caddr_t) & sc, sizeof sc);
    if (*devname != '/')
	devname = "/dev/fb";
    strncpy(sc.scr_fbname, devname, SCR_NAMESIZE);
    if (parent) {
	/* Running under "overview" */
	int         pfd;

	if ((pfd = open(parent, 2, 0)) < 0) {
	    fprintf(stderr, "Can't open parent %s\n", parent);
	    return (-1);
	}
	vsdev = win_getnewwindow();
	win_setlink(vsdev, WL_PARENT, win_fdtonumber(pfd));
	win_insert(vsdev);
	close(pfd);
    }
    else {
	/* Running alone */
	vsdev = win_screennew(&sc);
    }
    return (vsdev);
}

/* Do sun specific initialization */
#ifdef	notdef
static int pid;
#define	XTERM	"xterm"
static char *args[] = {
    XTERM,
    "=80x24+30+30",
    "-C",
    "-n",
    "console",
    0,
};

#include <sys/wait.h>

static SigChildHandler()
{
    register int dead;
    union wait status;

    while ((dead = wait3(&status, WNOHANG, NULL)) > 0) {
	if (dead == pid) {
	    exit(0);
	}
    }
}

static StartShell()
{
    signal(SIGCHLD, SigChildHandler);
    if ((pid = fork()) == 0) {
	{
	    register int i = getdtablesize();

	    while (--i >= 0) {
		close(i);
	    }
	}
	open("/dev/null", 2, 0);
	dup(0);
	dup(0);
	{
	    /* Copy the environment for Setenv */
	    extern char **environ;
	    register int i = 0;
	    char      **envnew;

	    while (environ[i] != NULL)
		i++;

	    envnew = (char **) malloc(sizeof(char *) * (i + 1));
	    for (; i >= 0; i--)
		envnew[i] = environ[i];
	    environ = envnew;
	}
	{
#define	HOSTNAMESIZE	128
	    char        DisplayName[HOSTNAMESIZE];

	    gethostname(DisplayName, HOSTNAMESIZE);
	    strcat(DisplayName, ":0");
	    Setenv("DISPLAY=", DisplayName);
#undef	HOSTNAMESIZE
	}
	execvp(XTERM, args);
	_exit(1);
    }
}
#endif

InitDisplay (info)
	register DEVICE *info;
{
    register int i;
    static vsCursor vsc;
    static vsBox vsm;
    static vsEventQueue vsq = {
			       NULL,
			       0,
			       0,
			       0,
    };
    struct screen sc;

    win_screenget(vsdev, &sc);
    info->height = sc.scr_rect.r_height;
    info->width = sc.scr_rect.r_width;
    if (parent) {
	/* running under "overview" */
	win_setrect(vsdev, &sc.scr_rect);
    }
    {
	struct fbtype fbt;
	int         fd = open(sc.scr_fbname, O_RDWR, 0);

	if (fd < 0 || ioctl(fd, FBIOGTYPE, &fbt) < 0) {
	    if (fd < 0)
		fprintf(stderr, "Can't open fb %s\n", sc.scr_fbname);
	    else
	        fprintf(stderr, "Can't FBIOGTYPE on %s\n", sc.scr_fbname);
	    return (-1);
	}
	close(fd);
	info->id = fbt.fb_type + SUN_BASE;
	info->planes = fbt.fb_depth;
	info->entries = fbt.fb_cmsize;
    }
    Display = pw_open(vsdev);
    PixRect = Display->pw_pixrect;
    pw_reversevideo(Display,0,1);
    {
	struct inputmask im;
	struct kiockey kk;
	int	kbfd, i = 0;

	if ((kbfd = open("/dev/kbd", O_RDWR, 0)) >= 0) {
#ifdef	RAW_KBD
		struct kiockey ok;

		for (i = 0; i < 0200; ++i) {
			ok.kio_station =
			kk.kio_station = kk.kio_entry = i;

			ok.kio_tablemask = kk.kio_tablemask = 0;
			ioctl(kbfd, KIOCGETKEY, &ok);
			sunkeymap[i] = ok;
			/*
			 * If I am a shift key then remap past what
			 * kbd.c knows about so no mapping is done
			 * Only shift keys can be used the with UPMASK
			 * due to assumptions in sun's kernel
			 */
			if ((ok.kio_entry & 0xf0) == SHIFTKEYS) {
				switch (ok.kio_entry) {
				default:
				case SHIFTKEYS+CAPSLOCK:
					kk.kio_entry = SHIFTKEYS+10;
					break;
				case SHIFTKEYS+SHIFTLOCK:
					kk.kio_entry = SHIFTKEYS+13;
					break;
				case SHIFTKEYS+LEFTSHIFT:
					kk.kio_entry = SHIFTKEYS+11;
					break;
				case SHIFTKEYS+RIGHTSHIFT:
					kk.kio_entry = SHIFTKEYS+14;
					break;
				case SHIFTKEYS+LEFTCTRL:
					kk.kio_entry = SHIFTKEYS+12;
					break;
				case SHIFTKEYS+RIGHTCTRL:
					kk.kio_entry = SHIFTKEYS+15;
					break;
				}
			} else if (ok.kio_entry == BUCKYBITS+METABIT)
				kk.kio_entry = SHIFTKEYS+09;
			else if (ok.kio_entry == BUCKYBITS+SYSTEMBIT)
				kk.kio_entry = ok.kio_entry;
			ioctl(kbfd, KIOCSETKEY, &kk);

			ok.kio_tablemask = kk.kio_tablemask = CTRLMASK;
			ioctl(kbfd, KIOCGETKEY, &ok);
			sunkeymap[i+00200] = ok;

			ok.kio_tablemask = kk.kio_tablemask = CAPSMASK;
			ioctl(kbfd, KIOCGETKEY, &ok);
			sunkeymap[i+00400] = ok;

			ok.kio_tablemask = kk.kio_tablemask = SHIFTMASK;
			ioctl(kbfd, KIOCGETKEY, &ok);
			sunkeymap[i+00600] = ok;

			ok.kio_tablemask = kk.kio_tablemask = UPMASK;
			ioctl(kbfd, KIOCGETKEY, &ok);
			sunkeymap[i+01000] = ok;
			if ((kk.kio_entry & 0xf0) == SHIFTKEYS)
				ioctl(kbfd, KIOCSETKEY, &kk);
		}
#else
		kk.kio_tablemask = 0;
		kk.kio_entry = ~(SHIFTKEYS+CAPSLOCK);
		do {
			kk.kio_station = i++;
			ioctl(kbfd, KIOCGETKEY, &kk);
		} while ( i < 128 && kk.kio_entry != SHIFTKEYS+CAPSLOCK);
		if (kk.kio_entry == SHIFTKEYS+CAPSLOCK) {
			kk.kio_tablemask = UPMASK;
			ioctl(kbfd, KIOCSETKEY, &kk);
			kk.kio_tablemask = CTRLMASK;
			ioctl(kbfd, KIOCSETKEY, &kk);
			kk.kio_tablemask = CAPSMASK;
			ioctl(kbfd, KIOCSETKEY, &kk);
			kk.kio_tablemask = SHIFTMASK;
			ioctl(kbfd, KIOCSETKEY, &kk);
		}
#endif
		close(kbfd);
	}
	input_imnull(&im);
	im.im_flags = IM_ASCII | IM_META | IM_NEGEVENT;
	win_setinputcodebit(&im, LOC_MOVE);
	win_setinputcodebit(&im, MS_LEFT);
	win_setinputcodebit(&im, MS_MIDDLE);
	win_setinputcodebit(&im, MS_RIGHT);
#ifdef	RAW_KBD
	win_setinputcodebit(&im, VKEY_FIRSTSHIFT+9);
	win_setinputcodebit(&im, VKEY_FIRSTSHIFT+10);
	win_setinputcodebit(&im, VKEY_FIRSTSHIFT+11);
	win_setinputcodebit(&im, VKEY_FIRSTSHIFT+12);
	win_setinputcodebit(&im, VKEY_FIRSTSHIFT+13);
	win_setinputcodebit(&im, VKEY_FIRSTSHIFT+14);
	win_setinputcodebit(&im, VKEY_FIRSTSHIFT+15);
#else
	win_setinputcodebit(&im, SHIFT_CAPSLOCK);
#endif
	win_setinputmask(vsdev, &im, NULL, WIN_NULLLINK);
    }
    if (fcntl(vsdev, F_SETFL, O_NDELAY) < 0)
	return (-1);
    {
	/* Following struct cannot be included from win_cursor.h */
	struct cursor {
		short	cur_xhot, cur_yhot;	/* offset of mouse position from shape*/
		int	cur_function;		/* relationship of shape to screen */
		struct	pixrect *cur_shape;	/* memory image to use */
		int	flags;			/* various options */
	
		short	horiz_hair_thickness;	/* horizontal crosshair height */
		int	horiz_hair_op;		/*            drawing op       */
		int	horiz_hair_color;	/*            color            */ 
		short	horiz_hair_length;	/*            width           */ 
		short	horiz_hair_gap;		/*            gap             */ 
	
		short	vert_hair_thickness;	/* vertical crosshair width  */
		int	vert_hair_op;		/*          drawing op       */
		int	vert_hair_color;	/*          color            */ 
		short	vert_hair_length;	/*          height           */ 
		short	vert_hair_gap;   	/*          gap              */ 
	} cs;
	static struct pixrect pr;

	cs.cur_xhot = cs.cur_yhot = cs.cur_function = 0;
	cs.flags = 0;
	cs.cur_shape = &pr;
	pr.pr_size.x = pr.pr_size.y = 0;
	win_setcursor(vsdev, &cs);
	win_setmouseposition(vsdev, info->width >> 1, info->height >> 1);
    }
    info->mouse = &vsc;
    info->mbox = &vsm;
    info->queue = &vsq;
    Define_input_handler(InputReader);
    SetUpInvPix();
    CurrentDevice = info;
#ifdef	notdef
    StartShell();
#endif
    return (0);
}

/* Check if display is dead */

DisplayDead ()
{
#ifdef	RAW_KBD
	int i;
	int kbfd = open("/dev/kbd", O_RDWR, 0);
	for (i = 0; i < 0200; ++i) {
		ioctl(kbfd, KIOCSETKEY, &sunkeymap[i]);
		if ((sunkeymap[i].kio_entry & 0xf0) == SHIFTKEYS)
			ioctl(kbfd, KIOCSETKEY, &sunkeymap[i+01000]);
		else if (sunkeymap[i].kio_entry == BUCKYBITS+METABIT)
			ioctl(kbfd, KIOCSETKEY, &sunkeymap[i+01000]);
	}
	close(kbfd);
#endif
	return(0);
}

/* the presumption here is that only one Allocate_space call is made/request */

#define ABUFSIZE 3072
static char ABuffer[3072];	/* random size buffer for allocate space */
caddr_t AllocateSpace (size)
	register int size;
{
	if (size < ABUFSIZE) return(ABuffer);
	errno = ENOMEM;
	return (NULL);
}

/* XXX - should be static data */
SetUpInvPix()
{
    register int i;

    for (i = 255; i >= 0; i--) {
	register int j = 1, k = 128, l = 8;

	while (l--) {
	    if ((i & j) != 0)
		InvPix[i] |= k;
	    j <<= 1;
	    k >>= 1;
	}
    }
}

#ifdef	notdef
Setenv (var, value)
/*
   sets the value of var to be arg in the Unix 4.2 BSD environment env.
   Var should end with '='.
   (bindings are of the form "var=value")
   This procedure assumes the memory for the first level of environ
   was allocated using malloc.
 */
register char *var, *value;
{
    extern char **environ;
    register int index = 0;

    while (environ[index] != NULL) {
	if (strncmp(environ[index], var, strlen(var)) == 0) {
	    /* found it */
	    environ[index] = (char *) malloc(strlen(var) + strlen(value));
	    strcpy(environ[index], var);
	    strcat(environ[index], value);
	    return;
	}
	index++;
    }

    if ((environ = (char **) realloc(environ, sizeof(char *) *
				     (index + 2))) == NULL) {
	fprintf(stderr, "Setenv: malloc out of memory\n");
	exit(1);
    }

    environ[index] = (char *) malloc(strlen(var) + strlen(value));
    strcpy(environ[index], var);
    strcat(environ[index], value);
    environ[++index] = NULL;
}
#endif
#endif	sun
