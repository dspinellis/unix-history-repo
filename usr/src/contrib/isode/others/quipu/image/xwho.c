/* xwho.c - who for X windows */

#ifndef	lint
static char *rcsid = "$Header: /f/osi/others/quipu/image/RCS/xwho.c,v 7.1 91/02/22 09:33:30 mrose Interim $";
#endif

/* 
 * $Header: /f/osi/others/quipu/image/RCS/xwho.c,v 7.1 91/02/22 09:33:30 mrose Interim $
 *
 *
 * $Log:	xwho.c,v $
 * Revision 7.1  91/02/22  09:33:30  mrose
 * Interim 6.8
 * 
 * Revision 7.0  89/11/23  22:00:06  mrose
 * Release 6.0
 * 
 */

/*
 *				  NOTICE
 *
 *    Acquisition, use, and distribution of this module and related
 *    materials are subject to the restrictions of a license agreement.
 *    Consult the Preface in the User's Manual for the full terms of
 *    this agreement.
 *
 */


#include <stdio.h>
#include "imagesbr.h"
#include <X11/Xlib.h>
#include <X11/Xutil.h>
#include "rwhod.h"
#include <netdb.h>
#include <sys/file.h>
#include "usr.dirent.h"


#define	NHOSTS	100

/*    DATA */

int	debug = 0;
int	errsw = 0;
static int  sleepsw = 60;

static char  *host_list[NHOSTS + 1];	/* Hosts to (not) list */
static char **host_end = host_list;
static int    dont_list = 0;

static char *myname = "xwho";


static char *display = NULL;
static char *geometry = NULL;

/*  */

typedef struct _frame {
    short x, y;
    unsigned int width, height;
    unsigned int bdrwidth;
    unsigned long border;
    unsigned long background;
}	OpaqueFrame;

static Display *DISP;
static int	SCRN;

static int    mapped;

static Window mywindow;
static OpaqueFrame myframe;

static char *fontname = "6x10";
static XFontStruct *myfont;

static int  bwidth;
static XSizeHints hints;
static XSetWindowAttributes xswattrs;
static unsigned long xswattrs_mask;

static unsigned long backpix, bdrpix;
static GC  forepix, highpix;


struct face {
    char    f_name[8 + 1];
    int	    f_active;

    int	    f_update;
    Window	f_window;
    OpaqueFrame f_frame;

    struct type_IMAGE_Image *f_imap;

    struct face *f_next;
};


struct host {
    char    h_name[32 + 1];
    int	    h_up;

    int	    h_update;
    char    h_string[32 + 2];
    Window  h_window;
    GC	    h_gc;
    int     h_ascent;
    OpaqueFrame h_frame;

    struct face *h_faces;

    struct host *h_next;
};

static int largest_h, largest_w;

static struct host *hosts;


long	time ();

/*    MAIN */

/* ARGSUSED */

main (argc, argv, envp)
int	argc;
char  **argv,
      **envp;
{
    int	    nfds;
    fd_set  rfds;

    arginit (argv);

    if (errsw)
	errsw = NOTOK;

    update_X ();
    
    FD_ZERO (&rfds);
    nfds = ConnectionNumber (DISP) + 1;
    FD_SET (ConnectionNumber (DISP), &rfds);
    for (;;) {
	fd_set	ifds;

	ifds = rfds;
	(void) xselect (nfds, &ifds, NULLFD, NULLFD, sleepsw);
	
	update_X ();
    }
}

/*    ARGINIT */

arginit (vec)
char  **vec;
{
    int	    n,
	    nhosts;
    register char  *ap,
		   *cp,
		   *lp;
    register struct hostent *hp;

    if (myname = rindex (*vec, '/'))
	myname++;
    if (myname == NULL || *myname == NULL)
	myname = *vec;

    isodetailor (myname, 1);
    lp = NULL;

    nhosts = 0;
    for (vec++; ap = *vec; vec++)
	if (*ap == '-')
	    switch (*++ap) {
		case 'd':
		    debug++;
		    break;

		case 'e': 
		    errsw++;
		    break;

		case 'l':
		    if ((lp = *++vec) == NULL)
			adios (NULLCP, "usage: %s -h local_dit", myname);
		    break;

		case 'r':
		    recording++;
		    break;

		case 's':
		    if ((ap = *++vec) == NULL
			    || sscanf (ap, "%d", &sleepsw) != 1
			    || sleepsw < 1)
			adios (NULLCP, "usage: %s -s seconds", myname);
		    break;

		case 'n':
		    dont_list++;
		    break;

		default: 
		    adios (NULLCP, "unknown switch -%s", ap);
	    }
	else
	    if (*ap == '=')
		geometry = ap;
	    else
		if ((cp = rindex (ap, ':')) && sscanf (++cp, "%d", &n) == 1)
		    display = ap;
		else {
		    if (nhosts++ >= NHOSTS)
			adios (NULLCP, "too many hosts");

		    if ((hp = gethostbyname (ap)) == NULL)
			adios (NULLCP, "%s: unknown host", ap);
		    if ((ap = malloc ((unsigned) (strlen (hp -> h_name) + 1)))
			    == NULL)
			adios (NULLCP, "out of memory");

		    (void) strcpy (*host_end++ = ap, hp -> h_name);
		}

    init_aka (myname, 1, lp);

    if (debug)
	ll_dbinit (pgm_log, myname);
    else
	ll_hdinit (pgm_log, myname);
    (void) ll_open (pgm_log);

    if ((DISP = XOpenDisplay (display)) == NULL)
	adios (NULLCP, "unable to open display \"%s\"",
		XDisplayName (display));
    SCRN = DefaultScreen (DISP);

    forepix = XCreateGC (DISP, RootWindow (DISP, SCRN), 0L, (XGCValues *)NULL);
    highpix = XCreateGC (DISP, RootWindow (DISP, SCRN), 0L, (XGCValues *)NULL);
    XCopyGC (DISP, DefaultGC (DISP, SCRN), (1L<<(GCLastBit+1)) - 1, forepix);
    XCopyGC (DISP, DefaultGC (DISP, SCRN), (1L<<(GCLastBit+1)) - 1, highpix);
    if ((cp = XGetDefault (DISP, myname, "ReverseVideo"))
	    && strcmp (cp, "on") == 0) {
	XSetForeground(DISP, forepix, WhitePixel(DISP, SCRN));
	XSetForeground(DISP, highpix, WhitePixel(DISP, SCRN));
	backpix = BlackPixel(DISP, SCRN);
	bdrpix  = WhitePixel(DISP, SCRN);
	XSetFunction(DISP, forepix, GXcopyInverted);
	XSetFunction(DISP, highpix, GXand);
    }
    else {
	XSetForeground(DISP, forepix, BlackPixel(DISP, SCRN));
	XSetForeground(DISP, highpix, BlackPixel(DISP, SCRN));
	backpix = WhitePixel(DISP, SCRN);
	bdrpix  = BlackPixel(DISP, SCRN);
	XSetFunction(DISP, forepix, GXcopy);
	XSetFunction(DISP, highpix, GXor);
    }

    XSetBackground (DISP, forepix, backpix);
    XSetBackground (DISP, highpix, backpix);

    if (cp = XGetDefault (DISP, myname, "BorderWidth"))
	bwidth = atoi (cp);
    else
	bwidth = 2;

    if (cp = XGetDefault (DISP, myname, "BodyFont"))
	fontname = cp;
    myfont = XLoadQueryFont (DISP, fontname);
}

/*    XWINDOWS */

static	update_X ()
{
    register struct host   *hp;
    register struct face   *fp;
    XGCValues gcvalues;

    service_X ();

    if (mywindow && !mapped)
	return;

    read_X ();

    layout_X ();

    if (mywindow == NULL)
	init_X ();

    for (hp = hosts; hp; hp = hp -> h_next) {
	if (hp -> h_update && display_this_host (hp -> h_name)) {
	    if (hp -> h_window) {
		XFreeGC (DISP, hp -> h_gc);
		XDestroyWindow (DISP, hp -> h_window);
	    }

	    if (debug)
		fprintf (stderr, "%s: %dx%d+%d+%d/%d\n",
			hp -> h_name, hp -> h_frame.width,
			hp -> h_frame.height, hp -> h_frame.x,
			hp -> h_frame.y, hp -> h_frame.bdrwidth);
	    hp -> h_window = XCreateSimpleWindow (DISP, mywindow,
					hp -> h_frame.x,
					hp -> h_frame.y,
					hp -> h_frame.width,
					hp -> h_frame.height,
					hp -> h_frame.bdrwidth,
					hp -> h_frame.border,
					hp -> h_frame.background);

	    XSelectInput (DISP, hp -> h_window, ExposureMask);

	    XMapWindow (DISP, hp -> h_window);

	    gcvalues.foreground = bdrpix;
	    gcvalues.background = backpix;
	    gcvalues.font = myfont -> fid;
	    hp -> h_gc = XCreateGC (DISP, hp -> h_window,
				    GCForeground | GCBackground | GCFont,
				    &gcvalues);
	}

	for (fp = hp -> h_faces; fp; fp = fp -> f_next)
	    if (fp -> f_update) {
		if (fp -> f_window)
		    XDestroyWindow (DISP, fp -> f_window);

		if (debug)
		    fprintf (stderr, "%s: %dx%d+%d+%d/%d\n",
			    fp -> f_name, fp -> f_frame.width,
			    fp -> f_frame.height, fp -> f_frame.x,
			    fp -> f_frame.y, fp -> f_frame.bdrwidth);
		fp -> f_window = XCreateSimpleWindow(DISP, mywindow,
					fp -> f_frame.x,
					fp -> f_frame.y,
					fp -> f_frame.width,
					fp -> f_frame.height,
					fp -> f_frame.bdrwidth,
					fp -> f_frame.border,
					fp -> f_frame.background);

		XSelectInput (DISP, fp -> f_window, ExposureMask);

		XMapWindow (DISP, fp -> f_window);
	    }
    }

    service_X ();
}

/*  */

static int  service_X ()
{
    int	    wh,
	    ww;
    register Window w;
    register struct face   *fp;
    register struct host   *hp;
    XEvent xevent;
    register XEvent *xe = &xevent;

    while (XPending (DISP)) {
	XNextEvent (DISP, xe);

	switch (xe -> type) {
	    case Expose: 
		if ((w = ((XExposeEvent *) xe) -> window) == mywindow) {
		    display_top ();
		    break;
		}

		for (hp = hosts; hp; hp = hp -> h_next) {
		    if (!display_this_host (hp -> h_name))
			continue;

		    if (hp -> h_window == w) {
			display_host (hp);
			break;
		    }

		    for (fp = hp -> h_faces; fp; fp = fp -> f_next)
			if (fp -> f_window == w)
			    break;
		    if (fp) {
			display_face (fp);
			break;
		    }
		}
		break;

	    case MapNotify:
		if (debug)
		    fprintf (stderr, "MapNotify\n");
		mapped = 1;
		display_top ();
		break;

	    case ConfigureNotify:
		if (debug)
		    fprintf (stderr, "ConfigureNotify %dx%d\n",
			     ((XConfigureEvent *) xe) -> height,
			     ((XConfigureEvent *) xe) -> width);
		if (((XConfigureEvent *) xe) -> window == mywindow
		    	&& (wh = ((XConfigureEvent *) xe) -> height) > 0
		        && (ww = ((XConfigureEvent *) xe) -> width) > 0)
		    myframe.height = wh, myframe.width = ww;
		break;
		
	    case UnmapNotify:
		if (debug)
		    fprintf (stderr, "UnmapNotify\n");
		mapped = 0;
		break;

	    case ReparentNotify:
		if (debug)
		    fprintf (stderr, "ReparentNotify\n");
		break;

	    default: 
		if (debug)
		    fprintf (stderr, "Event %d\n", xe -> type);
		break;
	}
    }
}

/*  */

static init_X ()
{
    char    def[BUFSIZ];

    myframe.bdrwidth = bwidth;
    myframe.height = largest_h + 100;
    if (myframe.height + bwidth * 2 > DisplayHeight (DISP, SCRN))
	myframe.height = DisplayHeight (DISP, SCRN) - bwidth * 2;
    myframe.width = largest_w + 100;
    if (myframe.width + bwidth * 2 > DisplayWidth (DISP, SCRN))
	myframe.width = DisplayWidth (DISP, SCRN) - bwidth * 2;
    myframe.x = DisplayWidth (DISP, SCRN) - (myframe.width + bwidth * 2);
    myframe.y = 0;
    (void) sprintf (def, "=%dx%d+%d+%d", myframe.width, myframe.height,
		    myframe.x, myframe.y);

    if (debug)
	fprintf (stderr, "def: %s, myframe: =%dx%d+%d+%d/%d\n", def,
		 myframe.width, myframe.height, myframe.x, myframe.y,
		 myframe.bdrwidth);

    hints.width = largest_w + 100;
    hints.height = largest_h + 100;
    hints.x = hints.y = 0;
    hints.flags = PSize | PPosition;

    xswattrs.border_pixel = bdrpix;
    xswattrs.background_pixel = backpix;
    xswattrs_mask = CWBackPixel | CWBorderPixel;

    mywindow = XCreateWindow (DISP, RootWindow (DISP, SCRN),
			      myframe.x, myframe.y,
			      myframe.width, myframe.height,
			      myframe.bdrwidth,
			      0, InputOutput, (Visual *) CopyFromParent,
			      xswattrs_mask, &xswattrs);

    XSetStandardProperties (DISP, mywindow, myname, "X Who", None,
			    (char **) 0, 0, &hints);

    XSelectInput (DISP, mywindow, ExposureMask | StructureNotifyMask);

    XMapWindow (DISP, mywindow);
    mapped = 0;
}

/*  */

static	layout_X ()
{
    int     h;
    register struct face   *fp;
    register struct host   *hp;
    XCharStruct mychar;

    h = largest_w = 0;

    for (hp = hosts; hp; hp = hp -> h_next) {
	int     hh,
	        hw;

	hh = hw = 0;
	if (hp -> h_window == NULL || hp -> h_frame.y != h) {
	    int	    direction_return,
		    font_ascent_return,
		    font_descent_return;

	    hp -> h_frame.bdrwidth = 2;
	    XTextExtents (myfont, hp -> h_string, strlen (hp -> h_string),
			  &direction_return, &font_ascent_return,
			  &font_descent_return, &mychar);
	    hp -> h_ascent = mychar.ascent;
	    hp -> h_frame.height = mychar.ascent + mychar.descent;
	    hp -> h_frame.border = backpix;
	    hp -> h_frame.background = backpix;
	    hp -> h_frame.x = 0;
	    hp -> h_frame.y = h;
	    hp -> h_update = 1;
	}
	else
	    hp -> h_update = 0;
	h += hp -> h_frame.height + 2 * hp -> h_frame.bdrwidth;

	for (fp = hp -> h_faces; fp; fp = fp -> f_next)
	    if (fp -> f_imap && fp -> f_imap -> height > hh)
		hh = fp -> f_imap -> height;

	for (fp = hp -> h_faces; fp; fp = fp -> f_next)
	    if (fp -> f_imap) {
		if (fp -> f_window == NULL
			|| fp -> f_frame.height != hh
			|| fp -> f_frame.width != fp -> f_imap -> width
			|| fp -> f_frame.x != hw
			|| fp -> f_frame.y != h) {
		    fp -> f_frame.bdrwidth = bwidth;
		    fp -> f_frame.height = hh;
		    fp -> f_frame.width = fp -> f_imap -> width;
		    fp -> f_frame.border = backpix;
		    fp -> f_frame.background = backpix;
		    fp -> f_frame.x = hw;
		    fp -> f_frame.y = h;

		    fp -> f_update = 1;
		}
		else
		    fp -> f_update = 0;

		hw += fp -> f_imap -> width + bwidth * 2;
	    }
	    else
		fp -> f_update = 0;
	if (hw > largest_w)
	    largest_w = hw;
	if (hp -> h_frame.width > largest_w)
	    largest_w = hp -> h_frame.width;

	if (hw > 0)
	    h += hh + 2;
	else {
	    h -= hp -> h_frame.height + 2 * hp -> h_frame.bdrwidth;
	    hp -> h_update = 0;
	    if (hp -> h_window) {
		XFreeGC (DISP, hp -> h_gc);
		XDestroyWindow (DISP, hp -> h_window);
		hp -> h_window = NULL;
	    }
	}
    }

    largest_h = h;
}

/*  */

static	display_top ()
{
    if (debug)
	fprintf (stderr, "top window\n");
}


static	display_host (hp)
register struct host *hp;
{
    if (debug)
	fprintf (stderr, "%s:\n", hp -> h_name);

    XDrawImageString (DISP, hp -> h_window, hp -> h_gc, 0, hp -> h_ascent,
		 hp -> h_string, strlen (hp -> h_string));
}

/*  */

static	display_face (fp)
register struct face *fp;
{
    int     sx,
            sy,
            dx,
            dy;
    unsigned int    h,
		    w;
    register struct type_IMAGE_Image *im = fp -> f_imap;
    register OpaqueFrame *xm = &fp -> f_frame;
    XImage *image;
    
    sx = max (im -> width - (int) xm -> width, 0) / 2;
    sy = max (im -> height - (int) xm -> height, 0) / 2;
    dx = max ((int) xm -> width - im -> width, 0) / 2;
    dy = max ((int) xm -> height - im -> height, 0) / 2;
    w = min (xm -> width, im -> width);
    h = min (xm -> height, im -> height);

    if (debug) {
	fprintf (stderr, "im: %dx%d frame:%dx%d\n",
		 im -> width, im -> height, xm -> width, xm -> height);
	fprintf (stderr, "sx=%d sy=%d dx=%d dy=%d w=%d h=%d\n",
		sx, sy, dx, dy, w, h);
    }

    image = XCreateImage (DISP, DefaultVisual (DISP, SCRN), 1, XYBitmap, 0,
			  im -> data -> qb_forw -> qb_data,
			  (unsigned int) im -> width,
			  (unsigned int) im -> height, 8, 0);
    if (image == NULL)
	adios (NULLCP, "XCreateImage failed");
    image -> byte_order = image -> bitmap_bit_order = LSBFirst;
    XClearWindow (DISP, fp -> f_window);
    XPutImage (DISP, fp -> f_window, forepix, image, sx, sy, dx, dy, w, h);

    XDestroyImage (image);
}

/*  */

static int facecmp (f1, f2)
struct face **f1,
	    **f2;
{
    return strcmp ((*f1) -> f_name, (*f2) -> f_name);
}


static int hostcmp (h1, h2)
struct host **h1,
	    **h2;
{
    return strcmp ((*h1) -> h_name, (*h2) -> h_name);
}


static	read_X ()
{
    int	    fd,
	    n;
    long    now;
    register struct dirent *dp;
    register struct face *fp,
			**fpp;
    register struct host *hp,
			**hpp;
    struct whod wds;
    register struct whod *wd = &wds;
    register struct whoent *we;
    static DIR *dd = NULL;

    (void) time (&now);
    for (hp = hosts; hp; hp = hp -> h_next) {
	hp -> h_up = 0;
	for (fp = hp -> h_faces; fp; fp = fp -> f_next)
	    fp -> f_active = 0;
    }
	
    if (dd == NULL) {
	if (chdir ("/usr/spool/rwho") == NOTOK)
	    adios ("/usr/spool/rwho", "unable to change directory to");

	if ((dd = opendir (".")) == NULL)
	    adios ("/usr/spool/rwho", "unable to read");
    }
    else
	rewinddir (dd);

    while (dp = readdir (dd)) {
	if (dp -> d_ino == 0 || strncmp (dp -> d_name, "whod.", 5) != 0)
	    continue;

	if ((fd = open (dp -> d_name, O_RDONLY)) == NOTOK)
	    continue;
	n = read (fd, (char *) wd, sizeof *wd);
	(void) close (fd);
	if ((n -= sizeof *wd - sizeof wd -> wd_we) < 0)
	    continue;

	if (now - wd -> wd_recvtime > 5 * 60 || n < sizeof *we)
	    continue;

	for (hp = hosts; hp; hp = hp -> h_next)
	    if (strncmp (hp -> h_name, wd -> wd_hostname,
			sizeof wd -> wd_hostname) == 0)
		break;
	if (hp == NULL) {
	    if ((hp = (struct host *) calloc (1, sizeof *hp)) == NULL)
		adios (NULLCP, "out of memory");
	    hp -> h_next = hosts;
	    hosts = hp;

	    (void) strncpy (hp -> h_name, wd -> wd_hostname,
		    sizeof wd -> wd_hostname);
	    (void) sprintf (hp -> h_string, "%s:", hp -> h_name);
	    hp -> h_frame.width = XTextWidth (myfont, hp -> h_string,
					      strlen (hp -> h_string));
	}

	hp -> h_up = 1;

	for (we = wd -> wd_we, n = n / sizeof *we; n > 0; we++, n--) {
	    if (we -> we_idle > 60 * 60)
		continue;

	    for (fp = hp -> h_faces; fp; fp = fp -> f_next)
		if (strncmp (fp -> f_name, we -> we_utmp.out_name,
			    sizeof we -> we_utmp.out_name) == 0)
		    break;
	    if (fp == NULL) {
		if ((fp = (struct face *) calloc (1, sizeof *fp)) == NULL)
		    adios (NULLCP, "out of memory");
		fp -> f_next = hp -> h_faces;
		hp -> h_faces = fp;

		(void) strncpy (fp -> f_name, we -> we_utmp.out_name,
			sizeof we -> we_utmp.out_name);

		if (display_this_host (hp -> h_name)
		        && (fp -> f_imap = fetch_image (fp -> f_name, NULLCP))
				    == NULL) {
		    if (recording)
			LLOG (pgm_log, LLOG_NOTICE,
			      ("no image for \"%s\" \"%s\"",
			       hp -> h_name, fp -> f_name));
		}
	    }

	    fp -> f_active = 1;
	}
    }

    for (hpp = &hosts; hp = *hpp;) {
	for (fpp = &hp -> h_faces; fp = *fpp;) {
	    if (!hp -> h_up || !fp -> f_active) {
		*fpp = fp -> f_next;
		if (fp -> f_window)
		    XDestroyWindow (DISP, fp -> f_window);
		free ((char *) fp);
		continue;
	    }

	    fpp = &fp -> f_next;
	}

	if (!hp -> h_up || hp -> h_faces == NULL) {
	    *hpp = hp -> h_next;
	    if (hp -> h_window) {
		XFreeGC (DISP, hp -> h_gc);
		XDestroyWindow (DISP, hp -> h_window);
	    }
	    free ((char *) hp);
	}
	else
	    hpp = &hp -> h_next;
    }

    {
	register int    i;
	register struct host **hq;

	i = 0;
	for (hp = hosts; hp; hp = hp -> h_next) {
	    register int j;
	    register struct face **fq;

	    i++, j = 0;
	    for (fp = hp -> h_faces; fp; fp = fp -> f_next)
		j++;
	    if (j > 1) {
		register struct face **faces;

		if ((faces = (struct face **)
		     		calloc ((unsigned) j, sizeof *faces)) == NULL)
		    continue;
		for (fp = hp -> h_faces, fq = faces;
			*fq = fp;
			fp = fp -> f_next, fq++)
		    continue;
		qsort ((char *) faces, j, sizeof *faces, facecmp);
		for (fq = faces, fpp = &hp -> h_faces;
		        *fpp = *fq;
		        fq++, fpp = &(*fpp) -> f_next)
		    continue;

		free ((char *) faces);
	    }
	}

	if (i > 1) {
	    register struct host **hostz;

	    if ((hostz = (struct host **)
			    calloc ((unsigned) i, sizeof *hostz)) == NULL)
		goto out;
	    for (hp = hosts, hq = hostz; *hq = hp; hp = hp -> h_next, hq++)
		continue;
	    qsort ((char *) hostz, i, sizeof *hostz, hostcmp);
	    for (hq = hostz, hpp = &hosts;
		    *hpp = *hq;
		    hq++, hpp = &(*hpp) -> h_next)
		continue;

	    free ((char *) hostz);
	}
    }

out: ;
}

/*  */

static int  display_this_host (n)
register char *n;
{
    register char **ap;

    if (host_list == host_end)
	return 1;

    for (ap = host_list; ap < host_end; ap++)
	if (strcmp (*ap, n) == 0)
	    return (!dont_list);

    return dont_list;
}
