/* xface.c - face agent for X windows */

#ifndef	lint
static char *rcsid = "$Header: /f/osi/others/quipu/image/RCS/xface.c,v 7.1 91/02/22 09:33:25 mrose Interim $";
#endif

/* 
 * $Header: /f/osi/others/quipu/image/RCS/xface.c,v 7.1 91/02/22 09:33:25 mrose Interim $
 *
 *
 * $Log:	xface.c,v $
 * Revision 7.1  91/02/22  09:33:25  mrose
 * Interim 6.8
 * 
 * Revision 7.0  89/11/23  22:00:03  mrose
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


#include <errno.h>
#include <stdio.h>
#include "imagesbr.h"
#include "internet.h"
#include <X11/Xlib.h>
#include <X11/Xutil.h>

/*    DATA */

static int  sd = NOTOK;
int	debug = 0;
int	errsw = 0;
static int	portsw = 0;
static int	ppidsw = 0;
static int	sleepsw = 30;

static char *myname = "xface";

static int	eventfd = NOTOK;
static int   (*eventfx)() = NULL;

static int   (*alarmfx)() = NULL;


static char *display = NULL;
static char *geometry = NULL;


static Display *DISP;
static int	SCRN;

static struct type_IMAGE_Image *myim = NULL;


typedef struct _frame {
    short x, y;
    unsigned int width, height;
    unsigned int bdrwidth;
    unsigned long border;
    unsigned long background;
}	Frame;


static int    mapped;
static int    parent;

static Window mywindow = 0;
static Frame myframe;


static unsigned long backpix, bdrpix;
static GC  forepix, highpix;

int	ALRMser (), XWINser ();


extern int  errno;

char   *getenv ();

/*    MAIN */

/* ARGSUSED */

main (argc, argv, envp)
int	argc;
char  **argv,
      **envp;
{
    char    buffer[BUFSIZ],
	   *vec[NVEC + 1];

    arginit (argv);

    if (portsw > 0)
	startsocket (portsw);

    if (ppidsw > 0)
	envinit ();

    if (errsw)
	errsw = NOTOK;

    for (;;) {
	if ((portsw > 0 ? readsocket (buffer) : getline (buffer)) == NOTOK)
	    break;

	if (str2vec (buffer, vec) != 2)
	    continue;

	fetch_face (vec[0], vec[1]);

	if (debug)
	    (void) fflush (stderr);
    }

    exit (0);
}

/*  */

static	fetch_face (host, user)
char   *host,
       *user;
{
    if ((myim = fetch_image (user, host)) == NULL && recording)
	LLOG (pgm_log, LLOG_NOTICE,
	      ("no image for \"%s\" \"%s\"", user, host));

    if (mywindow != NULL || myim)
	display_X ();
}

/*  */

static int  getline (buffer)
char   *buffer;
{
    register int    i;
    register char  *cp,
                   *ep;
    static int  sticky = 0;

    if (sticky) {
	sticky = 0;
	return NOTOK;
    }

    printf ("%s> ", myname);
    (void) fflush (stdout);

    for (ep = (cp = buffer) + BUFSIZ - 1; (i = getchar ()) != '\n';) {
	if (i == EOF) {
	    printf ("\n");
	    if (cp != buffer) {
		sticky++;
		break;
	    }

	    return NOTOK;
	}

	if (cp < ep)
	    *cp++ = i;
    }
    *cp = NULL;

    return OK;
}

/*    ARGINIT */

static	arginit (vec)
char  **vec;
{
    int	    n;
    register char  *ap,
		   *cp;
    
    if (myname = rindex (*vec, '/'))
	myname++;
    if (myname == NULL || *myname == NULL)
	myname = *vec;

    isodetailor (myname, 1);
    init_aka (myname, 0, NULLCP);

    if ((ap = getenv ("FACEPROC"))
	    && (cp = index (ap, ' '))
	    && sscanf (++cp, "%d", &n) == 1
	    && n >= 1)
	portsw = n;

    for (vec++; ap = *vec; vec++)
	if (*ap == '-')
	    switch (*++ap) {
		case 'd':
		    debug++;
		    break;

		case 'e': 
		    errsw++;
		    break;

		case 'p':
		    if ((ap = *++vec) == NULL
			    || sscanf (ap, "%d", &portsw) != 1
			    || portsw < 1)
			adios (NULLCP, "usage: %s -p port", myname);
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

		case 'u': 
		    ppidsw = getppid ();
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
		else
		    adios (NULLCP, "usage: %s [switches] host:display",
			   myname);
    
    if (debug)
	ll_dbinit (pgm_log, myname);
    else
	ll_hdinit (pgm_log, myname);

    if ((DISP = XOpenDisplay (display)) == NULL)
	adios (NULLCP, "unable to open display \"%s\"",
		XDisplayName (display));
    SCRN = DefaultScreen (DISP);
}

/*  */

static	envinit ()
{
    int     i,
            pid;

    if (debug)
	return;

    for (i = 0; (pid = fork ()) == NOTOK && i < 5; i++)
	sleep (5);
    switch (pid) {
	case NOTOK: 
	case OK: 
	    break;

	default: 
	    exit (0);
    }

    ll_hdinit (pgm_log, myname);
}

/*  */

static	display_X ()
{
    if (mywindow == NULL) {
	int	bwidth;
	char   *opt,
		def[BUFSIZ];
	XSizeHints hints;
	XSetWindowAttributes xswattrs;
	unsigned long xswattrs_mask;

	forepix = XCreateGC (DISP, RootWindow (DISP, SCRN), 0L,
			     (XGCValues *) NULL);
	highpix = XCreateGC (DISP, RootWindow (DISP, SCRN), 0L,
			     (XGCValues *) NULL);
	XCopyGC (DISP, DefaultGC (DISP, SCRN), (1L<<(GCLastBit+1)) - 1,
		 forepix);
	XCopyGC (DISP, DefaultGC (DISP, SCRN), (1L<<(GCLastBit+1)) - 1,
		 highpix);
	if ((opt = XGetDefault (DISP, myname, "ReverseVideo"))
		&& strcmp (opt, "on") == 0) {
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

	if (opt = XGetDefault (DISP, myname, "BorderWidth"))
	    bwidth = atoi (opt);
	else
	    bwidth = 2;

	myframe.bdrwidth = bwidth;
	myframe.height = myim -> height;
	if (myframe.height + bwidth * 2 > DisplayHeight (DISP, SCRN))
	    myframe.height = DisplayHeight (DISP, SCRN) - bwidth * 2;
	myframe.width = myim -> width;
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

	hints.width = myim -> width;
	hints.height = myim -> height;
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

	XSetStandardProperties (DISP, mywindow, myname, "Face Agent", None,
				(char **) 0, 0, &hints);

	XSelectInput (DISP, mywindow, ExposureMask | StructureNotifyMask);

	XMapWindow (DISP, mywindow);
	mapped = parent = 0;
    }
    else
	Redisplay ();

    eventfd = ConnectionNumber (DISP);
    eventfx = XWINser;
    alarmfx = ALRMser;

    XWINser (0);
}

/*  */

static	Redisplay ()
{
    int     sx,
            sy,
            dx,
            dy;
    unsigned int    h,
		    w;
    XImage *image;

    if (myim == NULL) {
	XClearWindow (DISP, mywindow);
	return;
    }

    sx = max (myim -> width - (int) myframe.width, 0) / 2;
    sy = max (myim -> height - (int) myframe.height, 0) / 2;
    dx = max ((int) myframe.width - myim -> width, 0) / 2;
    dy = max ((int) myframe.height - myim -> height, 0) / 2;
    w = min (myframe.width, myim -> width);
    h = min (myframe.height, myim -> height);

    if (debug) {
	fprintf (stderr, "im: %dx%d frame:%dx%d\n",
		 myim -> width, myim -> height, myframe.width, myframe.height);
	fprintf (stderr, "sx=%d sy=%d dx=%d dy=%d w=%d h=%d\n",
		sx, sy, dx, dy, w, h);
    }

    image = XCreateImage (DISP, DefaultVisual (DISP, SCRN), 1, XYBitmap, 0,
			  myim -> data -> qb_forw -> qb_data,
			  (unsigned int) myim -> width,
			  (unsigned int) myim -> height, 8, 0);
    if (image == NULL)
	adios (NULLCP, "XCreateImage failed");
    image -> byte_order = image -> bitmap_bit_order = LSBFirst;
    XClearWindow (DISP, mywindow);
    XPutImage (DISP, mywindow, forepix, image, sx, sy, dx, dy, w, h);

    XDestroyImage (image);
}

/*  */

static int  ALRMser ()
{
    if (mywindow && mapped) {
	if (parent)
	    XClearWindow (DISP, mywindow);
	else
	    XUnmapWindow (DISP, mywindow);
    }

    if (myim)
	myim = NULL;
}

/*  */

/* ARGSUSED */

static int  XWINser (io)
int	io;
{
    int	    ww,
	    wh;
    XEvent   xevent;
    register XEvent *xe = &xevent;

    while (XPending (DISP)) {
	XNextEvent (DISP, xe);

	switch (xe -> type) {
	    case Expose:
		if (debug)
		    fprintf (stderr, "Expose %d\n",
			     ((XExposeEvent *) xe) -> count);
		if (myim) {
		    if (((XExposeEvent *) xe) -> count > 0)
			break;
		    mapped = 1;
		    Redisplay ();
		}
		else {
unmap: ;
		    if (parent)
			XClearWindow (DISP, mywindow);
		    else
			XUnmapWindow (DISP, mywindow);
		}
		break;

	    case MapNotify:
		if (debug)
		    fprintf (stderr, "MapNotify (0x%x)\n",
			     myim);
		if (myim) {
		    mapped = 1;
		    Redisplay ();
		}
		else
		    goto unmap;
		break;

	    case ConfigureNotify:
		if (debug)
		    fprintf (stderr, "ConfigureNotify %dx%d\n",
			     ((XConfigureEvent *) xe) -> height,
			     ((XConfigureEvent *) xe) -> width);
		if ((wh = ((XConfigureEvent *) xe) -> height) > 0
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
		parent = 1;
		break;

	    default:
		if (debug)
		    fprintf (stderr, "Event %d\n", xe -> type);
		break;
	}
    }
}

/*    SOCKET */

int	startsocket (portno)
int	portno;
{
    struct sockaddr_in in_socket;
    register struct sockaddr_in *isock = &in_socket;

    isock -> sin_family = AF_INET;
    isock -> sin_port = htons ((u_short) portno);
    isock -> sin_addr.s_addr = INADDR_ANY;

    if ((sd = socket (AF_INET, SOCK_DGRAM, 0)) == NOTOK)
	adios ("socket", "unable to create");

    if (bind (sd, (struct sockaddr *) isock, sizeof *isock) == NOTOK)
	adios ("socket", "unable to bind");
}

/*  */

int	readsocket (buffer)
char   *buffer;
{
    int     cc;

    for (;;) {
	int     i,
		nfds;
	fd_set	imask;
	struct sockaddr_in  in_socket;
	struct sockaddr_in *isock = &in_socket;
	
	FD_ZERO (&imask);

	nfds = sd + 1;
	FD_SET (sd, &imask);
	if (eventfd != NOTOK) {
	    (*eventfx) (0);

	    if (eventfd >= nfds)
		nfds = eventfd + 1;
	    FD_SET (eventfd, &imask);
	}

	if (xselect (nfds, &imask, NULLFD, NULLFD, sleepsw) <= 0) {
	    if (errno == EINTR)
		continue;

	    if (ppidsw > 0 && kill (ppidsw, 0) == NOTOK) {
		(void) close (sd);
		return NOTOK;
	    }

	    if (alarmfx)
		(*alarmfx) ();

	    continue;
	}

	if (ppidsw > 0 && kill (ppidsw, 0) == NOTOK) {
	    (void) close (sd);
	    return NOTOK;
	}

	if (eventfd != NOTOK && FD_ISSET (eventfd, &imask))
	    (*eventfx) (1);

	if (!FD_ISSET (sd, &imask))
	    continue;

	i = sizeof *isock;
	if ((cc = recvfrom (sd, buffer, BUFSIZ, 0, (struct sockaddr *) isock,
		&i)) == NOTOK) {
	    if (errno == EINTR)
		continue;
	    adios ("failed", "recvfrom socket");
	}

	break;
    }

    buffer[cc] = NULL;

    return OK;
}
