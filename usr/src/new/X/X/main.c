#include <X/mit-copyright.h>

/* Copyright    Massachusetts Institute of Technology    1985, 1986	*/

/* Initialization and socket routines */

#ifndef lint
static char *rcsid_main_c = "$Header: main.c,v 10.17 86/11/06 19:42:02 jg Rel $";
#endif

#include <dbm.h>
#undef NULL
#include "Xint.h"
#include <errno.h>
#include <sys/socket.h>
#include <sys/time.h>
#include <fcntl.h>
#include <signal.h>
#include "rgb.h"
#include <netinet/in.h>
#include <sys/un.h>
#ifdef DNETCONN
#include <netdnet/dn.h>
#endif

extern int errno;
extern int sys_nerr;
extern char *sys_errlist[];
extern CURSOR *cursor;
extern short base_feep;
extern WINDOW *rootwindow;

int HangUp();
char *Xalloc(), *strcpy(), *strcat();
PIXMAP *PixmapSave(), *MakePixmap();
CURSOR *StoreCursor();
BITMAP *StoreBitmap();

static char *devdisp;			/* The display device */
static char *display;			/* The display number */
static int devdesc;			/* The display file descriptor */
DEVICE device;				/* The display info */
static int Qmax;			/* device.queue->size - 1 */
static int (*inputhandler)() = NULL;	/* input processor */
static int (*blockhandler)() = NULL;	/* called before select() */
static int (*wakeuphandler)() = NULL;	/* called after select() */

char *rgb_name = RGB_DB;	/* RGB database name */
int havergb = 0;		/* have RGB database? */

static short _back[16] = {0x8888, 0x2222, 0x4444, 0x1111,
			  0x8888, 0x2222, 0x4444, 0x1111,
			  0x8888, 0x2222, 0x4444, 0x1111,
			  0x8888, 0x2222, 0x4444, 0x1111};

PIXMAP *roottile;

static short _pointer[16] = {0xf00f, 0x8811, 0x8421, 0x8241,
			     0x4182, 0x2004, 0x1008, 0x0810,
			     0x0810, 0x1008, 0x2004, 0x4182,
			     0x8241, 0x8421, 0x8811, 0xf00f};

static short _ptrmask[16] = {0xf00f, 0xf81f, 0xfc3f, 0xfe7f,
			     0x7ffe, 0x3ffc, 0x1ff8, 0x0ff0,
			     0x0ff0, 0x1ff8, 0x3ffc, 0x7ffe,
			     0xfe7f, 0xfc3f, 0xf81f, 0xf00f};

static short _ssmask[16] = {0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0};

CURSOR *rootcursor;		/* background cursor */
static CURSOR *sscursor;	/* screen saver cursor */

static short default_click = 6;		/* KeyClick */
static short default_repeat = 1;	/* AutoRepeat */
static short default_lock = 1;		/* ShiftLock */
static short default_feep = 3;		/* FeepControl */
static short default_threshold = 2;	/* MouseControl */
static short default_acceleration = 4;
static short default_savertime = 10;	/* ScreenSaver */
static short default_savercycle = 60;
static char *default_pix0 = NULL;	/* Background */
static char *default_pix1 = NULL;	/* Foreground */
static short default_mono = 0;		/* Force monochrome */
static short default_blank = 0;		/* Blank video preferred */

static ColorDef basecolors[2] = {{BlackPixel, 0, 0, 0x8000},
				 {WhitePixel, 0xff00, 0xff00, 0xff00}};
static ColorDef randomcolors[2] = {{BlackPixel, 0, 0, 0},
				   {WhitePixel, 0, 0, 0}};

static int socketOn[maxsocks];	/* 1: active, 0: inactive */
int requestId[maxsocks];	/* Request count associated with the socket */
static char *sockbuf[maxsocks];	/* Request buffers */
char *bufptr[maxsocks];		/* Buffer pointers */
				/* Pointer to start of data in sockbuf */
int bufcnt[maxsocks];		/* Buffer counts */
				/* Count of data bytes available in sockbuf */
#define DEFBUFSIZE 4096		/* Default buffer size */
#define MAXBUFSIZE (1<<17)	/* Maximum buffer size */
static int bufsize[maxsocks];	/* Buffer sizes */
				/* Current size in bytes of sockbuf buffer */
#ifdef DUALTCP
int swapped[maxsocks];		/* 1: must byte swap on connection */
#endif
static int lastfdesc;		/* Maximum file descriptor */
static int maxsock;		/* Highest socket in use + 1 */
static int firstsock;		/* First possible socket */
static int firstmask;		/* First possible socket mask */
#ifdef DUALTCP
static int swapmask;		/* Swap ports */
#endif
static int requestmask;		/* Listener mask */
static int selmask[mskcnt];	/* Mask of open connections */
int havemask[mskcnt];		/* Connections with buffered requests */
int havestate;			/* <0: many, 0: none, >0: single */
static int servergrabber;	/* Grabbing client */
static int grabselmask[mskcnt];	/* Saved mask during GrabServer */
static int grabhavemask[mskcnt];/* Saved mask during GrabServer */
static int grabhavestate;	/* Saved state during GrabServer */
static int devmask;		/* Event mask */
int rr_counter;			/* Round robin counter */
struct timeval waittime = {0, 0};
struct timeval longtime = {0, 0};
#define OUTTIME 10		/* 10 seconds */
static struct timeval outtime = {OUTTIME, 0};
static struct timeval notime = {0, 0};
static unsigned errdelay = 15;
short blank_video;

main(argc, argv)
	int argc;
	char **argv;
{
	register int i;
	register char *arg;

	if (argc < 3)
	    Usage ();
	devdisp = display = argv[1];
	if (*display == '/')
	    display = "0";
	argc--;		/* ignore tty name */
	for (i = 2; i < argc; ) {
	    arg = argv[i];
	    i++;
	    if (strcmp (arg, "-a") == 0 && i < argc) {
		default_acceleration = atoi (argv[i]);
		i++;
		if (default_acceleration == 0) Usage ();
	    } else if (strcmp (arg, "-c") == 0) {
		default_click = 0;
	    } else if (strcmp (arg, "c") == 0 && i < argc) {
		default_click = atoi (argv[i]);
		i++;
		if (default_click > 8) Usage ();
	    } else if (strcmp (arg, "-f") == 0 && i < argc) {
		default_feep = atoi (argv[i]);
		i++;
		if (default_feep > 7) Usage ();
	    } else if (strcmp (arg, "-l") == 0) {
		default_lock = 0;
	    } else if (strcmp (arg, "l") == 0) {
		default_lock = 1;
	    } else if (strcmp (arg, "m") == 0) {
		default_mono = 1;
	    } else if (strcmp (arg, "-p") == 0 && i < argc) {
		default_savercycle = atoi (argv[i]);
		if (default_savercycle == 0) Usage ();
		i++;
	    } else if (strcmp (arg, "-r") == 0) {
		default_repeat = 0;
	    } else if (strcmp (arg, "r") == 0) {
		default_repeat = 1;
	    } else if (strcmp (arg, "-s") == 0 && i < argc) {
		default_savertime = atoi (argv[i]);
		if (default_savertime == 0) Usage ();
		i++;
	    } else if (strcmp (arg, "-t") == 0 && i < argc) {
		default_threshold = atoi (argv[i]);
		i++;
	    } else if (strcmp (arg, "-v") == 0) {
		default_blank = 1;
	    } else if (strcmp (arg, "v") == 0) {
		default_blank = 0;
	    } else if (strcmp (arg, "-0") == 0 && i < argc) {
		default_pix0 = argv[i];
		i++;
	    } else if (strcmp (arg, "-1") == 0 && i < argc) {
		default_pix1 = argv[i];
		i++;
	    } else if (strcmp (arg, "-D") == 0 && i < argc) {
		rgb_name = argv[i];
		i++;
	    } else {
		Usage ();
	    }
	}
	Initialize ();
	Dispatcher ();
}

Usage ()
{
	printf("usage:  X <display> [option ...] <tty>\n");
	printf("options: -a #, c #, -c, -f #, -l, l, m, -p #, -r, r, -s #, -t #, v, -v\n");
	printf("         -0 <color> -1 <color> -D <rgbdb>\n");
	exit(1);
}

/* Called from Dispatcher when there are no complete requests to process, or
 * when multiple sockets have requests, or when there is input to be read.
 */

Receive ()
{
	int mask[mskcnt];
	register int i, maskval, rrmask;
	register vsEventQueue *queue = device.queue;
	register vsEvent *ev;
	int mi, numread;
	union {
	    struct sockaddr sa;
#ifdef UNIXCONN
	    struct sockaddr_un un;
#endif
#ifdef TCPCONN
	    struct sockaddr_in in;
#endif
#ifdef DNETCONN
	    struct sockaddr_dn dn;
#endif
	} from;
	int fromlen;

	while (1) {

	    /* If there is input, deal with it */

	    while (queue->head != queue->tail) {
		ev = &queue->events[queue->head];
		switch (ev->vse_type) {
		    case VSE_MMOTION:	/* The mouse moved */
			Deal_with_movement ();
			break;
		    case VSE_BUTTON:	/* A key/button moved */
			ProcessInput (ev);
			break;
		}
		if (queue->head == Qmax)
		    queue->head = 0;
		else
		    queue->head++;
	    }

	    /* If we already have data available, don't try to read more */

	    if (havestate)
		break;

	    /* Wait for input or requests */

	    if (blockhandler) (*blockhandler)();
	    copybits(selmask, mask);
	    i = select (maxsock, mask, (int *) NULL, (int *) NULL, &waittime);
	    if (wakeuphandler) (*wakeuphandler)();
	    if (i <= 0) {
		/* A timeout or error occurred */
		if (!i)
		    Screen_saver ();
		else if (errno == EBADF)
		    Check_connections ();
		continue;
	    }

	    /* Check if only events */

	    if (mask[0] & devmask) {
		mask[0] &= ~devmask;
		if (inputhandler) (*inputhandler)();
		if (i == 1) continue;
	    }

	    /* If request socket, deal with the new request */

	    while (rrmask = (mask[0] & requestmask)) {
		i = ffs (rrmask) - 1;
		rrmask = 1 << i;
		mask[0] &= ~rrmask;
		if ((i = accept (i, (struct sockaddr *) NULL, (int *) NULL)) >= 0) {
		    fromlen = sizeof (from);
		    if (i >= lastfdesc ||
			getpeername (i, &from.sa, &fromlen) ||
			Invalid_host (&from.sa, fromlen)) {
			close (i);
		    } else {
#ifdef TCP_NODELAY
			if (fromlen && (from.sa.sa_family == AF_INET)) {
			    mi = 1;
			    setsockopt (i, IPPROTO_TCP, TCP_NODELAY,
					&mi, sizeof (int));
			}
#endif
			fcntl (i, F_SETFL, FNDELAY);
			socketOn[i] = 1;
			requestId[i] = 0;
			if (servergrabber) {
			    bitset(grabselmask, i);
			} else {
			    bitset(selmask, i);
			}
			bufptr[i] = sockbuf[i] = Xalloc (DEFBUFSIZE);
			bufcnt[i] = 0;
			bufsize[i] = DEFBUFSIZE;
			if (i >= maxsock)
			    maxsock = i + 1;
#ifdef DUALTCP
			if (rrmask & swapmask)
			    swapped[i] = 1;
#endif
		    }
		}
	    }

	    /* Read from all ready sockets */

	    mi = 0;
	    i = firstsock;
	    rrmask = firstmask;
	    maskval = mask[0];
	    while (1) {
		if (!maskval) {
		    if (mi == (mskcnt - 1))
			break;
		    mi++;
		    i = mi << 5;
		    rrmask = 1;
		    maskval = mask[mi];
		    continue;
		}
		while (!(maskval & rrmask)) {
		    rrmask += rrmask;
		    i++;
		}
		maskval &= ~rrmask;
		/* copy down any existing data */
		if (bufcnt[i] && (bufptr[i] != sockbuf[i]))
		    bcopy (bufptr[i], sockbuf[i], bufcnt[i]);

		/* then read as much as we can */
		bufptr[i] = sockbuf[i];
		if ((numread = read (i, bufptr[i] + bufcnt[i],
				     bufsize[i] - bufcnt[i])) <= 0)
		    Close_down (i);
		/* see if we have enough for a request */
		else if ((bufcnt[i] += numread) >= sizeof (XReq)) {
		    havemask[mi] |= rrmask;
		    if (havestate == 0)
			havestate = i;
		    else if (havestate > 0)
			havestate = -2;
		    else
			havestate--;
		}
		rrmask += rrmask;
		i++;
	    }
	}

	if (havestate > 0) {
	    rr_counter = havestate;
	    return;
	}

	/* Handle multiple requests on a round-robin basis */

	i = rr_counter + 1;
	while (1) {
	    if (i >= maxsock)
		i = firstsock;
	    rrmask = bitmask(i);
	    if (maskval = (maskword(havemask, i) & -rrmask))
		break;
	    i += 32;
	    i &= ~31;
	}

	while (!(maskval & rrmask)) {
	    rrmask += rrmask;
	    i++;
	}

	rr_counter = i;
	if (havestate == -1)
	    havestate = i;
}

/* Write data to client.
 * We might have to wait, if the client isn't keeping up with us.  We wait for
 * a short time, then close the connection.  This isn't a wonderful solution,
 * but it rarely seems to be a problem right now, and buffering output for
 * asynchronous delivery sounds complicated and expensive.
 */

Write (client, buf, total)
	int client, total;
	char *buf;
{
	int count = total;
	register int n;
	int mask[mskcnt];

	while (1) {
	    if ((n = write (client, buf, count)) == total)
		return;
	    if (n > 0) {
		buf += n;
		total -= n;
		if (total < count)
		    count = total;
	    } else if (errno == EMSGSIZE)
		count >>= 1;
	    else if (errno != EWOULDBLOCK)
		return;
	    if (blockhandler) (*blockhandler)();
	    singlebit(mask, client);
	    n = select (client + 1, (int *) NULL, mask, (int *) NULL, &outtime);
	    if (wakeuphandler) (*wakeuphandler)();
	    if (n != 1) {
		close (client);
		return;
	    }
	}
}

/* Read data from client.
 * Returns NULL if the data isn't immediately available, and backs up the
 * buffer structures to re-read the request.
 */

caddr_t Read_segment (client, size)
	register int client;
	register int size;
{
	register int idx, mask;
	char *ptr;

	if (bufcnt[client] >= size) {
	    ptr = bufptr[client];
	    bufptr[client] += size;
	    /* see if there is a request left */
	    if ((bufcnt[client] -= size) < sizeof (XReq)) {
		idx = maskidx(client);
		mask = bitmask(client);
		if (havemask[idx] & mask) {
		    havemask[idx] &= ~mask;
		    if (havestate < 0)
			havestate++;
		    else
			havestate = 0;
		}
	    }
	    return (ptr);
	}

	/* back up to the request */
	bufptr[client] -= sizeof (XReq);
	bufcnt[client] += sizeof (XReq);
	requestId[client]--;
#ifdef DUALTCP
	if (swapped[client])
	    Swap_request ((XReq *) bufptr[client]);
#endif
	/* but make it look like not enough is there */
	idx = maskidx(client);
	mask = bitmask(client);
	if (havemask[idx] & mask) {
	    havemask[idx] &= ~mask;
	    if (havestate < 0)
		havestate++;
	    else
		havestate = 0;
	}
	if (size + sizeof (XReq) > bufsize[client]) {
	    /* must increase the buffer to accomodate what's coming */
	    if (size <= MAXBUFSIZE) {
		ptr = Xalloc (bufsize[client] = size + sizeof (XReq));
		bcopy (bufptr[client], ptr, bufcnt[client]);
		free (sockbuf[client]);
		sockbuf[client] = bufptr[client] = ptr;
	    } else
		Close_down (client);
	}
	return (NULL);
}

/* Give client sole access to server */

Grab_server (client)
	register int client;
{
	register int idx, mask;

	if (servergrabber == 0) {
	    copybits(selmask, grabselmask);
	    clearbits(selmask);
	    selmask[0] = devmask | requestmask;
	    idx = maskidx(client);
	    mask = bitmask(client);
	    selmask[idx] |= mask;
	    copybits(havemask, grabhavemask);
	    clearbits(havemask);
	    grabhavestate = havestate;
	    if (grabhavemask[idx] & mask) {
		grabhavemask[idx] &= ~mask;
		havemask[idx] = mask;
		havestate = client;
		if (grabhavestate < 0)
		    grabhavestate++;
		else
		    grabhavestate = 0;
	    } else
		havestate = 0;
	    servergrabber = client;
	}
}

/* Restore global access to server */

Ungrab_server ()
{
	if (servergrabber) {
	    copybits(grabselmask, selmask);
	    if (havestate) {
		bitset(grabhavemask, servergrabber);
		if (grabhavestate == 0)
		    grabhavestate = servergrabber;
		else if (grabhavestate > 0)
		    grabhavestate = -2;
		else
		    grabhavestate--;
	    }
	    havestate = grabhavestate;
	    copybits(grabhavemask, havemask);
	    servergrabber = 0;
	}
}

/* Blank the screen when the server has been idle for a while */

Screen_saver ()
{
	int mask[mskcnt];
	PIXMAP *save = NULL;
	int video = -1;
	struct timeval tv;
	struct timezone tz;
	int n;

	/* zap the cursor */
	LoadCursor (sscursor);
	/* make sure mouse motion wakes us up */
	device.mbox->bottom = 0;
	if (blank_video)
	    video = SetVideo (0);
	if (!blank_video || video)
	    save = PixmapSave (0, 0, device.width, device.height);
	if (!blank_video && save == NULL)
	    video = SetVideo (0);
	do {
	    if (video) {
		/* randomize the pattern */
		gettimeofday (&tv, &tz);
		if (device.entries > 2) {
		    randomcolors[0].red = tv.tv_sec & 0x4 ? -1 : 0;
		    randomcolors[0].green = tv.tv_sec & 0x2 ? -1 : 0;
		    randomcolors[0].blue = tv.tv_sec & 0x1 ? -1 : 0;
		    randomcolors[1].red = tv.tv_usec & 0x40000 ? -1 : 0;
		    randomcolors[1].green = tv.tv_usec & 0x20000 ? -1 : 0;
		    randomcolors[1].blue = tv.tv_usec & 0x10000 ? -1 : 0;
		    StoreColors (2, randomcolors);
		}
		TileFill (rootwindow->tile,
			  (int) (tv.tv_sec >> 4) & 0xf, (int) tv.tv_sec & 0xf,
			  (BITMAP *) NULL, 0, 0, device.width, device.height,
			  &rootwindow->clip, 1, GXcopy, -1);
	    }
	    if (blockhandler) (*blockhandler)();
	    copybits(selmask, mask);
	    n = select (maxsock, mask, (int *) NULL, (int *) NULL, &longtime);
	    if (wakeuphandler) (*wakeuphandler)();
	} while (n == 0);
	/* now restore the world */
	if (video && device.entries > 2) {
	    Query_color (randomcolors[0].pixel, &randomcolors[0].red,
			 &randomcolors[0].green, &randomcolors[0].blue);
	    Query_color (randomcolors[1].pixel, &randomcolors[1].red,
			 &randomcolors[1].green, &randomcolors[1].blue);
	    StoreColors (2, randomcolors);
	}
	if (save) {
	    PixmapPut (save, 0, 0, device.width, device.height, 0, 0,
		       &rootwindow->clip, 1, GXcopy, -1);
	    FreePixmap (save);
	} else if (video)
	    Draw_window (rootwindow, 1, 1);
	else
	    SetVideo (1);
	LoadCursor (cursor);
}

/* Check the status of each client, and close our side of any goners. */

Check_connections ()
{
	register int i;
	int mask[mskcnt];

	for (i = firstsock; i < maxsock; i++) {
	    if (!socketOn[i])
		continue;
	    singlebit(mask, i);
	    if (select (i + 1, mask, (int *) NULL, (int *) NULL, &notime) < 0)
		Close_down (i);
	}
}

/* Close down a client, freeing its resources. */

Close_down (i)
	register int i;
{
	register int idx, mask;

	if (servergrabber)
	    Ungrab_server ();
	idx = maskidx(i);
	mask = bitmask(i);
	selmask[idx] &= ~mask;
	if (havemask[idx] & mask) {
	    havemask[idx] &= ~mask;
	    if (havestate < 0)
		havestate++;
	    else
		havestate = 0;
	}
	close (i);
	socketOn[i] = 0;
	if (i + 1 == maxsock) {
	    while ((maxsock > firstsock) && (!socketOn[maxsock - 1]))
		--maxsock;
	}
	free (sockbuf[i]);
#ifdef DUALTCP
	swapped[i] = 0;
#endif
	Free_client_resources (i);
	Deal_with_movement ();
	/* if nobody is left, clean up after them */
	if (maxsock == firstsock)
	    Restore_root ();
}

/* This is where we create the background, etc. */

Initialize ()
{
	char fname[32];
	int request, temp;
#ifdef TCPCONN
	int whichbyte;
	int tcpport1;
#ifdef DUALTCP
	int tcpport2;
#endif
	struct sockaddr_in mysock;
#ifndef SO_DONTLINGER
	static int linger[2] = { 0, 0 };
#endif
#endif
#ifdef UNIXCONN
	struct sockaddr_un unsock;
#endif
#ifdef DNETCONN
	struct sockaddr_dn dnsock;
#endif
	int retry;
	BITMAP *bit1, *bit2;
	datum dbent;
	RGB color;

	lastfdesc = getdtablesize() - 1;
	if (lastfdesc > maxsocks)
	    lastfdesc = maxsocks;
	/* hack test to decide where to log errors */
	if (write (2, fname, 0)) {
	    strcpy (fname, "/usr/adm/X");
	    strcat (fname, display);
	    strcat (fname, "msgs");
	    freopen (fname, "w+", stderr);
	}
	if (getpgrp (0) == 0)
	    setpgrp (0, getpid ());
	if (dbminit (rgb_name) == 0)
	    havergb = 1;

	requestmask = 0;
#ifdef DUALTCP
	swapmask = 0;
#endif

#ifdef TCPCONN
	tcpport1 = atoi (display);
	whichbyte = 1;
	if (*(char *) &whichbyte) {
#ifdef DUALTCP
	    tcpport2 = tcpport1 + X_TCP_BI_PORT;
#endif
	    tcpport1 += X_TCP_LI_PORT;
	} else {
#ifdef DUALTCP
	    tcpport2 = tcpport1 + X_TCP_LI_PORT;
#endif
	    tcpport1 += X_TCP_BI_PORT;
	}
#endif
	if ((devdesc = OpenDisplay (devdisp)) < 0)
	    Error ("Opening");

	signal (SIGPIPE, SIG_IGN);
#ifdef TCPCONN
	if ((temp = socket (AF_INET, SOCK_STREAM, 0)) < 0) {
	    Notice ("Creating TCP socket");
	} else {
	    request = temp;
	    bzero ((char *)&mysock, sizeof (mysock));
	    mysock.sin_family = AF_INET;
	    mysock.sin_port = htons (tcpport1);
	    mysock.sin_addr.s_addr = htonl(INADDR_ANY);
	    retry = 20;
	    while (bind (request, (struct sockaddr *) &mysock,
			 sizeof (mysock))) {
		if (--retry == 0)
		    Error ("Binding TCP socket");
		sleep (10);
	    }
#ifdef SO_DONTLINGER
	    if (setsockopt (request, SOL_SOCKET, SO_DONTLINGER, 0, 0))
#else
	    if (setsockopt (request, SOL_SOCKET, SO_LINGER,
			    linger, sizeof (linger)))
#endif
		Notice ("Setting TCP DONTLINGER");
	    if (listen (request, 5)) Error ("TCP Listening");
	    requestmask |= (1 << request);
	    Define_self (request);
	}
#ifdef DUALTCP
	if ((temp = socket (AF_INET, SOCK_STREAM, 0)) < 0) {
	    Notice ("Creating dual TCP socket");
	} else {
	    request = temp;
	    mysock.sin_port = htons (tcpport2);
	    retry = 20;
	    while (bind (request, (struct sockaddr *) &mysock,
			 sizeof (mysock))) {
		if (--retry == 0)
		    Error ("Binding dual TCP socket");
		sleep (10);
	    }
#ifdef SO_DONTLINGER
	    if (setsockopt (request, SOL_SOCKET, SO_DONTLINGER, 0, 0))
#else
	    if (setsockopt (request, SOL_SOCKET, SO_LINGER,
			    linger, sizeof (linger)))
#endif
		Notice ("Setting dual TCP DONTLINGER");
	    if (listen (request, 5)) Error ("dual TCP Listening");
	    requestmask |= (1 << request);
	    swapmask |= (1 << request);
	    Define_self (request);
	}
#endif
#endif
#ifdef UNIXCONN
	unsock.sun_family = AF_UNIX;
	strcpy (unsock.sun_path, X_UNIX_PATH);
	strcat (unsock.sun_path, display);
	unlink (unsock.sun_path);
	if ((temp = socket (AF_UNIX, SOCK_STREAM, 0)) < 0) {
	    Notice ("Creating Unix socket");
	} else {
	    request = temp;
	    if (bind (request, (struct sockaddr *) &unsock,
		      strlen (unsock.sun_path) + 2))
		Error ("Binding Unix socket");
	    if(chmod(unsock.sun_path, 0666))
		Error ("Setting Mode on Unix socket");
	    if (listen (request, 5)) Error ("Unix Listening");
	    requestmask |= (1 << request);
	}
#endif
#ifdef DNETCONN
	if ((temp = socket (AF_DECnet, SOCK_STREAM, 0)) < 0) {
	    Notice ("Creating DECnet socket");
	} else {
	    request = temp;
	    bzero ((char *)&dnsock, sizeof (dnsock));
	    dnsock.sdn_family = AF_DECnet;
	    sprintf(dnsock.sdn_objname, "X%d", atoi (display));
	    dnsock.sdn_objnamel = strlen(dnsock.sdn_objname);
	    if (bind (request, (struct sockaddr *) &dnsock, sizeof (dnsock)))
		    Error ("Binding DECnet socket");
	    if (listen (request, 5)) Error ("DECnet Listening");
	    requestmask |= (1 << request);
	    Define_self (request);
	}
#endif
	if (requestmask == 0) Error ("No Listeners");
	signal (SIGHUP, SIG_IGN);
	if (InitDisplay (&device)) Error ("Initializing");
	signal (SIGHUP, HangUp);
	Qmax = device.queue->size - 1;
	devmask = 1 << devdesc;
	selmask[0] = devmask | requestmask;
	rr_counter = request;
	havestate = 0;
	firstsock = maxsock = request + 1;
	firstmask = 1 << maxsock;

	if (default_mono && device.entries > 2)
	    device.entries = 2;
	Init_colormap ();
	if (device.entries > 2) {
	    if (default_pix0) {
		if (*default_pix0 == '#')
		    Parse_color (default_pix0+1, &basecolors[0]);
		else if TRUE(havergb) {
		    dbent.dptr = default_pix0;
		    dbent.dsize = strlen (default_pix0);
		    dbent = fetch (dbent);
		    if (dbent.dptr) {
			bcopy(dbent.dptr, (caddr_t)&color, sizeof (RGB));
			basecolors[0].red = color.red;
			basecolors[0].green = color.green;
			basecolors[0].blue = color.blue;
		    }
		}
	    }
	    if (default_pix1) {
		if (*default_pix1 == '#')
		    Parse_color (default_pix1+1, &basecolors[1]);
		else if TRUE(havergb) {
		    dbent.dptr = default_pix1;
		    dbent.dsize = strlen (default_pix1);
		    dbent = fetch (dbent);
		    if (dbent.dptr) {
			bcopy(dbent.dptr, (caddr_t)&color, sizeof (RGB));
			basecolors[1].red = color.red;
			basecolors[1].green = color.green;
			basecolors[1].blue = color.blue;
		    }
		}
	    }
	    Store_colors (2, basecolors);
	}
	bit1 = StoreBitmap (16, 16, (char *) _back);
	roottile = MakePixmap (bit1, WhitePixel, BlackPixel);
	if (--bit1->refcnt == 0)
	    FreeBitmap (bit1);
	Create_root_window (device.height, device.width, roottile);
	bit1 = StoreBitmap (16, 16, (char *) _pointer);
	bit2 = StoreBitmap (16, 16, (char *) _ptrmask);
	rootcursor = StoreCursor (GXcopy, bit1, WhitePixel, BlackPixel, bit2,
				  7, 7);
	if (--bit1->refcnt == 0)
	    FreeBitmap (bit1);
	if (--bit2->refcnt == 0)
	    FreeBitmap (bit2);
	bit1 = StoreBitmap (16, 16, (char *) _ssmask);
	sscursor = StoreCursor (GXnoop, bit1, WhitePixel, BlackPixel, bit1,
				7, 7);
	if (--bit1->refcnt == 0)
	    FreeBitmap (bit1);
	Restore_root ();
	errdelay = 0;
}

/* Parse a color spec:  RGB, RRGGBB, RRRGGGBBB, RRRRGGGGBBBB */

Parse_color (spec, def)
	register char *spec;
	register ColorDef *def;
{
	register int n, i;
	int r, g, b;
	char c;

	n = strlen (spec);
	if (n != 3 && n != 6 && n != 9 && n != 12)
	    return (-1);
	n /= 3;
	r = g = b = 0;
	do {
	    r = g;
	    g = b;
	    b = 0;
	    for (i = n; --i >= 0; ) {
		c = *spec++;
		b <<= 4;
		if (c >= '0' && c <= '9')
		    b |= c - '0';
		else if (c >= 'A' && c <= 'F')
		    b |= c - ('A' - 10);
		else if (c >= 'a' && c <= 'f')
		    b |= c - ('a' - 10);
		else return (-1);
	    }
	} while (*spec);
	n <<= 2;
	n = 16 - n;
	def->red = r << n;
	def->green = g << n;
	def->blue = b << n;
	return (0);
}

/* Restore the world to original condition */

Restore_root ()
{
	Free_window_storage ();
	Free_rectangle_storage ();
	if (device.entries > 2)
	    Store_colors (2, basecolors);
	Reset_cuts ();
	waittime.tv_sec = default_savertime * 60;
	longtime.tv_sec = default_savercycle * 60;
	SetKeyClick (default_click);
	SetAutoRepeat (default_repeat);
	Set_shiftlock (default_lock);
	base_feep = default_feep;
	blank_video = default_blank;
	rootwindow->mask = NoEvent;
	rootwindow->client = 0;
	rootwindow->tilemode = TileModeAbsolute;
	rootwindow->clipmode = ClipModeDrawThru;
	Change_background (rootwindow, roottile);
	Map_root_window ();
	Focus_keyboard (rootwindow);
	Register_cursor (rootwindow, rootcursor);
	Startup_mouse ();
	SetMouseCharacteristics (default_threshold, default_acceleration);
	Reset_hosts (display);
}

/* Exists for ddX to call if it needs to simulate input queue */

Define_input_handler (func)
    int (*func)();
{
    inputhandler = func;
}

/* Exists for ddX to call if it needs computes before blocking */

Define_block_handler (func)
    int (*func)();
{
    blockhandler = func;
}

/* Exists for ddX to call if it needs computes after unblocking */

Define_wakeup_handler (func)
    int (*func)();
{
    wakeuphandler = func;
}

/* Force connections to close on SIGHUP from init */

HangUp ()
{
	int i;

#ifdef GPROF
	chdir ("/tmp");
	exit (0);
#endif
	for (i = firstsock; i < maxsock; i++) {
	    if (socketOn[i])
		close (i);
	}
	if (DisplayDead ())
	    Error ("HangUp");
}

/* log a server error */

Notice (where)
	char *where;
{
	fprintf (stderr, "error: %s at %s\n",
		 (errno > 0 && errno < sys_nerr) ? sys_errlist[errno] : "?",
		 where);
	fflush (stderr);
}

/* log a fatal server error */

Error (where)
	char *where;
{
	fprintf (stderr, "fatal ");
	Notice (where);
	if (errdelay)
	    sleep (errdelay);
	exit (1);
}

/* log a device error */

DeviceError (why)
	char *why;
{
	fprintf (stderr, "device error: %s\n", why);
	fflush (stderr);
}
