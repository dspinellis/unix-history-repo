#include <X/mit-copyright.h>
/* $Header: XOpenDisplay.c,v 10.12 86/12/24 09:07:53 swick Exp $ */
/* Copyright    Massachusetts Institute of Technology    1985	*/
 
#include "XlibInternal.h"
#include <sys/socket.h>
#include <strings.h>
#include <sys/un.h>
 
/* 
 * Connects to a server, creates a Display object and returns a pointer to
 * the newly created Display back to the caller.
 */
Display *XOpenDisplay (display)
	register char *display;
{
	register Display *dpy;		/* New Display object being created. */
	char displaybuf[256];		/* Display string buffer. */
	register char *displayptr;	/* Display string buffer pointer. */
	char buf[4];			/* place to form display string */
	struct sockaddr_in inaddr;	/* INET socket address. */
	struct sockaddr_un unaddr;	/* UNIX socket address. */
	struct sockaddr *addr;		/* address to connect to */
	int addrlen;			/* length of address */
	int dispnum;			/* display number. */
	int indian;			/* to determine which indian. */
	struct hostent *host_ptr;
#ifdef DNETCONN
	char objname[20];		/* Object name buffer */
	int dnet = 0;			/* flag to indicate DECnet connect */
#endif
 
	register XReq *req;		/* XReq request packet pointer. */
	XRep rep;			/* XRep reply packet. */
 
	/* External declarations. */
	extern char *getenv();
	extern char *malloc();
	extern struct hostent *gethostbyname();
 
	/*
	 * Extract the host name and display number from the display
	 * specifier string.  The display specifier string is supplied
	 * as an argument to this routine.  If it is NULL or a pointer
	 * to NULL
	 */
	if (display == NULL || *display == '\0') {
		strncpy (displaybuf, XDisplayName(display), sizeof(displaybuf));
		if (*displaybuf == '\0') return (NULL);
	}
	else {
		/* Display is non-NULL, copy it into the display buffer. */
		strncpy(displaybuf, display, sizeof(displaybuf));
	}
	/* 
	 * Find the ':' seperator and cut out the hostname and the
	 * display number.
	 * NOTE - if DECnet is to be used, the display name is formated
	 * as "host::number"
	 */
	if ((displayptr = index(displaybuf,':')) == NULL) return (NULL);
#ifdef DNETCONN
	if (*(displayptr + 1) == ':') {
	    dnet++;
	    *(displayptr++) = '\0';
	}
#endif
	*(displayptr++) = '\0';
 
	/* displaybuf now contains only a null-terminated host name;
	 * displayptr points to the display number */
 
	/* If the display number is missing there is an error.
	 * Otherwise, convert string to an integer we can use */
	if (*displayptr == '\0') return(NULL);
	dispnum = atoi(displayptr);
 
	if (strcmp("unix", displaybuf) == 0) {
	    /* Connect locally using Unix domain. */
	    unaddr.sun_family = AF_UNIX;
	    strcpy(unaddr.sun_path, X_UNIX_PATH);
	    strcat(unaddr.sun_path, displayptr);
	    addr = (struct sockaddr *) &unaddr;
	    addrlen = strlen(unaddr.sun_path) + 2;
	} else {
#ifdef DNETCONN
	    if (!dnet) {
#endif
		/* If the hostname is missing default to the local host. */
		if (displaybuf[0] == '\0')
		    gethostname (displaybuf, sizeof (displaybuf));
		/* Get the statistics on the specified host. */
		if ((inaddr.sin_addr.s_addr = inet_addr(displaybuf)) == -1) {
			if ((host_ptr = gethostbyname(displaybuf)) == NULL) {
				/* No such host! */
				errno = EINVAL;
				return(NULL);
			}
			/* Check the address type for an internet host. */
			if (host_ptr->h_addrtype != AF_INET) {
				/* Not an Internet host! */
				errno = EPROTOTYPE;
				return(NULL);
			}
 
			/* Set up the socket data. */
			inaddr.sin_family = host_ptr->h_addrtype;
			bcopy((char *)host_ptr->h_addr, 
			      (char *)&inaddr.sin_addr, 
			      sizeof(inaddr.sin_addr));
		} else {
			inaddr.sin_family = AF_INET;
		}
		addr = (struct sockaddr *) &inaddr;
		addrlen = sizeof (struct sockaddr_in);
		inaddr.sin_port = dispnum;
		indian = 1;
		if (*(char *) &indian)
		    inaddr.sin_port += X_TCP_LI_PORT;
		else
		    inaddr.sin_port += X_TCP_BI_PORT;
		inaddr.sin_port = htons(inaddr.sin_port);
#ifdef DNETCONN
	    } else {
		/* If the nodename is missing default to the local node. */
		if (displaybuf[0] == '\0')
		    strcpy (displaybuf, "0");
		/* build the target object name. */
		sprintf (objname, "X%d", dispnum);
	    }
#endif
	}
 
	/* Malloc the new Display. */
	if ((dpy = (Display *)malloc(sizeof(Display))) == NULL) {
		/* Malloc call failed! */
		errno = ENOMEM;
		return(NULL);
	}
 
	dpy->height = dpy->width = 0;
	    /* If DisplayWidth or DisplayWidth is subsequently called,
	       these will be replaced by "real" values. */
 
	/* Open the network socket. */
#ifdef DNETCONN
	if (!dnet) {
#endif
	    if ((dpy->fd = socket(addr->sa_family, SOCK_STREAM, 0)) < 0) {
		    /* Socket call failed! */
		    /* errno set by system call. */
		    free ((char *)dpy);
		    return(NULL);
	    }
 
	    /* Open the connection to the specified X server. */
	    if (connect(dpy->fd, addr, addrlen) == -1) {
		    /* Connection call failed! */
		    /* errno set by system call. */
		    close (dpy->fd);
		    free ((char *)dpy);
		    return(NULL);
	    }
#ifdef DNETCONN
	} else {
	    if ((dpy->fd = dnet_conn(displaybuf, objname, SOCK_STREAM, 0, 0, 0, 0)) < 0) {
		    /* connect failed! */
		    /* errno set by dnet_conn. */
		    free ((char *)dpy);
		    return(NULL);
	    }
	}
#endif
 
	/* Salt away the host:display string for later use */
	buf[0] = ':';
#ifdef DNETCONN
	{
	    int b = 1;
	    if (dnet) buf[b++] = ':';
	    buf[b++] = '0' + dispnum;
	    buf[b] = '\0';
	}
#else DNETCONN
	buf[2] = '\0';
	buf[1] = '0' + dispnum;
#endif DNETCONN
	strcat(displaybuf, buf);
	if ((dpy->displayname = malloc(strlen(displaybuf) + 1)) == NULL) {
		close (dpy->fd);
		free ((char *)dpy);
		errno = ENOMEM;
		return(NULL);
	}
	strcpy (dpy->displayname, displaybuf);
 
	/* Set up the output buffers. */
	if ((dpy->bufptr = dpy->buffer = malloc(BUFSIZE)) == NULL) {
		/* Malloc call failed! */
	    	close (dpy->fd);
		free ((char *)dpy);
		errno = ENOMEM;
		return(NULL);
	}
	dpy->bufmax = dpy->buffer + BUFSIZE;
 
	/* Set up the input event queue and input event queue parameters. */
	dpy->head = dpy->tail = NULL;
	dpy->qlen = 0;
	/* Initialize MouseMoved event squishing. */
	dpy->squish = 1;
 
	_XlibCurrentDisplay = dpy;
 
	/* Send an X_SetUp request to the server. */
	GetReq(X_SetUp, 0);
 
	/* Send X_MakePixmap requests to get black and white
         * constant tile Pixmaps */
        GetReq(X_MakePixmap, 0);
	req->param.l[0] = 0;  /* no bitmap */
	req->paramu2 = BlackPixel;
	GetReq(X_MakePixmap, 0);
	req->param.l[0] = 0;
	req->paramu2 = WhitePixel;
	
	/* The following is needed to synchronize properly with errors,
	 * since three requests are outstanding and no replies have
	 * yet been read
	 */
	dpy->request = 1;
	
	/* Get reply to X_SetUp */
	if (!_XReply(dpy, &rep)) {
		/* There was an error in retrieving the reply. */
	    	close (dpy->fd);
		free (dpy->buffer);
		free ((char *)dpy);
		return(NULL);
	}
 
	/* Set the Display data returned by the X_SetUp call. */
	dpy->root = rep.param.l[0];	/* Root window id. */
	dpy->vnumber = rep.params2;	/* X protocol version number. */
	dpy->dtype = rep.params3;	/* Server's display type. */
	dpy->dplanes = rep.params4;	/* Number of display bit planes. */
	dpy->dcells = rep.paramu5;	/* Number of display color map cell. */
	
	/* Get reply to MakePixmap (black) */
	dpy->request++;
	if (!_XReply (dpy, &rep)) {
	    close (dpy->fd);
	    free (dpy->buffer);
	    free ((char *)dpy);
	    return (NULL);
	    }
	dpy->black = rep.param.l[0];
 
	/* Get reply to MakePixmap (white) */
	dpy->request++;
	if (!_XReply (dpy, &rep)) {
	    close (dpy->fd);
	    free (dpy->buffer);
	    free ((char *)dpy);
	    return (NULL);
	    }
	dpy->white = rep.param.l[0];
 
	return(dpy);
}
