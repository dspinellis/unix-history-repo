/*
 * $XConsortium: XConnDis.c,v 11.88 91/12/17 17:55:57 rws Exp $
 *
 * Copyright 1989 Massachusetts Institute of Technology
 *
 * Permission to use, copy, modify, and distribute this software and its
 * documentation for any purpose and without fee is hereby granted, provided
 * that the above copyright notice appear in all copies and that both that
 * copyright notice and this permission notice appear in supporting
 * documentation, and that the name of M.I.T. not be used in advertising
 * or publicity pertaining to distribution of the software without specific,
 * written prior permission.  M.I.T. makes no representations about the
 * suitability of this software for any purpose.  It is provided "as is"
 * without express or implied warranty.
 *
 * M.I.T. DISCLAIMS ALL WARRANTIES WITH REGARD TO THIS SOFTWARE, INCLUDING ALL
 * IMPLIED WARRANTIES OF MERCHANTABILITY AND FITNESS, IN NO EVENT SHALL M.I.T.
 * BE LIABLE FOR ANY SPECIAL, INDIRECT OR CONSEQUENTIAL DAMAGES OR ANY DAMAGES
 * WHATSOEVER RESULTING FROM LOSS OF USE, DATA OR PROFITS, WHETHER IN AN ACTION
 * OF CONTRACT, NEGLIGENCE OR OTHER TORTIOUS ACTION, ARISING OUT OF OR IN 
 * CONNECTION WITH THE USE OR PERFORMANCE OF THIS SOFTWARE.
 *
 * 
 * This file contains operating system dependencies.
 */

#define NEED_EVENTS

#include <X11/Xlibint.h>
#include <X11/Xos.h>
#include "Xlibnet.h"
#include <X11/Xauth.h>
#include <stdio.h>
#include <ctype.h>
#ifdef DNETCONN
#include <netdnet/dn.h>
#include <netdnet/dnetdb.h>
#endif

#ifdef STREAMSCONN
#define select _XSelect
#endif

#ifndef X_CONNECTION_RETRIES		/* number retries on ECONNREFUSED */
#define X_CONNECTION_RETRIES 5
#endif

#ifdef DNETCONN
static int MakeDECnetConnection();
#endif
#ifdef UNIXCONN
static int MakeUNIXSocketConnection();
#endif
#ifdef TCPCONN
static int MakeTCPConnection();
#endif
#ifdef STREAMSCONN
extern int _XMakeStreamsConnection();
#endif

static void GetAuthorization();

static char *copystring (src, len)
    char *src;
    int len;
{
    char *dst = Xmalloc (len + 1);

    if (dst) {
	strncpy (dst, src, len);
	dst[len] = '\0';
    }

    return dst;
}


/* 
 * Attempts to connect to server, given display name. Returns file descriptor
 * (network socket) or -1 if connection fails.  Display names may be of the
 * following format:
 *
 *     [hostname] : [:] displaynumber [.screennumber]
 *
 * The second colon indicates a DECnet style name.  No hostname is interpretted
 * as the most efficient local connection to a server on the same machine.  
 * This is usually:
 *
 *     o  shared memory
 *     o  local stream
 *     o  UNIX domain socket
 *     o  TCP to local host
 */
int _XConnectDisplay (display_name, fullnamep, dpynump, screenp,
		      auth_namep, auth_namelenp, auth_datap, auth_datalenp)
    char *display_name;
    char **fullnamep;			/* RETURN */
    int *dpynump;			/* RETURN */
    int *screenp;			/* RETURN */
    char **auth_namep;			/* RETURN */
    int *auth_namelenp;			/* RETURN */
    char **auth_datap;			/* RETURN */
    int *auth_datalenp;			/* RETURN */
{
    int family;
    int saddrlen;
    char *saddr;
    char *lastp, *p;			/* char pointers */
    char *phostname = NULL;		/* start of host of display */
    char *pdpynum = NULL;		/* start of dpynum of display */
    char *pscrnum = NULL;		/* start of screen of display */
    Bool dnet = False;			/* if true, then DECnet format */
    int idisplay;			/* required display number */
    int iscreen = 0;			/* optional screen number */
    int (*connfunc)();			/* method to create connection */
    int fd = -1;			/* file descriptor to return */
    int len;				/* length tmp variable */

    p = display_name;

    saddrlen = 0;			/* set so that we can clear later */
    saddr = NULL;

    /*
     * Step 1, find the hostname.  This is delimited by the required 
     * first colon.
     */
    for (lastp = p; *p && *p != ':'; p++) ;
    if (!*p) return -1;		/* must have a colon */

    if (p != lastp) {		/* no hostname given */
	phostname = copystring (lastp, p - lastp);
	if (!phostname) goto bad;	/* no memory */
    }


    /*
     * Step 2, see if this is a DECnet address by looking for the optional
     * second colon.
     */
    if (p[1] == ':') {			/* then DECnet format */
	dnet = True;
	p++;
    }

    /*
     * see if we're allowed to have a DECnet address
     */
#ifndef DNETCONN
    if (dnet) goto bad;
#endif

    
    /*
     * Step 3, find the display number.  This field is required and is 
     * delimited either by a nul or a period, depending on whether or not
     * a screen number is present.
     */

    for (lastp = ++p; *p && isascii(*p) && isdigit(*p); p++) ;
    if ((p == lastp) ||			/* required field */
	(*p != '\0' && *p != '.') ||	/* invalid non-digit terminator */
	!(pdpynum = copystring (lastp, p - lastp)))  /* no memory */
      goto bad;
    idisplay = atoi (pdpynum);


    /*
     * Step 4, find the screen number.  This field is optional.  It is 
     * present only if the display number was followed by a period (which
     * we've already verified is the only non-nul character).
     */

    if (*p) {
	for (lastp = ++p; *p && isascii(*p) && isdigit (*p); p++) ;
	if (*p ||			/* non-digits */
	    !(pscrnum = copystring (lastp, p - lastp)))	 /* no memory */
	  goto bad;
	iscreen = atoi (lastp);
    }



    /*
     * At this point, we know the following information:
     *
     *     phostname                hostname string or NULL
     *     idisplay                 display number
     *     iscreen                  screen number
     *     dnet                     DECnet boolean
     * 
     * We can now decide which transport to use based on the ConnectionFlags
     * build parameter the hostname string.  If phostname is NULL or equals
     * the string "local", then choose the best transport.  If phostname
     * is "unix", then choose BSD UNIX domain sockets (if configured).
     *
     * First, choose default transports:  DECnet else (TCP or STREAMS)
     */


#ifdef DNETCONN
    if (dnet)
      connfunc = MakeDECnetConnection;
    else
#endif
#ifdef TCPCONN
      connfunc = MakeTCPConnection;
#else
#ifdef STREAMSCONN
      connfunc = _XMakeStreamsConnection;
#else
      connfunc = NULL;
#endif
#endif

#ifdef UNIXCONN
    /*
     * Now that the defaults have been established, see if we have any 
     * special names that we have to override:
     *
     *     :N         =>     if UNIXCONN then unix-domain-socket
     *     ::N        =>     if UNIXCONN then unix-domain-socket
     *     unix:N     =>     if UNIXCONN then unix-domain-socket
     *
     * Note that if UNIXCONN isn't defined, then we can use the default
     * transport connection function set above.
     */
    if (!phostname) {
#ifdef apollo
	;   /* Unix domain sockets are *really* bad on apollos */
#else
	connfunc = MakeUNIXSocketConnection;
#endif
    }
    else if (strcmp (phostname, "unix") == 0) {
	connfunc = MakeUNIXSocketConnection;
    }
#endif
    if (!connfunc)
	goto bad;


#ifdef UNIXCONN
#define LOCALCONNECTION (!phostname || connfunc == MakeUNIXSocketConnection)
#else
#define LOCALCONNECTION (!phostname)
#endif

    if (LOCALCONNECTION) {
	/*
	 * Get the auth info for local hosts so that it doesn't have to be
	 * repeated everywhere; the particular values in these fields are
	 * not part of the protocol.
	 */
	char hostnamebuf[256];
	int len = _XGetHostname (hostnamebuf, sizeof hostnamebuf);

	family = FamilyLocal;
	if (len > 0) {
	    saddr = Xmalloc (len + 1);
	    if (saddr) {
		strcpy (saddr, hostnamebuf);
		saddrlen = len;
	    } else {
		saddrlen = 0;
	    }
	}
    }
#undef LOCALCONNECTION


    /*
     * Make the connection, also need to get the auth address info for
     * non-local connections.  Do retries in case server host has hit its
     * backlog (which, unfortunately, isn't distinguishable from there not
     * being a server listening at all, which is why we have to not retry
     * too many times).
     */
    if ((fd = (*connfunc) (phostname, idisplay, X_CONNECTION_RETRIES,
			   &family, &saddrlen, &saddr)) < 0)
      goto bad;
    if (fd >= OPEN_MAX)
	goto bad;

    /*
     * Set close-on-exec so that programs that fork() doesn't get confused.
     */

#ifdef FD_CLOEXEC
    (void) fcntl (fd, F_SETFD, FD_CLOEXEC);
#else
    (void) fcntl (fd, F_SETFD, 1);
#endif

    /*
     * Build the expanded display name:
     *
     *     [host] : [:] dpy . scr \0
     */
    len = ((phostname ? strlen(phostname) : 0) + 1 + (dnet ? 1 : 0) +
	   strlen(pdpynum) + 1 + (pscrnum ? strlen(pscrnum) : 1) + 1);
    *fullnamep = (char *) Xmalloc (len);
    if (!*fullnamep) goto bad;

    sprintf (*fullnamep, "%s%s%d.%d",
	     (phostname ? phostname : ""), (dnet ? "::" : ":"),
	     idisplay, iscreen);

    *dpynump = idisplay;
    *screenp = iscreen;
    if (phostname) Xfree (phostname);
    if (pdpynum) Xfree (pdpynum);
    if (pscrnum) Xfree (pscrnum);

    GetAuthorization(fd, family, saddr, saddrlen, idisplay,
		     auth_namep, auth_namelenp, auth_datap, auth_datalenp);
    return fd;


    /*
     * error return; make sure everything is cleaned up.
     */
  bad:
    if (fd >= 0) (void) close (fd);
    if (saddr) Xfree (saddr);
    if (phostname) Xfree (phostname);
    if (pdpynum) Xfree (pdpynum);
    if (pscrnum) Xfree (pscrnum);
    return -1;

}


/*****************************************************************************
 *                                                                           *
 *			   Make Connection Routines                          *
 *                                                                           *
 *****************************************************************************/

#ifdef DNETCONN				/* stupid makedepend */
#define NEED_BSDISH
#endif
#ifdef UNIXCONN
#define NEED_BSDISH
#endif
#ifdef TCPCONN
#define NEED_BSDISH
#endif

#ifdef NEED_BSDISH			/* makedepend can't handle #if */
/*
 * 4.2bsd-based systems
 */
#include <sys/socket.h>

#ifdef hpux
#define NO_TCP_H
#endif
#ifdef MOTOROLA
#ifdef SYSV
#define NO_TCP_H
#endif
#endif
#ifndef NO_TCP_H
#ifdef __OSF1__
#include <sys/param.h>
#endif
#include <netinet/tcp.h>
#endif
#endif /* NEED_BSDISH */


#ifdef DNETCONN
static int MakeDECnetConnection (phostname, idisplay, retries,
				 familyp, saddrlenp, saddrp)
    char *phostname;
    int idisplay;
    int retries;
    int *familyp;			/* RETURN */
    int *saddrlenp;			/* RETURN */
    char **saddrp;			/* RETURN */
{
    int fd;
    char objname[20];
    extern int dnet_conn();
    struct dn_naddr *dnaddrp, dnaddr;
    struct nodeent *np;

    if (!phostname) phostname = "0";

    /*
     * build the target object name.
     */
    sprintf (objname, "X$X%d", idisplay);

    /*
     * Attempt to open the DECnet connection, return -1 if fails; ought to
     * do some retries here....
     */
    if ((fd = dnet_conn (phostname, objname, SOCK_STREAM, 0, 0, 0, 0)) < 0) {
	return -1;
    }

    *familyp = FamilyDECnet;
    if (dnaddrp = dnet_addr (phostname)) {  /* stolen from xhost */
	dnaddr = *dnaddrp;
    } else {
	if ((np = getnodebyname (phostname)) == NULL) {
	    (void) close (fd);
	    return -1;
	}
	dnaddr.a_len = np->n_length;
	bcopy (np->n_addr, dnaddr.a_addr, np->n_length);
    }

    *saddrlenp = sizeof (struct dn_naddr);
    *saddrp = Xmalloc (*saddrlenp);
    if (!*saddrp) {
	(void) close (fd);
	return -1;
    }
    bcopy ((char *)&dnaddr, *saddrp, *saddrlenp);
    return fd;
}
#endif /* DNETCONN */


#ifdef UNIXCONN
#include <sys/un.h>

/*ARGSUSED*/
static int MakeUNIXSocketConnection (phostname, idisplay, retries,
				     familyp, saddrlenp, saddrp)
    char *phostname;
    int idisplay;
    int retries;
    int *familyp;			/* RETURN */
    int *saddrlenp;			/* RETURN */
    char **saddrp;			/* RETURN */
{
    struct sockaddr_un unaddr;		/* UNIX socket data block */
    struct sockaddr *addr;		/* generic socket pointer */
    int addrlen;			/* length of addr */
    int fd;				/* socket file descriptor */
#ifdef hpux /* this is disgusting */
    struct sockaddr_un ounaddr;		/* UNIX socket data block */
    struct sockaddr *oaddr;		/* generic socket pointer */
    int oaddrlen;			/* length of addr */
#endif

    unaddr.sun_family = AF_UNIX;
    sprintf (unaddr.sun_path, "%s%d", X_UNIX_PATH, idisplay);

    addr = (struct sockaddr *) &unaddr;
#ifdef SUN_LEN
    addrlen = SUN_LEN(&unaddr);
#else
    addrlen = strlen(unaddr.sun_path) + sizeof(unaddr.sun_family);
#endif

#ifdef hpux /* this is disgusting */
    ounaddr.sun_family = AF_UNIX;
    sprintf (ounaddr.sun_path, "%s%d", OLD_UNIX_PATH, idisplay);
    oaddr = (struct sockaddr *) &ounaddr;
    oaddrlen = strlen(ounaddr.sun_path) + sizeof(ounaddr.sun_family);
#endif

    /*
     * Open the network connection.
     */
    do {
	if ((fd = socket ((int) addr->sa_family, SOCK_STREAM, 0)) < 0) {
	    return -1;
	}

	if (connect (fd, addr, addrlen) < 0) {
	    int olderrno = errno;
	    (void) close (fd);
#ifdef hpux /* this is disgusting */
	    if (olderrno == ENOENT) {
		fd = socket ((int) oaddr->sa_family, SOCK_STREAM, 0);
		if (fd >= 0) {
		    if (connect (fd, oaddr, oaddrlen) >= 0)
			break;
		    olderrno = errno;
		    (void) close (fd);
		}
	    }
#endif
	    if (olderrno != ENOENT || retries <= 0) {
		errno = olderrno;
		return -1;
	    }
	    sleep (1);
	} else {
	    break;
	}
    } while (retries-- > 0);

    /*
     * Don't need to get auth info since we're local
     */
    return fd;
}
#endif /* UNIXCONN */


#ifdef TCPCONN
static int MakeTCPConnection (phostname, idisplay, retries,
			      familyp, saddrlenp, saddrp)
    char *phostname;
    int idisplay;
    int retries;
    int *familyp;			/* RETURN */
    int *saddrlenp;			/* RETURN */
    char **saddrp;			/* RETURN */
{
    char hostnamebuf[256];		/* tmp space */
    unsigned long hostinetaddr;		/* result of inet_addr of arpa addr */
    struct sockaddr_in inaddr;		/* IP socket */
    struct sockaddr *addr;		/* generic socket pointer */
    int addrlen;			/* length of addr */
    struct hostent *hp;			/* entry in hosts table */
    char *cp;				/* character pointer iterator */
    int fd;				/* file descriptor to return */
    int len;				/* length tmp variable */

#define INVALID_INETADDR ((unsigned long) -1)

    if (!phostname) {
	hostnamebuf[0] = '\0';
	(void) _XGetHostname (hostnamebuf, sizeof hostnamebuf);
	phostname = hostnamebuf;
    }

    /*
     * if numeric host name then try to parse it as such; do the number
     * first because some systems return garbage instead of INVALID_INETADDR
     */
    if (isascii(phostname[0]) && isdigit(phostname[0])) {
	hostinetaddr = inet_addr (phostname);
    } else {
	hostinetaddr = INVALID_INETADDR;
    }

    /*
     * try numeric
     */
    if (hostinetaddr == INVALID_INETADDR) {
	if ((hp = gethostbyname(phostname)) == NULL) {
	    /* No such host! */
	    return -1;
	}
	if (hp->h_addrtype != AF_INET) {  /* is IP host? */
	    /* Not an Internet host! */
	    return -1;
	}
 
	/* Set up the socket data. */
	inaddr.sin_family = hp->h_addrtype;
#if defined(CRAY) && defined(OLDTCP)
	/* Only Cray UNICOS3 and UNICOS4 will define this */
	{
	    long t;
	    bcopy ((char *)hp->h_addr, (char *)&t, sizeof(t));
	    inaddr.sin_addr = t;
	}
#else
	bcopy ((char *)hp->h_addr, (char *)&inaddr.sin_addr, 
	       sizeof(inaddr.sin_addr));
#endif /* CRAY and OLDTCP */
    } else {
#if defined(CRAY) && defined(OLDTCP)
	/* Only Cray UNICOS3 and UNICOS4 will define this */
	inaddr.sin_addr = hostinetaddr;
#else
	inaddr.sin_addr.s_addr = hostinetaddr;
#endif /* CRAY and OLDTCP */
	inaddr.sin_family = AF_INET;
    }

    addr = (struct sockaddr *) &inaddr;
    addrlen = sizeof (struct sockaddr_in);
    inaddr.sin_port = X_TCP_PORT + idisplay;
    inaddr.sin_port = htons (inaddr.sin_port);	/* may be funky macro */


    /*
     * Open the network connection.
     */
    do {
	if ((fd = socket ((int) addr->sa_family, SOCK_STREAM, 0)) < 0) {
	    return -1;
	}

	/*
	 * turn off TCP coalescence
	 */
#ifdef TCP_NODELAY
	{
	    int tmp = 1;
	    setsockopt (fd, IPPROTO_TCP, TCP_NODELAY, &tmp, sizeof (int));
	}
#endif

	/*
	 * connect to the socket; if there is no X server or if the backlog has
	 * been reached, then ECONNREFUSED will be returned.
	 */
	if (connect (fd, addr, addrlen) < 0) {
	    int olderrno = errno;
	    (void) close (fd);
	    if (olderrno != ECONNREFUSED || retries <= 0) {
		errno = olderrno;
		return -1;
	    }
	    sleep (1);
	} else {
	    break;
	}
    } while (retries-- > 0);


    /*
     * Success!  So, save the auth information
     */
#ifdef CRAY
#ifdef OLDTCP
    len = sizeof(inaddr.sin_addr);
#else
    len = SIZEOF_in_addr;
#endif /* OLDTCP */
    cp = (char *) &inaddr.sin_addr;
#else /* else not CRAY */
    len = sizeof(inaddr.sin_addr.s_addr);
    cp = (char *) &inaddr.sin_addr.s_addr;
#endif /* CRAY */

    /*
     * We are special casing the BSD hack localhost address
     * 127.0.0.1, since this address shouldn't be copied to
     * other machines.  So, we simply omit generating the auth info
     * since we set it to the local machine before calling this routine!
     */
    if (!((len == 4) && (cp[0] == 127) && (cp[1] == 0) &&
	  (cp[2] == 0) && (cp[3] == 1))) {
	*saddrp = Xmalloc (len);
	if (*saddrp) {
	    *saddrlenp = len;
	    bcopy (cp, *saddrp, len);
	    *familyp = FamilyInternet;
	} else {
	    *saddrlenp = 0;
	}
    }

    return fd;
}
#undef INVALID_INETADDR
#endif /* TCPCONN */



/*****************************************************************************
 *                                                                           *
 *			  Connection Utility Routines                        *
 *                                                                           *
 *****************************************************************************/

/* 
 * Disconnect from server.
 */

int _XDisconnectDisplay (server)

    int server;

{
    (void) close(server);
    return 0;
}



/*
 * This is an OS dependent routine which:
 * 1) returns as soon as the connection can be written on....
 * 2) if the connection can be read, must enqueue events and handle errors,
 * until the connection is writable.
 */
_XWaitForWritable(dpy)
    Display *dpy;
{
    unsigned long r_mask[MSKCNT];
    unsigned long w_mask[MSKCNT];
    int nfound;

    CLEARBITS(r_mask);
    CLEARBITS(w_mask);

    while (1) {
	BITSET(r_mask, dpy->fd);
        BITSET(w_mask, dpy->fd);

	do {
	    nfound = select (dpy->fd + 1, r_mask, w_mask,
			     (char *)NULL, (char *)NULL);
	    if (nfound < 0 && errno != EINTR)
		_XIOError(dpy);
	} while (nfound <= 0);

	if (_XANYSET(r_mask)) {
	    char buf[BUFSIZE];
	    long pend_not_register;
	    register long pend;
	    register xEvent *ev;

	    /* find out how much data can be read */
	    if (BytesReadable(dpy->fd, (char *) &pend_not_register) < 0)
		_XIOError(dpy);
	    pend = pend_not_register;

	    /* must read at least one xEvent; if none is pending, then
	       we'll just block waiting for it */
	    if (pend < SIZEOF(xEvent)) pend = SIZEOF(xEvent);
		
	    /* but we won't read more than the max buffer size */
	    if (pend > BUFSIZE) pend = BUFSIZE;

	    /* round down to an integral number of XReps */
	    pend = (pend / SIZEOF(xEvent)) * SIZEOF(xEvent);

	    _XRead (dpy, buf, pend);

	    /* no space between comma and type or else macro will die */
	    STARTITERATE (ev,xEvent, buf, (pend > 0),
			  (pend -= SIZEOF(xEvent))) {
		if (ev->u.u.type == X_Error)
		    _XError (dpy, (xError *) ev);
		else		/* it's an event packet; enqueue it */
		    _XEnq (dpy, ev);
	    }
	    ENDITERATE
	}
	if (_XANYSET(w_mask))
	    return;
    }
}


_XWaitForReadable(dpy)
  Display *dpy;
{
    unsigned long r_mask[MSKCNT];
    int result;
	
    CLEARBITS(r_mask);
    do {
	BITSET(r_mask, dpy->fd);
	result = select(dpy->fd + 1, r_mask,
			(char *)NULL, (char *)NULL, (char *)NULL);
	if (result == -1 && errno != EINTR) _XIOError(dpy);
    } while (result <= 0);
}


static int padlength[4] = {0, 3, 2, 1};	 /* make sure auth is multiple of 4 */

Bool
_XSendClientPrefix (dpy, client, auth_proto, auth_string)
     Display *dpy;
     xConnClientPrefix *client;		/* contains count for auth_* */
     char *auth_proto, *auth_string;	/* NOT null-terminated */
{
    int auth_length = client->nbytesAuthProto;
    int auth_strlen = client->nbytesAuthString;
    char padbuf[3];			/* for padding to 4x bytes */
    int pad;
    struct iovec iovarray[5], *iov = iovarray;
    int niov = 0;
    int len = 0;

#define add_to_iov(b,l) \
  { iov->iov_base = (b); iov->iov_len = (l); iov++, niov++; len += (l); }

    add_to_iov ((caddr_t) client, SIZEOF(xConnClientPrefix));

    /*
     * write authorization protocol name and data
     */
    if (auth_length > 0) {
	add_to_iov (auth_proto, auth_length);
	pad = padlength [auth_length & 3];
	if (pad) add_to_iov (padbuf, pad);
    }
    if (auth_strlen > 0) {
	add_to_iov (auth_string, auth_strlen);
	pad = padlength [auth_strlen & 3];
	if (pad) add_to_iov (padbuf, pad);
    }

#undef add_to_iov

    len -= WritevToServer (dpy->fd, iovarray, niov);

    /*
     * Set the connection non-blocking since we use select() to block.
     */
    /* ultrix reads hang on Unix sockets, hpux reads fail */
#if defined(O_NONBLOCK) && (!defined(ultrix) && !defined(hpux) && !defined(AIXV3))
    (void) fcntl (dpy->fd, F_SETFL, O_NONBLOCK);
#else
#ifdef FIOSNBIO
    {
	int arg = 1;
	ioctl (dpy->fd, FIOSNBIO, &arg);
    }
#else
#if defined(AIXV3) && defined(FIONBIO)
    {
	int arg;
	arg = 1;
	ioctl(dpy->fd, FIONBIO, &arg);
    }
#else
    (void) fcntl (dpy->fd, F_SETFL, FNDELAY);
#endif
#endif
#endif
    return len == 0;
}


#ifdef STREAMSCONN
#ifdef SVR4
#include <tiuser.h>
#else
#undef HASXDMAUTH
#endif
#endif

#ifdef SECURE_RPC
#include <rpc/rpc.h>
#ifdef ultrix
#include <time.h>
#include <rpc/auth_des.h>
#endif
#endif

/*
 * First, a routine for setting authorization data
 */
static int xauth_namelen = 0;
static char *xauth_name = NULL;	 /* NULL means use default mechanism */
static int xauth_datalen = 0;
static char *xauth_data = NULL;	 /* NULL means get default data */

/*
 * This is a list of the authorization names which Xlib currently supports.
 * Xau will choose the file entry which matches the earliest entry in this
 * array, allowing us to prioritize these in terms of the most secure first
 */

static char *default_xauth_names[] = {
#ifdef SECURE_RPC
    "SUN-DES-1",
#endif
#ifdef HASXDMAUTH
    "XDM-AUTHORIZATION-1",
#endif
    "MIT-MAGIC-COOKIE-1"
};

static int default_xauth_lengths[] = {
#ifdef SECURE_RPC
    9,	    /* strlen ("SUN-DES-1") */
#endif
#ifdef HASXDMAUTH
    19,	    /* strlen ("XDM-AUTHORIZATION-1") */
#endif
    18	    /* strlen ("MIT-MAGIC-COOKIE-1") */
};

#define NUM_DEFAULT_AUTH    (sizeof (default_xauth_names) / sizeof (default_xauth_names[0]))
    
static char **xauth_names = default_xauth_names;
static int  *xauth_lengths = default_xauth_lengths;

static int  xauth_names_length = NUM_DEFAULT_AUTH;

void XSetAuthorization (name, namelen, data, datalen)
    int namelen, datalen;		/* lengths of name and data */
    char *name, *data;			/* NULL or arbitrary array of bytes */
{
    char *tmpname, *tmpdata;

    if (xauth_name) Xfree (xauth_name);	 /* free any existing data */
    if (xauth_data) Xfree (xauth_data);

    xauth_name = xauth_data = NULL;	/* mark it no longer valid */
    xauth_namelen = xauth_datalen = 0;

    if (namelen < 0) namelen = 0;	/* check for bogus inputs */
    if (datalen < 0) datalen = 0;	/* maybe should return? */

    if (namelen > 0)  {			/* try to allocate space */
	tmpname = Xmalloc ((unsigned) namelen);
	if (!tmpname) return;
	bcopy (name, tmpname, namelen);
    } else {
	tmpname = NULL;
    }

    if (datalen > 0)  {
	tmpdata = Xmalloc ((unsigned) datalen);
	if (!tmpdata) {
	    if (tmpname) (void) Xfree (tmpname);
	    return;
	}
	bcopy (data, tmpdata, datalen);
    } else {
	tmpdata = NULL;
    }

    xauth_name = tmpname;		/* and store the suckers */
    xauth_namelen = namelen;
    if (tmpname)
    {
	xauth_names = &xauth_name;
	xauth_lengths = &xauth_namelen;
	xauth_names_length = 1;
    }
    else
    {
	xauth_names = default_xauth_names;
	xauth_lengths = default_xauth_lengths;
	xauth_names_length = NUM_DEFAULT_AUTH;
    }
    xauth_data = tmpdata;
    xauth_datalen = datalen;
    return;
}

#ifdef SECURE_RPC
/*
 * Create a credential that we can send to the X server.
 */
static int
auth_ezencode(servername, window, cred_out, len)
        char           *servername;
        int             window;
	char	       *cred_out;
        int            *len;
{
        AUTH           *a;
        XDR             xdr;

        a = authdes_create(servername, window, NULL, NULL);
        if (a == (AUTH *)NULL) {
                perror("auth_create");
                return 0;
        }
        xdrmem_create(&xdr, cred_out, *len, XDR_ENCODE);
        if (AUTH_MARSHALL(a, &xdr) == FALSE) {
                perror("auth_marshall");
                AUTH_DESTROY(a);
                return 0;
        }
        *len = xdr_getpos(&xdr);
        AUTH_DESTROY(a);
	return 1;
}
#endif

static void
GetAuthorization(fd, family, saddr, saddrlen, idisplay,
		 auth_namep, auth_namelenp, auth_datap, auth_datalenp)
    int fd;
    int family;
    int saddrlen;
    int idisplay;
    char *saddr;
    char **auth_namep;			/* RETURN */
    int *auth_namelenp;			/* RETURN */
    char **auth_datap;			/* RETURN */
    int *auth_datalenp;			/* RETURN */
{
#ifdef SECURE_RPC
    char rpc_cred[MAX_AUTH_BYTES];
#endif
#ifdef HASXDMAUTH
    char xdmcp_data[192/8];
#endif
    char *auth_name;
    int auth_namelen;
    char *auth_data;
    int auth_datalen;
    Xauth *authptr = NULL;

/*
 * Look up the authorization protocol name and data if necessary.
 */
    if (xauth_name && xauth_data) {
	auth_namelen = xauth_namelen;
	auth_name = xauth_name;
	auth_datalen = xauth_datalen;
	auth_data = xauth_data;
    } else {
	char dpynumbuf[40];		/* big enough to hold 2^64 and more */
	(void) sprintf (dpynumbuf, "%d", idisplay);

	authptr = XauGetBestAuthByAddr ((unsigned short) family,
				    (unsigned short) saddrlen,
				    saddr,
				    (unsigned short) strlen (dpynumbuf),
				    dpynumbuf,
				    xauth_names_length,
				    xauth_names,
				    xauth_lengths);
	if (authptr) {
	    auth_namelen = authptr->name_length;
	    auth_name = (char *)authptr->name;
	    auth_datalen = authptr->data_length;
	    auth_data = (char *)authptr->data;
	} else {
	    auth_namelen = 0;
	    auth_name = NULL;
	    auth_datalen = 0;
	    auth_data = NULL;
	}
    }
#ifdef HASXDMAUTH
    /*
     * build XDM-AUTHORIZATION-1 data
     */
    if (auth_namelen == 19 && !strncmp (auth_name, "XDM-AUTHORIZATION-1", 19))
    {
	int     j;
	long    now;
	for (j = 0; j < 8; j++)
	    xdmcp_data[j] = auth_data[j];
#ifdef STREAMSCONN /* && SVR4 */
	{
	    int			i;
	    struct netbuf	netb;
	    char		addrret[1024];

	    netb.maxlen = sizeof addrret;
	    netb.buf = addrret;
	    if (t_getname (fd, &netb, LOCALNAME) == -1)
		t_error ("t_getname");
	    /*
	     * XXX - assumes that the return data
	     * are in a struct sockaddr_in, and that
	     * the data structure is layed out in
	     * the normal fashion.  This WILL NOT WORK
	     * on a non 32-bit machine (same in Xstreams.c)
	     */
	    for (i = 4; i < 8; i++)
		xdmcp_data[j++] = netb.buf[i];
	    for (i = 2; i < 4; i++)
		xdmcp_data[j++] = netb.buf[i];
	}
#else
	{
	    unsigned long	addr;
	    unsigned short	port;
#ifdef TCPCONN
	    int	    addrlen;
	    struct sockaddr_in	in_addr;

	    addrlen = sizeof (in_addr);
	    if (getsockname (fd,
			     (struct sockaddr *) &in_addr,
			     &addrlen) != -1 &&
		addrlen >= sizeof in_addr &&
		in_addr.sin_family == AF_INET)
	    {
		addr = ntohl (in_addr.sin_addr.s_addr);
		port = ntohs (in_addr.sin_port);
	    }
	    else
#endif
	    {
		static unsigned long	unix_addr = 0xFFFFFFFF;
		addr = unix_addr--;
		port = getpid ();
	    }
	    xdmcp_data[j++] = (addr >> 24) & 0xFF;
	    xdmcp_data[j++] = (addr >> 16) & 0xFF;
	    xdmcp_data[j++] = (addr >>  8) & 0xFF;
	    xdmcp_data[j++] = (addr >>  0) & 0xFF;
	    xdmcp_data[j++] = (port >>  8) & 0xFF;
	    xdmcp_data[j++] = (port >>  0) & 0xFF;
	}
#endif
	time (&now);
	xdmcp_data[j++] = (now >> 24) & 0xFF;
	xdmcp_data[j++] = (now >> 16) & 0xFF;
	xdmcp_data[j++] = (now >>  8) & 0xFF;
	xdmcp_data[j++] = (now >>  0) & 0xFF;
	while (j < 192 / 8)
	    xdmcp_data[j++] = 0;
	XdmcpWrap (xdmcp_data, auth_data + 8,
		      xdmcp_data, j);
	auth_data = xdmcp_data;
	auth_datalen = j;
    }
#endif /* HASXDMAUTH */
#ifdef SECURE_RPC
    /*
     * The SUN-DES-1 authorization protocol uses the
     * "secure RPC" mechanism in SunOS 4.0+.
     */
    if (auth_namelen == 9 && !strncmp(auth_name, "SUN-DES-1", 9)) {
	char servernetname[MAXNETNAMELEN + 1];

	/*
	 * Copy over the server's netname from the authorization
	 * data field filled in by XauGetAuthByAddr().
	 */
	if (auth_datalen > MAXNETNAMELEN) {
	    auth_datalen = 0;
	    auth_data = NULL;
	} else {
	    bcopy(auth_data, servernetname, auth_datalen);
	    servernetname[auth_datalen] = '\0';

	    auth_datalen = sizeof (rpc_cred);
	    if (auth_ezencode(servernetname, 100, rpc_cred,
			      &auth_datalen))
		auth_data = rpc_cred;
	    else
		auth_data = NULL;
	}
    }
#endif
    if (saddr) Xfree (saddr);
    if (*auth_namelenp = auth_namelen)
    {
	if (*auth_namep = Xmalloc(auth_namelen))
	    bcopy(auth_name, *auth_namep, auth_namelen);
	else
	    *auth_namelenp = 0;
    }
    else
	*auth_namep = NULL;
    if (*auth_datalenp = auth_datalen)
    {
	if (*auth_datap = Xmalloc(auth_datalen))
	    bcopy(auth_data, *auth_datap, auth_datalen);
	else
	    *auth_datalenp = 0;
    }
    else
	*auth_datap = NULL;
    if (authptr) XauDisposeAuth (authptr);
}
