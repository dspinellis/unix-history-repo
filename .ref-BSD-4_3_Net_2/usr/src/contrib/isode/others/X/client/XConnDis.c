#include "copyright.h"
/* $XConsortium: XConnDis.c,v 11.36 88/09/16 11:09:34 jim Exp $ */
/* Copyright    Massachusetts Institute of Technology    1985, 1986	*/
#define NEED_EVENTS
/*
 * THIS IS AN OS DEPENDENT FILE! It should work on 4.2BSD derived
 * systems.  VMS and System V should plan to have their own version.
 */
#include <stdio.h>
#include <X11/Xos.h>
#include "Xlibint.h"
#include <sys/socket.h>
#ifndef hpux
#include <netinet/tcp.h>
#endif

#ifdef hpux
#include <sys/utsname.h>
#endif

#ifdef UNIXCONN
#include <sys/un.h>
#ifndef X_UNIX_PATH
#define X_UNIX_PATH "/tmp/.X11-unix/X"
#endif /* X_UNIX_PATH */
#endif /* UNIXCONN */
void bcopy();

#ifdef ISOCONN
#include <isode/psap.h>
#include <isode/tsap.h>
#include <isode/isoservent.h>

#ifdef ISODEBUG
extern char *isodetcpath;

int isodexbug = 0;
#endif /* ISODEBUG */
#endif /* ISOCONN */

#ifdef ISOCONN

/*
 * array of fd 2 family map so we can lookup right function below...
 * Its initialised at connection setup...
 */
int fd2family[MAXSOCKS];
/*
 * Globals for storing functions appropos each fd/socket type
 * UNIX_IO (0) map to sys calls
 * ISODE_IO (1) maps to my fns...
 */
extern int read(), TReadFromServer();
int (*readfn[])() = 
{
	read, TReadFromServer
};
extern int write(), TWriteToServer();
int (*writefn[])() =
{
	write, TWriteToServer
};
extern int readv(), TReadvFromServer();
int (*readvfn[])() =
{
	readv, TReadvFromServer
};
extern writev(), TWritevToServer();
int (*writevfn[])() =
{
	writev, TWritevToServer
};
extern int UBytesReadable(), TBytesReadable();
int (*ioctlfn[])() =
{
	UBytesReadable, TBytesReadable
};
extern int close(), TDiscFromServer();
int (*closefn[])() =
{
	close, TDiscFromServer
};
	
static
int iso_conn(server)
char *server;
{
	struct TSAPconnect tcs;
	struct TSAPconnect *tc = &tcs;
	struct TSAPdisconnect tds;
	struct TSAPdisconnect *td = &tds;
	struct QOStype qo;
	struct QOStype *qos = &qo;
	AEI   aei;
	struct PSAPaddr *pa, rpa, *mpa, rmpa;
	int ret;
	char *x;
	extern char *index();

#ifdef ISODEBUG
	isodetcpath = ISODEPATH;
#endif /* ISODEBUG */

/*
 * Get their TSAP from their AEI
 */
	if ((x = index(server, ':')) != NULL) {
		*(x++) = '\0';
		aei = str2aei(server, x);
	} else {
		aei = str2aei(server, DEFAULTTSERVICE);
	}

	if (aei == NULLAEI) {
		fprintf(stderr, "No AEI for %s\n", server);
		return -1;
	}

	if ((pa = aei2addr (aei)) == NULLPA) {
		fprintf (stderr, "address translation failed");
		return -1;
	}
	rpa = *pa; /* struct copy */
/* 
 * Get my address for completeness
 */
	mpa = NULLPA;
	if (x != NULL)
		aei = str2aei(TLocalHostName(), x);
	else
		aei = str2aei(TLocalHostName(), DEFAULTTSERVICE);
	if (aei == NULLAEI) {
		fprintf (stderr, "my AEI lookup failed %s\n",
			TLocalHostName());
		return -1;
	}
		
	if ((mpa = aei2addr (aei)) == NULLPA) {
		fprintf (stderr, "my address translation failed %s\n",
			TLocalHostName());
		return -1;
	}

	rmpa = *mpa; /* struct copy */
/*
 * No Xpedited required !
 */
	tc->tc_expedited = 0;
/* 
 * Just a guess - i dont understand
 * the comment in the documentation about TSDUsizes
 */
	tc->tc_tsdusize = 16000; 
	tc->tc_cc = 0;
	qos->qos_reliability = LOW_QUALITY; /* Well this is X after all */

	ret = TConnRequest(&(rmpa.pa_addr.sa_addr),
			&(rpa.pa_addr.sa_addr),
			0, 0, NULLCP, qos, tc, td);
			
/*
 * Should map errors to perrors somehow???
 */
	if (ret == NOTOK) {
#ifdef ISODEBUG
		if (isodexbug)
			fprintf(stderr, "TCR Failed %s\n", 
				TErrString(td->td_reason));
#endif /* ISODEBUG */
		return -1;
	}
	else {
#ifdef ISODEBUG
		if (isodexbug)
			fprintf(stderr, "client: isoconn ok\n");
#endif /* ISODEBUG */
		ret = tc->tc_sd;
		return ret;
	}
}
#endif /* ISOCONN */

/* 
 * Attempts to connect to server, given display name. Returns file descriptor
 * (network socket) or -1 if connection fails. The expanded display name
 * of the form hostname:number.screen ("::" if DECnet) is returned in a result
 * parameter. The screen number to use is also returned.
 */
int _XConnectDisplay (display_name, expanded_name, prop_name, screen_num)
    char *display_name;
    char *expanded_name;	/* return */
    char *prop_name;		/* return */
    int *screen_num;		/* return */

{
	char displaybuf[256];		/* Display string buffer */	
	register char *display_ptr;	/* Display string buffer pointer */
	register char *numbuf_ptr;	/* Server number buffer pointer */
	char *screen_ptr;		/* Pointer for locating screen num */
	int display_num;		/* Display number */
	struct sockaddr_in inaddr;	/* INET socket address. */
	unsigned long hostinetaddr;	/* result of inet_addr of arpa addr */
#ifdef UNIXCONN
	struct sockaddr_un unaddr;	/* UNIX socket address. */
#endif
	struct sockaddr *addr;		/* address to connect to */
        struct hostent *host_ptr;
	int addrlen;			/* length of address */
	extern char *getenv();
	extern struct hostent *gethostbyname();
        int fd;				/* Network socket */
	char numberbuf[16];
	char *dot_ptr = NULL;		/* Pointer to . before screen num */
#ifdef DNETCONN
	int dnet = 0;
	char objname[20];
	extern int dnet_conn();
#endif
#ifdef ISOCONN
	int isoconn = 0;
	char isochar = 'X';
	
/*
 * ISO Host name are "host:T-Namenumber"
 * T-Name = T (for TCP) or X (for X.25) should really be
 * TP4 or TP0 whathaveyou
 */
#endif /* ISOCONN */

	/* 
	 * Find the ':' seperator and extract the hostname and the
	 * display number.
	 * NOTE - if DECnet is to be used, the display name is formatted
	 * as "host::number"
	 */
	(void) strncpy(displaybuf, display_name, sizeof(displaybuf));
	if ((display_ptr = SearchString(displaybuf,':')) == NULL) return (-1);
#ifdef DNETCONN
	if (*(display_ptr + 1) == ':') {
	    dnet++;
	    *(display_ptr++) = '\0';
	}
#endif
#ifdef ISOCONN
	if ((*(display_ptr + 1) == 'X') || (*(display_ptr + 1) == 'T')) {
	    isochar = *(display_ptr + 1);
	    isoconn++;
	    *(display_ptr++) = '\0';
	}
#endif /* ISOCONN */
	*(display_ptr++) = '\0';
 
	/* displaybuf now contains only a null-terminated host name, and
	 * display_ptr points to the display number.
	 * If the display number is missing there is an error. */

	if (*display_ptr == '\0') return(-1);

	/*
	 * Build a string of the form <display-number>.<screen-number> in
	 * numberbuf, using ".0" as the default.
	 */
	screen_ptr = display_ptr;		/* points to #.#.propname */
	numbuf_ptr = numberbuf;			/* beginning of buffer */
	while (*screen_ptr != '\0') {
	    if (*screen_ptr == '.') {		/* either screen or prop */
		if (dot_ptr) {			/* then found prop_name */
		    screen_ptr++;
		    break;
		}
		dot_ptr = numbuf_ptr;		/* found screen_num */
		*(screen_ptr++) = '\0';
		*(numbuf_ptr++) = '.';
	    } else {
		*(numbuf_ptr++) = *(screen_ptr++);
	    }
	}

	/*
	 * If the spec doesn't include a screen number, add ".0" (or "0" if
	 * only "." is present.)
	 */
	if (dot_ptr == NULL) {			/* no screen num or prop */
	    dot_ptr = numbuf_ptr;
	    *(numbuf_ptr++) = '.';
	    *(numbuf_ptr++) = '0';
	} else {
	    if (*(numbuf_ptr - 1) == '.')
		*(numbuf_ptr++) = '0';
	}
	*numbuf_ptr = '\0';

	/*
	 * Return the screen number and property names in the result parameters
	 */
	*screen_num = atoi(dot_ptr + 1);
	strcpy (prop_name, screen_ptr);

	/*
	 * Convert the server number string to an integer.
	 */
	display_num = atoi(display_ptr);

	/*
	 * If the display name is missing, use current host.
	 */
	if (displaybuf[0] == '\0')
#ifdef DNETCONN
	    if (dnet) 
		(void) strcpy (displaybuf, "0");
            else
#endif
#ifdef UNIXCONN
		;	/* Do nothing if UNIX DOMAIN. Will be handled below. */
#else
#ifdef hpux
	    /*
	     * same host name crock as in server and xinit.
	     */
	    {
		struct utsname name;

		uname(&name);
		strcpy(displaybuf, name.nodename);
	    }
#else
		(void) gethostname (displaybuf, sizeof(displaybuf));
#endif /* hpux */
#endif /* UNIXCONN else TCPCONN (assumed) */

#ifdef DNETCONN
	if (dnet) {
	    /*
	     * build the target object name.
	     */
	    sprintf(objname, "X$X%d", display_num);
	    /*
	     * Attempt to open the DECnet connection, return -1 if fails.
	     */
	    if ((fd = dnet_conn(displaybuf, 
		   objname, SOCK_STREAM, 0, 0, 0, 0)) < 0)
		return(-1);	    /* errno set by dnet_conn. */
#ifdef ISOCONN
	    else
		fd2family[fd] = UNIX_IO;
#endif /* ISOCONN */
		
	} else
#endif
#ifdef ISOCONN
	if (isoconn) {
		if ((fd = iso_conn(displaybuf)) <  0)
			return(-1);
		else
			fd2family[fd] = ISODE_IO;
	} else
#endif 
	{
#ifdef UNIXCONN
	    if ((displaybuf[0] == '\0') || 
		(strcmp("unix", displaybuf) == 0)) {
		/* Connect locally using Unix domain. */
		unaddr.sun_family = AF_UNIX;
		(void) strcpy(unaddr.sun_path, X_UNIX_PATH);
		strcat(unaddr.sun_path, display_ptr);
		addr = (struct sockaddr *) &unaddr;
		addrlen = strlen(unaddr.sun_path) + 2;
		/*
		 * Open the network connection.
	 	 */
	        if ((fd = socket((int) addr->sa_family, SOCK_STREAM, 0)) < 0)
		    return(-1);	    /* errno set by system call. */
	    } else
#endif
	    {
		/* Get the statistics on the specified host. */
		hostinetaddr = inet_addr (displaybuf);
		if (hostinetaddr == -1) {
			if ((host_ptr = gethostbyname(displaybuf)) == NULL) {
				/* No such host! */
				errno = EINVAL;
				return(-1);
			}
			/* Check the address type for an internet host. */
			if (host_ptr->h_addrtype != AF_INET) {
				/* Not an Internet host! */
				errno = EPROTOTYPE;
				return(-1);
			}
 
			/* Set up the socket data. */
			inaddr.sin_family = host_ptr->h_addrtype;
#if defined(CRAY) && defined(OLDTCP)
			/* Only Cray UNICOS3 and UNICOS4 will define this */
			{
				long t;
				bcopy((char *)host_ptr->h_addr,
				      (char *)&t,
				      sizeof(inaddr.sin_addr));
				inaddr.sin_addr = t;
			}
#else
			bcopy((char *)host_ptr->h_addr, 
			      (char *)&inaddr.sin_addr, 
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
		inaddr.sin_port = display_num;
		inaddr.sin_port += X_TCP_PORT;
		inaddr.sin_port = htons(inaddr.sin_port);
		/*
		 * Open the network connection.
		 */

		if ((fd = socket((int) addr->sa_family, SOCK_STREAM, 0)) < 0)
		    return(-1);	    /* errno set by system call. */
#ifdef ISOCONN
		fd2family[fd] = UNIX_IO;
#endif /* ISOCONN */
		/* make sure to turn off TCP coalescence */
#ifdef TCP_NODELAY
		{
		int mi = 1;
		setsockopt (fd, IPPROTO_TCP, TCP_NODELAY, &mi, sizeof (int));
		}
#endif
	    }
 

	    if (connect(fd, addr, addrlen) == -1) {
		(void) close (fd);
		return(-1); 	    /* errno set by system call. */
	    }
        }

#ifdef ISOCONN
	if (!isoconn) {
#endif /* ISOCONN */
/* 
 * Not required for ISO T-Service
 */
	/*
	 * set it non-blocking.  This is so we can read data when blocked
	 * for writing in the library.
	 */
#ifdef FIOSNBIO
	{
	    int arg = 1;
	    ioctl(fd, FIOSNBIO, &arg);
	}
#else
	(void) fcntl(fd, F_SETFL, FNDELAY);
#endif /* FIOSNBIO */

#ifdef ISOCONN
	}
#endif /* ISOCONN */
	/*
	 * Return the id if the connection succeeded. Rebuild the expanded
	 * spec and return it in the result parameter.
	 */
	display_ptr = displaybuf-1;
	while (*(++display_ptr) != '\0')
	    ;
	*(display_ptr++) = ':';
#ifdef DNETCONN
	if (dnet)
	    *(display_ptr++) = ':';
#endif
#ifdef ISOCONN
	if (isoconn) {
	    *(display_ptr++) = isochar;
	}
#endif
	numbuf_ptr = numberbuf;
	while (*numbuf_ptr != '\0')
	    *(display_ptr++) = *(numbuf_ptr++);
	if (prop_name[0] != '\0') {
	    char *cp;

	    *(display_ptr++) = '.';
	    for (cp = prop_name; *cp; cp++) *(display_ptr++) = *cp;
	}
	*display_ptr = '\0';
	(void) strcpy(expanded_name, displaybuf);
	return(fd);

}

/* 
 * Disconnect from server.
 */

int _XDisconnectDisplay (server)

    int server;

{
#ifdef ISOCONN
    (void)CloseToServer(server);
#else /* ISOCONN */
    (void) close(server);
#endif /* ISOCONN */
}

#undef NULL
#define NULL ((char *) 0)
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
#ifdef ISOCONN
/* 
 * Note, we shouldnt get any connections to us as we're a client,
 * But the elements of style say we should check...
 * and of course, a TDR
 */
	    int vecp;
	    char *vec[4];
	    struct TSAPdisconnect tds;
            struct TSAPdisconnect *td = &tds;
	    nfound = TNetAccept(&vecp, vec, dpy->fd + 1, r_mask, w_mask, 
		NULL, NOTOK, td);
#ifdef ISODEBUG
	    if (nfound < 0) {
		int sverrno = errno;
		fprintf(stderr, "TNetAcc err %d %s\n", errno,
			TErrString(td->td_reason));
		errno = sverrno;
	    }
#endif /* ISODEBUG */
#ifdef ISODEBUG
	    if (vecp > 0) {	
		fprintf(stderr, "Weirdness, connect to a client?\n");
	    }
#endif
#else /* ISOCONN */
	    nfound = select (dpy->fd + 1, r_mask, w_mask, NULL, NULL);
#endif /* ISOCONN */
	    if (nfound < 0 && errno != EINTR)
		(*_XIOErrorFunction)(dpy);
	} while (nfound <= 0);

	if (ANYSET(r_mask)) {
	    char buf[BUFSIZE];
	    long pend_not_register;
	    register long pend;
	    register xEvent *ev;

	    /* find out how much data can be read */
	    if (BytesReadable(dpy->fd, (char *) &pend_not_register) < 0)
		(*_XIOErrorFunction)(dpy);
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
	if (ANYSET(w_mask))
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
#ifdef ISOCONN
/* 
 * Note, we shouldnt get any connections to us as we're a client,
 * But the elements of style say we should check...
 * and of course, a TDR
 */
	    int vecp;
	    char *vec[4];
	    struct TSAPdisconnect tds;
            struct TSAPdisconnect *td = &tds;

	    BITSET(r_mask, dpy->fd);
	    result = TNetAccept(&vecp, vec, dpy->fd + 1, r_mask, NULL, NULL, 
				NOTOK, td);
#else /* ISOCONN */
	BITSET(r_mask, dpy->fd);
	result = select(dpy->fd + 1, r_mask, NULL, NULL, NULL);
#endif /* ISOCONN */
	if (result == -1 && errno != EINTR) (*_XIOErrorFunction)(dpy);
    } while (result <= 0);
}

static int padlength[4] = {0, 3, 2, 1};

_XSendClientPrefix (dpy, client)
     Display *dpy;
     xConnClientPrefix *client;
{
	/*
	 * Authorization string stuff....  Must always transmit multiple of 4
	 * bytes.
	 */

        char *auth_proto = ""; 
	int auth_length;
	char *auth_string = "";
	int auth_strlen;
	char pad[3];
	char buffer[BUFSIZ], *bptr;

        int bytes=0;

        auth_length = strlen(auth_proto);
        auth_strlen = strlen(auth_string);
        client->nbytesAuthProto = auth_length;
	client->nbytesAuthString = auth_strlen;

	bytes = (SIZEOF(xConnClientPrefix) + 
                       auth_length + padlength[auth_length & 3] +
                       auth_strlen + padlength[auth_strlen & 3]);

	bcopy(client, buffer, SIZEOF(xConnClientPrefix));
        bptr = buffer + SIZEOF(xConnClientPrefix);
        if (auth_length)
	{
	    bcopy(auth_proto, bptr, auth_length);
            bptr += auth_length;
            if (padlength[auth_length & 3])
	    {
		bcopy(pad, bptr, padlength[auth_length & 3]);
	        bptr += padlength[auth_length & 3];
	    }
	}
        if (auth_strlen)
	{
	    bcopy(auth_string, bptr, auth_strlen);
            bptr += auth_strlen;
            if (padlength[auth_strlen & 3])
	    {
		bcopy(pad, bptr, padlength[auth_strlen & 3]);
	        bptr += padlength[auth_strlen & 3];
	    }
	}
	(void) WriteToServer(dpy->fd, buffer, bytes);
	return;
}

