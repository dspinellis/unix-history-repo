/* internet.c - TCP/IP abstractions */

#ifndef	lint
static char *rcsid = "$Header: /f/osi/compat/RCS/internet.c,v 7.3 91/02/22 09:15:14 mrose Interim $";
#endif

/* 
 * $Header: /f/osi/compat/RCS/internet.c,v 7.3 91/02/22 09:15:14 mrose Interim $
 *
 *
 * $Log:	internet.c,v $
 * Revision 7.3  91/02/22  09:15:14  mrose
 * Interim 6.8
 * 
 * Revision 7.2  90/11/21  11:29:41  mrose
 * sun
 * 
 * Revision 7.1  90/01/11  18:35:06  mrose
 * real-sync
 * 
 * Revision 7.0  89/11/23  21:23:03  mrose
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


/* LINTLIBRARY */

#include <ctype.h>
#include <errno.h>
#include <stdio.h>
#include "general.h"
#include "manifest.h"
#include "tailor.h"

/*  */

#ifdef	TCP
#include "internet.h"


extern int  errno;

/*    Berkeley UNIX: 4.2 */

#ifdef	SOCKETS

/* For real networking, nothing is better than 4BSD! */


int	start_tcp_client (sock, priv)
struct sockaddr_in *sock;
int	priv;
{
    register int    port;
    int     eindex,
	    sd;
#ifdef	BSD43
    int	    onoff;
#endif

    if ((sd = socket (AF_INET, SOCK_STREAM, 0)) == NOTOK) {
	SLOG (compat_log, LLOG_EXCEPTIONS, "failed", ("socket"));
	return NOTOK;
    }

    if (sock == NULL)
	goto got_socket;

    for (port = IPPORT_RESERVED - priv;; priv ? port-- : port++) {
	sock -> sin_port = htons ((u_short) port);

	if (bind (sd, (struct sockaddr *) sock, sizeof *sock) != NOTOK)
	    break;

	switch (errno) {
	    case EADDRINUSE: 
		if (!priv || (port >= IPPORT_RESERVED / 2))
		    continue;	/* else fall */

	    case EADDRNOTAVAIL: 
	    default:
		eindex = errno;
		SLOG (compat_log, LLOG_EXCEPTIONS, "failed", ("bind"));
		(void) close_tcp_socket (sd);
		errno = eindex;
		return NOTOK;
	}
    }

got_socket: ;
#ifndef	BSD43
    if (setsockopt (sd, SOL_SOCKET, SO_KEEPALIVE, NULLCP, 0) == NOTOK)
	SLOG (compat_log, LLOG_EXCEPTIONS, "failed", ("set SO_KEEPALIVE"));
#else
    onoff = 1;
    if (setsockopt (sd, SOL_SOCKET, SO_KEEPALIVE, (char *) &onoff,
		       sizeof onoff) == NOTOK)
	SLOG (compat_log, LLOG_EXCEPTIONS, "failed", ("set SO_KEEPALIVE"));
#endif

    return sd;
}

/*  */

int	start_tcp_server (sock, backlog, opt1, opt2)
struct sockaddr_in *sock;
int	backlog,
	opt1,
	opt2;
{
    register int    port;
    int     eindex,
	    sd;
#ifdef	BSD43
    int	    onoff;
#endif

    if ((sd = socket (AF_INET, SOCK_STREAM, 0)) == NOTOK) {
	SLOG (compat_log, LLOG_EXCEPTIONS, "failed", ("socket"));
	return NOTOK;
    }

    if (sock -> sin_port != 0) {
	if (bind (sd, (struct sockaddr *) sock, sizeof *sock) != NOTOK)
	    goto got_socket;

	eindex = errno;
	SLOG (compat_log, LLOG_EXCEPTIONS, "failed", ("bind"));
	(void) close_tcp_socket (sd);
	errno = eindex;
	return NOTOK;
    }

    for (port = IPPORT_RESERVED;; port++) {
	sock -> sin_port = htons ((u_short) port);

	if (bind (sd, (struct sockaddr *) sock, sizeof *sock) != NOTOK)
	    break;

	switch (errno) {
	    case EADDRINUSE: 
		continue;

	    case EADDRNOTAVAIL: 
	    default:
		eindex = errno;
		SLOG (compat_log, LLOG_EXCEPTIONS, "failed", ("bind"));
		(void) close_tcp_socket (sd);
		errno = eindex;
		return NOTOK;
	}
    }

got_socket: ;
#ifndef	BSD43
    if (setsockopt (sd, SOL_SOCKET, SO_KEEPALIVE, NULLCP, 0) == NOTOK)
	SLOG (compat_log, LLOG_EXCEPTIONS, "failed", ("set SO_KEEPALIVE"));
    if (opt1 && setsockopt (sd, SOL_SOCKET, opt1, NULLCP, 0) == NOTOK)
	SLOG (compat_log, LLOG_EXCEPTIONS, "failed",
	      ("set socket option 0x%x", opt1));
    if (opt2 && setsockopt (sd, SOL_SOCKET, opt2, NULLCP, 0) == NOTOK)
	SLOG (compat_log, LLOG_EXCEPTIONS, "failed",
	      ("set socket option 0x%x", opt2));
#else
    onoff = 1;
    if (setsockopt (sd, SOL_SOCKET, SO_KEEPALIVE, (char *) &onoff,
		       sizeof onoff) == NOTOK)
	SLOG (compat_log, LLOG_EXCEPTIONS, "failed", ("set SO_KEEPALIVE"));
    if (opt1
	    && setsockopt (sd, SOL_SOCKET, opt1, (char *) &onoff, sizeof onoff)
		    == NOTOK)
	SLOG (compat_log, LLOG_EXCEPTIONS, "failed",
	      ("set socket option 0x%x", opt1));
    if (opt2
	    && setsockopt (sd, SOL_SOCKET, opt2, (char *) &onoff, sizeof onoff)
		    == NOTOK)
	SLOG (compat_log, LLOG_EXCEPTIONS, "failed",
	      ("set socket option 0x%x", opt2));
#endif

    (void) listen (sd, backlog);

    return sd;
}

/*  */

int	join_tcp_client (fd, sock)
int	fd;
struct sockaddr_in *sock;
{
    int     eindex,
	    len = sizeof *sock,
	    result;

    if ((result = accept (fd, (struct sockaddr *) sock, &len)) == NOTOK) {
	eindex = errno;
	SLOG (compat_log, LLOG_EXCEPTIONS, "failed", ("accept"));
	errno = eindex;
    }

    return result;
}

/*  */

int	join_tcp_server (fd, sock)
int	fd;
struct sockaddr_in *sock;
{
    int     eindex,
	    result;

    if ((result = connect (fd, (struct sockaddr *) sock, sizeof *sock))
	    == NOTOK) {
	eindex = errno;
	SLOG (compat_log, LLOG_EXCEPTIONS, "failed", ("connect"));
	errno = eindex;
    }

    return result;
}

/*  */

close_tcp_socket (fd)
int	fd;
{
#ifdef	never_do_this_if_from_join_tcp_client
    (void) shutdown (fd, 2);
#endif

    return (close (fd));
}

#endif

/*    AT&T UNIX: 5r3 using TLI */


/*    AT&T UNIX: 5 with EXOS 8044 TCP/IP card */

#ifdef	EXOS

/* If we had a getsockname() for the EXOS card, then we could postpone some
   of the binding until connect time.  But since we don't, our hand is forced
   and we must prematurely bind the sockets to IP addresses. */


start_tcp_client (sock, priv)
struct sockaddr_in *sock;
int	priv;
{
    register int    port;
    int     sd;
    register struct hostent *hp;

    if (sock == NULL)
	return socket (SOCK_STREAM, 0, (struct sockaddr *) 0, SO_KEEPALIVE);

    if (sock -> sin_addr.s_addr == 0) {
	if ((hp = gethostbyname ("localhost")) == NULL) {
	    errno = EADDRNOTAVAIL;
	    return NOTOK;
	}
	sock -> sin_family = hp -> h_addrtype;
	inaddr_copy (hp, sock);
    }

    for (port = IPPORT_RESERVED - priv;; priv ? port-- : port++) {
	sock -> sin_port = htons ((u_short) port);

	if ((sd = socket (SOCK_STREAM, 0, (struct sockaddr *) sock,
			  SO_KEEPALIVE)) != NOTOK)
	    return sd;

	switch (errno) {
	    case EADDRINUSE: 
		if (!priv || (port >= IPPORT_RESERVED / 2))
		    continue;	/* else fall */

	    case EADDRNOTAVAIL: 
	    default: 
		return NOTOK;
	}
    }
}

/*  */

int	start_tcp_server (sock, backlog, opt1, opt2)
struct sockaddr_in *sock;
int	backlog,
	opt1,
	opt2;
{
    register int    port;
    int     sd;
    register struct hostent *hp;

    if (backlog != 1)
	return socket (SOCK_STREAM, 0, (struct sockaddr *) sock,
		    SO_ACCEPTCONN | SO_KEEPALIVE | opt1 | opt2);

    if (sock -> sin_addr.s_addr == 0) {
	if ((hp = gethostbyname ("localhost")) == NULL) {
	    errno = EADDRNOTAVAIL;
	    return NOTOK;
	}
	sock -> sin_family = hp -> h_addrtype;
	inaddr_copy (hp, sock);
    }

    for (port = IPPORT_RESERVED;; port++) {
	sock -> sin_port = htons ((u_short) port);

	if ((sd = socket (SOCK_STREAM, 0, (struct sockaddr *) sock,
		    SO_ACCEPTCONN | SO_KEEPALIVE | opt1 | opt2)) != NOTOK)
	    return sd;

	switch (errno) {
	    case EADDRINUSE: 
		continue;

	    case EADDRNOTAVAIL: 
	    default: 
		return NOTOK;
	}
    }
}

#endif

/*    GETHOSTENT PLUS */

static char *empty = NULL;
#ifdef	h_addr
static char *addrs[2] = { NULL };
#endif

struct hostent *gethostbystring (s)
char   *s;
{
    register struct hostent *h;
#ifndef	DG
    static u_long iaddr;
#else
    static struct in_addr iaddr;
#endif
    static struct hostent   hs;

    iaddr = inet_addr (s);
#ifndef	DG
    if (iaddr == NOTOK)
#else
    if (iaddr.s_addr == NOTOK)
#endif
	return gethostbyname (s);

    h = &hs;
    h -> h_name = s;
    h -> h_aliases = &empty;
    h -> h_addrtype = AF_INET;
    h -> h_length = sizeof (iaddr);
#ifdef	h_addr
    h -> h_addr_list = addrs;
    bzero ((char *) addrs, sizeof addrs);
#endif
    h -> h_addr = (char *) &iaddr;

    return h;
}

/*    AT&T UNIX: 5 with EXOS 8044 TCP/IP card */

#ifdef	EXOS

long	rhost ();
char   *raddr ();


struct hostent *gethostbyaddr (addr, len, type)
char   *addr;
int	len,
	type;
{
    long    iaddr;
    char   *name;
    static char buffer[BUFSIZ];
    static struct hostent   hs;
    register struct hostent *h = &hs;

    if (len != sizeof (long) || type != AF_INET)
	return NULL;
    bcopy (addr, (char *) &iaddr, len);
    if ((name = raddr (iaddr)) == NULL)
	return NULL;

    (void) strcpy (buffer, name);
    free (name);

    h -> h_name = buffer;
    h -> h_aliases = &empty;
    h -> h_addrtype = type;
    h -> h_length = len;
    h -> h_addr = addr;

    return h;
}


struct hostent *gethostbyname (name)
char   *name;
{
    static long iaddr;
    static char buffer[BUFSIZ];
    static struct hostent   hs;
    register struct hostent *h = &hs;

    if ((iaddr = rhost (&name)) == NOTOK)
	return NULL;

    (void) strcpy (buffer, name);
    free (name);

    h -> h_name = buffer;
    h -> h_aliases = &empty;
    h -> h_addrtype = AF_INET;
    h -> h_length = sizeof (iaddr);
    h -> h_addr = (char *) &iaddr;

    return h;

}

/*  */

/* really only need the "tsap" entry in this table... but why not? */

static struct servent   services[] = {
    "tsap", NULL, 102, "tcp",
    "miscellany", NULL, 17000, "lpp",

    "echo", NULL, 7, "tcp",		/* Network standard functions */
    "echo", NULL, 7, "udp",
    "sink", NULL, 9, "tcp",
    "sink", NULL, 9, "udp",
    "users", NULL, 11, "tcp",
    "users", NULL, 11, "udp",
    "daytime", NULL, 13, "tcp",
    "daytime", NULL, 13, "udp",
    "netstat", NULL, 15, "tcp",
    "netstat", NULL, 15, "udp",
    "qotd", NULL, 17, "tcp",
    "qotd", NULL, 17, "udp",
    "chargen", NULL, 19, "tcp",
    "chargen", NULL, 19, "udp",
    "ftp", NULL, 21, "tcp",
    "telnet", NULL, 23, "tcp",
    "smtp", NULL, 25, "tcp",
    "imagen", NULL, 35, "udp",
    "time", NULL, 37, "tcp",
    "time", NULL, 37, "udp",
    "name", NULL, 42, "tcp",
    "name", NULL, 42, "udp",
    "whois", NULL, 43, "tcp",
    "whois", NULL, 43, "udp",
    "nameserver", NULL, 53, "tcp",
    "nameserver", NULL, 53, "udp",
    "mtp", NULL, 57, "tcp",
    "hostnames", NULL, 101, "tcp",
    "pop", NULL, 109, "tcp",
    "pwdgen", NULL, 129, "tcp",
    "pwdgen", NULL, 129, "udp",
    "x25bridge", NULL, 146, "tcp",
    "iso-ip", NULL, 147, "udp",

    "tftp", NULL, 69, "udp",		/* Host specific functions */
    "rje", NULL, 77, "tcp",
    "nmui", NULL, 77, "udp",
    "finger", NULL, 79, "tcp",
    "finger", NULL, 79, "udp",
    "link", NULL, 87, "tcp",
    "supdup", NULL, 95, "tcp",
    "path", NULL, 117, "tcp",

    "exec", NULL, 512, "tcp",		/* UNIX TCP sockets */
    "login", NULL, 513, "tcp",
    "shell", NULL, 514, "tcp",
    "printer", NULL, 515, "tcp",
    "rfile", NULL, 522, "tcp",
    "ingreslock", NULL, 1524, "tcp",

    "biff", NULL, 512, "udp",		/* UNIX UDP sockets */
    "who", NULL, 513, "udp",
    "syslog", NULL, 514, "udp",
    "talk", NULL, 517, "udp",
    "routed", NULL, 520, "udp",
    "router_1", NULL, 521, "udp",

    NULL, &empty, 0, NULL
};



struct servent *getservbyname (name, proto)
register char   *name,
		*proto;
{
    register struct servent *s;

    for (s = services; s -> s_name; s++)
	if (strcmp (name, s -> s_name) == 0
		&& strcmp (proto, s -> s_proto) == 0) {
	    if (s -> s_aliases == NULL) {
		s -> s_aliases = &empty;
		s -> s_port = htons ((u_short) s -> s_port);
	    }

	    return s;
	}

    return NULL;
}

/*  */

#define	s2a(b)	(((int) (b)) & 0xff)

char   *inet_ntoa (in)
struct in_addr in;
{
    register char  *s = (char *) &in;
    static char addr[4 * 3 + 3 + 1];

    (void) sprintf (addr, "%d.%d.%d.%d",
	    s2a (s[0]), s2a (s[1]), s2a (s[2]), s2a (s[3]));

    return addr;
}


u_long	inet_addr (cp)
char   *cp;
{
    register int    base;
    register char   c;
    register u_long val;
    u_long	parts[4];
    register u_long *pp = parts;

    for (;;) {
	val = 0, base = 10;
	if (*cp == '0')
	    base = 8, cp++;
	if (*cp == 'x' || *cp == 'X')
	    base = 16, cp++;

	for (; isxdigit ((u_char) (c = *cp)); cp++)
	    if (base == 16)
		val = (val << 4) + (c + 10 - (islower ((u_char) c) ? 'a' : 'A'));
	    else
		if (isdigit ((u_char) c))
		    val = (val * base) + (c - '0');
		else
		    break;

	switch (*cp) {
	    case '.': 
		if (pp >= parts + 4)
		    return NOTOK;
		*pp++ = val, cp++;
		continue;

	    default: 
		if (*cp && !isspace ((u_char) *cp))
		    return NOTOK;
		*pp++ = val;
		break;
	}

	break;
    }

    switch (pp - parts) {
	case 1:
	    val = parts[0];
	    break;

	case 2:
	    val = ((parts[0] & 0xff) << 24)
			| (parts[1] & 0xffffff);
	    break;

	case 3:
	    val = ((parts[0] & 0xff) << 24)
			| ((parts[1] & 0xff) << 16)
			| (parts[2] & 0xffff);
	    break;

	case 4:
	    val = ((parts[0] & 0xff) << 24)
			| ((parts[1] & 0xff) << 16)
			| ((parts[2] & 0xff) << 8)
			| (parts[3] & 0xff);
	    break;

	default:
	    return NOTOK;
    }

    return htonl (val);
}

u_long	inet_network (cp)
char   *cp;
{
    register int    base;
    register char   c;
    register u_long val;
    u_long	parts[4];
    register u_long *pp = parts;

    for (;;) {
	val = 0, base = 10;
	if (*cp == '0')
	    base = 8, cp++;
	if (*cp == 'x' || *cp == 'X')
	    base = 16, cp++;

	for (; isxdigit ((u_char) (c = *cp)); cp++)
	    if (base == 16)
		val = (val << 4) + (c + 10 - (islower ((u_char) c) ? 'a' : 'A'));
	    else
		if (isdigit ((u_char) c))
		    val = (val * base) + (c - '0');
		else
		    break;

	switch (*cp) {
	    case '.': 
		if (pp >= parts + 4)
		    return NOTOK;
		*pp++ = val, cp++;
		continue;

	    default: 
		if (*cp && !isspace ((u_char) *cp))
		    return NOTOK;
		*pp++ = val;
		break;
	}

	break;
    }

    switch (pp - parts) {
	case 1:
	    val = (parts[0] & 0xff) << 24;
	    break;

	case 2:
	    val = ((parts[0] & 0xff) << 24)
			| ((parts[1] & 0xff) << 16);
	    break;

	case 3:
	    val = ((parts[0] & 0xff) << 24)
			| ((parts[1] & 0xff) << 16)
			| ((parts[2] & 0xff) << 8)
	    break;

	case 4:
	    val = ((parts[0] & 0xff) << 24)
			| ((parts[1] & 0xff) << 16)
			| ((parts[2] & 0xff) << 8)
			| (parts[3] & 0xff);
	    break;

	default:
	    return NOTOK;
    }

    return htonl (val);
}
#endif
#endif
