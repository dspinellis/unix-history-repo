#ifndef	BSD42
#undef	TTYD
#endif

#ifdef	TTYD
/* ttyw.c - the writer */

#include <errno.h>
#include <stdio.h>
#include <strings.h>
#include <sys/types.h>
#include <sys/socket.h>
#include <netinet/in.h>
#include <netdb.h>
#ifndef	hpux
#include <arpa/inet.h>
#endif
#include "ttyd.h"
#include "ttym.c"

/*  */

ttyw (command, host, line, user)
char   *command,
       *host,
       *line,
       *user;
{
    int	    privd,
            sd;
    unsigned    times;
    char        buffer[BUFSIZ];
    struct	hostent *hp;
    struct	servent *sp;
    struct sockaddr_in  tty_socket,
                       *tsock = &tty_socket;

    if (command == NULL) {
	errno = EINVAL;
	return NOTOK;
    }

    if ((sp = getservbyname ("ttyserver", "tcp")) == NULL) {
	errno = ENETDOWN;
	return NOTOK;
    }
    if (host == NULL)
	(void) gethostname (host = buffer, sizeof buffer);
    if ((hp = gethostbyname (host))==NULL) {
	errno = ENETDOWN;
	return NOTOK;
    }

    if (line && strncmp (line, "/dev/", strlen ("/dev/")) == 0)
	line += strlen ("/dev/");

    privd = *command >= 'A' && *command <= 'Z';/* crude */

/*  */

    for (times = 1; times <= 16; times *= 2) {
	if ((sd = getport (0, privd)) == NOTOK)
	    return NOTOK;

	bzero ((char *) tsock, sizeof *tsock);
	tsock -> sin_family = hp -> h_addrtype;
	tsock -> sin_port = sp -> s_port;
	bcopy (hp -> h_addr, (char *) &tsock -> sin_addr, hp -> h_length);

	if (connect (sd, (struct sockaddr *) tsock, sizeof *tsock) == NOTOK) {
	    (void) close (sd);
	    if (errno == ECONNREFUSED || errno == EINTR) {
		sleep (times);
		continue;
	    }
	    break;
	}

	ttym (sd, command, line, user, NULL);
	if (ttyv (sd) == NOTOK || ttyv (sd) == NOTOK) {
	    (void) close (sd);
	    errno = EPERM;	/* what else??? */
	    return NOTOK;
	}
	else
	    return sd;
    }

    return NOTOK;
}

/*  */

static int  getport (options, privd)
unsigned    options;
int     privd;
{
    int     sd,
            port;
    struct sockaddr_in  unx_socket,
                       *usock = &unx_socket;

    if ((sd = socket (AF_INET, SOCK_STREAM, 0)) == NOTOK)
	return sd;

    if (options & SO_DEBUG)
	(void) setsockopt (sd, SOL_SOCKET, SO_DEBUG, NULL, 0);
    (void) setsockopt (sd, SOL_SOCKET, SO_KEEPALIVE, NULL, 0);

    if (!privd)
	return sd;

    usock -> sin_family = AF_INET;
    usock -> sin_addr.s_addr = INADDR_ANY;

    for (port = IPPORT_RESERVED - 1; port > IPPORT_RESERVED / 2; port--) {
	usock -> sin_port = htons (port);

	switch (bind (sd, (struct sockaddr *) usock, sizeof *usock)) {
	    case NOTOK: 
		if (errno != EADDRINUSE && errno != EADDRNOTAVAIL)
		    return NOTOK;
		continue;

	    default: 
		return sd;
	}
    }

    return NOTOK;
}
#endif	TTYD
