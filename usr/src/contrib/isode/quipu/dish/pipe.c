/* pipe.c - */

#ifndef	lint
static char *rcsid = "$Header: /f/osi/quipu/dish/RCS/pipe.c,v 7.2 91/02/22 09:40:48 mrose Interim $";
#endif

/* 
 * $Header: /f/osi/quipu/dish/RCS/pipe.c,v 7.2 91/02/22 09:40:48 mrose Interim $
 *
 *
 * $Log:	pipe.c,v $
 * Revision 7.2  91/02/22  09:40:48  mrose
 * Interim 6.8
 * 
 * Revision 7.1  90/07/09  14:47:21  mrose
 * sync
 * 
 * Revision 7.0  89/11/23  22:20:17  mrose
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


#include <signal.h>
#include <stdio.h>
#include <errno.h>
#include "general.h"

#ifdef SOCKETS
#include "internet.h"
#endif
#include <sys/ioctl.h>
#ifdef	BSD42
#include <sys/file.h>
#endif
#ifdef SYS5
#include <fcntl.h>
#endif
#include <sys/stat.h>
#include "tailor.h"

#ifdef SOCKETS
int             sd, sd_current;
#else
char 		retpipe[LINESIZE];
int             fd, wfd;
#endif

#ifndef MIN
#define MIN(a,b) (( (b) < (a) ) ? (b) : (a) )
#endif

int parent_pid;
extern int	errno;		

init_pipe ()
{
	char parent [BUFSIZ];
	char   *cp;

#ifdef SOCKETS
	struct sockaddr_in      sin_buf;
	struct sockaddr_in      *sin = &sin_buf;
#else
#endif

	if ((cp = getenv ("DISHPARENT")) == NULLCP) {
		(void) sprintf (parent, "%d", getppid ());
		(void) setenv ("DISHPARENT", cp = parent);
	}


	if (sscanf (cp, "%d", &parent_pid) != 1) {
		(void) fprintf (stderr,"DISHPARENT malformed");
		return (NOTOK);
	}

#ifdef SOCKETS
	if (get_dish_sock (sin) != 0)
		return (NOTOK);

	if ((sd = start_tcp_server (sin, SOMAXCONN, 0, 0)) == NOTOK) {
	    perror ("start_tcp_server");
	    return NOTOK;
        }
#ifdef	FIOCLEX
	(void) ioctl (sd, FIOCLEX, NULLCP);
#endif
#else		

	if ((cp = getenv ("DISHPROC")) == NULL) {
		(void) fprintf (stderr, "no DISHPROC in environment\n");
		return (NOTOK);
	}

	(void) strcpy (retpipe, cp);
	(void) umask (0);

	if ((fd = open (retpipe, O_RDONLY)) < 0) {
		(void) mknod (retpipe, S_IFIFO | 0600, 0);
		if ((fd = open (retpipe, O_RDONLY)) < 0) {
			(void) fprintf (stderr, "ropen failed\n");
			(void) unlink (retpipe);
			return (NOTOK);
		}
	}

	if ((wfd = open (retpipe, O_WRONLY)) < 0) {
		(void) fprintf (stderr, "wr open failed\n");
		(void) unlink (retpipe);
		(void) close (fd);
		return (NOTOK);
	}
#endif

#ifdef	SETSID
	(void) setsid ();
#endif
#ifdef	TIOCNOTTY
	{
	    int	    xsd;

	    if ((xsd = open ("/dev/tty", O_RDWR)) != NOTOK) {
		(void) ioctl (xsd, TIOCNOTTY, NULLCP);
		(void) close (xsd);
	    }
	}
#else
#ifdef	SYS5
	(void) setpgrp ();
	(void) signal (SIGINT, SIG_IGN);
	(void) signal (SIGQUIT, SIG_IGN);
#endif
#endif
	return (OK);
}

exit_pipe ()
{
#ifdef SOCKETS
	(void) close_tcp_socket (sd);
#else
	(void) close (fd);
	(void) close (wfd);
	(void) unlink (retpipe);
#endif
}

read_pipe (buf,len)
char * buf;
int len;
{
#ifdef SOCKETS
	struct sockaddr_in sock;

	while ((sd_current = join_tcp_client (sd, &sock)) == NOTOK) {
		if (errno != EINTR) {
			perror("join_tcp_client");
			return (-1);
		}
	}
#ifdef	FIOCLEX
	(void) ioctl (sd_current, FIOCLEX, NULLCP);
#endif
#endif
	return (read_pipe_aux(buf,len));
}


read_pipe_aux (buf,len)
char * buf;
int len;
{
	int res;
#ifdef	SOCKETS
	register char *cp,
		      *ep;
#endif

	*buf = '\0';
#ifdef SOCKETS
	ep = (cp = buf) + len - 1;
	for (;;) {
	    switch (res = recv (sd_current, cp, ep - cp, 0)) {
		case NOTOK:
		    perror ("recv");
		    (void) close (sd_current);
		    return NOTOK;

		case OK:
		    break;

		default:
		    cp += res - 1;
		    if (*cp == '\n')
			break;
		    if (++cp < ep)
			continue;
		    break;
	    }
	    break;
	}
	*cp = NULL;

	return (cp - buf);
#else
	if ((res = read (fd, buf, len)) <= 0) {
		perror ("read error");
		reopen_ret ();
		return (-1);
	}
	*(buf + res) = 0;

	return (res);
#endif
}


#ifdef	SOCKETS
int	read_pipe_aux2 (buf, len)
char  **buf;
int    *len;
{
    int	    cc,
	    i,
	    j,
	    res;
    register char   *cp,
		    *dp,
		    *ep;
    char    buffer[BUFSIZ];

    *buf = NULL, *len = 0;

    switch (res = read_pipe_aux (buffer, sizeof buffer)) {
        case NOTOK:
        case OK:
	    return res;

	case 1:
	    *buf = buffer, *len = res;
	    return res;
		
	default:
	    if (sscanf (buffer + 1, "%d", &cc) != 1 || cc < 0) {
		(void) fprintf (stderr, "protocol botch\n");
		return NOTOK;
	    }
	    if ((cp = malloc ((unsigned) cc + 1)) == NULL) {
		perror ("malloc");
		return NOTOK;
	    }
	    *buf = cp, *len = cc;

	    dp = cp, j = cc;
	    if (ep = index (buffer + 1, '\n')) {
		(void) strcpy (dp, ++ep);
		i = strlen (ep);
		dp += i, j -= i;
	    }
	    break;
    }
	
    for (; j > 0; dp += i, j -= i)
	switch (i = recv (sd_current, dp, j, 0)) {
	    case NOTOK:
	        perror ("recv");
out: ;
		free (cp);
		*buf = NULL, *len = 0;
		(void) close (sd_current);
		return NOTOK;

	    case OK:
		(void) fprintf (stderr, "premature eof from peer\n");
		goto out;

	    default:
		break;
	}
	*dp = NULL;

	return res;
}
#endif


#ifndef	SOCKETS
send_pipe (buf)
char * buf;
{
	send_pipe_aux (buf);

	(void) close (file);
	reopen_ret ();
}
#endif

send_pipe_aux (buf)
char * buf;
{
    send_pipe_aux2 (buf, strlen (buf));
}

send_pipe_aux2 (buf, i)
char   *buf;
int	i;
{
int res;

#ifndef	SOCKETS
	if ((file = open (inbuf, O_WRONLY)) <= 0) {
		(void) fprintf (stderr, "error %s on %s\n",sys_errname (errno), inbuf);
		reopen_ret ();
		return;
	}
#endif

	while (i > 0) {
#ifdef	SOCKETS
		if ( (res= send(sd_current, buf, i, 0)) == -1) {
			perror("send");
			(void) close (sd_current);
			return;
		}
#else
		if ((res = write (file, buf, MIN (BUFSIZ,i))) == -1 ) {
			(void) fprintf (stderr,"result write error (2)\n");
			reopen_ret ();
			return;
		}
#endif
		buf += res, i -= res;
	}
}


#ifdef SOCKETS
get_dish_sock (isock)
struct sockaddr_in *isock;
{
	char * getenv ();
	char * ptr;
	char buffer [BUFSIZ];
	int     portno;
	char   *dp;
	register struct hostent *hp;

	if ((ptr = getenv ("DISHPROC")) == NULLCP) {
#ifdef	notanymore
	        char   *cp,
#endif
		portno = (getppid () & 0xffff) | 0x8000;
#ifdef	notanymore
		if ((hp = gethostbystring (cp = getlocalhost ())) == NULL) {
			(void) fprintf (stderr,"%s: unknown host", cp);
			return (-1);
		}
		(void) sprintf (buffer, "%s %d",
				inet_ntoa (*(struct in_addr *) hp -> h_addr),
				portno);
#else
		(void) sprintf (buffer, "127.0.0.1 %d", portno);
#endif
		(void) setenv ("DISHPROC", ptr = buffer);
	}

	if ((dp = index (ptr, ' ')) == NULLCP || sscanf (dp + 1, "%d", &portno) != 1) {
		(void) fprintf (stderr,"DISHPROC malformed");
		return (-1);
	}
	*dp = NULL;

	if ((hp = gethostbystring (ptr)) == NULL) {
		(void) fprintf (stderr,"%s: unknown host in DISHPROC", ptr);
		return (-1);
	}
	*dp = ' ';

	bzero ((char *) isock, sizeof *isock);
	isock -> sin_family = hp -> h_addrtype;
	isock -> sin_port = htons ((u_short) portno);
	inaddr_copy (hp, isock);

	return (0);

}

#else

reopen_ret ()
{
	(void) close (fd);
	(void) close (wfd);

	if ((fd = open (retpipe, O_RDONLY)) < 0) {
		if ( errno == EINTR ) {
			reopen_ret ();
			return;
		}
		(void) fprintf (stderr, "re-ropen failed\n");
		(void) unlink (retpipe);
		exit (-72);
	}
	if ((wfd = open (retpipe, O_WRONLY)) < 0) {
		(void) fprintf (stderr, "re-wr open failed\n");
		(void) unlink (retpipe);
		(void) close (fd);
		exit (-73);
	}
}
#endif
