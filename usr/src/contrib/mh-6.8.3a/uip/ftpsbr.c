/* ftpsbr.c - simple FTP client library (why doesn't BSD have one?!?) */

#ifndef	lint
static char ident[] = "@(#)$Id: ftpsbr.c,v 1.13 1993/08/25 17:25:27 jromine Exp $";
#endif

#include "../h/mh.h"
#include "../h/mhn.h"
#ifdef	FTP
#include <ctype.h>
#ifdef SVR4
#undef NULLVP          /* stdio.h */
#endif
#include <stdio.h>
#include <arpa/ftp.h>
#ifdef __STDC__
#include <stdarg.h>
#else
#include <varargs.h>
#endif

#ifdef __STDC__
static int	command(int arg1, ...);
#else
static int	command();
#endif

static int  ftp_quit(), ftp_read(), initconn(),
	    dataconn(), _command(), getreply();

/*    DATA */

#define	v_debug		debugsw
#define	v_verbose	verbosw


static	int	ftp_fd = NOTOK;
static	int	data_fd = NOTOK;

static	int	v_noise;

extern	int	v_debug;
extern	int	v_verbose;

/*  */

#if	defined(SYS5) && defined(AUX)
#define u_short ushort
#define u_long  ulong
#endif

#include <sys/types.h>
#include <sys/socket.h>
#include <netinet/in.h>
#include <netdb.h>

#if	defined(BIND) && !defined(h_addr)
#define	h_addr	h_addr_list[0]
#endif

#define	inaddr_copy(hp,sin) \
    bcopy ((hp) -> h_addr, (char *) &((sin) -> sin_addr), (hp) -> h_length)


struct hostent *gethostbystring ();

/*  */

extern	int	errno;
#ifndef	BSD44
extern	int	sys_nerr;
extern	char   *sys_errlist[];
#endif


#define	start_tcp_client(sock,priv) \
    	socket (AF_INET, SOCK_STREAM, 0)

#define	join_tcp_server(fd, sock) \
    	connect ((fd), (struct sockaddr *) (sock), sizeof *(sock))


/* ARGSUSED */

static int  start_tcp_server (sock, backlog, opt1, opt2)
struct sockaddr_in *sock;
int	backlog,
	opt1,
	opt2;
{
    int	    eindex,
	    sd;

    if ((sd = socket (AF_INET, SOCK_STREAM, 0)) == NOTOK)
	return NOTOK;

    if (bind (sd, (struct sockaddr *) sock, sizeof *sock) == NOTOK) {
	eindex = errno;
	(void) close (sd);
	errno = eindex;
    }
    else
	(void) listen (sd, backlog);

    return sd;
}

static	int	__len__;
#define	join_tcp_client(fd,sock) \
    	accept ((fd), (struct sockaddr *) (sock), \
		(__len__ = sizeof *(sock), &__len__))


#define	read_tcp_socket		read
#define	write_tcp_socket	write
#define	close_tcp_socket	close

/*  */

static void  _asprintf (bp, what, ap)	/* fmt, args, ... */
register char *bp;
char   *what;
va_list	ap;
{
    register int    eindex;
    char   *fmt;

    eindex = errno;

    *bp = '\0';
    fmt = va_arg (ap, char *);

    if (fmt) {
#ifndef	VSPRINTF
	struct _iobuf iob;
#endif

#ifndef	VSPRINTF
#ifdef	pyr
	bzero ((char *) &iob, sizeof iob);
	iob._file = _NFILE;
#endif
	iob._flag = _IOWRT | _IOSTRG;
#if	!defined(vax) && !defined(pyr) && !defined(sequent)
	iob._ptr = (unsigned char *) bp;
#else
	iob._ptr = bp;
#endif
	iob._cnt = BUFSIZ;
	_doprnt (fmt, ap, &iob);
	(void) putc ('\0', &iob);
#else
	(void) vsprintf (bp, fmt, ap);
#endif
	bp += strlen (bp);

    }

    if (what) {
	if (*what) {
	    (void) sprintf (bp, " %s: ", what);
	    bp += strlen (bp);
	}
	if (0 < eindex && eindex < sys_nerr)
	    (void) strcpy (bp, sys_errlist[eindex]);
	else
	    (void) sprintf (bp, "Error %d", eindex);
	bp += strlen (bp);
    }

    errno = eindex;
}

/*  */

int	ftp_get (host, user, password, cwd, remote, local, ascii, stayopen)
char   *host,
       *user,
       *password,
       *cwd,
       *remote,
       *local;
int	ascii,
	stayopen;
{
    return ftp_trans (host, user, password, cwd, remote, local, "RETR", ascii,
		      stayopen);
}

/*  */

int	ftp_trans (host, user, password, cwd, remote, local, cmd, ascii,
		   stayopen)
char   *host,
       *user,
       *password,
       *cwd,
       *remote,
       *local,
       *cmd;
int	ascii,
	stayopen;
{
    int	    result;

    if (stayopen <= 0) {
	result = ftp_quit ();
	if (host == NULL)
	    return result;
    }

    if (ftp_fd == NOTOK) {
	struct sockaddr_in in_socket;
	register struct hostent *hp;
	register struct servent *sp;

	if ((sp = getservbyname ("ftp", "tcp")) == NULL) {
	    fprintf (stderr, "tcp/ftp: unknown service");
	    return NOTOK;
	}
	if ((hp = gethostbystring (host)) == NULL) {
	    fprintf (stderr, "%s: unknown host\n", host);
	    return NOTOK;
	}
	in_socket.sin_family = hp -> h_addrtype;
	inaddr_copy (hp, &in_socket);
	in_socket.sin_port = sp -> s_port;

	if ((ftp_fd = start_tcp_client ((struct sockaddr_in *) NULL, 0))
	        == NOTOK) {
	    perror (host);
	    return NOTOK;
	}
	if (join_tcp_server (ftp_fd, &in_socket) == NOTOK) {
	    perror (host);
	    (void) close_tcp_socket (ftp_fd), ftp_fd = NOTOK;
	    return NOTOK;
	}
	(void) getreply (1, 0);

	if (v_verbose) {
	    fprintf (stdout, "Connected to %s\n", host);
	    (void) fflush (stdout);
	}

	if (user) {
	    if ((result = command (0, "USER %s", user)) == CONTINUE)
		result = command (1, "PASS %s", password);
	    if (result != COMPLETE) {
		result = NOTOK;
		goto out;
	    }
	}

	if (remote == NULL)
	    return OK;
    }

    if (cwd && ((result = command (0, "CWD %s", cwd)) != COMPLETE
		    && result != CONTINUE)) {
	result = NOTOK;
	goto out;
    }

    if (command (1, ascii ? "TYPE A" : "TYPE I") != COMPLETE) {
	result = NOTOK;
	goto out;
    }

    result = ftp_read (remote, local, cmd, ascii);

out: ;
    if (result != OK || !stayopen)
	(void) ftp_quit ();

    return result;
}

/*  */

static int  ftp_quit ()
{
    int	    n;

    if (ftp_fd == NOTOK)
	return OK;

    n = command (1, "QUIT");

    (void) close_tcp_socket (ftp_fd), ftp_fd = NOTOK;

    return (n == 0 || n == COMPLETE ? OK : NOTOK);
}

/*  */

static int  ftp_read (remote, local, cmd, ascii)
char    *remote,
	*local,
	*cmd;
int	ascii;
{
    int	    istdio = 0,
	    istore;
    register int    cc;
    int	    expectingreply = 0;
    char    buffer[BUFSIZ];
    FILE   *fp = NULL;

    if (initconn () == NOTOK)
	goto bad;

    v_noise = v_verbose;
    if (command (-1, *remote ? "%s %s" : "%s", cmd, remote) != PRELIM)
	goto bad;

    expectingreply++;
    if (dataconn () == NOTOK) {
bad: ;
        if (fp && !istdio)
	    (void) fclose (fp);
	if (data_fd != NOTOK)
	    (void) close_tcp_socket (data_fd), data_fd = NOTOK;
	if (expectingreply)
	    (void) getreply (-2, 0);

	return NOTOK;
    }

    istore = !strcmp (cmd, "STOR");

    if (istdio = !strcmp (local, "-"))
	fp = istore ? stdin : stdout;
    else
	if ((fp = fopen (local, istore ? "r" : "w")) == NULL) {
	    perror (local);
	    goto bad;
	}

    if (istore) {
	if (ascii) {
	    int	    c;
	    FILE   *out;

	    if (!(out = fdopen (data_fd, "w"))) {
		perror ("fdopen");
		goto bad;
	    }

	    while ((c = getc (fp)) != EOF) {
		if (c == '\n')
		    (void) putc ('\r', out);
		if (putc (c, out) == EOF) {
		    perror ("putc");
		    (void) fclose (out);
		    data_fd = NOTOK;
		    goto bad;
		}
	    }

	    (void) fclose (out);
	    data_fd = NOTOK;
	}
	else {
	    while ((cc = fread (buffer, sizeof *buffer, sizeof buffer, fp)) >0)
		if (write_tcp_socket (data_fd, buffer, cc) != cc) {
		    perror ("write_tcp_socket");
		    goto bad;
		}

	    (void) close_tcp_socket (data_fd), data_fd = NOTOK;
	}
    }
    else {
	if (ascii) {
	    int	    c;
	    FILE   *in;

	    if (!(in = fdopen (data_fd, "r"))) {
		perror ("fdopen");
		goto bad;
	    }

	    while ((c = getc (in)) != EOF) {
		if (c == '\r')
		    switch (c = getc (in)) {
		        case EOF:
		        case '\0':
		            c = '\r';
			    break;

			case '\n':
			    break;

			default:
			    (void) putc ('\r', fp);
			    break;
			}

		if (putc (c, fp) == EOF) {
		    perror ("putc");
		    (void) fclose (in);
		    data_fd = NOTOK;
		    goto bad;
		}
	    }

	    (void) fclose (in);
	    data_fd = NOTOK;
	}
	else {
	    while ((cc = read_tcp_socket (data_fd, buffer, sizeof buffer)) > 0)
		if (fwrite (buffer, sizeof *buffer, cc, fp) == 0) {
		    perror ("fwrite");
		    goto bad;
		}
	    if (cc < 0) {
		perror ("read_tcp_socket");
		goto bad;
	    }

	    (void) close_tcp_socket (data_fd), data_fd = NOTOK;
	}
    }

    if (!istdio)
	(void) fclose (fp);

    v_noise = v_verbose;
    return (getreply (1, 0) == COMPLETE ? OK : NOTOK);
}

/*  */

static int  initconn ()
{
    int	    len;
    register char  *a,
		   *p;
    struct sockaddr_in in_socket;

    if (getsockname (ftp_fd, (struct sockaddr *) &in_socket,
		     (len = sizeof in_socket, &len)) == NOTOK) {
	perror ("getsockname");
	return NOTOK;
    }
    in_socket.sin_port = 0;
    if ((data_fd = start_tcp_server (&in_socket, 1, 0, 0)) == NOTOK) {
	perror ("start_tcp_server");
	return NOTOK;
    }

    if (getsockname (data_fd, (struct sockaddr *) &in_socket,
		     (len = sizeof in_socket, &len)) == NOTOK) {
	perror ("getsockname");
	return NOTOK;
    }

    a = (char *) &in_socket.sin_addr;
    p = (char *) &in_socket.sin_port;

#define	UC(b)	(((int) b) & 0xff)
    if (command (1, "PORT %d,%d,%d,%d,%d,%d",
		      UC(a[0]), UC(a[1]), UC(a[2]), UC(a[3]),
		      UC(p[0]), UC(p[1])) == COMPLETE)
	return OK;

    return NOTOK;
}

/*  */

static int  dataconn ()
{
    int	    fd;
    struct sockaddr_in in_socket;
    
    if ((fd = join_tcp_client (data_fd, &in_socket)) == NOTOK) {
	perror ("join_tcp_client");
	return NOTOK;
    }
    (void) close_tcp_socket (data_fd);
    data_fd = fd;

    return OK;
}

/*  */

#ifndef	lint
#ifdef __STDC__
static int  command (int arg1, ...)
{
    int	    val;
    va_list ap;

    va_start (ap, arg1);

    val = _command (arg1, ap);

    va_end (ap);

    return val;
}
#else
static int  command (va_alist)
va_dcl
{
    int	    val;
    va_list ap;

    va_start (ap);

    val = va_arg (ap, int);
    val = _command (val, ap);

    va_end (ap);

    return val;
}
#endif

static int  _command (complete, ap)
int complete;
va_list ap;
{
    int	    len;
    char    buffer[BUFSIZ];

    if (ftp_fd == NOTOK)
	return NOTOK;

    _asprintf (buffer, NULLCP, ap);
    if (v_debug)
	fprintf (stderr, "<--- %s\n", buffer);

    (void) strcat (buffer, "\r\n");
    len = strlen (buffer);

    if (write_tcp_socket (ftp_fd, buffer, len) != len) {
	perror ("write_tcp_socket");
	return NOTOK;
    }

    return (getreply (complete, !strcmp (buffer, "QUIT")));
}
#else
/* VARARGS2 */

static int  command (complete, fmt)
int	complete;
char   *fmt;
{
    return command (complete, fmt);
}
#endif

/*  */

static int  getreply (complete, expecteof)
int	complete,
	expecteof;
{
    for (;;) {
	register int code,
		     dig,
		     n;
	int	continuation;
	register char *bp;
	char    buffer[BUFSIZ];

	code = dig = n = continuation = 0;
	bp = buffer;

	for (;;) {
	    char    c;

	    if (read_tcp_socket (ftp_fd, &c, 1) < 1) {
		if (expecteof)
		    return OK;

		perror ("read_tcp_socket");
		return DONE;
	    }
	    if (c == '\n')
		break;
	    *bp++ = c != '\r' ? c : '\0';

	    dig++;
	    if (dig < 4) {
		if (isdigit(c))
		    code = code * 10 + (c - '0');
		else				/* XXX: naughty FTP... */
		    if (isspace (c))
			continuation++;
	    }
	    else
		if (dig == 4 && c == '-')
		    continuation++;
	    if (n == 0)
		n = c;
	}

	if (v_debug)
	    fprintf (stderr, "---> %s\n", buffer);
	if (continuation)
	    continue;

	n -= '0';

	if (v_noise) {
	    fprintf (stdout, "%s\n", buffer);
	    (void) fflush (stdout);
	    v_noise = 0;
	}
	else
	    if ((complete == -1 && n != PRELIM)
		    || (complete == 0 && n != CONTINUE && n != COMPLETE)
		    || (complete == 1 && n != COMPLETE))
		fprintf (stderr, "%s\n", buffer);

	return n;
    }
}
#endif
