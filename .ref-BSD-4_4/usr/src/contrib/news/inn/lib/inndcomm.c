/*  $Revision: 1.18 $
**
**  Library routines to let other programs control innd.
*/
#include "configdata.h"
#include <stdio.h>
#include <ctype.h>
#include <errno.h>
#include <sys/types.h>
#include <sys/stat.h>
#include <fcntl.h>
#if	defined(DO_NEED_TIME)
#include <time.h>
#endif	/* defined(DO_NEED_TIME) */
#include <sys/time.h>
#include <sys/socket.h>
#if	defined(DO_HAVE_UNIX_DOMAIN)
#include <sys/un.h>
#endif	/* defined(DO_HAVE_UNIX_DOMAIN) */
#include "nntp.h"
#include "paths.h"
#include "libinn.h"
#include "clibrary.h"
#include "inndcomm.h"
#include "macros.h"

#define MIN_BUFFER_SIZE		4096

STATIC char			ICCsockname[sizeof _PATH_TEMPSOCK];
#if	defined(DO_HAVE_UNIX_DOMAIN)
STATIC struct sockaddr_un	ICCserv;
STATIC struct sockaddr_un	ICCclient;
#endif	/* defined(DO_HAVE_UNIX_DOMAIN) */
STATIC int			ICCfd;
STATIC int			ICCtimeout;
char				*ICCfailure;


/*
**  Set the timeout.
*/
void
ICCsettimeout(i)
    int		i;
{
    ICCtimeout = i;
}


/*
**  Get ready to talk to the server.
*/
int
ICCopen()
{
    int		mask;
    int		oerrno;

    /* Create a temporary name. */
    (void)strcpy(ICCsockname, _PATH_TEMPSOCK);
    (void)mktemp(ICCsockname);
    if (unlink(ICCsockname) < 0 && errno != ENOENT) {
	ICCfailure = "unlink";
	return -1;
    }

#if	defined(DO_HAVE_UNIX_DOMAIN)
    /* Make a socket and give it the name. */
    if ((ICCfd = socket(AF_UNIX, SOCK_DGRAM, 0)) < 0) {
	ICCfailure = "socket";
	return -1;
    }
    (void)memset((POINTER)&ICCclient, 0, sizeof ICCclient);
    ICCclient.sun_family = AF_UNIX;
    (void)strcpy(ICCclient.sun_path, ICCsockname);
    mask = umask(0);
    if (bind(ICCfd, (struct sockaddr *)&ICCclient,
		AF_UNIX_SOCKSIZE(ICCclient)) < 0) {
	oerrno = errno;
	(void)umask(mask);
	errno = oerrno;
	ICCfailure = "bind";
	return -1;
    }
    (void)umask(mask);

    /* Name the server's socket. */
    (void)memset((POINTER)&ICCserv, 0, sizeof ICCserv);
    ICCserv.sun_family = AF_UNIX;
    (void)strcpy(ICCserv.sun_path, _PATH_NEWSCONTROL);
#else
    /* Make a named pipe and open it. */
    mask = umask(0);
    if (mkfifo(ICCsockname, 0666) < 0) {
	oerrno = errno;
	(void)umask(mask);
	errno = oerrno;
	ICCfailure = "mkfifo";
	return -1;
    }
    (void)umask(mask);
    if ((ICCfd = open(ICCsockname, O_RDWR)) < 0) {
	ICCfailure = "open";
	return -1;
    }
#endif	/* defined(DO_HAVE_UNIX_DOMAIN) */

    ICCfailure = NULL;
    return 0;
}


/*
**  Close down.
*/
int
ICCclose()
{
    int		i;

    ICCfailure = NULL;
    i = 0;
    if (close(ICCfd) < 0) {
	ICCfailure = "close";
	i = -1;
    }
    if (unlink(ICCsockname) < 0 && errno != ENOENT) {
	ICCfailure = "unlink";
	i = -1;
    }
    return i;
}


/*
**  Get the server's pid.
*/
STATIC PID_T
ICCserverpid()
{
    PID_T		pid;
    FILE		*F;
    char		buff[SMBUF];

    pid = 1;
    if ((F = fopen(_PATH_SERVERPID, "r")) != NULL) {
	if (fgets(buff, sizeof buff, F) != NULL)
	    pid = atoi(buff);
	(void)fclose(F);
    }
    return pid;
}


/*
**  See if the server is still there.  When in doubt, assume yes.
**  Cache the pid since a rebooted server won't know about our pending
**  message.
*/
STATIC BOOL
ICCserveralive(pid)
    PID_T		pid;
{
    if (kill(pid, 0) > 0 || errno != ESRCH)
	return TRUE;
    return FALSE;
}


/*
**  Send an arbitrary command to the server.
*/
int
ICCcommand(cmd, argv, replyp)
    char		cmd;
    char		*argv[];
    char		**replyp;
{
    char		*buff;
    char		*p;
    char		*q;
    char		save;
    int			bufsiz;
    int			i;
#if	!defined(DO_HAVE_UNIX_DOMAIN)
    int			fd;
#endif	/* !defined(DO_HAVE_UNIX_DOMAIN) */
    int			len;
    FDSET		Rmask;
    struct timeval	T;
    PID_T		pid;

    /* Is server there? */
    pid = ICCserverpid();
    if (!ICCserveralive(pid)) {
	ICCfailure = "dead server";
	return -1;
    }

    /* Get the length of the buffer. */
    bufsiz = strlen(ICCsockname) + 1 + 1;
    for (i = 0; (p = argv[i]) != NULL; i++)
	bufsiz += 1 + strlen(p);
    if (bufsiz < MIN_BUFFER_SIZE)
	bufsiz = MIN_BUFFER_SIZE;
    buff = malloc((unsigned int)bufsiz);
    if (buff == NULL) {
	ICCfailure = "malloc";
	return -1;
    }
    if (replyp)
	*replyp = NULL;

    /* Format the message. */
    (void)sprintf(buff, "%s%c%c", ICCsockname, SC_SEP, cmd);
    for (p = buff + strlen(buff), i = 0; (q = argv[i]) != NULL; i++) {
	*p++ = SC_SEP;
	p += strlen(strcpy(p, q));
    }

    /* Send message. */
    ICCfailure = NULL;
    len = p - buff;
#if	defined(DO_HAVE_UNIX_DOMAIN)
    if (sendto(ICCfd, buff, len, 0,
	    (struct sockaddr *)&ICCserv, AF_UNIX_SOCKSIZE(ICCserv)) < 0) {
	DISPOSE(buff);
	ICCfailure = "sendto";
	return -1;
    }
#else
    if ((fd = open(_PATH_NEWSCONTROL, O_WRONLY)) < 0) {
	DISPOSE(buff);
	ICCfailure = "open";
	return -1;
    }
    if (write(fd, buff, len) != len) {
	i = errno;
	DISPOSE(buff);
	(void)close(fd);
	errno = i;
	ICCfailure = "write";
	return -1;
    }
    (void)close(fd);
#endif	/* defined(DO_HAVE_UNIX_DOMAIN) */

    /* Possibly get a reply. */
    switch (cmd) {
    default:
	if (ICCtimeout >= 0)
	    break;
	/* FALLTHROUGH */
    case SC_SHUTDOWN:
    case SC_XABORT:
    case SC_XEXEC:
	DISPOSE(buff);
	return 0;
    }

    /* Wait for the reply. */
    for ( ; ; ) {
	FD_ZERO(&Rmask);
	FD_SET(ICCfd, &Rmask);
	T.tv_sec = ICCtimeout ? ICCtimeout : 120;
	T.tv_usec = 0;
	i = select(ICCfd + 1, &Rmask, (FDSET *)NULL, (FDSET *)NULL, &T);
	if (i < 0) {
	    DISPOSE(buff);
	    ICCfailure = "select";
	    return -1;
	}
	if (i > 0 && FD_ISSET(ICCfd, &Rmask))
	    /* Server reply is there; go handle it. */
	    break;

	/* No data -- if we timed out, return. */
	if (ICCtimeout) {
	    DISPOSE(buff);
	    errno = ETIMEDOUT;
	    ICCfailure = "timeout";
	    return -1;
	}

	if (!ICCserveralive(pid)) {
	    DISPOSE(buff);
	    ICCfailure = "dead server";
	    return -1;
	}
    }

    /* Read the reply. */
    i = RECVorREAD(ICCfd, buff, bufsiz - 1);
    if (i < 0) {
	DISPOSE(buff);
	ICCfailure = "read";
	return -1;
    }
    buff[i] = '\0';

    /* Parse the reply; expected to be like "<exitcode><space><text>" */
    i = 0;
    if (CTYPE(isdigit, buff[0])) {
	for (p = buff; *p && CTYPE(isdigit, *p); p++)
	    continue;
	if (*p) {
	    save = *p;
	    *p = '\0';
	    i = atoi(buff);
	    *p = save;
	}
    }
    if (replyp)
	*replyp = buff;
    else
	DISPOSE(buff);
    return i;
}


/*
**  Send a "cancel" command.
*/
int
ICCcancel(msgid)
    char	*msgid;
{
    char	*args[2];

    args[0] = msgid;
    args[1] = NULL;
    return ICCcommand(SC_CANCEL, args, (char **)NULL);
}


/*
**  Send a "go" command.
*/
int
ICCgo(why)
    char	*why;
{
    char	*args[2];

    args[0] = why;
    args[1] = NULL;
    return ICCcommand(SC_GO, args, (char **)NULL);
}


/*
**  Send a "pause" command.
*/
int
ICCpause(why)
    char	*why;
{
    char	*args[2];

    args[0] = why;
    args[1] = NULL;
    return ICCcommand(SC_PAUSE, args, (char **)NULL);
}


/*
**  Send a "reserve" command.
*/
int
ICCreserve(why)
    char	*why;
{
    char	*args[2];

    args[0] = why;
    args[1] = NULL;
    return ICCcommand(SC_RESERVE, args, (char **)NULL);
}
