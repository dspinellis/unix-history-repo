/*  $Revision: 1.12 $
**
**  Routines for the local connect channel.  Create a Unix-domain stream
**  socket that processes on the local server connect to.  Once the
**  connection is set up, we speak NNTP.  The connect channel is used only
**  by rnews to feed in articles from the UUCP sites.
*/
#include "innd.h"
#if	defined(DO_HAVE_UNIX_DOMAIN)
#include <sys/un.h>

STATIC char	LCpath[] = _PATH_NNTPCONNECT;
STATIC CHANNEL	*LCchan;


/*
**  Read function.  Accept the connection and create an NNTP channel.
*/
STATIC FUNCTYPE
LCreader(cp)
    CHANNEL	*cp;
{
    int		fd;
    CHANNEL	*new;

    if (cp != LCchan) {
	syslog(L_ERROR, "%s internal LCreader wrong channel 0x%x not 0x%x",
	    LogName, cp, LCchan);
	return;
    }

    if ((fd = accept(cp->fd, (struct sockaddr *)NULL, (int *)NULL)) < 0) {
	syslog(L_ERROR, "%s cant accept CCreader %m", LogName);
	return;
    }
    new = NCcreate(fd, FALSE);
    new->Address.s_addr = MyAddress.s_addr;
    syslog(L_NOTICE, "%s connected %d", "localhost", new->fd);
}


/*
**  Write-done function.  Shouldn't happen.
*/
STATIC FUNCTYPE
LCwritedone()
{
    syslog(L_ERROR, "%s internal LCwritedone", LogName);
}
#endif	/* defined(DO_HAVE_UNIX_DOMAIN) */


/*
**  Create the channel.
*/
void
LCsetup()
{
#if	defined(DO_HAVE_UNIX_DOMAIN)
    int			i;
    struct sockaddr_un	server;

    /* Remove old detritus. */
    if (unlink(LCpath) < 0 && errno != ENOENT) {
	syslog(L_FATAL, "%s cant unlink %s %m", LogName, LCpath);
	exit(1);
    }

    /* Create a socket and name it. */
    if ((i = socket(AF_UNIX, SOCK_STREAM, 0)) < 0) {
	syslog(L_FATAL, "%s cant socket %s %m", LogName, LCpath);
	exit(1);
    }
    (void)memset((POINTER)&server, 0, sizeof server);
    server.sun_family = AF_UNIX;
    (void)strcpy(server.sun_path, LCpath);
    if (bind(i, (struct sockaddr *)&server, AF_UNIX_SOCKSIZE(server)) < 0) {
	syslog(L_FATAL, "%s cant bind %s %m", LogName, LCpath);
	exit(1);
    }

    /* Set it up to wait for connections. */
    if (listen(i, MAXLISTEN) < 0) {
	syslog(L_FATAL, "%s cant listen %s %m", LogName, LCpath);
	exit(1);
    }
    LCchan = CHANcreate(i, CTlocalconn, CSwaiting, LCreader, LCwritedone);
    syslog(L_NOTICE, "%s lcsetup %s", LogName, CHANname(LCchan));
    RCHANadd(LCchan);
#endif	/* defined(DO_HAVE_UNIX_DOMAIN) */
}


/*
**  Cleanly shut down the channel.
*/
void
LCclose()
{
#if	defined(DO_HAVE_UNIX_DOMAIN)
    CHANclose(LCchan, CHANname(LCchan));
    LCchan = NULL;
    if (unlink(LCpath) < 0)
	syslog(L_ERROR, "%s cant unlink %s %m", LogName, LCpath);
#endif	/* defined(DO_HAVE_UNIX_DOMAIN) */
}
