/*  $Revision: 1.8 $
**
*/
#include <stdio.h>
#include <errno.h>
#include <sys/types.h>
#include <sys/socket.h>
#include "configdata.h"
#include "paths.h"
#include "clibrary.h"
#include "nntp.h"
#include "macros.h"
#if	defined(DO_HAVE_UNIX_DOMAIN)
#include <sys/un.h>
#endif	/* defined(DO_HAVE_UNIX_DOMAIN) */


/*
**  Open a connection to the local InterNetNews NNTP server and optionally
**  create stdio FILE's for talking to it.  Return -1 on error.
*/
int
NNTPlocalopen(FromServerp, ToServerp, errbuff)
    FILE		**FromServerp;
    FILE		**ToServerp;
    char		*errbuff;
{
#if	defined(DO_HAVE_UNIX_DOMAIN)
    int			i;
    int			j;
    int			oerrno;
    struct sockaddr_un	server;
    FILE		*F;
    char		mybuff[NNTP_STRLEN + 2];
    char		*buff;

    buff = errbuff ? errbuff : mybuff;
    *buff = '\0';

    /* Create a socket. */
    if ((i = socket(AF_UNIX, SOCK_STREAM, 0)) < 0)
	return -1;

    /* Connect to the server. */
    (void)memset((POINTER)&server, 0, sizeof server);
    server.sun_family = AF_UNIX;
    (void)strcpy(server.sun_path, _PATH_NNTPCONNECT);
    if (connect(i, (struct sockaddr *)&server, AF_UNIX_SOCKSIZE(server)) < 0) {
	oerrno = errno;
	(void)close(i);
	errno = oerrno;
	return -1;
    }

    /* Connected -- now make sure we can post. */
    if ((F = fdopen(i, "r")) == NULL) {
	oerrno = errno;
	(void)close(i);
	errno = oerrno;
	return -1;
    }
    if (fgets(buff, sizeof mybuff, F) == NULL) {
	oerrno = errno;
	(void)fclose(F);
	errno = oerrno;
	return -1;
    }
    j = atoi(buff);
    if (j != NNTP_POSTOK_VAL && j != NNTP_NOPOSTOK_VAL) {
	(void)fclose(F);
	/* This seems like a reasonable error code to use... */
	errno = EPERM;
	return -1;
    }

    *FromServerp = F;
    if ((*ToServerp = fdopen(dup(i), "w")) == NULL) {
	oerrno = errno;
	(void)fclose(F);
	errno = oerrno;
	return -1;
    }
    return 0;
#else
    return NNTPconnect(LOOPBACK_HOST, FromServerp, ToServerp, errbuff);
#endif	/* defined(DO_HAVE_UNIX_DOMAIN) */
}
