/*  $Revision: 1.14 $
**
*/
#include <stdio.h>
#include <errno.h>
#include <sys/types.h>
#include <sys/socket.h>
#if	defined(DO_HAVE_UNIX_DOMAIN)
#include <sys/un.h>
#endif	/* defined(DO_HAVE_UNIX_DOMAIN) */
#include <netinet/in.h>
#include <netdb.h>
#include "configdata.h"
#include "nntp.h"
#include "paths.h"
#include "libinn.h"
#include "clibrary.h"


#if	!defined(ntohs) && !defined(NETSWAP)
extern unsigned short	ntohs();
#endif	/* !defined(ntohs) && !defined(NETSWAP) */


/*
**  Open a connection to an NNTP server and create stdio FILE's for talking
**  to it.  Return -1 on error.
*/
int
NNTPconnect(p, FromServerp, ToServerp, errbuff)
    register char	*p;
    FILE		**FromServerp;
    FILE		**ToServerp;
    char		*errbuff;
{
    char		**ap;
    char		*fakelist[2];
    register char	*dest;
    char		mybuff[NNTP_STRLEN + 2];
    char		*buff;
    register int	i;
    register int	j;
    int			oerrno;
    FILE		*F;
    struct hostent	*hp;
    struct hostent	fakehp;
    struct in_addr	quadaddr;
    struct sockaddr_in	server;

    buff = errbuff ? errbuff : mybuff;
    *buff = '\0';
    quadaddr.s_addr = inet_addr(p);
    if (quadaddr.s_addr != (unsigned long)-1) {
	/* Host was specified as a dotted-quad internet address.  Fill in
	 * the parts of the hostent struct that we need. */
	fakehp.h_length = sizeof quadaddr;
	fakehp.h_addrtype = AF_INET;
	hp = &fakehp;
	fakelist[0] = (char *)&quadaddr;
	fakelist[1] = NULL;
	ap = fakelist;
    }
    else if ((hp = gethostbyname(p)) != NULL) {
	/* Symbolic host name. */
#if	defined(h_addr)
	ap = hp->h_addr_list;
#else
	/* Fake up an address list for old systems. */
	fakelist[0] = (char *)hp->h_addr;
	fakelist[1] = NULL;
	ap = fakelist;
#endif	/* defined(h_addr) */
    }
    else
	/* Not a host name. */
	return -1;

    /* Set up the socket address. */
    (void)memset((POINTER)&server, 0, sizeof server);
    server.sin_family = hp->h_addrtype;
    server.sin_port = ntohs(NNTP_PORT);

    /* Loop through the address list, trying to connect. */
    for (; ap && *ap; ap++) {
	/* Make a socket and try to connect. */
	if ((i = socket(hp->h_addrtype, SOCK_STREAM, 0)) < 0)
	    break;
	/* Copy the address via inline memcpy:
	 *	(void)memcpy((POINTER)&server.sin_addr, (POINTER)*ap,
			(int)hp->h_length); */
	p = (char *)*ap;
	for (dest = (char *)&server.sin_addr, j = hp->h_length; --j >= 0; )
	    *dest++ = *p++;
	if (connect(i, (struct sockaddr *)&server, sizeof server) < 0) {
	    oerrno = errno;
	    (void)close(i);
	    errno = oerrno;
	    continue;
	}

	/* Connected -- now make sure we can post. */
	if ((F = fdopen(i, "r")) == NULL) {
	    oerrno = errno;
	    (void)close(i);
	    errno = oerrno;
	    continue;
	}
	if (fgets(buff, sizeof mybuff, F) == NULL) {
	    oerrno = errno;
	    (void)fclose(F);
	    errno = oerrno;
	    continue;
	}
	j = atoi(buff);
	if (j != NNTP_POSTOK_VAL && j != NNTP_NOPOSTOK_VAL) {
	    (void)fclose(F);
	    /* This seems like a reasonable error code to use... */
	    errno = EPERM;
	    break;
	}

	*FromServerp = F;
	if ((*ToServerp = fdopen(dup(i), "w")) == NULL) {
	    oerrno = errno;
	    (void)fclose(F);
	    errno = oerrno;
	    continue;
	}
	return 0;
    }

    return -1;
}



#if	defined(REM_INND)

int
NNTPremoteopen(FromServerp, ToServerp, errbuff)
    FILE		**FromServerp;
    FILE		**ToServerp;
    char		*errbuff;
{
    char		*p;

    if ((p = GetConfigValue(_CONF_SERVER)) == NULL) {
	if (errbuff)
	    (void)strcpy(errbuff, "What server?");
	return -1;
    }
    return NNTPconnect(p, FromServerp, ToServerp, errbuff);
}

#endif	/* defined(REM_INND) */



#if	defined(REM_NNTP)


/*
**  Open a connection to an NNTP server using the "clientlib" routines in
**  the NNTP distribution.  We also create stdio FILE's for talking over
**  the connection (which is easy since clientlib has them as globals.)
**  Return -1 on error.
*/
int
NNTPremoteopen(FromServerp, ToServerp, buff)
    FILE		**FromServerp;
    FILE		**ToServerp;
    char		*buff;
{
    extern FILE		*ser_rd_fp;
    extern FILE		*ser_wr_fp;
    extern char		*getserverbyfile();
    char		*p;
    int			i;

    if (buff)
	(void)strcpy(buff, "Text unavailable");
    if ((p = getserverbyfile(_PATH_SERVER)) == NULL)
	return -1;
    if ((i = server_init(p)) < 0)
	return -1;
    if (i != NNTP_POSTOK_VAL && i != NNTP_NOPOSTOK_VAL) {
	errno = EPERM;
	return -1;
    }
    if (ser_rd_fp == NULL || ser_wr_fp == NULL)
	return -1;

    *FromServerp = ser_rd_fp;
    *ToServerp = ser_wr_fp;
    return 0;
}
#endif	/* defined(REM_NNTP) */
