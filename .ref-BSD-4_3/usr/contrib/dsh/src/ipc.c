#include <stdio.h>
#include <sys/types.h>
#include <sys/socket.h>
#include <netinet/in.h>
#include <netdb.h>
#include <errno.h>

#define FIRSTPORT 1425
#define LASTPORT 64000

/* some useful definitions */
#define TRUE	1
#define FALSE	0
#define bool	char
#define HOSTNAMESIZE	32

int	errno;				/* the magic global */
char	ourhostname[HOSTNAMESIZE];	/* local host name */
long	ourhostaddr;			/* our host's address */
bool	havehost = FALSE;		/* true if ourhost* is valid */


/* get info about local host */
getmyhoststuff()
{
    char	*temp;

    gethostname (ourhostname, HOSTNAMESIZE);
    netaddr (ourhostname, &ourhostaddr);
    havehost = TRUE;
}

/* return our host's name */
char *
myhostname()
{
    if (!havehost) {
	getmyhoststuff ();
    }
    return ourhostname;
}

/* return our host's address */
myhostaddr()
{
    if (!havehost) {
	getmyhoststuff ();
    }
    return ourhostaddr;
}


/*
 *	make a datagram socket
 */
makedgsocket (pptr)
int *pptr;		/* port number */
{
    int		sock, i, rv;
    struct sockaddr_in	sin;

    /* set up INET address */
    sin.sin_family = AF_INET;
    sin.sin_addr.s_addr = myhostaddr();
    bzero (sin.sin_zero, 8);

    /* get a socket */
    sock = socket (AF_INET, SOCK_DGRAM, 0, 0);
    if (sock < 0) {
	error ("opening datagram socket");
    }
    if (*pptr == 0) {
	for (*pptr = FIRSTPORT; *pptr < LASTPORT; *pptr += 11) {
	    sin.sin_port = htons((u_short)*pptr);
	    rv = bind (sock, &sin, sizeof(sin), 0);
	    if (rv == 0) {
		break;
	    }
	}
    } else {
	sin.sin_port = htons((u_short)*pptr);
	rv = bind (sock, &sin, sizeof(sin), 0);
    }
    if (rv != 0) {
	close (sock);
	error ("binding datagram socket");
    }

    return (sock);
}


/* get the network address of a machine */
long
netaddr (name, addrptr)
char *name;
char *addrptr;
{
    long	iaddr;
    struct hostent *hp, *gethostbyname();

    hp = gethostbyname(name);
    bcopy (hp->h_addr, addrptr, hp->h_length);
}

/* are these two names synonymns? */
aresynonyms (name1, name2)
char *name1, *name2;
{
    struct hostent *hp, *gethostbyname();
    char **ptr;

    hp = gethostbyname(name1);
    if (hp == 0)
	return (FALSE);
    if (strcmp (name2, hp->h_name) == 0)
	return (TRUE);
    for (ptr = hp->h_aliases; *ptr != 0; ptr++) {
	if (strcmp (name2, *ptr) == 0)
	    return (TRUE);
    }
    return (FALSE);
}

/* receive a datagram via the inet */
recvdg (fd, buf, len)
int fd;			/* socket to receive over */
char * buf;		/* buffer to receive into */
int len;		/* size in bytes of that buffer */
{
    struct sockaddr faddr;
    int flen;
    int rv, i;

    flen = sizeof (faddr);
    rv = recvfrom (fd, buf, len, 0, &faddr, &flen);

    return (rv);
}

/* send an inet datagram */
senddg (fd, buf, len, destname, destport)
int fd;			/* socket to send via */
char *buf;		/* buffer to send */
int len;		/* number of bytes to send */
char *destname;		/* name of host we're sending to */
int destport;		/* the udp port on that node */
{
    struct sockaddr_in sin;
    int	rv, i;
    char *ptr;

    sin.sin_family = AF_INET;
    bzero (sin.sin_zero, 8);
    netaddr (destname, &(sin.sin_addr));
    sin.sin_port = htons ((u_short)destport);
    rv = sendto (fd, buf, len, 0, &sin, sizeof(sin));
    return (rv);
}
