/*  $Revision: 1.7 $
**
**  TCP forwarder.  Connect to a server on a host.  Send our stdin to the
**  server, and send server's output down our stdout.  Low-cost version
**  of rsh.  Can also be spawned out of inetd, e.g., to do forwarding.
**  Descended from code written by Jim Thompson <Jim.Thompson@central.sun.com>.
**  For example, when doing an "rcompress", add this to /etc/services:
**	compress        2001/tcp
**  and add this to /etc/inetd.conf on the server:
**	compress stream tcp nowait root /usr/ucb/compress in.compressd -c
*/
#include <stdio.h>
#include <ctype.h>
#include <netdb.h>
#include <sys/types.h>
#include <sys/socket.h>
#include <netinet/in.h>


#define BUFFER_SIZE	8192
/*
#define DEFAULT_HOST		"news.foo.com"
#define DEFAULT_SERVICE		"nntp"
#define DEFAULT_INETLOSS	1
*/
#define DEFAULT_HOST		"geraldo"
#define DEFAULT_SERVICE		"compress"
#define DEFAULT_INETLOSS	0


extern int		optind;
extern char		*optarg;

extern char		*malloc();
extern char		*memcpy();
extern char		*memset();
extern unsigned long	inet_addr();
extern struct servent	*getservbyname();
extern struct hostent	*gethostbyname();
extern void		exit();
extern void		perror();


/*
**  Open a socket to the specified service.
*/
int
OpenConnection(host, service)
    char			*host;
    char			*service;
{
    char			**ap;
    char			*fakelist[2];
    register int		i;
    struct servent		*sp;
    struct hostent		*hp;
    struct hostent		fakehp;
    struct in_addr		quadaddr;
    struct sockaddr_in		server;

    if ((sp = getservbyname(service, "tcp")) == NULL) {
	perror("Unknown service");
	return -1;
    }

    /* Get the host's address. */
    quadaddr.s_addr = inet_addr(host);
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
    else if ((hp = gethostbyname(host)) != NULL) {
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
    else {
	perror("Unknown host");
	return -1;
    }

    /* Set up the socket address. */
    (void)memset((char *)&server, 0, sizeof server);
    server.sin_family = hp->h_addrtype;
    server.sin_port = sp->s_port;

    /* Loop through the address list, trying to connect. */
    for (; ap && *ap; ap++) {
        /* Make a socket and try to connect. */
	if ((i = socket(hp->h_addrtype, SOCK_STREAM, 0)) < 0)
	    break;
	(void)memcpy((char *)&server.sin_addr, (char *)*ap, (int)hp->h_length);
	if (connect(i, (struct sockaddr *)&server, sizeof server) == 0)
	    return i;
	(void)close(i);
    }
    perror("Can't connect");
    return -1;
}


/*
**  Loop until the entire buffer is written.
*/
int
xwrite(fd, p, i)
    register int	fd;
    register char	*p;
    register int	i;
{
    register int	count;

    for ( ; i > 0; p += count, i -= count)
	if ((count = write(fd, p, i)) < 0)
	    return -1;
    return 0;
}


int
main(ac, av)
    int		ac;
    char	*av[];
{
    static char		USAGE[] = "Usage error\n";
    int			s;
    int			inetdloss;
    register int	i;
    char		*host;
    char		*service;
    char		*p;

    /* Set defaults. */
    host = DEFAULT_HOST;
    service = DEFAULT_SERVICE;
    inetdloss = DEFAULT_INETLOSS;

    /* Parse JCL. */
    while ((i = getopt(ac, av, "h:is:")) != EOF)
	switch (i) {
	default:
	    (void)write(1, USAGE, sizeof USAGE - 1);
	    exit(1);
	case 'h':
	    host = optarg;
	    break;
	case 'i':
	    inetdloss = 1;
	    break;
	case 's':
	    service = optarg;
	    break;
	}
    ac -= optind;
    if (ac) {
	(void)write(1, USAGE, sizeof USAGE - 1);
	exit(1);
    }

    if (inetdloss) {
	/* Work around broken inetd's. */
	(void)close(1);
	(void)dup(0);
    }

    /* Get a connection. */
    if ((s = OpenConnection(host, service)) < 0)
	exit(1);

    /* Get a buffer. */
    if ((p = malloc(BUFFER_SIZE)) == NULL) {
	perror("can't allocate buffer");
	exit(1);
    }

    switch (fork()) {
    case -1:
	perror("Can't fork");
	exit(1);
    case 0:
	/* Child -- send server output to our stdout. */
	while ((i = read(s, p, BUFFER_SIZE)) > 0)
	    if (xwrite(1, p, i) < 0) {
		perror("Can't write to client");
		break;
	    }
	break;
    default:
	/* Parent -- send our stdin to server. */
	while ((i = read(0, p, BUFFER_SIZE)) > 0)
	    if (xwrite(s, p, i) < 0) {
		perror("Can't write to server");
		break;
	    }
	(void)shutdown(s, 1);
	break;
    }

    exit(0);
    /* NOTREACHED */
}
