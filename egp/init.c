/* init.c  */

/* EGP User Process, ISI 23-Jun-84 */

/* Initialization routines */


#include "include.h"


/* Determine addresses and names of internet interfaces configured.
/* Results returned in global linked list of interface tables.
/* The interface names are required for later ioctl calls re interface status.
 *
 * External variables:
 * ifnet - pointer to interface list
 * n_interfaces - number of direct internet interfaces (set here) 
*/ 
int lookforinterfaces;		/* for later */

init_if()
{
	int	i,
		j,
		s,
		n_intf;
	struct in_addr	   addr;
	int	bufsize;
	struct ifreq  *ifreq_buf;	/* buffer for receiving interace
					 * configuration info from ioctl */
	struct ifconf ifconf_req;
	struct interface *ifp;
	u_long a;
	struct sockaddr_in *sin;

/* Determine internet addresses of all internet interfaces, except
/* ignore loopback interface.
*/

/* First must open a temporary socket to get a valid socket descriptor for
/* ioctl call.
*/

	if((s = getsocket(AF_INET, SOCK_DGRAM, 0, 0)) < 0)
		quit();

/* get interface configuration */
					/* allocate buffer for config. if
					 * insufficient double size */
	bufsize = 5*sizeof(struct ifreq)+1; /* ioctl assumes > size ifreq */
	for(;;) {
		ifreq_buf = (struct ifreq *)malloc(bufsize);
		if( ifreq_buf == 0) {
			printf("init_if: malloc: out of memory\n");
			quit();
		}
		ifconf_req.ifc_len = bufsize; /*sizeof(ifreq_buf);*/
		ifconf_req.ifc_req = ifreq_buf;

		if( ioctl(s, SIOCGIFCONF, (char *)&ifconf_req) ) {
			p_error("init_if:ioctl SIOCGIFCONF:\n");
			quit();
		}
					/* if spare buffer space for at least
					 * one more interface all found */
		if( (bufsize - ifconf_req.ifc_len) > sizeof (struct ifreq)) 
			break;
					/* else double buffer size and retry*/
		free((char *)ifreq_buf);
		if( bufsize > 40*sizeof(struct ifreq)) {
			printf("init_if: more than 39 interfaces\n");
			quit();
		}
		bufsize<<=1;
	}

	n_intf = ifconf_req.ifc_len/sizeof(struct ifreq); /* number
								interfaces */
	j=0;			/* internet interface index */
	for(i=0; i<n_intf ;i++){
		if(ifreq_buf[i].ifr_addr.sa_family != AF_INET)
			continue;

		addr.s_addr = ((struct sockaddr_in *)&ifreq_buf[i].ifr_addr)
						       ->sin_addr.s_addr;

		ifp = (struct interface *)malloc(sizeof (struct interface));
		if( ifp == 0) {
			printf("init_if: malloc: out of memory\n");
			quit();
		}
				/* save name for future ioctl calls */
		ifp->int_name = malloc(strlen(ifreq_buf[i].ifr_name) + 1);
		if( ifp->int_name == 0) {
			printf("init_if: malloc: out of memory\n");
			quit();
		}
		strcpy(ifp->int_name, ifreq_buf[i].ifr_name);
		ifp->int_addr = ifreq_buf[i].ifr_addr;
		ifp->int_metric = 0;
		if (ioctl(s, SIOCGIFFLAGS, (char *)&ifreq_buf[i]) < 0) {
			perror("ioctl (get netmask)");
			free ((char *) ifp);
			continue;
		}
		ifp->int_flags = ifreq_buf[i].ifr_flags | IFF_INTERFACE;
		/* no one cares about software loopback interfaces */
		if (ifp->int_flags & IFF_LOOPBACK)
			continue;
		if ((ifp->int_flags & IFF_UP) == 0 ||
		    ifp->int_addr.sa_family == AF_UNSPEC) {
			lookforinterfaces = 1;
			free ((char *) ifp);
			continue;
		}
		if (ioctl(s, SIOCGIFNETMASK, (char *)&ifreq_buf[i]) < 0) {
			perror("ioctl (get netmask)");
			free ((char *) ifp);
			continue;
		}
		sin = (struct sockaddr_in *)&ifreq_buf[i].ifr_addr;
		ifp->int_subnetmask = ntohl(sin->sin_addr.s_addr);
		sin = (struct sockaddr_in *)&ifp->int_addr;
		a = ntohl(sin->sin_addr.s_addr);
		if (IN_CLASSA(a))
			ifp->int_netmask = IN_CLASSA_NET;
		else if (IN_CLASSB(a))
			ifp->int_netmask = IN_CLASSB_NET;
		else
			ifp->int_netmask = IN_CLASSC_NET;
		ifp->int_net = a & ifp->int_netmask;
		ifp->int_subnet = a & ifp->int_subnetmask;
		ifp->int_next = ifnet;
		ifnet = ifp;

		TRACE_INT("init_if: interface name: %s, address %s\n", 
			ifp->int_name, inet_ntoa(addr.s_addr));/**/


/* if multiple interfaces with same net number exit as the rpoting tables
 * assume at most one interface per net.
 */
		for(ifp=ifnet->int_next;ifp!=0;ifp=ifp->int_next)
		    if(ifnet->int_subnet == ifp->int_subnet) {
			printf("init_if: Exit as multiple ");
			printf("interfaces to net of interface %s\n",
				inet_ntoa(addr.s_addr));
			quit();
		    };

		j++;
	}
	n_interfaces = j;	/* number internet interfaces */

	if(n_interfaces == 0){
		printf("if_init: Exit no interfaces\n");
		quit();
	}

	free((char *)ifreq_buf);
	close(s);
}

/* Open raw sockets for sending and receiving packets on each interface.
 *
 * External variables:
 * ifnet - interface list
 */ 

init_sock()
{
register struct	interface *ifp;
	struct	sockaddr_in *addr;

/* open socket on each interface */

	for(ifp=ifnet;ifp!=0;ifp=ifp->int_next){
		addr = (struct sockaddr_in *)&ifp->int_addr;
		addr->sin_port = 0;

/* Open raw socket for EGP packets */

		ifp->int_egpsock = getsocket(AF_INET, SOCK_RAW,
							addr, IPPROTO_EGP);
		if (ifp->int_egpsock < 0) {
			printf("init_sock: exit as failed to open");
			printf("egp socket\n");
			quit();
		}
/* open raw socket for receiving ICMP messages */

		ifp->int_icmpsock = getsocket(AF_INET, SOCK_RAW, addr,
							      IPPROTO_ICMP);
		if (ifp->int_icmpsock < 0){
			printf("init_sock: exit as failed to open");
			printf("icmp socket\n");
			quit();
		}

	} /* end for */


/* Open raw socket for receiving IMP messages */
/* not implemented */

	return;
}


/* getsocket gets a socket, retries later if ni buffers at present, and binds
 * an address to the socket unless sin is a null pointer.
 */


getsocket(domain, type, sin, protocol)
	int domain, type, protocol;
	struct sockaddr_in *sin;
{
	int retry, s;
	int bufsize = 4 * MAXPACKETSIZE;

	retry = 2;		/* if no buffers a retry might work */
	while ((s = socket(domain, type, protocol)) < 0 && retry--) {
		p_error("getsocket:socket");
		sleep(5);
	}
	if (s < 0)
		return (-1);

	if( sin )
		if (bind(s, sin, sizeof (*sin)) < 0) {
			p_error("getsocket:bind");
			return(-1);
		}

#ifdef SO_RCVBUF
	if (setsockopt(s, SOL_SOCKET, SO_RCVBUF, (char *)&bufsize,
	    sizeof(bufsize)) < 0)
		perror("getsocket: setsockopt SO_RCVBUF");
	bufsize = MAXPACKETSIZE + 1024;
	if (setsockopt(s, SOL_SOCKET, SO_SNDBUF, (char *)&bufsize,
	    sizeof(bufsize)) < 0)
		perror("getsocket: setsockopt SO_SNDBUF");
#endif
	return (s);
}



/* init_egpngh() reads names of EGP neighbors from EGPINITFILE
 * The format is: # comment
 *		  autonomoussystem value
 *		  egpneighbor name
 *		  egpmaxacquire value
 * where name is either a symbolic name in /etc/hosts or an internet address
 * in dot notation and value is the maximum nuber of EGP neighbors to acquire.
 * All EGP neighbors must be on the same net.
 * A state table is allocated for the neighbor and added to a linked list
 * terminated by a NULL pointer.
 *
 * External variables:
 *   mysystem - autonomous sytem number
 *   maxacq - maximum number of EGP neighbors to acquire
 *   nneigh - # egp neighbors
 *   egpngh - pointer to start of linked list of egp neighbor state tables.
 */


init_egpngh(fp)
	FILE *fp;
{
	struct sockaddr_in  addr;
	char name[MAXHOSTNAMELENGTH+1];
	int	error = FALSE,
		c;

	struct egpngh	*ngp,
			*egpngh_last;

	nneigh = 0;
	egpngh = NULL;
	bzero((char *)&addr, sizeof(addr));

	rewind(fp);
	while(fscanf(fp, "%s", name) != EOF) {	/* read first word of line and
						   compare to key words */
	    if( strcmp(name, "autonomoussystem") == 0) {
		if(fscanf(fp, "%d", &mysystem) != 1) error = TRUE;
	    }
	    else if( strcmp(name, "egpmaxacquire") == 0) {
		if(fscanf(fp, "%d", &maxacq) != 1)
		    error = TRUE;
		else
		    TRACE_INT("init_egpngh: egpmaxacquire = %d\n", maxacq);
	    }
	    else if( strcmp(name, "egpneighbor") == 0) {
		if( fscanf(fp, "%s", name) != 1) error = TRUE;
		else if( getnetorhostname("host", name, &addr)) {

		    ngp = (struct egpngh *)malloc( sizeof(struct egpngh));
		    if( ngp == NULL) {
			printf("init_egpngh: malloc: out of memory\n");
			quit();
		    }
				/* add to end of egp neighbor table linked
				   list so order same as in EGPINITFILE,
				   although simpler to add to front.
				 */
		    if( egpngh == NULL) egpngh = ngp;	/* first neighbor */
		    else egpngh_last->ng_next = ngp;
		    egpngh_last = ngp;
		    ngp->ng_next = NULL;
		    ngp->ng_addr = addr.sin_addr.s_addr;
		    nneigh++;
		    TRACE_INT("init_egpngh: EGP neighbor %s\n",
				 inet_ntoa(addr.sin_addr.s_addr));

#if SAMENET			/* check all neighbors on same net */
		    if( nneigh > 1) {
			if( inet_netof(ngp->ng_addr.i_long)
				!= inet_netof(egpngh->ng_addr.i_long) ) {
			    printf("init_egpngh: EGP neighbors on");
			    printf(" different nets\n");
			    error = TRUE;
			}
		    }
#endif
		}		/* end else if getnetorhostname... */

		else {
		    printf("init_egpngh: invalid neighbor name or");
		    printf(" address %s\n", name);
		    error = TRUE;
		}
	    }			/* end else egpneighbor */

	    do c = fgetc(fp); while (c != '\n' && c != EOF); /* next line */

	}			/* end while */

	if(mysystem == 0) {
	    printf("init_egpngh: autonomous system # 0\n");
	    error = TRUE;
	}
	if(maxacq == 0) {
		printf("init_egpngh: egpmaxacquire = 0\n");
		error = TRUE;
	}
	if(maxacq > nneigh) {
		printf("init_egpngh: egpmaxacquire = %d > # neighbors = %d\n",
			maxacq, nneigh);
		error = TRUE;
	}
	if(nneigh==0) {
		printf("init_egpngh: no EGP neighbors\n");
		error = TRUE;
	}
	if(error) {
	    printf("init_egpngh: %s: initialization error\n", EGPINITFILE);
	    quit();
	}

}


/* Given host or net name or internet address in dot notation assign the
 * internet address in byte format.
 * source is ../routed/startup.c with minor changes to detect syntax errors.
 *
 * Unfortunately the library routine inet_addr() does not detect mal formed
 * addresses that have characters or byte values > 255.
 */

getnetorhostname(type, name, sin)
	char *type, *name;
	struct sockaddr_in *sin;
{

	if (strcmp(type, "net") == 0) {
		struct netent *np = getnetbyname(name);
		u_long n;

		if (np == 0) {
			if( (n = inet_network(name)) == -1)
			return(0);
		}
		else {
			if (np->n_addrtype != AF_INET)
				return (0);
			n = np->n_net;
		}
		/*
		 * We use unshifted network numbers internally,
		 * but inet_network and getnetbyname return shifted numbers.
		 */
		if (n) while ((n & 0xff000000) == 0)
			n <<= 8;
		sin->sin_family = AF_INET;
		sin->sin_addr = inet_makeaddr(n, INADDR_ANY);
		return (1);
	}
	if (strcmp(type, "host") == 0) {
		struct hostent *hp = gethostbyname(name);

		if (hp == 0) {
			if( (sin->sin_addr.s_addr = inet_addr(name)) == -1)
				return(0);
		}
		else {
			if (hp->h_addrtype != AF_INET)
				return (0);
			bcopy(hp->h_addr, &sin->sin_addr, hp->h_length);
		}
		sin->sin_family = AF_INET;
		return (1);
	}
	return (0);
}


/* init_egp() initializes the state tables for each potential EGP neighbor
 */

init_egp()
{
	register	egpng	*ngp;
	register	u_short	nn;
		long	time;
		u_short	tmp;
		int	i;
	struct  interface *ifp;
	struct	sockaddr_in	dst;

	egpsleep = MINHELLOINT + HELLOMARGIN;
	rt_maxage = RT_MINAGE;		/* minimum time before routes are
					deleted when not updated */
	getod(&time);

	for (ngp = egpngh; ngp != NULL; ngp = ngp->ng_next) {
		ngp->ng_state = UNACQUIRED;
		ngp->ng_flags = 0;
		ngp->ng_sid = 1;
		ngp->ng_retry = 0;
		ngp->ng_hint = MINHELLOINT + HELLOMARGIN;
		ngp->ng_htime = time;
		ngp->ng_rcmd = 0;

/* Check that I have a direct net to neighbor */

		dst.sin_family = AF_INET;
		dst.sin_addr.s_addr = ngp->ng_addr;

		if( ( ifp = if_withnet(&dst) ) != NULL)
			ngp->ng_myaddr = sock_inaddr(&ifp->int_addr);
		else {
			TRACE_INT("init_egp: no direct net to %s\n",
					inet_ntoa(ngp->ng_addr));
			ngp->ng_state = EMPTY;
		}
	}

/* Debug */
#if ALLOWNONNEIGHBORS
	init_egp2();
#endif

}


/* Testing only, allows peers which do not share a direct net. The source
 * interface is defined by DEFAULTIF in defs.h
 */
#if ALLOWNONNEIGHBORS

init_egp2()
{
	reg	egpng	*ngp;

	printf("init_egp2 called\n");
	for (ngp = egpngh; ngp != NULL; ngp = ngp->ng_next) {
		if (ngp->ng_state != EMPTY) continue;
		ngp->ng_state = UNACQUIRED;
		ngp->ng_myaddr = inet_addr( DEFAULTIF);
	}
}
#endif
