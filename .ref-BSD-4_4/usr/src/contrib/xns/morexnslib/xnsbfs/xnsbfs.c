/* $Header: xnsbfs.c,v 1.4 87/01/16 15:46:52 ed Exp $ */

/*
 * $Log:	xnsbfs.c,v $
 * Revision 1.4  87/01/16  15:46:52  ed
 * Webster changes
 * 
 * Revision 1.4  87/01/16  15:46:52  ed
 * Added exit(0) for normal exit
 * 
 * Revision 1.3  86/12/16  15:29:56  ed
 * Add -a option to perform broadcast for Authentication servers
 * 
 * Revision 1.2  86/07/29  06:39:27  jqj
 * added some comments and code cleanup in main routine.
 * 
 * Revision 1.1  86/06/27  13:14:38  jqj
 * Initial revision
 * 
 */

/*
 * XNS broadcast for servers:
 *	syntax:  xnsbfs -a [xnshostaddress]
 *
 * With -a option, broadcast for Authentication servers,
 *		   else Clearinghouse servers
 * With no arguments, sends a broadcast query to all directly connected XNS
 *	networks looking for Clearinghouse servers
 * With an argument, sends a query to the particular address specified.
 *
 * Cornell University
 * Dept. of Computer Science
 *
 */
 

#include <sys/types.h>
#include <sys/time.h>
#include <sys/socket.h>
#include <sys/ioctl.h>
#include <net/if.h>
#include <netns/ns.h>
#include <netns/idp.h>
#include <signal.h>

#include <syslog.h>
#include <stdio.h>
#include <errno.h>

#include "pex.h"

#define DEFAULTNET 2269
#define BUFLEN 1024
#define BFSPEXClientType 2
#define BFSClearinghouseSocket 20
#define BFSClearinghouseProgram 2
#define BFSClearinghouseVersion 2
#define BFSAuthenticationSocket 21
#define BFSAuthenticationProgram 14
#define BFSAuthenticationVersion 2

extern char *ns_ntoa();
int authlookup= 0;

static int s;
static int rnum = 1;	/* PEX transaction ID */
static union {
	u_long c_long;
	u_short c_short[2];
} converter;

struct xnsifdata {
	struct sockaddr_ns myaddr;
	struct sockaddr_ns bcstaddr;
};

sendrequest(s,who,prognum,vernum)
	int s;
	struct sockaddr_ns *who;
	u_long prognum;
	u_short vernum;
{
	struct pex *pex;
	struct CourierData {
		u_short courierlow;
		u_short courierhigh;
		u_short couriermsgtype;
		u_short tid;
		u_short program[2];
		u_short version;
		u_short procval;
	} *cd;	
	char buf[576];
	int len;
	
	pex = (struct pex *)&buf[0];
	cd = (struct CourierData *)&buf[/* sizeof struct pex */ 6 ];
	
	pex->ph_idh = 0;
	pex->ph_idl = htons(rnum++);
	pex->ph_client = htons(BFSPEXClientType);
	
	cd->courierlow = cd->courierhigh = htons(3);
	cd->couriermsgtype = cd->tid = 0;
	converter.c_long = htonl(prognum);	
	cd->program[0] = converter.c_short[0];
	cd->program[1] = converter.c_short[1];
	cd->version = htons(vernum);
	cd->procval = 0;
	
	len = sizeof (*cd) + /* sizeof(*pex) */ 6;
	if ( sendto(s, (char*)buf, len, 0, who, sizeof (*who)) < 0) {
		perror("sendto");
	}
}

#define NUMIFS 20
main(argc, argv)
	int argc;
	char *argv[];
{
	struct ns_addr * daddr;
	struct xnsifdata baddrlist[NUMIFS];
	extern struct ns_addr *getXNSaddr();
	extern FILE *outfile; int i, numif;

	outfile = stdout;

	numif = localbcst(baddrlist);
	if (argc <= 1)
		for (i = 0; i < numif; i++)
		 	bfs(baddrlist[i]);
	else for (i=1; i < argc; i++)
		if ( strcmp(argv[i], "-a") == 0 ) {
			authlookup++;
			if (argc == 2) {
				for (i = 0; i < numif; i++)
				 	bfs(baddrlist[i]);
			}
		} else if ((daddr = getXNSaddr(argv[i])) != (struct ns_addr *) NULL) {
			baddrlist[i].bcstaddr.sns_family = AF_NS;
			if (daddr->x_host.s_host[0] == 0 &&
			    daddr->x_host.s_host[1] == 0 &&
			    daddr->x_host.s_host[2] == 0) {
			    	daddr->x_host.s_host[0] = 0xFFFF;
				daddr->x_host.s_host[1] = 0xFFFF;
				daddr->x_host.s_host[2] = 0xFFFF;
			}
	     		baddrlist[i-1].bcstaddr.sns_addr = *daddr;
	     		baddrlist[i-1].myaddr.sns_family = AF_NS;
			bfs(baddrlist[i-1]);
		}

	exit(0);
}



bfs(ifdata)
	struct xnsifdata ifdata;
{
	struct sockaddr_ns me, you, who;
	struct idp *idp;
	struct pex *pex;
	u_short *data;
	struct ns_addr *srvr;
	char	buf[576];
	int yoursize;
	int m,n,clienttype;
	int bytesReceived;
	static int on = 1;
	int mask;
	struct timeval tval;
	u_long prognum;
	u_short vernum;
	
	if ((s = socket(AF_NS, SOCK_DGRAM, 0)) < 0) {
		perror("socket");
		exit(1);
	}
	
	idp = (struct idp *)&buf[0];
	pex = (struct pex *) &buf[ sizeof (struct idp)];
	data = (u_short *) &buf[sizeof (struct idp) +  /*sizeof (struct pex)*/ 6];
	
	me = ifdata.myaddr;
	me.sns_port = htons(getpid()%1000+3000); /* so we don't have to be root */
	if (bind(s, &me, sizeof (me)) < 0 ) {
		perror("bind:");
		exit(1);
	}
	if (setsockopt(s, 0, SO_HEADERS_ON_INPUT, &on, sizeof(on))) {
		perror("setsockopt SEE HEADERS:");
		exit(1);
	}
	idp->idp_pt = NSPROTO_PE;
	if (setsockopt(s, 0, SO_DEFAULT_HEADERS, idp, sizeof(struct idp))) {
		perror("setsockopt SET HEADER:");
		exit(1);
	}
	if (setsockopt(s, SOL_SOCKET, SO_BROADCAST, &on, sizeof (on)) < 0) {
		perror("setsockopt SO_BROADCAST:");
		exit(1);
	}

	who = ifdata.bcstaddr;
	if (authlookup) {
		who.sns_port = htons(BFSAuthenticationSocket);
		prognum= BFSAuthenticationProgram;
		vernum= BFSAuthenticationVersion;
	} else {
		who.sns_port = htons(BFSClearinghouseSocket);
		prognum= BFSClearinghouseProgram;
		vernum= BFSClearinghouseVersion;
	}
	
	sendrequest(s,&who, prognum, vernum);
	tval.tv_sec = 3;
	tval.tv_usec = 0;
	mask = 1<<s;
	while ((select(20, &mask, 0, 0, &tval) > 0)
			&& (mask & 1<<s) ) {
		fflush(stdout);

		yoursize = sizeof (you);
		if ((n = recvfrom(s, (char *) buf, sizeof(buf), 0, &you, &yoursize)) < 0) {
			extern int errno;
			
			if (errno != EINTR)
				perror("rcvfrom:");
			continue;
		}
		tval.tv_sec = 3;
		tval.tv_usec = 0;
		mask = 1<<s;
	
		if ( idp->idp_pt != NSPROTO_PE ) {
			fprintf(stderr,"idp_pt = %d ?\n", idp->idp_pt);
			continue;
		}
		
		converter.c_short[0] = pex->ph_idh;
		converter.c_short[1] = pex->ph_idl;
		m = htonl(converter.c_long);
		clienttype = ntohs(pex->ph_client);	
		bytesReceived = n - sizeof (struct idp) - /*sizeof (struct pex)*/ 6;
		if ( clienttype != BFSPEXClientType)
			continue;
			
		/* got a BFS reply packet, figure out what it is */
		/* figure out size of packet */
		if (ntohs(data[2]) == 2 /* result */ ) {
			srvr = (struct ns_addr *)&data[6];
		}

		printdomains(you.sns_addr);

	} /* listen for more */
	close(s);
}		


/*
 * return the broadcast address (including network number) of the local network. 
 * We consider only broadcast nets, though a reasonable extension would be to
 * send a point-to-point BFS to the host at the other end of a p-to-p link.
 */	
localbcst(results)
	struct xnsifdata results[];
{
	int s, n, numinterfaces, numxnsifs;
	struct ifconf ifc;
	char buf[sizeof(struct ifreq) * (NUMIFS-1)]; /* array of structures */
	struct ifreq *ifr;
	extern char *malloc();
	
	if ((s = socket(AF_NS, SOCK_DGRAM, 0)) < 0) {
		perror("socket:");
		exit(1);
	}
	ifc.ifc_len = sizeof (buf);
        ifc.ifc_buf = buf;
        if (ioctl(s, SIOCGIFCONF, (char *)&ifc) < 0) {
		perror("ioctl SIOCGIFCONF:");
		numinterfaces = 0;
	} else
		numinterfaces = ifc.ifc_len / sizeof (struct ifreq);
	for (n = 0, ifr = ifc.ifc_req, numxnsifs = 0;
	     n < numinterfaces && numxnsifs < NUMIFS-1;
	     n++, ifr++) {
		/* no one cares about software loopback interfaces */
		/* we don't care about non-XNS addressess either */
		if (strncmp(ifr->ifr_name,"lo", 2)==0 ||
		    ifr->ifr_addr.sa_family != AF_NS)
			continue;
		results[numxnsifs].myaddr = 
				*(struct sockaddr_ns *) &ifr->ifr_addr;
		/* get IF flags */
                if (ioctl(s, SIOCGIFFLAGS, (char *)ifr) < 0) {
			perror("ioctl SIOCGIFFLAGS:");
                        continue;
		}
		if ((ifr->ifr_flags & IFF_UP) == 0 ||
		    (ifr->ifr_flags & IFF_BROADCAST) == 0) 
			continue;
		/* get the broadcast address */
		if (ioctl(s, SIOCGIFBRDADDR, (char *)ifr) < 0) {
			perror("ioctl SIOCGIFBRDADDR:");
			continue;
		}
		bcopy(  &ifr->ifr_broadaddr,
			&results[numxnsifs++].bcstaddr,
			sizeof(struct sockaddr_ns) );
	}
	close(s);
	return numxnsifs;
}
