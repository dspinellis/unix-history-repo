/*
 * This file implements functions for dealing with names in the XNS
 * environment.
 */

/*
 $Log:	names.c,v $
 * Revision 2.0  85/11/21  07:22:14  jqj
 * 4.3BSD standard release
 * 
 * Revision 1.2  85/10/21  12:58:41  root
 * Gould version:  eliminate various uses of ns_netof(), which doesn't
 * work on Gould.
 * 
 * Revision 1.1  85/10/17  10:13:45  bill
 * Initial revision
 * 
 * Revision 1.3  85/03/11  16:37:13  jqj
 * Public alpha-test version, released 11 March 1985
 * 
 * Revision 1.2  85/01/27  07:37:32  jqj
 * finished but undebugged version
 * 
 */

#ifndef lint
static char rcsid[] = "$Header: names.c,v 2.0 85/11/21 07:22:14 jqj Exp $";
#endif

#include <stdio.h>
#include <sys/types.h>		/* for ns.h and socket.h */
#include <sys/socket.h>		/* for ns.h */
#include <netns/ns.h>		/* for XNS addresses */
#include <netdb.h>



/*ARGSUSED*/
struct ns_addr *
ClearinghouseLookup(name,field)
	char *name, *field;
{
	/* not yet implemented */
	return(NULL);
}

struct ns_addr *
getXNSaddr(name)
	char *name;
{
	static struct ns_addr addr;
	long net,net1;
	int i;
	int hb[6];
	u_short socket;
	char *netname, *hostname, *socketname;
	extern char *index();

	netname = name;
	if (NULL == (hostname = index(name,'#'))) {
		/* no # means just a host name */
		netname = "0"; hostname = name; socketname = "0";
	}
	else if (NULL == (socketname = index(++hostname,'#')))
		/* one # means net#host */
		socketname = "0";
	else
		/* two # means net#host#socket */
		socketname += 1;

	*(u_long *)&addr.x_net = 0;
	for (i = 0; i < 6; i++)
		addr.x_host.c_host[i] = (char) 0;
	addr.x_port = 0;
	/*
	 * first try 2-273#2-852-151-014#socket
	 */
	if (1 < (i = sscanf(hostname,"%d-%d-%d-%d-%d#",
			&hb[0], &hb[1], &hb[2], &hb[3], &hb[4]))) {
		cvtbase(1000,256,hb,i,addr.x_host.c_host,6);
		i = sscanf(netname,"%ld-%ld#", &net, &net1);
		if (i > 1)
			net = net*1000+net1;
		i = sscanf(socketname,"%hd", &socket);
		*(u_long *)&addr.x_net = htonl(net);
		addr.x_port = htons(socket);
		return(&addr);
	}
	/*
	 * try form 8E1#0.0.AA.0.5E.E6#socket
	 */
	else if (1 < (i = sscanf(hostname,"%x.%x.%x.%x.%x.%x",
			&hb[0], &hb[1], &hb[2], &hb[3], &hb[4], &hb[5]))) {
		cvtbase(256,256,hb,i,addr.x_host.c_host,6);
		i = sscanf(netname,"%lx", &net);
		i = sscanf(socketname,"%hx", &socket);
		*(u_long *)&addr.x_net = htonl(net);
		addr.x_port = htons(socket);
		return(&addr);
	}
	/* code for alternate forms here */
	return(NULL);	/* no match */
}

static
cvtbase(oldbase,newbase,input,inlen,result,reslen)
	int oldbase, newbase;
	int input[];
	int inlen;
	unsigned char result[];
	int reslen;
{
	int d, e;
	long sum;

	e = 1;
	while (e > 0 && reslen > 0) {
		d = 0; e = 0; sum = 0;
		/* long division: input=input/newbase */
		while (d < inlen) {
			sum = sum*oldbase + (long) input[d];
			e += (sum > 0);
			input[d++] = sum / newbase;
			sum %= newbase;
		}
		result[--reslen] = sum;	/* accumulate remainder */
	}
	for (d=0; d < reslen; d++)
		result[d] = 0;
}


/*ARGSUSED*/
struct hostent *
getXNShostbyname (name)
	char *name;
{
/* can't use gethostent() since only internet addresses are understood */
	return(NULL);
}



struct ns_addr *
CourierName( name )
	char *name;
{
	struct hostent *haddr;
	struct ns_addr *paddr;

					/* first, try an explicit address */
					/* like 3#1.2.3.4.5.6#5 */
	if ((paddr = getXNSaddr(name)) != NULL)
		return(paddr);
					/* second, try a local cache lookup */
	if ((haddr = getXNShostbyname(name)) != NULL) {
		return((struct ns_addr*) haddr->h_addr);
	}
					/* finally, try a nonlocal lookup */
	return( ClearinghouseLookup(name,(char*)NULL) );
}
