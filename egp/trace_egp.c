/*	@(#)trace_egp.c	1.2 (Berkeley) 12/13/88 */

/* EGP User Process, ISI 23-Jun-84 */


#include "include.h"
#include <sys/resource.h>


traceaction(fd, action, rt)
	FILE *fd;			/* trace logfile */
	char *action;
	struct rt_entry *rt;
{
	struct sockaddr_in *dst, *gate;
	static struct bits {
		int	t_bits;
		char	*t_name;
	} flagbits[] = {
		{ RTF_UP,	"UP" },
		{ RTF_GATEWAY,	"GATEWAY" },
		{ RTF_HOST,	"HOST" },
		{ 0 }
	}, statebits[] = {
		{ RTS_PASSIVE,	"PASSIVE" },
		{ RTS_REMOTE,	"REMOTE" },
		{ RTS_INTERFACE,"INTERFACE" },
		{ RTS_CHANGED,	"CHANGED" },
		{ RTS_NOTINSTALL,"NOTINSTALL"},
		{ RTS_NOTADVISENR,"NOTADVISENR"},
		{ 0 }
	};
	register struct bits *p;
	register int first;
	char *cp;
	struct interface *ifp;

	if (fd == NULL)
		return;
	fprintf(fd, "%s ", action);
	dst = (struct sockaddr_in *)&rt->rt_dst;
	gate = (struct sockaddr_in *)&rt->rt_router;
	fprintf(fd, "dst %s, ", inet_ntoa(dst->sin_addr));
	fprintf(fd, "router %s, metric %d, flags",
	     inet_ntoa(gate->sin_addr), rt->rt_metric);
	cp = " %s";
	for (first = 1, p = flagbits; p->t_bits > 0; p++) {
		if ((rt->rt_flags & p->t_bits) == 0)
			continue;
		fprintf(fd, cp, p->t_name);
		if (first) {
			cp = "|%s";
			first = 0;
		}
	}
	fprintf(fd, ", state");
	cp = " %s";
	for (first = 1, p = statebits; p->t_bits > 0; p++) {
		if ((rt->rt_state & p->t_bits) == 0)
			continue;
		fprintf(fd, cp, p->t_name);
		if (first) {
			cp = "|%s";
			first = 0;
		}
	}

	putc('\n', fd);
	fflush(fd);
}


/* trace packets received */
/* Only trace data part - EGP packets include IP header but ICMP packets do
   not.
*/

tracercv(sock, protocol, packet, length, frompt)
	int sock, protocol, length;
	struct sockaddr_in *frompt;
	u_char packet[];
{
    struct sockaddr_in  sockname;
    int  socklen = sizeof (sockname),
	 i,
	 time;
    char *strtime;
    struct ip *pkt;
    int hlen;

    pkt = (struct ip *)packet;
    getod(&time);
    strtime = ctime(&time);
    switch (protocol) {
	case IPPROTO_EGP: 
	    hlen = pkt->ip_hl;
	    hlen <<= 2;			/* IP header length in octets */
					/* note 4.2 BSD uses IP data length,
					   not total length, internally */
	    TRACE_EGPPKT(EGP RECV, pkt->ip_src.s_addr, pkt->ip_dst.s_addr,
			packet+hlen, pkt->ip_len);
	    break;

	case IPPROTO_ICMP: 	/* IP header not passed with ICMP messages
				   */
	case IMPLINK_IP: 
	    if (getsockname (sock, &sockname, &socklen))
		p_error ("tracercv:getsockname");
	    printf ("Packet received at time %s", strtime);
	    printf("protocol %d, from addr %s",
		    protocol, inet_ntoa (frompt -> sin_addr));
	    printf (", on interface addr %s\n",
		    inet_ntoa (sockname.sin_addr));

	    printf ("Data octets (decimal):\n");
	    for (i = 0; i < length; i++)
		printf ("%d ", packet[i]);
	    printf ("\n\n");
	    break;
    }

    fflush(stdout);
    return;
}


/* Trace EGP packet */

traceegp( comment, src, dst, egp, length)
	u_char  *comment;
	u_long	src,	    /* if NULL omit src address */
		dst;
	struct	egppkt	*egp;
	int	length;
{
	reg	u_char	*data,
			*dataf;
	char	*strtime;
	int	time;
	struct	egppkt	*ep;

	getod(&time);
	strtime = ctime(&time);
	printf("%s", comment);
	if( src) printf(" %s", inet_ntoa( src));
	printf(" -> %s, len %d, %s", inet_ntoa( dst), length, strtime);

	printf("vers %d, typ %d, code %d, status %d, AS# %d, id %d",
		egp->egp_ver, egp->egp_type, egp->egp_code, egp->egp_status,
		ntohs(egp->egp_system), ntohs(egp->egp_id));
	if( length > sizeof( struct egppkt))
		switch ( egp->egp_type) {
		case EGPACQ:
			printf(", hello int %d, poll int %d", 
				ntohs( ((struct egpacq *)egp)->ea_hint),
				ntohs( ((struct egpacq *)egp)->ea_pint) );
			break;
		case EGPPOLL:
			printf(", src net %s",
				inet_ntoa( ((struct egppoll *)egp)->ep_net));
			break;
		case EGPNR:
			printf(", src net %s, #int %d, #ext %d\n",
				inet_ntoa( ((struct egpnr *)egp)->en_net),
				((struct egpnr *)egp)->en_igw,
				((struct egpnr *)egp)->en_egw );
			if(length > sizeof(struct egpnr)) {
				data = (u_char *)egp + sizeof( struct egpnr);
				dataf = (u_char *)egp + length;
				for( ; data < dataf; data++) 
					printf( "%d ", *data);
			}
			break;
		case EGPERR:
			printf(", error %d\nreturned header: ",
				ntohs( ((struct egperr *)egp)->ee_rsn) );
			ep = (struct egppkt *)((struct egperr*)egp)->ee_egphd;
			printf("vers %d, typ %d, code %d, status %d, AS# %d,",
				ep->egp_ver, ep->egp_type, ep->egp_code,
				ep->egp_status, ntohs(ep->egp_system) );
			printf(" id %d, %d, %d", ntohs(ep->egp_id),
				((struct egperr *)egp)->ee_egphd[10],
				((struct egperr *)egp)->ee_egphd[11] );
			break;
		}
		printf("\n\n");

	fflush(stdout);
	return;
}
