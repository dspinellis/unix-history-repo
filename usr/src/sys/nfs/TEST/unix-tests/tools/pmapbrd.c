/*	@(#)pmapbrd.c	1.2 90/01/03 NFS Rev 2 Testsuite
 *	1.4 Lachman ONC Test Suite source
 *
 * Test portmap broadcast rpc facility
 */

#include <rpc/rpc.h>
#include <rpc/pmap_prot.h>
#include <rpc/pmap_clnt.h>
#include <sys/socket.h>
#ifdef SVR3
#include <sys/fs/nfs/time.h>
#else
#include <sys/time.h>
#endif
#include <stdio.h>
#include <errno.h>
#include <net/if.h>
#include <sys/ioctl.h>
#include <arpa/inet.h>

#define MAX_BROADCAST_SIZE 1400

XDR xdr_stream;
extern int errno;
static struct timeval timeout = { 3, 0 };
struct sockaddr_in baddr; /* broadcast addresses */
typedef bool_t (*resultproc_t)();
char outbuf[MAX_BROADCAST_SIZE], inbuf[MAX_BROADCAST_SIZE];


/*
 * Structures and XDR routines for parameters to and replys from
 * the pmapper remote-call-service.
 */

struct rmtcallargs {
#ifdef SVR3
	ulong prog, vers, proc, arglen;
#else
	u_long prog, vers, proc, arglen;
#endif
	caddr_t args_ptr;
	xdrproc_t xdr_args;
};
static bool_t xdr_rmtcall_args();

struct rmtcallres {
#ifdef SVR3
	ulong *port_ptr;
	ulong resultslen;
#else
	u_long *port_ptr;
	u_long resultslen;
#endif
	caddr_t results_ptr;
	xdrproc_t xdr_results;
};
static bool_t xdr_rmtcallres();

#ifdef SVR3
#define RPROG (ulong)40000010
#define RVERS (ulong)1
#define RPROC_NUM (ulong)1
#else
#define RPROG (u_long)40000010
#define RVERS (u_long)1
#define RPROC_NUM (u_long)1
#endif


int i;
main(argc, argv)
int argc;
char **argv;
{
	SVCXPRT *transp;
	struct sockaddr_in sin;
	int pktspersec, count;
	int sock, readfds;
	enum clnt_stat clnt_stat;
#ifdef SVR3
	ulong result;
#else
	u_long result;
#endif
	bool_t eachresult();
	struct timeval t;
	int a, b;

	if (argc != 3) {
		fprintf(stderr, "usage: %s pktspersec count\n", argv[0]);
		exit(1);
	}

	pktspersec = atoi(argv[1]);
	if (pktspersec < 1) {
		fprintf(stderr,
			"%s: packet rate must greater than or equal to 1\n",
			argv[0]);
		exit(1);
	}
	count = atoi(argv[2]);

	sock = socket(AF_INET,SOCK_DGRAM,0);
        sin.sin_family = AF_INET;
        sin.sin_addr.s_addr = INADDR_ANY;
        sin.sin_port = htons(3300);

        if(bind(sock, (char *)&sin, sizeof (sin)) == -1) {
                perror("brd:  bind");
                exit(1);
        }
#ifdef SO_BROADCAST
	i = 1;
	if (setsockopt(sock, SOL_SOCKET, SO_BROADCAST, &i, sizeof(i)) == -1) {
		perror("brd: setsockopt");
		exit(1);
	}
#endif
	getbroadcastnets(sock, inbuf);
	baddr.sin_family = AF_INET;
	baddr.sin_port = htons(PMAPPORT);
	printf("broadcast addr %x\n", ntohl(baddr.sin_addr.s_addr));

	if (pktspersec == 1) {
		t.tv_sec = 1;
		t.tv_usec = 0;
	} else {
		t.tv_sec = 0;
		t.tv_usec = 1000000 / pktspersec;
	}
	printf("%d/sec for %d\n", pktspersec, count);

	for (i=0; i<count; i++) {
		/*
		 * modified verison of clnt_broadcast is called
		 */
		clnt_stat =
		    clnt_broadcast(sock, RPROG, RVERS, RPROC_NUM, xdr_void, &a,
		    xdr_void, &b, eachresult, &t);
		if(clnt_stat != RPC_TIMEDOUT) {
			printf("error: clnt_stat = %d\n", clnt_stat);
			clnt_perrno(clnt_stat);
			exit(-1);
		}
	}
}

bool_t
eachresult()
{
	return(1);
}

/*
 * XDR remote call arguments
 * written for XDR_ENCODE direction only
 */
static bool_t
xdr_rmtcall_args(xdrs, cap)
	register XDR *xdrs;
	register struct rmtcallargs *cap;
{
#ifdef SVR3
	uint lenposition, argposition, position;
#else
	u_int lenposition, argposition, position;
#endif

	if (xdr_u_long(xdrs, &(cap->prog)) &&
	    xdr_u_long(xdrs, &(cap->vers)) &&
	    xdr_u_long(xdrs, &(cap->proc))) {
		lenposition = XDR_GETPOS(xdrs);
		if (! xdr_u_long(xdrs, &(cap->arglen)))
		    return (FALSE);
		argposition = XDR_GETPOS(xdrs);
		if (! (*(cap->xdr_args))(xdrs, cap->args_ptr))
		    return (FALSE);
		position = XDR_GETPOS(xdrs);
#ifdef SVR3
		cap->arglen = (ulong)position - (ulong)argposition;
#else
		cap->arglen = (u_long)position - (u_long)argposition;
#endif
		XDR_SETPOS(xdrs, lenposition);
		if (! xdr_u_long(xdrs, &(cap->arglen)))
		    return (FALSE);
		XDR_SETPOS(xdrs, position);
		return (TRUE);
	}
	return (FALSE);
}

/*
 * XDR remote call results
 * written for XDR_DECODE direction only
 */
static bool_t
xdr_rmtcallres(xdrs, crp)
	register XDR *xdrs;
	register struct rmtcallres *crp;
{

#ifdef SVR3
	if (xdr_reference(xdrs, &crp->port_ptr, sizeof (ulong), xdr_u_long) &&
#else
	if (xdr_reference(xdrs, &crp->port_ptr, sizeof (u_long), xdr_u_long) &&
#endif
		xdr_u_long(xdrs, &crp->resultslen))
		return ((*(crp->xdr_results))(xdrs, crp->results_ptr));
	return (FALSE);
}

/*
 * The following is kludged-up support for simple rpc broadcasts.
 * Someday a large, complicated system will replace these trivial 
 * routines which only support udp/ip .
 */

static int
getbroadcastnets(sock, buf)
	int sock;  /* any valid socket will do */
	char *buf;  /* why allocxate more when we can use existing... */
{
	struct ifconf ifc;
        struct ifreq ifreq, *ifr;
	struct sockaddr_in *sin;
        int n, i;

	ifc.ifc_len = MAX_BROADCAST_SIZE;
        ifc.ifc_buf = buf;
        if (ioctl(sock, SIOCGIFCONF, (char *)&ifc) < 0) {
                perror("broadcast: ioctl (get interface configuration)");
                return (0);
        }
        ifr = ifc.ifc_req;
        for (i = 0, n = ifc.ifc_len/sizeof (struct ifreq); n > 0; n--, ifr++) {
                ifreq = *ifr;
                if (ioctl(sock, SIOCGIFFLAGS, (char *)&ifreq) < 0) {
                        perror("broadcast: ioctl (get interface flags)");
                        continue;
                }
                if ((ifreq.ifr_flags & IFF_BROADCAST) &&
		    (ifreq.ifr_flags & IFF_UP) &&
		    ifr->ifr_addr.sa_family == AF_INET) {
                        sin = (struct sockaddr_in *)&ifr->ifr_addr;
#ifdef SIOCGIFBRDADDR
			if (ioctl(sock, SIOCGIFBRDADDR, (char *)&ifreq) < 0) {
				baddr.sin_addr = inet_makeaddr(inet_netof(sin->sin_addr),
							       INADDR_ANY);
			} else {
				baddr.sin_addr = ((struct sockaddr_in *)&ifreq.ifr_addr)->sin_addr;
			}
		
#else
			baddr.sin_addr = inet_makeaddr(inet_netof
			    (sin->sin_addr.s_addr), INADDR_ANY);
#endif
			break;
                }
        }
}

enum clnt_stat 
clnt_broadcast(sock, prog, vers, proc, xargs, argsp, xresults, resultsp, eachresult, t)
	int		sock;
#ifdef SVR3
	ulong		prog;		/* program number */
	ulong		vers;		/* version number */
	ulong		proc;		/* procedure number */
#else
	u_long		prog;		/* program number */
	u_long		vers;		/* version number */
	u_long		proc;		/* procedure number */
#endif
	xdrproc_t	xargs;		/* xdr routine for args */
	caddr_t		argsp;		/* pointer to args */
	xdrproc_t	xresults;	/* xdr routine for results */
	caddr_t		resultsp;	/* pointer to results */
	resultproc_t	eachresult;	/* call with each result obtained */
	struct timeval *t;
{
	XDR *xdrs = &xdr_stream;
	enum clnt_stat stat;
	AUTH *unix_auth = authunix_create_default();
	int outlen, inlen, fromlen, readfds;
	register int mask, i;
	bool_t done = FALSE;
#ifdef SVR3
	register ulong xid;
	ulong port;
#else
	register u_long xid;
	u_long port;
#endif
	struct sockaddr_in raddr; /* broadcast and response addresses */
	struct rmtcallargs a;
	struct rmtcallres r;
	struct rpc_msg msg;

	mask = (1 << sock);
	msg.rm_xid = xid;
	msg.rm_direction = CALL;
	msg.rm_call.cb_rpcvers = RPC_MSG_VERSION;
	msg.rm_call.cb_prog = PMAPPROG;
	msg.rm_call.cb_vers = PMAPVERS;
	msg.rm_call.cb_proc = PMAPPROC_CALLIT;
	msg.rm_call.cb_cred = unix_auth->ah_cred;
	msg.rm_call.cb_verf = unix_auth->ah_verf;
	a.prog = prog;
	a.vers = vers;
	a.proc = proc;
	a.xdr_args = xargs;
	a.args_ptr = argsp;
	r.port_ptr = &port;
	r.xdr_results = xresults;
	r.results_ptr = resultsp;
	xdrmem_create(xdrs, outbuf, MAX_BROADCAST_SIZE, XDR_ENCODE);
	if ((! xdr_callmsg(xdrs, &msg)) || (! xdr_rmtcall_args(xdrs, &a))) {
		stat = RPC_CANTENCODEARGS;
		goto done_broad;
	}
	outlen = (int)xdr_getpos(xdrs);
	xdr_destroy(xdrs);
	/*
	 * Basic loop: broadcast a packet and wait a while for response(s).
	 * The response timeout grows larger per iteration.
	 */

	if (sendto(sock, outbuf, outlen, 0,
	    (struct socketaddr *)&baddr,
	    sizeof (struct sockaddr)) != outlen) {
		perror("Cannot send broadcast packet");
		stat = RPC_CANTSEND;
		goto done_broad;
	}
recv_again:
	msg.acpted_rply.ar_verf = _null_auth;
	msg.acpted_rply.ar_results.where = (caddr_t)&r;
	msg.acpted_rply.ar_results.proc = xdr_rmtcallres;
	readfds = mask;
	switch (select(32, &readfds, (int *)NULL, (int *)NULL, t)) {

	case 0:  /* timed out */
		stat = RPC_TIMEDOUT;
		goto done_broad;

	case -1:  /* some kind of error */
		if (errno == EINTR)
			goto recv_again;
		perror("Broadcast select problem");
		stat = RPC_CANTRECV;
		goto done_broad;

	}  /* end of select results switch */
	if ((readfds & mask) == 0)
		goto recv_again;
try_again:
	fromlen = sizeof(struct sockaddr);
	inlen = recvfrom(sock, inbuf, MAX_BROADCAST_SIZE, 0,
		(struct sockaddr *)&raddr, &fromlen);
	if (inlen < 0) {
		if (errno == EINTR)
			goto try_again;
		perror("Cannot receive reply to broadcast");
		stat = RPC_CANTRECV;
		goto done_broad;
	}
#ifdef SVR3
	if (inlen < sizeof(ulong))
#else
	if (inlen < sizeof(u_long))
#endif
		goto recv_again;
	/*
	 * see if reply transaction id matches sent id.
	 * If so, decode the results.
	 */
	xdrmem_create(xdrs, inbuf, inlen, XDR_DECODE);
	if (xdr_replymsg(xdrs, &msg)) {
		if ((msg.rm_xid == xid) &&
			(msg.rm_reply.rp_stat == MSG_ACCEPTED) &&
			(msg.acpted_rply.ar_stat == SUCCESS)) {
#ifdef SVR3
			raddr.sin_port = htons((ushort)port);
#else
			raddr.sin_port = htons((u_short)port);
#endif
			done = (*eachresult)(resultsp, &raddr);
		}
		/* otherwise, we just ignore the errors ... */
	} else {
		/* some kind of deserialization problem ... */
		if (msg.rm_xid == xid)
			fprintf(stderr, "Broadcast deserialization problem");
		/* otherwise, just random garbage */
	}
	xdrs->x_op = XDR_FREE;
	msg.acpted_rply.ar_results.proc = xdr_void;
	(void)xdr_replymsg(xdrs, &msg);
	(void)(*xresults)(xdrs, resultsp);
	xdr_destroy(xdrs);
	if (done) {
		stat = RPC_SUCCESS;
	} else {
		goto recv_again;
	}

done_broad:
	AUTH_DESTROY(unix_auth);
	return (stat);
}
