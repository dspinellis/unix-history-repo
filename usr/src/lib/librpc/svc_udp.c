/*
 * Sun RPC is a product of Sun Microsystems, Inc. and is provided for
 * unrestricted use provided that this legend is included on all tape
 * media and as a part of the software program in whole or part.  Users
 * may copy or modify Sun RPC without charge, but are not authorized
 * to license or distribute it to anyone else except as part of a product or
 * program developed by the user.
 * 
 * SUN RPC IS PROVIDED AS IS WITH NO WARRANTIES OF ANY KIND INCLUDING THE
 * WARRANTIES OF DESIGN, MERCHANTIBILITY AND FITNESS FOR A PARTICULAR
 * PURPOSE, OR ARISING FROM A COURSE OF DEALING, USAGE OR TRADE PRACTICE.
 * 
 * Sun RPC is provided with no support and without any obligation on the
 * part of Sun Microsystems, Inc. to assist in its use, correction,
 * modification or enhancement.
 * 
 * SUN MICROSYSTEMS, INC. SHALL HAVE NO LIABILITY WITH RESPECT TO THE
 * INFRINGEMENT OF COPYRIGHTS, TRADE SECRETS OR ANY PATENTS BY SUN RPC
 * OR ANY PART THEREOF.
 * 
 * In no event will Sun Microsystems, Inc. be liable for any lost revenue
 * or profits or other special, indirect and consequential damages, even if
 * Sun has been advised of the possibility of such damages.
 * 
 * Sun Microsystems, Inc.
 * 2550 Garcia Avenue
 * Mountain View, California  94043
 */
#ifndef lint
static char sccsid[] = "@(#)svc_udp.c 1.4 85/03/14 Copyr 1984 Sun Micro";
#endif

/*
 * svc_udp.c,
 * Server side for UDP/IP based RPC.  (Does some caching in the hopes of
 * achieving execute-at-most-once semantics.)
 *
 * Copyright (C) 1984, Sun Microsystems, Inc.
 */

#include <stdio.h>
#include "types.h"
#include <netinet/in.h>
#include <sys/socket.h>
#include <errno.h>
#include "xdr.h"
#include "auth.h"
#include "clnt.h"
#include "rpc_msg.h"
#include "svc.h"

char *mem_alloc();

#define rpc_buffer(xprt) ((xprt)->xp_p1)

static bool_t		svcudp_recv();
static bool_t		svcudp_reply();
static enum xprt_stat	svcudp_stat();
static bool_t		svcudp_getargs();
static bool_t		svcudp_freeargs();
static void		svcudp_destroy();

static struct xp_ops svcudp_op = {
	svcudp_recv,
	svcudp_stat,
	svcudp_getargs,
	svcudp_reply,
	svcudp_freeargs,
	svcudp_destroy
};

extern int errno;

/*
 * kept in xprt->xp_p2
 */
struct svcudp_data {
	u_long	su_xid;		/* transaction id */
	XDR	su_xdrs;	/* XDR handle */
	char	su_verfbody[MAX_AUTH_BYTES];	/* verifier body */
};
#define	su_data(xprt)	((struct svcudp_data *)(xprt->xp_p2))

/*
 * Usage:
 *	xprt = svcudp_create(sock);
 *
 * If sock<0 then a socket is created, else sock is used.
 * If the socket, sock is not bound to a port then svcudp_create
 * binds it to an arbitrary port.  In any (successful) case,
 * xprt->xp_sock is the registered socket number and xprt->xp_port is the
 * associated port number.
 * Once *xprt is initialized, it is registered as a transporter;
 * see (svc.h, xprt_register).
 * The routines returns NULL if a problem occurred.
 */
SVCXPRT *
svcudp_create(sock)
	register int sock;
{
	bool_t madesock = FALSE;
	register SVCXPRT *xprt;
	register struct svcudp_data *su;
	struct sockaddr_in addr;
	int len = sizeof(struct sockaddr_in);

	if (sock == RPC_ANYSOCK) {
		if ((sock = socket(AF_INET, SOCK_DGRAM, IPPROTO_UDP)) < 0) {
			perror("svcudp_create: socket creation problem");
			return ((SVCXPRT *)NULL);
		}
		madesock = TRUE;
	}
	addr.sin_addr.s_addr = 0;
	addr.sin_family = AF_INET;
	addr.sin_port = 0;
	(void)bind(sock, (struct sockaddr *)&addr, len);
	if (getsockname(sock, (caddr_t)&addr, &len) != 0) {
		perror("svcudp_create - cannot getsockname");
		if (madesock)
			(void)close(sock);
		return ((SVCXPRT *)NULL);
	}
	xprt = (SVCXPRT *)mem_alloc(sizeof(SVCXPRT));
	if (xprt == NULL) {
		fprintf(stderr, "svcudp_create: out of memory\n");
		return (NULL);
	}
	if ((rpc_buffer(xprt) = mem_alloc(UDPMSGSIZE)) == NULL) {
		fprintf(stderr, "svcudp_create: out of memory\n");
		return (NULL);
	}
	su = (struct svcudp_data *)mem_alloc(sizeof(*su));
	if (su == NULL) {
		fprintf(stderr, "svcudp_create: out of memory\n");
		return (NULL);
	}
	xdrmem_create(
	    &(su->su_xdrs), rpc_buffer(xprt), UDPMSGSIZE, XDR_DECODE);
	xprt->xp_p2 = (caddr_t)su;
	xprt->xp_verf.oa_base = su->su_verfbody;
	xprt->xp_ops = &svcudp_op;
	xprt->xp_port = ntohs(addr.sin_port);
	xprt->xp_sock = sock;
	xprt_register(xprt);
	return (xprt);
}
 
static enum xprt_stat
svcudp_stat(xprt)
	SVCXPRT *xprt;
{

	return (XPRT_IDLE); 
}

static bool_t
svcudp_recv(xprt, msg)
	register SVCXPRT *xprt;
	struct rpc_msg *msg;
{
	register struct svcudp_data *su = su_data(xprt);
	register XDR *xdrs = &(su->su_xdrs);
	register int rlen;

    again:
	xprt->xp_addrlen = sizeof(struct sockaddr_in);
	rlen = recvfrom(xprt->xp_sock, rpc_buffer(xprt), UDPMSGSIZE,
	    0, (struct sockaddr *)&(xprt->xp_raddr), &(xprt->xp_addrlen));
	if (rlen == -1 && errno == EINTR)
		goto again;
	if (rlen < 4*sizeof(u_long))
		return (FALSE);
	xdrs->x_op = XDR_DECODE;
	XDR_SETPOS(xdrs, 0);
	if (! xdr_callmsg(xdrs, msg))
		return (FALSE);
	su->su_xid = msg->rm_xid;
	return (TRUE);
}

static bool_t
svcudp_reply(xprt, msg)
	register SVCXPRT *xprt; 
	struct rpc_msg *msg; 
{
	register struct svcudp_data *su = su_data(xprt);
	register XDR *xdrs = &(su->su_xdrs);
	register int slen;
	register bool_t stat = FALSE;

	xdrs->x_op = XDR_ENCODE;
	XDR_SETPOS(xdrs, 0);
	msg->rm_xid = su->su_xid;
	if (xdr_replymsg(xdrs, msg)) {
		slen = (int)XDR_GETPOS(xdrs);
		if (sendto(xprt->xp_sock, rpc_buffer(xprt), slen, 0,
		    (struct sockaddr *)&(xprt->xp_raddr), xprt->xp_addrlen)
		    == slen)
			stat = TRUE;
	}
	return (stat);
}

static bool_t
svcudp_getargs(xprt, xdr_args, args_ptr)
	SVCXPRT *xprt;
	xdrproc_t xdr_args;
	caddr_t args_ptr;
{

	return ((*xdr_args)(&(su_data(xprt)->su_xdrs), args_ptr));
}

static bool_t
svcudp_freeargs(xprt, xdr_args, args_ptr)
	SVCXPRT *xprt;
	xdrproc_t xdr_args;
	caddr_t args_ptr;
{
	register XDR *xdrs = &(su_data(xprt)->su_xdrs);

	xdrs->x_op = XDR_FREE;
	return ((*xdr_args)(xdrs, args_ptr));
}

static void
svcudp_destroy(xprt)
	register SVCXPRT *xprt;
{
	register struct svcudp_data *su = su_data(xprt);

	xprt_unregister(xprt);
	(void)close(xprt->xp_sock);
	XDR_DESTROY(&(su->su_xdrs));
	mem_free((caddr_t)su, sizeof(struct svcudp_data));
	mem_free(rpc_buffer(xprt), UDPMSGSIZE);
	mem_free((caddr_t)xprt, sizeof(SVCXPRT));
}
