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
static char sccsid[] = "@(#)svc_raw.c 1.3 85/03/14 Copyr 1984 Sun Micro";
#endif

/*
 * svc_raw.c,   This a toy for simple testing and timing.
 * Interface to create an rpc client and server in the same UNIX process.
 * This lets us similate rpc and get rpc (round trip) overhead, without
 * any interference from the kernal.
 *
 * Copyright (C) 1984, Sun Microsystems, Inc.
 */

#include "types.h"
#include <netinet/in.h>
#include "xdr.h"
#include "auth.h"
#include "clnt.h"
#include "rpc_msg.h"
#include "svc.h"

#define NULL ((caddr_t)0)

/*
 * This is the "network" that we will be moving data over
 */
extern char _raw_buf[UDPMSGSIZE];

static bool_t		svcraw_recv();
static enum xprt_stat 	svcraw_stat();
static bool_t		svcraw_getargs();
static bool_t		svcraw_reply();
static bool_t		svcraw_freeargs();
static void		svcraw_destroy();

static struct xp_ops server_ops = {
	svcraw_recv,
	svcraw_stat,
	svcraw_getargs,
	svcraw_reply,
	svcraw_freeargs,
	svcraw_destroy
};

static SVCXPRT server;
static XDR xdr_stream;
static char verf_body[MAX_AUTH_BYTES];

SVCXPRT *
svcraw_create()
{

	server.xp_sock = 0;
	server.xp_port = 0;
	server.xp_ops = &server_ops;
	server.xp_verf.oa_base = verf_body;
	xdrmem_create(&xdr_stream, _raw_buf, UDPMSGSIZE, XDR_FREE);
	return (&server);
}

static enum xprt_stat
svcraw_stat()
{

	return (XPRT_IDLE);
}

static bool_t
svcraw_recv(xprt, msg)
	SVCXPRT *xprt;
	struct rpc_msg *msg;
{
	register XDR *xdrs = &xdr_stream;

	xdrs->x_op = XDR_DECODE;
	XDR_SETPOS(xdrs, 0);
	if (! xdr_callmsg(xdrs, msg))
	       return (FALSE);
	return (TRUE);
}

static bool_t
svcraw_reply(xprt, msg)
	SVCXPRT *xprt;
	struct rpc_msg *msg;
{
	register XDR *xdrs = &xdr_stream;

	xdrs->x_op = XDR_ENCODE;
	XDR_SETPOS(xdrs, 0);
	if (! xdr_replymsg(xdrs, msg))
	       return (FALSE);
	(void)XDR_GETPOS(xdrs);  /* called just for overhead */
	return (TRUE);
}

static bool_t
svcraw_getargs(xprt, xdr_args, args_ptr)
	SVCXPRT *xprt;
	xdrproc_t xdr_args;
	caddr_t args_ptr;
{

	return ((*xdr_args)(&xdr_stream, args_ptr));
}

static bool_t
svcraw_freeargs(xprt, xdr_args, args_ptr)
	SVCXPRT *xprt;
	xdrproc_t xdr_args;
	caddr_t args_ptr;
{ 
	register XDR *xdrs = &xdr_stream;

	xdrs->x_op = XDR_FREE;
	return ((*xdr_args)(xdrs, args_ptr));
} 

static void
svcraw_destroy()
{
}
