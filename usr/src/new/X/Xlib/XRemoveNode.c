/* $Header: XRemoveNode.c,v 10.2 86/02/01 15:39:31 tony Rel $ */
/* Copyright    Massachusetts Institute of Technology    1985	*/
 
#include "XlibInternal.h"
#ifdef DNETCONN
#include <netdnet/dn.h>
#endif
 
XRemoveNode (node)
	struct dn_naddr *node;
{
#ifdef DNETCONN
	register Display *dpy;
	register XReq *req;
 
	GetReq(X_RemoveHost, 0);
	req->func = XAF_DECnet;
	bcopy (node, &(req->param.l[0]), sizeof (struct dn_naddr));
#endif
}
 
