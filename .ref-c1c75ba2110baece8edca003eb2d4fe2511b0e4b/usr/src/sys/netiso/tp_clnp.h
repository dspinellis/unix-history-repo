/***********************************************************
		Copyright IBM Corporation 1987

                      All Rights Reserved

Permission to use, copy, modify, and distribute this software and its 
documentation for any purpose and without fee is hereby granted, 
provided that the above copyright notice appear in all copies and that
both that copyright notice and this permission notice appear in 
supporting documentation, and that the name of IBM not be
used in advertising or publicity pertaining to distribution of the
software without specific, written prior permission.  

IBM DISCLAIMS ALL WARRANTIES WITH REGARD TO THIS SOFTWARE, INCLUDING
ALL IMPLIED WARRANTIES OF MERCHANTABILITY AND FITNESS, IN NO EVENT SHALL
IBM BE LIABLE FOR ANY SPECIAL, INDIRECT OR CONSEQUENTIAL DAMAGES OR
ANY DAMAGES WHATSOEVER RESULTING FROM LOSS OF USE, DATA OR PROFITS,
WHETHER IN AN ACTION OF CONTRACT, NEGLIGENCE OR OTHER TORTIOUS ACTION,
ARISING OUT OF OR IN CONNECTION WITH THE USE OR PERFORMANCE OF THIS
SOFTWARE.

******************************************************************/

/*
 * ARGO Project, Computer Sciences Dept., University of Wisconsin - Madison
 */
/* 
 * ARGO TP
 *
 * $Header: tp_clnp.h,v 5.1 88/10/12 12:16:36 root Exp $
 * $Source: /usr/argo/sys/netiso/RCS/tp_clnp.h,v $
 *
 * AF_ISO net-dependent structures and include files
 *
 */


#ifndef __TP_CLNP__
#define __TP_CLNP__

#ifndef SOCK_STREAM
#include "socket.h"
#endif SOCK_STREAM

#ifndef RTFREE
#include "../net/route.h"
#endif
#include "../netiso/iso.h"
#include "../netiso/clnp.h"
#include "../netiso/iso_pcb.h"
#ifndef IF_DEQUEUE
#include "../net/if.h"
#endif
#include "../netiso/iso_var.h"

struct isopcb tp_isopcb;	
	/* queue of active inpcbs for tp ; for tp with dod ip */

#endif __TP_CLNP__
