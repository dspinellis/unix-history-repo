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
 * $Header: cons.h,v 4.4 88/09/09 19:01:28 nhall Exp $
 * $Source: /usr/argo/sys/netiso/RCS/cons.h,v $
 *
 * interface between TP and CONS
 */

#define	CONSOPT_X25CRUD	0x01		/* set x.25 call request user data */

struct dte_addr {
	u_char 	dtea_addr[7];
	u_char	dtea_niblen;
};

#ifdef	KERNEL

#define CONN_OPEN		0x33
#define CONN_CONFIRM	0x30
#define CONN_REFUSE		0x31
#define CONN_CLOSE		0x32

#define	CONS_IS_DGM		0x1
#define	CONS_NOT_DGM	0x0

#ifndef	PRC_NCMDS
#include "protosw.h"
#endif	PRC_NCMDS

#define PRC_CONS_SEND_DONE 2 /* something unused in protosw.h */

#endif	KERNEL
