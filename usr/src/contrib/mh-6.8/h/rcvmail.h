/* rcvmail.h - rcvmail hook definitions */
/* @(#)$Id: rcvmail.h,v 1.3 1992/12/15 00:20:22 jromine Exp $ */

#ifndef	MMDFMTS
#include <ctype.h>
#include <errno.h>
#include <setjmp.h>
#include <stdio.h>
#include <sys/types.h>
#include "../mts/sendmail/smail.h"
#else	/* MMDFMTS */
#include "../mts/mmdf/util.h"
#include "../mts/mmdf/mmdf.h"
#endif	/* MMDFMTS */


#ifndef	MMDFI
#define	RCV_MOK	0
#define	RCV_MBX	1
#else	/* MMDFI */
#define	RCV_MOK	RP_MOK
#define	RCV_MBX	RP_MECH
#endif	/* MMDFI */


#ifdef	NRTC			/* sigh */
#undef	RCV_MOK
#undef	RCV_MBX

#define	RCV_MOK	RP_MOK
#define	RCV_MBX	RP_MECH
#endif	/* NRTC */
