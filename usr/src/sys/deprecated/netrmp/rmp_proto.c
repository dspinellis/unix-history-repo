/*
 * Copyright (c) 1988 University of Utah.
 * Copyright (c) 1990 The Regents of the University of California.
 * All rights reserved.
 *
 * This code is derived from software contributed to Berkeley by
 * the Systems Programming Group of the University of Utah Computer
 * Science Department.
 *
 * %sccs.include.redist.c%
 *
 * from: Utah $Hdr: rmp_proto.c 1.3 89/06/07$
 *
 *	@(#)rmp_proto.c	7.1 (Berkeley) %G%
 */

#include "param.h"
#include "socket.h"
#include "protosw.h"
#include "domain.h"

#include "rmp.h"

#ifdef RMP
/*
 * HP Remote Maintenance Protocol (RMP) family: BOOT
 */

extern	struct domain	rmpdomain;
extern	int		raw_usrreq(), rmp_output();

struct protosw rmpsw[] = {
  {	SOCK_RAW,	&rmpdomain,	RMPPROTO_BOOT,	PR_ATOMIC|PR_ADDR,
	0,		rmp_output,	0,		0,
	raw_usrreq,
	0,		0,		0,		0,
  },
};

struct domain rmpdomain = {
	AF_RMP, "RMP", 0, 0, 0, rmpsw, &rmpsw[sizeof(rmpsw)/sizeof(rmpsw[0])]
};

#endif
