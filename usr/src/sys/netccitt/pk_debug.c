/*
 * Copyright (c) University of British Columbia, 1984
 * Copyright (c) 1990 The Regents of the University of California.
 * All rights reserved.
 *
 * This code is derived from software contributed to Berkeley by
 * the Laboratory for Computation Vision and the Computer Science Department
 * of the University of British Columbia.
 *
 * %sccs.include.redist.c%
 *
 *	@(#)pk_debug.c	7.6 (Berkeley) %G%
 */

#include "param.h"
#include "systm.h"
#include "mbuf.h"
#include "socket.h"
#include "protosw.h"
#include "socketvar.h"
#include "errno.h"

#include "../net/if.h"

#include "x25.h"
#include "pk.h"
#include "pk_var.h"

char	*pk_state[] = {
	"Listen",	"Ready",	"Received-Call",
	"Sent-Call",	"Data-Transfer","Received-Clear",
	"Sent-Clear",
};

char   *pk_name[] = {
	"Call",		"Call-Conf",	"Clear",
	"Clear-Conf",	"Data",		"Intr",		"Intr-Conf",
	"Rr",		"Rnr",		"Reset",	"Reset-Conf",
	"Restart",	"Restart-Conf",	"Reject",	"Diagnostic",
	"Invalid"
};

pk_trace (xcp, m, dir)
struct x25config *xcp;
register struct mbuf *m;
char *dir;
{
	register char *s;
	struct x25_packet *xp = mtod(m, struct x25_packet *);
	register int i, len = 0, cnt = 0;

	if (xcp -> xc_ptrace == 0)
		return;

	i = pk_decode (xp) / MAXSTATES;
	for (; m; m = m -> m_next) {
		len = len + m -> m_len;
		++cnt;
	}
	printf ("LCN=%d %s:	%s	#=%d, len=%d ",
		LCN(xp), dir, pk_name[i], cnt, len);
	for (s = (char *) xp, i = 0; i < 5; ++i, ++s)
		printf ("%x ", (int) * s & 0xff);
	printf ("\n");
}
