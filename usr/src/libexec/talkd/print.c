/*
 * Copyright (c) 1983 Regents of the University of California.
 * All rights reserved.
 *
 * Redistribution and use in source and binary forms are permitted
 * provided that: (1) source distributions retain this entire copyright
 * notice and comment, and (2) distributions including binaries display
 * the following acknowledgement:  ``This product includes software
 * developed by the University of California, Berkeley and its contributors''
 * in the documentation or other materials provided with the distribution
 * and in all advertising materials mentioning features or use of this
 * software. Neither the name of the University nor the names of its
 * contributors may be used to endorse or promote products derived
 * from this software without specific prior written permission.
 * THIS SOFTWARE IS PROVIDED ``AS IS'' AND WITHOUT ANY EXPRESS OR
 * IMPLIED WARRANTIES, INCLUDING, WITHOUT LIMITATION, THE IMPLIED
 * WARRANTIES OF MERCHANTABILITY AND FITNESS FOR A PARTICULAR PURPOSE.
 */

#ifndef lint
static char sccsid[] = "@(#)print.c	5.7 (Berkeley) 6/1/90";
#endif /* not lint */

/* debug print routines */

#include <stdio.h>
#include <syslog.h>
#include <sys/param.h>

#include <protocols/talkd.h>

static	char *types[] =
    { "leave_invite", "look_up", "delete", "announce" };
#define	NTYPES	(sizeof (types) / sizeof (types[0]))
static	char *answers[] = 
    { "success", "not_here", "failed", "machine_unknown", "permission_denied",
      "unknown_request", "badversion", "badaddr", "badctladdr" };
#define	NANSWERS	(sizeof (answers) / sizeof (answers[0]))

print_request(cp, mp)
	char *cp;
	register CTL_MSG *mp;
{
	char tbuf[80], *tp;
	
	if (mp->type > NTYPES) {
		(void)sprintf(tbuf, "type %d", mp->type);
		tp = tbuf;
	} else
		tp = types[mp->type];
	syslog(LOG_DEBUG, "%s: %s: id %d, l_user %s, r_user %s, r_tty %s",
	    cp, tp, mp->id_num, mp->l_name, mp->r_name, mp->r_tty);
}

print_response(cp, rp)
	char *cp;
	register CTL_RESPONSE *rp;
{
	char tbuf[80], *tp, abuf[80], *ap;
	
	if (rp->type > NTYPES) {
		(void)sprintf(tbuf, "type %d", rp->type);
		tp = tbuf;
	} else
		tp = types[rp->type];
	if (rp->answer > NANSWERS) {
		(void)sprintf(abuf, "answer %d", rp->answer);
		ap = abuf;
	} else
		ap = answers[rp->answer];
	syslog(LOG_DEBUG, "%s: %s: %s, id %d", cp, tp, ap, ntohl(rp->id_num));
}
