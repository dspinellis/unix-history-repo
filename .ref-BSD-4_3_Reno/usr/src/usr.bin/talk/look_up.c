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
static char sccsid[] = "@(#)look_up.c	5.6 (Berkeley) 6/1/90";
#endif /* not lint */

#include "talk_ctl.h"

/*
 * See if the local daemon has an invitation for us.
 */
check_local()
{
	CTL_RESPONSE response;
	register CTL_RESPONSE *rp = &response;

	/* the rest of msg was set up in get_names */
#ifdef MSG_EOR
	/* copy new style sockaddr to old, swap family (short in old) */
	msg.ctl_addr = *(struct osockaddr *)&ctl_addr;
	msg.ctl_addr.sa_family = htons(ctl_addr.sin_family);
#else
	msg.ctl_addr = *(struct sockaddr *)&ctl_addr;
#endif
	/* must be initiating a talk */
	if (!look_for_invite(rp))
		return (0);
	/*
	 * There was an invitation waiting for us, 
	 * so connect with the other (hopefully waiting) party 
	 */
	current_state = "Waiting to connect with caller";
	do {
		if (rp->addr.sa_family != AF_INET)
			p_error("Response uses invalid network address");
		errno = 0;
		if (connect(sockt, &rp->addr, sizeof (rp->addr)) != -1)
			return (1);
	} while (errno == EINTR);
	if (errno == ECONNREFUSED) {
		/*
		 * The caller gave up, but his invitation somehow
		 * was not cleared. Clear it and initiate an 
		 * invitation. (We know there are no newer invitations,
		 * the talkd works LIFO.)
		 */
		ctl_transact(his_machine_addr, msg, DELETE, rp);
		close(sockt);
		open_sockt();
		return (0);
	}
	p_error("Unable to connect with initiator");
	/*NOTREACHED*/
}

/*
 * Look for an invitation on 'machine'
 */
look_for_invite(rp)
	CTL_RESPONSE *rp;
{
	struct in_addr machine_addr;

	current_state = "Checking for invitation on caller's machine";
	ctl_transact(his_machine_addr, msg, LOOK_UP, rp);
	/* the switch is for later options, such as multiple invitations */
	switch (rp->answer) {

	case SUCCESS:
		msg.id_num = htonl(rp->id_num);
		return (1);

	default:
		/* there wasn't an invitation waiting for us */
		return (0);
	}
}
