#ifndef lint
static char sccsid[] = "@(#)look_up.c	1.2 (Berkeley) %G%";
#endif

#include "talk_ctl.h"

/*
 * See if the local daemon has a invitation for us
 */
check_local()
{
	CTL_RESPONSE response;

	/* the rest of msg was set up in get_names */
	msg.ctl_addr = ctl_addr;
	if (!look_for_invite(&response))	/* must be initiating a talk */
		return (0);
	/*
	 * There was an invitation waiting for us, 
	 * so connect with the other (hopefully waiting) party 
	 */
	current_state = "Waiting to connect with caller";
again:
	if (connect(sockt, &response.addr, sizeof(response.addr)) != -1)
		return (1);
	if (errno == EINTR)
		goto again;
	if (errno == ECONNREFUSED) {
		/*
		 * The caller gave up, but his invitation somehow
		 * was not cleared. Clear it and initiate an 
		 * invitation. (We know there are no newer invitations,
		 * the talkd works LIFO.)
		 */
		ctl_transact(his_machine_addr, msg, DELETE, &response);
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
look_for_invite(response)
	CTL_RESPONSE *response;
{
	struct in_addr machine_addr;

	current_state = "Checking for invitation on caller's machine";
	ctl_transact(his_machine_addr, msg, LOOK_UP, response);
	/* the switch is for later options, such as multiple invitations */
	switch (response->answer) {

	case SUCCESS:
		msg.id_num = response->id_num;
		return (1);

	default :
		/* there wasn't an invitation waiting for us */
		return (0);
	}
}
