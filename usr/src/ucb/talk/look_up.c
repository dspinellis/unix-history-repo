/* $Header: look_up.c 1.2 83/03/28 00:34:22 moore Exp $ */

#include "talk_ctl.h"

    /* see if the local daemon has a invitation for us */

check_local()
{
    CTL_RESPONSE response;

	/* the rest of msg was set up in get_names */

    msg.ctl_addr = ctl_addr;

    if (!look_for_invite(&response)) {

	    /* we must be initiating a talk */

	return(0);
    }

        /*
	 * there was an invitation waiting for us, 
	 * so connect with the other (hopefully waiting) party 
	 */

    current_state = "Waiting to connect with caller";

    while (connect(sockt, &response.addr, sizeof(response.addr)) != 0) {
	if (errno == ECONNREFUSED) {

		/* the caller gave up, but his invitation somehow
		 * was not cleared. Clear it and initiate an 
		 * invitation. (We know there are no newer invitations,
		 * the talkd works LIFO.)
		 */

	    ctl_transact(his_machine_addr, msg, DELETE, &response);
	    close(sockt);
	    open_sockt();
	    return(0);
	} else if (errno == EINTR) {
		/* we have returned from an interupt handler */
	    continue;
	} else {
	    p_error("Unable to connect with initiator");
	}
    }

    return(1);
}

    /* look for an invitation on 'machine' */

look_for_invite(response)
CTL_RESPONSE *response;
{
    struct in_addr machine_addr;

    current_state = "Checking for invitation on caller's machine";

    ctl_transact(his_machine_addr, msg, LOOK_UP, response);

	/* the switch is for later options, such as multiple 
	   invitations */

    switch (response->answer) {

	case SUCCESS:

	    msg.id_num = response->id_num;
	    return(1);

	default :
		/* there wasn't an invitation waiting for us */
	    return(0);
    }
}
