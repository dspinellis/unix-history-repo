/* $Header: process.c 1.5 83/03/28 20:16:07 moore Exp $ */

    /* process.c handles the requests, which can be of three types:

		ANNOUNCE - announce to a user that a talk is wanted

		LEAVE_INVITE - insert the request into the table
		
		LOOK_UP - look up to see if a request is waiting in
			  in the table for the local user

		DELETE - delete invitation

     */

#include "ctl.h"
char *strcpy();
CTL_MSG *find_request();
CTL_MSG *find_match();

process_request(request, response)
CTL_MSG *request;
CTL_RESPONSE *response;
{
    CTL_MSG *ptr;

    response->type = request->type;
    response->id_num = 0;

    switch (request->type) {

	case ANNOUNCE :

	    do_announce(request, response);
	    break;

	case LEAVE_INVITE :

	    ptr = find_request(request);
	    if (ptr != (CTL_MSG *) 0) {
		response->id_num = ptr->id_num;
		response->answer = SUCCESS;
	    } else {
		insert_table(request, response);
	    }
	    break;

	case LOOK_UP :

	    ptr = find_match(request);
	    if (ptr != (CTL_MSG *) 0) {
		response->id_num = ptr->id_num;
		response->addr = ptr->addr;
		response->answer = SUCCESS;
	    } else {
		response->answer = NOT_HERE;
	    }
	    break;

	case DELETE :

	    response->answer = delete_invite(request->id_num);
	    break;

	default :

	    response->answer = UNKNOWN_REQUEST;
	    break;

    }
}

struct hostent *gethostbyaddr();

do_announce(request, response)
CTL_MSG *request;
CTL_RESPONSE *response;
{
    struct hostent *hp;
    CTL_MSG *ptr;
    int result;

	/* see if the user is logged */

    result = find_user(request->r_name, request->r_tty);

    if (result != SUCCESS) {
	response->answer = result;
	return;
    }

    hp = gethostbyaddr(&request->ctl_addr.sin_addr,
			  sizeof(struct in_addr), AF_INET);

    if ( hp == (struct hostent *) 0 ) {
	response->answer = MACHINE_UNKNOWN;
	return;
    }

    ptr = find_request(request);
    if (ptr == (CTL_MSG *) 0) {
	insert_table(request,response);
	response->answer = announce(request, hp->h_name);
    } else if (request->id_num > ptr->id_num) {
	    /*
	     * this is an explicit re-announce, so update the id_num
	     * field to avoid duplicates and re-announce the talk 
	     */
	ptr->id_num = response->id_num = new_id();
	response->answer = announce(request, hp->h_name);
    } else {
	    /* a duplicated request, so ignore it */
	response->id_num = ptr->id_num;
	response->answer = SUCCESS;
    }

    return;
}

#include <utmp.h>

/*
 * Search utmp for the local user
 */

find_user(name, tty)
char *name;
char *tty;
{
    struct utmp ubuf;

    int fd;

    if ((fd = open("/etc/utmp", 0)) == -1) {
	print_error("Can't open /etc/utmp");
	return(FAILED);
    }

    while (read(fd, (char *) &ubuf, sizeof ubuf) == sizeof(ubuf)) {
	if (strncmp(ubuf.ut_name, name, sizeof ubuf.ut_name) == 0) {
	    if (*tty == '\0') {
		    /* no particular tty was requested */
		(void) strcpy(tty, ubuf.ut_line);
		close(fd);
		return(SUCCESS);
	    } else if (strcmp(ubuf.ut_line, tty) == 0) {
		close(fd);
		return(SUCCESS);
	    }
	}
    }

    close(fd);
    return(NOT_HERE);
}
