/*
 * Copyright (c) 1983 Regents of the University of California.
 * All rights reserved.  The Berkeley software License Agreement
 * specifies the terms and conditions for redistribution.
 */

#ifndef lint
static char sccsid[] = "@(#)process.c	5.2 (Berkeley) 6/8/85";
#endif not lint

/*
 * process.c handles the requests, which can be of three types:
 *	ANNOUNCE - announce to a user that a talk is wanted
 *	LEAVE_INVITE - insert the request into the table
 *	LOOK_UP - look up to see if a request is waiting in
 *		  in the table for the local user
 *	DELETE - delete invitation
 */
#include "ctl.h"
#include <sys/stat.h>
#include <stdio.h>

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
		} else
			insert_table(request, response);
		break;

	case LOOK_UP :
		ptr = find_match(request);
		if (ptr != (CTL_MSG *) 0) {
			response->id_num = ptr->id_num;
			response->addr = ptr->addr;
			response->answer = SUCCESS;
		} else
			response->answer = NOT_HERE;
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
	if (hp == (struct hostent *)0) {
		response->answer = MACHINE_UNKNOWN;
		return;
	}
	ptr = find_request(request);
	if (ptr == (CTL_MSG *) 0) {
		insert_table(request,response);
		response->answer = announce(request, hp->h_name);
		return;
	}
	if (request->id_num > ptr->id_num) {
		/*
		 * this is an explicit re-announce, so update the id_num
		 * field to avoid duplicates and re-announce the talk 
		 */
		ptr->id_num = response->id_num = new_id();
		response->answer = announce(request, hp->h_name);
		return;
	}
	/* a duplicated request, so ignore it */
	response->id_num = ptr->id_num;
	response->answer = SUCCESS;
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
	int status;
	FILE *fd;
	struct stat statb;
	char ftty[20];

	if ((fd = fopen("/etc/utmp", "r")) == NULL) {
		perror("Can't open /etc/utmp");
		return (FAILED);
	}
#define SCMPN(a, b)	strncmp(a, b, sizeof (a))
	status = NOT_HERE;
	(void) strcpy(ftty, "/dev/");
	while (fread((char *) &ubuf, sizeof ubuf, 1, fd) == 1)
		if (SCMPN(ubuf.ut_name, name) == 0) {
			if (*tty == '\0') {
				status = PERMISSION_DENIED;
				/* no particular tty was requested */
				(void) strcpy(ftty+5, ubuf.ut_line);
				if (stat(ftty,&statb) == 0) {
					if (!(statb.st_mode & 020))
						continue;
					(void) strcpy(tty, ubuf.ut_line);
					status = SUCCESS;
					break;
				}
			}
			if (strcmp(ubuf.ut_line, tty) == 0) {
				status = SUCCESS;
				break;
			}
		}
	fclose(fd);
	return (status);
}
