/*
 * Copyright (c) 1983 Regents of the University of California.
 * All rights reserved.  The Berkeley software License Agreement
 * specifies the terms and conditions for redistribution.
 */

#ifndef lint
static char sccsid[] = "@(#)get_addrs.c	5.1 (Berkeley) %G%";
#endif not lint

#include "talk_ctl.h"

struct	hostent *gethostbyname();
struct	servent *getservbyname();

get_addrs(my_machine_name, his_machine_name)
	char *my_machine_name;
	char *his_machine_name;
{
	struct hostent *hp;
	struct servent *sp;

	msg.pid = getpid();
	/* look up the address of the local host */
	hp = gethostbyname(my_machine_name);
	if (hp == (struct hostent *) 0) {
		printf("This machine doesn't exist. Boy, am I confused!\n");
		exit(-1);
	}
	bcopy(hp->h_addr, (char *)&my_machine_addr, hp->h_length);
	/* if he is on the same machine, then simply copy */
	if (bcmp((char *)&his_machine_name, (char *)&my_machine_name,
	    sizeof(his_machine_name)) == 0)
		bcopy((char *)&my_machine_addr, (char *)&his_machine_addr,
		    sizeof(his_machine_name));
	else {
		/* look up the address of the recipient's machine */
		hp = gethostbyname(his_machine_name);
		if (hp == (struct hostent *) 0 ) {
			printf("%s is an unknown host\n", his_machine_name);
			exit(-1);
		}
		bcopy(hp->h_addr, (char *) &his_machine_addr, hp->h_length);
	}
	/* find the daemon portal */
	sp = getservbyname("talk", "udp");
	if (sp == 0) {
		p_error("This machine doesn't support talk");
		exit(-1);
	}
	daemon_port = sp->s_port;
}
