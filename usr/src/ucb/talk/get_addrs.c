/*
 * Copyright (c) 1983 Regents of the University of California.
 * All rights reserved.  The Berkeley software License Agreement
 * specifies the terms and conditions for redistribution.
 */

#ifndef lint
static char sccsid[] = "@(#)get_addrs.c	5.2 (Berkeley) 3/13/86";
#endif not lint

#include "talk_ctl.h"
#include <netdb.h>

get_addrs(my_machine_name, his_machine_name)
	char *my_machine_name, *his_machine_name;
{
	struct hostent *hp;
	struct servent *sp;

	msg.pid = htonl(getpid());
	/* look up the address of the local host */
	hp = gethostbyname(my_machine_name);
	if (hp == (struct hostent *) 0) {
		fprintf(stderr,
		    "talk: %s: Can't figure out network address.\n",
		    my_machine_name);
		exit(-1);
	}
	bcopy(hp->h_addr, (char *)&my_machine_addr, hp->h_length);
	/*
	 * If the callee is on-machine, just copy the
	 * network address, otherwise do a lookup...
	 */
	if (strcmp(his_machine_name, my_machine_name)) {
		hp = gethostbyname(his_machine_name);
		if (hp == (struct hostent *) 0 ) {
			fprintf(stderr,
			    "talk: %s: Can't figure out network address.\n",
			    his_machine_name);
			exit(-1);
		}
		bcopy(hp->h_addr, (char *) &his_machine_addr, hp->h_length);
	} else
		his_machine_addr = my_machine_addr;
	/* find the server's port */
	sp = getservbyname("ntalk", "udp");
	if (sp == 0) {
		fprintf(stderr, "talk: %s/%s: service is not registered.\n",
		     "ntalk", "udp");
		exit(-1);
	}
	daemon_port = sp->s_port;
}
