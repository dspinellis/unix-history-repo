/*
 * Copyright (c) 1983 Regents of the University of California.
 * All rights reserved.  The Berkeley software License Agreement
 * specifies the terms and conditions for redistribution.
 */

#ifndef lint
static char sccsid[] = "@(#)broadcast.c	1.1 (Berkeley) %G%";
#endif not lint

#include "globals.h"
#include <protocols/timed.h>

extern int sock;
extern struct sockaddr_in server;

/* 
 * `broadcast' broadcasts the given message on the local network 
 * at the broadcast address set in main.c
 */

broadcast(msg) 
struct tsp *msg;
{
	int length;
	extern struct in_addr broadcastaddr;
	int bytenetorder();

	msg->tsp_vers = TSPVERSION;
	bytenetorder(msg);

	server.sin_addr = broadcastaddr;
	length = sizeof(struct sockaddr_in);
	if (sendto(sock, (char *)msg, sizeof(struct tsp), 0, 
					&server, length) < 0) {
		syslog(LOG_ERR, "timed: sendto: %m");
		exit(1);
	}
}
