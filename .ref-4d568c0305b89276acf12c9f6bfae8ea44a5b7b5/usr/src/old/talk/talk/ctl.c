/*
 * Copyright (c) 1983 Regents of the University of California.
 * All rights reserved.  The Berkeley software License Agreement
 * specifies the terms and conditions for redistribution.
 */

#ifndef lint
static char sccsid[] = "@(#)ctl.c	5.1 (Berkeley) 6/6/85";
#endif not lint

/*
 * This file handles haggling with the various talk daemons to
 * get a socket to talk to. sockt is opened and connected in
 * the progress
 */

#include "talk_ctl.h"

struct	sockaddr_in daemon_addr = { AF_INET };
struct	sockaddr_in ctl_addr = { AF_INET };
struct	sockaddr_in my_addr = { AF_INET };

    /* inet addresses of the two machines */
struct	in_addr my_machine_addr;
struct	in_addr his_machine_addr;

u_short daemon_port;	/* port number of the talk daemon */

int	ctl_sockt;
int	sockt;
int	invitation_waiting = 0;

CTL_MSG msg;

open_sockt()
{
    int length;

    my_addr.sin_addr = my_machine_addr;
    my_addr.sin_port = 0;
    sockt = socket(AF_INET, SOCK_STREAM, 0);
    if (sockt <= 0)
	p_error("Bad socket");
    if (bind(sockt, (struct sockaddr *)&my_addr, sizeof(my_addr)) != 0)
	p_error("Binding local socket");
    length = sizeof(my_addr);
    if (getsockname(sockt, (struct sockaddr *)&my_addr, &length) == -1)
	p_error("Bad address for socket");
}

/* open the ctl socket */
open_ctl() 
{
    int length;

    ctl_addr.sin_port = 0;
    ctl_addr.sin_addr = my_machine_addr;
    ctl_sockt = socket(AF_INET, SOCK_DGRAM, 0);
    if (ctl_sockt <= 0)
	p_error("Bad socket");
    if (bind(ctl_sockt, (struct sockaddr *)&ctl_addr, sizeof(ctl_addr)) != 0)
	p_error("Couldn't bind to control socket");
    length = sizeof(ctl_addr);
    if (getsockname(ctl_sockt, (struct sockaddr *)&ctl_addr, &length) == -1)
	p_error("Bad address for ctl socket");
}

/* print_addr is a debug print routine */
print_addr(addr)
struct sockaddr_in addr;
{
    int i;

    printf("addr = %x, port = %o, family = %o zero = ",
	    addr.sin_addr, addr.sin_port, addr.sin_family);
    for (i = 0; i<8;i++)
	printf("%o ", (int)addr.sin_zero[i]);
    putchar('\n');
}
