/*
 * Copyright (c) 1983 Regents of the University of California.
 * All rights reserved.  The Berkeley software License Agreement
 * specifies the terms and conditions for redistribution.
 */

#ifndef lint
static char sccsid[] = "@(#)byteorder.c	1.1 (Berkeley) %G%";
#endif not lint

#include "globals.h"
#include <protocols/timed.h>

/*
 * Two routines to do the necessary byte swapping for timed protocol
 * messages. Protocol is defined in /usr/include/protocols/timed.h
 */

bytenetorder(ptr)
struct tsp *ptr;
{
	ptr->tsp_seq = htons((u_short)ptr->tsp_seq);
	ptr->tsp_time.tv_sec = htonl((u_long)ptr->tsp_time.tv_sec);
	ptr->tsp_time.tv_usec = htonl((u_long)ptr->tsp_time.tv_usec);
}

bytehostorder(ptr)
struct tsp *ptr;
{
	ptr->tsp_seq = ntohs((u_short)ptr->tsp_seq);
	ptr->tsp_time.tv_sec = ntohl((u_long)ptr->tsp_time.tv_sec);
	ptr->tsp_time.tv_usec = ntohl((u_long)ptr->tsp_time.tv_usec);
}
