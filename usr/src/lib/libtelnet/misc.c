/*-
 * Copyright (c) 1991 The Regents of the University of California.
 * All rights reserved.
 *
 * %sccs.include.redist.c%
 */

#ifndef lint
static char sccsid[] = "@(#)misc.c	5.2 (Berkeley) %G%";
#endif /* not lint */

/*
 * Copyright (c) 1988, 1990 Regents of the University of California.
 * All rights reserved.
 *
 * Redistribution and use in source and binary forms are permitted provided
 * that: (1) source distributions retain this entire copyright notice and
 * comment, and (2) distributions including binaries display the following
 * acknowledgement:  ``This product includes software developed by the
 * University of California, Berkeley and its contributors'' in the
 * documentation or other materials provided with the distribution and in
 * all advertising materials mentioning features or use of this software.
 * Neither the name of the University nor the names of its contributors may
 * be used to endorse or promote products derived from this software without
 * specific prior written permission.
 * THIS SOFTWARE IS PROVIDED ``AS IS'' AND WITHOUT ANY EXPRESS OR IMPLIED
 * WARRANTIES, INCLUDING, WITHOUT LIMITATION, THE IMPLIED WARRANTIES OF
 * MERCHANTABILITY AND FITNESS FOR A PARTICULAR PURPOSE.
 */

#include "misc.h"

char *RemoteHostName;
char *LocalHostName;
char *UserNameRequested = 0;
int ConnectedCount = 0;

	void
auth_encrypt_init(local, remote, name, server)
	char *local;
	char *remote;
	char *name;
	int server;
{
	RemoteHostName = remote;
	LocalHostName = local;
#if	defined(AUTHENTICATION)
	auth_init(name, server);
#endif
#if	defined(ENCRYPTION)
	encrypt_init(name, server);
#endif
	if (UserNameRequested) {
		free(UserNameRequested);
		UserNameRequested = 0;
	}
}

	void
auth_encrypt_user(name)
	char *name;
{
	extern char *strdup();

	if (UserNameRequested)
		free(UserNameRequested);
	UserNameRequested = name ? strdup(name) : 0;
}

	void
auth_encrypt_connect(cnt)
	int cnt;
{
}

	void
printd(data, cnt)
	unsigned char *data;
	int cnt;
{
	if (cnt > 16)
		cnt = 16;
	while (cnt-- > 0) {
		printf(" %02x", *data);
		++data;
	}
}
