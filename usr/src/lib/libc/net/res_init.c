/*
 * Copyright (c) 1985 Regents of the University of California.
 * All rights reserved.  The Berkeley software License Agreement
 * specifies the terms and conditions for redistribution.
 */

#ifndef lint
static char sccsid[] = "@(#)res_init.c	5.4 (Berkeley) %G%";
#endif not lint

#include <sys/types.h>
#include <sys/socket.h>
#include <netinet/in.h>
#include <stdio.h>
#include <arpa/nameser.h>
#include <arpa/resolv.h>

/*
 * Resolver configuration file. Contains the address of the
 * inital name server to query and the default domain for
 * non fully qualified domain names.
 */

#ifdef CONFFILE
char	*conffile = CONFFILE;
#else
char	*conffile = "/usr/local/lib/resolv.conf";
#endif

/*
 * Resolver state default settings
 */
struct state _res = {
	90,
	2,
	RES_RECURSE|RES_DEFNAMES,
};

/*
 * Read the configuration file for default settings.
 * Return true if the name server address is initialized.
 */
res_init()
{
	FILE *fp;
	char buf[BUFSIZ], *cp;
	int n;
	extern u_long inet_addr();
	extern char *index(), *getenv();

	_res.options |= RES_INIT;
	_res.nsaddr.sin_family = AF_INET;
	_res.nsaddr.sin_addr.s_addr = INADDR_ANY;
	_res.nsaddr.sin_port = htons(NAMESERVER_PORT);

	/* first try reading the config file */
	if ((fp = fopen(conffile, "r")) != NULL) {
		if (fgets(_res.defdname, sizeof(_res.defdname), fp) == NULL)
			_res.defdname[0] = '\0';
		else if ((cp = index(_res.defdname, '\n')) != NULL)
			*cp = '\0';
		if (fgets(buf, sizeof (buf), fp) != NULL)
			_res.nsaddr.sin_addr.s_addr = inet_addr(buf);
		(void) fclose(fp);
	}

	/* Allow user to override the local domain definition */
	if ((cp = getenv("LOCALDOMAIN")) != NULL)
		strncpy(_res.defdname, cp, sizeof(_res.defdname));
}
