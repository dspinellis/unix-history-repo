/*
 * Copyright (c) 1985 Regents of the University of California.
 * All rights reserved.  The Berkeley software License Agreement
 * specifies the terms and conditions for redistribution.
 */

#ifndef lint
static char sccsid[] = "@(#)res_init.c	5.8 (Berkeley) %G%";
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
char	*conffile = "/etc/resolv.conf";
#endif

/*
 * Resolver state default settings
 */
struct state _res = {
	10,
	4,
	RES_RECURSE|RES_DEFNAMES,
};

/*
 * Set up default settings.  If the configuration file exist, the values
 * there will have precedence.  Otherwise, the server address is set to
 * INADDR_ANY and the default domain name comes from the gethostname().
 *
 * The configuration file should only be used if you want to redefine your
 * domain or run without a server on your machine.
 *
 * Return 0 if completes successfully, -1 on error
 */
res_init()
{
	FILE *fp;
	char buf[BUFSIZ], *cp;
	extern u_long inet_addr();
	extern char *index();
	extern char *strcpy(), *strncpy();
#ifdef DEBUG
	extern char *getenv();
#endif

	_res.nsaddr.sin_family = AF_INET;
	_res.nsaddr.sin_addr.s_addr = INADDR_ANY;
	_res.defdname[0] = '\0';
	_res.nsaddr.sin_port = htons(NAMESERVER_PORT);

	if ((fp = fopen(conffile, "r")) != NULL) {
		while (fgets(buf, sizeof(buf), fp) != NULL) {
			if (!strncmp(buf, "domain", sizeof("domain") - 1)) {
				cp = buf + sizeof("domain") - 1;
				while (*cp == ' ' || *cp == '\t')
					cp++;
				if (*cp == '\0')
					continue;
				(void)strncpy(_res.defdname, cp,
				        sizeof(_res.defdname));
				_res.defdname[sizeof(_res.defdname) - 1] = '\0';
				if ((cp = index(_res.defdname, '\n')) != NULL)
					*cp = '\0';
				continue;
			}
			if (!strncmp(buf, "resolver", sizeof("resolver") - 1)) {
				cp = buf + sizeof("resolver") - 1;
				while (*cp == ' ' || *cp == '\t')
					cp++;
				if (*cp == '\0')
					continue;
				_res.nsaddr.sin_addr.s_addr = inet_addr(cp);
				if (_res.nsaddr.sin_addr.s_addr == (unsigned)-1)
					_res.nsaddr.sin_addr.s_addr =
						INADDR_ANY;
				continue;
			}
		}
		(void) fclose(fp);
	}
	if (_res.defdname[0] == 0) {
		if (gethostname(buf, sizeof(_res.defdname)) == 0 &&
		    (cp = index(buf, '.')))
			(void)strcpy(_res.defdname, cp + 1);
	}

#ifdef DEBUG
	/* Allow user to override the local domain definition */
	if ((cp = getenv("LOCALDOMAIN")) != NULL)
		(void)strncpy(_res.defdname, cp, sizeof(_res.defdname));
#endif
	_res.options |= RES_INIT;
	return(0);
}
