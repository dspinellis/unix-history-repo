#ifndef lint
static char sccsid[] = "@(#)res_init.c	4.1 (Berkeley) %G%";
#endif

#include <sys/types.h>
#include <sys/socket.h>
#include <netinet/in.h>
#include <stdio.h>
#include <nameser.h>
#include <resolv.h>

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
	extern char *index();

	_res.options |= RES_INIT;
	_res.nsaddr.sin_family = AF_INET;
	_res.nsaddr.sin_addr.s_addr = INADDR_ANY;
	_res.nsaddr.sin_port = HTONS(53);	/* well known port number */

	/* first try reading the config file */
	if ((fp = fopen(CONFFILE, "r")) != NULL) {
		if (fgets(_res.defdname, MAXDNAME, fp) == NULL)
			_res.defdname[0] = '\0';
		else if ((cp = index(_res.defdname, '\n')) != NULL)
			*cp = '\0';
		if (fgets(buf, sizeof (buf), fp) != NULL) {
			(void) fclose(fp);
			_res.nsaddr.sin_addr.s_addr = inet_addr(buf);
			return (1);
		}
		(void) fclose(fp);
	}

	/* next, try getting the address of this host */

	/* finally, try broadcast */

	return (0);
}
