/*
 *  Hunt
 *  Copyright (c) 1985 Conrad C. Huang, Gregory S. Couch, Kenneth C.R.C. Arnold
 *  San Francisco, California
 *
 *  Copyright (c) 1985 Regents of the University of California.
 *  All rights reserved.  The Berkeley software License Agreement
 *  specifies the terms and conditions for redistribution.
 */

# include	"hunt.h"
# include	<signal.h>

do_connect(name)
char	*name;
{
	static long	uid;
	extern char	*ttyname();

	uid = htonl(getuid());
	(void) write(Socket, (char *) &uid, sizeof uid);
	(void) write(Socket, name, NAMELEN);
	(void) strcpy(Buf, ttyname(fileno(stderr)));
	(void) write(Socket, Buf, NAMELEN);
# ifdef MONITOR
	(void) write(Socket, (char *) &Am_monitor, sizeof Am_monitor);
# endif MONITOR
}
