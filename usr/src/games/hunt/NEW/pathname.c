/*
 *  Hunt
 *  Copyright (c) 1985 Conrad C. Huang, Gregory S. Couch, Kenneth C.R.C. Arnold
 *  San Francisco, California
 *
 *  Copyright (c) 1985 Regents of the University of California.
 *  All rights reserved.  The Berkeley software License Agreement
 *  specifies the terms and conditions for redistribution.
 */

/*
 * There is no particular significance to the numbers assigned
 * to Test_port.  They're just random numbers greater than the
 * range reserved for privileged sockets.
 */

# include	<sys/types.h>

# ifdef DEBUG

char	*Driver =	"/va/conrad/games/src/hunt/huntd.dbg";
# ifdef INTERNET
u_short	Test_port =	('h' << 8) | 't';
# else INTERNET
char	*Sock_name =	"/tmp/hunt";
char	*Stat_name =	"/tmp/hunt.stats";
# endif INTERNET

# else DEBUG

char	*Driver =	HUNTD;
# ifdef INTERNET
u_short	Test_port =	('h' << 8) | 't';
# else INTERNET
char	*Sock_name =	"/tmp/hunt";
char	*Stat_name =	"/tmp/hunt.stats";
# endif INTERNET

# endif DEBUG
