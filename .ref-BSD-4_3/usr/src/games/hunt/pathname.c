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
 * to Test_port and Sock_port.  They're just random numbers greater
 * than then range reserved for privileged sockets.
 */

# ifdef DEBUG

char	*Driver =	"/va/conrad/games/src/hunt/hunt.driver.dbg";
# ifdef INTERNET
int	Test_port =	('h' << 8) | 't';
int	Sock_port =	('h' << 8) | 's';
# else INTERNET
char	*Sock_name =	"/tmp/hunt";
# endif INTERNET

# else DEBUG

char	*Driver =	"/usr/games/lib/hunt.driver";
# ifdef INTERNET
int	Test_port =	('h' << 8) | 't';
int	Sock_port =	('h' << 8) | 's';
# else INTERNET
char	*Sock_name =	"/tmp/hunt";
# endif INTERNET

# endif DEBUG
