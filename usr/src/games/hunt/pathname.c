/*
 * Copyright (c) 1985 Regents of the University of California.
 * All rights reserved.
 *
 * Redistribution and use in source and binary forms are permitted
 * provided that the above copyright notice and this paragraph are
 * duplicated in all such forms and that any documentation,
 * advertising materials, and other materials related to such
 * distribution and use acknowledge that the software was developed
 * by the University of California, Berkeley.  The name of the
 * University may not be used to endorse or promote products derived
 * from this software without specific prior written permission.
 * THIS SOFTWARE IS PROVIDED ``AS IS'' AND WITHOUT ANY EXPRESS OR
 * IMPLIED WARRANTIES, INCLUDING, WITHOUT LIMITATION, THE IMPLIED
 * WARRANTIES OF MERCHANTIBILITY AND FITNESS FOR A PARTICULAR PURPOSE.
 */

#ifndef lint
static char sccsid[] = "@(#)pathname.c	5.2 (Berkeley) 6/27/88";
#endif /* not lint */

/*
 *  Hunt
 *  Copyright (c) 1985 Conrad C. Huang, Gregory S. Couch, Kenneth C.R.C. Arnold
 *  San Francisco, California
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
