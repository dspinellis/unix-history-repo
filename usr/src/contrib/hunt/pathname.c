/*
 *  Hunt
 *  Copyright (c) 1985 Conrad C. Huang, Gregory S. Couch, Kenneth C.R.C. Arnold
 *  San Francisco, California
 */

/*
 * There is no particular significance to the numbers assigned
 * to Test_port.  They're just random numbers greater than the
 * range reserved for privileged sockets.
 */

# include	<sys/types.h>

# ifdef DEBUG

char	*Driver =	"/home/socr/a/conrad/games/src/hunt/huntd.dbg";
#  ifdef INTERNET
u_short	Test_port =	('h' << 8) | 't';
#  else
char	*Sock_name =	"/tmp/hunt";
char	*Stat_name =	"/tmp/hunt.stats";
#  endif

# else

char	*Driver =	HUNTD;
#  ifdef INTERNET
u_short	Test_port =	('h' << 8) | 't';
#  else
char	*Sock_name =	"/tmp/hunt";
char	*Stat_name =	"/tmp/hunt.stats";
#  endif

# endif
