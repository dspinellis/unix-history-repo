/*	kernel.h	4.3	82/09/08	*/

/*
 * Global variables for the kernel
 */

/* 1.1 */
long	hostid;
char	hostname[32];
int	hostnamelen;
int	nextpid;

/* 1.2 */
struct	timeval boottime;
struct	timeval time;
struct	timezone tz;			/* XXX */
int	hz;
int	tick;
int	lbolt;				/* awoken once a second */
int	unsel(),unrto();
