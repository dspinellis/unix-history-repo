/*	kernel.h	4.1	82/09/04	*/

/*
 * Global variables for the kernel
 */

/* 1.1 */
long	hostid;
char	hostname[32];
int	nextpid;

/* 1.2 */
struct	timeval boottime;
struct	timeval time;
struct	timezone tz;			/* XXX */
