/*	kernel.h	4.2	82/09/06	*/

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
