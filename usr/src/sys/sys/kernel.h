/*	kernel.h	4.4	82/09/12	*/

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
int	selitexpire();
int	realitexpire();

#ifdef GPROF
extern	int profiling;
extern	char *s_lowpc;
extern	u_long s_textsize;
extern	u_short *kcount;
#endif
