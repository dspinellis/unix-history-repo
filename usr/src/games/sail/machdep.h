/*
 * sccsid = "@(#)machdep.h	2.5 %G%";
 */
#define LOGFILE "/usr/games/lib/saillog"
#define DRIVER1 "driver"
#define DRIVER2 "/usr/games/lib/saildriver"
#define DRIVER3 "/usr/public/.driver"
#define DRIVERNAME "driver"
#define SETUID			/* player and driver run setuid */

#define TIMEOUT 300		/* Sync() time out */

#define BUFSIZE 4096

/* for 4.2bsd machines */
#define blockalarm()	((void) sigblock(1 << SIGALRM-1))
#define unblockalarm()	((void) sigsetmask(sigblock(0) & ~(1 << SIGALRM-1)))

/* for 2.9bsd machines (onyx)
typedef int void;
#define blockalarm()	((void) sighold(SIGALRM))
#define unblockalarm()	((void) sigrelse(SIGALRM))
*/
