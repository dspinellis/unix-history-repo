/* config file for ntp compilation */

#define SUN_FLT_BUG		/* sun 3 problems*/
#define SUN_3_3			/* Sun 3.3 strangeness */
#define SETTICKADJ		/* attempt to modify kernel's `tickadj'
				   variable at run time */
#define DEBUG			/* enable optional debugging trace */
#define REFCLOCK		/* define if you have a reference clock
				   attached to your machine.  (untested
				   by UMD) */

#undef	 BROADCAST_NTP		/* experimental support for broadcast NTP */
#undef XADJTIME2		/* experimental support for second-order
				   clock adjustment  */
#undef PSTI			/* define along with REFCLOCK if you have
				   a PSTI clock attached that you'd like
				   to use a a reference clock */
#undef XTAL	1		/* 0 for line freq clock, or
				   1 for crystal controlled clock (default)*/
#undef	 NOSWAP			/* allow use of plock() to prevent swapping */

	
/* if you dont have random/srandom - use the below workaround */
/*
#define srandom(x) srand(x)
#define random()	rand()
*/

#ifndef	WAYTOOBIG
#define	WAYTOOBIG	1000.0	/* Too many seconds to correct, something is
				 * really wrong */
#endif

#ifndef	XTAL
#define	XTAL	1	/* crystal controlled clock by default */
#endif

#ifndef	NTPINITFILE
#define	NTPINITFILE	"/usr/adm/ntp.conf"
#endif

#ifndef	NTPDRIFTCOMP
#define	NTPDRIFTCOMP	"/usr/adm/ntp.drift"
#endif
