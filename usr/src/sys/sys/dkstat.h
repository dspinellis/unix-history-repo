/*	dkstat.h	3.1	%G%	*/

/*
 * Instrumentation
 */
#define	CPUSTATES	4

#define	CP_USER		0
#define	CP_NICE		1
#define	CP_SYS		2
#define	CP_IDLE		3

#define	DK_NDRIVE	4
#define	DK_NSTATES	16		/* 2^DK_NDRIVE */

int	dk_busy;
long	dk_time[CPUSTATES][DK_NSTATES];
long	dk_numb[DK_NDRIVE];
long	dk_wds[DK_NDRIVE];

long	tk_nin;
long	tk_nout;
