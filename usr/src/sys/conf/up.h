/*	up.h	4.1	11/9/80	*/

/*
 * UNIBUS address of disk controller (e.g. EMULEX SC-21)
 */
#define	UPADDR		((struct device *)(UBA0_DEV + 0176700))

/*
 * Number of drives
 */
#define	NUP	2

/*
 * Drive numbers assigned for iostat/vmstat
 * Currently drives on hp are 0 and 1, drives on up are 2 and 3.
 */
#ifndef	DK_N
#define	DK_N	2
#define	DK_NMAX	3
#endif
