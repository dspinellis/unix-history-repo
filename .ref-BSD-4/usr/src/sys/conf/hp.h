/*	hp.h	4.1	11/9/80	*/

/*
 * MASSBUS on which rp06/rm03/rm05 is
 */
#define	HPMBA		MBA0
#define	HPMBANUM	0

/*
 * Number of drives
 */
#define	NHP	2

/*
 * Drive numbers assigned for iostat/vmstat
 * Currently drives on hp are 0 and 1, drives on up are 2 and 3.
 */
#ifndef	DK_N
#define	DK_N	0
#define	DK_NMAX	1
#endif
