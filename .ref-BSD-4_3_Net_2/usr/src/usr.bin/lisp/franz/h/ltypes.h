/*					-[Fri Mar  4 12:11:36 1983 by jkf]-
 * 	ltypes.h			$Locker:  $
 * lisp data type defs
 *
 * $Header: ltypes.h,v 1.2 83/03/04 12:30:22 jkf Exp $
 *
 * (c) copyright 1982, Regents of the University of California
 */
 
/* type flags */

#define	UNBO	-1
#define	STRNG	0
#define	ATOM	1
#define	INT	2
#define	DTPR	3
#define DOUB	4
#define	BCD	5
#define	PORT	6
#define	ARRAY	7
#define OTHER   8
#define SDOT	9
#define VALUE	10

#define HUNK2	11		/* The hunks */
#define HUNK4	12
#define HUNK8	13
#define HUNK16	14
#define HUNK32	15
#define HUNK64	16
#define HUNK128	17

#define VECTOR  18
#define VECTORI 19

