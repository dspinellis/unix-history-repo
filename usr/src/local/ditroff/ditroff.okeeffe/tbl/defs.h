/*	@(#)defs.h	1.4 (Berkeley) %G%	*/
#define max(a,b)	((a) > (b) ? (a) : (b))
#define min(a,b)	((a) < (b) ? (a) : (b))
#define MAXLIN 1024
/*
 * Maximum columns to be output
 */
#define MAXCOL 64

/*
 * MAXCOL is amount of columns to be specified per column
 *
 *  for relation of MAXCOL and MAXHEAD, see next example:
 *	.TS
	l l l		)
	a s s		) Maximum MAXHEAD
	r r n.		)
	-----
	  |
	  Maximum MAXCOL
 *
 * Every table can be MAXCOL, with in each column MAXHEAD
 * specification (lines)
 *
 * Don't make MAXCOL bigger width adjusting nregs in globals.c (bwk)
 */

#define MAXHEAD	64
#define MAXCHS		8192
#define MAXRPT		256
#define CLLEN		128
#define SHORTLINE	4
#define	ZEROW		001
#define HALFUP		002
#define	CTOP		004
#define CDOWN		010
#define CLEFT		000
#define CMID		001
#define CRIGHT		002
#define S1		31
#define S2		32
#define TMP		38
#define SF		35
#define SL		34
#define LSIZE		33
#define SIND		37
#define SVS		36
#define LEFT 1
#define RIGHT 2
#define THRU 3
#define TOP 1
#define BOT 2
#define MACROS "/usr/share/tmac/tmac.s"
#define PYMACS "/usr/share/tmac/tmac.m"

/* devices supported */
#define DEVPSC	3
#define DEVVER	2
#define HARRIS	1
#define CAT	0

struct colstr {
	char *col, *rcol;
	};

#define dprint	if(dbg)printf
