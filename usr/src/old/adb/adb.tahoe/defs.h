/*	defs.h	1.3	86/11/20	*/

/*
 * adb - vax string table version; common definitions
 */

#include <sys/param.h>
#include <sys/dir.h>
#include <machine/psl.h>
#include <machine/pte.h>
#include <sys/user.h>
#include <ctype.h>
#include <a.out.h>

#include "mac.h"
#include "mode.h"
#include "head.h"

/* access modes */
#define RD	0
#define WT	1

#define NSP	0
#define	ISP	1
#define	DSP	2
#define STAR	4

/*
 * Symbol types, used internally in calls to findsym routine.
 * One the VAX this all degenerates since I & D symbols are indistinct.
 * Basically we get NSYM==0 for `=' command, ISYM==DSYM otherwise.
 */
#define NSYM	0
#define DSYM	1		/* Data space symbol */
#define ISYM	DSYM		/* Instruction space symbol == DSYM on VAX */

#define BKPTSET	1
#define BKPTEXEC 2

#define BPT	0x30
#define KCALL	0xcf
#define CASEL	0xfc
#define TBIT	0x10
#define	SETTRC	0
#define	RDUSER	2
#define	RIUSER	1
#define	WDUSER	5
#define WIUSER	4
#define	RUREGS	3
#define	WUREGS	6
#define	CONTIN	7
#define	EXIT	8
#define SINGLE	9

/* the quantities involving ctob() are located in the kernel stack. */
/* the others are in the pcb. */
#define KSP	0
#define USP	(ctob(UPAGES)-9*sizeof (int))
#define R0	(ctob(UPAGES)-25*sizeof (int))
#define R1	(ctob(UPAGES)-24*sizeof (int))
#define R2	(ctob(UPAGES)-23*sizeof (int))
#define R3	(ctob(UPAGES)-22*sizeof (int))
#define R4	(ctob(UPAGES)-21*sizeof (int))
#define R5	(ctob(UPAGES)-20*sizeof (int))
#define R6	(ctob(UPAGES)-19*sizeof (int))
#define R7	(ctob(UPAGES)-18*sizeof (int))
#define R8	(ctob(UPAGES)-17*sizeof (int))
#define R9	(ctob(UPAGES)-16*sizeof (int))
#define R10	(ctob(UPAGES)-15*sizeof (int))
#define R11	(ctob(UPAGES)-14*sizeof (int))
#define R12	(ctob(UPAGES)-13*sizeof (int))
#define FP	(ctob(UPAGES)-10*sizeof (int))
#define PC	(ctob(UPAGES)-2*sizeof (int))
#define PSL	(ctob(UPAGES)-1*sizeof (int))
#define P0BR	72
#define P0LR	76
#define P1BR	80
#define P1LR	84
#define P2BR	88
#define P2LR	92
#define ACHI	(ctob(UPAGES)-6*sizeof (int))
#define ACLO	(ctob(UPAGES)-7*sizeof (int))
#define HFS	(ctob(UPAGES)-8*sizeof (int))

#define MAXOFF	1024
#define MAXPOS	80
#define MAXLIN	256
#define QUOTE	0200
#define ALIGN	-4

#ifndef vax
#define leng(a)		((long)((unsigned)(a)))
#define shorten(a)	(((a) >> 16) & 0xffff)
#define	itol(a,b)	(((a) << 16) | ((b) & 0xffff))
#define	byte(a)		(((a) >> 24) & 0xff)
#define	btol(a)		((a) << 24)
#else
#define leng(a)		itol(0,a)
#define	shorten(a)	((a) & 0xffff)
#define	itol(a,b)	(((a) & 0xffff) | ((b) << 16))
#define	byte(a)		((a) & 0xff)
#define	btol(a)		(a)
#endif

/* result type declarations */
L_INT		inkdot();
POS		get();
POS		chkget();
STRING		exform();
L_INT		round();
BKPTR		scanbkpt();
VOID		fault();

struct	pcb	pcb;
int	kernel;
int	kcore;
struct	pte *sbr;
int	slr;
int	masterpcbb;
