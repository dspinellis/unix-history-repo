/*	defs.h	4.1	81/05/14	*/

/*
 * adb - vax string table version; common definitions
 */

#include <sys/param.h>
#include <sys/dir.h>
#include <sys/psl.h>
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
#define STARCOM 0200

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

#define USERPS	PSL
#define USERPC	PC
#define BPT	03
#define TBIT	020
#define FD	0200
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
#define ESP	4
#define SSP	8
#define USP	(ctob(UPAGES)-5*sizeof(int))
#define R0	(ctob(UPAGES)-18*sizeof(int))
#define R1	(ctob(UPAGES)-17*sizeof(int))
#define R2	(ctob(UPAGES)-16*sizeof(int))
#define R3	(ctob(UPAGES)-15*sizeof(int))
#define R4	(ctob(UPAGES)-14*sizeof(int))
#define R5	(ctob(UPAGES)-13*sizeof(int))
#define R6	(ctob(UPAGES)-12*sizeof(int))
#define R7	(ctob(UPAGES)-11*sizeof(int))
#define R8	(ctob(UPAGES)-10*sizeof(int))
#define R9	(ctob(UPAGES)-9*sizeof(int))
#define R10	(ctob(UPAGES)-8*sizeof(int))
#define R11	(ctob(UPAGES)-7*sizeof(int))
#define AP	(ctob(UPAGES)-21*sizeof(int))
#define FP	(ctob(UPAGES)-20*sizeof(int))
#define PC	(ctob(UPAGES)-2*sizeof(int))
#define PSL	(ctob(UPAGES)-1*sizeof(int))
#define P0BR	80
#define P0LR	84
#define P1BR	88
#define P1LR	92

#define MAXOFF	255
#define MAXPOS	80
#define MAXLIN	128
#define EOF	0
#define EOR	'\n'
#define SP	' '
#define TB	'\t'
#define QUOTE	0200
#define STRIP	0177
#define LOBYTE	0377
#define EVEN	-2

/* long to ints and back (puns) */
union {
	INT	I[2];
	L_INT	L;
} itolws;

#ifndef vax
#define leng(a)		((long)((unsigned)(a)))
#define shorten(a)	((int)(a))
#define itol(a,b)	(itolws.I[0]=(a), itolws.I[1]=(b), itolws.L)
#else
#define leng(a)		itol(0,a)
#define shorten(a)	((short)(a))
#define itol(a,b)	(itolws.I[0]=(b), itolws.I[1]=(a), itolws.L)
#endif

/* result type declarations */
L_INT		inkdot();
POS		get();
POS		chkget();
STRING		exform();
L_INT		round();
BKPTR		scanbkpt();
VOID		fault();
