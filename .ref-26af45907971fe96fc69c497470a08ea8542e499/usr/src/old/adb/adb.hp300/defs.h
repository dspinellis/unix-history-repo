/*	defs.h	4.6	86/03/26	*/

/*
 * adb - vax string table version; common definitions
 */
#include <sys/param.h>

#include <machine/psl.h>
#ifdef NEWVM
typedef	unsigned long Ooff_t;
#define	off_t Ooff_t
#include <hp300/pte.h>
#else
#include <machine/pte.h>
#endif

#include <sys/user.h>

#include <ctype.h>
#include <a.out.h>
#include <sys/ptrace.h>

#ifdef NEWVM
#undef TRUE
#undef FALSE
#endif
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
#define PSP	8
#define STARCOM 0200

#ifdef pdp11
I guess I should look this up in the 2.10 source...
#endif

#ifdef vax
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
#define getradj(i)	0
#define lget	get

/* puns from <sys/ptrace.h> */
#define	CONTIN	PT_CONTINUE
#define SINGLE	PT_STEP

/* the quantities involving ctob() are located in the kernel stack. */
/* the others are in the pcb. */
#define KSP	0
#define ESP	4
#define SSP	8
#define USP	(ctob(UPAGES)-5*sizeof(int))

/* should read this in from reg.h */
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
#endif

#ifdef hp300
#define NSYM	0		/* ??? */
#define DSYM	1		/* Data space symbol */
#define ISYM	DSYM		/* Instruction space symbol */

#define BKPTSET	1
#define BKPTEXEC 2

extern L_INT	getradj();

#define BPT	0x4e42

/* puns from <sys/ptrace.h> */
#define	CONTIN	PT_CONTINUE
#define SINGLE	PT_STEP

/*
 * Appropriated from dbx (returning a favor)
 * `10' comes from:
 *	(4)	stack starts 4 bytes in from end of uarea
 *	(2)	trap type 0 frame format word is 2 bytes back from that
 *	(4)	trap type 0 pc is 4 bytes back from that
 */
#define	regloc(reg)	(ctob(UPAGES) + (sizeof(int) * ((reg) - 17)) - 10)

/* select map */
#define	mapptr(s)	(((s)&DSP)?&datmap:(((s)&PSP)?&physmap:&txtmap))

#define KSP	12	/* who knows? */

/* should read this in from reg.h */
#define	D0	regloc(0)
#define	D1	regloc(1)
#define	D2	regloc(2)
#define	D3	regloc(3)
#define	D4	regloc(4)
#define	D5	regloc(5)
#define	D6	regloc(6)
#define	D7	regloc(7)
#define	A0	regloc(8)
#define	A1	regloc(9)
#define	A2	regloc(10)
#define	A3	regloc(11)
#define	A4	regloc(12)
#define	A5	regloc(13)
#define	A6	regloc(14)
#define	A7	regloc(15)
/*
 * In 4.4 these have changed, stackadj (pad) field is 4 bytes not 2
 * so PSW/PC are 2 bytes further away from D0.
 */
#if defined(BSD4_4) || defined(HPBSD)
#define	PSW	(regloc(16)+2)
#define	PC	(regloc(17)+2)
#else
#define	PSW	regloc(16)
#define	PC	regloc(17)
#endif
#define FP	A6
#define USP	A7

#define P0BR	60
#define P0LR	64
#define P1BR	68
#define P1LR	72

#define	FP0	332
#define	FP1	344
#define	FP2	356
#define	FP3	368
#define	FP4	380
#define	FP5	392
#define	FP6	404
#define	FP7	416
#define	FPCR	428
#define	FPSR	432
#define	FPIAR	436
#endif

#if !defined(pdp11) && !defined(vax) && !defined(hp300)

"edit this file to accommodate your machine's addressing"

#endif

#define MAXOFF	255
#define MAXPOS	80
#define MAXLIN	128
#define EOR	'\n'
#define SP	' '
#define TB	'\t'
#define QUOTE	0200
#define STRIP	0177
#define EVEN	-2

/* long to ints and back (puns) */
union {
	INT	I[2];
	L_INT	L;
} itolws;

#ifdef pdp11
#define leng(a)		((long)((unsigned)(a)))
#define shorten(a)	((int)(a))
#define itol(a,b)	(itolws.I[0]=(a), itolws.I[1]=(b), itolws.L)
#define lobyte(a)	((a) & 0377)
#endif

#ifdef vax
#define leng(a)		itol(0,a)
#define shorten(a)	((short)(a))
#define itol(a,b)	(itolws.I[0]=(b), itolws.I[1]=(a), itolws.L)
#define lobyte(a)	((a) & 0377)
#endif

#ifdef mc68000
#define leng(a)		itol(0,a)
#define shorten(a)	((short)(a))
#define itol(a,b)	(itolws.I[0]=(a), itolws.I[1]=(b), itolws.L)
#define lobyte(a)	(((a) >> 8) & 0377)
#endif

#if !defined(vax) && !defined(pdp11) && !defined(mc68000)

"edit this file to suit your machine's byte order"

#endif

/* result type declarations */
L_INT		inkdot();
POS		get();
POS		chkget();
POS		lchkget();
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
