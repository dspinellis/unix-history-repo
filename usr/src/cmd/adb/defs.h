#
/*
 *
 *	UNIX debugger - common definitions
 *
 */



/*	Layout of a.out file (fsym):
 *
 *	header of 8 words	magic number 405, 407, 410, 411
 *				text size	)
 *				data size	) in bytes but even
 *				bss size	)
 *				symbol table size
 *				entry point
 *				{unused}
 *				flag set if no relocation
 *
 *
 *	header:		0
 *	text:		16
 *	data:		16+textsize
 *	relocation:	16+textsize+datasize
 *	symbol table:	16+2*(textsize+datasize) or 16+textsize+datasize
 *
 */


#include <sys/param.h>
#include <sys/dir.h>
#include <sys/reg.h>
#include <sys/user.h>
#include <sgtty.h>
#include "mac.h"
#include "mode.h"


#define VARB	11
#define VARD	13
#define VARE	14
#define VARM	22
#define VARS	28
#define VART	29

#define COREMAGIC 0140000

#define RD	0
#define WT	1
#define NSP	0
#define	ISP	1
#define	DSP	2
#define STAR	4
#define STARCOM 0200
#define DSYM	7
#define ISYM	2
#define ASYM	1
#define NSYM	0
#define ESYM	(-1)
#define BKPTSET	1
#define BKPTEXEC 2
#define	SYMSIZ	100
#define MAXSIG	20

#define USERPS	2*(512-1)
#define USERPC	2*(512-2)
#define BPT	03
#define FD	0200
#define	SETTRC	0
#define	RDUSER	2
#define	RIUSER	1
#define	WDUSER	5
#define WIUSER	4
#define	RUREGS	3
#define	WUREGS	6
#define	CONTIN	7
#define	SINGLE	9
#define	EXIT	8

#define FROFF	(&(0->fpsr))
#define FRLEN	25
#define FRMAX	6

#define	ps	-1
#define	pc	-2
#define	sp	-6
#define	r5	-9
#define	r4	-10
#define	r3	-11
#define	r2	-12
#define	r1	-5
#define	r0	-3

#define MAXOFF	255
#define MAXPOS	80
#define MAXLIN	128
#define EOF	0
#define EOR	'\n'
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

#define leng(a)		((long)((unsigned)(a)))
#define shorten(a)	((int)(a))
#define itol(a,b)	(itolws.I[0]=(a), itolws.I[1]=(b), itolws.L)



/* result type declarations */
L_INT		inkdot();
SYMPTR		lookupsym();
SYMPTR		symget();
POS		get();
POS		chkget();
STRING		exform();
L_INT		round();
BKPTR		scanbkpt();
VOID		fault();

typedef struct sgttyb TTY;
TTY	adbtty, usrtty;
#include <setjmp.h>
jmp_buf erradb;
