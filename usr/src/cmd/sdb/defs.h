#
/*
 *
 *	UNIX debugger - common definitions
 *
 */



/*	Layout of a.out file (fsym):
 *
 *	header of 8 longwords
 *				magic number 410
 *				text size	)
 *				data size	) padded with 0 to multiple of 4 bytes
 *				bss size	)
 *				symbol table size
 *				entry address
 *				size of text relocation info
 *				size of data relocation info
 *
 *
 *	header:		0
 *	text:		32
 *	data:		32+textsize
 *	text reloc:	32+textsize+datasize
 *	data reloc:	32+textsize+datasize+textreloc
 *	symbol table:	32+textsize+datasize+textreloc+datareloc
 *
 */

#ifdef EDDT
#define printf printadb
#endif

#include <sys/param.h>
#include <sys/dir.h>
#include <sys/psl.h>
#include <sys/user.h>
#include "mac.h"
#include "mode.h"


#define VARB	11
#define VARD	13
#define VARE	14
#define VARM	22
#define VARS	28
#define VART	29

#define COREMAGIC 0140000

/* access modes */
#define RD	0
#define WT	1

/* access spaces */
#define NSP	0
#define	ISP	1
#define	DSP	2

#define STAR	4
#define STARCOM 0200
#define DSYM	4
#define ISYM	4
#define ASYM	2
#define NSYM	0
#define ESYM	(-1)
#define BKPTSET	1
#define BKPTEXEC 2
#define	SYMSIZ	100
#define MAXSIG	20

#define USERPS	PSL
#define USERPC	PC
#define BPT	03
#define TBIT	020
#define FD	0200

/* ptracew modes */
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

#define FROFF	(&(0->fpsr))
#define FRLEN	25
#define FRMAX	6

/* the quantities involving ctob() are located in the kernel stack.
/* the others are in the pcb.
*/
#define KSP 0
#define ESP 4
#define SSP 8
#define USP (ctob(4)-5*4)
#define R0 (ctob(4)-19*4)
#define R1 (ctob(4)-18*4)
#define R2 (ctob(4)-17*4)
#define R3 (ctob(4)-16*4)
#define R4 (ctob(4)-15*4)
#define R5 (ctob(4)-14*4)
#define R6 (ctob(4)-13*4)
#define R7 (ctob(4)-12*4)
#define R8 (ctob(4)-11*4)
#define R9 (ctob(4)-10*4)
#define R10 (ctob(4)-9*4)
#define R11 (ctob(4)-8*4)
#define AP (ctob(4)-7*4)
#define FP (ctob(4)-6*4)
#define PC (ctob(4)-2*4)
#define PSL (ctob(4)-1*4)
#define P0BR 80
#define P0LR 84
#define P1BR 88
#define P1LR 92

#define MAXOFF	255
#define MAXPOS	80
#define MAXLIN	128
#define EOR	'\n'
#define SP	' '
#define TB	'\t'
#define QUOTE	0200
#define STRIP	0177
#define LOBYTE	0377
#define EVEN	-2


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
SYMPTR		lookupsym();
SYMPTR		symget();
POS		get();
POS		chkget();
STRING		exform();
L_INT		round();
BKPTR		scanbkpt();
VOID		fault();


INT		mkfault;
INT		executing;
CHAR		*lp;
L_INT		maxoff;
L_INT		maxpos;
ADDR		sigint;
ADDR		sigqit;
INT		wtflag;
L_INT		maxfile;
L_INT		maxstor;
L_INT		txtsiz;
L_INT		datsiz;
L_INT		datbas;
L_INT		stksiz;
STRING		errflg;
INT		magic;
L_INT		entrypt;

CHAR		lastc;

STRING		symfil;
STRING		corfil;
MAP		txtmap;
MAP		datmap;
