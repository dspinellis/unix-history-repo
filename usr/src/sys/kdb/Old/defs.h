/*
 * Copyright (c) 1986, 1989 Regents of the University of California.
 * All rights reserved.  The Berkeley software License Agreement
 * specifies the terms and conditions for redistribution.
 *
 *	@(#)defs.h	7.9 (Berkeley) 2/20/90
 */

/*
 * adb - string table version; common definitions
 */

#include "param.h"
#include "user.h"
#include "proc.h"

#include "machine/psl.h"
#include "machine/pte.h"

#include <a.out.h>
#include <ctype.h>

struct	pcb kdbpcb;		/* must go before redef.h */

#include "redef.h"
#include "machine/kdbparam.h"

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

/*
 * setexit/reset tokens..
 */
#define	ERROR	1
#define	NEXT	6
#define	SINGLE	7
#define	CONTIN	8
#define	PANIC	9

#define LINSIZ	256
#define MAXOFF	65535
#define MAXPOS	80
#define MAXLIN	256
#define QUOTE	0200

#define LOBYTE	0377
#define STRIP	0177

#define SP	' '
#define TB	'\t'
#define EOR	'\n'
#define	CTRL(c)	(c&037)

#define	eqstr(a,b)	(strcmp(a,b)==0)

typedef	unsigned ADDR;
typedef	unsigned POS;

typedef	struct bkpt {
	ADDR	loc;
	ADDR	ins;
	short	count;
	short	initcnt;
	short	flag;
#define MAXCOM	64
	char	comm[MAXCOM];
	struct	bkpt *nxtbkpt;
} BKPT, *BKPTR;

typedef	struct {
	char	*rname;
	int	*rkern;
} REGLIST, *REGPTR;

ADDR	maxoff;
ADDR	localval;
int	mkfault;
long	var[36];
char	*errflg;
long	dot;
int	dotinc;
long	adrval;
int	adrflg;
long	cntval;
int	cntflg;

/* result type declarations */
long	inkdot();
u_int	get();
u_int	chkget();
u_int	bchkget();
char	*exform();
BKPTR	scanbkpt();

struct	nlist *symtab, *esymtab;
struct	nlist *cursym;
struct	nlist *lookup();
