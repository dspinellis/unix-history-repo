#include "machine.h"
/*
 *	UNIX debugger
 */

#define MAXCOM	64
#define MAXARG	32
#define LINSIZ	256
TYPE	long	ADDR;
#ifndef vax
TYPE	int		INT;
#else
TYPE	short	INT;
#endif
TYPE	int		VOID;
TYPE	long int	L_INT;
TYPE	float		REAL;
TYPE	double		L_REAL;
TYPE	unsigned	POS;
TYPE	char		BOOL;
TYPE	char		CHAR;
TYPE	char		*STRING;
TYPE	char		MSG[];
TYPE	struct map	MAP;
TYPE	MAP		*MAPPTR;
TYPE	struct symtab	SYMTAB;
TYPE	SYMTAB		*SYMPTR;
TYPE	struct symslave SYMSLAVE;
TYPE	struct bkpt	BKPT;
TYPE	BKPT		*BKPTR;


/* file address maps */
struct map {
	L_INT	b1;
	L_INT	e1;
	L_INT	f1;
	L_INT	b2;
	L_INT	e2;
	L_INT	f2;
	INT	ufd;
};


/* slave table for symbols */
struct symslave {
	SYMV	valslave;
	INT	typslave;
};

struct bkpt {
	ADDR	loc;
	ADDR	ins;
	INT	count;
	INT	initcnt;
	INT	flag;
	CHAR	comm[MAXCOM];
	BKPT	*nxtbkpt;
};

TYPE	struct reglist	REGLIST;
TYPE	REGLIST		*REGPTR;
struct reglist {
	STRING	rname;
	INT	roffs;
};

struct {
	INT	junk[2];
	INT	fpsr;
	REAL	Sfr[6];
};

struct {
	INT	junk[2];
	INT	fpsr;
	L_REAL	Lfr[6];
};

