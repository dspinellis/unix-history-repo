/*	mode.h	1.2	87/03/22	*/

#include "machine.h"
/*
 * sdb/adb - common definitions for old srb style code
 */

#define MAXCOM	64
#define MAXARG	32
#define LINSIZ	1024
TYPE	unsigned	ADDR;
TYPE	short	INT;
TYPE	int		VOID;
TYPE	int		L_INT;
TYPE	float		REAL;
TYPE	double		L_REAL;
TYPE	unsigned	POS;
TYPE	char		BOOL;
TYPE	char		CHAR;
TYPE	char		*STRING;
TYPE	char		MSG[];
TYPE	struct map	MAP;
TYPE	MAP		*MAPPTR;
TYPE	struct bkpt	BKPT;
TYPE	BKPT		*BKPTR;
TYPE	int		(*SIG)();


/* file address maps */
struct map {
	POS	b1;
	POS	e1;
	POS	f1;
	POS	b2;
	POS	e2;
	POS	f2;
	INT	ufd;
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
	int	*rkern;
};
