/*	head.h	1.1	86/02/25	*/

ADDR	maxoff;
ADDR	localval;

struct	nlist *symtab, *esymtab;
struct	nlist *cursym;
struct	nlist *lookup();

struct	exec filhdr;

long	var[36];

int	xargc;

MAP	txtmap;
MAP	datmap;
INT	wtflag;
INT	fcor;
INT	fsym;
L_INT	maxfile;
L_INT	maxstor;
INT	signo;

union {
	struct	user U;
	char	UU[ctob(UPAGES)];
} udot;
#define	u	udot.U

char	*corfil, *symfil;

