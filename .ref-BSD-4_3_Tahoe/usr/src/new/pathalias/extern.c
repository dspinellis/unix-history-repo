/* pathalias -- by steve bellovin, as told to peter honeyman */
#ifndef lint
static char	*sccsid = "@(#)extern.c	8.1 (down!honey) 86/01/19";
#endif lint

#include "def.h"

node	*Home;
char	*Cfile;
char	**Ifiles;
char	*ProgName;
int	Vflag;
int	Cflag;
int	Iflag;
int	Tflag;
int	Lineno = 1;
char	*Netchars = "!:@%";	/* sparse, but sufficient */
int	Ncount;
int	Lcount;
char	*Graphout;
char	*Linkout;
node	**Table;		/* hash table + priority queue */
long	Tabsize;		/* size of Table */	
node	*Private;		/* list of private nodes in this file */
long	Hashpart;		/* used while mapping -- above is heap */
int	Scanstate = NEWLINE;	/* scanner (yylex) state */
