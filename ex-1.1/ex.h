#define	VISUAL
/*
 * Ex - text editor
 * Bill Joy UCB June 1977
 *
 * Based on an earlier editor "ex" written by
 * William Joy and Charles Haley.
 *
 * And of course an inestimable debt to "ed"!
 */

#define	STATIC
#define	CHAR

#define	NIL	0
#define	QUOTE	0200

#define	HUP	1
#define	INTR	2
#define	QUIT	3

#define	FNSIZE	64
#define	LBSIZE	512

#define	EOF	-1

char	ruptible, inglobal, inopen, inconf, listf, endline, laste, intty;
char	shudclob, diddle, die;

int	chngflag, xchngflag, tchngflag;

char	/* savedfile[FNS, */ file[FNSIZE], altfile[FNSIZE];
char	linebuf[LBSIZE], genbuf[LBSIZE];

int	*address(), *addr1, *addr2;
int	*zero, *one, *dot, *dol, *unddol, *endcore, *fendcore;
int	*unddel, *undap1, *undap2, *undadot;
char	undkind;

#define	UNDCHANGE	0
#define	UNDMOVE		1
#define	UNDALL		2
#define	UNDNONE		3

int	io, erfile, tfile;
char	*globp, *erpath;
int	names[27];
int	outcol;
char	home[30];
char	*Command;

int	getfile(), gettty(), getchar(), getsub();


#include "ex_vars.h"

#define	eq(a, b)	(strcmp(a, b) == 0)

#define	CTRL(c)	('c' & 037)

#define	ECHO	010
#define	RAW	040

char	normtty;
int	normf;
struct {
	int	fildes;
	int	nunused;
	char	*xfree;
	char	buff[512];
} obuf;
int	oldhup, onhup(), oldquit, onintr();

struct {
	int	Atime[2];
	int	Auid;
	int	Alines;
	int	Afname[FNSIZE];
	int	Ablocks[100];
} header;

#define	savedfile	header.Afname
#define	blocks		header.Ablocks

int	dirtcnt;

char	recov;

char	TTYNAM[];
int	TMODE;

int	lastc, peekc;
#define	lastchar()	lastc
#define	setlastchar(c)	lastc = c
#define	ungetchar(c)	peekc = c

char	aiflag;
#define	setai(i)	aiflag = i

int	pid, rpid, status, tty[3];
char	allredraw, pfast;
int	mask, vcntcol;
