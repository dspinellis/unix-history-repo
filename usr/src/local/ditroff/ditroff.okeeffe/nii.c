#ifndef lint
static char sccsid[] = "@(#)nii.c	1.1 (CWI) 85/07/17";
#endif lint

#include "tdef.h"
#ifdef NROFF
#include "tw.h"
#endif
#include "s.h"
#include "d.h"
#include "v.h"
#include <sgtty.h>

int	*vlist = (int *)
&v;
struct s *frame, *stk, *ejl;
struct s *nxf;

int	pipeflg;
#ifdef NROFF
int	hflg;
int	eqflg;
#endif

#ifndef NROFF
int	xpts;
int	*pslp;
int	psflg;
int	ppts;
int	pfont;
int	paper;
int	mpts;
int	mfont;
int	cs;
int	code;
int	ccs;
int	bd;
int	back;
#endif

int	level;
int	stdi;
int	waitf;
int	nofeed;
int	quiet;
int	stop;
char	ibuf[IBUFSZ];
char	xbuf[IBUFSZ];
char	*ibufp;
char	*xbufp;
char	*eibuf;
char	*xeibuf;
tchar	cbuf[NC];
tchar	*cp;
int	nx;
int	mflg;
tchar	ch = 0;
int	cps;
int	ibf;
int	ttyod;
struct sgttyb ttys;
int	iflg;
char	*enda;
int	rargc;
char	**argp;
int	trtab[NTRTAB];
int	lgf;
int	copyf;
tchar	ch0;
int	cwidth;
filep ip;
int	nlflg;
tchar	*ap;
int	donef;
int	nflush;
int	nchar;
tchar	rchar;
int	nfo;
int	ifile;
int	padc;
int	raw;
int	ifl[NSO];
int	ifi;
int	flss;
int	nonumb;
int	trap;
int	tflg;
int	ejf;
int	gflag;
int	dilev;
int	tlss;
filep offset;
int	em;
int	ds;
filep woff;
int	app;
int	ndone;
int	lead;
int	ralss;
filep nextb;
tchar	nrbits;
int	nform;
int	oldmn;
int	newmn;
int	macerr;
filep apptr;
int	diflg;
filep roff;
int	wbfi;
int	inc[NN];
int	fmt[NN];
int	evi;
int	vflag;
int	noscale;
int	po1;
int	nlist[NTRAP];
int	mlist[NTRAP];
int	evlist[EVLSZ];
int	ev;
int	tty;
int	sfont	= FT;	/* appears to be "standard" font; used by .ul */
int	sv;
int	esc;
int	widthp;
int	xfont;
int	setwdf;
int	xbitf;
int	over;
int	nhyp;
tchar	**hyp;
int	*olinep;
int	ttysave = -1;
int	dotT;
char	*unlkp;
int	no_out;
