/*-
 * Copyright (c) 1990 The Regents of the University of California.
 * All rights reserved.
 *
 * This code is derived from software contributed to Berkeley by
 * William Jolitz.
 *
 * %sccs.include.386.c%
 *
 *	@(#)pccons.c	5.1 (Berkeley) %G%
 */

/*
 * code to work keyboard & display for console
 */
#include "param.h"
#include "conf.h"
#include "dir.h"
#include "ioctl.h"
#include "user.h"
#include "proc.h"
#include "tty.h"
#include "uio.h"
#include "callout.h"
#include "systm.h"
#include "kernel.h"
#include "syslog.h"
#include "icu.h"

struct	tty cons;

struct	consoftc {
	char	cs_flags;
#define	CSF_ACTIVE	0x1	/* timeout active */
#define	CSF_POLLING	0x2	/* polling for input */
	char	cs_lastc;	/* last char sent */
	int	cs_timo;	/* timeouts since interrupt */
	u_long	cs_wedgecnt;	/* times restarted */
} consoftc;

/*
 * We check the console periodically to make sure
 * that it hasn't wedged.  Unfortunately, if an XOFF
 * is typed on the console, that can't be distinguished
 * from more catastrophic failure.
 */
#define	CN_TIMERVAL	(hz)		/* frequency at which to check cons */
#define	CN_TIMO		(2*60)		/* intervals to allow for output char */

int	cnstart();
int	ttrstrt();
char	partab[];

/*
 * Wait for CP to accept last CP command sent
 * before setting up next command.
 */
#define	waitforlast(timo) { \
	if (cnlast) { \
		(timo) = 10000; \
		do \
			uncache((char *)&cnlast->cp_unit); \
		while ((cnlast->cp_unit&CPTAKE) == 0 && --(timo)); \
	} \
}

u_char inb();

/*ARGSUSED*/
cnopen(dev, flag)
	dev_t dev;
{
	register struct tty *tp;
	int unit = minor(dev);

	tp = &cons;
	if (tp->t_state&TS_XCLUDE && u.u_uid != 0)
		return (EBUSY);
	tp->t_oproc = cnstart;
	if ((tp->t_state&TS_ISOPEN) == 0) {
		ttychars(tp);
		tp->t_state = TS_ISOPEN|TS_CARR_ON;
		tp->t_flags = EVENP|ECHO|XTABS|CRMOD;
		INTREN(IRQ1);
		splnone();
	}
	return ((*linesw[tp->t_line].l_open)(dev, tp));
}


cnclose(dev)
	dev_t dev;
{
	(*linesw[cons.t_line].l_close)(&cons);
	ttyclose(&cons);
	INTRDIS(IRQ1);
}

/*ARGSUSED*/
cnread(dev, uio)
	dev_t dev;
	struct uio *uio;
{
	return ((*linesw[cons.t_line].l_read)(&cons, uio));
}

/*ARGSUSED*/
cnwrite(dev, uio)
	dev_t dev;
	struct uio *uio;
{
	return ((*linesw[cons.t_line].l_write)(&cons, uio));
}

/*
 * Got a console receive interrupt -
 * the console processor wants to give us a character.
 * Catch the character, and see who it goes to.
 */
cnrint(dev)
	dev_t dev;
{
	int c;

	c = sgetc(1);
	if (c&0x100)return;
	if (consoftc.cs_flags&CSF_POLLING)
		return;
#ifdef KDB
	if (kdbrintr(c, &cons))
		return;
#endif
	(*linesw[cons.t_line].l_rint)(c&0xff, &cons);
}

cnioctl(dev, cmd, addr, flag)
	dev_t dev;
	caddr_t addr;
{
	register struct tty *tp = &cons;
	register error;
 
	error = (*linesw[tp->t_line].l_ioctl)(tp, cmd, addr);
	if (error >= 0)
		return error;
	if ((error = ttioctl(tp, cmd, addr, flag)) < 0)
		error = ENOTTY;
	else if (cmd == TIOCSETP || cmd == TIOCSETN)
		cnparams(tp);
	return (error);
}

int	consintr = 1;
/*
 * Got a console transmission interrupt -
 * the console processor wants another character.
 */
cnxint(dev)
	dev_t dev;
{
	register struct tty *tp;
	register int unit;

	if (!consintr)
		return;
	cons.t_state &= ~TS_BUSY;
	consoftc.cs_timo = 0;
	if (cons.t_line)
		(*linesw[cons.t_line].l_start)(&cons);
	else
		cnstart(&cons);
}

cnstart(tp)
	register struct tty *tp;
{
	register c, s;

	s = spltty();
	if (tp->t_state & (TS_TIMEOUT|TS_BUSY|TS_TTSTOP))
		goto out;
	if (tp->t_outq.c_cc <= TTLOWAT(tp)) {
		if (tp->t_state&TS_ASLEEP) {
			tp->t_state &= ~TS_ASLEEP;
			wakeup((caddr_t)&tp->t_outq);
		}
		if (tp->t_wsel) {
			selwakeup(tp->t_wsel, tp->t_state & TS_WCOLL);
			tp->t_wsel = 0;
			tp->t_state &= ~TS_WCOLL;
		}
	}
	while (tp->t_outq.c_cc != 0) {
		c = getc(&tp->t_outq) & 0xff;
		if ((tp->t_flags & (RAW|LITOUT)) == 0) {
			if (c > 0177)
			{
				timeout(ttrstrt, (caddr_t)tp, (c&0177));
				tp->t_state |= TS_TIMEOUT;
				break;
			}
			c &= 0177;
		}
		sput(c,0xc);
	}
out:
	splx(s);
}

cnputc(c)
	char c;
{

	if (c == '\n')
		sput('\r',0xb);
	sput(c, 0xb);
}

/*
 * Print a character on console.
 */
cnputchar(c, tp)
	char c;
	register struct tty *tp;
{
	sput(c,0xa);
	if (c=='\n') getchar();
}


cngetc()
{
	register int c, s;

	s = spltty();		/* block cnrint while we poll */
	if (c == '\r')
		c = '\n';
	splx(s);
	return (c);
}

cngetchar(tp)
	register struct tty *tp;
{
	int c;

	c = sgetc(0);
	return (c&0xff);
}

/*
 * Set line parameters
 */
cnparams(tp)
	register struct tty *tp;
{
}

#ifdef KDB
/*
 * Turn input polling on/off (used by debugger).
 */
cnpoll(onoff)
	int onoff;
{
}
#endif

#define	CRT_TXTADDR	Crtat
#define	COL		80
#define	ROW		25
#define	CHR		2


u_short *Crtat = ((u_short *)0xb8000);
u_short	*crtat ;
u_char	color = 0xe ;
int row;

sput(c, ca) u_char c, ca; {

	if(inb(0x64)&2 == 0)sgetc(1);
	if (crtat == 0) {
		INTREN(IRQ1); /* XXX */
		crtat = CRT_TXTADDR; bzero (crtat,COL*ROW*CHR);
	}
	/*if (crtat >= (CRT_TXTADDR+COL*(ROW-1))) {
		crtat = CRT_TXTADDR+COL*(ROW-1); row = 0;
	}*/
		if (crtat >= CRT_TXTADDR+COL*(ROW-1)) { /* scroll */
			bcopy(CRT_TXTADDR+COL, CRT_TXTADDR,COL*(ROW-1)*CHR);
			bzero (CRT_TXTADDR+COL*(ROW-1),COL*CHR) ;
			crtat -= COL ;
		}
	switch(c) {

	case '\t':
		do {
			*crtat++ = (ca<<8)| ' '; row++ ;
		} while (row %8);
		break;

	case '\010':
		crtat--; row--;
		break;

	case '\r':
		/*bzero (crtat,(COL-row)*CHR) ;*/ crtat -= row ; row = 0;
		break;

	case '\n':
		if (crtat >= CRT_TXTADDR+COL*(ROW-1)) { /* scroll */
			bcopy(CRT_TXTADDR+COL, CRT_TXTADDR,COL*(ROW-1)*CHR);
			bzero (CRT_TXTADDR+COL*(ROW-1),COL*CHR) ;
			crtat -= COL ;
		}
		crtat += COL ;
		break;

	default:
		*crtat++ = (ca<<8)| c; row++ ;
		if (row >= COL) {
		/*bzero (crtat,(COL-row)*CHR) ;*/ crtat -= row ; row = 0;
		if (crtat >= CRT_TXTADDR+COL*(ROW-1)) { /* scroll */
			bcopy(CRT_TXTADDR+COL, CRT_TXTADDR,COL*(ROW-1)*CHR);
			bzero (CRT_TXTADDR+COL*(ROW-1),COL*CHR) ;
			crtat -= COL ;
		}
		crtat += COL ;
		}
		break ;
	}
}
#define	L		0x0001	/* locking function */
#define	SHF		0x0002	/* keyboard shift */
#define	ALT		0x0004	/* alternate shift -- alternate chars */
#define	NUM		0x0008	/* numeric shift  cursors vs. numeric */
#define	CTL		0x0010	/* control shift  -- allows ctl function */
#define	CPS		0x0020	/* caps shift -- swaps case of letter */
#define	ASCII		0x0040	/* ascii code for this key */
#define	STP		0x0080	/* stop output */
#define	FUNC		0x0100	/* function key */
#define	SYSREQ		0x0200	/* sys req key */

unsigned	__debug ;
u_short action[] = {
0,     ASCII, ASCII, ASCII, ASCII, ASCII, ASCII, ASCII,		/* scan  0- 7 */
ASCII, ASCII, ASCII, ASCII, ASCII, ASCII, ASCII, ASCII,		/* scan  8-15 */
ASCII, ASCII, ASCII, ASCII, ASCII, ASCII, ASCII, ASCII,		/* scan 16-23 */
ASCII, ASCII, ASCII, ASCII, ASCII,   CTL, ASCII, ASCII,		/* scan 24-31 */
ASCII, ASCII, ASCII, ASCII, ASCII, ASCII, ASCII, ASCII,		/* scan 32-39 */
ASCII, ASCII, SHF  , ASCII, ASCII, ASCII, ASCII, ASCII,		/* scan 40-47 */
ASCII, ASCII, ASCII, ASCII, ASCII, ASCII,  SHF,  ASCII,		/* scan 48-55 */
  ALT, ASCII, CPS|L, FUNC , FUNC , FUNC , FUNC , FUNC ,		/* scan 56-63 */
FUNC , FUNC , FUNC , FUNC , FUNC , NUM|L, STP|L, ASCII,		/* scan 64-71 */
ASCII, ASCII, ASCII, ASCII, ASCII, ASCII, ASCII, ASCII,		/* scan 72-79 */
ASCII, ASCII, ASCII, ASCII,     0,     0,     0,     0,		/* scan 80-87 */
0,0,0,0,0,0,0,0,
0,0,0,0,0,0,0,0, 0,0,0,0,0,0,0,0,
0,0,0,0,0,0,0,0, 0,0,0,0,0,0,0,0,	} ;

u_char unshift[] = {	/* no shift */
0,     033  , '1'  , '2'  , '3'  , '4'  , '5'  , '6'  ,		/* scan  0- 7 */
'7'  , '8'  , '9'  , '0'  , '-'  , '='  , 0177 ,'\t'  ,		/* scan  8-15 */

'q'  , 'w'  , 'e'  , 'r'  , 't'  , 'y'  , 'u'  , 'i'  ,		/* scan 16-23 */
'o'  , 'p'  , '['  , ']'  , '\r' , CTL  , 'a'  , 's'  ,		/* scan 24-31 */

'd'  , 'f'  , 'g'  , 'h'  , 'j'  , 'k'  , 'l'  , ';'  ,		/* scan 32-39 */
'\'' , '`'  , SHF  , '\\' , 'z'  , 'x'  , 'c'  , 'v'  ,		/* scan 40-47 */

'b'  , 'n'  , 'm'  , ','  , '.'  , '/'  , SHF  ,   '*',		/* scan 48-55 */
ALT  , ' '  , CPS|L,     1,     2,    3 ,     4,     5,		/* scan 56-63 */

    6,     7,     8,     9,    10, NUM|L, STP|L,   '7',		/* scan 64-71 */
  '8',   '9',   '-',   '4',   '5',   '6',   '+',   '1',		/* scan 72-79 */

  '2',   '3',   '0',   '.',     0,     0,     0,     0,		/* scan 80-87 */
0,0,0,0,0,0,0,0,
0,0,0,0,0,0,0,0, 0,0,0,0,0,0,0,0,
0,0,0,0,0,0,0,0, 0,0,0,0,0,0,0,0,	} ;

u_char shift[] = {	/* shift shift */
0,     033  , '!'  , '@'  , '#'  , '$'  , '%'  , '^'  ,		/* scan  0- 7 */
'&'  , '*'  , '('  , ')'  , '_'  , '+'  , 0177 ,'\t'  ,		/* scan  8-15 */
'Q'  , 'W'  , 'E'  , 'R'  , 'T'  , 'Y'  , 'U'  , 'I'  ,		/* scan 16-23 */
'O'  , 'P'  , '{'  , '}'  , '\r' , CTL  , 'A'  , 'S'  ,		/* scan 24-31 */
'D'  , 'F'  , 'G'  , 'H'  , 'J'  , 'K'  , 'L'  , ':'  ,		/* scan 32-39 */
'"'  , '~'  , SHF  , '|'  , 'Z'  , 'X'  , 'C'  , 'V'  ,		/* scan 40-47 */
'B'  , 'N'  , 'M'  , '<'  , '>'  , '?'  , SHF  ,   '*',		/* scan 48-55 */
ALT  , ' '  , CPS|L,     0,     0, ' '  ,     0,     0,		/* scan 56-63 */
    0,     0,     0,     0,     0, NUM|L, STP|L,   '7',		/* scan 64-71 */
  '8',   '9',   '-',   '4',   '5',   '6',   '+',   '1',		/* scan 72-79 */
  '2',   '3',   '0',   '.',     0,     0,     0,     0,		/* scan 80-87 */
0,0,0,0,0,0,0,0,
0,0,0,0,0,0,0,0, 0,0,0,0,0,0,0,0,
0,0,0,0,0,0,0,0, 0,0,0,0,0,0,0,0,	} ;

u_char ctl[] = {	/* CTL shift */
0,     033  , '!'  , 000  , '#'  , '$'  , '%'  , 036  ,		/* scan  0- 7 */
'&'  , '*'  , '('  , ')'  , 037  , '+'  , 034  ,'\177',		/* scan  8-15 */
021  , 027  , 005  , 022  , 024  , 031  , 025  , 011  ,		/* scan 16-23 */
017  , 020  , 033  , 035  , '\r' , CTL  , 001  , 023  ,		/* scan 24-31 */
004  , 006  , 007  , 010  , 012  , 013  , 014  , ';'  ,		/* scan 32-39 */
'\'' , '`'  , SHF  , 034  , 032  , 030  , 003  , 026  ,		/* scan 40-47 */
002  , 016  , 015  , '<'  , '>'  , '?'  , SHF  ,   '*',		/* scan 48-55 */
ALT  , ' '  , CPS|L,     0,     0, ' '  ,     0,     0,		/* scan 56-63 */
CPS|L,     0,     0,     0,     0,     0,     0,     0,		/* scan 64-71 */
    0,     0,     0,     0,     0,     0,     0,     0,		/* scan 72-79 */
    0,     0,     0,     0,     0,     0,     0,     0,		/* scan 80-87 */
    0,     0,   033, '7'  , '4'  , '1'  ,     0, NUM|L,		/* scan 88-95 */
'8'  , '5'  , '2'  ,     0, STP|L, '9'  , '6'  , '3'  ,		/*scan  96-103*/
'.'  ,     0, '*'  , '-'  , '+'  ,     0,     0,     0,		/*scan 104-111*/
0,0,0,0,0,0,0,0, 0,0,0,0,0,0,0,0,	} ;

#ifdef notdef
struct key {
	u_short action;		/* how this key functions */
	char	ascii[8];	/* ascii result character indexed by shifts */
};
#endif

u_char shfts, ctls, alts, caps, num, stp;

#define	KBSTATP	0x64	/* kbd status port */
#define		KBS_INP_BUF_FUL	0x02	/* kbd char ready */
#define	KBDATAP	0x60	/* kbd data port */
#define	KBSTATUSPORT	0x61	/* kbd status */

u_char odt;

int sgetc(on) {
	u_char dt, brk;
	u_short act;
	
loop:
	do {
		while (inb(0x64)&2) ;
		dt = inb(0x60);
	} while (dt == odt);
	odt = dt;
if(dt == 0x54) _exit();
	brk = dt & 0x80 ; dt = dt & 0x7f ;

	act = action[dt];
	if (act&SHF) {
		if(brk)	shfts = 0; else shfts = 1;
	}
	if (act&ALT) {
		if(brk)	alts = 0; else alts = 1;
	}
	if (act&NUM) {
		if (act&L) {
			if(!brk) num ^= 1;
		} else if(brk)	num = 0; else num = 1;
	}
	if (act&CTL) {
		if(brk)	ctls = 0; else ctls = 1;
	}
	if (act&CPS) {
		if (act&L) {
			if(!brk) caps ^= 1;
		} else if(brk)	caps = 0; else caps = 1;
	}
	if (act&STP) {
		if (act&L) {
			if(!brk) stp ^= 1;
		} else if(brk)	stp = 0; else stp = 1;
	}
	if ((act&FUNC) &&  brk) {
		unsigned key = unshift[dt] ;
		if(__debug & (1<<key))
			__debug &= ~(1<<key) ;
		else
			__debug |= (1<<key) ;
	}
	if(stp) goto loop;
	if ((act&ASCII) && !brk) {
		u_char chr;

		if (shfts){
			 chr = shift[dt] ; } else {
		if (ctls) {
			chr = ctl[dt] ; } else {
		chr = unshift[dt] ; } }
		if (caps && (chr >= 'a' && chr <= 'z')) {
			chr -= 'a' - 'A' ;
		}
		return(chr);
	}
	if(on) return (0x100); else goto loop;
}

pg(p,q,r,s,t,u,v,w,x,y,z) char *p; {
	printf(p,q,r,s,t,u,v,w,x,y,z);
	printf("\n");
	return(getchar());
}

/* special characters */
#define bs	8
#define lf	10	
#define cr	13	
#define cntlc	3	
#define del	0177	
#define cntld	4

getchar()
{
	register char thechar;
	register delay;
	int x;

	consoftc.cs_flags |= CSF_POLLING;
	x=splhigh();
sput('>',0xe);
	while (1) {
		thechar = (char) sgetc(0);
	consoftc.cs_flags &= ~CSF_POLLING;
	splx(x);
		switch (thechar) {
		    default: if (thechar >= ' ')
			     	sput(thechar,0xe);
			     return(thechar);
		    case cr:
		    case lf: sput(cr,0xe);
			     sput(lf,0xe);
			     return(lf);
		    case bs:
		    case del:
			     sput(bs,0xe);
			     sput(' ',0xe);
			     sput(bs,0xe);
			     return(thechar);
		    case cntlc:
			     sput('^',0xe) ; sput('C',0xe) ; sput('\r',0xe) ; sput('\n',0xe) ;
			     _exit(-2) ;
		    case cntld:
			     sput('^',0xe) ; sput('D',0xe) ; sput('\r',0xe) ; sput('\n',0xe) ;
			     return(0);
		}
	}
}
