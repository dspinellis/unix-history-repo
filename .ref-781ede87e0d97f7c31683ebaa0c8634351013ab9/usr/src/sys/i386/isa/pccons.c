/*-
 * Copyright (c) 1990 The Regents of the University of California.
 * All rights reserved.
 *
 * This code is derived from software contributed to Berkeley by
 * William Jolitz.
 *
 * Added support for ibmpc term type and improved keyboard support. -Don Ahn
 *
 * %sccs.include.386.c%
 *
 *	@(#)pccons.c	5.4 (Berkeley) %G%
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
#include "machine/isa/isa_device.h"
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

int cnprobe(), cnattach();

struct	isa_driver cndriver = {
	cnprobe, cnattach, "cn",
};

#define	COL		80
#define	ROW		25
#define	CHR		2
#define MONO_BASE	0x3B4
#define MONO_BUF	0xB0000
#define CGA_BASE	0x3D4
#define CGA_BUF		0xB8000
#define IOPHYSMEM	0xA0000

u_char	color = 0xe ;
static unsigned int addr_6845 = MONO_BASE;
u_short *Crtat = (u_short *)MONO_BUF;

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

cnprobe(dev)
struct isa_device *dev;
{
	u_char c;
	int again = 0;

	/* Enable interrupts and keyboard controller */
	while (inb(0x64)&2); outb(0x64,0x60);
	while (inb(0x64)&2); outb(0x60,0x4D);

	/* Start keyboard stuff RESET */
	while (inb(0x64)&2);	/* wait input ready */
	outb(0x60,0xFF);	/* RESET */
	while((c=inb(0x60))!=0xFA) {
		if ((c == 0xFE) ||  (c == 0xFF)) {
			if(!again)printf("KEYBOARD disconnected: RECONNECT \n");
			while (inb(0x64)&2);	/* wait input ready */
			outb(0x60,0xFF);	/* RESET */
			again = 1;
		}
	}
	/* pick up keyboard reset return code */
	while((c=inb(0x60))!=0xAA)nulldev();
	return 1;
}

cnattach(dev)
struct isa_device *dev;
{
	u_short *cp = Crtat + (CGA_BUF-MONO_BUF)/CHR;
	u_short was;

	/* Crtat initialized to point to MONO buffer   */
	/* if not present change to CGA_BUF offset     */
	/* ONLY ADD the difference since locore.s adds */
	/* in the remapped offset at the right time    */

	was = *Crtat;
	*Crtat = (u_short) 0xA55A;
	if (*Crtat != 0xA55A)
		printf("<mono>");
	else	printf("<color>");
	*Crtat = was;
}

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
		splnone();
	}
	return ((*linesw[tp->t_line].l_open)(dev, tp));
}


cnclose(dev)
	dev_t dev;
{
	(*linesw[cons.t_line].l_close)(&cons);
	ttyclose(&cons);
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
cnrint(dev, irq, cpl)
	dev_t dev;
{
	int c;

	c = sgetc(1);
	if (c&0x100) return;
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
		splx(s);
		sput(c,0x7);
		s = spltty();
	}
out:
	splx(s);
}

cnputc(c)
	char c;
{
	if (c == '\n')
		sput('\r',0x3);
	sput(c, 0x3);
}

/*
 * Print a character on console.
 */
cnputchar(c, tp)
	char c;
	register struct tty *tp;
{
	sput(c,0x2);
	if (c=='\n') getchar();
}


cngetc()
{
	register int c, s;

	s = spltty();		/* block cnrint while we poll */
	c = sgetc(0);
	if (c == '\r') c = '\n';
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

extern int hz;

sysbeepstop()
{
	/* disable counter 2 */
	outb(0x61,inb(0x61)&0xFC);
}

sysbeep()
{
	/* enable counter 2 */
	outb(0x61,inb(0x61)|3);
	/* set command for counter 2, 2 byte write */
	outb(0x43,0xB6);
	/* send 0x637 for 750 HZ */
	outb(0x42,0x37);
	outb(0x42,0x06);
	timeout(sysbeepstop,0,hz/4);
}

/* cursor() sets an offset (0-1999) into the 80x25 text area    */

static u_short *crtat = 0;
char bg_at = 0x0f;
char so_at = 0x70;

cursor()
{ 	int pos = crtat - Crtat;

	outb(addr_6845,14);
	outb(addr_6845+1,pos >> 8);
	outb(addr_6845,15);
	outb(addr_6845+1,pos&0xff);
}

/* sput has support for emulation of the 'ibmpc' termcap entry. */
/* This is a bare-bones implementation of a bare-bones entry    */
/* One modification: Change li#24 to li#25 to reflect 25 lines  */

sput(c, ca)
u_char c, ca;
{

	static int esc,ebrac,eparm,cx,cy,row,so;

	if (crtat == 0) {
		u_short *cp = Crtat + (CGA_BUF-MONO_BUF)/CHR, was;
		unsigned cursorat;

		/* Crtat initialized to point to MONO buffer   */
		/* if not present change to CGA_BUF offset     */
		/* ONLY ADD the difference since locore.s adds */
		/* in the remapped offset at the right time    */

		was = *cp;
		*cp = (u_short) 0xA55A;
		if (*cp != 0xA55A) {
			addr_6845 = MONO_BASE;
		} else {
			*cp = was;
			addr_6845 = CGA_BASE;
			Crtat = Crtat + (CGA_BUF-MONO_BUF)/CHR;
		}
		/* Extract cursor location */
		outb(addr_6845,14);
		cursorat = inb(addr_6845+1)<<8 ;
		outb(addr_6845,15);
		cursorat |= inb(addr_6845+1);

		crtat = Crtat + cursorat;
		fillw((bg_at<<8)|' ', crtat, COL*ROW-cursorat);
	}
	switch(c) {
	case 0x1B:
		esc = 1; ebrac = 0; eparm = 0;
		break;

	case '\t':
		do {
			*crtat++ = (ca<<8)| ' '; row++ ;
		} while (row %8);
		break;

	case '\010':
		crtat--; row--;
		if (row < 0) row += COL;  /* non-destructive backspace */
		break;

	case '\r':
		crtat -= row ; row = 0;
		break;

	case '\n':
		crtat += COL ;
		break;

	default:
		if (esc) {
			if (ebrac) {
				switch(c) {
				case 'm': /* no support for standout */
					if (!cx) so = 0;
					else so = 1;
					esc = 0; ebrac = 0; eparm = 0;
					break;
				case 'A': /* back one row */
					crtat -= COL;
					esc = 0; ebrac = 0; eparm = 0;
					break;
				case 'B': /* down one row */
					crtat += COL;
					esc = 0; ebrac = 0; eparm = 0;
					break;
				case 'C': /* right cursor */
					crtat++; row++;
					esc = 0; ebrac = 0; eparm = 0;
					break;
				case 'J': /* Clear to end of display */
					fillw((bg_at<<8)+' ', crtat,
						Crtat+COL*ROW-crtat);
					esc = 0; ebrac = 0; eparm = 0;
					break;
				case 'K': /* Clear to EOL */
					fillw((bg_at<<8)+' ', crtat, COL-row);
					esc = 0; ebrac = 0; eparm = 0;
					break;
				case 'H': /* Cursor move */
					if ((!cx)||(!cy)) {
						crtat = Crtat;
						row = 0;
					} else {
						crtat = Crtat+(cx-1)*COL+cy-1;
						row = cy-1;
					}
					esc = 0; ebrac = 0; eparm = 0;
					break;
				case ';': /* Switch params in cursor def */
					eparm = 1;
					return;
				default: /* Only numbers valid here */
					if ((c >= '0')&&(c <= '9')) {
						if (eparm) {
							cy *= 10;
							cy += c - '0';
						} else {
							cx *= 10;
							cx += c - '0';
						}
					} else {
						esc = 0; ebrac = 0; eparm = 0;
					}
					return;
				}
				break;
			} else if (c == 'c') { /* Clear screen & home */
				fillw((bg_at<<8)+' ', Crtat,COL*ROW);
				crtat = Crtat; row = 0;
				esc = 0; ebrac = 0; eparm = 0;
			} else if (c == '[') { /* Start ESC [ sequence */
				ebrac = 1; cx = 0; cy = 0; eparm = 0;
			} else { /* Invalid, clear state */
				 esc = 0; ebrac = 0; eparm = 0;
			}
		} else {
			if (c == 7) {
				sysbeep();
			}
			/* Print only printables */
			else /*if (c >= ' ') */ {
				while(inb(0x3da)&1)nulldev();
				if (so) {
					*crtat++ = (so_at<<8)| c; row++ ;
				} else {
					*crtat++ = (ca<<8)| c; row++ ;
				}
				if (row >= COL) row = 0;
				break ;
			}
		}
	}
	if (crtat >= Crtat+COL*(ROW)) { /* scroll check */
		bcopy(Crtat+COL,Crtat,COL*(ROW-1)*CHR);
		fillw ((bg_at<<8)+' ', Crtat+COL*(ROW-1),COL) ;
		crtat -= COL ;
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
#define	SCROLL		0x0200	/* scroll lock key */

unsigned	__debug = 0;
u_short action[] = {
0,     ASCII, ASCII, ASCII, ASCII, ASCII, ASCII, ASCII,		/* scan  0- 7 */
ASCII, ASCII, ASCII, ASCII, ASCII, ASCII, ASCII, ASCII,		/* scan  8-15 */
ASCII, ASCII, ASCII, ASCII, ASCII, ASCII, ASCII, ASCII,		/* scan 16-23 */
ASCII, ASCII, ASCII, ASCII, ASCII,   CTL, ASCII, ASCII,		/* scan 24-31 */
ASCII, ASCII, ASCII, ASCII, ASCII, ASCII, ASCII, ASCII,		/* scan 32-39 */
ASCII, ASCII, SHF  , ASCII, ASCII, ASCII, ASCII, ASCII,		/* scan 40-47 */
ASCII, ASCII, ASCII, ASCII, ASCII, ASCII,  SHF,  ASCII,		/* scan 48-55 */
  ALT, ASCII, CPS  , FUNC , FUNC , FUNC , FUNC , FUNC ,		/* scan 56-63 */
FUNC , FUNC , FUNC , FUNC , FUNC , NUM, STP, ASCII,		/* scan 64-71 */
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
ALT  , ' '  , CPS,     1,     2,    3 ,     4,     5,		/* scan 56-63 */

    6,     7,     8,     9,    10, NUM, STP,   '7',		/* scan 64-71 */
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
ALT  , ' '  , CPS,     0,     0, ' '  ,     0,     0,		/* scan 56-63 */
    0,     0,     0,     0,     0, NUM, STP,   '7',		/* scan 64-71 */
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
ALT  , ' '  , CPS,     0,     0, ' '  ,     0,     0,		/* scan 56-63 */
CPS,     0,     0,     0,     0,     0,     0,     0,		/* scan 64-71 */
    0,     0,     0,     0,     0,     0,     0,     0,		/* scan 72-79 */
    0,     0,     0,     0,     0,     0,     0,     0,		/* scan 80-87 */
    0,     0,   033, '7'  , '4'  , '1'  ,     0, NUM,		/* scan 88-95 */
'8'  , '5'  , '2'  ,     0, STP, '9'  , '6'  , '3'  ,		/*scan  96-103*/
'.'  ,     0, '*'  , '-'  , '+'  ,     0,     0,     0,		/*scan 104-111*/
0,0,0,0,0,0,0,0, 0,0,0,0,0,0,0,0,	} ;

#ifdef notdef
struct key {
	u_short action;		/* how this key functions */
	char	ascii[8];	/* ascii result character indexed by shifts */
};
#endif

u_char shfts, ctls, alts, caps, num, stp, scroll;

#define	KBSTAT		0x64	/* kbd status port */
#define	KBS_INP_BUF_FUL	0x02	/* kbd char ready */
#define	KBDATA		0x60	/* kbd data port */
#define	KBSTATUSPORT	0x61	/* kbd status */

update_led()
{
	while (inb(0x64)&2);	/* wait input ready */
	outb(0x60,0xED);	/* LED Command */
	while (inb(0x64)&2);	/* wait input ready */
	outb(0x60,scroll | 2*num | 4*caps);
}

reset_cpu() {
	while(1) {
		while (inb(0x64)&2);	/* wait input ready */
		outb(0x64,0xFE);	/* Reset Command */
		DELAY(4000000);
		while (inb(0x64)&2);	/* wait input ready */
		outb(0x64,0xFF);	/* Keyboard Reset Command */
	}
	/* NOTREACHED */
}

/*
sgetc(noblock) : get a character from the keyboard. If noblock = 0 wait until
a key is gotten.  Otherwise return a 0x100 (256).
*/
int sgetc(noblock)
{
	u_char dt; unsigned key;
loop:
	/* First see if there is something in the keyboard port */
	if (inb(KBSTAT)&1) dt = inb(KBDATA);
	else { if (noblock) return (0x100); else goto loop; }

	/* Check for cntl-alt-del */
	if ((dt == 83)&&ctls&&alts) _exit();

	/* Check for make/break */
	if (dt & 0x80) {
		/* break */
		dt = dt & 0x7f ;
		switch (action[dt]) {
		case SHF: shfts = 0; break;
		case ALT: alts = 0; break;
		case CTL: ctls = 0; break;
		case FUNC:
			/* Toggle debug flags */
			key = unshift[dt];
			if(__debug & (1<<key)) __debug &= ~(1<<key) ;
			else __debug |= (1<<key) ;
			break;
		}
	} else {
		/* make */
		dt = dt & 0x7f ;
		switch (action[dt]) {
		/* LOCKING KEYS */
		case NUM: num ^= 1; update_led(); break;
		case CPS: caps ^= 1; update_led(); break;
		case SCROLL: scroll ^= 1; update_led(); break;
		case STP: stp ^= 1; if(stp) goto loop; break;

		/* NON-LOCKING KEYS */
		case SHF: shfts = 1; break;
		case ALT: alts = 1; break;
		case CTL: ctls = 1; break;
		case ASCII:
			if (shfts) dt = shift[dt];
			else if (ctls) dt = ctl[dt];
			else dt = unshift[dt];
			if (caps && (dt >= 'a' && dt <= 'z')) dt -= 'a' - 'A';
			return(dt);
		}
	}
	if (noblock) return (0x100); else goto loop;
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
	sput('>',0x6);
	/*while (1) {*/
		thechar = (char) sgetc(0);
		consoftc.cs_flags &= ~CSF_POLLING;
		splx(x);
		switch (thechar) {
		    default: if (thechar >= ' ')
			     	sput(thechar,0x6);
			     return(thechar);
		    case cr:
		    case lf: sput(cr,0x6);
			     sput(lf,0x6);
			     return(lf);
		    case bs:
		    case del:
			     sput(bs,0x6);
			     sput(' ',0x6);
			     sput(bs,0x6);
			     return(thechar);
		    /*case cntlc:
			     sput('^',0xe) ; sput('C',0xe) ; sput('\r',0xe) ; sput('\n',0xe) ;
			     _exit(-2) ; */
		    case cntld:
			     sput('^',0x6) ; sput('D',0x6) ; sput('\r',0x6) ; sput('\n',0x6) ;
			     return(0);
		}
	/*}*/
}
