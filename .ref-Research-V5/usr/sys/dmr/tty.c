#
/*
 *	Copyright 1973 Bell Telephone Laboratories Inc
 */

/*
 * general TTY subroutines
 */
#include "../param.h"
#include "../systm.h"
#include "../user.h"
#include "../tty.h"
#include "../proc.h"
#include "../inode.h"
#include "../file.h"
#include "../reg.h"
#include "../conf.h"

struct { int int; };
#define	PS	0177776

char	maptab[]
{
	000,000,000,000,004,000,000,000,
	000,000,000,000,000,000,000,000,
	000,000,000,000,000,000,000,000,
	000,000,000,000,000,000,000,000,
	000,'|',000,'#',000,000,000,'`',
	'{','}',000,000,000,000,000,000,
	000,000,000,000,000,000,000,000,
	000,000,000,000,000,000,000,000,
	'@',000,000,000,000,000,000,000,
	000,000,000,000,000,000,000,000,
	000,000,000,000,000,000,000,000,
	000,000,000,000,000,000,'~',000,
	000,'A','B','C','D','E','F','G',
	'H','I','J','K','L','M','N','O',
	'P','Q','R','S','T','U','V','W',
	'X','Y','Z',000,000,000,000,000,
};

struct cblock {
	struct cblock *c_next;
	char info[6];
};

struct cblock cfree[NCLIST];
struct cblock *cfreelist;

struct {
	int ttrcsr;
	int ttrbuf;
	int tttcsr;
	int tttbuf;
};

gtty()
{
	int v[3];
	register *up, *vp;

	vp = v;
	sgtty(vp);
	if (u.u_error)
		return;
	up = u.u_arg[0];
	suword(up, *vp++);
	suword(++up, *vp++);
	suword(++up, *vp++);
}

stty()
{
	register int *up;

	up = u.u_arg[0];
	u.u_arg[0] = fuword(up);
	u.u_arg[1] = fuword(++up);
	u.u_arg[2] = fuword(++up);
	sgtty(0);
}

sgtty(v)
{
	register struct file *fp;
	register struct inode *ip;

	if ((fp = getf(u.u_ar0[R0])) == NULL)
		return;
	ip = fp->f_inode;
	if ((ip->i_mode&IFMT) != IFCHR) {
		u.u_error = ENOTTY;
		return;
	}
	(*cdevsw[ip->i_addr[0].d_major].d_sgtty)(ip->i_addr[0], v);
}

wflushtty(atp)
struct tty *atp;
{
	register struct tty *tp;

	tp = atp;
	spl5();
	while (tp->t_outq.c_cc)
		sleep(&tp->t_outq, TTOPRI);
	flushtty(tp);
	spl0();
}

cinit()
{
	register int ccp;
	register struct cblock *cp;
	register struct cdevsw *cdp;

	ccp = cfree;
	for (cp=(ccp+07)&~07; cp <= &cfree[NCLIST-1]; cp++) {
		cp->c_next = cfreelist;
		cfreelist = cp;
	}
	ccp = 0;
	for(cdp = cdevsw; cdp->d_open; cdp++)
		ccp++;
	nchrdev = ccp;
}

/*
 * flush all TTY queues
 */
flushtty(atp)
struct tty *atp;
{
	register struct tty *tp;
	register int sps;

	tp = atp;
	while (getc(&tp->t_canq) >= 0);
	while (getc(&tp->t_outq) >= 0);
	wakeup(&tp->t_rawq);
	wakeup(&tp->t_outq);
	sps = PS->int;
	spl5();
	while (getc(&tp->t_rawq) >= 0);
	tp->t_delct = 0;
	PS->int = sps;
}

/*
 * transfer raw list to canonical list,
 * doing code conversion.
 */
canon(atp)
struct tty *atp;
{
	register char *bp;
	char *bp1;
	register struct tty *tp;
	register int c;

	tp = atp;
	spl5();
	while (tp->t_delct==0) {
		if ((tp->t_state&CARR_ON)==0)
			return(0);
		sleep(&tp->t_rawq, TTIPRI);
	}
	spl0();
loop:
	bp = &canonb[2];
	while ((c=getc(&tp->t_rawq)) >= 0) {
		if (c==0377) {
			tp->t_delct--;
			break;
		}
		if ((tp->t_flags&RAW)==0) {
			if (bp[-1]!='\\') {
				if (c==tp->t_erase) {
					if (bp > &canonb[2])
						bp--;
					continue;
				}
				if (c==tp->t_kill)
					goto loop;
				if (c==CEOT)
					continue;
			} else
			if (maptab[c] && (maptab[c]==c || (tp->t_flags&LCASE))) {
				if (bp[-2] != '\\')
					c = maptab[c];
				bp--;
			}
		}
		*bp++ = c;
		if (bp>=canonb+CANBSIZ)
			break;
	}
	bp1 = bp;
	bp = &canonb[2];
	c = &tp->t_canq;
	while (bp<bp1)
		putc(*bp++, c);
	return(1);
}

/*
 * place a character on raw TTY input queue, putting in delimiters
 * and waking up top half as needed.
 * also echo if required.
 */
ttyinput(ac, atp)
struct tty *atp;
{
	register int t_flags, c;
	register struct tty *tp;

	tp = atp;
	c = ac;
	t_flags = tp->t_flags;
	if ((c =& 0177) == '\r' && t_flags&CRMOD)
		c = '\n';
	if ((t_flags&RAW)==0 && (c==CQUIT || c==CINTR)) {
		signal(tp, c==CINTR? SIGINT:SIGQIT);
		flushtty(tp);
		return;
	}
	if (tp->t_rawq.c_cc>=TTYHOG) {
		flushtty(tp);
		return;
	}
	if (t_flags&LCASE && c>='A' && c<='Z')
		c =+ 'a'-'A';
	putc(c, &tp->t_rawq);
	if (t_flags&RAW || c=='\n' || c==004) {
		wakeup(&tp->t_rawq);
		if (putc(0377, &tp->t_rawq)==0)
			tp->t_delct++;
	}
	if (t_flags&ECHO) {
		ttyoutput(c, tp);
		ttstart(tp);
	}
}

/*
 * put character on TTY output queue, adding delays,
 * expanding tabs, and handling the CR/NL bit.
 */
ttyoutput(ac, tp)
struct tty *tp;
{
	register int c;
	register struct tty *rtp;
	register char *colp;
	int ctype;

	rtp = tp;
	c = ac&0177;
	if (c==004 && (rtp->t_flags&RAW)==0)
		return;
	if (c=='\t' && rtp->t_flags&XTABS) {
		do
			ttyoutput(' ', rtp);
		while (rtp->t_col&07);
		return;
	}
	if (rtp->t_flags&LCASE) {
		colp = "({)}!|^~'`";
		while(*colp++)
			if(c == *colp++) {
				ttyoutput('\\', rtp);
				c = colp[-2];
				break;
			}
		if ('a'<=c && c<='z')
			c =+ 'A' - 'a';
	}
	if (c=='\n' && rtp->t_flags&CRMOD)
		ttyoutput('\r', rtp);
	if (putc(c, &rtp->t_outq))
		return;
	colp = &rtp->t_col;
	ctype = partab[c];
	c = 0;
	switch (ctype&077) {

	/* ordinary */
	case 0:
		(*colp)++;

	/* non-printing */
	case 1:
		break;

	/* backspace */
	case 2:
		if (*colp)
			(*colp)--;
		break;

	/* newline */
	case 3:
		if (*colp)
			c = max((*colp>>4) + 3, 6);
		*colp = 0;
		break;

	/* tab */
	case 4:
		if ((rtp->t_flags&NTDELAY)==0)
			c = 1 - (*colp | ~07);
		*colp =| 07;
		(*colp)++;
		break;

	/* vertical motion */
	case 5:
		c = 0177;
		break;

	/* carriage return */
	case 6:
		c = 6;
		*colp = 0;
	}
	if (c && (rtp->t_flags&NODELAY)==0)
		putc(c|0200, &rtp->t_outq);
}

ttrstrt(atp)
{
	register struct tty *tp;

	tp = atp;
	tp->t_state =& ~TIMEOUT;
	ttstart(tp);
}

ttstart(atp)
struct tty *atp;
{
	register int *addr, c;
	register struct tty *tp;
	struct { int (*func)(); };

	tp = atp;
	addr = tp->t_addr;
	if (tp->t_state&SSTART) {
		(*addr.func)(tp);
		return;
	}
	if ((addr->tttcsr&DONE)==0 || tp->t_state&TIMEOUT)
		return;
	if ((c=getc(&tp->t_outq)) >= 0) {
		if (c<=0177)
			addr->tttbuf = c | (partab[c]&0200);
		else {
			timeout(ttrstrt, tp, c&0177);
			tp->t_state =| TIMEOUT;
		}
	}
}

ttread(atp)
struct tty *atp;
{
	register struct tty *tp;

	tp = atp;
	if (tp->t_canq.c_cc || canon(tp))
		while (tp->t_canq.c_cc && passc(getc(&tp->t_canq))>=0);
}

ttwrite(atp)
struct tty *atp;
{
	register struct tty *tp;
	register int c;

	tp = atp;
	while ((c=cpass())>=0) {
		spl5();
		while (tp->t_outq.c_cc > TTHIWAT) {
			ttstart(tp);
			sleep(&tp->t_outq, TTOPRI);
		}
		spl0();
		ttyoutput(c, tp);
	}
	ttstart(tp);
}

ttystty(atp, av)
int *atp, *av;
{
	register  *tp, *v;

	tp = atp;
	if(v = av) {
		*v++ = tp->t_speeds;
		v->lobyte = tp->t_erase;
		v->hibyte = tp->t_kill;
		v[1] = tp->t_flags;
		return(1);
	}
	wflushtty(tp);
	v = u.u_arg;
	tp->t_speeds = *v++;
	tp->t_erase = v->lobyte;
	tp->t_kill = v->hibyte;
	tp->t_flags = v[1];
	return(0);
}
