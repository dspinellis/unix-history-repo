#
/*
 * general TTY subroutines
 */
#include "/sys/nsys/param.h"
#include "/sys/nsys/systm.h"
#include "/sys/nsys/user.h"
#include "/sys/nsys/tty.h"
#include "/sys/nsys/proc.h"
#include "/sys/nsys/inode.h"
#include "/sys/nsys/file.h"
#include "/sys/nsys/reg.h"
#include "/sys/nsys/conf.h"

struct { int int; };
#define	PS	0177776

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
	int v[3], *up;

	sgtty(v);
	if (u.u_error)
		return;
	up = u.u_arg[0];
	suword(up, v[0]);
	suword(++up, v[1]);
	suword(++up, v[2]);
}

stty()
{
	int *up;

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

wflushtty(tp)
struct tty *tp;
{

	spl5();
	while (tp->t_outq.c_cc)
		sleep(&tp->t_outq, TTOPRI);
	flushtty(tp);
	spl0();
}

cinit()
{
	int ccp;
	register struct cblock *cp;

	ccp = cfree;
	for (cp=(ccp+07)&~07; cp <= &cfree[NCLIST-1]; cp++) {
		cp->c_next = cfreelist;
		cfreelist = cp;
	}
}

/*
 * flush all TTY queues
 */
flushtty(tp)
struct tty *tp;
{
	int sps;

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
canon(tp)
struct tty *tp;
{
	register char *bp, *bp1;
	register int c;

	while (tp->t_delct==0)
		sleep(&tp->t_rawq, TTIPRI);
loop:
	bp = &canonb[1];
	while ((c=getc(&tp->t_rawq)) >= 0) {
		if ((tp->t_flags&RAW)==0) {
			if (bp[-1]!='\\') {
				if (c==CERASE) {
					if (bp > &canonb[1])
						bp--;
					continue;
				}
				if (c==CKILL)
					goto loop;
				if (c==CEOT)
					continue;
			} else if (c==CERASE||c==CKILL||c==CEOT)
				bp--;
		}
		if (c==0377) {
			tp->t_delct--;
			break;
		}
		*bp++ = c;
		if (bp>=canonb+CANBSIZ)
			break;
	}
	bp1 = &canonb[1];
	c = &tp->t_canq;
	while (bp1<bp)
		putc(*bp1++, c);
	return(1);
}

/*
 * place a character on raw TTY input queue, putting in delimiters
 * and waking up top half as needed.
 * also echo if required.
 */
ttyinput(c, tp)
struct tty *tp;
{
	int t_flags;

	t_flags = tp->t_flags;
	if ((c =& 0177) == '\r' && t_flags&CRMOD)
		c = '\n';
	if ((t_flags&RAW)==0 && (c==tp->t_quit || c==tp->t_intrup)) {
		signal(tp, c==tp->t_intrup? SIGINT:SIGQIT);
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
ttyoutput(c, tp)
struct tty *tp;
{
	int delay;
	register struct tty *rtp;
	register char *colp;

	rtp = tp;
	if ((c=&0177)==004 && (rtp->t_flags&RAW)==0)
		return;
	if (c=='\t' && rtp->t_flags&XTABS) {
		do
			ttyoutput(' ', rtp);
		while (rtp->t_col&07);
		return;
	}
	if (c=='\n' && rtp->t_flags&CRMOD)
		ttyoutput('\r', rtp);
	if (putc(c, &rtp->t_outq))
		return;
	delay = 0;
	colp = &rtp->t_col;
	if ((rtp->t_flags&NODELAY)==0) switch (partab[c]&077) {

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
			delay = max((*colp>>4) + 3, 6);
		*colp = 0;
		break;

	/* tab */
	case 4:
		if ((rtp->t_flags&NTDELAY)==0)
			delay = 1 - (*colp | ~07);
		*colp =| 07;
		(*colp)++;
		break;

	/* vertical motion */
	case 5:
		delay = 0177;
		break;

	/* carriage return */
	case 6:
		delay = 6;
		*colp = 0;
	}
	if (delay)
		putc(delay|0200, &rtp->t_outq);
}

ttrstrt(tp)
{
	tp->t_state =& ~TIMEOUT;
	ttstart(tp);
}

ttstart(tp)
struct tty *tp;
{
	int *addr;
	int c;

	addr = tp->t_addr;
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

ttread(tp)
struct tty *tp;
{
	if (tp->t_canq.c_cc || canon(tp))
		while (tp->t_canq.c_cc && passc(getc(&tp->t_canq))>=0);
}

ttwrite(tp)
struct tty *tp;
{
	int c;

	while (cpass(&c)>=0) {
		while (tp->t_outq.c_cc > TTHIWAT) {
			ttstart(tp);
			sleep(&tp->t_outq, TTOPRI);
		}
		ttyoutput(c, tp);
	}
	ttstart(tp);
}
