/*
 *  Line printer driver
 */

#include "../h/param.h"
#include "../h/dir.h"
#include "../h/user.h"
#include "../h/tty.h"
#include "../h/uba.h"

struct device *lp_addr[] = { (struct device *)(UBA0_DEV+0177514) };

#define	LPPRI	PZERO+10
#define	LPLOWAT	50
#define	LPHIWAT	100
#define LPMAX 2

struct device {
	short	lpcsr, lpbuf;
};
int lp_cnt = 1;

struct lp {
	struct	clist l_outq;
	char	flag, ind;
	short	ccc, mcc, mlc;
	short	line, col;
} lp_dt[LPMAX];

#define	OPEN	010
#define	CAP	020
#define	NOCR	040
#define	ASLP	0100

#define	FORM	014

lpopen(dev, flag)
{
	register unit;
	register struct lp *lp;

	unit = dev&07;
	if (unit >= lp_cnt || unit >= LPMAX ||
	 (lp = &lp_dt[unit])->flag || lp_addr[unit]->lpcsr <0 ) {
		u.u_error = EIO;
		return;
	}
	lp->flag = (dev&077)|OPEN;
	lp->ind = 4;
	lp->col = 80;
	lp->line = 66;
	lp_addr[unit]->lpcsr |= IENABLE;
	lpoutput(unit, FORM);
}

lpclose(dev)
{
	register unit;

	unit = dev&07;
	lpoutput(unit, FORM);
	lp_dt[unit].flag = 0;
}

lpwrite(dev)
{
	register c;

	while ((c=cpass())>=0)
		lpoutput(dev&07, c);
}

lpoutput(dev, c)
register dev, c;
{
	register struct lp *lp;

	lp = &lp_dt[dev];
	if(lp->flag&CAP) {
		if(c>='a' && c<='z')
			c += 'A'-'a'; else
		switch(c) {
		case '{':
			c = '(';
			goto esc;
		case '}':
			c = ')';
			goto esc;
		case '`':
			c = '\'';
			goto esc;
		case '|':
			c = '!';
			goto esc;
		case '~':
			c = '^';
		esc:
			lpoutput(dev, c);
			lp->ccc--;
			c = '-';
		}
	}
	switch(c) {
	case '\t':
		lp->ccc = ((lp->ccc+8-lp->ind) & ~7) + lp->ind;
		return;
	case '\n':
		lp->mlc++;
		if(lp->mlc >= lp->line )
			c = FORM;
	case FORM:
		lp->mcc = 0;
		if (lp->mlc) {
			lpputc(dev, c);
			if(c == FORM)
				lp->mlc = 0;
		}
	case '\r':
		lp->ccc = lp->ind;
		return;
	case 010:
		if(lp->ccc > lp->ind)
			lp->ccc--;
		return;
	case ' ':
		lp->ccc++;
		return;
	default:
		if(lp->ccc < lp->mcc) {
			if (lp->flag&NOCR) {
				lp->ccc++;
				return;
			}
			lpputc(dev, '\r');
			lp->mcc = 0;
		}
		if(lp->ccc < lp->col) {
			while(lp->ccc > lp->mcc) {
				lpputc(dev, ' ');
				lp->mcc++;
			}
			lpputc(dev, c);
			lp->mcc++;
		}
		lp->ccc++;
	}
}

lpputc(dev, c)
register dev, c;
{
	register struct lp *lp;

	lp = &lp_dt[dev];
	spl4();
	while (lp->l_outq.c_cc > LPHIWAT) {
		lp->flag |= ASLP;
		sleep(lp, LPPRI);
	}
	putc(c, &lp->l_outq);
	lpintr(dev);
	spl0();
}

lpintr(dev)
register dev;
{
	register struct lp *lp;
	register c;

	lp = &lp_dt[dev];
	while (lp_addr[dev]->lpcsr&DONE && (c = getc(&lp->l_outq)) >= 0)
		lp_addr[dev]->lpbuf = c;
	if (lp->l_outq.c_cc <= LPLOWAT && lp->flag&ASLP) {
		lp->flag &= ~ASLP;
		wakeup(lp);
	}
}

lpioctl(dev, cmd, addr, flag)
register caddr_t addr;
{
register int m;
struct lp *lp;
struct {char lsg_flag, lsg_ind; short lsg_line, lsg_col;} lpios;

	lp = &lp_dt[dev];
	switch (cmd) {

	case ('v'<<8)+0:
		lpios.lsg_flag = lp->flag;
		lpios.lsg_ind = lp->ind;
		lpios.lsg_line = lp->line;
		lpios.lsg_col = lp->col;
		copyout(&lpios, addr, sizeof lpios);
		return;

	case ('v'<<8)+1:
		m = copyin(addr, &lpios, sizeof lpios);
		if (m == -1) {
			u.u_error = EFAULT;
			return;
		}
		lp->flag = lpios.lsg_flag;
		lp->ind = lpios.lsg_ind;
		lp->line = lpios.lsg_line;
		lp->col = lpios.lsg_col;
		return;

	default:
		u.u_error = ENOTTY;
		return;
	}
}
