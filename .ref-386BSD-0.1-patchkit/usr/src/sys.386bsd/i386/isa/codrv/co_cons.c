/*-
 * Copyright (c) 1990 The Regents of the University of California.
 * Copyright (c) 1992 by Holger Veit
 * All rights reserved.
 *
 * This code is derived from software contributed to Berkeley by
 * William Jolitz and Don Ahn.
 * Significant parts are added and rewritten by Holger Veit.
 *
 * Redistribution and use in source and binary forms, with or without
 * modification, are permitted provided that the following conditions
 * are met:
 * 1. Redistributions of source code must retain the above copyright
 *    notice, this list of conditions and the following disclaimer.
 * 2. Redistributions in binary form must reproduce the above copyright
 *    notice, this list of conditions and the following disclaimer in the
 *    documentation and/or other materials provided with the distribution.
 * 3. All advertising materials mentioning features or use of this software
 *    must display the following acknowledgement:
 *	This product includes software developed by the University of
 *	California, Berkeley and its contributors.
 * 4. Neither the name of the University nor the names of its contributors
 *    may be used to endorse or promote products derived from this software
 *    without specific prior written permission.
 *
 * THIS SOFTWARE IS PROVIDED BY THE REGENTS AND CONTRIBUTORS ``AS IS'' AND
 * ANY EXPRESS OR IMPLIED WARRANTIES, INCLUDING, BUT NOT LIMITED TO, THE
 * IMPLIED WARRANTIES OF MERCHANTABILITY AND FITNESS FOR A PARTICULAR PURPOSE
 * ARE DISCLAIMED.  IN NO EVENT SHALL THE REGENTS OR CONTRIBUTORS BE LIABLE
 * FOR ANY DIRECT, INDIRECT, INCIDENTAL, SPECIAL, EXEMPLARY, OR CONSEQUENTIAL
 * DAMAGES (INCLUDING, BUT NOT LIMITED TO, PROCUREMENT OF SUBSTITUTE GOODS
 * OR SERVICES; LOSS OF USE, DATA, OR PROFITS; OR BUSINESS INTERRUPTION)
 * HOWEVER CAUSED AND ON ANY THEORY OF LIABILITY, WHETHER IN CONTRACT, STRICT
 * LIABILITY, OR TORT (INCLUDING NEGLIGENCE OR OTHERWISE) ARISING IN ANY WAY
 * OUT OF THE USE OF THIS SOFTWARE, EVEN IF ADVISED OF THE POSSIBILITY OF
 * SUCH DAMAGE.
 *
 *	@(#)co_cons.c	$Revision: 1.11 $ (Contributed to 386bsd) $Date: 93/01/23 23:14:33 $
 */
static char *rcsid = "$Header: /usr/src/sys.386bsd/i386/isa/codrv/RCS/co_cons.c,v 1.11 93/01/23 23:14:33 root Exp Locker: root $";

/*
 * History: See CO_HISTORY
 */

#include "co.h"
#include "pc.h"
#if NCO == 1
#if NPC == 0

/*
 * code to work keyboard & display for PC-style console
 */
#include "co_hdr.h"

unsigned	__debug = 0;	/* 0xffe, exported to elsewhere */
static unsigned	__color;

struct tty *dev2tty(dev_t dev)
{
	register n = minor(dev);

	/* also checks valid minor # */
	if (n < nvty)
		return &pccons[n];
	else
		return 0;
}

static int tty2vty(struct tty *tp)
{
	int i;
	for (i=0; i<nvty; i++)
		if (vtys[i].ttyp==tp) return i;
	return -1;
}

/*
 * open a vty device. Name 'pcopen' is historical
 *
 */
int pcopen(dev_t dev, int flag, int mode, struct proc *p)
{
	register struct tty *tp = dev2tty(dev);

	if (!tp) return ENXIO;

	/* increment ref count */
	vtys[minor(dev)].ttycnt++;

	tp->t_oproc = pcstart;
	tp->t_param = pcparam;
	tp->t_dev = dev;

	consoftc.cs_flags |= CO_OPEN|CO_INITTTY;
	consoftc.cs_opencnt++;

	if ((tp->t_state & TS_ISOPEN) == 0) {
		tp->t_state |= TS_WOPEN;
		ttychars(tp);
		tp->t_iflag = TTYDEF_IFLAG;
		tp->t_oflag = TTYDEF_OFLAG;
		tp->t_cflag = TTYDEF_CFLAG;
		tp->t_lflag = TTYDEF_LFLAG;
		tp->t_ispeed = tp->t_ospeed = TTYDEF_SPEED;
		tp->t_iflag &= ~ISTRIP;		
		pcparam(tp, &tp->t_termios);
		ttsetwater(tp);
	} else if (tp->t_state&TS_XCLUDE && p->p_ucred->cr_uid != 0)
		return (EBUSY);
	tp->t_state |= TS_CARR_ON;
	return ((*linesw[tp->t_line].l_open)(dev, tp));
}

/*
 * Close a vty
 */
/*ARGSUSED*/
int pcclose(dev_t dev, int flag, int mode, struct proc *p)
{
	register struct tty *tp = dev2tty(dev);
	struct vty *vp = &vtys[minor(dev)];

	if (!tp) return ENXIO;	

	/* decrement vty reference count */
	if (vp->ttycnt > 0) vp->ttycnt--;

	if (consoftc.cs_opencnt==0) {
		consoftc.cs_flags &= ~CO_OPEN;

		/* reset the keyboard state */
		reset_kbd_flags();
		initrb(&co_buf);
	}
	else
		consoftc.cs_opencnt--;

	(*linesw[tp->t_line].l_close)(tp, flag);
	ttyclose(tp);

	return(0);
}

/*
 * read from vty 
 */
/*ARGSUSED*/
int pcread(dev_t dev, struct uio *uio, int flag)
{
	register struct tty *tp = dev2tty(dev);
	if (!tp) return ENXIO;
	
	/* this does not belong to here, but anybody always wants to
	   strip the 8th bit, very likely the shell */
	tp->t_iflag &= ~ISTRIP;		
	return ((*linesw[tp->t_line].l_read)(tp, uio, flag));
}

/*
 * write to vty
 */
/*ARGSUSED*/
int pcwrite(dev_t dev, struct uio *uio, int flag)
{
	register struct tty *tp = dev2tty(dev);
	if (!tp) return ENXIO;

	/* we allow writing, but we don't know where it goes to */
	/*if (consoftc.cs_flags & CO_OPENRAW) return EBUSY; */

	return ((*linesw[tp->t_line].l_write)(tp, uio, flag));
}

/*
 * execute vty ioctl
 * This does no longer accept keyboard and vga oriented ioctls
 */
int pcioctl(dev_t dev, int cmd, caddr_t data, int flag)
{
	/* call the ioctl handler */
	return consioctl(dev, cmd, data, flag);
}

int	pcconsintr = 1;
/*
 * Got a console transmission interrupt -
 * the console processor wants another character.
 *
 * -hv- is this really used?
 */
pcxint(dev)
	dev_t dev;
{
	register struct tty *tp = dev2tty(dev);
	register int unit;

	if (!tp) return;

	if (!pcconsintr)
		return;
	tp->t_state &= ~TS_BUSY;
	consoftc.cs_timo = 0;
	if (tp->t_line)
		(*linesw[tp->t_line].l_start)(tp);
	else
		pcstart(tp);
}

int pcstart(register struct tty *tp)
{
	int c, s, vp;

	s = spltty();
	if (tp->t_state & (TS_TIMEOUT|TS_BUSY|TS_TTSTOP))
		goto out;
	do {
		if (RB_LEN(&tp->t_out) <= tp->t_lowat) {
			if (tp->t_state&TS_ASLEEP) {
				tp->t_state &= ~TS_ASLEEP;
				wakeup((caddr_t)&tp->t_out);
			}
			if (tp->t_wsel) {
				selwakeup(tp->t_wsel, tp->t_state & TS_WCOLL);
				tp->t_wsel = 0;
				tp->t_state &= ~TS_WCOLL;
			}
		}
		if (RB_LEN(&tp->t_out) == 0)
			goto out;
		c = getc(&tp->t_out);
		tp->t_state |= TS_BUSY;
		splx(s);
		vp = tty2vty(tp);
		if (vp < 0) panic("pcstart: unknown vty");
		sput(vp, c, 0);
		tp->t_state &= ~TS_BUSY;
		(void)spltty();
	} while(1);
out:
	splx(s);
}

/* interface for console device */
void pccnprobe(struct consdev *cp)
{
	int	maj;
	int	dev = 0;

	/* locate the major number */
	for (maj = 0; maj < nchrdev; maj++)
		if (cdevsw[maj].d_open == pcopen)
			break;

maj = 12;
	/* initialize required fields */
	cp->cn_dev = makedev(maj, 0);
	cp->cn_tp = &pccons[0];
	cp->cn_pri = CN_INTERNAL;
}

/* interface for console device */
/* ARGSUSED */
void pccninit(struct consdev *cp)
{
	/*
	 * For now, don't screw with it.
	 */
	/* crtat = 0; */
}

/* interface for console device */
/* ARGSUSED */
void pccnputc(dev_t dev, int c)
{
	int clr = __color;

	if (!dev2tty(dev)) return;	/* ignore if invalid */

	if (clr == 0)
		clr = 0x30;
	else
		clr |= 0x60;

	if (c == '\n')
		sput(minor(dev), '\r', clr);
	sput(minor(dev), c, clr);
}

#ifdef notyetused
/*
 * Print a character on console.
 */
pcputchar(c, tp)
	char c;
	register struct tty *tp;
{
	sput(0, c, 0x2);
	if (c=='\n') getchar();
}
#endif

/* interface for console device */
/* ARGSUSED */
int pccngetc(dev_t dev)
{
	register int s;
	register XCHAR *cp;

	s = spltty();		/* block cointr while we poll */
	while ((cp = kbd_sgetc(0))==NULL);
	splx(s);
	if (*cp == '\r') return('\n');
	return (char)*cp;
}

#ifdef notyetused
pcgetchar(tp)
	register struct tty *tp;
{
	XCHAR *cp;

	cp = kbd_sgetc(0);	/* this is surely ASCII */
	return (char)(*cp&0xff);
}
#endif

/*
 * Set line parameters
 */
int pcparam(struct tty *tp, struct termios *t)
{
	register int cflag = t->c_cflag;
        /* and copy to tty */
        tp->t_ispeed = t->c_ispeed;
        tp->t_ospeed = t->c_ospeed;
        tp->t_cflag = cflag;

	return(0);
}

#ifdef KDB
/*
 * Turn input polling on/off (used by debugger).
 */
/*ARGSUSED*/
int pcpoll(int onoff)
{
}
#endif

#ifdef OLD_PATCHKIT
int pg(char *p,int q,int r,int s,int t,int u,int v,int w,int x,int y,int z)
{
	printf(p,q,r,s,t,u,v,w,x,y,z);
	printf("\n");
	return(getchar());
}
#endif

/* special characters */
#define bs	8
#define lf	10	
#define cr	13	
#define cntlc	3	
#define del	0177	
#define cntld	4

int getchar()
{
	XCHAR		thechar,*c;
	register	delay;
	int		x;

	consoftc.cs_flags |= CO_POLLING;
	x = splhigh();
	sput(0, '>', 1);
	/*while (1) {*/
		while ((c=kbd_sgetc(0))==NULL);
		thechar = *c;
		consoftc.cs_flags &= ~CO_POLLING;
		splx(x);
		switch (thechar) {
		    default: if (thechar >= ' ')
			     	sput(0, thechar, 1);
			     return(thechar);
		    case cr:
		    case lf: sput(0, '\r', 1);
		    		sput(0, '\n', 1);
			     return(lf);
		    case bs:
		    case del:
			     sput(0, '\b', 1);
			     sput(0, ' ', 1);
			     sput(0, '\b', 1);
			     return(thechar);
		    case cntlc:
			     sput(0, '^', 1) ; sput(0, 'C', 1) ; sput(0, '\r', 1) ; sput(0, '\n', 1) ;
			     cpu_reset();
		    case cntld:
			     sput(0, '^', 1) ; sput(0, 'D', 1) ; sput(0, '\r', 1) ; sput(0, '\n', 1) ;
			     return(0);
		}
	/*}*/
}

#ifdef notyetused
#include "machine/stdarg.h"
static nrow;

#define	DPAUSE 1
void
#ifdef __STDC__
dprintf(unsigned flgs, const char *fmt, ...)
#else
dprintf(flgs, fmt /*, va_alist */)
        char *fmt;
	unsigned flgs;
#endif
{	extern unsigned __debug;
	va_list ap;

	if((flgs&__debug) > DPAUSE) {
		__color = ffs(flgs&__debug)+1;
		va_start(ap,fmt);
		kprintf(fmt, 1, (struct tty *)0, ap);
		va_end(ap);
	if (flgs&DPAUSE || nrow%24 == 23) { 
		int x;
		x = splhigh();
		if (nrow%24 == 23) nrow = 0;
		(void)kbd_sgetc(0);
		splx(x);
	}
	}
	__color = 0;
}
#endif

#endif /* NPC=0 */
#endif /* NCO=1 */
