/*	ttynew.c	4.13	81/07/22	*/

/*
 * New version of tty driver, supported
 * as NTTYDISC.  Adapted from a tty.c written
 * by J.E.Kulp of IIASA, Austria (jekulp@mc)
 */
#include "../h/param.h"
#include "../h/systm.h"
#include "../h/dir.h"
#include "../h/user.h"
#include "../h/tty.h"
#include "../h/proc.h"
#include "../h/mx.h"
#include "../h/inode.h"
#include "../h/file.h"
#include "../h/reg.h"
#include "../h/conf.h"
#include "../h/buf.h"
#include "../h/dk.h"

char	partab[];

/*
 * Input mapping table-- if an entry is non-zero, when the
 * corresponding character is typed preceded by "\" the escape
 * sequence is replaced by the table value.  Mostly used for
 * upper-case only terminals.
 */

char	maptab[];		/* see tty.c */

/*
 * shorthand
 */
#define	q1	tp->t_rawq
#define	q2	tp->t_canq
#define	q3	tp->t_outq
#define	q4	tp->t_un.t_ctlq

#define	OBUFSIZ	100

/*
 * routine called on opens while tp->t_line == NTTYDISC
 * establishes a process group for distribution of
 * quits and interrupts from the tty.
 * (actually, pp->p_pgrp can't be 0 when this routine
 * is called since NTTYDISC is not the default discipline)
 */
ntyopen(dev, tp)
dev_t dev;
register struct tty *tp;
{
	register struct proc *pp;

	pp = u.u_procp;
	tp->t_dev = dev;
	if(pp->p_pgrp == 0) {
		u.u_ttyp = tp;
		u.u_ttyd = dev;
		if (tp->t_pgrp == 0)
			tp->t_pgrp = pp->p_pid;
		pp->p_pgrp = tp->t_pgrp;
	}
	tp->t_state &= ~WOPEN;
	tp->t_state |= ISOPEN;
	if (tp->t_line != NTTYDISC)
		wflushtty(tp);
}

/*
 * clean tp on last close
 */
ntyclose(tp)
register struct tty *tp;
{

	wflushtty(tp);
	/* let ttyclose do the work if we are really closing */
}

/*
 * block transfer input handler.
 */
ntyrend(tp, pb, pe)
register struct tty *tp;
register char *pb, *pe;
{
	int	tandem;

	tandem = tp->t_flags&TANDEM;
	if (tp->t_flags&RAW) {
		(void) b_to_q(pb, pe-pb, &tp->t_rawq);
		if (tp->t_chan)
			(void) sdata(tp->t_chan);
		wakeup((caddr_t)&tp->t_rawq);
	} else {
		tp->t_flags &= ~TANDEM;
		while (pb < pe)
			ntyinput(*pb++, tp);
		tp->t_flags |= tandem;
	}
	if (tandem)
		ttyblock(tp);
}

/*
 * reinput pending characters after state switch
 * call at spl5().
 */
ntypend(tp)
register struct tty *tp;
{
	struct clist tq;
	register c;

	tp->t_local &= ~LPENDIN;
	tp->t_lstate |= LSTYPEN;
	tq = tp->t_rawq;
	tp->t_rawq.c_cc = 0;
	tp->t_rawq.c_cf = tp->t_rawq.c_cl = 0;
	while ((c = getc(&tq)) >= 0)
		ntyinput(c, tp);
	tp->t_lstate &= ~LSTYPEN;
}

/*
 * Place a character on raw TTY input queue, putting in delimiters
 * and waking up top half as needed.
 * Also echo if required.
 * The arguments are the character and the appropriate
 * tty structure.
 */
ntyinput(c, tp)
register c;
register struct tty *tp;
{
	register int t_flags;
	int i;

	if (tp->t_local&LPENDIN)
		ntypend(tp);
	tk_nin++;
	c &= 0377;
	t_flags = tp->t_flags;
	if (t_flags&TANDEM)
		ttyblock(tp);
	if ((t_flags&RAW)==0) {
		if ((tp->t_lstate&LSTYPEN) == 0)
			c &= 0177;
	/* check for literal nexting very first */
		if (tp->t_lstate&LSLNCH) {
			c |= 0200;
			tp->t_lstate &= ~LSLNCH;
		}
		if (c==tlun.t_lnextc) {
			if (tp->t_flags&ECHO)
				ntyout("^\b", tp);
			tp->t_lstate |= LSLNCH;
	/* check for output control functions */
		} else if (c==tun.t_stopc) {
			if ((tp->t_state&TTSTOP)==0) {
				tp->t_state |= TTSTOP;
				(*cdevsw[major(tp->t_dev)].d_stop)(tp);
				return;
			}
			if (c!=tun.t_startc)
				return;
		} else if (c==tun.t_startc) {
			tp->t_state &= ~TTSTOP;
			tp->t_local &= ~LFLUSHO;
			ttstart(tp);
			return;
		} else if (c==tlun.t_flushc) {
			if (tp->t_local & LFLUSHO)
				tp->t_local &= ~LFLUSHO;
			else {
				flushtty(tp, FWRITE);
				ntyecho(c, tp);
				if (tp->t_rawq.c_cc+tp->t_canq.c_cc)
					ntyretype(tp);
				tp->t_local |= LFLUSHO;
			}
			ttstart(tp);
			return;
		} else if (c==tlun.t_suspc || c==tun.t_intrc ||
			   c==tun.t_quitc) {
			if ((tp->t_local & LNOFLSH) == 0)
				flushtty(tp,
				    c==tlun.t_suspc ? FREAD : FREAD|FWRITE);
			ntyecho(c, tp);
			c = c==tun.t_intrc ? SIGINT :
				((c==tun.t_quitc) ? SIGQUIT : SIGTSTP);
			if (tp->t_chan)
				scontrol(tp->t_chan, M_SIG, c);
			gsignal(tp->t_pgrp, c);
	/* check for buffer editing functions - cooked mode */
		} else if ((t_flags&CBREAK) == 0) {
			if ((tp->t_lstate&LSQUOT) &&
			    (c==tp->t_erase||c==tp->t_kill)) {
				ntyrub(unputc(&tp->t_rawq), tp);
				c |= 0200;
			}
			if (c==tp->t_erase) {
				if (tp->t_rawq.c_cc)
					ntyrub(unputc(&tp->t_rawq), tp);
			} else if (c==tp->t_kill) {
				if (tp->t_local&LCRTKIL &&
				    tp->t_rawq.c_cc == tp->t_rocount) {
					while (tp->t_rawq.c_cc)
						ntyrub(unputc(&tp->t_rawq), tp);
				} else {
					ntyecho(c, tp);
					ntyecho('\n', tp);
					while (getc(&tp->t_rawq) > 0)
						;
					tp->t_rocount = 0;
				}
				tp->t_lstate = 0;
			} else if (c==tlun.t_werasc) {
				if (tp->t_rawq.c_cc == 0)
					goto out;
				do {
					c = unputc(&tp->t_rawq);
					if (c != ' ' && c != '\t')
						goto erasenb;
					ntyrub(c, tp);
				} while (tp->t_rawq.c_cc);
				goto out;
			    erasenb:
				do {
					ntyrub(c, tp);
					if (tp->t_rawq.c_cc == 0)
						goto out;
					c = unputc(&tp->t_rawq);
				} while (c != ' ' && c != '\t');
				(void) putc(c, &tp->t_rawq);
			} else if (c==tlun.t_rprntc) {
				ntyretype(tp);
	/* check for cooked mode input buffer overflow */
			} else if (tp->t_rawq.c_cc + tp->t_canq.c_cc > TTYHOG) {
				/* we should start a timeout that flushes the
				   buffer if it stays full - same in CBREAK */
				if (tp->t_outq.c_cc < TTHIWAT(tp))
					(void) ntyoutput(CTRL(g), tp);
	/* put data char in q for user and wakeup if a break char */
			} else if (putc(c, &tp->t_rawq) >= 0) {
				if (!ntbreakc(c, tp)) {
					if (tp->t_rocount++ == 0)
						tp->t_rocol = tp->t_col;
				} else {
					tp->t_rocount = 0;
					catq(&tp->t_rawq, &tp->t_canq);
					if (tp->t_chan)
						(void) sdata(tp->t_chan);
					wakeup((caddr_t)&tp->t_rawq);
					if (tp->t_local&LINTRUP)
						gsignal(tp->t_pgrp, SIGTINT);
				}
				tp->t_lstate &= ~LSQUOT;
				if (c == '\\')
					tp->t_lstate |= LSQUOT;
				if (tp->t_lstate&LSERASE) {
					tp->t_lstate &= ~LSERASE;
					(void) ntyoutput('/', tp);
				}
				i = tp->t_col;
				ntyecho(c, tp);
				if (c==tun.t_eofc && tp->t_flags&ECHO) {
					i = MIN(2, tp->t_col - i);
					while (i > 0) {
						(void) ntyoutput('\b', tp);
						i--;
					}
				}
			}
	/* CBREAK mode */
		} else if (tp->t_rawq.c_cc > TTYHOG) {
			if (tp->t_outq.c_cc < TTHIWAT(tp))
				(void) ntyoutput(CTRL(g), tp);
		} else if (putc(c, &tp->t_rawq) >= 0) {
			if (tp->t_local&LINTRUP)
				gsignal(tp->t_pgrp, SIGTINT);
			if (tp->t_chan)
				(void) sdata(tp->t_chan);
			wakeup((caddr_t)&tp->t_rawq);
			ntyecho(c, tp);
		}
	/* RAW mode */
	} else if (tp->t_rawq.c_cc > TTYHOG) 
		flushtty(tp, FREAD|FWRITE);
	else {
		if (putc(c, &tp->t_rawq) >= 0) {
			if (tp->t_local&LINTRUP)
				gsignal(tp->t_pgrp, SIGTINT);
			if (tp->t_chan)
				(void) sdata(tp->t_chan);
			wakeup((caddr_t)&tp->t_rawq);
		}
		ntyecho(c, tp);
	}
out:
	if (tp->t_local & LDECCTQ && tp->t_state & TTSTOP &&
	    tun.t_startc != tun.t_stopc)
		return;
	tp->t_state &= ~TTSTOP;
	tp->t_local &= ~LFLUSHO;
	ttstart(tp);
}

/*
 * put character on TTY output queue, adding delays,
 * expanding tabs, and handling the CR/NL bit.
 * It is called both from the top half for output, and from
 * interrupt level for echoing.
 * The arguments are the character and the tty structure.
 * Returns < 0 if putc succeeds, otherwise returns char to resend
 * Must be recursive.
 */
ntyoutput(c, tp)
register c;
register struct tty *tp;
{
	register char *colp;
	register ctype;

	if (tp->t_flags&RAW || tp->t_local&LLITOUT) {
		if (tp->t_local&LFLUSHO)
			return (-1);
		if (putc(c, &tp->t_outq))
			return(c);
		tk_nout++;
		return (-1);
	}
	/*
	 * Ignore EOT in normal mode to avoid hanging up
	 * certain terminals.
	 */
	c &= 0177;
	if (c==CEOT && (tp->t_flags&CBREAK)==0)
		return (-1);
	/*
	 * Turn tabs to spaces as required
	 */
	if (c=='\t' && (tp->t_flags&TBDELAY)==XTABS) {
		register int s;

		c = 8 - (tp->t_col&7);
		if ((tp->t_local&LFLUSHO) == 0) {
			s = spl5();		/* don't interrupt tabs */
			c -= b_to_q("        ", c, &tp->t_outq);
			tk_nout += c;
			splx(s);
		}
		tp->t_col += c;
		return (c ? -1 : '\t');
	}
	tk_nout++;
	/*
	 * for upper-case-only terminals,
	 * generate escapes.
	 */
	if (tp->t_flags&LCASE) {
		colp = "({)}!|^~'`";
		while(*colp++)
			if(c == *colp++) {
				if (ntyoutput('\\', tp) >= 0)
					return (c);
				c = colp[-2];
				break;
			}
		if ('A'<=c && c<='Z') {
			if (ntyoutput('\\', tp) >= 0)
				return (c);
		} else if ('a'<=c && c<='z')
			c += 'A' - 'a';
	}
	/*
	 * turn <nl> to <cr><lf> if desired.
	 */
	if (c=='\n' && tp->t_flags&CRMOD)
		if (ntyoutput('\r', tp) >= 0)
			return (c);
	if (c=='~' && tp->t_local&LTILDE)
		c = '`';
	if ((tp->t_local&LFLUSHO) == 0 && putc(c, &tp->t_outq))
		return (c);
	/*
	 * Calculate delays.
	 * The numbers here represent clock ticks
	 * and are not necessarily optimal for all terminals.
	 * The delays are indicated by characters above 0200.
	 * In raw mode there are no delays and the
	 * transmission path is 8 bits wide.
	 */
	colp = &tp->t_col;
	ctype = partab[c];
	c = 0;
	switch (ctype&077) {

	case ORDINARY:
		(*colp)++;

	case CONTROL:
		break;

	case BACKSPACE:
		if (*colp)
			(*colp)--;
		break;

	case NEWLINE:
		ctype = (tp->t_flags >> 8) & 03;
		if(ctype == 1) { /* tty 37 */
			if (*colp)
				c = max(((unsigned)*colp>>4) + 3, (unsigned)6);
		} else
		if(ctype == 2) { /* vt05 */
			c = 6;
		}
		*colp = 0;
		break;

	case TAB:
		ctype = (tp->t_flags >> 10) & 03;
		if(ctype == 1) { /* tty 37 */
			c = 1 - (*colp | ~07);
			if(c < 5)
				c = 0;
		}
		*colp |= 07;
		(*colp)++;
		break;

	case VTAB:
		if(tp->t_flags & VTDELAY) /* tty 37 */
			c = 0177;
		break;

	case RETURN:
		ctype = (tp->t_flags >> 12) & 03;
		if(ctype == 1) { /* tn 300 */
			c = 5;
		} else if(ctype == 2) { /* ti 700 */
			c = 10;
		} else if(ctype == 3) { /* concept 100 */
			int i;
			if ((i = *colp) >= 0)
				for (; i<9; i++)
					(void) putc(0177, &tp->t_outq);
		}
		*colp = 0;
	}
	if(c && (tp->t_local&LFLUSHO) == 0)
		(void) putc(c|0200, &tp->t_outq);
	return (-1);
}

/*
 * Called from device's read routine after it has
 * calculated the tty-structure given as argument.
 */
ntread(tp)
register struct tty *tp;
{
	register struct clist *qp;
	register c, first;

	if ((tp->t_state&CARR_ON)==0)
		return(0);
loop:
	(void) spl5();
	if (tp->t_local&LPENDIN)
		ntypend(tp);
	(void) spl0();
	while (tp == u.u_ttyp && u.u_procp->p_pgrp != tp->t_pgrp) {
		if (u.u_signal[SIGTTIN] == SIG_IGN ||
		    u.u_signal[SIGTTIN] == SIG_HOLD ||
		    (u.u_procp->p_flag&SDETACH) ||
		    u.u_procp->p_flag&SVFORK)
			return (0);
		gsignal(u.u_procp->p_pgrp, SIGTTIN);
		sleep((caddr_t)&lbolt, TTIPRI);
	}
	if (tp->t_flags&RAW) {
		(void) spl5();
		if (tp->t_rawq.c_cc <= 0) {
			if ((tp->t_state&CARR_ON)==0 ||
			    (tp->t_chan && getf(u.u_ap[0])->f_flag&FMP)) {
				(void) spl0();
				return (0);
			}
			if (tp->t_local&LINTRUP &&
			    u.u_signal[SIGTINT] != SIG_DFL) {
				u.u_error = EIO;
				(void) spl0();
				return (0);
			}
			sleep((caddr_t)&tp->t_rawq, TTIPRI);
			(void) spl0();
			goto loop;
		}
		(void) spl0();
		while (tp->t_rawq.c_cc && passc(getc(&tp->t_rawq))>=0)
			;
		return (0);
	} else {
		qp = tp->t_flags & CBREAK ? &tp->t_rawq : &tp->t_canq;
		(void) spl5();
		if (qp->c_cc <= 0) {
			if ((tp->t_state&CARR_ON)==0 ||
			    (tp->t_chan!=NULL && getf(u.u_ap[0])->f_flag&FMP)) {
				(void) spl0();
				return (0);
			}
			if (tp->t_local&LINTRUP &&
			    u.u_signal[SIGTINT] != SIG_DFL) {
				u.u_error = EIO;
				(void) spl0();
				return (0);
			}
			sleep((caddr_t)&tp->t_rawq, TTIPRI);
			(void) spl0();
			goto loop;
		}
		(void) spl0();
		first = 1;
		while ((c = getc(qp)) >= 0) {
			if (tp->t_flags&CRMOD && c == '\r')
				c = '\n';
			if (tp->t_flags&LCASE && c <= 0177)
				if (tp->t_lstate&LSBKSL) {
					if (maptab[c])
						c = maptab[c];
					tp->t_lstate &= ~LSBKSL;
				} else if (c >= 'A' && c <= 'Z')
					c += 'a' - 'A';
				else if (c == '\\') {
					tp->t_lstate |= LSBKSL;
					continue;
				}
			if (c == tlun.t_dsuspc) {
				gsignal(tp->t_pgrp, SIGTSTP);
				if (first) {
					sleep((caddr_t)&lbolt, TTIPRI);
					goto loop;
				}
				break;
			}
			if (c == tun.t_eofc && (tp->t_flags&CBREAK)==0)
				break;
			if (passc(c & 0177) < 0)
				break;
			if ((tp->t_flags&CBREAK)==0 && ntbreakc(c, tp))
				break;
			first = 0;
		}
		tp->t_lstate &= ~LSBKSL;
	}

	if (tp->t_state&TBLOCK && tp->t_rawq.c_cc < TTYHOG/5) {
		if (putc(tun.t_startc, &tp->t_outq)==0) {
			tp->t_state &= ~TBLOCK;
			ttstart(tp);
		}
		tp->t_char = 0;
	}

	return (tp->t_rawq.c_cc + tp->t_canq.c_cc);
}

/*
 * Called from the device's write routine after it has
 * calculated the tty-structure given as argument.
 */
caddr_t
ntwrite(tp)
register struct tty *tp;
{
#ifdef vax
	/*
	 * THE POSITIONING OF CP, CC, AND CE ARE CRITICAL
	 * AND MUST NOT BE CHANGED WITHOUT PATCHING
	 * THE 'ASM' INLINES BELOW.  WATCH OUT.
	 */
#endif
	register char *cp;
	register int cc, ce;
	register i;
	char obuf[OBUFSIZ];
	register c;
	int hiwat = TTHIWAT(tp);

	if ((tp->t_state&CARR_ON)==0)
		return (NULL);
loop:
	while (u.u_procp->p_pgrp != tp->t_pgrp && tp == u.u_ttyp &&
	    (tp->t_local&LTOSTOP) && (u.u_procp->p_flag&SVFORK)==0 &&
	    u.u_signal[SIGTTOU] != SIG_IGN &&
	    u.u_signal[SIGTTOU] != SIG_HOLD &&
	    (u.u_procp->p_flag&SDETACH)==0) {
		gsignal(u.u_procp->p_pgrp, SIGTTOU);
		sleep((caddr_t)&lbolt, TTIPRI);
	}
	while (u.u_count) {
		cc = MIN(u.u_count, OBUFSIZ);
		cp = obuf;
		iomove(cp, (unsigned)cc, B_WRITE);
		if (u.u_error)
			break;
		if (tp->t_outq.c_cc > hiwat)
			goto ovhiwat;
		if (tp->t_local&LFLUSHO)
			continue;
		if (tp->t_flags&LCASE || tp->t_local&LTILDE) {
			while (cc--) {
				c = *cp++;
				tp->t_rocount = 0;
				while((c = ntyoutput(c, tp)) >= 0) {
					/* out of clists, wait a bit */
					ttstart(tp);
					sleep((caddr_t)&lbolt, TTOPRI);
					tp->t_rocount = 0;
				}
				if (tp->t_outq.c_cc > hiwat)
					goto ovhiwat;
			}
			continue;
		}
		while (cc) {
			if (tp->t_flags&RAW || tp->t_local&LLITOUT)
				ce = cc;
			else {
#ifdef vax
				asm("	scanc	r9,(r10),_partab,$077");
				asm("	subl3	r0,r9,r8");
#else
				ce=0;
				while(((partab[*(unsigned char *)(cp+ce)]&077)==0)&&(ce<cc))
					ce++;
#endif
				if (ce==0) {
					tp->t_rocount = 0;
					if (ntyoutput(*cp, tp) >= 0) {
						ttstart(tp);
						sleep((caddr_t)&lbolt, TTOPRI);
						continue;
					}
					cp++;
					cc--;
					if (tp->t_outq.c_cc > hiwat)
						goto ovhiwat;
				}
			}
			tp->t_rocount = 0;
			i=b_to_q(cp,ce,&tp->t_outq);
			ce-=i;
			tk_nout+=ce;
			tp->t_col+=ce;
			cp+=ce;
			cc-=ce;
			if (i) {
				ttstart(tp);
				sleep((caddr_t)&lbolt, TTOPRI);
			}
			if (ce || tp->t_outq.c_cc > hiwat)
				goto ovhiwat;
		}
	}
	ttstart(tp);
	return(NULL);

ovhiwat:
	(void) spl5();
	u.u_base -= cc;
	u.u_offset -= cc;
	u.u_count += cc;
	if (tp->t_outq.c_cc <= hiwat) {
		(void) spl0();
		goto loop;
	}
	ttstart(tp);
	tp->t_state |= ASLEEP;
	if (tp->t_chan && u.u_segflg == 0 && (getf(u.u_ap[0])->f_flag&FMP)) {
		(void) spl0();
		return ((caddr_t)&tp->t_outq);
	}
	sleep((caddr_t)&tp->t_outq, TTOPRI);
	(void) spl0();
	goto loop;
}

/*
 * Rubout one character from the rawq of tp
 * as cleanly as possible.
 */
ntyrub(c, tp)
register c;
register struct tty *tp;
{
	register char *cp;
	register int savecol;
	int s;
	char *nextc();

	if ((tp->t_flags&ECHO)==0)
		return;
	tp->t_local &= ~LFLUSHO;
	c &= 0377;
	if (tp->t_local&LCRTBS) {
		if (tp->t_rocount == 0) {
			/*
			 * Screwed by ttwrite; retype
			 */
			ntyretype(tp);
			return;
		}
		if (c==('\t'|0200) || c==('\n'|0200))
			ntyrubo(tp, 2);
		else switch(partab[c&=0177] & 0177) {

		case ORDINARY:
			if (tp->t_flags&LCASE && c >= 'A' && c <= 'Z')
				ntyrubo(tp, 2);
			else
				ntyrubo(tp, 1);
			break;

		case VTAB:
		case BACKSPACE:
		case CONTROL:
		case RETURN:
			if (tp->t_local & LCTLECH)
				ntyrubo(tp, 2);
			break;

		case TAB:
			if (tp->t_rocount < tp->t_rawq.c_cc) {
				ntyretype(tp);
				return;
			}
			s = spl5();
			savecol = tp->t_col;
			tp->t_lstate |= LSCNTTB;
			tp->t_local |= LFLUSHO;
			tp->t_col = tp->t_rocol;
			for (cp = tp->t_rawq.c_cf; cp; cp = nextc(&tp->t_rawq, cp))
				ntyecho(*cp, tp);
			tp->t_local &= ~LFLUSHO;
			tp->t_lstate &= ~LSCNTTB;
			splx(s);
			/*
			 * savecol will now be length of the tab
			 */
			savecol -= tp->t_col;
			tp->t_col += savecol;
			if (savecol > 8)
				savecol = 8;		/* overflow screw */
			while (--savecol >= 0)
				(void) ntyoutput('\b', tp);
			break;

		default:
			panic("ttyrub");
		}
	} else if (tp->t_local&LPRTERA) {
		if ((tp->t_lstate&LSERASE) == 0) {
			(void) ntyoutput('\\', tp);
			tp->t_lstate |= LSERASE;
		}
		ntyecho(c, tp);
	} else
		ntyecho(tp->t_erase, tp);
	tp->t_rocount--;
}

/*
 * Crt back over cnt chars perhaps
 * erasing them.
 */
ntyrubo(tp, cnt)
register struct tty *tp;
int cnt;
{

	while (--cnt >= 0)
		ntyout(tp->t_local&LCRTERA ? "\b \b" : "\b", tp);
}

/*
 * Reprint the rawq line.
 * We assume c_cc has already been checked.
 */
ntyretype(tp)
register struct tty *tp;
{
	register char *cp;
	char *nextc();
	int s;

	if (tlun.t_rprntc != 0377)
		ntyecho(tlun.t_rprntc, tp);
	(void) ntyoutput('\n', tp);
	s = spl5();
	for (cp = tp->t_canq.c_cf; cp; cp = nextc(&tp->t_canq, cp))
		ntyecho(*cp, tp);
	for (cp = tp->t_rawq.c_cf; cp; cp = nextc(&tp->t_rawq, cp))
		ntyecho(*cp, tp);
	tp->t_lstate &= ~LSERASE;
	splx(s);
	tp->t_rocount = tp->t_rawq.c_cc;
	tp->t_rocol = 0;
}

/*
 * Echo a typed character to the terminal
 */
ntyecho(c, tp)
register c;
register struct tty *tp;
{

	if ((tp->t_lstate & LSCNTTB) == 0)
		tp->t_local &= ~LFLUSHO;
	if ((tp->t_flags&ECHO) == 0)
		return;
	c &= 0377;
	if (tp->t_flags&RAW) {
		(void) ntyoutput(c, tp);
		return;
	}
	if (c == '\r' && tp->t_flags&CRMOD)
		c = '\n';
	if (tp->t_local&LCTLECH) {
		if ((c&0177) <= 037 && c!='\t' && c!='\n' || (c&0177)==0177) {
			(void) ntyoutput('^', tp);
			c &= 0177;
			if (c == 0177)
				c = '?';
			else if (tp->t_flags&LCASE)
				c += 'a' - 1;
			else
				c += 'A' - 1;
		}
	}
	(void) ntyoutput(c & 0177, tp);
}

/*
 * Is c a break char for tp?
 */
ntbreakc(c, tp)
register c;
register struct tty *tp;
{
	return (c == '\n' || c == tun.t_eofc || c == tun.t_brkc ||
		c == '\r' && (tp->t_flags&CRMOD));
}

/*
 * send string cp to tp
 */
ntyout(cp, tp)
register char *cp;
register struct tty *tp;
{
	register char c;

	while (c = *cp++)
		(void) ntyoutput(c, tp);
}
