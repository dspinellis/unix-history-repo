#define	KERNEL	1
#include "../h/pk.p"


/*
 * kernel support routines.
 */

struct pack *pklines[NPLINES];
int	maxwindow =2;

/*
 * start initial synchronization.
 * allocate space.
 */
pkopen(dev, tp, addr)
register struct tty *tp;
caddr_t addr;
{
register struct pack *pk;
register i;
int pktimeout();
char **bp;
static	timer_on;
int s;
struct	piocb	piocb;


	if (tp->t_line)
		return;
	/*
	 * copy user parameters
	 */
	if (copyin(addr, (caddr_t)&piocb, sizeof (piocb))) {
		u.u_error = EFAULT;
		return;
	}
	npbits = dtom(sizeof(struct pack));
	pk = (struct pack *)getepack(npbits);
	if (pk==NULL)
		goto notsobad;
	pkzero((caddr_t)pk,sizeof (struct pack));
	pk->p_rwindow = piocb.window;
	if (pk->p_rwindow > maxwindow)
		pk->p_rwindow = maxwindow;
	pk->p_rsize = piocb.psize;
	if (pk->p_rsize > 512 || pk->p_rsize & 037)
		goto notsobad;
	pk->p_mode = piocb.mode;
	if (pk->p_mode & 01)
		pkdebug++;
	/*
	 * try to allocate input window
	 */
	pk->p_bits = dtom(pk->p_rsize);
	for(i=0; i<pk->p_rwindow; i++) {
		bp = (char **)getepack(pk->p_bits);
		if (bp==NULL)
			break;
		*bp = (char *)pk->p_ipool;
		pk->p_ipool = bp;
	}

	if (i==0 && bp==NULL)
		goto notsobad;
	pk->p_rwindow = i;


	/*
	 * start timer process,
	 * wait for synchronization.
	 */
	flushtty(tp);
	s = spl6();
	pkdisc = tp->t_line = piocb.t;
	pk->p_ttyp = tp;
	tp->t_linep = (caddr_t)pk;
	q2.c_cf = q2.c_cl = NULL;
	q1.c_cf = q1.c_cl = (caddr_t)&pk->p_ihbuf;
	q1.c_cc = -HDRSIZ;
	if (tp->t_iproc != NULL)
		(*tp->t_iproc)(tp);

	pk->p_rmsg = M_INITA;
	for(i=0; i<NPLINES; i++) {
		if (pklines[i]==NULL) {
			pklines[i] = pk;
			goto found;
		}
	}
	goto notsobad;
found:
	pk->p_timer++;
	if (timer_on==0) {
		timer_on++;
		pktimeout();
	}
	splx(s);
	SLEEP(&pk->p_state, PKOPRI);
	pkreset(pk);

	if ((pk->p_state&LIVE)==0) {
		pk->p_state = DOWN;
		pk->p_rmsg = 0;
notsobad:
		u.u_error = ENXIO;
		return;
	}

	pksetgrp(tp);
	pkioctl(DIOCGETP, tp, addr);

}




/*
 * unix ioctl interface
 */
pkioctl(com,tp,addr)
register struct tty *tp;
caddr_t addr;
{
struct piocb piocb;
register struct pack *pk;

	pk = (struct pack *)tp->t_linep;
	if (com == DIOCGETP) {
		piocb.window = pk->p_swindow;
		piocb.psize  = pk->p_xsize;
		piocb.state  = pk->p_state;
		if (copyout((caddr_t)&piocb, addr, sizeof(piocb))) {
			u.u_error = EFAULT;
		}
		if (u.u_error==0)
			u.u_r.r_val1 = piocb.psize;
	}
}


/*
 * Arrange for the device (i.e. tp)
 * to be able to generate signals if need be.
 */
pksetgrp(tp)
register struct tty *tp;
{
register struct proc *pp;

	pp = u.u_procp;
	if (pp->p_pgrp == 0)
		pp->p_pgrp = pp->p_pid;
	if (tp->t_pgrp == 0)
		tp->t_pgrp = pp->p_pgrp;
}



/*
 * Shut down io.
 * The problem is mainly input since the
 * device driver may have a buffer.
 */
pkturnoff(tp)
register struct tty *tp;
{
register char **bp;
register struct pack *pk;
register s;

	pk = PADDR;
	LOCK;
	bp = pk->p_io;
	tp->t_line = 0;
	q1.c_cf = NULL;
	flushtty(tp);
	if (bp!=NULL) {
		*bp = (char *)pk->p_ipool;
		pk->p_ipool = bp;
	}
	UNLOCK;
}



/*
 * link dead?
 */
pklive(pk)
register struct pack *pk;
{
register struct tty  *tp;

	tp = pk->p_ttyp;
	if (tp->t_line!=pkdisc || tp->t_linep!=(caddr_t)pk) {
		return(0);
	}
	return(tp->t_state&CARR_ON);
}



/*
 * timeout process:
 * wakes up periodically to check status
 * of active lines.
 */
pktimeout()
{
register struct pack *pk;
extern time_t time;
register i;

	for(i=0;i<NPLINES;i++) {
		if ((pk=pklines[i])==NULL)
			continue;
		if (pk->p_nout == pk->p_tout) {
			if (pk->p_xcount && pk->p_timer==0) {
				pk->p_timer = 3;
				pk->p_state |= WAITO;
			}
		} else
			pk->p_tout = pk->p_nout;
		if (pk->p_timer==0) {
			if (pk->p_state & BADFRAME) {
				pk->p_msg |= M_RJ;
				pk->p_state &= ~BADFRAME;
				goto startup;
			}
			if (pk->p_rmsg)
				goto startup;
			WAKEUP(&pk->p_ps);
			continue;
		}
		if (--pk->p_timer == 0) {
			if ((pk->p_state&LIVE)==0) {
			startup:
				pk->p_timer = 1;
			} else
			if (pk->p_state & WAITO) {
				if (pk->p_state&DRAINO)  {
					pk->p_state |= DOWN; 
				} else {
					pk->p_state |= RXMIT;
				}
				pkoutput(pk);
				pk->p_timer = 5+2*pkzot;
			}
			WAKEUP(&pk->p_ps);
			pk->p_msg |= pk->p_rmsg;
			if (pk->p_msg)
				pkoutput(pk);
		}
	}
	timeout(pktimeout, (caddr_t)pk, 60);


	/*
	 * randomize timeouts.
	 */
	pkzot = 2 + time&07;
}


