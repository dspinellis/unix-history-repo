#include "/sys/nsys/param.h"
#include "/sys/nsys/user.h"
#include "/sys/nsys/proc.h"
#include "/sys/nsys/text.h"
#include "/sys/nsys/systm.h"
#include "/sys/nsys/file.h"
#include "/sys/nsys/inode.h"
#include "/sys/nsys/buf.h"

#define	PS	0177776
struct	{
	int	integ;
};

sleep(chan, pri)
{
	int s;
	register *rp;

	s = PS->integ;
	if(pri >= 0) {
		if(issig())
			goto psig;
		rp = u.u_procp;
		rp->p_wchan = chan;
		rp->p_stat = SWAIT;
		rp->p_pri = pri;
		spl0();
		if(runin != 0) {
			runin = 0;
			wakeup(&runin);
		}
		swtch();
		if(issig()) {
		psig:
			spl0();
			u.u_rsav[0] = u.u_qsav[0];
			u.u_rsav[1] = u.u_qsav[1];
			retu(u.u_procp->p_addr);
			return;
		}
	} else {
		rp = u.u_procp;
		rp->p_wchan = chan;
		rp->p_stat = SSLEEP;
		rp->p_pri = pri;
		spl0();
		swtch();
	}
	PS->integ = s;
}

wakeup(chan)
{
	register struct proc *p;
	register i;
	int n;

loop:
	n = 0;
	p = &proc[0];
	for(i=0; i<NPROC; i++) {
		if(p->p_wchan == chan) {
			if(runout!=0 && (p->p_flag&SLOAD)==0) {
				runout = 0;
				n++;
			}
			p->p_wchan = 0;
			p->p_stat = SRUN;
			runrun++;
		}
		p++;
	}
	if(n) {
		chan = &runout;
		goto loop;
	}
}

#define	NDIS	5
cookies[NDIS]
{
	20,
	30,
	10,
	5,
	2
};
sched()
{
	static struct proc *p1, *p2;
	register struct proc *rp;
	int a;

	p1 = p2 = &proc[0];

	/*
	 * find user to swap in
	 */

loop:
	spl6();
	rp = p1;
	for(a=0; a<NPROC; a++) {
		rp++;
		if(rp >= &proc[NPROC])
			rp = &proc[0];
		if(rp->p_stat==SRUN && (rp->p_flag&SLOAD)==0) {
			p1 = rp;
			goto found;
		}
	}
	runout++;
	sleep(&runout, PSWP);
	goto loop;

	/*
	 * find core for that user
	 */

found:
	spl0();
	rp = p1;
	a = rp->p_size;
	if((rp=rp->p_textp) != NULL)
		if(rp->x_ccount == 0)
			a =+ rp->x_size;
	if((a=malloc(coremap, a)) != NULL)
		goto found2;

	/*
	 * if none, find user to
	 * swap out
	 */

	spl6();
	rp = p2;
	for(a=0; a<NPROC; a++) {
		rp++;
		if(rp >= &proc[NPROC])
			rp = &proc[0];
		if((rp->p_flag&(SSYS|SLOCK|SLOAD))==SLOAD &&
		    rp->p_stat == SWAIT) {
			rp->p_ndis = 0;
			goto found1;
		}
	}
	for(a=0; a<NPROC; a++) {
		rp++;
		if(rp >= &proc[NPROC])
			rp = &proc[0];
		if((rp->p_flag&(SSYS|SLOCK|SLOAD))==SLOAD &&
		    rp->p_cook == 0 &&
		   (rp->p_stat==SRUN || rp->p_stat==SSLEEP)) {
			rp->p_ndis++;
			if(rp->p_ndis >= NDIS)
				rp->p_ndis--;
			goto found1;
		}
	}
	runin++;
	sleep(&runin, PSWP);
	goto found;

	/*
	 * swap user out
	 */

found1:
	spl0();
	rp->p_flag =& ~SLOAD;
	p2 = rp;
	xswap(rp, 1, 0);
	goto found;

	/*
	 * swap user in
	 */

found2:
	if((rp=p1->p_textp) != NULL) {
		if(rp->x_ccount == 0) {
			if(swap(rp->x_daddr, a, rp->x_size, B_READ))
				goto swaper;
			rp = p1->p_textp;
			rp->x_caddr = a;
			a =+ rp->x_size;
		}
		rp->x_ccount++;
	}
	rp = p1;
	if(swap(rp->p_addr, a, rp->p_size, B_READ))
		goto swaper;
	rp = p1;
	mfree(swapmap, (rp->p_size+7)/8, rp->p_addr);
	rp = p1;
	rp->p_addr = a;
	rp->p_flag =| SLOAD;
	rp->p_cook = cookies[rp->p_ndis];
	goto loop;

swaper:
	panic("swap error");
}

swtch()
{
	static int *p;
	register i, n;
	register struct proc *rp;

	if(p == 0)
		p = &proc[0];
	savu(u.u_rsav);
	retu(proc[0].p_addr);

loop:
	rp = p;
	p = NULL;
	n = 127;
	for(i=0; i<NPROC; i++) {
		rp++;
		if(rp >= &proc[NPROC])
			rp = &proc[0];
		if(rp->p_stat==SRUN && (rp->p_flag&SLOAD)==SLOAD) {
			if(rp->p_pri < n) {
				p = rp;
				n = rp->p_pri;
			}
		}
	}
	if(p == NULL) {
		p = rp;
		idle();
		goto loop;
	}
	retu(p->p_addr);
	sureg();
	if(p->p_flag&SSWAP) {
		p->p_flag =& ~SSWAP;
		u.u_rsav[0] = u.u_ssav[0];
		u.u_rsav[1] = u.u_ssav[1];
		retu(p->p_addr);
	}
	return(1);
}

newproc()
{
	int a1, a2, n;
	struct proc *p, *up;
	register struct proc *rpp;
	register *rip;

	for(rpp = &proc[0]; rpp < &proc[NPROC]; rpp++)
		if(rpp->p_stat == NULL)
			goto found;
	panic("no procs");

found:
	/*
	 * make proc entry for new proc
	 */

	p = rpp;
	rip = u.u_procp;
	up = rip;
	rpp->p_stat = SRUN;
	rpp->p_flag = SLOAD;
	rpp->p_ttyp = rip->p_ttyp;
	rpp->p_textp = rip->p_textp;
	rpp->p_pid = ++mpid;
	rpp->p_ppid = rip->p_pid;
	rpp->p_ndis = 0;
	rpp->p_cook = cookies[0];

	/*
	 * make duplicate entries
	 * where needed
	 */

	for(rip = &u.u_ofile[0]; rip < &u.u_ofile[NOFILE];)
		if((rpp = *rip++) != NULL)
			rpp->f_count++;
	if((rpp=up->p_textp) != NULL) {
		rpp->x_count++;
		rpp->x_ccount++;
	}
	u.u_cdir->i_count++;

	/*
	 * swap out old process
	 * to make image of new proc
	 */

	savu(u.u_rsav);
	u.u_procp = p;
	rip = up;
	n = rip->p_size;
	a1 = rip->p_addr;
	p->p_size = n;
	a2 = malloc(coremap, n);
	if(a2 == NULL) {
		up->p_stat = SIDL;
		p->p_addr = a1;
		savu(u.u_ssav);
		xswap(p, 0, 0);
		p->p_flag =| SSWAP;
		up->p_stat = SRUN;
	} else {
		p->p_addr = a2;
		while(n--)
			copyseg(a1++, a2++);
	}
	u.u_procp = up;
	return(0);
}

expand(newsize)
{
	int i, n, a1, a2;
	int *p;

	p = u.u_procp;
	n = p->p_size;
	p->p_size = newsize;
	a1 = p->p_addr;
	if(n >= newsize) {
		mfree(coremap, n-newsize, a1+newsize);
		return;
	}
	savu(u.u_rsav);
	a2 = malloc(coremap, newsize);
	if(a2 == NULL) {
		savu(u.u_ssav);
		xswap(p, 1, n);
		p->p_flag =| SSWAP;
		swtch();
		/* no return */
	}
	p->p_addr = a2;
	for(i=0; i<n; i++)
		copyseg(a1+i, a2++);
	mfree(coremap, n, a1);
	retu(p->p_addr);
	sureg();
}
