#include "/sys/nsys/param.h"
#include "/sys/nsys/systm.h"
#include "/sys/nsys/user.h"
#include "/sys/nsys/proc.h"

#define	UMODE	0170000
#define	CSW	0177570
#define	PS	0177776
struct	{
	int	integ;
};

clock(dev, sp, r4, r3, r2, r1, nps, r0, pc, ps)
{
	struct callo *p1;
	register struct callo *p2;
	register struct proc *pp;
	int i, p;

	/*
	 * restart clock
	 */

	LKS->integ = 0115;

	/*
	 * display register
	 */

	i = CSW->integ;
	p = PS->integ;
	if((i&1) != 0) {
		i--;
		PS->integ = 030340;
	} else
		PS->integ = 0340;
	CSW->integ = fuword(i);
	PS->integ = p;

	/*
	 * callouts
	 * if none, just return
	 */

	if(callout[0].c_func == 0)
		goto out;

	/*
	 * if ps is high, just update times
	 */

	if((ps&0340) != 0) {
		p2 = &callout[0];
		while(p2->c_time<=0 && p2->c_func!=0)
			p2++;
		p2->c_time--;
		goto out;
	}

	/*
	 * update times and callout
	 */

	spl5();
	if(--callout[0].c_time <= 0) {
		p1 = &callout[0];
		while(p1->c_func != 0 && p1->c_time <= 0) {
			(*p1->c_func)(p1->c_arg);
			p1++;
		}
		p2 = &callout[0];
		while(p2->c_func = p1->c_func) {
			p2->c_time = p1->c_time;
			p2->c_arg = p1->c_arg;
			p1++;
			p2++;
		}
	}

	/*
	 * lightning bolt time-out
	 * and time of day
	 */

out:
	if((ps&UMODE) == UMODE)
		u.u_utime++; else
		u.u_stime++;
	if(++lbolt >= 60) {
		if((ps&0340) != 0)
			return;
		lbolt =- 60;
		if(++time[1] == 0)
			++time[0];
		spl0();
		if(time[1]==tout[1] && time[0]==tout[0])
			wakeup(tout);
		if((time[1]&03) == 0)
			wakeup(&lbolt);
		p = 0;
		pp = &proc[0];
		for(i=0; i<NPROC; i++) {
			if(pp->p_flag&SLOAD)
				p++;
			pp++;
		}
		p = 10/p + 1;
		i = 0;
		while(pp > &proc[0]) {
			pp--;
			if(pp->p_flag&SLOAD) {
				pp->p_cook =- p;
				if(pp->p_cook <= 0) {
					i++;
					pp->p_cook = 0;
				}
			}
		}
		if(i!=0 && runin!=0) {
			runin = 0;
			wakeup(&runin);
		}
		if((ps&UMODE) == UMODE) {
			if(issig())
				psig(&r0);
			swtch();
		}
	}
}

timeout(fun, arg, tim)
{
	register struct callo *p1, *p2;
	register int s;

	s = PS->integ;
	spl7();
	p1 = &callout[0];
	while(p1->c_func != 0 && p1->c_time <= tim) {
		tim =- p1->c_time;
		p1++;
	}
	p1->c_time =- tim;
	p2 = p1;
	while(p2->c_func != 0)
		p2++;
	while(p2 >= p1) {
		(p2+1)->c_time = p2->c_time;
		(p2+1)->c_func = p2->c_func;
		(p2+1)->c_arg = p2->c_arg;
		p2--;
	}
	p1->c_time = tim;
	p1->c_func = fun;
	p1->c_arg = arg;
	PS->integ = s;
}
