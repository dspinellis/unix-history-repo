#
/*
 *	Copyright 1973 Bell Telephone Laboratories Inc
 */

#include "../param.h"
#include "../systm.h"
#include "../user.h"
#include "../proc.h"

#define	UMODE	0170000
#define	CSW	0177570
#define	PS	0177776
struct	{
	int	integ;
};

clock(dev, sp, r1, nps, r0, pc, ps)
{
	register struct callo *p1, *p2;
	register struct proc *pp;

	/*
	 * restart clock
	 */

	*lks = 0115;

	/*
	 * display register
	 */

	display();

	/*
	 * callouts
	 * if none, just return
	 * else update first non-zero time
	 */

	if(callout[0].c_func == 0)
		goto out;
	p2 = &callout[0];
	while(p2->c_time<=0 && p2->c_func!=0)
		p2++;
	p2->c_time--;

	/*
	 * if ps is high, just return
	 */

	if((ps&0340) != 0)
		goto out;

	/*
	 * callout
	 */

	spl5();
	if(callout[0].c_time <= 0) {
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
	if((ps&UMODE) == UMODE) {
		u.u_utime++;
		if(u.u_prof[3])
			incupc(pc, u.u_prof);
	} else
		u.u_stime++;
	if(++lbolt >= 60) {
		if((ps&0340) != 0)
			return;
		lbolt =- 60;
		if(++time[1] == 0)
			++time[0];
		spl1();
		if(time[1]==tout[1] && time[0]==tout[0])
			wakeup(tout);
		if((time[1]&03) == 0)
			wakeup(&lbolt);
		for(pp = &proc[0]; pp < &proc[NPROC]; pp++)
		if(pp->p_time != 127)
			pp->p_time++;
		if(runin!=0) {
			runin = 0;
			wakeup(&runin);
		}
		if((ps&UMODE) == UMODE) {
			u.u_ar0 = &r0;
			pp = u.u_procp;
			if(issig())
				psig();
			if(pp->p_pri < 105)
				pp->p_pri++;
			savfp();
			swtch();
		}
	}
}

timeout(fun, arg, tim)
{
	register struct callo *p1, *p2;
	register t;
	int s;

	t = tim;
	s = PS->integ;
	p1 = &callout[0];
	spl7();
	while(p1->c_func != 0 && p1->c_time <= t) {
		t =- p1->c_time;
		p1++;
	}
	p1->c_time =- t;
	p2 = p1;
	while(p2->c_func != 0)
		p2++;
	while(p2 >= p1) {
		(p2+1)->c_time = p2->c_time;
		(p2+1)->c_func = p2->c_func;
		(p2+1)->c_arg = p2->c_arg;
		p2--;
	}
	p1->c_time = t;
	p1->c_func = fun;
	p1->c_arg = arg;
	PS->integ = s;
}
