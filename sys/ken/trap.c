#include "/sys/nsys/param.h"
#include "/sys/nsys/systm.h"
#include "/sys/nsys/user.h"
#include "/sys/nsys/proc.h"
#include "/sys/nsys/reg.h"
#include "/sys/nsys/text.h"

#define	EBIT	1
#define	UMODE	0170000
struct	{
	int	count;
	int	(*call)();
} sysent[64];
struct {
	char	hibyte;
	char	lobyte;
};
char	regloc[8]
{
	R0, R1, R2, R3, R4, R5, R6, R7
};

trap(dev, sp, r4, r3, r2, r1, nps, r0, pc, ps)
char *sp;
{
	int i, a;

	if(dev == 8) {
		psignal(u.u_procp, SIGFPT);
		return;
	}
	if((ps&UMODE) != UMODE)
		goto bad;
	if(dev==9 && sp<-u.u_ssize*64) {
		(&r0)[regloc[ssr[1].lobyte&07]] =- ssr[1].lobyte>>3;
		(&r0)[regloc[ssr[1].hibyte&07]] =- ssr[1].hibyte>>3;
		pc = ssr[2];
		if(!estabur(u.u_tsize, u.u_dsize, u.u_ssize+SINCR)) {
			u.u_ssize =+ SINCR;
			expand(u.u_procp->p_size+SINCR);
			a = u.u_procp->p_addr + u.u_procp->p_size;
			for(i=0; i<u.u_ssize; i++) {
				a--;
				copyseg(a-SINCR, a);
			}
			return;
		}
	}
	u.u_error = 0;
	switch(dev) {

	case 0:
		i = SIGBUS;
		goto def;

	case 1:
		i = SIGINS;
		goto def;

	case 2:
		i = SIGTRC;
		goto def;

	case 3:
		i = SIGIOT;
		goto def;

	case 5:
		i = SIGEMT;
		goto def;

	case 9:
		i = SIGSEG;
		goto def;

	def:
		psignal(u.u_procp, i);

	default:
		u.u_error = dev+100;

	case 6:;
	}
	if(u.u_error)
		goto err;
	ps =& ~EBIT;
	dev = fuword(pc-2)&077;
	if(dev == 0) { /* indirect */
		a = fuword(pc);
		pc =+ 2;
		dev = fuword(a)&077;
		a =+ 2;
	} else {
		a = pc;
		pc =+ sysent[dev].count*2;
	}
	for(i=0; i<sysent[dev].count; i++) {
		u.u_arg[i] = fuword(a);
		a =+ 2;
	}
	u.u_dirp = u.u_arg[0];
	u.u_ar0 = &r0;
	trap1(sysent[dev].call);
	if(u.u_error >= 100)
		psignal(u.u_procp, SIGSYS);
err:
	if(issig())
		psig();
	if(u.u_error != 0) {
		ps =| EBIT;
		r0 = u.u_error;
	}
	u.u_procp->p_pri = PUSER + u.u_procp->p_ndis;
	if((i=u.u_procp->p_textp)!=0 && i->x_ccount==0)
		panic("text not loaded");
	return;

bad:
	printf("dev = %o\n", dev);
	printf("sp  = %o\n", sp);
	printf("pc  = %o\n", pc);
	printf("ps  = %o\n", ps);
	printf("aps = %o\n", &ps);
	panic("trap");
}

trap1(f)
int (*f)();
{

	savu(u.u_qsav);
	(*f)();
}

nosys()
{

	printf("sys %d\n", u.u_ar0[-7]);
	u.u_error = 100;
}

nullsys()
{
}
