/*	kern_time.c	5.1	82/07/15	*/

#include "../h/param.h"
#include "../h/systm.h"
#include "../h/dir.h"
#include "../h/user.h"
#include "../h/reg.h"
#include "../h/inode.h"
#include "../h/proc.h"
#include "../h/clock.h"
#include "../h/mtpr.h"
#include "../h/timeb.h"
#include "../h/times.h"
#include "../h/reboot.h"
#include "../h/fs.h"
#include "../h/conf.h"
#include "../h/buf.h"
#include "../h/mount.h"

/*
 * return the current time (old-style entry)
 */
gtime()
{
	u.u_r.r_time = time;
	if (clkwrap())
		clkset();
}

/*
 * New time entry-- return TOD with milliseconds, timezone,
 * DST flag
 */
ftime()
{
	register struct a {
		struct	timeb	*tp;
	} *uap;
	struct timeb t;
	register unsigned ms;

	uap = (struct a *)u.u_ap;
	(void) spl7();
	t.time = time;
	ms = lbolt;
	(void) spl0();
	if (ms > hz) {
		ms -= hz;
		t.time++;
	}
	t.millitm = (1000*ms)/hz;
	t.timezone = timezone;
	t.dstflag = dstflag;
	if (copyout((caddr_t)&t, (caddr_t)uap->tp, sizeof(t)) < 0)
		u.u_error = EFAULT;
	if (clkwrap())
		clkset();
}

/*
 * Set the time
 */
stime()
{
	register struct a {
		time_t	time;
	} *uap;

	uap = (struct a *)u.u_ap;
	if (suser()) {
		bootime += uap->time - time;
		time = uap->time;
		clkset();
	}
}

times()
{
	register struct a {
		time_t	(*times)[4];
	} *uap;
	struct tms tms;

	tms.tms_utime = u.u_vm.vm_utime;
	tms.tms_stime = u.u_vm.vm_stime;
	tms.tms_cutime = u.u_cvm.vm_utime;
	tms.tms_cstime = u.u_cvm.vm_stime;
	uap = (struct a *)u.u_ap;
	if (copyout((caddr_t)&tms, (caddr_t)uap->times, sizeof(struct tms)) < 0)
		u.u_error = EFAULT;
}
