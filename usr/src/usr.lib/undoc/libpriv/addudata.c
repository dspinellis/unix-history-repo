/*	@(#)addudata.c	4.4	(Melbourne)	82/05/30	*/

/*
 * Add to udata structs together
 */

#include <sys/types.h>
#include <udata.h>

#define	Max(a)	if (from->a == 0 || to->a && from->a > to->a) to->a = from->a

addudata(to, from)
register struct udata *to;
register struct udata *from;
{
	Max(ud_expires);
	if (from->ud_maxrss > to->ud_maxrss)
		to->ud_maxrss = from->ud_maxrss;
	if (from->ud_maxfile > to->ud_maxfile)
		to->ud_maxfile = from->ud_maxfile;
	if (from->ud_maxcore > to->ud_maxcore)
		to->ud_maxcore = from->ud_maxcore;
	if (from->ud_maxstack > to->ud_maxstack)
		to->ud_maxstack = from->ud_maxstack;
	if (from->ud_maxdata > to->ud_maxdata)
		to->ud_maxdata = from->ud_maxdata;
	if (from->ud_maxcpu > to->ud_maxcpu)
		to->ud_maxcpu = from->ud_maxcpu;
	if (from->ud_maxlogin > to->ud_maxlogin)
		to->ud_maxlogin = from->ud_maxlogin;
	to->ud_raise &= from->ud_raise;
	to->ud_ttys |= from->ud_ttys;
	to->ud_grps |= from->ud_grps;
	to->ud_umask &= from->ud_umask;
	/*
	 * the next line is NOT GOOD ENOUGH
	 */
	to->ud_flags[0] |= from->ud_flags[0];
	/*
	 * and the rest of this is positively hopeless
	 * combining these things is bloody horrible, & may in fact
	 * be impossible (because of limitation to <= 4 ranges)
	 * So we will just take the 'to' times unless there are no
	 * time restrictions on the 'from' case
	 */
	if (from->ud_logon[0].tr_low.xt_ok == from->ud_logon[0].tr_high.xt_ok)
		to->ud_logon[0] = from->ud_logon[0];
}
