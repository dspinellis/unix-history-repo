/*	kern_prot.c	5.1	82/07/15	*/

/*
 * System calls related to protection
 */

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

getuid()
{

	u.u_r.r_val1 = u.u_ruid;
	u.u_r.r_val2 = u.u_uid;
}

setuid()
{
	register uid;
	register struct a {
		int	uid;
	} *uap;

	uap = (struct a *)u.u_ap;
	uid = uap->uid;
	if (u.u_ruid == uid || u.u_uid == uid || suser()) {
		u.u_uid = uid;
		u.u_procp->p_uid = uid;
		u.u_ruid = uid;
	}
}

getgid()
{

	u.u_r.r_val1 = u.u_rgid;
	u.u_r.r_val2 = u.u_gid;
}

setgid()
{
	register gid;
	register struct a {
		int	gid;
	} *uap;

	uap = (struct a *)u.u_ap;
	gid = uap->gid;
	if (u.u_rgid == gid || u.u_gid == gid || suser()) {
		u.u_gid = gid;
		u.u_rgid = gid;
	}
}
