/*	sys_file.c	5.8	82/12/17	*/

#include "../machine/reg.h"

#include "../h/param.h"
#include "../h/systm.h"
#include "../h/dir.h"
#include "../h/user.h"
#include "../h/inode.h"
#include "../h/proc.h"
#include "../h/timeb.h"
#include "../h/times.h"
#include "../h/reboot.h"
#include "../h/fs.h"
#include "../h/conf.h"
#include "../h/buf.h"
#include "../h/mount.h"
#include "../h/file.h"

portal()
{

}

utimes()
{

}

/*
 * Lock a file:
 * Ip is the inode associated with the file to be locked,
 * flags is the current "locking state" of the file relative
 * to the process' file descriptor (i.e. u_pofile), and cmd
 * is the new action to be applied.  We need flags in case
 * we're changing a shared lock to a exclusive lock, or vice versa.
 *
 * NB: the inode should't be ilocked before the call as the
 *     only fields we modify are private to the flocki and
 *     funlocki; and later accesses to the inode would block
 *     in ilock where they're not interruptible.
 */
flocki(ip, flags, cmd)
	register struct inode *ip;
	int flags, cmd;
{
	register int priority = PLOCK;

	if (cmd&FEXLOCK)
		priority++;
	/*
	 * If there's a exclusive lock currently applied
	 * to the file, or someone waiting to get a
	 * exclusive lock, then we've gotta wait for the
	 * lock with everyone else.
	 */
again:
	while (ip->i_flag&(IEXLOCK|ILWAIT)) {
		if (cmd&FNBLOCK) {
			u.u_error = EWOULDBLOCK;
			return (flags);
		}
		/*
		 * If we're holding a write
		 * lock, then release it.
		 */
		if (flags&UF_EXLOCK) {
			funlocki(ip, UF_EXLOCK);
			flags &= ~UF_EXLOCK;
			goto again;
		}
		ip->i_flag |= ILWAIT;
		sleep((caddr_t)&ip->i_exlockc, priority);
	}
	if (cmd&FEXLOCK) {
		cmd &= ~FSHLOCK;
		/*
		 * Must wait for any shared locks to finish
		 * before we try to apply a exclusive lock.
		 */
		while (ip->i_flag&ISHLOCK) {
			if (cmd&FNBLOCK) {
				u.u_error = EWOULDBLOCK;
				return (flags);
			}
			/*
			 * If we're holding a shared
			 * lock, then release it.
			 */
			if (flags&UF_SHLOCK) {
				funlocki(ip, UF_SHLOCK);
				flags &= ~UF_SHLOCK;
				goto again;
			}
			ip->i_flag |= ILWAIT;
			sleep((caddr_t)&ip->i_shlockc, PLOCK);
		}
	}
	if (flags&(UF_SHLOCK|UF_EXLOCK))
		panic("flocki");
	if (cmd&FSHLOCK) {
		ip->i_shlockc++;
		ip->i_flag |= ISHLOCK;
		flags |= UF_SHLOCK;
	}
	if (cmd&FEXLOCK) {
		ip->i_exlockc++;
		ip->i_flag |= IEXLOCK;
		flags |= UF_EXLOCK;
	}
	return (flags);
}

/*
 * Unlock a file.
 */
funlocki(ip, locktype)
	register struct inode *ip;
	int locktype;
{
	int flags;

	if (ip == NULL)
		panic("funlocki");
	flags = ip->i_flag;
	if (locktype&UF_SHLOCK) {
		if ((flags&ISHLOCK) == 0)
			panic("no shared lock");
		if (--ip->i_shlockc == 0) {
			ip->i_flag &= ~ISHLOCK;
			if (flags&ILWAIT)
				wakeup((caddr_t)&ip->i_shlockc);
		}
	}
	if (locktype&UF_EXLOCK) {
		if ((flags&IEXLOCK) == 0)
			panic("no exclusive lock");
		if (--ip->i_exlockc == 0) {
			ip->i_flag &= ~(IEXLOCK|ILWAIT);
			if (flags&ILWAIT)
				wakeup((caddr_t)&ip->i_exlockc);
		}
	}
}
