/*	ioctl.c	4.4	81/03/08	*/

/*
 * Ioctl.
 */
#include "../h/param.h"
#include "../h/systm.h"
#include "../h/dir.h"
#include "../h/user.h"
#include "../h/tty.h"
#include "../h/proc.h"
#include "../h/inode.h"
#include "../h/file.h"
#include "../h/conf.h"

/*
 * stty/gtty writearound
 */
stty()
{
	u.u_arg[2] = u.u_arg[1];
	u.u_arg[1] = TIOCSETP;
	ioctl();
}

gtty()
{
	u.u_arg[2] = u.u_arg[1];
	u.u_arg[1] = TIOCGETP;
	ioctl();
}

/*
 * ioctl system call
 * Check legality, execute common code, and switch out to individual
 * device routine.
 */
ioctl()
{
	register struct file *fp;
	register struct inode *ip;
	register struct a {
		int	fdes;
		int	cmd;
		caddr_t	cmarg;
	} *uap;
	register dev_t dev;
	register fmt;

	uap = (struct a *)u.u_ap;
	if ((fp = getf(uap->fdes)) == NULL)
		return;
	if ((fp->f_flag & (FREAD|FWRITE)) == 0) {
		u.u_error = EBADF;
		return;
	}
	if (uap->cmd==FIOCLEX) {
		u.u_pofile[uap->fdes] |= EXCLOSE;
		return;
	}
	if (uap->cmd==FIONCLEX) {
		u.u_pofile[uap->fdes] &= ~EXCLOSE;
		return;
	}
	ip = fp->f_inode;
	fmt = ip->i_mode & IFMT;
	if (fmt != IFCHR && fmt != IFMPC) {
/* begin local */
		if (uap->cmd==FIONREAD && (fmt == IFREG || fmt == IFDIR)) {
			off_t nread = ip->i_size - fp->f_un.f_offset;

			if (copyout((caddr_t)&nread, uap->cmarg, sizeof(off_t)))
				u.u_error = EFAULT;
		} else
/* end local */
			u.u_error = ENOTTY;
		return;
	}
	dev = ip->i_un.i_rdev;
	u.u_r.r_val1 = 0;
	if ((u.u_procp->p_flag&SNUSIG) && setjmp(u.u_qsav)) {
		u.u_eosys = RESTARTSYS;
		return;
	}
	(*cdevsw[major(dev)].d_ioctl)(dev, uap->cmd, uap->cmarg, 0);
}

/*
 * Do nothing specific version of line
 * discipline specific ioctl command.
 */
/*ARGSUSED*/
nullioctl(tp, cmd, addr)
struct tty *tp;
caddr_t addr;
{

	return (cmd);
}
