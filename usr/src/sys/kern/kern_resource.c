/*	kern_resource.c	4.6	82/02/27	*/

#include "../h/param.h"
#include "../h/systm.h"
#include "../h/acct.h"
#include "../h/dir.h"
#include "../h/user.h"
#include "../h/inode.h"
#include "../h/proc.h"
#include "../h/seg.h"

struct	inode *acctp;

/*
 * Perform process accounting functions.
 */
sysacct()
{
	register struct inode *ip;
	register struct a {
		char	*fname;
	} *uap;

	uap = (struct a *)u.u_ap;
	if (suser()) {
		if (uap->fname==NULL) {
			if (ip = acctp) {
				ilock(ip);
				iput(ip);
				acctp = NULL;
			}
			return;
		}
		if (acctp) {
			u.u_error = EBUSY;
			return;
		}
		ip = namei(uchar, 0, 1);
		if(ip == NULL)
			return;
		if((ip->i_mode & IFMT) != IFREG) {
			u.u_error = EACCES;
			iput(ip);
			return;
		}
		acctp = ip;
		irele(ip);
	}
}

struct	acct acctbuf;
/*
 * On exit, write a record on the accounting file.
 */
acct()
{
	register i;
	register struct inode *ip;
	off_t siz;
	register struct acct *ap = &acctbuf;

	if ((ip=acctp)==NULL)
		return;
	ilock(ip);
	for (i=0; i<sizeof(ap->ac_comm); i++)
		ap->ac_comm[i] = u.u_comm[i];
	ap->ac_utime = compress((long)u.u_vm.vm_utime);
	ap->ac_stime = compress((long)u.u_vm.vm_stime);
	ap->ac_etime = compress((long)(time - u.u_start));
	ap->ac_btime = u.u_start;
	ap->ac_uid = u.u_ruid;
	ap->ac_gid = u.u_rgid;
	ap->ac_mem = 0;
	if (i = u.u_vm.vm_utime + u.u_vm.vm_stime)
		ap->ac_mem = (u.u_vm.vm_ixrss + u.u_vm.vm_idsrss) / i;
	ap->ac_io = compress((long)(u.u_vm.vm_inblk + u.u_vm.vm_oublk));
	ap->ac_tty = u.u_ttyd;
	ap->ac_flag = u.u_acflag;
	siz = ip->i_size;
	u.u_offset = siz;
	u.u_base = (caddr_t)ap;
	u.u_count = sizeof(acctbuf);
	u.u_segflg = 1;
	u.u_error = 0;
	writei(ip);
	if(u.u_error)
		ip->i_size = siz;
	irele(ip);
}

/*
 * Produce a pseudo-floating point representation
 * with 3 bits base-8 exponent, 13 bits fraction.
 */
compress(t)
register long t;
{
	register exp = 0, round = 0;

	while (t >= 8192) {
		exp++;
		round = t&04;
		t >>= 3;
	}
	if (round) {
		t++;
		if (t >= 8192) {
			t >>= 3;
			exp++;
		}
	}
	return((exp<<13) + t);
}
