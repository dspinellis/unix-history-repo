/*-
 * Copyright (c) 1982, 1986, 1989, 1993
 *	The Regents of the University of California.  All rights reserved.
 * (c) UNIX System Laboratories, Inc.
 * All or some portions of this file are derived from material licensed
 * to the University of California by American Telephone and Telegraph
 * Co. or Unix System Laboratories, Inc. and are reproduced herein with
 * the permission of UNIX System Laboratories, Inc.
 *
 * %sccs.include.redist.c%
 *
 *	from: @(#)kern_acct.c 8.8 (Berkeley) 5/14/95
 */

#include <sys/param.h>
#include <sys/proc.h>
#include <sys/mount.h>
#include <sys/vnode.h>
#include <sys/file.h>
#include <sys/syslog.h>
#include <sys/kernel.h>

acct(a1, a2, a3)
	struct proc *a1;
	struct acct_args /* {
		syscallarg(char *) path;
	} */ *a2;
	int *a3;
{
	/*
	 * Body deleted.
	 */
	return (ENOSYS);
}

acct_process(a1)
	struct proc *a1;
{

	/*
	 * Body deleted.
	 */
	return;
}

/*
 * Periodically check the file system to see if accounting
 * should be turned on or off. Beware the case where the vnode
 * has been vgone()'d out from underneath us, e.g. when the file
 * system containing the accounting file has been forcibly unmounted.
 */

/*
 * Values associated with enabling and disabling accounting
 */
int	acctsuspend = 2;	/* stop accounting when < 2% free space left */
int	acctresume = 4;		/* resume when free space risen to > 4% */
int	acctchkfreq = 15;	/* frequency (in seconds) to check space */

/*
 * SHOULD REPLACE THIS WITH A DRIVER THAT CAN BE READ TO SIMPLIFY.
 */
struct	vnode *acctp;
struct	vnode *savacctp;

/* ARGSUSED */
void
acctwatch(a)
	void *a;
{
	struct statfs sb;

	if (savacctp) {
		if (savacctp->v_type == VBAD) {
			(void) vn_close(savacctp, FWRITE, NOCRED, NULL);
			savacctp = NULL;
			return;
		}
		(void)VFS_STATFS(savacctp->v_mount, &sb, (struct proc *)0);
		if (sb.f_bavail > acctresume * sb.f_blocks / 100) {
			acctp = savacctp;
			savacctp = NULL;
			log(LOG_NOTICE, "Accounting resumed\n");
		}
	} else {
		if (acctp == NULL)
			return;
		if (acctp->v_type == VBAD) {
			(void) vn_close(acctp, FWRITE, NOCRED, NULL);
			acctp = NULL;
			return;
		}
		(void)VFS_STATFS(acctp->v_mount, &sb, (struct proc *)0);
		if (sb.f_bavail <= acctsuspend * sb.f_blocks / 100) {
			savacctp = acctp;
			acctp = NULL;
			log(LOG_NOTICE, "Accounting suspended\n");
		}
	}
	timeout(acctwatch, NULL, acctchkfreq * hz);
}
