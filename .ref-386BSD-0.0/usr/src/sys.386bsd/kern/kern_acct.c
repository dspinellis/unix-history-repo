/*
 * Copyright (c) 1982, 1986, 1989 Regents of the University of California.
 * All rights reserved.
 *
 * Redistribution and use in source and binary forms, with or without
 * modification, are permitted provided that the following conditions
 * are met:
 * 1. Redistributions of source code must retain the above copyright
 *    notice, this list of conditions and the following disclaimer.
 * 2. Redistributions in binary form must reproduce the above copyright
 *    notice, this list of conditions and the following disclaimer in the
 *    documentation and/or other materials provided with the distribution.
 * 3. All advertising materials mentioning features or use of this software
 *    must display the following acknowledgement:
 *	This product includes software developed by the University of
 *	California, Berkeley and its contributors.
 * 4. Neither the name of the University nor the names of its contributors
 *    may be used to endorse or promote products derived from this software
 *    without specific prior written permission.
 *
 * THIS SOFTWARE IS PROVIDED BY THE REGENTS AND CONTRIBUTORS ``AS IS'' AND
 * ANY EXPRESS OR IMPLIED WARRANTIES, INCLUDING, BUT NOT LIMITED TO, THE
 * IMPLIED WARRANTIES OF MERCHANTABILITY AND FITNESS FOR A PARTICULAR PURPOSE
 * ARE DISCLAIMED.  IN NO EVENT SHALL THE REGENTS OR CONTRIBUTORS BE LIABLE
 * FOR ANY DIRECT, INDIRECT, INCIDENTAL, SPECIAL, EXEMPLARY, OR CONSEQUENTIAL
 * DAMAGES (INCLUDING, BUT NOT LIMITED TO, PROCUREMENT OF SUBSTITUTE GOODS
 * OR SERVICES; LOSS OF USE, DATA, OR PROFITS; OR BUSINESS INTERRUPTION)
 * HOWEVER CAUSED AND ON ANY THEORY OF LIABILITY, WHETHER IN CONTRACT, STRICT
 * LIABILITY, OR TORT (INCLUDING NEGLIGENCE OR OTHERWISE) ARISING IN ANY WAY
 * OUT OF THE USE OF THIS SOFTWARE, EVEN IF ADVISED OF THE POSSIBILITY OF
 * SUCH DAMAGE.
 *
 *	from: @(#)kern_acct.c	7.18 (Berkeley) 5/11/91
 */

#include "param.h"
#include "systm.h"
#include "namei.h"
#include "resourcevar.h"
#include "proc.h"
#include "ioctl.h"
#include "termios.h"
#include "tty.h"
#include "vnode.h"
#include "mount.h"
#include "kernel.h"
#include "file.h"
#include "acct.h"
#include "syslog.h"

/*
 * Values associated with enabling and disabling accounting
 */
int	acctsuspend = 2;	/* stop accounting when < 2% free space left */
int	acctresume = 4;		/* resume when free space risen to > 4% */
struct	timeval chk = { 15, 0 };/* frequency to check space for accounting */
struct  vnode *acctp;		/* file to which to do accounting */
struct  vnode *savacctp;	/* file to which to do accounting when space */

/*
 * Enable or disable process accounting.
 *
 * If a non-null filename is given, that file is used to store accounting
 * records on process exit. If a null filename is given process accounting
 * is suspended. If accounting is enabled, the system checks the amount
 * of freespace on the filesystem at timeval intervals. If the amount of
 * freespace is below acctsuspend percent, accounting is suspended. If
 * accounting has been suspended, and freespace rises above acctresume,
 * accounting is resumed.
 */
/* ARGSUSED */
sysacct(p, uap, retval)
	struct proc *p;
	struct args {
		char	*fname;
	} *uap;
	int *retval;
{

	/*
	 * Body deleted.
	 */
	return (ENOSYS);
}

/*
 * Periodically check the file system to see if accounting
 * should be turned on or off.
 */
acctwatch(resettime)
	struct timeval *resettime;
{
	struct statfs sb;

	if (savacctp) {
		(void)VFS_STATFS(savacctp->v_mount, &sb, (struct proc *)0);
		if (sb.f_bavail > acctresume * sb.f_blocks / 100) {
			acctp = savacctp;
			savacctp = NULL;
			log(LOG_NOTICE, "Accounting resumed\n");
			return;
		}
	}
	if (acctp == NULL)
		return;
	(void)VFS_STATFS(acctp->v_mount, &sb, (struct proc *)0);
	if (sb.f_bavail <= acctsuspend * sb.f_blocks / 100) {
		savacctp = acctp;
		acctp = NULL;
		log(LOG_NOTICE, "Accounting suspended\n");
	}
	timeout(acctwatch, (caddr_t)resettime, hzto(resettime));
}

/*
 * This routine calculates an accounting record for a process and,
 * if accounting is enabled, writes it to the accounting file.
 */
acct(p)
	register struct proc *p;
{

	/*
	 * Body deleted.
	 */
	return;
}
