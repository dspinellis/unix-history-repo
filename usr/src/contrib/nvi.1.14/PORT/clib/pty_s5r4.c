/*-
 * Copyright (c) 1993
 *	The Regents of the University of California.
 * All rights reserved.
 *
 * This code is derived from software contributed to Berkeley by
 * Brian Hirt.
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
 */

#ifndef lint
static char sccsid[] = "@(#)pty_s5r4.c	8.1 (Berkeley) 12/22/93";
#endif /* not lint */

#include <sys/types.h>
#include <sys/cdefs.h>
#include <sys/ioctl.h>
#include <sys/stat.h>
#include <sys/stropts.h>

#include <errno.h>
#include <fcntl.h>
#include <grp.h>
#include <stdio.h>
#include <string.h>
#include <termios.h>
#include <unistd.h>


/*
 * ptym_open --
 *	This function opens a master pty and returns the file descriptor
 *	to it.  pts_name is also returned which is the name of the slave.
 */
static int
ptym_open(pts_name)
	char *pts_name;
{
	int fdm;
	char *ptr;

	strcpy(pts_name,"/dev/ptmx");
	if ( (fdm = open(pts_name,O_RDWR)) < 0 )
		return(-1);

	if (grantpt(fdm) < 0) 
	{
		close(fdm);
		return(-2);
	}

	if (unlockpt(fdm) < 0) 
	{
		close(fdm);
		return(-3);
	}

	if (unlockpt(fdm) < 0) 
	{
		close(fdm);
		return(-3);
	}

	/* get slave's name */
	if ( (ptr = ptsname(fdm)) == NULL) 
	{
		close(fdm);
		return(-3);
	}
	strcpy(pts_name,ptr);
	return(fdm);
}

/*
 * ptys_open --
 *	This function opens the slave pty.
 */
static int
ptys_open(fdm, pts_name)
	int fdm;
	char *pts_name;
{
	int fds;

	if ( (fds = open(pts_name, O_RDWR)) < 0) 
	{
		close(fdm);
		return(-5);
	}

	if (ioctl(fds, I_PUSH, "ptem") < 0) 
	{
		close(fds);
		close(fdm);
		return(-6);
	}

	if (ioctl(fds, I_PUSH, "ldterm") < 0) 
	{
		close(fds);
		close(fdm);
		return(-7);
	}

	if (ioctl(fds, I_PUSH, "ttcompat") < 0) 
	{
		close(fds);
		close(fdm);
		return(-8);
	}

	return(fds);
}

int
openpty(amaster, aslave, name, termp, winp)
	int *amaster, *aslave;
	char *name;
	struct termios *termp;
{
	register int master, slave, ttygid;

	/* open master terminal */
	if ((master = ptym_open(name)) < 0)  
	{
		errno = ENOENT;	/* out of ptys */
		return(-1);
	}

	/* open slave terminal */
	if ((slave = ptys_open(master, name)) >= 0) 
	{
		*amaster = master;
		*aslave = slave;
	} 
	else 
	{
		errno = ENOENT;	/* out of ptys */
		return(-1);
	}

	if (termp)
		(void) tcsetattr(slave, TCSAFLUSH, termp);
	if (winp)
		(void) ioctl(slave, TIOCSWINSZ, (char *)winp);

	return (0);
}
