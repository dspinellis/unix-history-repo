/*
 * Copyright (c) 1989, 1993
 *	The Regents of the University of California.  All rights reserved.
 *
 * This code is derived from software contributed to Berkeley by
 * Rick Macklem at The University of Guelph.
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
static char copyright[] =
"@(#) Copyright (c) 1989, 1993\n\
	The Regents of the University of California.  All rights reserved.\n";
#endif not lint

#ifndef lint
static char sccsid[] = "@(#)nfsiod.c	8.1 (Berkeley) 6/5/93";
#endif not lint

#include <stdio.h>
#include <signal.h>
#include <fcntl.h>
#include <sys/syslog.h>
#include <sys/param.h>
#include <sys/ioctl.h>
#include <sys/wait.h>
#include <sys/ucred.h>
#include <nfs/nfsv2.h>
#include <nfs/nfs.h>

/* Global defs */
#ifdef DEBUG
int debug = 1;
#else
int debug = 0;
#endif
extern int errno;
void reapchild();

/*
 * Nfsiod does asynchronous buffered I/O on behalf of the NFS client.
 * It does not have to be running for correct operation, but will improve
 * throughput. The one optional argument is the number of children to fork.
 */
main(argc, argv)
	int argc;
	char *argv[];
{
	register int i;
	int cnt;

	if (argc != 2 || (cnt = atoi(argv[1])) <= 0 || cnt > 20)
		cnt = 1;
	if (debug == 0) {
		daemon(0, 0);
		signal(SIGINT, SIG_IGN);
		signal(SIGQUIT, SIG_IGN);
		signal(SIGHUP, SIG_IGN);
	}
	signal(SIGCHLD, reapchild);
	openlog("nfsiod:", LOG_PID, LOG_DAEMON);
	for (i = 1; i < cnt; i++)
		if (fork() == 0)
			break;
	if (nfssvc(NFSSVC_BIOD, (char *)0) < 0)
		syslog(LOG_ERR, "nfssvc failed %m");
}

void
reapchild()
{

	while (wait3((int *) NULL, WNOHANG, (struct rusage *) NULL))
		;
}
