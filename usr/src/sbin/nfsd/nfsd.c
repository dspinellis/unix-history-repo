/*
 * Copyright (c) 1989 The Regents of the University of California.
 * All rights reserved.
 *
 * This code is derived from software contributed to Berkeley by
 * Rick Macklem at The University of Guelph.
 *
 * Redistribution and use in source and binary forms are permitted
 * provided that the above copyright notice and this paragraph are
 * duplicated in all such forms and that any documentation,
 * advertising materials, and other materials related to such
 * distribution and use acknowledge that the software was developed
 * by the University of California, Berkeley.  The name of the
 * University may not be used to endorse or promote products derived
 * from this software without specific prior written permission.
 * THIS SOFTWARE IS PROVIDED ``AS IS'' AND WITHOUT ANY EXPRESS OR
 * IMPLIED WARRANTIES, INCLUDING, WITHOUT LIMITATION, THE IMPLIED
 * WARRANTIES OF MERCHANTABILITY AND FITNESS FOR A PARTICULAR PURPOSE.
 */

#ifndef lint
char copyright[] =
"@(#) Copyright (c) 1989 Regents of the University of California.\n\
 All rights reserved.\n";
#endif not lint

#ifndef lint
static char sccsid[] = "@(#)nfsd.c	5.3 (Berkeley) %G%";
#endif not lint

#include <stdio.h>
#include <syslog.h>
#include <signal.h>
#include <fcntl.h>
#include <sys/types.h>
#include <sys/ioctl.h>
#include <sys/stat.h>
#include <sys/mount.h>
#include <sys/socket.h>
#include <sys/socketvar.h>
#include <netdb.h>
#include <rpc/rpc.h>
#include <rpc/pmap_clnt.h>
#include <rpc/pmap_prot.h>
#include <nfs/rpcv2.h>
#include <nfs/nfsv2.h>

/* Global defs */
#ifdef DEBUG
#define	syslog(e, s)	fprintf(stderr,(s))
int debug = 1;
#else
int debug = 0;
#endif

/*
 * Nfs server daemon mostly just a user context for nfssvc()
 * 1 - do file descriptor and signal cleanup
 * 2 - create server socket
 * 3 - register socket with portmap
 * 4 - nfssvc(sock)
 */
main(argc, argv)
	int argc;
	char *argv[];
{
	register int i;
	int cnt, sock;
	struct sockaddr_in saddr;
	u_long msk, mtch;

	if (debug == 0) {
		if (fork())
			exit(0);
		{ int s;
		for (s = 0; s < 10; s++)
			(void) close(s);
		}
		(void) open("/dev/null", O_RDONLY);
		(void) open("/dev/null", O_WRONLY);
		(void) dup2(1, 2);
		{ int tt = open("/dev/tty", O_RDWR);
		  if (tt > 0) {
			ioctl(tt, TIOCNOTTY, (char *)0);
			close(tt);
		  }
		}
		(void) setpgrp(0, 0);
		signal(SIGTSTP, SIG_IGN);
		signal(SIGTTIN, SIG_IGN);
		signal(SIGTTOU, SIG_IGN);
		signal(SIGINT, SIG_IGN);
		signal(SIGQUIT, SIG_IGN);
		signal(SIGTERM, SIG_IGN);
		signal(SIGHUP, SIG_IGN);
	}
	openlog("nfsd:", LOG_PID, LOG_DAEMON);
	if ((sock = socket(AF_INET, SOCK_DGRAM, 0)) < 0) {
		syslog(LOG_ERR, "Can't create socket");
		exit(1);
	}
	saddr.sin_family = AF_INET;
	saddr.sin_addr.s_addr = INADDR_ANY;
	saddr.sin_port = htons(NFS_PORT);
	if (bind(sock, &saddr, sizeof(saddr)) < 0) {
		syslog(LOG_ERR, "Can't bind addr");
		exit(1);
	}
	pmap_unset(RPCPROG_NFS, NFS_VER2);
	if (!pmap_set(RPCPROG_NFS, NFS_VER2, IPPROTO_UDP, NFS_PORT)) {
		syslog(LOG_ERR, "Can't register with portmap");
		exit(1);
	}
	if (argc == 2) {
		if ((cnt = atoi(argv[1])) <= 0 || cnt > 20)
			cnt = 1;
		msk = 0;
		mtch = 0;
	} else if (argc == 4) {
		if ((cnt = atoi(argv[1])) <= 0 || cnt > 20)
			cnt = 1;
		msk = inet_addr(argv[2]);
		mtch = inet_addr(argv[3]);
	} else {
		cnt = 1;
		msk = 0;
		mtch = 0;
	}
	for (i = 1; i < cnt; i++)
		if (fork() == 0)
			break;
	if (nfssvc(sock, msk, mtch) < 0)	/* Only returns on error */
		syslog(LOG_ERR, "nfssvc() failed %m");
	exit();
}
