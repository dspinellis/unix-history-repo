/*
 * Copyright (c) 1989 The Regents of the University of California.
 * All rights reserved.
 *
 * This code is derived from software contributed to Berkeley by
 * Rick Macklem at The University of Guelph.
 *
 * Redistribution and use in source and binary forms are permitted provided
 * that: (1) source distributions retain this entire copyright notice and
 * comment, and (2) distributions including binaries display the following
 * acknowledgement:  ``This product includes software developed by the
 * University of California, Berkeley and its contributors'' in the
 * documentation or other materials provided with the distribution and in
 * all advertising materials mentioning features or use of this software.
 * Neither the name of the University nor the names of its contributors may
 * be used to endorse or promote products derived from this software without
 * specific prior written permission.
 * THIS SOFTWARE IS PROVIDED ``AS IS'' AND WITHOUT ANY EXPRESS OR IMPLIED
 * WARRANTIES, INCLUDING, WITHOUT LIMITATION, THE IMPLIED WARRANTIES OF
 * MERCHANTABILITY AND FITNESS FOR A PARTICULAR PURPOSE.
 */

#ifndef lint
char copyright[] =
"@(#) Copyright (c) 1989 Regents of the University of California.\n\
 All rights reserved.\n";
#endif not lint

#ifndef lint
static char sccsid[] = "@(#)nfsd.c	5.8 (Berkeley) 6/29/90";
#endif not lint

#include <sys/types.h>
#include <sys/signal.h>
#include <sys/ioctl.h>
#include <sys/stat.h>
#include <sys/wait.h>
#include <sys/mount.h>
#include <sys/socket.h>
#include <sys/socketvar.h>
#include <stdio.h>
#include <syslog.h>
#include <fcntl.h>
#include <string.h>
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
struct hadr {
	u_long	ha_sad;
	struct hadr *ha_next;
};
struct hadr hphead;

/*
 * Nfs server daemon mostly just a user context for nfssvc()
 * 1 - do file descriptor and signal cleanup
 * 2 - create server socket
 * 3 - register socket with portmap
 * For SOCK_DGRAM, just fork children and send them into the kernel
 * by calling nfssvc()
 * For connection based sockets, loop doing accepts. When you get a new socket
 * from accept, fork a child that drops into the kernel via. nfssvc.
 * This child will return from nfssvc when the connection is closed, so
 * just shutdown() and exit().
 * The arguments are:
 * -t - support tcp nfs clients
 * -u - support udp nfs clients
 */
main(argc, argv)
	int argc;
	char **argv;
{
	register int i;
	register char *cp, *cp2;
	register struct hadr *hp;
	int udpcnt, sock, msgsock, tcpflag = 0, udpflag = 0, ret, len;
	char opt;
	union wait chldstat;
	extern int optind;
	extern char *optarg;
	struct sockaddr_in saddr, msk, mtch, peername;

	while ((opt = getopt(argc, argv, "t:u:")) != EOF)
		switch (opt) {
		case 't':
			tcpflag++;
			if (cp = index(optarg, ',')) {
				*cp++ = '\0';
				msk.sin_addr.s_addr = inet_addr(optarg);
				if (msk.sin_addr.s_addr == -1)
					usage();
				if (cp2 = index(cp, ','))
					*cp2++ = '\0';
				mtch.sin_addr.s_addr = inet_addr(cp);
				if (mtch.sin_addr.s_addr == -1)
					usage();
				cp = cp2;
				hphead.ha_next = (struct hadr *)0;
				while (cp) {
					if (cp2 = index(cp, ','))
						*cp2++ = '\0';
					hp = (struct hadr *)
						malloc(sizeof (struct hadr));
					hp->ha_sad = inet_addr(cp);
					if (hp->ha_sad == -1)
						usage();
					hp->ha_next = hphead.ha_next;
					hphead.ha_next = hp;
					cp = cp2;
				}
			} else
				usage();
			break;
		case 'u':
			udpflag++;
			if (cp = index(optarg, ',')) {
				*cp++ = '\0';
				msk.sin_addr.s_addr = inet_addr(optarg);
				if (msk.sin_addr.s_addr == -1)
					usage();
				if (cp2 = index(cp, ','))
					*cp2++ = '\0';
				mtch.sin_addr.s_addr = inet_addr(cp);
				if (mtch.sin_addr.s_addr == -1)
					usage();
				if (cp2)
					udpcnt = atoi(cp2);
				if (udpcnt < 1 || udpcnt > 20)
					udpcnt = 1;
			} else
				usage();
			break;
		default:
		case '?':
			usage();
		};
	if (optind == 1) {
		if (argc > 1)
			udpcnt = atoi(*++argv);
		if (udpcnt < 1 || udpcnt > 20)
			udpcnt = 1;
		msk.sin_addr.s_addr = mtch.sin_addr.s_addr = 0;
		udpflag++;
	}
	if (debug == 0) {
		daemon(0, 0);
		signal(SIGINT, SIG_IGN);
		signal(SIGQUIT, SIG_IGN);
		signal(SIGTERM, SIG_IGN);
		signal(SIGHUP, SIG_IGN);
	}
	openlog("nfsd:", LOG_PID, LOG_DAEMON);
	pmap_unset(RPCPROG_NFS, NFS_VER2);
	if (udpflag) {
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
		if (!pmap_set(RPCPROG_NFS, NFS_VER2, IPPROTO_UDP, NFS_PORT)) {
			syslog(LOG_ERR, "Can't register with portmap");
			exit(1);
		}
	
		/*
		 * Send the nfs datagram servers right down into the kernel
		 */
		for (i = 0; i < udpcnt; i++)
			if (fork() == 0) {
				ret = nfssvc(sock, &msk, sizeof(msk),
						&mtch, sizeof(mtch));
				if (ret < 0)
					syslog(LOG_ERR, "nfssvc() failed %m");
				exit();
			}
		close(sock);
	}

	/*
	 * Now set up the master STREAM server waiting for tcp connections.
	 */
	if (tcpflag) {
		if ((sock = socket(AF_INET, SOCK_STREAM, 0)) < 0) {
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
		if (listen(sock, 5) < 0) {
			syslog(LOG_ERR, "Listen failed");
			exit(1);
		}
		if (!pmap_set(RPCPROG_NFS, NFS_VER2, IPPROTO_TCP, NFS_PORT)) {
			syslog(LOG_ERR, "Can't register with portmap");
			exit(1);
		}
		/*
		 * Loop forever accepting connections and sending the children
		 * into the kernel to service the mounts.
		 */
		for (;;) {
			if ((msgsock = accept(sock, (struct sockaddr *)0,
				(int *)0)) < 0) {
				syslog(LOG_ERR, "Accept failed: %m");
				exit(1);
			}
			/*
			 * Grab child termination status' just so defuncts
			 * are not left lying about.
			 */
			while (wait3(&chldstat, WNOHANG, (struct rusage *)0))
				;
			len = sizeof(peername);
			if (getsockname(msgsock, &peername, &len) < 0) {
				syslog(LOG_ERR, "Getsockname failed\n");
				exit(1);
			}
			if ((peername.sin_addr.s_addr & msk.sin_addr.s_addr)
				!= mtch.sin_addr.s_addr) {
				hp = hphead.ha_next;
				while (hp) {
					if (peername.sin_addr.s_addr ==
						hp->ha_sad)
						break;
					hp = hp->ha_next;
				}
				if (hp == NULL) {
					shutdown(msgsock, 2);
					close(msgsock);
					continue;
				}
			}
			if (fork() == 0) {
				close(sock);
				ret = nfssvc(msgsock, &msk, sizeof(msk),
						&mtch, sizeof(mtch));
				shutdown(msgsock, 2);
				if (ret < 0)
					syslog(LOG_NOTICE,
					    "Nfssvc STREAM Failed");
				exit();
			}
			close(msgsock);
		}
	}
}

usage()
{
	fprintf(stderr, "nfsd [-t msk,mtch[,addrs]] [-u msk,mtch,numprocs]\n");
	exit(1);
}
