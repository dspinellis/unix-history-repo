/*
 * Copyright (c) 1989 The Regents of the University of California.
 * All rights reserved.
 *
 * This code is derived from software contributed to Berkeley by
 * Rick Macklem at The University of Guelph.
 *
 * %sccs.include.redist.c%
 */

#ifndef lint
char copyright[] =
"@(#) Copyright (c) 1989 Regents of the University of California.\n\
 All rights reserved.\n";
#endif not lint

#ifndef lint
static char sccsid[] = "@(#)nfsd.c	5.12 (Berkeley) %G%";
#endif not lint

#include <stdio.h>
#include <signal.h>
#include <fcntl.h>
#include <strings.h>
#include <pwd.h>
#include <grp.h>
#include <sys/types.h>
#include <sys/syslog.h>
#include <sys/param.h>
#include <sys/errno.h>
#include <sys/ioctl.h>
#include <sys/stat.h>
#include <sys/wait.h>
#include <sys/uio.h>
#include <sys/namei.h>
#include <sys/ucred.h>
#include <sys/mount.h>
#include <sys/socket.h>
#include <sys/socketvar.h>
#include <rpc/rpc.h>
#include <rpc/pmap_clnt.h>
#include <rpc/pmap_prot.h>
#ifdef ISO
#include <netiso/iso.h>
#endif
#include <nfs/rpcv2.h>
#include <nfs/nfsv2.h>
#include <nfs/nfs.h>
#ifdef KERBEROS
#include <kerberosIV/krb.h>
#endif

/* Global defs */
#ifdef DEBUG
#define	syslog(e, s)	fprintf(stderr,(s))
int	debug = 1;
#else
int	debug = 0;
#endif
struct	nfsd_srvargs nsd;
extern	int errno;
char	**Argv = NULL;		/* pointer to argument vector */
char	*LastArg = NULL;	/* end of argv */
void	reapchild();

#ifdef KERBEROS
char		lnam[ANAME_SZ];
KTEXT_ST	kt;
AUTH_DAT	auth;
char		inst[INST_SZ];
#endif /* KERBEROS */

/*
 * Nfs server daemon mostly just a user context for nfssvc()
 * 1 - do file descriptor and signal cleanup
 * 2 - fork the nfsd(s)
 * 3 - create server socket(s)
 * 4 - register socket with portmap
 * For connectionless protocols, just pass the socket into the kernel via.
 * nfssvc().
 * For connection based sockets, loop doing accepts. When you get a new socket
 * from accept, pass the msgsock into the kernel via. nfssvc().
 * The arguments are:
 * -u - support udp nfs clients
 * -t - support tcp nfs clients
 * -c - support iso cltp clients
 * -r - reregister with portmapper
 * followed by "n" which is the number of nfsds' to fork off
 */
main(argc, argv, envp)
	int argc;
	char *argv[], *envp[];
{
	register int i;
	register char *cp, **cpp;
	register struct ucred *cr = &nsd.nsd_cr;
	struct passwd *pwd;
	struct group *grp;
	int sock, msgsock, tcpflag = 0, udpflag = 0, ret, len;
	int cltpflag = 0, tp4flag = 0, tpipflag = 0, connect_type_cnt = 0;
	int maxsock, tcpsock, tp4sock, tpipsock, nfsdcnt = 4;
	int nfssvc_flag, opt, on = 1, reregister = 0;
	struct sockaddr_in inetaddr, inetpeer;
#ifdef ISO
	struct sockaddr_iso isoaddr, isopeer;
#endif
	struct nfsd_args nfsdargs;
	fd_set ready, sockbits;
	extern int optind;
	extern char *optarg;

	/*
	 *  Save start and extent of argv for setproctitle.
	 */
	Argv = argv;
	if (envp == 0 || *envp == 0)
		envp = argv;
	while (*envp)
		envp++;
	LastArg = envp[-1] + strlen(envp[-1]);
	while ((opt = getopt(argc, argv, "utcr")) != EOF)
		switch (opt) {
		case 'u':
			udpflag++;
			break;
		case 't':
			tcpflag++;
			break;
		case 'r':
			reregister++;
			break;
#ifdef ISO
		case 'c':
			cltpflag++;
			break;
#ifdef notyet
		case 'i':
			tp4cnt++;
			break;
		case 'p':
			tpipcnt++;
			break;
#endif /* notyet */
#endif	/* ISO */
		default:
		case '?':
			usage();
		};
	if (optind < argc)
		nfsdcnt = atoi(argv[optind]);
	if (nfsdcnt < 1 || nfsdcnt > 20)
		nfsdcnt = 4;

	if (debug == 0) {
		daemon(0, 0);
		signal(SIGINT, SIG_IGN);
		signal(SIGQUIT, SIG_IGN);
		signal(SIGTERM, SIG_IGN);
		signal(SIGHUP, SIG_IGN);
	}
	signal(SIGCHLD, reapchild);

	if (reregister) {
		if (udpflag && !pmap_set(RPCPROG_NFS, NFS_VER2, IPPROTO_UDP,
		    NFS_PORT)) {
			fprintf(stderr,
			    "Can't register with portmap for UDP\n");
			exit(1);
		}
		if (tcpflag && !pmap_set(RPCPROG_NFS, NFS_VER2, IPPROTO_TCP,
		    NFS_PORT)) {
			fprintf(stderr,
			    "Can't register with portmap for TCP\n");
			exit(1);
		}
		exit(0);
	}
	openlog("nfsd:", LOG_PID, LOG_DAEMON);

	for (i = 0; i < nfsdcnt; i++)
	    if (fork() == 0) {
		setproctitle("nfsd-srv");
		nfssvc_flag = NFSSVC_NFSD;
		nsd.nsd_nfsd = (struct nfsd *)0;
#ifdef KERBEROS
		nsd.nsd_authstr = (char *)kt.dat;
#endif
		while (nfssvc(nfssvc_flag, (caddr_t)&nsd) < 0) {
		    if (errno == ENEEDAUTH) {
			nfssvc_flag = (NFSSVC_NFSD | NFSSVC_AUTHINFAIL);
#ifdef KERBEROS
			kt.length = nsd.nsd_authlen;
			kt.mbz = 0;
			strcpy(inst, "*");
			if (krb_rd_req(&kt, "rcmd", inst, nsd.nsd_haddr,
			    &auth, "") == RD_AP_OK &&
			    krb_kntoln(&auth, lnam) == KSUCCESS &&
			    (pwd = getpwnam(lnam))) {
			    cr->cr_uid = pwd->pw_uid;
			    cr->cr_groups[0] = pwd->pw_gid;
			    cr->cr_ngroups = 1;
			    setgrent();
			    while (grp = getgrent()) {
				if (grp->gr_gid == cr->cr_groups[0])
				    continue;
				cpp = grp->gr_mem;
				while (*cpp) {
				    if (!strcmp(*cpp, lnam))
					break;
				    cpp++;
				}
				if (*cpp) {
				    cr->cr_groups[cr->cr_ngroups++] = grp->gr_gid;
				    if (cr->cr_ngroups == NGROUPS)
					break;
				}
			    }
			    endgrent();
			    nfssvc_flag = (NFSSVC_NFSD | NFSSVC_AUTHIN);
			}
#endif	/* KERBEROS */
		    } else {
fprintf(stderr, "errno=%d\n",errno);
			syslog(LOG_ERR, "Nfsd died %m");
			exit(1);
		    }
		}
		exit();
	    }

	/*
	 * If we are serving udp, set up the socket.
	 */
	if (udpflag) {
		if ((sock = socket(AF_INET, SOCK_DGRAM, 0)) < 0) {
			syslog(LOG_ERR, "Can't create udp socket");
			exit(1);
		}
		inetaddr.sin_family = AF_INET;
		inetaddr.sin_addr.s_addr = INADDR_ANY;
		inetaddr.sin_port = htons(NFS_PORT);
		inetaddr.sin_len = sizeof(inetaddr);
		if (bind(sock, (struct sockaddr *)&inetaddr, sizeof(inetaddr)) < 0) {
			syslog(LOG_ERR, "Can't bind udp addr");
			exit(1);
		}
		if (!pmap_set(RPCPROG_NFS, NFS_VER2, IPPROTO_UDP, NFS_PORT)) {
			syslog(LOG_ERR, "Can't register with udp portmap");
			exit(1);
		}
		nfsdargs.sock = sock;
		nfsdargs.name = (caddr_t)0;
		nfsdargs.namelen = 0;
		if (nfssvc(NFSSVC_ADDSOCK, &nfsdargs) < 0) {
			syslog(LOG_ERR, "Can't Add UDP socket");
			exit(1);
		}
		close(sock);
	}

	/*
	 * If we are serving cltp, set up the socket.
	 */
#ifdef ISO
	if (cltpflag) {
		if ((sock = socket(AF_ISO, SOCK_DGRAM, 0)) < 0) {
			syslog(LOG_ERR, "Can't create cltp socket");
			exit(1);
		}
		bzero((caddr_t)&isoaddr, sizeof (isoaddr));
		isoaddr.siso_family = AF_ISO;
		isoaddr.siso_tlen = 2;
		cp = TSEL(&isoaddr);
		*cp++ = (NFS_PORT >> 8);
		*cp = (NFS_PORT & 0xff);
		isoaddr.siso_len = sizeof(isoaddr);
		if (bind(sock, (struct sockaddr *)&isoaddr, sizeof(isoaddr)) < 0) {
			syslog(LOG_ERR, "Can't bind cltp addr");
			exit(1);
		}
#ifdef notyet
		/*
		 * Someday this should probably use "rpcbind", the son of
		 * portmap.
		 */
		if (!pmap_set(RPCPROG_NFS, NFS_VER2, IPPROTO_UDP, NFS_PORT)) {
			syslog(LOG_ERR, "Can't register with udp portmap");
			exit(1);
		}
#endif /* notyet */
		nfsdargs.sock = sock;
		nfsdargs.name = (caddr_t)0;
		nfsdargs.namelen = 0;
		if (nfssvc(NFSSVC_ADDSOCK, &nfsdargs) < 0) {
			syslog(LOG_ERR, "Can't Add UDP socket");
			exit();
		}
		close(sock);
	}
#endif	/* ISO */

	/*
	 * Now set up the master server socket waiting for tcp connections.
	 */
	FD_ZERO(&sockbits);
	if (tcpflag) {
		if ((tcpsock = socket(AF_INET, SOCK_STREAM, 0)) < 0) {
			syslog(LOG_ERR, "Can't create tcp socket");
			exit(1);
		}
		if (setsockopt(tcpsock, SOL_SOCKET, SO_REUSEADDR,
		    (char *) &on, sizeof(on)) < 0)
			syslog(LOG_ERR, "setsockopt SO_REUSEADDR: %m");
		inetaddr.sin_family = AF_INET;
		inetaddr.sin_addr.s_addr = INADDR_ANY;
		inetaddr.sin_port = htons(NFS_PORT);
		inetaddr.sin_len = sizeof (inetaddr);
		if (bind(tcpsock, (struct sockaddr *)&inetaddr, sizeof (inetaddr)) < 0) {
			syslog(LOG_ERR, "Can't bind tcp addr");
			exit(1);
		}
		if (listen(tcpsock, 5) < 0) {
			syslog(LOG_ERR, "Listen failed");
			exit(1);
		}
		if (!pmap_set(RPCPROG_NFS, NFS_VER2, IPPROTO_TCP, NFS_PORT)) {
			syslog(LOG_ERR, "Can't register tcp with portmap");
			exit(1);
		}
		FD_SET(tcpsock, &sockbits);
		maxsock = tcpsock;
		connect_type_cnt++;
	}

#ifdef notyet
	/*
	 * Now set up the master server socket waiting for tp4 connections.
	 */
	if (tp4flag) {
		if ((tp4sock = socket(AF_ISO, SOCK_SEQPACKET, 0)) < 0) {
			syslog(LOG_ERR, "Can't create tp4 socket");
			exit(1);
		}
		if (setsockopt(tp4sock, SOL_SOCKET, SO_REUSEADDR,
		    (char *) &on, sizeof(on)) < 0)
			syslog(LOG_ERR, "setsockopt SO_REUSEADDR: %m");
		bzero((caddr_t)&isoaddr, sizeof (isoaddr));
		isoaddr.siso_family = AF_ISO;
		isoaddr.siso_tlen = 2;
		cp = TSEL(&isoaddr);
		*cp++ = (NFS_PORT >> 8);
		*cp = (NFS_PORT & 0xff);
		isoaddr.siso_len = sizeof(isoaddr);
		if (bind(tp4sock, (struct sockaddr *)&isoaddr, sizeof (isoaddr)) < 0) {
			syslog(LOG_ERR, "Can't bind tp4 addr");
			exit(1);
		}
		if (listen(tp4sock, 5) < 0) {
			syslog(LOG_ERR, "Listen failed");
			exit(1);
		}
		/*
		 * Someday this should probably use "rpcbind".
		 */
		if (!pmap_set(RPCPROG_NFS, NFS_VER2, IPPROTO_TCP, NFS_PORT)) {
			syslog(LOG_ERR, "Can't register tcp with portmap");
			exit(1);
		}
		FD_SET(tp4sock, &sockbits);
		maxsock = tp4sock;
		connect_type_cnt++;
	}

	/*
	 * Now set up the master server socket waiting for tpip connections.
	 */
	if (tpipflag) {
		if ((tpipsock = socket(AF_INET, SOCK_SEQPACKET, 0)) < 0) {
			syslog(LOG_ERR, "Can't create tpip socket");
			exit(1);
		}
		if (setsockopt(tpipsock, SOL_SOCKET, SO_REUSEADDR,
		    (char *) &on, sizeof(on)) < 0)
			syslog(LOG_ERR, "setsockopt SO_REUSEADDR: %m");
		inetaddr.sin_family = AF_INET;
		inetaddr.sin_addr.s_addr = INADDR_ANY;
		inetaddr.sin_port = htons(NFS_PORT);
		inetaddr.sin_len = sizeof (inetaddr);
		if (bind(tpipsock, (struct sockaddr *)&inetaddr, sizeof (inetaddr)) < 0) {
			syslog(LOG_ERR, "Can't bind tcp addr");
			exit(1);
		}
		if (listen(tpipsock, 5) < 0) {
			syslog(LOG_ERR, "Listen failed");
			exit(1);
		}
		/*
		 * Someday this should use "rpcbind"
		 */
		if (!pmap_set(RPCPROG_NFS, NFS_VER2, IPPROTO_TCP, NFS_PORT)) {
			syslog(LOG_ERR, "Can't register tcp with portmap");
			exit(1);
		}
		FD_SET(tpipsock, &sockbits);
		maxsock = tpipsock;
		connect_type_cnt++;
	}
#endif	/* notyet */

	if (connect_type_cnt == 0)
		exit();
	setproctitle("nfsd-master");
	/*
	 * Loop forever accepting connections and passing the sockets
	 * into the kernel for the mounts.
	 */
	for (;;) {
		ready = sockbits;
		if (connect_type_cnt > 1) {
			if (select(maxsock + 1, &ready, (fd_set *)0,
				(fd_set *)0, (struct timeval *)0) < 1) {
				syslog(LOG_ERR, "Select failed");
				exit(1);
			}
		}
		if (tcpflag && FD_ISSET(tcpsock, &ready)) {
			len = sizeof(inetpeer);
			if ((msgsock = accept(tcpsock,
			    (struct sockaddr *)&inetpeer, &len)) < 0) {
				syslog(LOG_ERR, "Accept failed: %m");
				exit(1);
			}
			bzero((char *)inetpeer.sin_zero,
			    sizeof(inetpeer.sin_zero));
			if (setsockopt(msgsock, SOL_SOCKET,
			    SO_KEEPALIVE, (char *) &on, sizeof(on)) < 0)
				syslog(LOG_ERR,
				    "setsockopt SO_KEEPALIVE: %m");
			nfsdargs.sock = msgsock;
			nfsdargs.name = (caddr_t)&inetpeer;
			nfsdargs.namelen = sizeof(inetpeer);
			nfssvc(NFSSVC_ADDSOCK, &nfsdargs);
			close(msgsock);
		}
#ifdef notyet
		if (tp4flag && FD_ISSET(tp4sock, &ready)) {
			len = sizeof(isopeer);
			if ((msgsock = accept(tp4sock,
			    (struct sockaddr *)&isopeer, &len)) < 0) {
				syslog(LOG_ERR, "Accept failed: %m");
				exit(1);
			}
			if (setsockopt(msgsock, SOL_SOCKET,
			    SO_KEEPALIVE, (char *) &on, sizeof(on)) < 0)
				syslog(LOG_ERR,
				    "setsockopt SO_KEEPALIVE: %m");
			nfsdargs.sock = msgsock;
			nfsdargs.name = (caddr_t)&isopeer;
			nfsdargs.namelen = len;
			nfssvc(NFSSVC_ADDSOCK, &nfsdargs);
			close(msgsock);
		}
		if (tpipflag && FD_ISSET(tpipsock, &ready)) {
			len = sizeof(inetpeer);
			if ((msgsock = accept(tpipsock,
			    (struct sockaddr *)&inetpeer, &len)) < 0) {
				syslog(LOG_ERR, "Accept failed: %m");
				exit(1);
			}
			if (setsockopt(msgsock, SOL_SOCKET,
			    SO_KEEPALIVE, (char *) &on, sizeof(on)) < 0)
				syslog(LOG_ERR,
				    "setsockopt SO_KEEPALIVE: %m");
			nfsdargs.sock = msgsock;
			nfsdargs.name = (caddr_t)&inetpeer;
			nfsdargs.namelen = len;
			nfssvc(NFSSVC_ADDSOCK, &nfsdargs);
			close(msgsock);
		}
#endif	/* notyet */
	}
}

usage()
{
	fprintf(stderr, "nfsd [-u] [-t] [-c] [-r] [num_nfsds]\n");
	exit(1);
}

void
reapchild()
{

	while (wait3((int *) NULL, WNOHANG, (struct rusage *) NULL))
		;
}

setproctitle(a)
	char *a;
{
	register char *cp;
	char buf[80];

	cp = Argv[0];
	(void) sprintf(buf, "%s", a);
	(void) strncpy(cp, buf, LastArg - cp);
	cp += strlen(cp);
	while (cp < LastArg)
		*cp++ = ' ';
}
