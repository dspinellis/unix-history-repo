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
static char sccsid[] = "@(#)mountd.c	5.1 (Berkeley) %G%";
#endif not lint

#include <stdio.h>
#include <strings.h>
#include <syslog.h>
#include <signal.h>
#include <fcntl.h>
#include <sys/param.h>
#include <sys/types.h>
#include <sys/ioctl.h>
#include <sys/stat.h>
#include <sys/dir.h>
#include <sys/uio.h>
#include <sys/namei.h>
#include <sys/mount.h>
#include <sys/socket.h>
#include <sys/socketvar.h>
#include <sys/errno.h>
#include <netdb.h>
#include <rpc/rpc.h>
#include <rpc/pmap_clnt.h>
#include <rpc/pmap_prot.h>
#include <nfs/rpcv2.h>
#include <nfs/nfsv2.h>

struct ufid {
	u_short	ufid_len;
	ino_t	ufid_ino;
	long	ufid_gen;
};
/*
 * Structures for keeping the mount list and export list
 */
struct mountlist {
	struct mountlist *ml_next;
	struct mountlist *ml_prev;
	char	ml_host[RPCMNT_NAMELEN+1];
	char	ml_dirp[RPCMNT_PATHLEN+1];
};

struct exportlist {
	struct exportlist *ex_next;
	struct exportlist *ex_prev;
	struct grouplist *ex_groups;
	int	ex_rootuid;
	int	ex_exflags;
	char	ex_dirp[RPCMNT_PATHLEN+1];
};

struct grouplist {
	struct grouplist *gr_next;
	char	gr_name[RPCMNT_NAMELEN+1];
};

/* Global defs */
int xdr_fhs(), xdr_mlist(), xdr_dir(), xdr_explist();
int mntsrv(), get_exportlist();
struct exportlist exphead;
struct mountlist mlhead;
char exname[MAXPATHLEN];
int def_rootuid = -2;
extern int errno;
#ifdef DEBUG
int debug = 1;
#else
int debug = 0;
#endif

/*
 * Mountd server for NFS mount protocol as described in:
 *  Networking on the Sun Workstation,
 *  Part #800-1324-03 Rev. B
 *  Network File System Protocol Specification Chap. 3
 * The optional argument is the exports file name
 * default: /etc/exports
 */
main(argc, argv)
	int argc;
	char *argv[];
{
	SVCXPRT *transp;

	if (debug == 0) {
		if (fork())
			exit(0);
		{ int s;
		for (s = 0; s < 10; s++)
			(void) close(s);
		}
		(void) open("/", O_RDONLY);
		(void) dup2(0, 1);
		(void) dup2(0, 2);
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
	}
	openlog("mountd:", LOG_PID, LOG_DAEMON);
	mlhead.ml_next = mlhead.ml_prev = (struct mountlist *)0;
	exphead.ex_next = exphead.ex_prev = (struct exportlist *)0;
	if (argc == 2) {
		strncpy(exname, argv[1], MAXPATHLEN-1);
		exname[MAXPATHLEN-1] = '\0';
	} else
		strcpy(exname, "/etc/exports");
	get_exportlist();
	signal(SIGHUP, get_exportlist);
	if ((transp = svcudp_create(RPC_ANYSOCK)) == NULL) {
		syslog(LOG_ERR, "Can't create socket");
		exit(1);
	}
	pmap_unset(RPCPROG_MNT, RPCMNT_VER1);
	if (!svc_register(transp, RPCPROG_MNT, RPCMNT_VER1, mntsrv, IPPROTO_UDP)) {
		syslog(LOG_ERR, "Can't register mount");
		exit(1);
	}
	svc_run();
	syslog(LOG_ERR, "Mountd died");
}

/*
 * The mount rpc service
 */
mntsrv(rqstp, transp)
	register struct svc_req *rqstp;
	register SVCXPRT *transp;
{
	register struct mountlist *mlp;
	register struct exportlist *ep;
	register struct grouplist *grp;
	struct mountlist *mlp2;
	nfsv2fh_t nfh;
	struct authunix_parms *ucr;
	struct stat stb;
	struct hostent *hp;
	struct sockaddr_in saddr;
	char dirpath[RPCMNT_PATHLEN+1];
	int ok = 0;
	int bad = ENOENT;
	int omask;

fprintf(stderr,"in mntsrv\n");
	if (rqstp->rq_proc == NULLPROC) {
		if (!svc_sendreply(transp, xdr_void, (caddr_t)0))
			syslog(LOG_ERR, "Can't send reply");
		return;
	}

	/* Get authorization */
	switch (rqstp->rq_cred.oa_flavor) {
	case AUTH_UNIX:
		ucr = (struct authunix_parms *)rqstp->rq_clntcred;
		if (ucr->aup_uid == 0)
			break;
		/* Fall thru to */
fprintf(stderr,"weak auth\n");
	case AUTH_NULL:
	default:
		svcerr_weakauth(transp);
		return;
	}

	saddr.sin_family = AF_INET;
	saddr.sin_addr.s_addr = ntohl(transp->xp_raddr.sin_addr.s_addr);
	saddr.sin_port = 0;
	hp = gethostbyaddr((caddr_t)&saddr, transp->xp_addrlen, AF_INET);
fprintf(stderr,"net_addr=0x%x\n",transp->xp_raddr.sin_addr.s_addr);
fprintf(stderr,"aft gethost hp=0x%x\n",hp);
	switch (rqstp->rq_proc) {
	case RPCMNT_MOUNT:
fprintf(stderr,"in mnt req\n");
		if (!svc_getargs(transp, xdr_dir, dirpath)) {
			svcerr_decode(transp);
			return;
		}

fprintf(stderr,"dirpath=%s\n",dirpath);
		/* If no hostname, return err */
#ifdef notdef
		if (hp == NULL) {
			if (!svc_sendreply(transp, xdr_long, (caddr_t)&bad))
				syslog(LOG_ERR, "Can't send reply");
			return;
		}

#endif
		/* Check to see if it's a valid dirpath */
		if (stat(dirpath, &stb) < 0 || (stb.st_mode&S_IFMT) !=
			S_IFDIR) {
			if (!svc_sendreply(transp, xdr_long, (caddr_t)&bad))
				syslog(LOG_ERR, "Can't send reply");
			return;
		}

fprintf(stderr,"Look in exports list\n");
		/* Check in the exports list */
		omask = sigblock(sigmask(SIGHUP));
		ep = exphead.ex_next;
		while (ep != NULL) {
			if (!strcmp(ep->ex_dirp, dirpath)) {
				grp = ep->ex_groups;
				if (grp == NULL)
					break;
				while (grp != NULL) {
					if (!strcmp(grp->gr_name, hp->h_name))
						break;
					grp = grp->gr_next;
				}
				bad = EACCES;
				if (!svc_sendreply(transp, xdr_long, (caddr_t)&bad))
					syslog(LOG_ERR, "Can't send reply");
				sigsetmask(omask);
				return;
			}
			ep = ep->ex_next;
		}
		sigsetmask(omask);
		if (ep == NULL) {
			bad = EACCES;
			if (!svc_sendreply(transp, xdr_long, (caddr_t)&bad))
				syslog(LOG_ERR, "Can't send reply");
			return;
		}

fprintf(stderr,"get file handle\n");
		/* Get the file handle */
		bzero((caddr_t)&nfh, sizeof(nfh));
		if (getfh(dirpath, (fhandle_t *)&nfh) < 0) {
			bad = errno;
			if (!svc_sendreply(transp, xdr_long, (caddr_t)&bad))
				syslog(LOG_ERR, "Can't send reply");
			return;
		}
{ struct ufid *ufp;
ufp = (struct ufid *)&nfh.fh_generic;
fprintf(stderr,"ftyp=%d fnum=%d\n",nfh.fh_generic.fh_fsid.val[1],
nfh.fh_generic.fh_fsid.val[0]);
fprintf(stderr,"fid num=%d gen=%d\n",ufp->ufid_ino,ufp->ufid_gen);
}
		if (!svc_sendreply(transp, xdr_fhs, (caddr_t)&nfh))
			syslog(LOG_ERR, "Can't send reply");
		mlp = (struct mountlist *)malloc(sizeof(struct mountlist));
fprintf(stderr,"add to list\n");
#ifdef notdef
		if (mlp != NULL) {
			strcpy(mlp->ml_host, hp->h_name);
			strcpy(mlp->ml_dirp, dirpath);
			mlp->ml_prev = &mlhead;
			mlp->ml_next = mlhead.ml_next;
			if (mlhead.ml_next != NULL)
				mlhead.ml_next->ml_prev = mlp;
			mlhead.ml_next = mlp;
		}
#endif
		return;
	case RPCMNT_DUMP:
		if (!svc_sendreply(transp, xdr_mlist, (caddr_t)0))
			syslog(LOG_ERR, "Can't send reply");
		return;
	case RPCMNT_UMOUNT:
		if (!svc_getargs(transp, xdr_dir, dirpath)) {
			svcerr_decode(transp);
			return;
		}
		if (hp != NULL) {
			mlp = mlhead.ml_next;
			while (mlp != NULL) {
				if (!strcmp(mlp->ml_host, hp->h_name) &&
				    !strcmp(mlp->ml_dirp, dirpath)) {
					mlp->ml_prev->ml_next = mlp->ml_next;
					if (mlp->ml_next != NULL)
						mlp->ml_next->ml_prev =
						   mlp->ml_prev;
					free((caddr_t)mlp);
					break;
				}
				mlp = mlp->ml_next;
			}
		}
		if (!svc_sendreply(transp, xdr_void, (caddr_t)0))
			syslog(LOG_ERR, "Can't send reply");
		return;
	case RPCMNT_UMNTALL:
		if (hp != NULL) {
			mlp = mlhead.ml_next;
			while (mlp != NULL) {
				if (!strcmp(mlp->ml_host, hp->h_name)) {
					mlp2 = mlp;
					mlp->ml_prev->ml_next = mlp->ml_next;
					if (mlp->ml_next != NULL)
						mlp->ml_next->ml_prev =
						   mlp->ml_prev;
					mlp = mlp->ml_next;
					free((caddr_t)mlp2);
				} else
					mlp = mlp->ml_next;
			}
		}
		if (!svc_sendreply(transp, xdr_void, (caddr_t)0))
			syslog(LOG_ERR, "Can't send reply");
		return;
	case RPCMNT_EXPORT:
		if (!svc_sendreply(transp, xdr_explist, (caddr_t)0))
			syslog(LOG_ERR, "Can't send reply");
		return;
	default:
		svcerr_noproc(transp);
		return;
	}
}

/*
 * Xdr conversion for a dirpath string
 */
xdr_dir(xdrsp, dirp)
	XDR *xdrsp;
	char *dirp;
{
	return (xdr_string(xdrsp, &dirp, RPCMNT_PATHLEN));
}

/*
 * Xdr routine to generate fhstatus
 */
xdr_fhs(xdrsp, nfh)
	XDR *xdrsp;
	nfsv2fh_t *nfh;
{
	int ok = 0;

	if (!xdr_long(xdrsp, &ok))
		return (0);
fprintf(stderr,"eo xdr_fhs\n");
	return (xdr_opaque(xdrsp, (caddr_t)nfh, NFSX_FH));
}

xdr_mlist(xdrsp, cp)
	XDR *xdrsp;
	caddr_t cp;
{
	register struct mountlist *mlp;
	int true = 1;
	int false = 0;
	char *strp;

	mlp = mlhead.ml_next;
	while (mlp != NULL) {
		if (!xdr_bool(xdrsp, &true))
			return (0);
		strp = &mlp->ml_host[0];
		if (!xdr_string(xdrsp, &strp, RPCMNT_NAMELEN))
			return (0);
		strp = &mlp->ml_dirp[0];
		if (!xdr_string(xdrsp, &strp, RPCMNT_PATHLEN))
			return (0);
		mlp = mlp->ml_next;
	}
	if (!xdr_bool(xdrsp, &false))
		return (0);
	return (1);
}

/*
 * Xdr conversion for export list
 */
xdr_explist(xdrsp, cp)
	XDR *xdrsp;
	caddr_t cp;
{
	register struct exportlist *ep;
	register struct grouplist *grp;
	int true = 1;
	int false = 0;
	char *strp;
	int omask;

	omask = sigblock(sigmask(SIGHUP));
	ep = exphead.ex_next;
	while (ep != NULL) {
		if (!xdr_bool(xdrsp, &true))
			goto errout;
		strp = &ep->ex_dirp[0];
		if (!xdr_string(xdrsp, &strp, RPCMNT_PATHLEN))
			goto errout;
		grp = ep->ex_groups;
		while (grp != NULL) {
			if (!xdr_bool(xdrsp, &true))
				goto errout;
			strp = &grp->gr_name[0];
			if (!xdr_string(xdrsp, &strp, RPCMNT_NAMELEN))
				goto errout;
			grp = grp->gr_next;
		}
		if (!xdr_bool(xdrsp, &false))
			goto errout;
		ep = ep->ex_next;
	}
	sigsetmask(omask);
	if (!xdr_bool(xdrsp, &false))
		return (0);
	return (1);
errout:
	sigsetmask(omask);
	return (0);
}

#define LINESIZ	10240
char line[LINESIZ];

/*
 * Get the export list
 */
get_exportlist()
{
	register struct exportlist *ep, *ep2;
	register struct grouplist *grp, *grp2;
	FILE *inf;
	char *cp, *endcp;
	int len;
	int rootuid, exflags;

	/*
	 * First, get rid of the old list
	 */
	ep = exphead.ex_next;
	while (ep != NULL) {
		grp = ep->ex_groups;
		while (grp != NULL) {
			grp2 = grp;
			grp = grp->gr_next;
			free((caddr_t)grp2);
		}
		ep2 = ep;
		ep = ep->ex_next;
		free((caddr_t)ep2);
	}

	/*
	 * Read in the exports file and build the list, calling
	 * exportfs() as we go along
	 */
	exphead.ex_next = exphead.ex_prev = (struct exportlist *)0;
	if ((inf = fopen(exname, "r")) == NULL) {
		syslog(LOG_ERR, "Can't open %s", exname);
		exit(2);
	}
	while (fgets(line, LINESIZ, inf)) {
		exflags = 0;
		rootuid = def_rootuid;
		cp = line;
		nextfield(&cp, &endcp);
		len = endcp-cp;
		if (len <= RPCMNT_PATHLEN && len > 0) {
			ep = (struct exportlist *)malloc(sizeof(*ep));
			ep->ex_next = ep->ex_prev = (struct exportlist *)0;
			ep->ex_groups = (struct grouplist *)0;
			bcopy(cp, ep->ex_dirp, len);
			ep->ex_dirp[len] = '\0';
		} else
			goto err;
		cp = endcp;
		nextfield(&cp, &endcp);
		len = endcp-cp;
		while (len > 0) {
			if (len <= RPCMNT_NAMELEN) {
				if (*cp == '-') {
					cp++;
					switch (*cp) {
					case 'o':
						exflags |= M_EXRDONLY;
						break;
					case 'r':
						if (*++cp == '=')
							rootuid = atoi(++cp);
						break;
					default:
						syslog(LOG_WARNING,
						  "Bad -%c option in %s",
						  *cp, exname);
						break;
					};
				} else {
					grp = (struct grouplist *)malloc(*grp);
					if (grp == NULL)
						goto err;
					bcopy(cp, grp->gr_name, len);
					grp->gr_name[len] = '\0';
					grp->gr_next = ep->ex_groups;
					ep->ex_groups = grp;
				}
			}
			cp = endcp;
			nextfield(&cp, &endcp);
			len = endcp-cp;
		}
		if (exportfs(ep->ex_dirp, rootuid, exflags) < 0) {
			syslog(LOG_WARNING, "Can't export %s", ep->ex_dirp);
			free((caddr_t)ep);
		} else {
			ep->ex_rootuid = rootuid;
			ep->ex_exflags = exflags;
			ep->ex_next = exphead.ex_next;
			ep->ex_prev = &exphead;
			if (ep->ex_next != NULL)
				ep->ex_next->ex_prev = ep;
			exphead.ex_next = ep;
		}
	}
	fclose(inf);
	return;
err:
	syslog(LOG_ERR, "Bad /etc/exports, mountd Failed");
	exit(2);
}

/*
 * Parse out the next white space separated field
 */
nextfield(cp, endcp)
	char **cp;
	char **endcp;
{
	register char *p;

	p = *cp;
	while (*p == ' ' || *p == '\t')
		p++;
	if (*p == '\n' || *p == '\0') {
		*cp = *endcp = p;
		return;
	}
	*cp = p++;
	while (*p != ' ' && *p != '\t' && *p != '\n' && *p != '\0')
		p++;
	*endcp = p;
}
