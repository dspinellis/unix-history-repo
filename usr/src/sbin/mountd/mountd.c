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
static char sccsid[] = "@(#)mountd.c	5.7 (Berkeley) %G%";
#endif not lint

#include <sys/param.h>
#include <sys/ioctl.h>
#include <sys/stat.h>
#include <sys/file.h>
#include <sys/mount.h>
#include <sys/socket.h>
#include <sys/errno.h>
#include <sys/signal.h>
#include <stdio.h>
#include <string.h>
#include <syslog.h>
#include <netdb.h>
#include <rpc/rpc.h>
#include <rpc/pmap_clnt.h>
#include <rpc/pmap_prot.h>
#include <nfs/rpcv2.h>
#include <nfs/nfsv2.h>
#include "pathnames.h"

struct ufid {
	u_short	ufid_len;
	ino_t	ufid_ino;
	long	ufid_gen;
};
/*
 * Structures for keeping the mount list and export list
 */
struct mountlist {
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
	struct hostent *gr_hp;
};

/* Global defs */
int xdr_fhs(), xdr_mlist(), xdr_dir(), xdr_explist();
int mntsrv(), get_exportlist();
struct exportlist exphead;
int mlfile;
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
 * NFS: Network File System Protocol Specification, RFC1094, Appendix A
 * The optional argument is the exports file name
 * default: _PATH_EXPORTS
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
	exphead.ex_next = exphead.ex_prev = (struct exportlist *)0;
	if (argc == 2) {
		strncpy(exname, argv[1], MAXPATHLEN-1);
		exname[MAXPATHLEN-1] = '\0';
	} else
		strcpy(exname, _PATH_EXPORTS);
	get_exportlist();
	signal(SIGHUP, get_exportlist);
	{ FILE *pidfile = fopen(_PATH_MOUNTDPID, "w");
	  if (pidfile != NULL) {
		fprintf(pidfile, "%d\n", getpid());
		fclose(pidfile);
	  }
	}
	if ((mlfile = open(_PATH_RMOUNTLIST, (O_RDWR|O_CREAT), 0600)) < 0) {
		syslog(LOG_ERR, "Can't open mountlist file");
		exit(1);
	}
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
	register struct grouplist *grp;
	register u_long **addrp;
	register struct exportlist *ep;
	struct mountlist ml;
	nfsv2fh_t nfh;
	struct authunix_parms *ucr;
	struct stat stb;
	struct hostent *hp;
	u_long saddr;
	char dirpath[RPCMNT_PATHLEN+1];
	int ok = 0;
	int bad = ENOENT;
	int omask;
	uid_t uid = -2;

	/* Get authorization */
	switch (rqstp->rq_cred.oa_flavor) {
	case AUTH_UNIX:
		ucr = (struct authunix_parms *)rqstp->rq_clntcred;
		uid = ucr->aup_uid;
		break;
	case AUTH_NULL:
	default:
		break;
	}

	saddr = transp->xp_raddr.sin_addr.s_addr;
	hp = (struct hostent *)0;
	switch (rqstp->rq_proc) {
	case NULLPROC:
		if (!svc_sendreply(transp, xdr_void, (caddr_t)0))
			syslog(LOG_ERR, "Can't send reply");
		return;
	case RPCMNT_MOUNT:
		if (uid != 0) {
			svcerr_weakauth(transp);
			return;
		}
		if (!svc_getargs(transp, xdr_dir, dirpath)) {
			svcerr_decode(transp);
			return;
		}

		/* Check to see if it's a valid dirpath */
		if (stat(dirpath, &stb) < 0 || (stb.st_mode&S_IFMT) !=
			S_IFDIR) {
			if (!svc_sendreply(transp, xdr_long, (caddr_t)&bad))
				syslog(LOG_ERR, "Can't send reply");
			return;
		}

		/* Check in the exports list */
		omask = sigblock(sigmask(SIGHUP));
		ep = exphead.ex_next;
		while (ep != NULL) {
			if (!strcmp(ep->ex_dirp, dirpath)) {
				grp = ep->ex_groups;
				if (grp == NULL)
					break;

				/* Check for a host match */
				addrp = (u_long **)grp->gr_hp->h_addr_list;
				for (;;) {
					if (**addrp == saddr)
						break;
					if (*++addrp == NULL)
						if (grp = grp->gr_next) {
							addrp = (u_long **)
								grp->gr_hp->h_addr_list;
						} else {
							bad = EACCES;
							if (!svc_sendreply(transp, xdr_long, (caddr_t)&bad))
								syslog(LOG_ERR, "Can't send reply");
							sigsetmask(omask);
							return;
						}
				}
				hp = grp->gr_hp;
				break;
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

		/* Get the file handle */
		bzero((caddr_t)&nfh, sizeof(nfh));
		if (getfh(dirpath, (fhandle_t *)&nfh) < 0) {
			bad = errno;
			if (!svc_sendreply(transp, xdr_long, (caddr_t)&bad))
				syslog(LOG_ERR, "Can't send reply");
			return;
		}
#ifdef notnow
nfh.fh_generic.fh_fid.fid_gen = htonl(nfh.fh_generic.fh_fid.fid_gen);
#endif
		if (!svc_sendreply(transp, xdr_fhs, (caddr_t)&nfh))
			syslog(LOG_ERR, "Can't send reply");
		if (hp == NULL)
			hp = gethostbyaddr((caddr_t)&saddr, sizeof(saddr), AF_INET);
		if (hp) {
			if (!fnd_mnt(hp->h_name, dirpath)) {
				strcpy(ml.ml_host, hp->h_name);
				strcpy(ml.ml_dirp, dirpath);
				write(mlfile, (caddr_t)&ml, sizeof(ml));
			}
		}
		return;
	case RPCMNT_DUMP:
		if (!svc_sendreply(transp, xdr_mlist, (caddr_t)0))
			syslog(LOG_ERR, "Can't send reply");
		return;
	case RPCMNT_UMOUNT:
		if (uid != 0) {
			svcerr_weakauth(transp);
			return;
		}
		if (!svc_getargs(transp, xdr_dir, dirpath)) {
			svcerr_decode(transp);
			return;
		}
		hp = gethostbyaddr((caddr_t)&saddr, sizeof(saddr), AF_INET);
		if (hp && fnd_mnt(hp->h_name, dirpath)) {
			ml.ml_host[0] = '\0';
			write(mlfile, (caddr_t)&ml, sizeof(ml));
		}
		if (!svc_sendreply(transp, xdr_void, (caddr_t)0))
			syslog(LOG_ERR, "Can't send reply");
		return;
	case RPCMNT_UMNTALL:
		if (uid != 0) {
			svcerr_weakauth(transp);
			return;
		}
		hp = gethostbyaddr((caddr_t)&saddr, sizeof(saddr), AF_INET);
		if (hp) {
			lseek(mlfile, (off_t)0, L_SET);
			while (read(mlfile, (caddr_t)&ml, sizeof(ml)) ==
				sizeof(ml)) {
				if (!strcmp(hp->h_name, ml.ml_host)) {
					lseek(mlfile, (off_t)-sizeof(ml),
						L_INCR);
					ml.ml_host[0] = '\0';
					write(mlfile, (caddr_t)&ml, sizeof(ml));
				}
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
	return (xdr_opaque(xdrsp, (caddr_t)nfh, NFSX_FH));
}

xdr_mlist(xdrsp, cp)
	XDR *xdrsp;
	caddr_t cp;
{
	struct mountlist ml;
	int true = 1;
	int false = 0;
	char *strp;

	lseek(mlfile, (off_t)0, L_SET);
	while (read(mlfile, (caddr_t)&ml, sizeof(ml)) == sizeof(ml)) {
		if (ml.ml_host[0] != '\0') {
			if (!xdr_bool(xdrsp, &true))
				return (0);
			strp = &ml.ml_host[0];
			if (!xdr_string(xdrsp, &strp, RPCMNT_NAMELEN))
				return (0);
			strp = &ml.ml_dirp[0];
			if (!xdr_string(xdrsp, &strp, RPCMNT_PATHLEN))
				return (0);
		}
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
			strp = grp->gr_hp->h_name;
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
	register struct hostent *hp, *nhp;
	register char **addrp, **naddrp;
	register int i;
	register struct grouplist *grp, *grp2;
	register struct exportlist *ep, *ep2;
	struct ufs_args args;
	FILE *inf;
	char *cp, *endcp;
	char savedc;
	int len;
	int rootuid, exflags;

	/*
	 * First, get rid of the old list
	 */
	ep = exphead.ex_next;
	while (ep != NULL) {
		grp = ep->ex_groups;
		while (grp != NULL) {
			addrp = grp->gr_hp->h_addr_list;
			while (*addrp)
				free(*addrp++);
			free((caddr_t)grp->gr_hp->h_addr_list);
			free(grp->gr_hp->h_name);
			free((caddr_t)grp->gr_hp);
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
		exflags = MNT_EXPORTED;
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
			savedc = *endcp;
			*endcp = '\0';
			if (len <= RPCMNT_NAMELEN) {
				if (*cp == '-') {
					cp++;
					switch (*cp) {
					case 'o':
						exflags |= MNT_EXRDONLY;
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
				} else if (hp = gethostbyname(cp)) {
					grp = (struct grouplist *)
						malloc(sizeof(struct grouplist));
					if (grp == NULL)
						goto err;
					nhp = grp->gr_hp = (struct hostent *)
						malloc(sizeof(struct hostent));
					if (nhp == NULL)
						goto err;
					bcopy((caddr_t)hp, (caddr_t)nhp,
						sizeof(struct hostent));
					i = strlen(hp->h_name)+1;
					nhp->h_name = (char *)malloc(i);
					if (nhp->h_name == NULL)
						goto err;
					bcopy(hp->h_name, nhp->h_name, i);
					addrp = hp->h_addr_list;
					i = 1;
					while (*addrp++)
						i++;
					naddrp = nhp->h_addr_list = (char **)
						malloc(i*sizeof(char *));
					if (naddrp == NULL)
						goto err;
					addrp = hp->h_addr_list;
					while (*addrp) {
						*naddrp = (char *)
						    malloc(hp->h_length);
						bcopy(*addrp, *naddrp,
							hp->h_length);
						addrp++;
						naddrp++;
					}
					*naddrp = (char *)0;
					grp->gr_next = ep->ex_groups;
					ep->ex_groups = grp;
				}
			}
			cp = endcp;
			*cp = savedc;
			nextfield(&cp, &endcp);
			len = endcp-cp;
		}
		args.fspec = 0;
		args.exflags = exflags;
		args.exroot = rootuid;
		if (mount(MOUNT_UFS, ep->ex_dirp, MNT_UPDATE, &args) < 0) {
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
	syslog(LOG_ERR, "Bad Exports File, mountd Failed");
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

/*
 * Search the remote mounts file for a match.
 * Iff found
 *	- set file offset to record and return TRUE
 * else
 *	- set file offset to an unused record if found or eof otherwise
 *	  and return FALSE
 */
fnd_mnt(host, dirp)
	char *host;
	char *dirp;
{
	struct mountlist ml;
	off_t off, pos;

	off = -1;
	pos = 0;
	lseek(mlfile, (off_t)0, L_SET);
	while (read(mlfile, (caddr_t)&ml, sizeof(ml)) == sizeof(ml)) {
		if (!strcmp(host, ml.ml_host) && !strcmp(dirp, ml.ml_dirp)) {
			lseek(mlfile, (off_t)-sizeof(ml), L_INCR);
			return (TRUE);
		} else if (ml.ml_host[0] == '\0') {
			off = pos;
		}
		pos += sizeof(ml);
	}
	if (off != -1)
		lseek(mlfile, off, L_SET);
	else
		lseek(mlfile, (off_t)0, L_XTND);
	return (FALSE);
}
