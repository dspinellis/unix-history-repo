/*
 * Copyright (c) 1989 The Regents of the University of California.
 * All rights reserved.
 *
 * This code is derived from software contributed to Berkeley by
 * Herb Hasler and Rick Macklem at The University of Guelph.
 *
 * %sccs.include.redist.c%
 */

#ifndef lint
char copyright[] =
"@(#) Copyright (c) 1989 Regents of the University of California.\n\
 All rights reserved.\n";
#endif not lint

#ifndef lint
static char sccsid[] = "@(#)mountd.c	5.15 (Berkeley) %G%";
#endif not lint

#include <pwd.h>
#include <grp.h>
#include <unistd.h>
#include <stdlib.h>
#include <fcntl.h>
#include <sys/param.h>
#include <sys/ioctl.h>
#include <sys/stat.h>
#include <sys/file.h>
#include <sys/ucred.h>
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
#ifdef ISO
#include <netiso/iso.h>
#endif
#include <nfs/rpcv2.h>
#include <nfs/nfsv2.h>
#include "pathnames.h"

#define DEF_NAME "default"

#define MNT_HOST   0
#define MNT_GROUP  1
#define	MNT_ISO    2

struct namelist {
	char name[RPCMNT_NAMELEN+1];
	struct namelist *next;
};
struct namegrp {
	char gname[RPCMNT_NAMELEN+1];
	struct namegrp *next;
	struct namelist *names;
};
/*
 * Structures for keeping the mount list and export list
 */
struct mountlist {
	struct mountlist *ml_next;
	char	ml_host[RPCMNT_NAMELEN+1];
	char	ml_dirp[RPCMNT_PATHLEN+1];
};

struct exportlist {
	struct exportlist *ex_next;
	struct exportlist *ex_prev;
	struct grouplist *ex_groups;
	int	ex_defset;
	char	ex_dirp[RPCMNT_PATHLEN+1];
};

union grouptypes {
	struct hostent *gt_hostent;
	struct groupnames *gt_grpname;
#ifdef ISO
	struct sockaddr_iso *gt_isoaddr;
#endif
};

struct grouplist {
	int type;
	int exflags;
	struct ucred anoncr;
	union grouptypes gr_ptr;
	struct grouplist *gr_next;
};

struct al_mnt {
	struct al_mnt *al_next;
	fsid_t	al_mnted;
};

struct groupnames {
	char gn_name[RPCMNT_NAMELEN+1];
	struct grouplist *gn_glist;
	struct groupnames *gn_next;
};

/* Global defs */
int mntsrv(), umntall_each(), xdr_fhs(), xdr_mlist(), xdr_dir(), xdr_explist();
void get_exportlist(), send_umntall(), nextfield(), do_opt();
void get_mountlist(), add_mlist(), del_mlist(), free_exp(), free_grp();
void get_group(), get_host(), do_group();
char *realpath();
#ifdef ISO
struct iso_addr *iso_addr();
#endif
struct exportlist exphead;
struct mountlist *mlhead;
struct groupnames *grpnames;
char exname[MAXPATHLEN];
struct ucred def_anon = {
	(u_short) 1,
	(uid_t) -2,
	1,
	(gid_t) -2,
};
int root_only = 1;
extern int errno;
struct al_mnt *al_head = (struct al_mnt *)0;
#ifdef DEBUG
int debug = 1;
#else
int debug = 0;
#endif

/*
 * Mountd server for NFS mount protocol as described in:
 * NFS: Network File System Protocol Specification, RFC1094, Appendix A
 * The optional arguments are the exports file name
 * default: _PATH_EXPORTS
 * and "-n" to allow nonroot mount.
 */
main(argc, argv)
	int argc;
	char **argv;
{
	SVCXPRT *transp;
	int c;
	extern int optind;
	extern char *optarg;

	while ((c = getopt(argc, argv, "n")) != EOF)
		switch (c) {
		case 'n':
			root_only = 0;
			break;
		default:
			fprintf(stderr, "Usage: mountd [-n] [export_file]\n");
			exit(1);
		};
	argc -= optind;
	argv += optind;
	grpnames = (struct groupnames *)0;
	exphead.ex_next = exphead.ex_prev = (struct exportlist *)0;
	mlhead = (struct mountlist *)0;
	if (argc == 1) {
		strncpy(exname, *argv, MAXPATHLEN-1);
		exname[MAXPATHLEN-1] = '\0';
	} else
		strcpy(exname, _PATH_EXPORTS);
	openlog("mountd:", LOG_PID, LOG_DAEMON);
	if (debug)
		fprintf(stderr,"Getting export list.\n");
	get_exportlist();
	if (debug)
		fprintf(stderr,"Getting mount list.\n");
	get_mountlist();
	if (debug)
		fprintf(stderr,"Here we go.\n");
	if (debug == 0) {
		daemon(0, 0);
		signal(SIGINT, SIG_IGN);
		signal(SIGQUIT, SIG_IGN);
	}
	signal(SIGHUP, get_exportlist);
	signal(SIGTERM, send_umntall);
	{ FILE *pidfile = fopen(_PATH_MOUNTDPID, "w");
	  if (pidfile != NULL) {
		fprintf(pidfile, "%d\n", getpid());
		fclose(pidfile);
	  }
	}
	if ((transp = svcudp_create(RPC_ANYSOCK)) == NULL) {
		syslog(LOG_ERR, "Can't create socket");
		exit(1);
	}
	pmap_unset(RPCPROG_MNT, RPCMNT_VER1);
	if (!svc_register(transp, RPCPROG_MNT, RPCMNT_VER1, mntsrv,
	    IPPROTO_UDP)) {
		syslog(LOG_ERR, "Can't register mount");
		exit(1);
	}
	svc_run();
	syslog(LOG_ERR, "Mountd died");
	exit(1);
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
	nfsv2fh_t nfh;
	struct authunix_parms *ucr;
	struct stat stb;
	struct hostent *hp;
	u_long saddr;
	char rpcpath[RPCMNT_PATHLEN+1], dirpath[MAXPATHLEN];
	int bad = ENOENT;
	int found, matched;
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
		if ((uid != 0 && root_only) || uid == -2) {
			svcerr_weakauth(transp);
			return;
		}
		if (!svc_getargs(transp, xdr_dir, rpcpath)) {
			svcerr_decode(transp);
			return;
		}

		/*
		 * Get the real pathname and make sure it is a directory
		 * that exists.
		 */
		if (realpath(rpcpath, dirpath) == 0 || stat(dirpath, &stb) < 0
			|| (stb.st_mode&S_IFMT) != S_IFDIR) {
			chdir("/");	/* Just in case realpath doesn't */
			if (debug)
				fprintf(stderr,"stat failed on %s\n",dirpath);
			if (!svc_sendreply(transp, xdr_long, (caddr_t)&bad))
				syslog(LOG_ERR, "Can't send reply");
			return;
		}

		/* Check in the exports list */
		omask = sigblock(sigmask(SIGHUP));
		ep = exphead.ex_next;
		found = FALSE;
		matched = FALSE;
		while (ep != NULL && !found && !matched) {
			struct grouplist *tgrp;
			if (debug)
			    fprintf(stderr,"dirp=[%s]\n",ep->ex_dirp);
			if (!strcmp(ep->ex_dirp, dirpath)) {
			    if (ep->ex_defset)
				grp = (struct grouplist *)0;
			    else
				grp = ep->ex_groups;
			    if (grp == NULL) {
				if (debug)
				    fprintf(stderr,"grp is null\n");
				found = TRUE;
			    } 
			    while (grp && !found) {
				matched = TRUE;
				if (debug)
				    fprintf(stderr,"type = [%d]\n",grp->type);
				if (grp->type == MNT_GROUP) {
				    tgrp = grp->gr_ptr.gt_grpname->gn_glist;
				    if (tgrp)
					addrp = (u_long **)
					   tgrp->gr_ptr.gt_hostent->h_addr_list;
				    while(tgrp && !found) {
					if (debug)
					    fprintf(stderr, "cmp [%d] [%d]\n",
						**addrp,saddr);
					if (**addrp == saddr) {
					    found = TRUE;
					    hp = tgrp->gr_ptr.gt_hostent;
					    break;
					}
					if (*++addrp == NULL) {
					    tgrp = tgrp->gr_next;
					    if (tgrp == NULL)
						break;
					    addrp = (u_long **)tgrp->
						gr_ptr.gt_hostent->h_addr_list;
					}
				    }
				} else if (grp->type == MNT_HOST) {
				    addrp = (u_long **)
					grp->gr_ptr.gt_hostent->h_addr_list;
				    while (*addrp) {
					if (debug)
					    fprintf(stderr, "cmp [%d] [%d]\n",
						**addrp,saddr);
					if (**addrp == saddr) {
					    found = TRUE;
					    hp = grp->gr_ptr.gt_hostent;
					    break;
					}
					addrp++;
				    }
				}
				grp = grp->gr_next;
			    }
			}
			ep = ep->ex_next;
		}
		if (!found) {
			bad = EACCES;
			if (!svc_sendreply(transp, xdr_long, (caddr_t)&bad))
				syslog(LOG_ERR, "Can't send reply");
			sigsetmask(omask);
			return;
		} else {
			/* Get the file handle */
			bzero((caddr_t)&nfh, sizeof(nfh));
			if (getfh(dirpath, (fhandle_t *)&nfh) < 0) {
				bad = errno;
				fprintf(stderr,
				    "Couldn't get file handle for %s.\n",
				    dirpath);
				if (!svc_sendreply(transp, xdr_long,
				    (caddr_t)&bad))
					syslog(LOG_ERR, "Can't send reply");
				sigsetmask(omask);
				return;
			}
			if (!svc_sendreply(transp, xdr_fhs, (caddr_t)&nfh))
				syslog(LOG_ERR, "Can't send reply");
			if (hp == NULL)
				hp = gethostbyaddr((caddr_t)&saddr,
				    sizeof(saddr), AF_INET);
			if (hp)
				add_mlist(hp->h_name, dirpath);
			else
				add_mlist(inet_ntoa(transp->xp_raddr.sin_addr),
					dirpath);
			if (debug)
				fprintf(stderr,"Mount successfull.\n");
		}
		sigsetmask(omask);
		return;
	case RPCMNT_DUMP:
		if (!svc_sendreply(transp, xdr_mlist, (caddr_t)0))
			syslog(LOG_ERR, "Can't send reply");
		return;
	case RPCMNT_UMOUNT:
		if ((uid != 0 && root_only) || uid == -2) {
			svcerr_weakauth(transp);
			return;
		}
		if (!svc_getargs(transp, xdr_dir, dirpath)) {
			svcerr_decode(transp);
			return;
		}
		if (!svc_sendreply(transp, xdr_void, (caddr_t)0))
			syslog(LOG_ERR, "Can't send reply");
		hp = gethostbyaddr((caddr_t)&saddr, sizeof(saddr), AF_INET);
		if (hp)
			del_mlist(hp->h_name, dirpath);
		del_mlist(inet_ntoa(transp->xp_raddr.sin_addr), dirpath);
		return;
	case RPCMNT_UMNTALL:
		if ((uid != 0 && root_only) || uid == -2) {
			svcerr_weakauth(transp);
			return;
		}
		if (!svc_sendreply(transp, xdr_void, (caddr_t)0))
			syslog(LOG_ERR, "Can't send reply");
		hp = gethostbyaddr((caddr_t)&saddr, sizeof(saddr), AF_INET);
		if (hp)
			del_mlist(hp->h_name, (char *)0);
		del_mlist(inet_ntoa(transp->xp_raddr.sin_addr), (char *)0);
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
	register struct mountlist *mlp;
	int true = 1;
	int false = 0;
	char *strp;

	mlp = mlhead;
	while (mlp) {
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
	register struct grouplist *grp, *tgrp;
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
			if (grp->type == MNT_GROUP) {
				tgrp = grp->gr_ptr.gt_grpname->gn_glist;
				while (tgrp) {
					if (!xdr_bool(xdrsp, &true))
						goto errout;
					strp = tgrp->gr_ptr.gt_hostent->h_name;
					if (!xdr_string(xdrsp, &strp,
					    RPCMNT_NAMELEN))
						goto errout;
					tgrp = tgrp->gr_next;
				}
			} else if (grp->type == MNT_HOST) {
				if (!xdr_bool(xdrsp, &true))
					goto errout;
				strp = grp->gr_ptr.gt_hostent->h_name;
				if (!xdr_string(xdrsp, &strp, RPCMNT_NAMELEN))
					goto errout;
			}
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
void
get_exportlist()
{
	struct grouplist *grp, *tgrp;
	struct al_mnt *al_mp, *t_almp;
	register struct exportlist *ep, *ep2;
	struct groupnames *t_gn, *t_gn2;
	struct ucred anoncr;
	FILE *inf;
	char *cp, *endcp;
	char savedc;
	int len, dirplen, def_set;
	int exflags;

	/*
	 * First, get rid of the old list
	 */
	ep = exphead.ex_next;
	while (ep != NULL) {
		ep2 = ep;
		ep = ep->ex_next;
		free_exp(ep2);
	}
	exphead.ex_next = exphead.ex_prev = (struct exportlist *)0;

	t_gn = grpnames;
	while(t_gn != NULL) {
		t_gn2 = t_gn;
		t_gn = t_gn->gn_next;
		free_grp(t_gn2);
	}
	grpnames = (struct groupnames *)0;

	al_mp = al_head;
	while (al_mp) {
		t_almp = al_mp;
		al_mp = al_mp->al_next;
		free((caddr_t)t_almp);
	}
	al_head = (struct al_mnt *)0;

	/*
	 * Read in the exports file and build the list, calling
	 * mount() as we go along to push the export rules into the kernel.
	 */
	if ((inf = fopen(exname, "r")) == NULL) {
		syslog(LOG_ERR, "Can't open %s", exname);
		exit(2);
	}
	while (fgets(line, LINESIZ, inf)) {
		def_set = TRUE;
		if (debug)
			fprintf(stderr,"Got line %s\n",line);
		cp = line;
		nextfield(&cp, &endcp);
		if (*cp == '#')
			goto nextline;
		if (*cp != '/') {
			/* create group listing of names */
			get_group(cp, ep);
			goto nextline;
		}
		exflags = MNT_EXPORTED;
		anoncr = def_anon;

		/*
		 * Create new exports list entry
		 */
		len = endcp-cp;
		if (len <= RPCMNT_PATHLEN && len > 0) {
			/*
			 * See if this directory is already in the list.
			 */
			ep = exphead.ex_next;
			while (ep) {
				if (!strcmp(ep->ex_dirp, cp))
					break;
				ep = ep->ex_next;
			}
			if (ep == (struct exportlist *)0) {
				ep = (struct exportlist *)malloc(sizeof(*ep));
				if (ep == NULL)
					goto err;
				ep->ex_next = (struct exportlist *)0;
				ep->ex_prev = (struct exportlist *)0;
				ep->ex_groups = (struct grouplist *)0;
				ep->ex_defset = FALSE;
				bcopy(cp, ep->ex_dirp, len);
				ep->ex_dirp[len] = '\0';
			}
			dirplen = len;
			if (debug)
				fprintf(stderr, "Making new ep. [%s]\n",
				    ep->ex_dirp);
		} else {
			syslog(LOG_ERR, "Bad Exports File line: %s\n", line);
			goto nextline;
		}
		cp = endcp;
		nextfield(&cp, &endcp);
		len = endcp-cp;
		while (len > 0) {
			savedc = *endcp;
			*endcp = '\0';
			if (len > RPCMNT_NAMELEN)
				goto more;
			if (*cp == '-') {
				do_opt(cp + 1, ep, &exflags, &anoncr);
				exflags |= MNT_EXPORTED;
				def_set = TRUE;
				if (debug)
					fprintf(stderr, "got r=%d, ex=%d\n",
					    anoncr.cr_uid,exflags);
				goto more;
			} else {
				def_set = FALSE;
				if (*cp == '$') {
					do_group(cp + 1, endcp, &grp);
					grp->type = MNT_GROUP;
				} else {
					get_host(cp, endcp, ep, &grp);
				}
				if (grp != NULL) {
					grp->exflags = exflags;
					grp->anoncr = anoncr;
					grp->gr_next = ep->ex_groups;
					ep->ex_groups = grp;
				}
			}
		more:
			cp = endcp;
			*cp = savedc;
			nextfield(&cp, &endcp);
			len = endcp - cp;
		}
		if (def_set == TRUE) {
		    if (ep->ex_defset == TRUE)
			syslog(LOG_ERR, "Default specified again dir:%s\n",
				ep->ex_dirp);
		    else {
			struct hostent *hpe;

			ep->ex_defset = TRUE;
			if (debug)
				fprintf(stderr,"Adding a default entry\n");
			/* add a default group and make the grp list NULL */
			hpe = (struct hostent *)malloc(sizeof(struct hostent));
			if (hpe == NULL) {
				syslog(LOG_ERR,"No more memory: mountd Failed");
				exit(2);
			}
			tgrp = (struct grouplist *)
			    malloc(sizeof(struct grouplist));
			if (tgrp == NULL) {
				syslog(LOG_ERR,"No more memory: mountd Failed");
				exit(2);
			}
			tgrp->anoncr = anoncr;
			tgrp->exflags = exflags;
			hpe->h_name = (char *)malloc(sizeof(DEF_NAME)+1);
			if (hpe->h_name == NULL) {
				syslog(LOG_ERR,"No more memory: mountd Failed");
				exit(2);
			}
			strcpy(hpe->h_name,DEF_NAME);
			hpe->h_addrtype = AF_INET;
			hpe->h_length = sizeof (u_long);
			hpe->h_addr_list = INADDR_ANY;
			tgrp->gr_ptr.gt_hostent = hpe;
			tgrp->gr_next = ep->ex_groups;
			ep->ex_groups = tgrp;
		    }
		}
		grp = ep->ex_groups;
		while (grp != NULL) {
			exflags = grp->exflags;
			anoncr = grp->anoncr;
			if (grp->type == MNT_GROUP) {
				tgrp = grp->gr_ptr.gt_grpname->gn_glist;
				while(tgrp != NULL) {
					if (do_mount(ep, tgrp, exflags, &anoncr,
					    dirplen) == FALSE)
						goto nextline;
					tgrp = tgrp->gr_next;
				}
			} else {
				if (do_mount(ep, grp, exflags, &anoncr, dirplen)
				    == FALSE)
					goto nextline;
			}
			grp = grp->gr_next;
		}
		if (cp)
			*cp = savedc;
		if (ep->ex_prev == (struct exportlist *)0) {
			ep->ex_next = exphead.ex_next;
			ep->ex_prev = &exphead;
			if (ep->ex_next != NULL)
				ep->ex_next->ex_prev = ep;
			exphead.ex_next = ep;
		}
nextline:
		;
	}
	fclose(inf);
	return;
err:
	syslog(LOG_ERR, "No more memory: mountd Failed");
	exit(2);
}

do_mount(ep, grp, exflags, anoncrp, dirplen)
	struct exportlist *ep;
	struct grouplist *grp;
	int exflags, dirplen;
	struct ucred *anoncrp;
{
	int done, found;
	register u_long **addrp;
	struct sockaddr_in sin;
	struct statfs stfsbuf;
	struct ufs_args args, targs;
	struct al_mnt *al_mp;
	char *cp, savedc;

	args.fspec = 0;
	args.exflags = exflags;
	args.anon = *anoncrp;
	sin.sin_family = AF_INET;
	sin.sin_port = 0;
	sin.sin_len = sizeof(sin);
	if (grp->type == MNT_HOST)
		addrp = (u_long **)grp->gr_ptr.gt_hostent->h_addr_list;
	done = FALSE;
	while(!done) {
		if (grp->type == MNT_HOST) {
			if (!strcmp(grp->gr_ptr.gt_hostent->h_name, DEF_NAME))
				sin.sin_addr.s_addr = INADDR_ANY;
			else
				sin.sin_addr.s_addr = **addrp;
			args.saddr = (struct sockaddr *)&sin;
			args.slen = sizeof(sin);
#ifdef ISO
		} else if (grp->type == MNT_ISO) {
			args.saddr = (struct sockaddr *)grp->gr_ptr.gt_isoaddr;
			args.slen = sizeof (struct sockaddr_iso);
#endif	/* ISO */
		} else {
			syslog(LOG_ERR, "Bad grouptype");
			free_exp(ep);
			return (FALSE);
		}
		if (statfs(ep->ex_dirp, &stfsbuf) < 0) {
			if (debug) {
				fprintf(stderr,"statfs failed.\n");
			}
			syslog(LOG_ERR, "Invalid path: %s", ep->ex_dirp);
			free_exp(ep);
			return(FALSE);
		}
		found = FALSE;
		for (al_mp = al_head; al_mp && !found; al_mp = al_mp->al_next)
			if (al_mp->al_mnted.val[0] == stfsbuf.f_fsid.val[0] &&
			    al_mp->al_mnted.val[1] == stfsbuf.f_fsid.val[1])
				found = TRUE;
		if (!found) {
			/* first time for fs, so must send a MNT_DELEXPORT
			 * to clear the old export list held in the kernel
			 * for this fs.
			 */
			al_mp = (struct al_mnt *)malloc(sizeof (struct al_mnt));
			al_mp->al_mnted = stfsbuf.f_fsid;
			al_mp->al_next = al_head;
			al_head = al_mp;
			targs.fspec = 0;
			targs.exflags = MNT_DELEXPORT;
			cp = (char *)0;
			while (mount(MOUNT_UFS, ep->ex_dirp,
			       stfsbuf.f_flags | MNT_UPDATE, &targs) < 0) {
				if (debug) {
					fprintf(stderr,
					    "tried [%s][%d]\n",
					    ep->ex_dirp,errno);
				}
				if (cp == NULL)
					cp = ep->ex_dirp + dirplen - 1;
				else
					*cp = savedc;
				cp--;
				/* back up over the last component */
				while (*cp == '/' && cp > ep->ex_dirp)
					cp--;
				while (*(cp - 1) != '/' && cp > ep->ex_dirp)
					cp--;
				if (cp == ep->ex_dirp) {
					if (debug) {
						fprintf(stderr,"mnt unsucc\n");
					}
					syslog(LOG_ERR,
					    "Can't export %s", ep->ex_dirp);
					free_exp(ep);
					return(FALSE);
				}
				savedc = *cp;
				*cp = '\0';
			}
			if (cp != NULL) {
				*cp = savedc;
			}
		}
		cp = (char *)0;
		while (mount(MOUNT_UFS, ep->ex_dirp,
		       stfsbuf.f_flags | MNT_UPDATE, &args) < 0) {
			if (errno == EPERM) {
				syslog(LOG_ERR,
				     "Can't change attributes for %s.\n",
				     ep->ex_dirp);
				if (cp != NULL)
					*cp = savedc;
				break;
			}
			if (cp == NULL)
				cp = ep->ex_dirp + dirplen - 1;
			else
				*cp = savedc;
			cp--;
			/* back up over the last component */
			while (*cp == '/' && cp > ep->ex_dirp)
				cp--;
			while (*(cp - 1) != '/' && cp > ep->ex_dirp)
				cp--;
			if (cp == ep->ex_dirp) {
				if (debug) {
					fprintf(stderr,"mnt unsucc\n");
				}
				syslog(LOG_ERR, "Can't export %s", ep->ex_dirp);
				free_exp(ep);
				return(FALSE);
			}
			savedc = *cp;
			*cp = '\0';
		}
		if (addrp == NULL)
			done = TRUE;
		else {
			++addrp;
			if (*addrp == NULL)
				done = TRUE;
		}
		if (cp != NULL)
			*cp = savedc;
	}
	return(TRUE);
}


/*
 * Parse out the next white space separated field
 */
void
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
 * Parse the option string
 */
void
do_opt(cpopt, ep, exflagsp, cr)
	register char *cpopt;
	struct exportlist *ep;
	int *exflagsp;
	struct ucred *cr;
{
	register char *cpoptarg, *cpoptend;
	int allflag;

	while (cpopt && *cpopt) {
		if (cpoptend = index(cpopt, ','))
			*cpoptend++ = '\0';
		if (cpoptarg = index(cpopt, '='))
			*cpoptarg++ = '\0';
		if (!strcmp(cpopt, "ro") || !strcmp(cpopt, "o")) {
			*exflagsp |= MNT_EXRDONLY;
		} else if ((!strcmp(cpopt, "root") || !strcmp(cpopt, "r") ||
		    !(allflag = strcmp(cpopt, "allanon"))) && cpoptarg) {
			parsecred(cpoptarg, cr);
			if (allflag == 0)
				*exflagsp |= MNT_EXPORTANON;
		} else if (!strcmp(cpopt, "kerb") || !strcmp(cpopt, "k")) {
			*exflagsp |= MNT_EXKERB;
		} else
			syslog(LOG_ERR, "opt %s ignored for %s", cpopt,
			       ep->ex_dirp);
		cpopt = cpoptend;
	}
}

/*
 * Parse a description of a credential.
 */
parsecred(namelist, cr)
	char *namelist;
	register struct ucred *cr;
{
	register char *name;
	register int cnt;
	char *names;
	struct passwd *pw;
	struct group *gr;
	int ngroups, groups[NGROUPS + 1];

	/*
	 * Set up the unpriviledged user.
	 */
	cr->cr_ref = 1;
	cr->cr_uid = -2;
	cr->cr_groups[0] = -2;
	cr->cr_ngroups = 1;
	/*
	 * Get the user's password table entry.
	 */
	names = strsep(&namelist, " \t\n");
	name = strsep(&names, ":");
	if (isdigit(*name) || *name == '-')
		pw = getpwuid(atoi(name));
	else
		pw = getpwnam(name);
	/*
	 * Credentials specified as those of a user.
	 */
	if (names == NULL) {
		if (pw == NULL) {
			syslog(LOG_ERR, "Unknown user: %s\n", name);
			return;
		}
		cr->cr_uid = pw->pw_uid;
		ngroups = NGROUPS + 1;
		if (getgrouplist(pw->pw_name, pw->pw_gid, groups, &ngroups))
			syslog(LOG_ERR, "Too many groups\n");
		/*
		 * Convert from int's to gid_t's and compress out duplicate
		 */
		cr->cr_ngroups = ngroups - 1;
		cr->cr_groups[0] = groups[0];
		for (cnt = 2; cnt < ngroups; cnt++)
			cr->cr_groups[cnt - 1] = groups[cnt];
		return;
	}
	/*
	 * Explicit credential specified as a colon separated list:
	 *	uid:gid:gid:...
	 */
	if (pw != NULL)
		cr->cr_uid = pw->pw_uid;
	else if (isdigit(*name) || *name == '-')
		cr->cr_uid = atoi(name);
	else {
		syslog(LOG_ERR, "Unknown user: %s\n", name);
		return;
	}
	cr->cr_ngroups = 0;
	while (names != NULL && *names != '\0' && cr->cr_ngroups < NGROUPS) {
		name = strsep(&names, ":");
		if (isdigit(*name) || *name == '-') {
			cr->cr_groups[cr->cr_ngroups++] = atoi(name);
		} else {
			if ((gr = getgrnam(name)) == NULL) {
				syslog(LOG_ERR, "Unknown group: %s\n", name);
				continue;
			}
			cr->cr_groups[cr->cr_ngroups++] = gr->gr_gid;
		}
	}
	if (names != NULL && *names != '\0' && cr->cr_ngroups == NGROUPS)
		syslog(LOG_ERR, "Too many groups\n");
}

#define	STRSIZ	(RPCMNT_NAMELEN+RPCMNT_PATHLEN+50)
/*
 * Routines that maintain the remote mounttab
 */
void
get_mountlist()
{
	register struct mountlist *mlp, **mlpp;
	register char *eos, *dirp;
	int len;
	char str[STRSIZ];
	FILE *mlfile;

	if (((mlfile = fopen(_PATH_RMOUNTLIST, "r")) == NULL) &&
	    ((mlfile = fopen(_PATH_RMOUNTLIST, "w")) == NULL)) {
		syslog(LOG_ERR, "Can't open %s", _PATH_RMOUNTLIST);
		return;
	}
	mlpp = &mlhead;
	while (fgets(str, STRSIZ, mlfile) != NULL) {
		if ((dirp = index(str, '\t')) == NULL &&
		    (dirp = index(str, ' ')) == NULL)
			continue;
		mlp = (struct mountlist *)malloc(sizeof (*mlp));
		len = dirp-str;
		if (len > RPCMNT_NAMELEN)
			len = RPCMNT_NAMELEN;
		bcopy(str, mlp->ml_host, len);
		mlp->ml_host[len] = '\0';
		while (*dirp == '\t' || *dirp == ' ')
			dirp++;
		if ((eos = index(dirp, '\t')) == NULL &&
		    (eos = index(dirp, ' ')) == NULL &&
		    (eos = index(dirp, '\n')) == NULL)
			len = strlen(dirp);
		else
			len = eos-dirp;
		if (len > RPCMNT_PATHLEN)
			len = RPCMNT_PATHLEN;
		bcopy(dirp, mlp->ml_dirp, len);
		mlp->ml_dirp[len] = '\0';
		mlp->ml_next = (struct mountlist *)0;
		*mlpp = mlp;
		mlpp = &mlp->ml_next;
	}
	fclose(mlfile);
}

void
del_mlist(hostp, dirp)
	register char *hostp, *dirp;
{
	register struct mountlist *mlp, **mlpp;
	FILE *mlfile;
	int fnd = 0;

	mlpp = &mlhead;
	mlp = mlhead;
	while (mlp) {
		if (!strcmp(mlp->ml_host, hostp) &&
		    (!dirp || !strcmp(mlp->ml_dirp, dirp))) {
			fnd = 1;
			*mlpp = mlp->ml_next;
			free((caddr_t)mlp);
		}
		mlpp = &mlp->ml_next;
		mlp = mlp->ml_next;
	}
	if (fnd) {
		if ((mlfile = fopen(_PATH_RMOUNTLIST, "w")) == NULL) {
			syslog(LOG_WARNING,"Can't update %s", _PATH_RMOUNTLIST);
			return;
		}
		mlp = mlhead;
		while (mlp) {
			fprintf(mlfile, "%s %s\n", mlp->ml_host, mlp->ml_dirp);
			mlp = mlp->ml_next;
		}
		fclose(mlfile);
	}
}

void
add_mlist(hostp, dirp)
	register char *hostp, *dirp;
{
	register struct mountlist *mlp, **mlpp;
	FILE *mlfile;

	mlpp = &mlhead;
	mlp = mlhead;
	while (mlp) {
		if (!strcmp(mlp->ml_host, hostp) && !strcmp(mlp->ml_dirp, dirp))
			return;
		mlpp = &mlp->ml_next;
		mlp = mlp->ml_next;
	}
	mlp = (struct mountlist *)malloc(sizeof (*mlp));
	strncpy(mlp->ml_host, hostp, RPCMNT_NAMELEN);
	mlp->ml_host[RPCMNT_NAMELEN] = '\0';
	strncpy(mlp->ml_dirp, dirp, RPCMNT_PATHLEN);
	mlp->ml_dirp[RPCMNT_PATHLEN] = '\0';
	mlp->ml_next = (struct mountlist *)0;
	*mlpp = mlp;
	if ((mlfile = fopen(_PATH_RMOUNTLIST, "a")) == NULL) {
		syslog(LOG_WARNING, "Can't update %s", _PATH_RMOUNTLIST);
		return;
	}
	fprintf(mlfile, "%s %s\n", mlp->ml_host, mlp->ml_dirp);
	fclose(mlfile);
}

/*
 * This function is called via. SIGTERM when the system is going down.
 * It sends a broadcast RPCMNT_UMNTALL.
 */
void
send_umntall()
{
	(void) clnt_broadcast(RPCPROG_MNT, RPCMNT_VER1, RPCMNT_UMNTALL,
		xdr_void, (caddr_t)0, xdr_void, (caddr_t)0, umntall_each);
	exit(0);
}

umntall_each(resultsp, raddr)
	caddr_t resultsp;
	struct sockaddr_in *raddr;
{
	return (1);
}

/*
 * Free up an exports list component
 */
void
free_exp(ep)
	register struct exportlist *ep;
{
	register struct grouplist *grp;
	struct grouplist *grp2;

	grp = ep->ex_groups;
	while (grp != NULL) {
		grp2 = grp;
		grp = grp->gr_next;
		free_grp(grp2);
	}
	free((caddr_t)ep);
}

/*
 * Free up a group list.
 */
void
free_grp(grp)
	register struct grouplist *grp;
{
	register char **addrp;

	if (grp->type == MNT_HOST) {
		addrp = grp->gr_ptr.gt_hostent->h_addr_list;
		while (addrp && *addrp)
			free(*addrp++);
		free((caddr_t)grp->gr_ptr.gt_hostent->h_addr_list);
		free(grp->gr_ptr.gt_hostent->h_name);
		free((caddr_t)grp->gr_ptr.gt_hostent);
	}
#ifdef ISO
	else if (grp->type == MNT_ISO)
		free((caddr_t)grp->gr_ptr.gt_isoaddr);
#endif
	free((caddr_t)grp);
}

void
get_group(line, ep)
	char *line;
	struct export_list *ep;
{
	int done;
	struct grouplist *grp;
	struct groupnames *t_gn;
	char *cp, *endcp, savedc;

	cp = line;
	nextfield(&cp, &endcp);
	savedc = *endcp;
	*endcp = NULL;
	if (*(endcp-1) == '=') {
		*(endcp-1) = NULL;
	}
	/* check to see if this group exists already */
	t_gn = grpnames;
	while(t_gn != NULL) {
		if (strcmp(t_gn->gn_name,cp) == 0) {
			syslog(LOG_ERR,"Group redifined, second ignored.");
			return;
		}
		t_gn = t_gn->gn_next;
	}

	/* make a new group list entry */
	t_gn = (struct groupnames *)malloc(sizeof(struct groupnames));
	if (t_gn == NULL) {
		syslog(LOG_ERR,"Group: Couldn't Malloc.");
		exit(2);
	}
	strcpy(t_gn->gn_name,cp);
	t_gn->gn_next = grpnames;
	grpnames = t_gn;
	t_gn->gn_glist = NULL;
	*endcp = savedc;
	cp = endcp;
	done = FALSE;
	while(!done) {
		nextfield(&cp, &endcp);
		if (cp == endcp)
			done = TRUE;
		else {
			savedc = *endcp;
			*endcp = NULL;
			if (strcmp(cp, "=")) {
				/* add to group list */
				get_host(cp, endcp, ep, &grp);
				if (grp != NULL) {
					grp->gr_next = t_gn->gn_glist;
					t_gn->gn_glist = grp;
				}
			}
			*endcp = savedc;
			cp = endcp;
		}
	}
}

void
get_host(cp, endcp, ep, gp)
	char *cp, *endcp;
	struct exportlist *ep;
	struct grouplist **gp;
{
	register struct hostent *hp, *nhp;
	register struct grouplist *grp;
	register char **addrp, **naddrp;
	struct hostent t_host;
	int i;
	u_long saddr;
	char *aptr[2];
#ifdef ISO
	struct iso_addr *isop;
	struct sockaddr_iso *isoaddr;
#endif

	if (isdigit(*cp)) {
		saddr = inet_addr(cp);
		if (saddr == -1) {
			syslog(LOG_ERR,
			    "Bad Exports File, %s: %s", cp,
			    "inet_addr failed, ignored");
			*gp = NULL;
			return;
		}
		hp = &t_host;
		hp->h_name = cp;
		hp->h_addrtype = AF_INET;
		hp->h_length = sizeof (u_long);
		hp->h_addr_list = aptr;
		aptr[0] = (char *)&saddr;
		aptr[1] = (char *)0;
#ifdef ISO
	} else if (!strncmp(cp, "iso=", 4)) {
		if ((isop = iso_addr(cp + 4)) == NULL) {
			syslog(LOG_ERR,
			    "Bad Exports File, %s: %s", cp,
			    "iso_addr failed, ignored");
			*gp = NULL;
			return;
		}
		isoaddr = (struct sockaddr_iso *)
		    malloc(sizeof (struct sockaddr_iso));
		if (isoaddr == NULL)
			goto err1;
		bzero((caddr_t)isoaddr, sizeof (struct sockaddr_iso));
		bcopy((caddr_t)isop, (caddr_t)isoaddr->siso_addr,
			sizeof (struct iso_addr));
		isoaddr->siso_len = sizeof (struct sockaddr_iso);
		isoaddr->siso_family = AF_ISO;
		grp = (struct grouplist *)
			malloc(sizeof(struct grouplist));
		if (grp == NULL)
			goto err1;
		grp->type = MNT_ISO;
		grp->gr_ptr.gt_isoaddr = isoaddr;
		*gp = grp;
		return;
#endif	/* ISO */
	} else if ((hp = gethostbyname(cp)) == NULL) {
		syslog(LOG_ERR, "Bad Exports File, %s: %s",
		    cp, "Gethostbyname failed, ignored");
		*gp = NULL;
		return;
	}
	grp = (struct grouplist *)
		malloc(sizeof(struct grouplist));
	if (grp == NULL)
		goto err1;
	grp->type = MNT_HOST;
	nhp = grp->gr_ptr.gt_hostent = (struct hostent *)
		malloc(sizeof(struct hostent));
	if (nhp == NULL)
		goto err1;
	bcopy((caddr_t)hp, (caddr_t)nhp,
		sizeof(struct hostent));
	i = strlen(hp->h_name)+1;
	nhp->h_name = (char *)malloc(i);
	if (nhp->h_name == NULL)
		goto err1;
	bcopy(hp->h_name, nhp->h_name, i);
	addrp = hp->h_addr_list;
	i = 1;
	while (*addrp++)
		i++;
	naddrp = nhp->h_addr_list = (char **)
		malloc(i*sizeof(char *));
	if (naddrp == NULL)
		goto err1;
	addrp = hp->h_addr_list;
	while (*addrp) {
		*naddrp = (char *)
		    malloc(hp->h_length);
		if (*naddrp == NULL)
		    goto err1;
		bcopy(*addrp, *naddrp,
			hp->h_length);
		addrp++;
		naddrp++;
	}
	*naddrp = (char *)0;
	*gp = grp;
	return;
err1:
	syslog(LOG_ERR, "No more memory: mountd Failed");
	exit(2);
}

void
do_group(cp, endcp, gp)
	char *cp, *endcp;
	struct grouplist **gp;
{
	int found;
	struct groupnames *t_gn;

	t_gn = grpnames;
	found = FALSE;
	while((t_gn != NULL) && !found) {
		if(strcmp(t_gn->gn_name,cp) == 0) {
			found = TRUE;
			*gp = (struct grouplist *)
			    malloc(sizeof(struct grouplist));
			if (*gp == NULL) {
				syslog(LOG_ERR,"No more memory: mountd Failed");
				exit(2);
			}
			(*gp)->gr_ptr.gt_grpname = (struct groupnames *)
			    malloc(sizeof(struct groupnames));
			if ((*gp)->gr_ptr.gt_grpname == NULL) {
				syslog(LOG_ERR,"No more memory: mountd Failed");
				exit(2);
			}
			(*gp)->gr_ptr.gt_grpname->gn_glist = t_gn->gn_glist;
			return;
		}
		t_gn = t_gn->gn_next;
	}
	*gp = NULL;
}

/*
 * char *realpath(const char *path, char resolved_path[MAXPATHLEN])
 *
 * find the real name of path, by removing all ".", ".."
 * and symlink components.
 *
 * Jan-Simon Pendry, September 1991.
 */
char *
realpath(path, resolved)
	char *path;
	char resolved[MAXPATHLEN];
{
	int d = open(".", O_RDONLY);
	int rootd = 0;
	char *p, *q;
	struct stat stb;
	char wbuf[MAXPATHLEN];

	strcpy(resolved, path);

	if (d < 0)
		return 0;

loop:;
	q = strrchr(resolved, '/');
	if (q) {
		p = q + 1;
		if (q == resolved)
			q = "/";
		else {
			do
				--q;
			while (q > resolved && *q == '/');
			q[1] = '\0';
			q = resolved;
		}
		if (chdir(q) < 0)
			goto out;
	} else
		p = resolved;

	if (lstat(p, &stb) == 0) {
		if (S_ISLNK(stb.st_mode)) {
			int n = readlink(p, resolved, MAXPATHLEN);
			if (n < 0)
				goto out;
			resolved[n] = '\0';
			goto loop;
		}
		if (S_ISDIR(stb.st_mode)) {
			if (chdir(p) < 0)
				goto out;
			p = "";
		}
	}

	strcpy(wbuf, p);
	if (getcwd(resolved, MAXPATHLEN) == 0)
		goto out;
	if (resolved[0] == '/' && resolved[1] == '\0')
		rootd = 1;

	if (*wbuf) {
		if (strlen(resolved) + strlen(wbuf) + rootd + 1 > MAXPATHLEN) {
			errno = ENAMETOOLONG;
			goto out;
		}
		if (rootd == 0)
			strcat(resolved, "/");
		strcat(resolved, wbuf);
	}

	if (fchdir(d) < 0)
		goto out;
	(void) close(d);

	return resolved;

out:;
	(void) close(d);
	return 0;
}
