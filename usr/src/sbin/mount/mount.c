/*
 * Copyright (c) 1980, 1989 The Regents of the University of California.
 * All rights reserved.
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
"@(#) Copyright (c) 1980, 1989 The Regents of the University of California.\n\
 All rights reserved.\n";
#endif /* not lint */

#ifndef lint
static char sccsid[] = "@(#)mount.c	5.34 (Berkeley) %G%";
#endif /* not lint */

#include "pathnames.h"
#include <sys/param.h>
#include <sys/file.h>
#include <sys/time.h>
#include <sys/wait.h>
#include <fstab.h>
#include <errno.h>
#include <stdio.h>
#include <signal.h>
#include <strings.h>
#include <sys/mount.h>
#ifdef NFS
#include <sys/socket.h>
#include <sys/socketvar.h>
#include <netdb.h>
#include <rpc/rpc.h>
#include <rpc/pmap_clnt.h>
#include <rpc/pmap_prot.h>
#include <nfs/rpcv2.h>
#include <nfs/nfsv2.h>
#include <nfs/nfs.h>
#endif

#define DEFAULT_ROOTUID	-2

#define	BADTYPE(type) \
	(strcmp(type, FSTAB_RO) && strcmp(type, FSTAB_RW) && \
	    strcmp(type, FSTAB_RQ))
#define	SETTYPE(type) \
	(!strcmp(type, FSTAB_RW) || !strcmp(type, FSTAB_RQ))

int fake, verbose, updateflg, mnttype;
char *mntname, **envp;
char **vfslist, **makevfslist();

#ifdef NFS
int xdr_dir(), xdr_fh();
char *getnfsargs();
struct nfs_args nfsdefargs = {
	(struct sockaddr *)0,
	SOCK_DGRAM,
	0,
	(nfsv2fh_t *)0,
	0,
	NFS_WSIZE,
	NFS_RSIZE,
	NFS_TIMEO,
	NFS_RETRANS,
	(char *)0,
};

struct nfhret {
	u_long	stat;
	nfsv2fh_t nfh;
};
#define	DEF_RETRY	10000
int retrycnt;
#define	BGRND	1
#define	ISBGRND	2
int opflags = 0;
#endif

main(argc, argv, arge)
	int argc;
	char **argv;
	char **arge;
{
	extern char *optarg;
	extern int optind;
	register struct fstab *fs;
	register int cnt;
	int all, ch, rval, flags, ret, pid, i;
	long mntsize;
	struct statfs *mntbuf, *getmntpt();
	char *type, *options = NULL;
	FILE *pidfile;

	envp = arge;
	all = 0;
	type = NULL;
	mnttype = MOUNT_UFS;
	mntname = "ufs";
	while ((ch = getopt(argc, argv, "afrwuvt:o:")) != EOF)
		switch((char)ch) {
		case 'a':
			all = 1;
			break;
		case 'f':
			fake = 1;
			break;
		case 'r':
			type = FSTAB_RO;
			break;
		case 'u':
			updateflg = MNT_UPDATE;
			break;
		case 'v':
			verbose = 1;
			break;
		case 'w':
			type = FSTAB_RW;
			break;
		case 'o':
			options = optarg;
			break;
		case 't':
			vfslist = makevfslist(optarg);
			mnttype = getmnttype(optarg);
			break;
		case '?':
		default:
			usage();
			/* NOTREACHED */
		}
	argc -= optind;
	argv += optind;

	/* NOSTRICT */

	if (all) {
		rval = 0;
		while (fs = getfsent()) {
			if (BADTYPE(fs->fs_type))
				continue;
			if (badvfsname(fs->fs_vfstype, vfslist))
				continue;
			/* `/' is special, it's always mounted */
			if (!strcmp(fs->fs_file, "/"))
				flags = MNT_UPDATE;
			else
				flags = updateflg;
			mnttype = getmnttype(fs->fs_vfstype);
			rval |= mountfs(fs->fs_spec, fs->fs_file, flags,
			    type, options, fs->fs_mntops);
		}
		exit(rval);
	}

	if (argc == 0) {
		if (verbose || fake || type)
			usage();
		if ((mntsize = getmntinfo(&mntbuf, MNT_NOWAIT)) == 0) {
			fprintf(stderr,
				"mount: cannot get mount information\n");
			exit(1);
		}
		for (i = 0; i < mntsize; i++) {
			if (badvfstype(mntbuf[i].f_type, vfslist))
				continue;
			prmount(mntbuf[i].f_mntfromname, mntbuf[i].f_mntonname,
				mntbuf[i].f_flags);
		}
		exit(0);
	}

	if (argc == 1 && updateflg) {
		if ((mntbuf = getmntpt(*argv)) == NULL) {
			fprintf(stderr,
			    "mount: unknown special file or file system %s.\n",
			    *argv);
			exit(1);
		}
		mnttype = mntbuf->f_type;
		if (!strcmp(mntbuf->f_mntfromname, "root_device")) {
			fs = getfsfile("/");
			strcpy(mntbuf->f_mntfromname, fs->fs_spec);
		}
		ret = mountfs(mntbuf->f_mntfromname, mntbuf->f_mntonname,
		    updateflg, type, options, NULL);
	} else if (argc == 1) {
		if (!(fs = getfsfile(*argv)) && !(fs = getfsspec(*argv))) {
			fprintf(stderr,
			    "mount: unknown special file or file system %s.\n",
			    *argv);
			exit(1);
		}
		if (BADTYPE(fs->fs_type)) {
			fprintf(stderr,
			    "mount: %s has unknown file system type.\n", *argv);
			exit(1);
		}
		mnttype = getmnttype(fs->fs_vfstype);
		ret = mountfs(fs->fs_spec, fs->fs_file, updateflg,
		    type, options, fs->fs_mntops);
	} else if (argc != 2) {
		usage();
		ret = 1;
	} else {
		ret = mountfs(argv[0], argv[1], updateflg, type, options, NULL);
	}
	if ((pidfile = fopen(_PATH_MOUNTDPID, "r")) != NULL) {
		pid = 0;
		fscanf(pidfile, "%d", &pid);
		fclose(pidfile);
		if (pid > 0)
			kill(pid, SIGHUP);
	}
	exit (ret);
}

mountfs(spec, name, flags, type, options, mntopts)
	char *spec, *name, *type, *options, *mntopts;
	int flags;
{
	extern int errno;
	register int cnt;
	int argc, status, i;
	struct ufs_args args;
	struct nfs_args nfsargs;
	char *argp, *argv[50];
	char execname[MAXPATHLEN + 1], flagval[12];

	nfsargs = nfsdefargs;
	if (mntopts)
		getstdopts(mntopts, &flags);
	if (options)
		getstdopts(options, &flags);
	if (type)
		getstdopts(type, &flags);
	switch (mnttype) {
	case MOUNT_UFS:
		if (mntopts)
			getufsopts(mntopts, &flags);
		if (options)
			getufsopts(options, &flags);
		args.fspec = spec;
		args.exroot = DEFAULT_ROOTUID;
		if (flags & MNT_RDONLY)
			args.exflags = MNT_EXRDONLY;
		else
			args.exflags = 0;
		argp = (caddr_t)&args;
		break;

#ifdef NFS
	case MOUNT_NFS:
		retrycnt = DEF_RETRY;
		if (mntopts)
			getnfsopts(mntopts, &nfsargs, &opflags, &retrycnt);
		if (options)
			getnfsopts(options, &nfsargs, &opflags, &retrycnt);
		if (argp = getnfsargs(spec, &nfsargs))
			break;
		return (1);
#endif /* NFS */

	case MOUNT_MFS:
	default:
		argv[0] = mntname;
		argc = 1;
		if (flags) {
			argv[argc++] = "-F";
			sprintf(flagval, "%d", flags);
			argv[argc++] = flagval;
		}
		if (mntopts)
			argc += getexecopts(mntopts, &argv[argc]);
		if (options)
			argc += getexecopts(options, &argv[argc]);
		argv[argc++] = spec;
		argv[argc++] = name;
		argv[argc++] = NULL;
		sprintf(execname, "%s/mount_%s", _PATH_EXECDIR, mntname);
		if (verbose) {
			printf("exec: %s", execname);
			for (i = 1; i < argc; i++)
				printf(" %s", argv[i]);
			printf("\n");
		}
		if (fake)
			break;
		if (i = vfork()) {
			if (i == -1) {
				perror("mount: vfork starting file system");
				return (1);
			}
			if (waitpid(i, &status, 0) != -1 &&
			    WIFEXITED(status) &&
			    WEXITSTATUS(status) != 0)
				return (WEXITSTATUS(status));
			spec = mntname;
			goto out;
		}
		execve(execname, argv, envp);
		fprintf(stderr, "mount: cannot exec %s for %s: ",
			execname, name);
		perror("");
		exit (1);
		/* NOTREACHED */

	}
	if (!fake && mount(mnttype, name, flags, argp)) {
		if (opflags & ISBGRND)
			exit(1);
		fprintf(stderr, "%s on %s: ", spec, name);
		switch (errno) {
		case EMFILE:
			fprintf(stderr, "Mount table full\n");
			break;
		case EINVAL:
			if (flags & MNT_UPDATE)
				fprintf(stderr, "Specified device does %s\n",
					"not match mounted device");
			else
				fprintf(stderr, "Bogus super block\n");
			break;
		case EOPNOTSUPP:
			fprintf(stderr, "Operation not supported\n");
			break;
		default:
			perror((char *)NULL);
			break;
		}
		return(1);
	}

out:
	if (verbose)
		prmount(spec, name, flags);

	if (opflags & ISBGRND)
		exit();
	else
		return(0);
}

static
prmount(spec, name, flags)
	char *spec, *name;
	long flags;
{

	if (opflags & ISBGRND)
		return;
	printf("%s on %s", spec, name);
	if (flags & MNT_RDONLY)
		printf(" (read-only)");
	if (flags & MNT_NOEXEC)
		printf(" (noexec)");
	if (flags & MNT_NOSUID)
		printf(" (nosuid)");
	if (flags & MNT_NODEV)
		printf(" (nodev)");
	if (flags & MNT_SYNCHRONOUS)
		printf(" (synchronous)");
	if (flags & MNT_QUOTA)
		printf(" (with quotas)");
	if (flags & MNT_LOCAL)
		printf(" (local)");
	if (flags & MNT_EXPORTED)
		if (flags & MNT_EXRDONLY)
			printf(" (NFS exported read-only)");
		else
			printf(" (NFS exported)");
	printf("\n");
}

getmnttype(fstype)
	char *fstype;
{

	mntname = fstype;
	if (!strcmp(fstype, "ufs"))
		return (MOUNT_UFS);
	if (!strcmp(fstype, "nfs"))
		return (MOUNT_NFS);
	if (!strcmp(fstype, "mfs"))
		return (MOUNT_MFS);
	return (0);
}

usage()
{

	fprintf(stderr, "usage:\n  mount %s %s\n  mount %s\n  mount %s\n",
		"[ -frwu ] [ -t nfs | ufs | external_type ]",
		"[ -o options ] special node",
		"[ -afrwu ] [ -t nfs | ufs | external_type ]",
		"[ -frwu ] special | node");
	exit(1);
}

getstdopts(options, flagp)
	char *options;
	long *flagp;
{
	register char *opt;
	int negative;
	char *optbuf[BUFSIZ], *strtok();

	strcpy(optbuf, options);
	for (opt = strtok(optbuf, ","); opt; opt = strtok(NULL, ",")) {
		if (opt[0] == 'n' && opt[1] == 'o') {
			negative++;
			opt += 2;
		} else {
			negative = 0;
		}
		if (!negative && !strcasecmp(opt, FSTAB_RO)) {
			*flagp |= MNT_RDONLY;
			continue;
		}
		if (!negative && !strcasecmp(opt, FSTAB_RW)) {
			*flagp &= ~MNT_RDONLY;
			continue;
		}
		if (!strcasecmp(opt, "exec")) {
			if (negative)
				*flagp |= MNT_NOEXEC;
			else
				*flagp &= ~MNT_NOEXEC;
			continue;
		}
		if (!strcasecmp(opt, "suid")) {
			if (negative)
				*flagp |= MNT_NOSUID;
			else
				*flagp &= ~MNT_NOSUID;
			continue;
		}
		if (!strcasecmp(opt, "dev")) {
			if (negative)
				*flagp |= MNT_NODEV;
			else
				*flagp &= ~MNT_NODEV;
			continue;
		}
		if (!strcasecmp(opt, "synchronous")) {
			if (!negative)
				*flagp |= MNT_SYNCHRONOUS;
			else
				*flagp &= ~MNT_SYNCHRONOUS;
			continue;
		}
	}
}

getufsopts(options, flagp)
	char *options;
	long *flagp;
{

	return;
}

getexecopts(options, argv)
	char *options;
	char **argv;
{
	register int argc = 0;
	register char *opt;
	char *strtok();

	for (opt = strtok(options, ","); opt; opt = strtok(NULL, ",")) {
		if (opt[0] != '-')
			continue;
		argv[argc++] = opt;
		if (opt[2] == '\0' || opt[2] != '=')
			continue;
		opt[2] = '\0';
		argv[argc++] = &opt[3];
	}
	return (argc);
}

struct statfs *
getmntpt(name)
	char *name;
{
	long mntsize;
	register long i;
	struct statfs *mntbuf;

	mntsize = getmntinfo(&mntbuf, MNT_NOWAIT);
	for (i = 0; i < mntsize; i++) {
		if (!strcmp(mntbuf[i].f_mntfromname, name) ||
		    !strcmp(mntbuf[i].f_mntonname, name))
			return (&mntbuf[i]);
	}
	return ((struct statfs *)0);
}

static int skipvfs;

badvfstype(vfstype, vfslist)
	long vfstype;
	char **vfslist;
{

	if (vfslist == 0)
		return(0);
	while (*vfslist) {
		if (vfstype == getmnttype(*vfslist))
			return(skipvfs);
		vfslist++;
	}
	return (!skipvfs);
}

badvfsname(vfsname, vfslist)
	char *vfsname;
	char **vfslist;
{

	if (vfslist == 0)
		return(0);
	while (*vfslist) {
		if (strcmp(vfsname, *vfslist) == 0)
			return(skipvfs);
		vfslist++;
	}
	return (!skipvfs);
}

char **
makevfslist(fslist)
	char *fslist;
{
	register char **av, *nextcp;
	register int i;
	char *malloc();

	if (fslist == NULL)
		return (NULL);
	if (fslist[0] == 'n' && fslist[1] == 'o') {
		fslist += 2;
		skipvfs = 1;
	}
	for (i = 0, nextcp = fslist; *nextcp; nextcp++)
		if (*nextcp == ',')
			i++;
	av = (char **)malloc((i+2) * sizeof(char *));
	if (av == NULL)
		return (NULL);
	nextcp = fslist;
	i = 0;
	av[i++] = nextcp;
	while (nextcp = index(nextcp, ',')) {
		*nextcp++ = '\0';
		av[i++] = nextcp;
	}
	av[i++] = 0;
	return (av);
}

#ifdef NFS
/*
 * Handle the getoption arg.
 * Essentially update "opflags", "retrycnt" and "nfsargs"
 */
getnfsopts(optarg, nfsargsp, opflagsp, retrycntp)
	char *optarg;
	struct nfs_args *nfsargsp;
	int *opflagsp;
	int *retrycntp;
{
	register char *cp, *nextcp;
	int num;
	char *nump;

	cp = optarg;
	while (cp != NULL && *cp != '\0') {
		if ((nextcp = index(cp, ',')) != NULL)
			*nextcp++ = '\0';
		if ((nump = index(cp, '=')) != NULL) {
			*nump++ = '\0';
			num = atoi(nump);
		} else
			num = -1;
		/*
		 * Just test for a string match and do it
		 */
		if (!strcmp(cp, "bg")) {
			*opflagsp |= BGRND;
		} else if (!strcmp(cp, "soft")) {
			nfsargsp->flags |= NFSMNT_SOFT;
		} else if (!strcmp(cp, "intr")) {
			nfsargsp->flags |= NFSMNT_INT;
		} else if (!strcmp(cp, "tcp")) {
			nfsargsp->sotype = SOCK_STREAM;
		} else if (!strcmp(cp, "noconn")) {
			nfsargsp->flags |= NFSMNT_NOCONN;
		} else if (!strcmp(cp, "retry") && num > 0) {
			*retrycntp = num;
		} else if (!strcmp(cp, "rsize") && num > 0) {
			nfsargsp->rsize = num;
			nfsargsp->flags |= NFSMNT_RSIZE;
		} else if (!strcmp(cp, "wsize") && num > 0) {
			nfsargsp->wsize = num;
			nfsargsp->flags |= NFSMNT_WSIZE;
		} else if (!strcmp(cp, "timeo") && num > 0) {
			nfsargsp->timeo = num;
			nfsargsp->flags |= NFSMNT_TIMEO;
		} else if (!strcmp(cp, "retrans") && num > 0) {
			nfsargsp->retrans = num;
			nfsargsp->flags |= NFSMNT_RETRANS;
		}
		cp = nextcp;
	}
}

char *
getnfsargs(spec, nfsargsp)
	char *spec;
	struct nfs_args *nfsargsp;
{
	extern int errno;
	register CLIENT *clp;
	struct hostent *hp;
	static struct sockaddr_in saddr;
	struct timeval pertry, try;
	enum clnt_stat clnt_stat;
	int so = RPC_ANYSOCK;
	char *hostp, *delimp;
	u_short tport;
	static struct nfhret nfhret;
	static char nam[MNAMELEN + 1];

	strncpy(nam, spec, MNAMELEN);
	nam[MNAMELEN] = '\0';
	if ((delimp = index(spec, '@')) != NULL) {
		hostp = delimp + 1;
	} else if ((delimp = index(spec, ':')) != NULL) {
		hostp = spec;
		spec = delimp + 1;
	} else {
		fprintf(stderr,
		    "No <host>:<dirpath> or <dirpath>@<host> spec\n");
		return (0);
	}
	*delimp = '\0';
	if ((hp = gethostbyname(hostp)) == NULL) {
		fprintf(stderr, "Can't get net id for host\n");
		return (0);
	}
	bcopy(hp->h_addr, (caddr_t)&saddr.sin_addr, hp->h_length);
	nfhret.stat = EACCES;	/* Mark not yet successful */
	while (retrycnt > 0) {
		saddr.sin_family = AF_INET;
		saddr.sin_port = htons(PMAPPORT);
		if ((tport = pmap_getport(&saddr, RPCPROG_NFS,
		    NFS_VER2, IPPROTO_UDP)) == 0) {
			if ((opflags & ISBGRND) == 0)
				clnt_pcreateerror("NFS Portmap");
		} else {
			saddr.sin_port = 0;
			pertry.tv_sec = 10;
			pertry.tv_usec = 0;
			if ((clp = clntudp_create(&saddr, RPCPROG_MNT,
			    RPCMNT_VER1, pertry, &so)) == NULL) {
				if ((opflags & ISBGRND) == 0)
					clnt_pcreateerror("Cannot MNT PRC");
			} else {
				clp->cl_auth = authunix_create_default();
				try.tv_sec = 10;
				try.tv_usec = 0;
				clnt_stat = clnt_call(clp, RPCMNT_MOUNT,
				    xdr_dir, spec, xdr_fh, &nfhret, try);
				if (clnt_stat != RPC_SUCCESS) {
					if ((opflags & ISBGRND) == 0)
						clnt_perror(clp, "Bad MNT RPC");
				} else {
					auth_destroy(clp->cl_auth);
					clnt_destroy(clp);
					retrycnt = 0;
				}
			}
		}
		if (--retrycnt > 0) {
			if (opflags & BGRND) {
				opflags &= ~BGRND;
				if (fork())
					return (0);
				else
					opflags |= ISBGRND;
			} 
			sleep(10);
		}
	}
	if (nfhret.stat) {
		if (opflags & ISBGRND)
			exit(1);
		fprintf(stderr, "Can't access %s: ", spec);
		errno = nfhret.stat;
		perror(NULL);
		return (0);
	}
	saddr.sin_port = htons(tport);
	nfsargsp->addr = (struct sockaddr *) &saddr;
	nfsargsp->fh = &nfhret.nfh;
	nfsargsp->hostname = nam;
	return ((caddr_t)nfsargsp);
}

/*
 * xdr routines for mount rpc's
 */
xdr_dir(xdrsp, dirp)
	XDR *xdrsp;
	char *dirp;
{
	return (xdr_string(xdrsp, &dirp, RPCMNT_PATHLEN));
}

xdr_fh(xdrsp, np)
	XDR *xdrsp;
	struct nfhret *np;
{
	if (!xdr_u_long(xdrsp, &(np->stat)))
		return (0);
	if (np->stat)
		return (1);
	return (xdr_opaque(xdrsp, (caddr_t)&(np->nfh), NFSX_FH));
}
#endif /* NFS */
