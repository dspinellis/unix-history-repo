/*
 * Copyright (c) 1980 Regents of the University of California.
 * All rights reserved.  The Berkeley software License Agreement
 * specifies the terms and conditions for redistribution.
 */

#ifndef lint
char copyright[] =
"@(#) Copyright (c) 1980 Regents of the University of California.\n\
 All rights reserved.\n";
#endif not lint

#ifndef lint
static char sccsid[] = "@(#)mount.c	5.13 (Berkeley) %G%";
#endif not lint

#include "pathnames.h"
#include <sys/param.h>
#include <sys/file.h>
#include <sys/time.h>
#include <sys/wait.h>
#include <fstab.h>
#include <errno.h>
#include <stdio.h>
#include <strings.h>
#include <sys/dir.h>
#include <sys/uio.h>
#include <sys/namei.h>
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

#define	BADTYPE(type) \
	(strcmp(type, FSTAB_RO) && strcmp(type, FSTAB_RW) && \
	    strcmp(type, FSTAB_RQ))
#define	SETTYPE(type) \
	(!strcmp(type, FSTAB_RW) || !strcmp(type, FSTAB_RQ))

static int fake, verbose, mnttype;
char **envp;

#ifdef NFS
int xdr_dir(), xdr_fh();
char *getnfsargs();
struct nfs_args nfsargs = {
	(struct sockaddr_in *)0,
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
int retrycnt = 10000;
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
	int all, ch, rval, sfake, i;
	long mntsize;
	struct statfs statfsbuf, *mntbuf;
	char *type, *options = NULL;

	envp = arge;
	all = 0;
	type = NULL;
	mnttype = MOUNT_UFS;
	while ((ch = getopt(argc, argv, "afrwvt:o:")) != EOF)
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
			if (mnttype = getmnttype(optarg))
				break;
			/* fall through */
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
		for (sfake = fake; fs = getfsent(); fake = sfake) {
			if (BADTYPE(fs->fs_type))
				continue;
			/* `/' is special, it's always mounted */
			if (!strcmp(fs->fs_file, "/"))
				fake = 1;
			if ((mnttype = getmnttype(fs->fs_vfstype)) == 0) {
				fprintf(stderr,
				    "%s %s type of file system is unknown.\n",
				    "mount:", fs->fs_vfstype);
				continue;
			}
			rval |= mountfs(fs->fs_spec, fs->fs_file,
			    type ? type : fs->fs_type, options, fs->fs_mntops);
		}
		exit(rval);
	}

	if (argc == 0) {
		if (verbose || fake || type)
			usage();
		if ((mntsize = getfsstat(0, 0)) < 0) {
			perror("mount");
			exit(1);
		}
		mntbuf = 0;
		do {
			if (mntbuf)
				free(mntbuf);
			i = (mntsize + 1) * sizeof(struct statfs);
			if ((mntbuf = (struct statfs *)malloc(i)) == 0) {
				fprintf(stderr,
					"no space for mount table buffer\n");
				exit(1);
			}
			if ((mntsize = getfsstat(mntbuf, i)) < 0) {
				perror("mount");
				exit(1);
			}
		} while (i == mntsize * sizeof(struct statfs));
		for (i = 0; i < mntsize; i++)
			prmount(mntbuf[i].f_mntfromname, mntbuf[i].f_mntonname,
				mntbuf[i].f_flags);
		exit(0);
	}

	if (argc == 1) {
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
		if ((mnttype = getmnttype(fs->fs_vfstype)) == 0) {
			fprintf(stderr,
			    "mount: %s type of file system is unknown.\n",
			    fs->fs_vfstype);
			exit(1);
		}
		exit(mountfs(fs->fs_spec, fs->fs_file,
		    type ? type : fs->fs_type, options, fs->fs_mntops));
	}

	if (argc != 2)
		usage();

	exit(mountfs(argv[0], argv[1], type ? type : "rw", options, NULL));
}

mountfs(spec, name, type, options, mntopts)
	char *spec, *name, *type, *options, *mntopts;
{
	extern int errno;
	register int cnt;
	int flags, argc, status, i;
	struct ufs_args args;
	char *argp, *argv[50];

	flags = 0;
	if (!strcmp(type, FSTAB_RO))
		flags |= M_RDONLY;
	if (options)
		getstdopts(options, &flags);
	if (mntopts)
		getstdopts(mntopts, &flags);
	switch (mnttype) {
	case MOUNT_UFS:
		if (options)
			getufsopts(options, &flags);
		if (mntopts)
			getufsopts(mntopts, &flags);
		args.fspec = spec;
		argp = (caddr_t)&args;
		break;

#ifdef NFS
	case MOUNT_NFS:
		if (options)
			getnfsopts(options, &nfsargs, &opflags,
				&retrycnt);
		if (mntopts)
			getnfsopts(mntopts, &nfsargs, &opflags,
				&retrycnt);
		if (argp = getnfsargs(spec, name, type))
			break;
		return (1);
#endif /* NFS */

#ifdef MFS
	case MOUNT_MFS:
		argv[0] = "memfs";
		argc = 1;
		if (options)
			argc += getmfsopts(options, &argv[argc]);
		if (mntopts)
			argc += getmfsopts(mntopts, &argv[argc]);
		argv[argc++] = spec;
		argv[argc++] = name;
		if (verbose) {
			printf("exec:");
			for (i = 0; i < argc; i++)
				printf(" %s", argv[i]);
			printf("\n");
		}
		if (fake)
			break;
		if (i = vfork()) {
			if (i == -1) {
				perror("mount: vfork for memfs");
				return (1);
			}
			if (waitpid(i, &status, 0) != -1 &&
			    WIFEXITED(status) &&
			    WEXITSTATUS(status) != 0)
				return (WEXITSTATUS(status));
			spec = "memfs";
			goto out;
		}
		execve(_PATH_MEMFS, argv, envp);
		perror(_PATH_MEMFS);
		exit (1);
#endif /* MFS */

	default:
		if (opflags & ISBGRND)
			exit(1);
		fprintf(stderr, "%d: unknown mount type\n", mnttype);
		exit(1);
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
			fprintf(stderr, "Bogus super block\n");
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
	register char *root;

	if (opflags & ISBGRND)
		return;
	/*
	 * trim trailing /'s and find last component of name
	 */
	for (root = index(spec, '\0'); *--root == '/';)
		/* void */;
	*++root = '\0';
	if (root = rindex(spec, '/'))
		spec = root + 1;
	printf("%s on %s", spec, name);
	if (flags & M_RDONLY)
		printf(" (read-only)");
	if (flags & M_NOEXEC)
		printf(" (noexec)");
	if (flags & M_NOSUID)
		printf(" (nosuid)");
	if (flags & M_NODEV)
		printf(" (nodev)");
	if (flags & M_SYNCHRONOUS)
		printf(" (synchronous)");
	printf("\n");
}

getmnttype(fstype)
	char *fstype;
{

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
	fprintf(stderr, "usage: mount [-afrw]\nor mount [-frw] special | node\nor mount [-frw] special node\n");
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
		if (!strcasecmp(opt, "exec")) {
			if (negative)
				*flagp |= M_NOEXEC;
			continue;
		}
		if (!strcasecmp(opt, "suid")) {
			if (negative)
				*flagp |= M_NOSUID;
			continue;
		}
		if (!strcasecmp(opt, "dev")) {
			if (negative)
				*flagp |= M_NODEV;
			continue;
		}
		if (!strcasecmp(opt, "synchronous")) {
			if (!negative)
				*flagp |= M_SYNCHRONOUS;
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

#ifdef MFS
getmfsopts(options, argv)
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
#endif /* MFS */

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
getnfsargs(spec)
	char *spec;
{
	register CLIENT *clp;
	struct hostent *hp;
	struct sockaddr_in saddr;
	struct timeval pertry, try;
	enum clnt_stat clnt_stat;
	int so = RPC_ANYSOCK;
	char *hostp, *delimp;
	u_short tport;
	struct nfhret nfhret;
	char nam[MNAMELEN + 1];

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
		fprintf(stderr, "Can't access %s, errno=%d\n", spec,
		    nfhret.stat);
		return (0);
	}
	saddr.sin_port = htons(tport);
	nfsargs.addr = &saddr;
	nfsargs.fh = &nfhret.nfh;
	nfsargs.hostname = nam;
	return ((caddr_t)&nfsargs);
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
