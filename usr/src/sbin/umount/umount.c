/*-
 * Copyright (c) 1980, 1989, 1993
 *	The Regents of the University of California.  All rights reserved.
 *
 * %sccs.include.redist.c%
 */

#ifndef lint
static char copyright[] =
"@(#) Copyright (c) 1980, 1989, 1993\n\
	The Regents of the University of California.  All rights reserved.\n";
#endif /* not lint */

#ifndef lint
static char sccsid[] = "@(#)umount.c	8.2 (Berkeley) %G%";
#endif /* not lint */

#include <sys/param.h>
#include <sys/stat.h>
#include <sys/mount.h>

#ifdef NFS
#include <sys/time.h>
#include <sys/socket.h>
#include <sys/socketvar.h>
#include <netdb.h>
#include <rpc/rpc.h>
#include <rpc/pmap_clnt.h>
#include <rpc/pmap_prot.h>
#include <nfs/rpcv2.h>
#endif

#include <err.h>
#include <fstab.h>
#include <stdio.h>
#include <stdlib.h>
#include <string.h>
#include <unistd.h>

#ifdef NFS
char	*nfshost;
int	namematch __P((struct hostent *, char *));
int	xdr_dir __P((XDR *, char *));
#endif

typedef enum { MNTON, MNTFROM } mntwhat;

int	all, errs, fake, vflag;
int	fflag = 0;
char	*namelist[] = INITMOUNTNAMES;
int	*typelist;

int	badtype __P((int, int *));
int	fsnametotype __P((char *));
char	*getmntname __P((char *, mntwhat, int *));
int	*maketypelist __P((char *));
void	umountall __P((int *));
int	umountfs __P((char *, int *));
void	usage __P((void));

int
main(argc, argv)
	int argc;
	char **argv;
{
	int ch;

	sync();
	while ((ch = getopt(argc, argv, "afFh:t:v")) != EOF)
		switch (ch) {
		case 'a':
			all++;
			break;
		case 'f':
			fflag = MNT_FORCE;
			break;
		case 'F':
			fake++;
			break;
#ifdef	NFS
		case 'h':
			/* -h flag implies -a, and "-t nfs" if no -t flag */
			nfshost = optarg;
			all++;
			if (typelist == NULL)
				typelist = maketypelist("nfs");
			break;
#endif /* NFS */
		case 't':
			typelist = maketypelist(optarg);
			break;
		case 'v':
			vflag++;
			break;
		default:
			usage();
			/* NOTREACHED */
		}
	argc -= optind;
	argv += optind;

	if (argc == 0 && !all)
		usage();

	if (setfsent() == 0)
		err(1, "%s", FSTAB);

	if (all) {
		if (argc > 0)
			usage();
		umountall(typelist);
		exit(0);
	}

	while (argc > 0) {
		if (umountfs(*argv++, 0) == 0)
			errs++;
		argc--;
	}
	exit(errs ? 1 : 0);
}

void
usage()
{
	fprintf(stderr,
		"usage:\n%s\n%s\n",
		"  umount [-fv] special | node",
#ifndef	NFS
		"  umount -a[fv] [-t fstypelist]"
#else
		"  umount -a[fv] [-h host] [-t fstypelist]"
#endif
	      );
	exit(1);
}

void
umountall(typelist)
	int *typelist;
{
	char *cp;
	struct fstab *fs;

	while (fs = getfsent()) {
		if (badtype(fsnametotype(fs->fs_vfstype), typelist))
			continue;
		if (strcmp(fs->fs_file, "/") == 0)
			continue;
		if (strcmp(fs->fs_type, FSTAB_RW) &&
		    strcmp(fs->fs_type, FSTAB_RO) &&
		    strcmp(fs->fs_type, FSTAB_RQ))
			continue;
		cp = (char *)malloc((size_t)strlen(fs->fs_file) + 1);
		if (cp == NULL)
			err(2, "malloc");
		strcpy(cp, fs->fs_file);
		umountall(typelist);
		break;
	}
	if (fs) {
		(void) umountfs(cp, typelist);
		free(cp);
	}
}

int
umountfs(name, typelist)
	char *name;
	int *typelist;
{
#ifdef NFS
	enum clnt_stat clnt_stat;
	CLIENT *clp;
	char *delimp, *hostp;
	struct hostent *hp = 0;
	struct sockaddr_in saddr;
	struct timeval pertry, try;
	int so = RPC_ANYSOCK;
#endif /* NFS */
	char *mntpt;
	struct stat stbuf;
	int type;
	char rname[MAXPATHLEN];

	if (realpath(name, rname) == NULL) {
		warn("%s", rname);
		return (0);
	}

	name = rname;

	if (stat(name, &stbuf) < 0) {
		if (((mntpt = getmntname(name, MNTFROM, &type)) == 0) &&
		    ((mntpt = getmntname(name, MNTON, &type)) == 0)) {
			warnx("%s: not currently mounted", name);
			return (0);
		}
	} else if ((stbuf.st_mode & S_IFMT) == S_IFBLK) {
		if ((mntpt = getmntname(name, MNTON, &type)) == 0) {
			warnx("%s: not currently mounted", name);
			return (0);
		}
	} else if ((stbuf.st_mode & S_IFMT) == S_IFDIR) {
		mntpt = name;
		if ((name = getmntname(mntpt, MNTFROM, &type)) == 0) {
			warnx("%s: not currently mounted", mntpt);
			return (0);
		}
	} else {
		warnx("%s: not a directory or special device", name);
		return (0);
	}

	if (badtype(type, typelist))
		return (1);
#ifdef NFS
	if ((delimp = strchr(name, '@')) != NULL) {
		hostp = delimp + 1;
		*delimp = '\0';
		hp = gethostbyname(hostp);
		*delimp = '@';
	} else if ((delimp = strchr(name, ':')) != NULL) {
		*delimp = '\0';
		hostp = name;
		hp = gethostbyname(hostp);
		name = delimp+1;
		*delimp = ':';
	}

	if (!namematch(hp, nfshost))
		return (1);
#endif	/* NFS */
	if (!fake && unmount(mntpt, fflag) < 0) {
		warn("%s", mntpt);
		return (0);
	}
	if (vflag)
		warnx("%s: unmounted from %s", name, mntpt);

#ifdef	NFS
	if (!fake && (hp != NULL) && ((fflag & MNT_FORCE) == 0)) {
		*delimp = '\0';
		bzero(&saddr, sizeof(saddr));
		saddr.sin_family = AF_INET;
		saddr.sin_port = 0;
		bcopy(hp->h_addr, &saddr.sin_addr, hp->h_length);
		pertry.tv_sec = 3;
		pertry.tv_usec = 0;
		if ((clp = clntudp_create(&saddr, RPCPROG_MNT, RPCMNT_VER1,
		    pertry, &so)) == NULL) {
			clnt_pcreateerror("Cannot MNT PRC");
			return (1);
		}
		clp->cl_auth = authunix_create_default();
		try.tv_sec = 20;
		try.tv_usec = 0;
		clnt_stat = clnt_call(clp, RPCMNT_UMOUNT, xdr_dir, name,
			xdr_void, (caddr_t)0, try);
		if (clnt_stat != RPC_SUCCESS) {
			clnt_perror(clp, "Bad MNT RPC");
			return (1);
		}
		auth_destroy(clp->cl_auth);
		clnt_destroy(clp);
	}
#endif /* NFS */
	return (1);
}

char *
getmntname(name, what, type)
	char *name;
	mntwhat what;
	int *type;
{
	int i, mntsize;
	struct statfs *mntbuf;

	if ((mntsize = getmntinfo(&mntbuf, MNT_NOWAIT)) == 0) {
		warn("getmntinfo");
		return (NULL);
	}
	for (i = 0; i < mntsize; i++) {
		if (what == MNTON && !strcmp(mntbuf[i].f_mntfromname, name)) {
			if (type)
				*type = mntbuf[i].f_type;
			return (mntbuf[i].f_mntonname);
		}
		if (what == MNTFROM && !strcmp(mntbuf[i].f_mntonname, name)) {
			if (type)
				*type = mntbuf[i].f_type;
			return (mntbuf[i].f_mntfromname);
		}
	}
	return (NULL);
}

static int skipvfs;

int
badtype(type, typelist)
	int type;
	int *typelist;
{
	if (typelist == 0)
		return (0);

	while (*typelist) {
		if (type == *typelist)
			return (skipvfs);
		typelist++;
	}
	return (!skipvfs);
}

int *
maketypelist(fslist)
	char *fslist;
{
	char *nextcp;
	int *av, i;

	if (fslist == NULL)
		return (NULL);

	if (fslist[0] == 'n' && fslist[1] == 'o') {
		fslist += 2;
		skipvfs = 1;
	} else
		skipvfs = 0;
	for (i = 0, nextcp = fslist; *nextcp; nextcp++)
		if (*nextcp == ',')
			i++;

	av = (int *)malloc((i+2) * sizeof(int));
	if (av == NULL)
		return (NULL);

	for (i = 0; fslist; fslist = nextcp) {
		if (nextcp = strchr(fslist, ','))
			*nextcp++ = '\0';
		av[i++] = fsnametotype(fslist);
	}
	av[i++] = 0;
	return (av);
}

int
fsnametotype(name)
	char *name;
{
	char **cp;

	for (cp = namelist; *cp; cp++)
		if (strcmp(name, *cp) == 0)
			return (cp - namelist);
	return (MOUNT_NONE);
}

#ifdef NFS
int
namematch(hp, nfshost)
	struct hostent *hp;
	char *nfshost;
{
	char *cp;
	char **np;

	if (hp == NULL || nfshost == NULL)
		return (1);

	if (strcasecmp(nfshost, hp->h_name) == 0)
		return (1);

	if (cp = strchr(hp->h_name, '.')) {
		*cp = '\0';
		if (strcasecmp(nfshost, hp->h_name) == 0)
			return (1);
	}
	for (np = hp->h_aliases; *np; np++) {
		if (strcasecmp(nfshost, *np) == 0)
			return (1);
		if (cp = strchr(*np, '.')) {

			*cp = '\0';
			if (strcasecmp(nfshost, *np) == 0)
				return (1);
		}
	}
	return (0);
}

/*
 * xdr routines for mount rpc's
 */
int
xdr_dir(xdrsp, dirp)
	XDR *xdrsp;
	char *dirp;
{
	return (xdr_string(xdrsp, &dirp, RPCMNT_PATHLEN));
}
#endif /* NFS */
