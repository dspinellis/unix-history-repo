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
static char sccsid[] = "@(#)umount.c	5.10 (Berkeley) %G%";
#endif not lint

/*
 * umount
 */
#include <sys/param.h>
#include <stdio.h>
#include <fstab.h>
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

#ifdef NFS
extern int errno;
int xdr_dir();
char	*index();
#endif
int	vflag, all, errs;
int	fflag = MNT_NOFORCE;
char	*rindex(), *getmntname();
#define MNTON	1
#define MNTFROM	2

main(argc, argv)
	int argc;
	char **argv;
{

	argc--, argv++;
	sync();
again:
	if (argc > 0 && !strcmp(*argv, "-v")) {
		vflag++;
		argc--, argv++;
		goto again;
	}
	if (argc > 0 && !strcmp(*argv, "-f")) {
		fflag = MNT_FORCE;
		argc--, argv++;
		goto again;
	}
	if (argc > 0 && !strcmp(*argv, "-a")) {
		all++;
		argc--, argv++;
		goto again;
	}
	if (argc == 0 && !all) {
		fprintf(stderr,
			"Usage: umount [ -a ] [ -f ] [ -v ] [ dev ... ]\n");
		exit(1);
	}
	if (all) {
		if (setfsent() == 0)
			perror(FSTAB), exit(1);
		umountall();
		exit(0);
	} else
		setfsent();
	while (argc > 0) {
		if (umountfs(*argv++) == 0)
			errs++;
		argc--;
	}
	exit(errs);
}

umountall()
{
	struct fstab *fs, *allocfsent();

	if ((fs = getfsent()) == (struct fstab *)0)
		return;
	fs = allocfsent(fs);
	umountall();
	if (strcmp(fs->fs_file, "/") == 0) {
		freefsent(fs);
		return;
	}
	if (strcmp(fs->fs_type, FSTAB_RW) &&
	    strcmp(fs->fs_type, FSTAB_RO) &&
	    strcmp(fs->fs_type, FSTAB_RQ)) {
		freefsent(fs);
		return;
	}
	(void) umountfs(fs->fs_file);
	freefsent(fs);
}

struct fstab *
allocfsent(fs)
	register struct fstab *fs;
{
	register struct fstab *new;
	register char *cp;
	char *malloc();

	new = (struct fstab *)malloc((unsigned)sizeof (*fs));
	cp = malloc((unsigned)strlen(fs->fs_file) + 1);
	strcpy(cp, fs->fs_file);
	new->fs_file = cp;
	cp = malloc((unsigned)strlen(fs->fs_type) + 1);
	strcpy(cp, fs->fs_type);
	new->fs_type = cp;
	cp = malloc((unsigned)strlen(fs->fs_spec) + 1);
	strcpy(cp, fs->fs_spec);
	new->fs_spec = cp;
	new->fs_passno = fs->fs_passno;
	new->fs_freq = fs->fs_freq;
	return (new);
}

freefsent(fs)
	register struct fstab *fs;
{

	if (fs->fs_file)
		free(fs->fs_file);
	if (fs->fs_spec)
		free(fs->fs_spec);
	if (fs->fs_type)
		free(fs->fs_type);
	free((char *)fs);
}

umountfs(name)
	char *name;
{
	char *mntpt;
	struct stat stbuf;
#ifdef NFS
	register CLIENT *clp;
	struct hostent *hp;
	struct sockaddr_in saddr;
	struct timeval pertry, try;
	enum clnt_stat clnt_stat;
	int so = RPC_ANYSOCK;
	char *hostp, *delimp;
#endif

	if (stat(name, &stbuf) < 0) {
		if ((mntpt = getmntname(name, MNTON)) == 0)
			return (0);
	} else if ((stbuf.st_mode & S_IFMT) == S_IFBLK) {
		if ((mntpt = getmntname(name, MNTON)) == 0)
			return (0);
	} else if ((stbuf.st_mode & S_IFMT) == S_IFDIR) {
		mntpt = name;
		if ((name = getmntname(mntpt, MNTFROM)) == 0)
			return (0);
	} else {
		fprintf(stderr, "%s: not a directory or special device\n",
			name);
		return (0);
	}
	if (unmount(mntpt, fflag) < 0) {
		perror(mntpt);
		return (0);
	}
	if (vflag)
		fprintf(stderr, "%s: Unmounted from %s\n", name, mntpt);
#ifdef NFS
	if ((delimp = index(name, '@')) != NULL) {
		hostp = delimp + 1;
		*delimp = '\0';
	} else {
		return (1);
	}
	if ((hp = gethostbyname(hostp)) != NULL) {
		bcopy(hp->h_addr,(caddr_t)&saddr.sin_addr,hp->h_length);
		saddr.sin_family = AF_INET;
		saddr.sin_port = 0;
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
#endif NFS
	return (1);
}

char *
getmntname(name, what)
	char *name;
	int what;
{
	int mntsize, i;
	struct statfs *mntbuf;

	if ((mntsize = getmntinfo(&mntbuf)) == 0) {
		perror("umount");
		return (0);
	}
	for (i = 0; i < mntsize; i++) {
		if (what == MNTON && !strcmp(mntbuf[i].f_mntfromname, name))
			return (mntbuf[i].f_mntonname);
		if (what == MNTFROM && !strcmp(mntbuf[i].f_mntonname, name))
			return (mntbuf[i].f_mntfromname);
	}
	fprintf(stderr, "%s: not currently mounted\n", name);
	return (0);
}

#ifdef NFS
/*
 * xdr routines for mount rpc's
 */
xdr_dir(xdrsp, dirp)
	XDR *xdrsp;
	char *dirp;
{
	return (xdr_string(xdrsp, &dirp, RPCMNT_PATHLEN));
}
#endif NFS
