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
static char sccsid[] = "@(#)umount.c	5.4 (Berkeley) %G%";
#endif not lint

/*
 * umount
 */
#include <sys/param.h>
#include <stdio.h>
#include <fstab.h>
#include <mtab.h>
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

struct	mtab mtab[NMOUNT];

char	*rindex();
int	vflag, all, errs;
#ifdef NFS
extern int errno;
int xdr_dir();
char	*index();
#endif

main(argc, argv)
	int argc;
	char **argv;
{
	register struct mtab *mp;
	register char *p1, *p2;
	int mf;

	argc--, argv++;
	sync();
	mf = open("/etc/mtab", 0);
	read(mf, (char *)mtab, sizeof (mtab));
again:
	if (argc > 0 && !strcmp(*argv, "-v")) {
		vflag++;
		argc--, argv++;
		goto again;
	}
	if (argc > 0 && !strcmp(*argv, "-a")) {
		all++;
		argc--, argv++;
		goto again;
	}
	if (argc == 0 && !all) {
		fprintf(stderr, "Usage: umount [ -a ] [ -v ] [ dev ... ]\n");
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

	if ((fs = getfsent()) == 0)
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
	if (umountfs(fs->fs_file) < 0)
		perror(fs->fs_file);
	freefsent(fs);
}

struct fstab *
allocfsent(fs)
	register struct fstab *fs;
{
	register struct fstab *new;
	register char *cp;
	char *malloc();

	new = (struct fstab *)malloc(sizeof (*fs));
	cp = malloc(strlen(fs->fs_file) + 1);
	strcpy(cp, fs->fs_file);
	new->fs_file = cp;
	cp = malloc(strlen(fs->fs_type) + 1);
	strcpy(cp, fs->fs_type);
	new->fs_type = cp;
	cp = malloc(strlen(fs->fs_spec) + 1);
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

struct	mtab zeromtab;

umountfs(name)
	char *name;
{
	register char *p1, *p2;
	register struct	mtab *mp;
	int mf;
	struct fstab *fs;
#ifdef NFS
	register CLIENT *clp;
	struct hostent *hp;
	struct sockaddr_in saddr;
	struct timeval pertry, try;
	enum clnt_stat clnt_stat;
	int so = RPC_ANYSOCK;
	u_short tport;
	char *hostp, *delimp;
#endif

	fs = getfsspec(name);
	if (fs != NULL)
		name = fs->fs_file;
#ifdef NFS
	else
		fs = getfsfile(name);
#endif
	if (umount(name, MNT_NOFORCE) < 0) {
		perror(name);
		return (0);
	}
#ifdef NFS
	if (fs != NULL) {
		if ((delimp = index(fs->fs_spec, '@')) != NULL) {
			hostp = delimp+1;
			*delimp = '\0';
		} else {
			goto out;
		}
		if ((hp = gethostbyname(hostp)) != NULL) {
			bcopy(hp->h_addr,(caddr_t)&saddr.sin_addr,hp->h_length);
			saddr.sin_family = AF_INET;
			saddr.sin_port = 0;
			pertry.tv_sec = 3;
			pertry.tv_usec = 0;
			if ((clp = clntudp_create(&saddr, RPCPROG_MNT,
				RPCMNT_VER1, pertry, &so)) == NULL) {
				clnt_pcreateerror("Cannot MNT PRC");
				goto out;
			}
			clp->cl_auth = authunix_create_default();
			try.tv_sec = 20;
			try.tv_usec = 0;
			clnt_stat = clnt_call(clp, RPCMNT_UMOUNT, xdr_dir, fs->fs_spec,
				xdr_void, (caddr_t)0, try);
			if (clnt_stat != RPC_SUCCESS) {
				clnt_perror(clp, "Bad MNT RPC");
				goto out;
			}
			auth_destroy(clp->cl_auth);
			clnt_destroy(clp);
		}
	} else
		fprintf(stderr, "Warning: no /etc/fstab entry found\n");
out:
#endif NFS
	if (vflag)
		fprintf(stderr, "%s: Unmounted\n", name);
	for (mp = mtab; mp < &mtab[NMOUNT]; mp++) {
		if (strncmp(mp->m_path, name, sizeof (mp->m_path)))
			continue;
		*mp = zeromtab;
		for (mp = &mtab[NMOUNT]; mp >= mtab; mp--)
			if (mp->m_path[0])
				break;
		mp++;
		mf = creat("/etc/mtab", 0644);
		write(mf, (char *)mtab, (mp-mtab) * sizeof (struct mtab));
		return (1);
	}
	fprintf(stderr, "%s: Not mounted\n", name);
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
