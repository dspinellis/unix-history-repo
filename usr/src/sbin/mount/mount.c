/*
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
static char sccsid[] = "@(#)mount.c	8.10 (Berkeley) %G%";
#endif /* not lint */

#include <sys/param.h>
#include <sys/wait.h>
#include <sys/mount.h>

#include <err.h>
#include <errno.h>
#include <fstab.h>
#include <signal.h>
#include <stdio.h>
#include <stdlib.h>
#include <string.h>
#include <unistd.h>

#include "pathnames.h"


int debug, force, verbose, mnttype, skipvfs;
char *mntname;

int    badvfsname __P((char *, char **));
int    badvfstype __P((int, char **));
int    getexecopts __P((char *, char **));
struct statfs
      *getmntpt __P((char *));
int    getmnttype __P((char *));
void   getstdopts __P((char *, int *));
void   getufsopts __P((char *, int *));
char **makevfslist __P((char *));
int    mountfs __P((char *, char *, int, char *, char *, char *));
void   prmount __P((char *, char *, int));
void   usage __P((void));

int
main(argc, argv)
	int argc;
	char *argv[];
{
	struct fstab *fs;
	struct statfs *mntbuf;
	FILE *pidfile;
	long mntsize;
	int all, ch, i, pid, ret, rval, updateflg;
	char *cp, *type, *options, **vfslist;

	mntname = "ufs";
	mnttype = MOUNT_UFS;

	all = updateflg = 0;
	options = type = NULL;
	vfslist = NULL;
	while ((ch = getopt(argc, argv, "adfo:rwt:uv")) != EOF)
		switch(ch) {
		case 'a':
			all = 1;
			break;
		case 'd':
			debug = 1;
			break;
		case 'f':
			force = 1;
			break;
		case 'o':
			options = optarg;
			break;
		case 'r':
			type = FSTAB_RO;
			break;
		case 't':
			vfslist = makevfslist(optarg);
			mnttype = getmnttype(optarg);
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
		case '?':
		default:
			usage();
			/* NOTREACHED */
		}
	argc -= optind;
	argv += optind;

#define	BADTYPE(type)							\
	(strcmp(type, FSTAB_RO) &&					\
	    strcmp(type, FSTAB_RW) && strcmp(type, FSTAB_RQ))

	if (all) {
		rval = 0;
		while ((fs = getfsent()) != NULL) {
			if (BADTYPE(fs->fs_type))
				continue;
			if (badvfsname(fs->fs_vfstype, vfslist))
				continue;
			/* `/' is special, it's always mounted. */
			mnttype = getmnttype(fs->fs_vfstype);
			rval |= mountfs(fs->fs_spec, fs->fs_file, updateflg,
			    type, options, fs->fs_mntops);
		}
		exit(rval);
	}

	if (argc == 0) {
		if (verbose || debug || type)
			usage();
		if ((mntsize = getmntinfo(&mntbuf, MNT_NOWAIT)) == 0) {
			errx(1, "cannot get mount information");
		}
		for (i = 0; i < mntsize; i++) {
			if (badvfstype(mntbuf[i].f_type, vfslist))
				continue;
			prmount(mntbuf[i].f_mntfromname,
			     mntbuf[i].f_mntonname, mntbuf[i].f_flags);
		}
		exit(0);
	}

	if (argc == 1 && vfslist != NULL)
		usage();

	if (argc == 1 && updateflg) {
		if ((mntbuf = getmntpt(*argv)) == NULL)
			errx(1,
			    "unknown special file or file system %s.", *argv);
		mnttype = mntbuf->f_type;
		if ((fs = getfsfile(mntbuf->f_mntonname)) == NULL)
			errx(1, "can't find fstab entry for %s.", *argv);
		mntname = fs->fs_vfstype;

		/*
		 * Default type to fstab version if none specified on the
		 * command line.
		 */
		if (type == NULL)
			type = fs->fs_type;

		/*
		 * Default options to fstab version if none specified on the
		 * command line.
		 */
		if (options == NULL)
			options = fs->fs_mntops;
		else {
			/*
			 * Concat the two strings with the command line
			 * options last so that they will override the
			 * fstab options.
			 */
			i = strlen(fs->fs_mntops) + strlen(options) + 2;
			if ((cp = malloc((size_t)i)) == NULL)
				errx(1, "-u malloc failed");
			(void)snprintf(cp, i, "%s,%s", fs->fs_mntops, options);
			options = cp;
		}
		ret = mountfs(fs->fs_spec,
		    mntbuf->f_mntonname, updateflg, type, options, NULL);
	} else if (argc == 1) {
		if ((fs = getfsfile(*argv)) == NULL &&
		    (fs = getfsspec(*argv)) == NULL)
			errx(1,
			    "unknown special file or file system %s.\n", *argv);
		if (BADTYPE(fs->fs_type))
			errx(1, "%s has unknown file system type.\n", *argv);
		mnttype = getmnttype(fs->fs_vfstype);
		ret = mountfs(fs->fs_spec,
		    fs->fs_file, updateflg, type, options, fs->fs_mntops);
	} else if (argc != 2) {
		usage();
		ret = 1;
	} else {
		/*
		 * If -t flag has not been specified, and spec contains either
		 * a ':' or a '@' then assume that an NFS filesystem is being
		 * specified ala Sun.
		 */
		if (vfslist == NULL &&
		    (strchr(argv[0], ':') || strchr(argv[0], '@'))) {
			mnttype = MOUNT_NFS;
			mntname = "nfs";
		}
		ret = mountfs(argv[0], argv[1], updateflg, type, options, NULL);
	}
	if ((pidfile = fopen(_PATH_MOUNTDPID, "r")) != NULL) {
		pid = 0;
		(void)fscanf(pidfile, "%ld", &pid);
		(void)fclose(pidfile);
		if (pid > 0 && kill(pid, SIGHUP))
			err(1, "signal mountd");
	}

	exit(ret);
}

int
mountfs(spec, name, flags, type, options, mntopts)
	char *spec, *name, *type, *options, *mntopts;
	int flags;
{
	struct ufs_args args;
	pid_t pid;
	int argc, i, status;
	char *argp, *argv[50];
	char execname[MAXPATHLEN + 1], flagval[12], mntpath[MAXPATHLEN];

	if (mntopts)
		getstdopts(mntopts, &flags);
	if (options)
		getstdopts(options, &flags);
	if (type)
		getstdopts(type, &flags);
	if (force)
		flags |= MNT_FORCE;

	if (realpath(name, mntpath) == 0) {
		warn("%s", mntpath);
		return (1);
	}

	name = mntpath;

	if (strcmp(name, "/") == 0)
		flags |= MNT_UPDATE;

	switch (mnttype) {
	case MOUNT_UFS:
		if (mntopts)
			getufsopts(mntopts, &flags);
		if (options)
			getufsopts(options, &flags);
		args.fspec = spec;
#define	DEFAULT_ROOTUID	-2
		args.export.ex_root = DEFAULT_ROOTUID;
		if (flags & MNT_RDONLY)
			args.export.ex_flags = MNT_EXRDONLY;
		else
			args.export.ex_flags = 0;
		argp = (caddr_t)&args;
		break;
	case MOUNT_MFS:
	case MOUNT_NFS:
	default:
		argv[0] = mntname;
		argc = 1;
		if (flags) {
			argv[argc++] = "-F";
			(void)snprintf(flagval, sizeof(flagval), "%d", flags);
			argv[argc++] = flagval;
		}
		if (mntopts)
			argc += getexecopts(mntopts, &argv[argc]);
		if (options)
			argc += getexecopts(options, &argv[argc]);
		argv[argc++] = spec;
		argv[argc++] = name;
		argv[argc++] = NULL;
		snprintf(execname, sizeof(execname),
		    "%s/mount_%s", _PATH_EXECDIR, mntname);
		if (verbose) {
			(void)printf("exec: %s", execname);
			for (i = 1; i < argc - 1; i++)
				(void)printf(" %s", argv[i]);
			(void)printf("\n");
		}
		if (debug)
			break;
		if (pid = vfork()) {
			if (pid == -1) {
				warn("vfork starting file system");
				return (1);
			}
			if (waitpid(pid, &status, 0) != -1 &&
			    WIFEXITED(status) &&
			    WEXITSTATUS(status) != 0)
				return (WEXITSTATUS(status));
			spec = mntname;
			goto out;
		}
		execv(execname, argv);
		err(1, "cannot exec %s for %s", execname, name);
		/* NOTREACHED */

	}
	if (!debug && mount(mnttype, name, flags, argp)) {
		(void)fprintf(stderr, "%s on %s: ", spec, name);
		switch (errno) {
		case EMFILE:
			(void)fprintf(stderr, "Mount table full\n");
			break;
		case EINVAL:
			if (flags & MNT_UPDATE)
				(void)fprintf(stderr, "Specified device %s\n",
					"does not match mounted device");
			else if (mnttype == MOUNT_UFS)
				(void)fprintf(stderr, "Bogus super block\n");
			else
				perror(NULL);
			break;
		default:
			perror(NULL);
			break;
		}
		return (1);
	}

out:	if (verbose)
		prmount(spec, name, flags);
	return (0);
}

void
prmount(spec, name, flags)
	char *spec, *name;
	int flags;
{
	int first;

	(void)printf("%s on %s", spec, name);
	if (!(flags & MNT_VISFLAGMASK)) {
		(void)printf("\n");
		return;
	}
	first = 0;
#define	PR(msg)	(void)printf("%s%s", !first++ ? " (" : ", ", msg)
	if (flags & MNT_RDONLY)
		PR("read-only");
	if (flags & MNT_NOEXEC)
		PR("noexec");
	if (flags & MNT_NOSUID)
		PR("nosuid");
	if (flags & MNT_NODEV)
		PR("nodev");
	if (flags & MNT_SYNCHRONOUS)
		PR("synchronous");
	if (flags & MNT_ASYNC)
		PR("asynchronous");
	if (flags & MNT_QUOTA)
		PR("with quotas");
	if (flags & MNT_LOCAL)
		PR("local");
	if (flags & MNT_UNION)
		PR("union");
	if (flags & MNT_EXPORTED)
		PR("NFS exported");
	(void)printf(")\n");
}

int
getmnttype(fstype)
	char *fstype;
{

	mntname = fstype;
	return (strcmp(fstype, "ufs") == 0 ? MOUNT_UFS : 0);
}

void
getstdopts(options, flagp)
	char *options;
	int *flagp;
{
	int negative;
	char *opt, optbuf[BUFSIZ];

	(void)strcpy(optbuf, options);
	for (opt = strtok(optbuf, ","); opt; opt = strtok(NULL, ",")) {
		if (opt[0] == '-')
			continue;
		if (opt[0] == 'n' && opt[1] == 'o') {
			negative++;
			opt += 2;
		} else
			negative = 0;
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
		if (!strcasecmp(opt, "asynchronous")) {
			if (!negative)
				*flagp |= MNT_ASYNC;
			else
				*flagp &= ~MNT_ASYNC;
			continue;
		}
		if (!strcasecmp(opt, "union")) {
			if (!negative)
				*flagp |= MNT_UNION;
			else
				*flagp &= ~MNT_UNION;
			continue;
		}
		(void)fprintf(stderr, "mount: %s: unknown option\n", opt);
	}
}

/* ARGSUSED */
void
getufsopts(options, flagp)
	char *options;
	int *flagp;
{

	return;
}

int
getexecopts(options, argv)
	char *options, **argv;
{
	int argc;
	char *opt;

	argc = 0;
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
	struct statfs *mntbuf;
	long i, mntsize;

	mntsize = getmntinfo(&mntbuf, MNT_NOWAIT);
	for (i = 0; i < mntsize; i++) {
		if (!strcmp(mntbuf[i].f_mntfromname, name) ||
		    !strcmp(mntbuf[i].f_mntonname, name))
			return (&mntbuf[i]);
	}
	return (NULL);
}

int
badvfstype(vfstype, vfslist)
	int vfstype;
	char **vfslist;
{

	if (vfslist == NULL)
		return (0);
	while (*vfslist != NULL) {
		if (vfstype == getmnttype(*vfslist))
			return (skipvfs);
		vfslist++;
	}
	return (!skipvfs);
}

int
badvfsname(vfsname, vfslist)
	char *vfsname;
	char **vfslist;
{

	if (vfslist == NULL)
		return (0);
	while (*vfslist != NULL) {
		if (strcmp(vfsname, *vfslist) == 0)
			return (skipvfs);
		vfslist++;
	}
	return (!skipvfs);
}

char **
makevfslist(fslist)
	char *fslist;
{
	int i;
	char **av, *nextcp;

	if (fslist == NULL)
		return (NULL);
	if (fslist[0] == 'n' && fslist[1] == 'o') {
		fslist += 2;
		skipvfs = 1;
	}
	for (i = 0, nextcp = fslist; *nextcp; nextcp++)
		if (*nextcp == ',')
			i++;
	av = malloc((size_t)(i + 2) * sizeof(char *));
	if (av == NULL)
		return (NULL);
	nextcp = fslist;
	i = 0;
	av[i++] = nextcp;
	while ((nextcp = index(nextcp, ',')) != NULL) {
		*nextcp++ = '\0';
		av[i++] = nextcp;
	}
	av[i++] = NULL;
	return (av);
}

void
usage()
{

	(void)fprintf(stderr,
		"usage:\n  mount %s %s\n  mount %s\n  mount %s\n",
		"[ -frwu ] [ -t ufs | external_type ]",
		"[ -o options ] special node",
		"[ -afrwu ] [ -t ufs | external_type ]",
		"[ -frwu ] special | node");
	exit(1);
}
