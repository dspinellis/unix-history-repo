/*
 * Copyright (c) 1988 The Regents of the University of California.
 * All rights reserved.
 *
 * This code is derived from software contributed to Berkeley by
 * David Hitz of Auspex Systems Inc.
 *
 * %sccs.include.redist.c%
 */

#ifndef lint
char copyright[] =
"@(#) Copyright (c) 1988 The Regents of the University of California.\n\
 All rights reserved.\n";
#endif /* not lint */

#ifndef lint
static char sccsid[] = "@(#)cp.c	5.20 (Berkeley) %G%";
#endif /* not lint */

/*
 * cp copies source files to target files.
 * 
 * The global PATH_T structures "to" and "from" always contain paths to the
 * current source and target files, respectively.  Since cp does not change
 * directories, these paths can be either absolute or dot-realative.
 * 
 * The basic algorithm is to initialize "to" and "from", and then call the
 * recursive copy() function to do the actual work.  If "from" is a file,
 * copy copies the data.  If "from" is a directory, copy creates the
 * corresponding "to" directory, and calls itself recursively on all of
 * the entries in the "from" directory.
 */

#include <sys/param.h>
#include <sys/stat.h>
#include <sys/file.h>
#include <sys/dir.h>
#include <sys/time.h>
#include <stdio.h>
#include <errno.h>
#include <stdlib.h>
#include <string.h>

#define	type(st)	((st).st_mode & S_IFMT)

typedef struct {
	char	p_path[MAXPATHLEN + 1];	/* pointer to the start of a path. */
	char	*p_end;			/* pointer to NULL at end of path. */
} PATH_T;

PATH_T from = { "", from.p_path };
PATH_T to = { "", to.p_path };

uid_t myuid;
int exit_val, myumask;
int iflag, pflag, orflag, rflag;
int (*statfcn)();
char *buf, *pname;
char *path_append(), *path_basename();

main(argc, argv)
	int argc;
	char **argv;
{
	extern int optind;
	struct stat to_stat;
	register int c, r;
	int symfollow, lstat(), stat();
	char *old_to, *p;

	/*
	 * cp is used by mv(1) -- except for usage statements, print
	 * the "called as" program name.
	 */
	pname = (p = rindex(*argv,'/')) ? ++p : *argv;

	symfollow = 0;
	while ((c = getopt(argc, argv, "Rfhipr")) != EOF) {
	switch ((char)c) {
		case 'f':
			iflag = 0;
			break;
		case 'h':
			symfollow = 1;
			break;
		case 'i':
			iflag = isatty(fileno(stdin));
			break;
		case 'p':
			pflag = 1;
			break;
		case 'R':
			rflag = 1;
			break;
		case 'r':
			orflag = 1;
			break;
		case '?':
		default:
			usage();
			break;
		}
	}
	argc -= optind;
	argv += optind;

	if (argc < 2)
		usage();

	if (rflag && orflag) {
		(void)fprintf(stderr,
		    "cp: the -R and -r options are mutually exclusive.\n");
		exit(1);
	}

	buf = (char *)malloc(MAXBSIZE);
	if (!buf) {
		(void)fprintf(stderr, "%s: out of space.\n", pname);
		exit(1);
	}

	myuid = getuid();

	/* copy the umask for explicit mode setting */
	myumask = umask(0);
	(void)umask(myumask);

	/* consume last argument first. */
	if (!path_set(&to, argv[--argc]))
		exit(exit_val);

	statfcn = symfollow || !rflag ? stat : lstat;

	/*
	 * Cp has two distinct cases:
	 *
	 * Case (1)	  $ cp [-rip] source target
	 *
	 * Case (2)	  $ cp [-rip] source1 ... directory
	 *
	 * In both cases, source can be either a file or a directory.
	 *
	 * In (1), the target becomes a copy of the source. That is, if the
	 * source is a file, the target will be a file, and likewise for
	 * directories.
	 *
	 * In (2), the real target is not directory, but "directory/source".
	 */

	r = stat(to.p_path, &to_stat);
	if (r == -1 && errno != ENOENT) {
		error(to.p_path);
		exit(1);
	}
	if (r == -1 || type(to_stat) != S_IFDIR) {
		/*
		 * Case (1).  Target is not a directory.
		 */
		if (argc > 1) {
			usage();
			exit(1);
		}
		if (!path_set(&from, *argv))
			exit(exit_val);
		copy();
	}
	else {
		/*
		 * Case (2).  Target is a directory.
		 */
		for (;; ++argv) {
			if (!path_set(&from, *argv))
				continue;
			old_to = path_append(&to, path_basename(&from), -1);
			if (!old_to)
				continue;
			copy();
			if (!--argc)
				break;
			path_restore(&to, old_to);
		}
	}
	exit(exit_val);
}

/* copy file or directory at "from" to "to". */
copy()
{
	struct stat from_stat, to_stat;
	int dne, statval;

	statval = statfcn(from.p_path, &from_stat);
	if (statval == -1) {
		error(from.p_path);
		return;
	}

	/* not an error, but need to remember it happened */
	if (stat(to.p_path, &to_stat) == -1)
		dne = 1;
	else {
		if (to_stat.st_dev == from_stat.st_dev &&
		    to_stat.st_ino == from_stat.st_ino) {
			(void)fprintf(stderr,
			    "%s: %s and %s are identical (not copied).\n",
			    pname, to.p_path, from.p_path);
			exit_val = 1;
			return;
		}
		dne = 0;
	}

	switch(type(from_stat)) {
	case S_IFLNK:
		copy_link(!dne);
		return;
	case S_IFDIR:
		if (!rflag && !orflag) {
			(void)fprintf(stderr,
			    "%s: %s is a directory (not copied).\n",
			    pname, from.p_path);
			exit_val = 1;
			return;
		}
		if (dne) {
			/*
			 * If the directory doesn't exist, create the new
			 * one with the from file mode plus owner RWX bits,
			 * modified by the umask.  Trade-off between being
			 * able to write the directory (if from directory is
			 * 555) and not causing a permissions race.  If the
			 * umask blocks owner writes cp fails.
			 */
			if (mkdir(to.p_path, from_stat.st_mode|S_IRWXU) < 0) {
				error(to.p_path);
				return;
			}
		}
		else if (type(to_stat) != S_IFDIR) {
			(void)fprintf(stderr, "%s: %s: not a directory.\n",
			    pname, to.p_path);
			return;
		}
		copy_dir();
		/*
		 * If not -p and directory didn't exist, set it to be the
		 * same as the from directory, umodified by the umask;
		 * arguably wrong, but it's been that way forever.
		 */
		if (pflag)
			setfile(&from_stat, 0);
		else if (dne)
			(void)chmod(to.p_path, from_stat.st_mode);
		return;
	case S_IFCHR:
	case S_IFBLK:
		if (rflag) {
			copy_special(&from_stat, !dne);
			return;
		}
		break;
	case S_IFIFO:
		if (rflag) {
			copy_fifo(&from_stat, !dne);
			return;
		}
		break;
	}
	copy_file(&from_stat, dne);
}

copy_file(fs, dne)
	struct stat *fs;
	int dne;
{
	register int from_fd, to_fd, rcount, wcount;
	struct stat to_stat;

	if ((from_fd = open(from.p_path, O_RDONLY, 0)) == -1) {
		error(from.p_path);
		return;
	}

	/*
	 * If the file exists and we're interactive, verify with the user.
	 * If the file DNE, set the mode to be the from file, minus setuid
	 * bits, modified by the umask; arguably wrong, but it makes copying
	 * executables work right and it's been that way forever.  (The
	 * other choice is 666 or'ed with the execute bits on the from file
	 * modified by the umask.)
	 */
	if (!dne) {
		if (iflag) {
			int checkch, ch;

			(void)fprintf(stderr, "overwrite %s? ", to.p_path);
			checkch = ch = getchar();
			while (ch != '\n' && ch != EOF)
				ch = getchar();
			if (checkch != 'y') {
				(void)close(from_fd);
				return;
			}
		}
		to_fd = open(to.p_path, O_WRONLY|O_TRUNC, 0);
	} else
		to_fd = open(to.p_path, O_WRONLY|O_CREAT|O_TRUNC,
		    fs->st_mode & ~(S_ISUID|S_ISGID));

	if (to_fd == -1) {
		error(to.p_path);
		(void)close(from_fd);
		return;
	}

	while ((rcount = read(from_fd, buf, MAXBSIZE)) > 0) {
		wcount = write(to_fd, buf, rcount);
		if (rcount != wcount || wcount == -1) {
			error(to.p_path);
			break;
		}
	}
	if (rcount < 0)
		error(from.p_path);
	if (pflag)
		setfile(fs, to_fd);
	/*
	 * If the source was setuid or setgid, lose the bits unless the
	 * copy is owned by the same user and group.
	 */
	else if (fs->st_mode & (S_ISUID|S_ISGID) && fs->st_uid == myuid)
		if (fstat(to_fd, &to_stat))
			error(to.p_path);
#define	RETAINBITS	(S_ISUID|S_ISGID|S_ISVTX|S_IRWXU|S_IRWXG|S_IRWXO)
		else if (fs->st_gid == to_stat.st_gid && fchmod(to_fd,
		    fs->st_mode & RETAINBITS & ~myumask))
			error(to.p_path);
	(void)close(from_fd);
	(void)close(to_fd);
}

copy_dir()
{
	struct stat from_stat;
	struct direct *dp, **dir_list;
	register int dir_cnt, i;
	char *old_from, *old_to;

	dir_cnt = scandir(from.p_path, &dir_list, NULL, NULL);
	if (dir_cnt == -1) {
		(void)fprintf(stderr, "%s: can't read directory %s.\n",
		    pname, from.p_path);
		exit_val = 1;
	}

	/*
	 * Instead of handling directory entries in the order they appear
	 * on disk, do non-directory files before directory files.
	 * There are two reasons to do directories last.  The first is
	 * efficiency.  Files tend to be in the same cylinder group as
	 * their parent, whereas directories tend not to be.  Copying files
	 * all at once reduces seeking.  Second, deeply nested tree's
	 * could use up all the file descriptors if we didn't close one
	 * directory before recursivly starting on the next.
	 */
	/* copy files */
	for (i = 0; i < dir_cnt; ++i) {
		dp = dir_list[i];
		if (dp->d_namlen <= 2 && dp->d_name[0] == '.'
		    && (dp->d_name[1] == NULL || dp->d_name[1] == '.'))
			goto done;
		old_from = path_append(&from, dp->d_name, (int)dp->d_namlen);
		if (!old_from)
			goto done;

		if (statfcn(from.p_path, &from_stat) < 0) {
			error(dp->d_name);
			path_restore(&from, old_from);
			goto done;
		}
		if (type(from_stat) == S_IFDIR) {
			path_restore(&from, old_from);
			continue;
		}
		old_to = path_append(&to, dp->d_name, (int)dp->d_namlen);
		if (old_to) {
			copy();
			path_restore(&to, old_to);
		}
		path_restore(&from, old_from);
done:		dir_list[i] = NULL;
		(void)free((void *)dp);
	}

	/* copy directories */
	for (i = 0; i < dir_cnt; ++i) {
		dp = dir_list[i];
		if (!dp)
			continue;
		old_from = path_append(&from, dp->d_name, (int) dp->d_namlen);
		if (!old_from) {
			(void)free((void *)dp);
			continue;
		}
		old_to = path_append(&to, dp->d_name, (int) dp->d_namlen);
		if (!old_to) {
			(void)free((void *)dp);
			path_restore(&from, old_from);
			continue;
		}
		copy();
		free((void *)dp);
		path_restore(&from, old_from);
		path_restore(&to, old_to);
	}
	free((void *)dir_list);
}

copy_link(exists)
	int exists;
{
	int len;
	char link[MAXPATHLEN];

	if ((len = readlink(from.p_path, link, sizeof(link))) == -1) {
		error(from.p_path);
		return;
	}
	link[len] = '\0';
	if (exists && unlink(to.p_path)) {
		error(to.p_path);
		return;
	}
	if (symlink(link, to.p_path)) {
		error(link);
		return;
	}
}

copy_fifo(from_stat, exists)
	struct stat *from_stat;
	int exists;
{
	if (exists && unlink(to.p_path)) {
		error(to.p_path);
		return;
	}
	if (mkfifo(to.p_path, from_stat->st_mode)) {
		error(to.p_path);
		return;
	}
	if (pflag)
		setfile(from_stat, 0);
}

copy_special(from_stat, exists)
	struct stat *from_stat;
	int exists;
{
	if (exists && unlink(to.p_path)) {
		error(to.p_path);
		return;
	}
	if (mknod(to.p_path, from_stat->st_mode,  from_stat->st_rdev)) {
		error(to.p_path);
		return;
	}
	if (pflag)
		setfile(from_stat, 0);
}

setfile(fs, fd)
	register struct stat *fs;
	int fd;
{
	static struct timeval tv[2];

	fs->st_mode &= S_ISUID|S_ISGID|S_IRWXU|S_IRWXG|S_IRWXO;

	tv[0].tv_sec = fs->st_atime;
	tv[1].tv_sec = fs->st_mtime;
	if (utimes(to.p_path, tv))
		error(to.p_path);
	/*
	 * Changing the ownership probably won't succeed, unless we're root
	 * or POSIX_CHOWN_RESTRICTED is not set.  Set uid/gid before setting
	 * the mode; current BSD behavior is to remove all setuid bits on
	 * chown.  If chown fails, lose setuid/setgid bits.
	 */
	if (fd ? fchown(fd, fs->st_uid, fs->st_gid) :
	    chown(to.p_path, fs->st_uid, fs->st_gid)) {
		if (errno != EPERM)
			error(to.p_path);
		fs->st_mode &= ~(S_ISUID|S_ISGID);
	}
	if (fd ? fchmod(fd, fs->st_mode) : chmod(to.p_path, fs->st_mode))
		error(to.p_path);
}

ismember(gid)
	gid_t gid;
{
	register int cnt;
	static int ngroups, groups[NGROUPS];

	if (!ngroups) {
		ngroups = getgroups(NGROUPS, groups);
		if (ngroups == -1) {
			ngroups = 0;
			exit_val = 1;
			(void)fprintf(stderr, "%s: %s\n",
			    pname, strerror(errno));
			return(0);
		}
	}
	for (cnt = ngroups; cnt--;)
		if (gid == groups[cnt])
			return(1);
	return(0);
}

error(s)
	char *s;
{
	exit_val = 1;
	(void)fprintf(stderr, "%s: %s: %s\n", pname, s, strerror(errno));
}

/********************************************************************
 * Path Manipulation Routines.
 ********************************************************************/

/*
 * These functions manipulate paths in PATH_T structures.
 * 
 * They eliminate multiple slashes in paths when they notice them, and keep
 * the path non-slash terminated.
 *
 * Both path_set() and path_append() return 0 if the requested name
 * would be too long.
 */

#define	STRIP_TRAILING_SLASH(p) { \
	while ((p)->p_end > (p)->p_path && (p)->p_end[-1] == '/') \
		*--(p)->p_end = 0; \
	}

/*
 * Move specified string into path.  Convert "" to "." to handle BSD
 * semantics for a null path.  Strip trailing slashes.
 */
path_set(p, string)
	register PATH_T *p;
	char *string;
{
	if (strlen(string) > MAXPATHLEN) {
		(void)fprintf(stderr, "%s: %s: name too long.\n",
		    pname, string);
		exit_val = 1;
		return(0);
	}

	(void)strcpy(p->p_path, string);
	p->p_end = p->p_path + strlen(p->p_path);

	if (p->p_path == p->p_end) {
		*p->p_end++ = '.';
		*p->p_end = 0;
	}

	STRIP_TRAILING_SLASH(p);
	return(1);
}

/*
 * Append specified string to path, inserting '/' if necessary.  Return a
 * pointer to the old end of path for restoration.
 */
char *
path_append(p, name, len)
	register PATH_T *p;
	char *name;
	int len;
{
	char *old;

	old = p->p_end;
	if (len == -1)
		len = strlen(name);

	/*
	 * The final "+ 1" accounts for the '/' between old path and name.
	 */
	if ((len + p->p_end - p->p_path + 1) > MAXPATHLEN) {
		(void)fprintf(stderr,
		    "%s: %s/%s: name too long.\n", pname, p->p_path, name);
		exit_val = 1;
		return(0);
	}

	/*
	 * This code should always be executed, since paths shouldn't
	 * end in '/'.
	 */
	if (p->p_end[-1] != '/') {
		*p->p_end++ = '/';
		*p->p_end = 0;
	}

	(void)strncat(p->p_end, name, len);
	p->p_end += len;
	*p->p_end = 0;

	STRIP_TRAILING_SLASH(p);
	return(old);
}

/*
 * Restore path to previous value.  (As returned by path_append.)
 */
path_restore(p, old)
	PATH_T *p;
	char *old;
{
	p->p_end = old;
	*p->p_end = 0;
}

/*
 * Return basename of path.  (Like basename(1).)
 */
char *
path_basename(p)
	PATH_T *p;
{
	char *basename;

	basename = rindex(p->p_path, '/');
	return(basename ? ++basename : p->p_path);
}

usage()
{
	(void)fprintf(stderr,
"usage: cp [-Rfhip] src target;\n   or: cp [-Rfhip] src1 ... srcN directory\n");
	exit(1);
}
