/*
 * Copyright (c) 1988, 1993
 *	The Regents of the University of California.  All rights reserved.
 *
 * This code is derived from software contributed to Berkeley by
 * David Hitz of Auspex Systems Inc.
 *
 * %sccs.include.redist.c%
 */

#ifndef lint
static char copyright[] =
"@(#) Copyright (c) 1988, 1993\n\
	The Regents of the University of California.  All rights reserved.\n";
#endif /* not lint */

#ifndef lint
static char sccsid[] = "@(#)cp.c	8.1 (Berkeley) %G%";
#endif /* not lint */

/*
 * Cp copies source files to target files.
 * 
 * The global PATH_T structure "to" always contains the path to the
 * current target file.  Since fts(3) does not change directories,
 * this path can be either absolute or dot-realative.
 * 
 * The basic algorithm is to initialize "to" and use fts(3) to traverse
 * the file hierarchy rooted in the argument list.  A trivial case is the
 * case of 'cp file1 file2'.  The more interesting case is the case of
 * 'cp file1 file2 ... fileN dir' where the hierarchy is traversed and the
 * path (relative to the root of the traversal) is appended to dir (stored
 * in "to") to form the final target path.
 */

#include <sys/param.h>
#include <sys/stat.h>
#include <sys/mman.h>
#include <sys/time.h>

#include <dirent.h>
#include <fcntl.h>
#include <errno.h>
#include <unistd.h>
#include <stdio.h>
#include <stdlib.h>
#include <string.h>
#include <fts.h>
#include "extern.h"

#define	STRIP_TRAILING_SLASH(p) {					\
        while ((p).p_end > (p).p_path && (p).p_end[-1] == '/')		\
                *--(p).p_end = 0;					\
}

static void	copy __P((FTS *));
static int	mastercmp __P((const FTSENT **, const FTSENT **));

PATH_T to = { to.p_path, "" };

uid_t myuid;
int exit_val, myumask;
int iflag, orflag, pflag, rflag;
char *progname;

static enum { FILE_TO_FILE, FILE_TO_DIR, DIR_TO_DNE } type;

int
main(argc, argv)
	int argc;
	char *argv[];
{
	struct stat to_stat, tmp_stat;
	FTS *ftsp;
	register int c, r;
	int fts_options, Hflag, hflag;
	char *p, *target;

	/*
	 * The utility cp(1) is used by mv(1) -- except for usage statements,
	 * print the "called as" program name.
	 */
	progname = (p = rindex(*argv,'/')) ? ++p : *argv;

	/*
         * Symbolic link handling is as follows:
         * 1.  Follow all symbolic links on the argument line.
         * 2.  Otherwise, don't follow symbolic links UNLESS options -h 
         *     (in conjuction with -R) or -r (for backward compatibility) are 
         *     set, in which case follow all symbolic links, or when the -H
         *     option is set (in conjuction with -R), in which case follow 
         *     all symbolic links on the command line.
         * 
         */
	Hflag = hflag = 0;
	fts_options = FTS_NOCHDIR | FTS_LOGICAL;
	while ((c = getopt(argc, argv, "HRfhipr")) != EOF) 
		switch ((char)c) {
		case 'H':
			Hflag = 1;
			fts_options |= FTS_COMFOLLOW;
			break;
		case 'f':
			iflag = 0;
			break;
		case 'h':
			hflag = 1;
			break;
		case 'i':
			iflag = isatty(fileno(stdin));
			break;
		case 'p':
			pflag = 1;
			break;
		case 'R':
			fts_options &= ~FTS_LOGICAL;
			fts_options |= FTS_PHYSICAL;
			rflag = 1;
			break;
		case 'r':
			orflag = 1;
			fts_options &= ~FTS_PHYSICAL;
			fts_options |= FTS_LOGICAL;
			break;
		case '?':
		default:
			usage();
			break;
		}
	argc -= optind;
	argv += optind;

	if (argc < 2)
		usage();

	if (orflag) {
		if (rflag) {
			(void)fprintf(stderr,
	    "cp: the -R and -r options are mutually exclusive.\n");
			exit(1);
		}
		if (Hflag || hflag) {
			(void)fprintf(stderr,
	    "cp: the -r and the -H and -h options are mutually exclusive.\n");
			exit(1);
		}
	}

	if (hflag) {
		fts_options &= ~FTS_PHYSICAL;
		fts_options |= FTS_LOGICAL;
	}

	myuid = getuid();

	/* Copy the umask for explicit mode setting. */
	myumask = umask(0);
	(void)umask(myumask);

	/* Save the target base in "to". */
	target = argv[--argc];
	if (strlen(target) > MAXPATHLEN) {
		err("%s: name too long", target);
		exit(1);
	}
	(void)strcpy(to.p_path, target);
	to.p_end = to.p_path + strlen(to.p_path);
        if (to.p_path == to.p_end) {
		*to.p_end++ = '.';
		*to.p_end = 0;
	}
        STRIP_TRAILING_SLASH(to);
	to.target_end = to.p_end;

	/* Set end of argument list for fts(3). */
	argv[argc] = NULL;     
	
	/*
	 * Cp has two distinct cases:
	 *
	 * cp [-R] source target
	 * cp [-R] source1 ... sourceN directory
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
		err("%s: %s", to.p_path, strerror(errno));
		exit(1);
	}
	if (r == -1 || !S_ISDIR(to_stat.st_mode)) {
		/*
		 * Case (1).  Target is not a directory.
		 */ 
		if (argc > 1) {
			usage();
			exit(1);
		}
		/*
		 * Need to detect the case:
		 *	cp -R dir foo
		 * Where dir is a directory and foo does not exist, where
		 * we want pathname concatenations turned on but not for
		 * the initial mkdir().
		 */
		if (r == -1) {
			if (orflag || (rflag && (hflag || Hflag)))
				stat(*argv, &tmp_stat);
			else
				lstat(*argv, &tmp_stat);
			
			if (S_ISDIR(tmp_stat.st_mode) && (rflag || orflag))
				type = DIR_TO_DNE;
			else
				type = FILE_TO_FILE;
		} else
			type = FILE_TO_FILE;
	} else
		/*
		 * Case (2).  Target is a directory.
		 */
		type = FILE_TO_DIR;

	if ((ftsp = fts_open(argv, fts_options, mastercmp)) == NULL) {
		err("%s", strerror(errno)); 
	        exit(1);
	}
	copy(ftsp);
	fts_close(ftsp);

	exit(exit_val);
}

static void
copy(ftsp)
	FTS *ftsp;
{
	register FTSENT *curr;
	register int base, nlen;
	struct stat to_stat;
	int dne;
	char *c, *n;

	while (curr = fts_read(ftsp)) {
		switch(curr->fts_info) {
		case FTS_NS:
		case FTS_ERR:
			err("%s: %s",
			    curr->fts_path, strerror(curr->fts_errno));
			exit_val = 1;
			continue;
		case FTS_DC:
			err("%s: directory causes a cycle", curr->fts_path);
			exit_val = 1;
			continue;
		case FTS_DP:
			continue;
		}

		/*
		 * If we are in case (2) or (3) above, we need to append the 
                 * source name to the target name.  
                 */
		if (type != FILE_TO_FILE) {
			if ((curr->fts_namelen +
			    to.target_end - to.p_path + 1) > MAXPATHLEN) {
				err("%s/%s: name too long (not copied)", 
				    to.p_path, curr->fts_name);
				continue;
			}

			/*
			 * Need to remember the roots of traversals to create
			 * correct pathnames.  If there's a directory being
			 * copied to a non-existent directory, e.g.
			 *	cp -R a/dir noexist
			 * the resulting path name should be noexist/foo, not
			 * noexist/dir/foo (where foo is a file in dir), which
			 * is the case where the target exists.
			 *
			 * Also, check for "..".  This is for correct path
			 * concatentation for paths ending in "..", e.g.
			 *	cp -R .. /tmp
			 * Paths ending in ".." are changed to ".".  This is
			 * tricky, but seems the easiest way to fix the problem.
			 */
			if (curr->fts_level == FTS_ROOTLEVEL)
				if (type != DIR_TO_DNE) {
					c = rindex(curr->fts_path, '/');
					base = (c == NULL) ? 0 : 
						(int) (c - curr->fts_path + 1);

					if (!strcmp(&curr->fts_path[base], 
					    ".."))
						base += 1;
				} else
					base = curr->fts_pathlen;

			if (to.target_end[-1] != '/') {
				*to.target_end = '/';
				*(to.target_end + 1) = 0;
			}
			n = &curr->fts_path[base];
			nlen = curr->fts_pathlen - base;

			(void)strncat(to.target_end + 1, n, nlen);
			to.p_end = to.target_end + nlen + 1;
			*to.p_end = 0;
			STRIP_TRAILING_SLASH(to);
		}

		/* Not an error but need to remember it happened */
		if (stat(to.p_path, &to_stat) == -1)
			dne = 1;
		else {
			if (to_stat.st_dev == curr->fts_statp->st_dev &&
			    to_stat.st_ino == curr->fts_statp->st_ino) {
				(void)fprintf(stderr,
			    "%s: %s and %s are identical (not copied).\n",
				    progname, to.p_path, curr->fts_path);
				exit_val = 1;
				if (S_ISDIR(curr->fts_statp->st_mode))
					(void)fts_set(ftsp, curr, FTS_SKIP);
				continue;
			}
			dne = 0;
		}

		switch (curr->fts_statp->st_mode & S_IFMT) {
		case S_IFLNK:
			copy_link(curr, !dne);
			break;
		case S_IFDIR:
			if (!rflag && !orflag) {
				(void)fprintf(stderr,
				    "%s: %s is a directory (not copied).\n",
				    progname, curr->fts_path);
				(void)fts_set(ftsp, curr, FTS_SKIP);
				exit_val = 1;
				break;
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
				if (mkdir(to.p_path, 
				    curr->fts_statp->st_mode|S_IRWXU) < 0) {
					err("%s: %s", to.p_path, 
					    strerror(errno));
					return;
		                }
			} else if (!S_ISDIR(to_stat.st_mode)) {
				(void)fprintf(stderr, 
				    "%s: %s: not a directory.\n", progname, 
				    to.p_path);
				return;
			}
			/*
			 * If not -p and directory didn't exist, set it to be
			 * the same as the from directory, umodified by the 
                         * umask; arguably wrong, but it's been that way 
                         * forever.
			 */
			if (pflag)
				setfile(curr->fts_statp, 0);
			else if (dne)
				(void)chmod(to.p_path, 
				    curr->fts_statp->st_mode);
			break;
		case S_IFCHR:
		case S_IFBLK:
			if (rflag)
				copy_special(curr->fts_statp, !dne);
			else
				copy_file(curr, dne);
			break;
		case S_IFIFO:
			if (rflag)
				copy_fifo(curr->fts_statp, !dne);
			else 
				copy_file(curr, dne);
			break;
		default:
			copy_file(curr, dne);
			break;
		}
	}
}

/*
 * mastercmp --
 *	The comparison function for the copy order.  The order is to copy
 *	non-directory files before directory files.  The reason for this
 *	is because files tend to be in the same cylinder group as their
 *	parent directory, whereas directories tend not to be.  Copying the
 *	files first reduces seeking.
 */
static int
mastercmp(a, b)
	const FTSENT **a, **b;
{
	register int a_info, b_info;

	a_info = (*a)->fts_info;
	if (a_info == FTS_ERR || a_info == FTS_NS || a_info == FTS_DNR)
		return (0);
	b_info = (*b)->fts_info;
	if (b_info == FTS_ERR || b_info == FTS_NS || b_info == FTS_DNR)
		return (0);
	if (a_info == FTS_D)
		return (-1);
	if (b_info == FTS_D)
		return (1);
	return (0);
}
