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
static char sccsid[] = "@(#)cp.c	5.28 (Berkeley) %G%";
#endif /* not lint */

/*
 * cp copies source files to target files.
 * 
 * The global PATH_T structures "to" always containa the path to the
 * current target file, respectively.  Since fts(3) does not change
 * directories, this path can be either absolute or dot-realative.
 * 
 * The basic algorithm is to initialize "to" and use fts(3) to traverse
 * the file hierarchy rooted in the argument list.  A trivial case is the
 * case of 'cp file1 file2'. The more interesing case is the case of
 * 'cp file1 file2 ... fileN dir' where the hierarchy is traversed and the
 * path (relative to the root of the traversal) is appended to dir 
 * (stored in "to") to form the final target path.
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


#define FILE_TO_FILE 1
#define FILE_TO_DIR  2

#define STRIP_TRAILING_SLASH(p) { \
        while ((p).p_end > (p).p_path && (p).p_end[-1] == '/') \
                *--(p).p_end = 0; \
		}

static void copy __P((int, FTS *));
static int mastercmp __P((const FTSENT **, const FTSENT **));
PATH_T to = { to.p_path, "" };

uid_t myuid;
int exit_val, myumask;
int iflag, pflag;
char *progname;

static int rflag, orflag;
int
main(argc, argv)
	int argc;
	char **argv;
{
	extern int optind;
	struct stat to_stat;
	register int c, r;
	char *p;
	char *target;
	int fts_options;
	int type;
	FTS *ftsp;

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
         *     set.
         */
	rflag = orflag = 0;
	fts_options = FTS_NOCHDIR | FTS_PHYSICAL;
	while ((c = getopt(argc, argv, "HRfhipr")) != EOF) 
		switch ((char)c) {
		case 'H':
			fts_options |= FTS_COMFOLLOW;
			break;
		case 'f':
			iflag = 0;
			break;
		case 'h':
			fts_options &= ~FTS_PHYSICAL;
			fts_options |= FTS_LOGICAL;
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

	if (rflag && orflag) {
		(void)fprintf(stderr,
		    "cp: the -R and -r options are mutually exclusive.\n");
		exit(1);
	}

	myuid = getuid();

	/* copy the umask for explicit mode setting */
	myumask = umask(0);
	(void)umask(myumask);

	/* Save the target base in "to" */
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

	/* Want to know when end of list for fts. */
	argv[argc] = NULL;     
	
	/*
	 * Cp has two distinct cases:
	 *
	 * % cp [-rip] source target
	 * % cp [-rip] source1 ... directory
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
		type = FILE_TO_FILE;
	}
	else
		/*
		 * Case (2).  Target is a directory.
		 */
		type = FILE_TO_DIR;

	if ((ftsp = fts_open(argv, fts_options, mastercmp)) == NULL) {
		err("%s", strerror(errno)); 
	        exit(1);
	}
	copy(type, ftsp);
	fts_close(ftsp);

	exit(exit_val);
}

static void
copy(type, ftsp)
	int type;
	FTS *ftsp;
{
	struct stat to_stat;
	int dne;
	register FTSENT *curr;
	
	while(curr = fts_read(ftsp)) {
		if (curr->fts_info == FTS_ERR || curr->fts_info == FTS_NS) {
			err("%s:%s", curr->fts_path, curr->fts_errno);
			exit_val = 1;
			return;
		}
		if (curr->fts_info == FTS_DP)
			continue;
		if (curr->fts_info == FTS_DC) {
			err("%s: directory causes a cycle", curr->fts_path);
			exit_val = 1;
			return;
		}

		/*
		 * If we are in case(2) above, we need to append the 
                 * source name to the target name.  
                 */
		if (type == FILE_TO_DIR) {
			if ((curr->fts_namelen + to.target_end - to.p_path + 1)
			    > MAXPATHLEN) {
				err("%s/%s: name too long (not copied)", 
				    to.p_path, curr->fts_name);
				continue;
			}
			if (to.target_end[-1] != '/') {
				*to.target_end = '/';
				*(to.target_end + 1) = 0;
			}
			(void)strncat(to.target_end + 1, curr->fts_name, 
                            curr->fts_namelen);
			to.p_end = to.target_end + curr->fts_namelen + 1;
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
				return;
			}
			dne = 0;
		}

		switch(curr->fts_statp->st_mode & S_IFMT) {
		case S_IFLNK:
			copy_link(curr, !dne);
			break;
		case S_IFDIR:
			if (!rflag && !orflag) {
				(void)fprintf(stderr,
				    "%s: %s is a directory (not copied).\n",
				    progname, curr->fts_path);
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
				if (mkdir(to.p_path, 
				    curr->fts_statp->st_mode|S_IRWXU) < 0) {
					err("%s: %s", to.p_path, 
					    strerror(errno));
					return;
		                }
			}
			else if (!S_ISDIR(to_stat.st_mode)) {
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
			if (rflag) {
				copy_special(curr->fts_statp, !dne);
				return;
			}
			break;
		case S_IFIFO:
			if (rflag) {
				copy_fifo(curr->fts_statp, !dne);
				return;
			}
			break;
		default:
			copy_file(curr, dne);
			break;
		}
	}
}

/*
 * Mastercmp() is the comparison function for the copy order.  The order is
 * to copy non-directory files before directory files.  There are two reasons
 * to do directories last.  The first is efficiency.  Files tend to be in the 
 * same cylinder group as their parent, whereas directories tend not to be.
 * Copying files all at once reduces seeking.  Second, deeply nested trees
 * could use up all the file descriptors if we didn't close one directory 
 * before recursively starting on the next.
 */

static int
mastercmp(a, b)
	const FTSENT **a, **b;
{
	int a_info, b_info;

	a_info = (*a)->fts_info;
	if (a_info == FTS_ERR || a_info == FTS_NS || a_info == FTS_DNR)
		return 0;
	b_info = (*b)->fts_info;
	if (b_info == FTS_ERR || b_info == FTS_NS || b_info == FTS_DNR)
		return 0;

	if (a_info == FTS_D)
		return -1;
	if (b_info == FTS_D)
		return 1;
	
	return 0;
}







