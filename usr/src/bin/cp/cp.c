/*
 * Copyright (c) 1988 The Regents of the University of California.
 * All rights reserved.
 *
 * This code is derived from software contributed to Berkeley by
 * David Hitz of Auspex Systems Inc.
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
 * WARRANTIES OF MERCHANTIBILITY AND FITNESS FOR A PARTICULAR PURPOSE.
 */

#ifndef lint
char copyright[] =
"@(#) Copyright (c) 1988 The Regents of the University of California.\n\
 All rights reserved.\n";
#endif /* not lint */

#ifndef lint
static char sccsid[] = "@(#)cp.c	5.2 (Berkeley) %G%";
#endif /* not lint */

/*
 * cp copies source files to target files.
 * 
 * The global path_t structures "to" and "from" always contain paths to the
 * current source and target files, respectively.  Since cp does not change
 * directories, these paths can be either absolute or dot-realative.
 * 
 * The basic algorithm is to initialize "to" and "from", and then call the
 * recursive copy() function to do the actual work.  If "from" is a file,
 * copy copies the data.  If "from" is a directory, copy creates the
 * corresponding "to" directory, and calls itself recursively on all of
 * the entries in the "from" directory.
 * 
 * Instead of handling directory entries in the order they appear on disk,
 * copy() does non-directory files before directory files.
 * 
 * There are two reasons to do directories last.  The first is efficiency.
 * Files tend to be in the same cylinder group as their parent, whereas
 * directories tend not to be. Copying files all at once reduces seeking.
 * 
 * Second, deeply nested tree's could use up all the file descriptors if we
 * didn't close one directory before recursivly starting on the next.
 */

#include <sys/param.h>
#include <sys/stat.h>
#include <sys/file.h>
#include <sys/dir.h>
#include <sys/time.h>

#include <stdio.h>
#include <errno.h>
#include <strings.h>

typedef struct {
	char	*p_path;	/* Pointer to the start of a path. */
	char	*p_end;		/* Pointer to NULL at end of path. */
} path_t;

char *path_append(), *path_basename();
void path_restore();

int exit_val, my_umask;
int interactive_flag, preserve_flag, recursive_flag;
char *buf;				/* I/O; malloc for best alignment. */
char from_buf[MAXPATHLEN + 1],		/* Source path buffer. */
     to_buf[MAXPATHLEN + 1];		/* Target path buffer. */
path_t from = {from_buf, from_buf};
path_t to = {to_buf, to_buf};

main(argc, argv)
	int argc;
	char **argv;
{
	extern int optind, errno;
	struct stat to_stat;
	register int c, r;
	char *old_to, *malloc();

	while ((c = getopt(argc, argv, "Ripr")) != EOF) {
	switch ((char) c) {
		case 'i':
			interactive_flag = isatty(fileno(stdin));
			break;
		case 'p':
			preserve_flag = 1;
			(void)umask(0);
			break;
		case 'r':
		case 'R':
			recursive_flag = 1;
			break;
		case '?':
		default:
			usage();
			break;
		}
	}
	argc -= optind;		/* argc is count of remaining arguments. */
	argv += optind;		/* argv[0] is first remaining argument. */

	if (argc < 2)
		usage();

	my_umask = umask(0);
	(void)umask(my_umask);

	buf = (char *)malloc(MAXBSIZE);
	if (!buf) {
		fprintf(stderr, "cp: out of space.\n");
		exit(1);
	}

	/* Consume last argument first. */
	if (!path_set(&to, argv[--argc]))
		exit(exit_val);

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
	if (r == -1 || (to_stat.st_mode & S_IFMT) != S_IFDIR) {
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
		for (; argc; --argc, ++argv) {
			if (!path_set(&from, *argv))
				continue;
			old_to = path_append(&to, path_basename(&from), -1);
			if (!old_to)
				continue;
			copy();
			path_restore(&to, old_to);
		}
	}
	exit(exit_val);
}

/*
 * Copy file or directory at "from" to "to".
 */
copy()
{
	struct stat from_stat, to_stat;
	int new_target_dir = 0;

	if (stat(from.p_path, &from_stat) == -1) {
		error(from.p_path);
		return;
	}
	/*
	 * This is not an error, but we need to remember that it happened.
	 */
	if (stat(to.p_path, &to_stat) == -1)
		to_stat.st_ino = -1;
	else if (to_stat.st_dev == from_stat.st_dev &&
	    to_stat.st_ino == from_stat.st_ino) {
		fprintf(stderr,
		    "cp: \"%s\" and \"%s\" are identical (not copied).\n",
		    to.p_path, from.p_path);
		exit_val = 1;
		return;
	}

	if ((from_stat.st_mode & S_IFMT) != S_IFDIR) {
		if (!copy_file(from_stat.st_mode))
			return;
	}
	else {
		if (!recursive_flag) {
			(void)fprintf(stderr,
			   "cp: \"%s\" is a directory (not copied).\n",
			   from.p_path);
			exit_val = 1;
			return;
		}
		if (to_stat.st_ino == -1) {
			if (mkdir(to.p_path, 0777) < 0) {
				error(to.p_path);
				return;
			}
			new_target_dir = 1;
		}
		else if ((to_stat.st_mode & S_IFMT) != S_IFDIR) {
			(void)fprintf(stderr,
			   "cp: \"%s\": not a directory.\n",
			   to.p_path);
			return;
		}
		copy_dir();
	}
	/* Preserve old times/modes if necessary. */
	if (preserve_flag)
		(void)chmod(to.p_path, (int) from_stat.st_mode);
	else if (new_target_dir)
		(void)chmod(to.p_path, (int) from_stat.st_mode & ~my_umask);
	if (preserve_flag || new_target_dir) {
		static struct timeval tv[2];

		tv[0].tv_sec = from_stat.st_atime;
		tv[1].tv_sec = from_stat.st_mtime;
		if (utimes(to.p_path, tv))
			error(to.p_path);
	}
}

copy_file(mode)
	u_short mode;			/* Permissions for new file. */
{
	int from_fd, to_fd, rcount, wcount, r;
	char c;

	from_fd = open(from.p_path, O_RDONLY, 0);
	if (from_fd == -1) {
		error(from.p_path);
		(void)close(from_fd);
		return(0);
	}

	/*
	 * In the interactive case, use O_EXCL to notice existing files. If
	 * the file exists, verify with the user.
	 */
	to_fd = open(to.p_path,
	     (interactive_flag ? O_EXCL : 0) | O_WRONLY | O_CREAT | O_TRUNC,
		 mode);

	if (to_fd == -1 && errno == EEXIST && interactive_flag) {
		(void)fprintf(stderr, "overwrite \"%s\"? ", to.p_path);
		r = scanf("%1s", &c);
		if (r != 1 || c != 'y')
			return(0);
		/* Try again. */
		to_fd = open(to.p_path, O_WRONLY | O_CREAT | O_TRUNC, mode);
	}

	if (to_fd == -1) {
		error(to.p_path);
		(void)close(from_fd);
		return(0);
	}

	while ((rcount = read(from_fd, buf, MAXBSIZE)) > 0) {
		wcount = write(to_fd, buf, rcount);
		if (rcount != wcount || wcount == -1) {
			error(from.p_path);
			break;
		}
	}
	(void)close(from_fd);
	(void)close(to_fd);
	return(1);
}

copy_dir()
{
	struct stat from_stat;
	char *old_from, *old_to;
	struct direct *dp, **dir_list;
	int dir_cnt, i;

	dir_cnt = scandir(from.p_path, &dir_list, NULL, NULL);
	if (dir_cnt == -1) {
		(void)fprintf(stderr, "cp: Can't read directory \"%s\".\n",
		    from.p_path);
		exit_val = 1;
	}

	/* Copy files first. */
	for (i = 0; i < dir_cnt; ++i) {
		dp = dir_list[i];
		if (dp->d_namlen <= 2 && dp->d_name[0] == '.'
		    && (dp->d_name[1] == NULL || dp->d_name[1] == '.')) {
			(void)free((char *)dp);
			dir_list[i] = NULL;
			continue;
		}
		old_from = path_append(&from, dp->d_name, (int)dp->d_namlen);
		if (!old_from) {
			dir_list[i] = NULL;
			(void)free((char *)dp);
			continue;
		}

		if (stat(from.p_path, &from_stat) < 0) {
			error(dp->d_name);
			path_restore(&from, old_from);
			continue;
		}

		if ((from_stat.st_mode & S_IFMT) != S_IFDIR) {
			old_to = path_append(&to, dp->d_name,
			    (int)dp->d_namlen);
			if (!old_to) {
				dir_list[i] = NULL;
				(void)free((char *)dp);
				continue;
			}
			copy();
			path_restore(&to, old_to);
			dir_list[i] = NULL;
			(void)free((char *)dp);
		}
		path_restore(&from, old_from);
	}

	/* Then copy directories. */
	for (i = 0; i < dir_cnt; ++i) {
		dp = dir_list[i];
		if (!dp)
			continue;
		old_from = path_append(&from, dp->d_name, (int) dp->d_namlen);
		if (!old_from) {
			(void)free((char *)dp);
			continue;
		}
		old_to = path_append(&to, dp->d_name, (int) dp->d_namlen);
		if (!old_to) {
			(void)free((char *)dp);
			path_restore(&from, old_from);
			continue;
		}
		copy();
		free((char *)dp);
		path_restore(&from, old_from);
		path_restore(&to, old_to);
	}
	free((char *)dir_list);
}

error(s)
	char *s;
{
	extern int errno;

	exit_val = 1;
	(void)fprintf(stderr, "cp: %s: %s\n", s, strerror(errno));
}

/********************************************************************
 * Path Manipulation Routines.
 ********************************************************************/

/*
 * These functions manipulate paths in "path_t" structures.
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
	register path_t *p;
	char *string;
{
	if (strlen(string) > MAXPATHLEN) {
		fprintf(stderr, "cp: \"%s\": Name too long.\n", string);
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
	register path_t *p;
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
		fprintf(stderr,
		    "cp: \"%s/%s\": Name too long.\n", p->p_path, name);
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
void
path_restore(p, old)
	path_t *p;
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
	path_t *p;
{
	char *basename;

	basename = rindex(p->p_path, '/');
	if (!basename)
		basename = p->p_path;
	return(basename);
}

usage()
{
	(void)fprintf(stderr,
	   "usage: cp [-ip] f1 f2; or: cp [-irp] f1 ... fn directory\n");
	exit(1);
}
