static char *sccsid = "@(#)rm.c	4.14 (Berkeley) %G%";

/*
 * rm - for ReMoving files, directories & trees.
 */

#include <stdio.h>
#include <sys/param.h>
#include <sys/stat.h>
#include <sys/dir.h>
#include <sys/file.h>

int	fflg;			/* -f force - supress error messages */
int	iflg;			/* -i interrogate user on each file */
int	rflg;			/* -r recurse */

int	errcode;		/* true if errors occured */

char	*strcpy();

main(argc, argv)
	char *argv[];
{
	register char *arg;

	fflg = !isatty(0);
	iflg = 0;
	rflg = 0;
	while (argc > 1 && argv[1][0] == '-') {
		arg = *++argv;
		argc--;

		/*
		 *  all files following a null option are considered file names
		 */
		if (arg[1] == '\0')
			break;

		while (*++arg != '\0')
			switch(*arg) {
			case 'f':
				fflg++;
				break;

			case 'i':
				iflg++;
				break;

			case 'R':
			case 'r':
				rflg++;
				break;

			default:
				fprintf(stderr, "usage: rm [-rif] file ...\n");
				exit(1);
			}
	}

	if (argc < 2) {
		fprintf(stderr, "usage: rm [-rif] file ...\n");
		exit(1);
	}

	while (--argc > 0)
		(void) rm(*++argv, 0);

	exit(errcode != 0);
}

struct nambuf {
	char	*name;			/* pointer to name */
	struct	nambuf *next;		/* linked list of names */
} path, *pathp;

/*
 * Return TRUE if sucessful. Recursive with -r (rflg)
 */
rm(arg, level)
	char arg[];
{
	int ok;				/* true if recursive rm succeeded */
	struct stat buf;		/* for finding out what a file is */
	struct direct *dp;		/* for reading a directory */
	DIR *dirp;			/* for reading a directory */
	char name[MAXNAMLEN + 1];	/* buffer for file name */
	char prevname[MAXNAMLEN + 1];	/* previous name for -r */
	struct nambuf nambuf, *pp;

	if (dotname(arg)) {
		fprintf(stderr, "rm: cannot remove `.' or `..'\n");
		return (0);
	}
	if (level == 0) {
		path.name = arg;
		path.next = NULL;
		pathp = &path;
	}
	if (lstat(arg, &buf)) {
		if (!fflg)
			error("nonexistent");
		errcode++;
		return (0);		/* error */
	}
	if ((buf.st_mode&S_IFMT) == S_IFDIR) {
		if (!rflg) {
			error("directory");
			errcode++;
			return (0);
		}
		if (access(arg, R_OK|W_OK|X_OK) != 0) {
			if (rmdir(arg) == 0)
				return (1);	/* salvaged: removed empty dir */
			if (!fflg)
				error("not changed");
			errcode++;
			return (0);		/* error */
		}
		if (iflg && level != 0) {
			if (!yes("remove directory"))
				return (0);	/* didn't remove everything */
		}
		if (chdir(arg) < 0 || (dirp = opendir(".")) == NULL) {
			if (!fflg)
				error("cannot read?");
			errcode++;
			return (0);
		}
		nambuf.name = name;
		nambuf.next = NULL;
		pathp->next = &nambuf;
		pp = pathp;
		pathp = &nambuf;
		prevname[0] = '\0';
		while ((dp = readdir(dirp)) != NULL) {
			if (dotname(dp->d_name)) {
				strcpy(prevname, dp->d_name);
				continue;
			}
			strcpy(name, dp->d_name);
			closedir(dirp);
			ok = rm(name, level + 1);
			if ((dirp = opendir(".")) == NULL) {
				if (!fflg)
					error("cannot read?");
				errcode++;
				break;
			}
			/* pick up where we left off */
			if (prevname[0] != '\0') {
				while ((dp = readdir(dirp)) != NULL &&
				    strcmp(prevname, dp->d_name) != 0)
					;
			}
			/* skip the one we just failed to delete */
			if (!ok) {
				dp = readdir(dirp);
				if (dp != NULL && strcmp(name, dp->d_name) != 0)
					error("internal synchronization error");
				strcpy(prevname, name);
			}
		}
		closedir(dirp);
		pathp = pp;
		pathp->next = NULL;
		if (chdir("..") < 0) {
			if (!fflg)
				error("cannot cd to '..'?");
			errcode++;
			return (0);
		}
		if (iflg) {
			if (!yes("remove"))
				return (0);
		}
		if (rmdir(arg) < 0) {
			if (!fflg || iflg)
				error("not removed");
			errcode++;
			return (0);
		}
		return (1);
	}

	if (iflg) {
		if (!yes("remove"))
			return (0);
	} else if (!fflg) {
		if ((buf.st_mode&S_IFMT) != S_IFLNK && access(arg, W_OK) < 0) {
			sprintf(name, "override protection %o for",
				buf.st_mode&0777);
			if (!yes(name))
				return (0);
		}
	}
	if (unlink(arg) < 0) {
		if (!fflg || iflg)
			error("not removed");
		errcode++;
		return (0);
	}
	return (1);
}

/*
 * boolean: is it "." or ".." ?
 */
dotname(s)
	char *s;
{
	if (s[0] == '.')
		if (s[1] == '.')
			if (s[2] == '\0')
				return (1);
			else
				return (0);
		else if (s[1] == '\0')
			return (1);
	return (0);
}

/*
 * Get a yes/no answer from the user.
 */
yes(msg)
	char *msg;
{
	register struct nambuf *pp;
	int i, b;

	printf("rm: %s %s", msg, path.name);
	for (pp = &path; pp->next != NULL; pp = pp->next)
		printf("/%s", pp->next->name);
	printf("? ");
	i = b = getchar();
	while (b != '\n' && b != EOF)
		b = getchar();
	return (i == 'y');
}

/*
 * Print the current path and error message.
 */
error(msg)
	char *msg;
{
	register struct nambuf *pp;

	fprintf(stderr, "rm: %s", path.name);
	for (pp = &path; pp->next != NULL; pp = pp->next)
		fprintf(stderr, "/%s", pp->next->name);
	fprintf(stderr, ": %s\n", msg);
}
