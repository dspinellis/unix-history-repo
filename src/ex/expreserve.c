/* Copyright (c) 1979 Regents of the University of California */
#include <stdio.h>
#include <ctype.h>
#include <sys/types.h>
#include <sys/stat.h>
#include <sys/dir.h>
#include <pwd.h>

/*
 * Expreserve - preserve a file in /usr/preserve
 * Bill Joy UCB November 13, 1977
 *
 * This routine is very naive - it doesn't remove anything from
 * /usr/preserve... this may mean that we will be unable to preserve
 * stuff there... the danger in doing anything with /usr/preserve
 * is that the clock may be screwed up and we may get confused.
 *
 * We are called in two ways - first from the editor with no argumentss
 * and the standard input open on the temp file. Second with an argument
 * to preserve the entire contents of /tmp (root only).
 *
 * BUG: should do something about preserving Rx... (register contents)
 *      temporaries.
 */

#define	LBLKS	125
#define	FNSIZE	128

struct 	header {
	time_t	Time;			/* Time temp file last updated */
	short	Uid;			/* This users identity */
	short	Flines;			/* Number of lines in file */
	char	Savedfile[FNSIZE];	/* The current file name */
	short	Blocks[LBLKS];		/* Blocks where line pointers stashed */
} H;

#ifdef	lint
#define	ignore(a)	Ignore(a)
#define	ignorl(a)	Ignorl(a)
#else
#define	ignore(a)	a
#define	ignorl(a)	a
#endif

struct	passwd *getpwuid();
off_t	lseek();
FILE	*popen();

#define eq(a, b) strcmp(a, b) == 0

main(argc)
	int argc;
{
	register FILE *tf;
	struct direct dirent;
	struct stat stbuf;

	/*
	 * If only one argument, then preserve the standard input.
	 */
	if (argc == 1) {
		if (copyout((char *) 0))
			exit(1);
		exit(0);
	}

	/*
	 * If not super user, then can only preserve standard input.
	 */
	if (getuid()) {
		fprintf(stderr, "NOT super user\n");
		exit(1);
	}

	/*
	 * ... else preserve all the stuff in /tmp, removing
	 * it as we go.
	 */
	if (chdir("/tmp") < 0) {
		perror("/tmp");
		exit(1);
	}

	tf = fopen(".", "r");
	if (tf == NULL) {
		perror("/tmp");
		exit(1);
	}
	while (fread((char *) &dirent, sizeof dirent, 1, tf) == 1) {
		if (dirent.d_ino == 0)
			continue;
		/*
		 * Ex temporaries must begin with Ex;
		 * we check that the 10th character of the name is null
		 * so we won't have to worry about non-null terminated names
		 * later on.
		 */
		if (dirent.d_name[0] != 'E' || dirent.d_name[1] != 'x' || dirent.d_name[10])
			continue;
		if (stat(dirent.d_name, &stbuf))
			continue;
		if ((stbuf.st_mode & S_IFMT) != S_IFREG)
			continue;
		/*
		 * Save the bastard.
		 */
		ignore(copyout(dirent.d_name));
	}
	exit(0);
}

char	pattern[] =	"/usr/preserve/Exaa`XXXXX";

/*
 * Copy file name into /usr/preserve/...
 * If name is (char *) 0, then do the standard input.
 * We make some checks on the input to make sure it is
 * really an editor temporary, generate a name for the
 * file (this is the slowest thing since we must stat
 * to find a unique name), and finally copy the file.
 */
copyout(name)
	char *name;
{
	int i;
	static int reenter;
	char buf[BUFSIZ];

	/*
	 * The first time we put in the digits of our
	 * process number at the end of the pattern.
	 */
	if (reenter == 0) {
		mkdigits(pattern);
		reenter++;
	}

	/*
	 * If a file name was given, make it the standard
	 * input if possible.
	 */
	if (name != 0) {
		ignore(close(0));
		/*
		 * Need read/write access for arcane reasons
		 * (see below).
		 */
		if (open(name, 2) < 0)
			return (-1);
	}

	/*
	 * Get the header block.
	 */
	ignorl(lseek(0, 0l, 0));
	if (read(0, (char *) &H, sizeof H) != sizeof H) {
format:
		if (name == 0)
			fprintf(stderr, "Buffer format error\n");
		return (-1);
	}

	/*
	 * Consistency checsks so we don't copy out garbage.
	 */
	if (H.Flines < 0) {
#ifdef DEBUG
		fprintf(stderr, "Negative number of lines\n");
#endif
		goto format;
	}
	if (H.Blocks[0] != 1 || H.Blocks[1] != 2) {
#ifdef DEBUG
		fprintf(stderr, "Blocks %d %d\n", H.Blocks[0], H.Blocks[1]);
#endif
		goto format;
	}
	if (name == 0 && H.Uid != getuid()) {
#ifdef DEBUG
		fprintf(stderr, "Wrong user-id\n");
#endif
		goto format;
	}
	if (lseek(0, 0l, 0)) {
#ifdef DEBUG
		fprintf(stderr, "Negative number of lines\n");
#endif
		goto format;
	}

	/*
	 * If no name was assigned to the file, then give it the name
	 * LOST, by putting this in the header.
	 */
	if (H.Savedfile[0] == 0) {
		strcpy(H.Savedfile, "LOST");
		ignore(write(0, (char *) &H, sizeof H));
		H.Savedfile[0] = 0;
		lseek(0, 0l, 0);
	}

	/*
	 * File is good.  Get a name and create a file for the copy.
	 */
	mknext(pattern);
	ignore(close(1));
	if (creat(pattern, 0600) < 0) {
		if (name == 0)
			perror(pattern);
		return (1);
	}

	/*
	 * Make the target be owned by the owner of the file.
	 */
	ignore(chown(pattern, H.Uid, 0));

	/*
	 * Copy the file.
	 */
	for (;;) {
		i = read(0, buf, BUFSIZ);
		if (i < 0) {
			if (name)
				perror("Buffer read error");
			ignore(unlink(pattern));
			return (-1);
		}
		if (i == 0) {
			if (name) {
				ignore(unlink(name));
				notify(H.Uid, H.Savedfile);
			}
			return (0);
		}
		if (write(1, buf, i) != i) {
			if (name == 0)
				perror(pattern);
			unlink(pattern);
			return (-1);
		}
	}
}

/*
 * Blast the last 5 characters of cp to be the process number.
 */
mkdigits(cp)
	char *cp;
{
	register int i, j;

	for (i = getpid(), j = 5, cp += strlen(cp); j > 0; i /= 10, j--)
		*--cp = i % 10 | '0';
}

/*
 * Make the name in cp be unique by clobbering up to
 * three alphabetic characters into a sequence of the form 'aab', 'aac', etc.
 * Mktemp gets weird names too quickly to be useful here.
 */
mknext(cp)
	char *cp;
{
	char *dcp;
	struct stat stb;

	dcp = cp + strlen(cp) - 1;
	while (isdigit(*dcp))
		dcp--;
whoops:
	if (dcp[0] == 'z') {
		dcp[0] = 'a';
		if (dcp[-1] == 'z') {
			dcp[-1] = 'a';
			if (dcp[-2] == 'z')
				fprintf(stderr, "Can't find a name\n");
			dcp[-2]++;
		} else
			dcp[-1]++;
	} else
		dcp[0]++;
	if (stat(cp, &stb) == 0)
		goto whoops;
}

/*
 * Notify user uid that his file fname has been saved.
 */
notify(uid, fname)
	int uid;
	char *fname;
{
	struct passwd *pp = getpwuid(uid);
	register FILE *mf;
	char cmd[BUFSIZ];

	if (pp == NULL)
		return;
	sprintf(cmd, "mail %s", pp->pw_name);
	mf = popen(cmd, "w");
	if (mf == NULL)
		return;
	setbuf(mf, cmd);
	if (fname[0] == 0)
		fprintf(mf,
"A copy of an editor buffer of yours was saved when the system went down.\n\
No name was associated with this buffer so it has been named \"LOST\".\n"
		);
	else
		fprintf(mf,
"A copy of an editor buffer of your file \"%s\"\n\
was saved when the system went down.\n",
		fname);
	fprintf(mf,
"This buffer can be retrieved using the \"recover\" command of the editor.\n\
An easy way to do this is to give the command ``ex -r'' followed by the\n\
name of file you were editing.  This works for ``edit'' and ``vi'' also.\n"
		);
	pclose(mf);
}

/*
 *	people making love
 *	never exactly the same
 *	just like a snowflake 
 */

#ifdef lint
Ignore(a)
	int a;
{

	a = a;
}

Ignorl(a)
	long a;
{

	a = a;
}
#endif
