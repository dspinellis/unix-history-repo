static char vprmSCCSid[] = "@(#)vprm.c	1.4\t11/4/87";

#include <sys/param.h>
#include <sys/dir.h>
#include <sys/stat.h>
#include <stdio.h>

char *basename();

extern	char _sobuf[];

main(argc, argv)
	int argc;
	char *argv[];
{
	setbuf(stdout, _sobuf);
	argc--, argv++;
	if (argc == 0) {
		fprintf(stderr, "usage: vprm [ id ... ] [ filename ... ] [ user ... ]\n");
		exit(1);
	}

	/* Look for something to delete both in Varian and Versatec spool dirs. */
	delete("Varian", "/usr/spool/vad", argc, argv);
	delete("Versatec", "/usr/spool/vpd", argc, argv);
}

/*
 * Look through the argv list for things to delete.
 */

delete(devname, spooldir, argc, argv)
	char *devname, *spooldir, *argv[];
	int argc;
{
	DIR *dir;		/* The spool dir. */
	struct direct *dirp;	/* An entry read from the spool dir.*/
	int deletion = 0;	/* Flag noting something has been deleted. */

	/* Change to the spool directory. */
	if (chdir(spooldir) < 0) {
		perror(spooldir);
		return(1);
	}

	/* Open it. */
	if ((dir = opendir(".")) == NULL) {
		perror(spooldir);
		return(1);
	}

	printf("%s -", devname);

	/*
	 * Loop through the args and the spool dir, looking for a spool
	 * command file (has a prefix of 'df'),
	 * and trying to match it with the argument.
	 */
	while (argc-- > 0) {
		rewinddir(dir);
		while ((dirp = readdir(dir)) != NULL) {
			if (dirp->d_name[0] == 'd' &&
			    dirp->d_name[1] == 'f' &&
			    delcheck(dirp->d_name, *argv)) {
				printf(" removing %s", &(dirp->d_name[3]));
				deletion = 1;
			}
		}
		argv++;
	}
	closedir(dir);
	if (!deletion)
		printf(" nothing to remove\n");
	else
		putchar('\n');
}


/*
 * delcheck tries to match the given arg against the given command file in
 * various ways.  For instance, if dfname = 'dfa12345', then there is a match if
 * arg == 'dfa12345', or
 * arg == '12345', or
 * arg == the name given on the L line of the file (the owner), or
 * arg == the basename of a file given on a command line in the file.
 * If there is a match, all the U (unlink) commands in the command file
 * are executed, and then the command file itself is unlinked.
 */

int
delcheck(dfname, arg)
	char *dfname, *arg;
{
	FILE *df = NULL;	/* The command file. */
	int delfile = 0;	/* A match was found, so do a deletion. */
	char line[256];		/* Command line in command file. */

	if (strcmp(arg, dfname) == 0)
		delfile = 1;	/* arg == 'dfa12345'. */
	else if (strcmp(arg, dfname+3) == 0)
		delfile = 1;	/* arg == '12345' (skip 'dfa'). */
	else {			/* No match; look inside on command lines. */
		if ((df = fopen(dfname, "r")) == NULL)
			return(0);
		while (!delfile && getline(df, line)) {
			switch (line[0]) {
				case 'L':
					/* Check owner name. */
					if (strcmp(arg, line+1) == 0)
						delfile = 1;
					break;

				case 'C':
				case 'V':
				case 'F':
				case 'G':
				case 'P':
				case 'T':
					/* Check command line arg. */
					if (strcmp (basename(arg), basename(line)) == 0)
						delfile = 1;
					break;
			}
		}
	}

	if (delfile) {
		if (df == NULL)		/* File not open yet. */
			df = fopen(dfname, "r");
		if (df == NULL)
			return(0);
		
		/* Run through the command file, executing Unlink commands. */
		rewind(df);
		while (getline(df, line)) {
			if (line[0] == 'U')
				unlink(line+1);
		}

		unlink(dfname);		/* Now unlink the command file itself. */
	}

	if (df != NULL)
		fclose(df);
	return(delfile);
}




getline(file, line)
	FILE *file;
	char *line;
{
	register int i, c;

	i = 0;
	while ((c = getc(file)) != '\n') {
		if (c <= 0)
			return(0);
		if (i < 256)
			line[i++] = c;
	}
	line[i++] = 0;
	return (1);
}


/*
 * basename gets the final component of the given pathname. E.g. "c" in
 * /a/b/c.
 */

char *
basename(pathname)
	char *pathname;
{
	register char *lastname;

	lastname = pathname + strlen(pathname)-1; /* Move to last char in name. */
	while (lastname >= pathname) {
		if (*lastname == '/')
			return(lastname + 1);
		lastname--;
	}
	return(pathname);		/* No /'s at all. */
}
