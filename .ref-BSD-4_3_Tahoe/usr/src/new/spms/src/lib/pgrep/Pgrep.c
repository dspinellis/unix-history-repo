static char *rcsid = "$Header$";
/*
 * pgrep - search files for a pattern
 *
 * Author: Peter J. Nicklin
 */
#include <sys/param.h>
#include <stdio.h>
#include "bin.h"
#include "getarg.h"
#include "null.h"
#include "path.h"
#include "slist.h"
#include "spms.h"
#include "system.h"
#include "yesno.h"

char *PGN;				/* pointer to program name */
char PGNAME[PATHSIZE]; 			/* program name buffer */
int READMF;				/* read makefile for source files? */

/*
 * grep options
 */
char *_PATFILE = NULL;			/* file containing patterns */
int _IGNORECASE;			/* ignore letter case */
int _LIST;				/* list file names only */
int _LINE;				/* precede matched lines by line nos */
int _WORD;				/* search for pattern as a word */

main(argc, argv)
	int argc;
	char **argv;
{
	char *buildcmd();		/* build command to execute on files */
	char **buildgrepargv();		/* build grep command args */
	char *command;			/* command to execute on files */
	char **grepargv;		/* grep command args pointer array */
	char *grepcmd;			/* search command */
	char *greppath;			/* location of grep command */
	char *makefile;			/* makefile name */
	char *pathcat();		/* pathname concatenation */
	char *pattern;			/* pattern command line argument */
	char *slappend();		/* append key */
	int forkgrep();			/* fork grep command */
	int readmf();			/* read makefile */
	int status = 0;			/* exit status */
	SLIST *filelist;		/* list of file names */
	SLIST *grep();			/* capture file names from grep */
	SLIST *slinit();		/* initialize list */
	void slrm();			/* remove list item */

	PGN = pathcat(PGNAME, SPMSLIB, "pgrep");

	command = NULL;
	grepcmd = "grep";
	greppath = GREP;
	makefile = NULL;

	{
	register char *s;		/* option pointer */
	while (--argc > 0 && **++argv == '-')
		{
		for (s = argv[0]+1; *s != '\0'; s++)
			switch (*s)
				{
				case 'C':
					command = GETARG(s);
					_LIST++;
					goto endfor;
				case 'F':
					_PATFILE = GETARG(s);
					goto endfor;
				case 'e':
					grepcmd = "egrep";
					greppath = EGREP;
					break;
				case 'f':
					makefile = GETARG(s);
					goto endfor;
				case 'i':
					_IGNORECASE++;
					break;
				case 'l':
					_LIST++;
					break;
				case 'm':
					READMF++;
					break;
				case 'n':
					_LINE++;
					break;
				case 'w':
					_WORD++;
					break;
				default:
					warn("bad option -%c", *s);
					status = 1;
					goto endfor;
				}
		endfor: continue;
		}
	}
	if (status == 1 ||
	   (argc < 1 && _PATFILE == NULL) ||
	   (argc < 2 && !READMF && _PATFILE == NULL))
		{
		warn("usage: %s [-eilmnw] [-f makefile] [-C command] [-F patfile] [pattern [file ...]]", PGN);
		exit(2);
		}

	if (_PATFILE == NULL)		/* pattern is a command line argument */
		{
		pattern = *argv;
		argv++, argc--;
		}

	/* read file names from command line */
	filelist = slinit();
	while (argc-- > 0)
		if (slappend(*argv++, filelist) == NULL)
			exit(2);

	/* read file names from makefile */
	if (READMF)
		{
		/* get name of makefile */
		if (makefile == NULL)
			makefile = "makefile";
		if (!FILEXIST(makefile))
			{
			makefile = "Makefile";
			if (!FILEXIST(makefile))
				{
				perror("pgrep: makefile");
				exit(2);
				}
			}

		/* get file names from makefile */
		if (readmf(makefile, filelist) == NO)
			exit(2);
		}
	if (SLNUM(filelist) == 0)
		exit(status);

	if ((grepargv = buildgrepargv(grepcmd, pattern, filelist)) == NULL)
		exit(2);
	if (command != NULL)
		{
		slrm(CNULL, filelist);
		if ((filelist = grep(greppath, grepargv)) == NULL)
			exit(2);
		if (SLNUM(filelist) > 0)
			{
			system(buildcmd(command, filelist));
			status = 1;
			}
		}
	else	{
		status = forkgrep(greppath, grepargv);
		}
	exit(status);
}



/*
 * buildcmd() creates a command string to submit to a shell. Returns
 * command string, or NULL if out of memory.
 */
char *
buildcmd(command, filelist)
	char *command;			/* command to execute */
	SLIST *filelist;		/* list of files to run command on */
{
	char **argv;			/* command argument list */
	char *argvtos();		/* convert cmd args to string */
	char *malloc();			/* memory allocator */
	char **sargv;			/* start of command argument list */
	char *slget();			/* get next key */
	int argc;			/* number of args in command */
	void slrewind();		/* rewind list */

	if ((argv = (char **) malloc((unsigned)(SLNUM(filelist)+2)*sizeof(char *))) == NULL)
		{
		warn("out of memory");
		return(NULL);
		}
	sargv = argv;
	*argv++ = command;
	argc = 1;
	slrewind(filelist);
	while ((*argv++ = slget(filelist)) != NULL)
		argc++;
	return(argvtos(argc, sargv));
}



/*
 * buildgrepargv() creates an argv string pointer array for the grep
 * command to pass to execv(). Returns argv, or NULL if out of memory.
 */
char **
buildgrepargv(grepcmd, pattern, filelist)
	char *grepcmd;			/* name of grep command */
	char *pattern;			/* search pattern */
	SLIST *filelist;		/* list of files to search */
{
	char **argv;			/* command argument list */
	char *malloc();			/* memory allocator */
	char **sargv;			/* start of command argument list */
	char *slget();			/* get next key */
	void slrewind();		/* rewind list */

	if ((argv = (char **) malloc((unsigned)(SLNUM(filelist)+8)*sizeof(char *))) == NULL)
		{
		warn("out of memory");
		return(NULL);
		}
	sargv = argv;
	*argv++ = grepcmd;
	if (_IGNORECASE)
		*argv++ = "-i";
	if (_LIST)
		*argv++ = "-l";
	if (_LINE)
		*argv++ = "-n";
	if (_WORD)
		*argv++ = "-w";
	if (_PATFILE != NULL)
		{
		*argv++ = "-f";
		*argv++ = _PATFILE;
		}
	else	{
		*argv++ = pattern;
		}
	slrewind(filelist);
	while ((*argv++ = slget(filelist)) != NULL)
		continue;
	return(sargv);
}



/*
 * forkgrep() forks a grep command.
 */
forkgrep(greppath, grepargv)
	char **grepargv;		/* grep command args pointer array */
	char *greppath;			/* location of grep command */
{
	int pid;			/* process identity */
	int status;			/* child return status */
	int w;				/* a child id */

	if ((pid = FORK()) == 0)
		{
		execv(greppath, grepargv);
		if (*PGN != '\0')
			fprintf(stderr, "%s: ", PGN);
		fprintf(stderr, "can't exec %s\n", greppath);
		exit(2);
		}
	while ((w = wait(&status)) != pid && w != -1)
		continue;
	status >>= NBBY;
	status &=  0xff;
	/*
	 * Because pexec clobbers grep's "matches found" zero exit status
	 * (with the "no matches found" exit status, which will probably
	 * occur in some directories), lib/pgrep reverses these two statuses.
	 * /bin/pgrep restores them.
	 */
	switch (status)
		{
		case 0:
			status = 1;
			break;
		case 1:
			status = 0;
			break;
		}
	return(status);
}
