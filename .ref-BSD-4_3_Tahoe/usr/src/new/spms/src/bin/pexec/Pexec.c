static char *rcsid = "$Header$";
/*
 * pexec - execute command over project hierarchy
 *
 * Author: Peter J. Nicklin
 */
#include "bin.h"
#include "macro.h"
#include "getarg.h"
#include "null.h"
#include "path.h"
#include "pdtyp.h"
#include "slist.h"
#include "spms.h"
#include "yesno.h"

char *COMMAND;				/* command string to be executed */
char *PGN = "pexec";			/* program name */
char *SHELLNAME;			/* name of command shell */
char *SHELLPATH;			/* pathname of command shell */
int CSHELL = 0;				/* use csh or sh? */
int CSHRC = NO;				/* execute .cshrc if csh shell */
int DEBUG = NO;				/* print pexec debugging info */
int ERRSTATUS = 1;			/* pexec error status */
int EXECUTE = YES;			/* execute command? */
int IGNORE_BAD_EXIT = NO;		/* exit if command doesn't return 0 */
int NOQUERY;				/* query user about quitting? */
int PRINT_HEADING = YES;		/* print headings for project dirs */
int PVINDEX;				/* environ index for PROJECT variable */
PDTYP PDIRTYP;				/* project directory type labels list */
SLIST *ENVLIST;				/* project environment variable list */

main(argc, argv)
	int argc;
	char **argv;
{
	extern int PPDEBUG;		/* project pathname debug flag */
	char *argvtos();		/* convert cmd args to string */
	char *getcwp();			/* get current working project */
	char *getshell();		/* get command shell pathname */
	char *pathtail();		/* remove pathname head */
	char *ppathname = CURPROJECT;	/* project pathname */
	int atoi();			/* string to integer conversion */
	int build_pdset();		/* create set of project dirs */
	int ch_dir();			/* change current working directory */
	int check_pdset();		/* check ordering of set of proj dirs */
	int exec_pdset();		/* execute cmds in set of proj dirs */
	int execcmd();			/* execute command in directory */
	int getpvindex();		/* get PROJECT env. variable index */
	int pdtparse();			/* parse boolean type label expr */
	int status = 0;			/* exit status */
	int xppath();			/* expand project pathname */
	PATH pathbuf;			/* pathname struct buffer */
	SLIST *slinit();		/* initialize list */
	void debug_pdset();		/* print dirs + types after sorting */
	void init_pdset();		/* initialize set of project dirs */
	void print_title();		/* print project directory title */
	void sort_pdset();		/* sort set of project dirs */

	{
	register char *s;		/* option pointer */
	while (--argc > 0 && **++argv == '-')
		{
		for (s = argv[0]+1; *s != '\0'; s++)
			switch (*s)
				{
				case '?':
					NOQUERY++;
					break;
				case 'D':
					DEBUG = YES;
					PPDEBUG = YES;
					break;
				case 'P':
					ppathname = GETARG(s);
					if (*ppathname == '\0')
						{
						warn("missing project name");
						status = 1;
						}
					goto endfor;
				case 'T':
					if (pdtparse(GETARG(s), &PDIRTYP) == NO)
						status = 1;
					goto endfor;
				case 'X':
					ERRSTATUS = atoi(GETARG(s));
					goto endfor;
				case 'c':
					CSHRC = YES;
					break;
				case 'i':
					IGNORE_BAD_EXIT = YES;
					break;
				case 'q':
					PRINT_HEADING = NO;
					break;
				case 'x':
					EXECUTE = NO;
					break;
				default:
					warn("bad option -%c", *s);
					status = 1;
					goto endfor;
				}
		endfor: continue;
		}
	}
	if (status == 1 || argc < 1)
		{
		warn("usage: pexec [-?ciqx] [-P pdirname] [-T typexpr] command");
		pxexit();
		}
	/*
	 * The PROJECT environment variable must exist because
	 * it has to be modified before each command execution.
	 */
	if ((PVINDEX = getpvindex()) < 0)
		{
		warn("no project environment");
		pxexit();
		}

	if ((COMMAND = argvtos(argc, argv)) == NULL)
		pxexit();
	SHELLPATH = getshell();
	SHELLNAME = pathtail(SHELLPATH);
	if (EQUAL(SHELLNAME, pathtail(CSH)))
		CSHELL++;

	/* convert project pathname to regular pathname */
	if (xppath(ppathname, &pathbuf) == -1)
		{
		patherr(ppathname);
		pxexit();
		}
	else switch (pathbuf.p_mode & P_IFMT)
		{
		case P_IFNEW:
		case P_IFREG:
			warn("%s: no such project or project directory", ppathname);
			pxexit();
		case P_IFPDIR:
			IGNORE_BAD_EXIT = NO;
			if (PRINT_HEADING == YES)
				print_title(ppathname);
			ch_dir(pathbuf.p_path);
			if (EXECUTE == YES)
				status |= execcmd(pathbuf.p_project);
			break;
		case P_IFHOME:
		case P_IFPROOT:
			if (PDIRTYP.pfxsize == 0)
				status |= execproject(ppathname, pathbuf.p_path);
			else	{
				ENVLIST = slinit();
				init_pdset();
				if (build_pdset(ppathname, pathbuf.p_path) != 0)
					pxexit();
				sort_pdset();
				if (DEBUG == YES)
					debug_pdset();
				if (check_pdset() != 0)
					pxexit();
				status |= exec_pdset();
				}
			break;
		}
	exit(status);
}
