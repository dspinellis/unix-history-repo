static char *rcsid = "$Header$";
/*
 * phelp - on-line help for a project
 *
 * Author: Peter J. Nicklin
 */
#include <sys/types.h>
#include <sys/dir.h>
#include <stdio.h>
#include "getarg.h"
#include "macro.h"
#include "null.h"
#include "path.h"
#include "phelp.h"
#include "spms.h"
#include "yesno.h"

char *PGN = "phelp";			/* program name */
char PHELP_CMD[PATHSIZE];		/* help command file pathname */
char PHELP_HELP[PATHSIZE];		/* help introduction file pathname */
int INTERACTIVE;			/* phelp interactive? */
int HELPLEVEL;				/* help hierarchy level */

main(argc, argv)
	int argc;
	char **argv;
{
	extern int PPDEBUG;		/* project pathname debug flag */
	char *getcwp();			/* get current working project */
	char *pathcat();		/* pathname concatenation */
	char *ppathname = CURPROJECT;	/* project pathname */
	int isintract();		/* find out if interactive */
	int printhelp();		/* print a help file */
	int printtopic();		/* print topic file and index */
	int processtopic();		/* process help topic on stack */
	int status = 0;			/* exit status */
	void gettopic();		/* get next help topic from stdin */
	void printnotopics();		/* print "no topics" error message */
	void prompt();			/* prompt for a topic */
	void puttopic();		/* add help topic to request queue */

	{
	register char *s;		/* option pointer */
	while (--argc > 0 && **++argv == '-')
		{
		for (s = argv[0]+1; *s != '\0'; s++)
			switch (*s)
				{
				case 'D':
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
				default:
					warn("bad option -%c", *s);
					status = 1;
					goto endfor;
				}
		endfor: continue;
		}
	}
	if (status == 1)
		fatal("usage: phelp [-P projectname] [topic [subtopic ...]]");

	if (getcwp() == NULL)
		fatal("There is no project environment");

	if (chghelp(ppathname) == NO)
		exit(1);

	pathcat(PHELP_CMD, SPMSLIB, PHELP_CMD_FILE);
	pathcat(PHELP_HELP, SPMSLIB, PHELP_HELP_FILE);

	INTERACTIVE = isintract();
	
	if (argc == 0 || (argc == 1 && PHELP))
		{			/* general introduction */
		if (printtopic(PHELP_HELP, CURDIR) > 1)
			{
			printnotopics(ppathname);
			exit(1);
			}
		}
	else	{			/* command line arguments */
		while (argc-- > 0)
			puttopic(*argv++);
		status = processtopic();
		}
	if (INTERACTIVE == YES)		/* further help topics */
		while (HELPLEVEL >= 0)
			{
			prompt();
			gettopic();
			status = processtopic();
			}
	
	exit(status);
}



/*
 * chghelp() changes to another help hierarchy. Returns YES if
 * successful, otherwise NO.
 */
chghelp(ppathname)
	char *ppathname;		/* project pathname */
{
	char helpath[PATHSIZE];		/* help directory pathname */
	char *oldcwp;			/* old current working project */
	char *pathcat();		/* pathname concatenation */
	char *strsav();			/* save a string somewhere */
	int chproject();		/* change project */
	void printnotopics();		/* print "no topics" error message */

	oldcwp = strsav(getcwp());
	if (chproject(ppathname) == NO)
		{
		free(oldcwp);
		return(NO);
		}
	pathcat(helpath, getcwp(), "help");
	if (!CHDIR(helpath))
		{
		printnotopics(ppathname);
		return(NO);
		}
	return(YES);
}



/*
 * mkndir() makes a directory name by appending ".d" to a topic name.
 */
char *
mkndir(basename)
	char *basename;			/* topic name */
{
	static char dirnam[MAXNAMLEN];	/* directory name buffer */
	char *strcpy();			/* string copy */
	char *strcat();			/* string concatenation */

	strcpy(dirnam, basename);
	strcat(dirnam, ".d");
	return(dirnam);
}



/*
 * printhelp() prints a help file on output stream ofp. Returns YES if
 * file can be opened and read, otherwise NO.
 */
printhelp(helpfile, ofp)
	char *helpfile;			/* file containing help information */
	register FILE *ofp;		/* output stream */
{
	register FILE *ifp;		/* input file stream */
	register int c;			/* current character */
	FILE *fopen();			/* open file */

	if ((ifp = fopen(helpfile, "r")) == NULL)
		return(NO);
	if ((c = getc(ifp)) != EOF)
		{
		putc(c, ofp);
		while ((c = getc(ifp)) != EOF)
			putc(c, ofp);
		fclose(ifp);
		return(YES);
		}
	else	{
		fclose(ifp);
		return(NO);
		}
}



/* printtopic() prints a help topic plus a list of available subtopics. The
 * output is piped through more(1) if phelp is interactive. Returns 0 if
 * topic and subtopics printed; 1 if subtopics printed only; 2 if file printed
 * only; 3 if nothing printed.
 */
printtopic(topic, subtopicdir)
	char *topic;			/* name of topic file to be printed */
	char *subtopicdir;		/* name of subtopic directory */
{
	FILE *popen();			/* open pipe for writing */
	FILE *ofp;			/* output stream */
	int mkindex();			/* make topic index */
	int printhelp();		/* print help file */
	int status = 0;			/* printhelp status */
	void printindex();		/* print topic index */

	if (INTERACTIVE == NO || (ofp = popen("more -d", "w")) == NULL)
		ofp = stdout;
	if (printhelp(topic, ofp) == NO)
		status = 1;
	if (mkindex(subtopicdir) == YES)
		printindex(ofp);
	else
		status += 2;
	if (ofp != stdout)
		pclose(ofp);
	return(status);
}
