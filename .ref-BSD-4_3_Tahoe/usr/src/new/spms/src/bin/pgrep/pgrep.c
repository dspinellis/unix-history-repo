static char *rcsid = "$Header$";
/*
 * pgrep - search files in a project for a pattern
 *
 * Author: Peter J. Nicklin
 */
#include <sys/param.h>
#include <stdio.h>
#include "getarg.h"
#include "null.h"
#include "path.h"
#include "spms.h"
#include "system.h"

#define CMDBUFSIZE	1024
#define MAXPXARGV	12

char *PGN = "pgrep";			/* program name */

main(argc, argv)
	int argc;
	char **argv;
{
	register char *gp;		/* pointer to grep command buffer */
	register char **px;		/* pointer to pexec command args */
	char cwd[PATHSIZE];		/* current working directory pathname */
	char *getwd();			/* get current working directory */
	char gpcmdbuf[CMDBUFSIZE];	/* SPMSLIB/grep command buffer */
	char *patfile = NULL;		/* pattern file name */
	char *pathcat();		/* pathname concatenation */
	char pexeccmd[PATHSIZE];	/* location of pexec command */
	char *pxargv[MAXPXARGV];	/* pexec command and args */
	char *strpcpy();		/* copy string and update pointer */
	int command;			/* execute command on files? */
	int pid;			/* process identity */
	int readmf;			/* read makefile for source files? */
	int status = 0;			/* exit status */
	int strlen();			/* string length */
	int w;				/* a child id */

	pathcat(pexeccmd, SPMSBIN, "pexec");
	pathcat(gpcmdbuf, SPMSLIB, "pgrep");
	gp = gpcmdbuf + strlen(gpcmdbuf);
	px = pxargv;
	*px++ = "pexec";
	*px++ = "-iX2";			/* ignore non-zero return codes */
					/* set pexec error code to 2 */
	command = 0;
	readmf = 0;
	{
	register char *s;		/* option pointer */
	while (--argc > 0 && **++argv == '-')
		{
		for (s = argv[0]+1; *s != '\0'; s++)
			switch (*s)
				{
				case 'C':
					command++;
					gp = strpcpy(gp, " -C\"");
					gp = strpcpy(gp, GETARG(s));
					gp = strpcpy(gp, "\"");
					goto endfor;
				case 'D':
					*px++ = "-D";
					break;
				case 'F':
					gp = strpcpy(gp, " -F");
					patfile = GETARG(s);
					if (*patfile != _RDIRC)
						{
						if (getwd(cwd) == NULL)
							{
							warn(cwd);
							status = 1;
							}
						gp = strpcpy(gp, cwd);
						gp = strpcpy(gp, PATHSEP);
						}
					gp = strpcpy(gp, patfile);
					goto endfor;
				case 'P':
					*px++ = "-P";
					*px++ = GETARG(s);
					goto endfor;
				case 'T':
					*px++ = "-T";
					*px++ = GETARG(s);
					goto endfor;
				case 'e':
					gp = strpcpy(gp, " -e");
					break;
				case 'f':
					gp = strpcpy(gp, " -f");
					gp = strpcpy(gp, GETARG(s));
					gp = strpcpy(gp, " -m");
					readmf++;
					goto endfor;
				case 'i':
					gp = strpcpy(gp, " -i");
					break;
				case 'l':
					gp = strpcpy(gp, " -l");
					break;
				case 'm':
					gp = strpcpy(gp, " -m");
					readmf++;
					break;
				case 'n':
					gp = strpcpy(gp, " -n");
					break;
				case 'w':
					gp = strpcpy(gp, " -w");
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
	   (argc < 1 && patfile == NULL) ||
	   (argc < 2 && !readmf && patfile == NULL))
		{
		warn("usage: pgrep [-eilmnw] [-f makefile] [-C command] [-F patfile]\n       [-P pdirname] [-T typexpr] [pattern [file ...]]");
		exit(2);
		}

	if (!command)			/* turn off quit query if no command */
		*px++ = "-?";
	if (patfile == NULL)
		{
		gp = strpcpy(gp, " \"");
		gp = strpcpy(gp, *argv);
		gp = strpcpy(gp, "\"");
		argv++, argc--;
		}
	while (argc-- > 0)
		{
		gp = strpcpy(gp, " ");
		gp = strpcpy(gp, *argv++);
		}

	*px++ = gpcmdbuf;
	*px = NULL;

	if ((pid = FORK()) == 0)
		{
		execv(pexeccmd, pxargv);
		if (*PGN != '\0')
			fprintf(stderr, "%s: ", PGN);
		fprintf(stderr, "can't exec %s\n", pexeccmd);
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
	 * Hence, we have to reverse them back here.
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
	exit(status);
}
