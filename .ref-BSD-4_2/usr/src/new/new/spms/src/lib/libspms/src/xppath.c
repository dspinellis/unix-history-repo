/* $Header$ */

/*
 * Author: Peter J. Nicklin
 */

/*
 * xppath() expands project pathname ppathname into regular pathname. Both
 * the pathname type labels and description are ignored for efficiency.
 * Returns -1 on error, otherwise zero.
 */
#include <pwd.h>
#include <stdio.h>
#include <sys/types.h>
#include <sys/stat.h>
#include "null.h"
#include "path.h"
#include "pdb.h"
#include "pld.h"
#include "system.h"
#include "yesno.h"
/*
 * pathname states
 */
#define START	0001
#define HDIR	0002
#define PDIR1	0004
#define PDIR2	0010
#define DIR1	0020
#define DIR2	0040
#define EOP	0100
#define ERROR	0200
/*
 * pathname errors
 */
#define E_UNKNOWN	0
#define E_SYNTAX	1
#define E_NOTPDIR	2
#define E_NOTDIR	3

extern int errno;
extern int sys_nerr;
extern char *sys_errlist[];
extern char PDBERR[];			/* project link dir error message */

static char *pp;			/* project pathname pointer */
static char *perrlist[] =
	{
	"unknown error",
	"pathname syntax error",
	"no such project directory",
	"no such file, directory, or project",
	};

char *PATHERR;				/* current pathname error condition */
int PPDEBUG = NO;			/* project pathname debug flag */

xppath(ppathname, pb)
	char *ppathname;		/* project pathname */
	register PATH *pb;		/* pathname struct pointer */
{
	register char *p;		/* regular pathname pointer */
	register short pstat;		/* pathname state */
	char *getcwp();			/* get current project pathname */
	char *gethdir();		/* get home directory pathname */
	char *hdir;			/* home directory pointer */
	char *mkalias();		/* construct alias from pathname */
	char *optpath();		/* optimize pathname */
	char *pathcat();		/* pathname concatenation */
	char *pbgetstring();		/* get specified string field */
	char *pdir;			/* project directory pathname */
	char pplex();			/* pathname lexical analyzer */
	char *strcpy();			/* string copy */
	char *strpcpy();		/* copy string to pathname */
	char token[PATHSIZE];		/* receiving token */
	int closepdb();			/* close database */
	int pbfndflag();		/* find flag field */
	int pfndent();			/* find and load database entry */
	int strlen();			/* string length */
	PDB *openpdb();			/* open database */
	PDB *pdbp;			/* database stream */
	struct passwd *getpwdir();	/* get password file entry for dir */
	struct stat stbuf;		/* file state buffer */

	PATHERR = perrlist[E_UNKNOWN];
	pstat = START;
	pp = ppathname;
	pb->p_mode = P_IFREG;
	pb->p_alias = optpath(strcpy(pb->p_buf, mkalias(ppathname)));
	p = pb->p_path = pb->p_buf + strlen(pb->p_buf) + 1;
	*pb->p_project = '\0';

	while (pstat != EOP)
		{
		switch (pplex(token))
			{
			case _HDIRC:	/* user's home directory */
				if (pstat == START)
					{
					if ((hdir = gethdir(token+1)) != NULL)
						{
						p = strpcpy(p, hdir);
						strcpy(pb->p_project, pb->p_path);
						pb->p_mode = P_IFHOME;
						pstat = HDIR;
						break;
						}
					}
				PATHERR = perrlist[E_SYNTAX];
				pstat = ERROR;
				break;
			case _PPSC:	/* project pathname separator char */
				if ((pstat&(START|HDIR|PDIR2)) == 0)
					{
					if ((pstat&DIR1) != 0)
						PATHERR = perrlist[E_SYNTAX];
					else
						PATHERR = perrlist[E_NOTDIR];
					pstat = ERROR;
					break;
					}
				if (pstat == START)
					{
					pb->p_mode = P_IFHOME;
					p = strpcpy(p, gethdir((char *) NULL));
					strcpy(pb->p_project, pb->p_path);
					}
				p = strpcpy(p, PATHSEP);
				pstat = PDIR1;
				break;
			case _PSC:	/* pathname separator character */
				if ((pstat&(START|HDIR|PDIR2|DIR2)) == 0)
					{
					PATHERR = perrlist[E_SYNTAX];
					pstat = ERROR;
					break;
					}
				if (pstat == START && (pdir = getcwp()) != NULL)
					strcpy(pb->p_project, pdir);
				p = strpcpy(p, PATHSEP);
				pstat = DIR1;
				break;
			case '\0':
				*p = '\0';
				pstat = EOP;
				break;
			default:
				/* project dir, regular dir, or file */
				if ((pstat&(START|PDIR1|DIR1)) == 0)
					{
					PATHERR = perrlist[E_SYNTAX];
					pstat = ERROR;
					break;
					}
				if (pstat == DIR1)
					{
			regdir:		p = strpcpy(p, token);
					pb->p_mode = P_IFREG;
					pstat = DIR2;
					break;
					}
				/* initialize pathname */
				if (pstat == START)
					{
					if ((pdir = getcwp()) == NULL)
						goto regdir;
					strcpy(pb->p_project, pdir);
					}
				else	{
					pdir = pb->p_path;
					}
				if ((pdbp = openpdb(PLDNAME, pdir, "r")) == NULL)
					{
					PATHERR = PDBERR;
					pstat = ERROR;
					break;
					}
				/* project directory? */
				if (pfndent(token, pdbp) == NO)
					{
					closepdb(pdbp);
					if ((pstat&PDIR1) != 0)
						{/* project dir must follow ^ */
						PATHERR = perrlist[E_NOTPDIR];
						pstat = ERROR;
						break;
						}
					goto regdir;
					}
				if (pstat == START)
					{
					p = strpcpy(p, pdir);
					p = strpcpy(p, PATHSEP);
					}
				if (pbfndflag(PROOTDIR) == YES)
					{
					pb->p_mode = P_IFPROOT;
					pstat = PDIR2;
					}
				else	{
					pb->p_mode = P_IFPDIR;
					pstat = DIR2;
					}
				/* is new pathname absolute? */
				if (*pbgetstring(PDIRPATH, p) == _RDIRC)
					p = strpcpy(pb->p_path, p);
				else while (*p != '\0') p++;
				closepdb(pdbp);
				if (pb->p_mode == P_IFPROOT)
					strcpy(pb->p_project, pb->p_path);
				break;
			}
		if (pstat == ERROR)
			{
			*p = '\0';
			break;
			}
		}
	if (pstat != ERROR)
		{
		optpath(pb->p_path);
		optpath(pb->p_project);
		if (stat(pb->p_path, &stbuf) == 0)
			{
			/*
			 * a regular dir may be a root project directory
			 * specified as ~user, but expanded by the shell.
			 */
			if (pb->p_mode == P_IFREG &&
			   (stbuf.st_mode&S_IFMT) == S_IFDIR)
				{
				char pathbuf[PATHSIZE];

				pathcat(pathbuf, pb->p_path, PLDNAME);
				if (FILEXIST(pathbuf) &&
				    getpwdir(pb->p_path) != NULL)
					{
					strcpy(pb->p_project, pb->p_path);
					pb->p_mode = P_IFHOME;
					}
				}
			pb->p_mode |= (unsigned long) stbuf.st_mode;
			}
		else	{
			if(errno < sys_nerr)
				PATHERR = sys_errlist[errno];
			if (pb->p_mode == P_IFREG)
				pb->p_mode = P_IFNEW;
			else	{	/* a project directory must exist */
				pstat = ERROR;
				}
			}
		pb->p_type = pb->p_path + strlen(pb->p_path) + 1;
		pb->p_desc = pb->p_type + 1;
		*pb->p_type = *pb->p_desc = '\0';
		}
	if (PPDEBUG == YES)
		warn("%s --> %s", ppathname, pb->p_path);
	return((pstat == ERROR) ? -1 : 0);
}



/*
 * pplex() gets next token. Returns first character of token.
 */
static char
pplex(token)
	register char *token;		/* receiving token */
{
	register char *rpp;		/* project pathname pointer */
	char t;				/* 1st token character */

	rpp = pp;
	t = *rpp;
	switch (t)
		{
		case _PPSC:
			*token++ = *rpp++;
			while (*rpp == _PPSC) rpp++;
			break;
		case _PSC:
			*token++ = *rpp++;
			while (*rpp == _PSC) rpp++;
			break;
		case '\0':
			break;
		default:
			while (*rpp != _PPSC && *rpp != _PSC && *rpp != '\0')
				*token++ = *rpp++;
			break;
		}
	*token = '\0';
	pp = rpp;
	return(t);
}



/*
 * patherr() prints the error message PATHERR to stderr stream and
 * returns constant 1.
 */
patherr(mesg)
	char *mesg;			/* error message */
{
	int strlen();			/* string length */

	if (strlen(mesg) > 0)
		warn("%s: %s", mesg, PATHERR);
	else
		warn("%s", PATHERR);
	return(1);
}
