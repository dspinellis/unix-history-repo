static char *rcsid = "$Header$";
/*
 * prmdir - remove a project directory
 *
 * Author: Peter J. Nicklin
 */
#include <sys/param.h>
#include <signal.h>
#include <stdio.h>
#include "getarg.h"
#include "macro.h"
#include "null.h"
#include "path.h"
#include "pdb.h"
#include "pld.h"
#include "slist.h"
#include "spms.h"
#include "system.h"
#include "yesno.h"

char CWD[PATHSIZE];			/* current working directory */
char *CWP;				/* current working project */
char *RMFLAG = "";			/* rm "-f" flag */
char *PGN = "prmdir";			/* program name */
int RECURSIVE = 0;			/* remove project dirs recursively */
int UNDEFINE = 0;			/* remove proj dir definitions only */
int WANT_TO_EXIT = 0;			/* advisory exit flag */

main(argc, argv)
	int argc;
	char **argv;
{
	extern int PPDEBUG;		/* project pathname debug flag */
	char *getcwp();			/* get current working project */
	char *getwd();			/* get current working directory */
	int isfg();			/* is process in foreground? */
	int onintr();			/* process signals */
	int rmpdir();			/* remove a project directory */
	int rmtyp();			/* remove project dir type labels */
	int unpdir();			/* undefine project directory */
	int status = 0;			/* exit status */
	int xppath();			/* expand project pathname */
	PATH pathbuf;			/* pathname struct buffer */
	SLIST *pdirtyp;			/* project directory type labels list */
	SLIST *slinit();		/* initialize singly-linked list */
	void typargtolist();		/* type labels -> pdirtyp list */

	pdirtyp = slinit();
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
				case 'T':
					typargtolist(GETARG(s), pdirtyp);
					if (*s == '\0')
						status = 1;
					goto endfor;
				case 'f':
					RMFLAG = "-f";
					break;
				case 'r':
					RECURSIVE++;
					break;
				case 'u':
					UNDEFINE++;
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
		fatal("usage: prmdir [-fru] [-T type[,type...]] pdirname ...");

	if ((CWP = getcwp()) == NULL)
		fatal("no project environment");
	if (getwd(CWD) == NULL)
		fatal("can't find current working directory");
	if (isfg() == YES)
		{
		signal(SIGHUP, onintr);
		signal(SIGINT, onintr);
		signal(SIGQUIT, onintr);
		}

	for (; argc > 0; ++argv, --argc)
		{
		if (xppath(*argv, &pathbuf) == -1)
			{
			patherr(*argv);
			status = 1;
			continue;
			}
		switch (pathbuf.p_mode & P_IFMT)
			{
			case P_IFPDIR:
				if (SLNUM(pdirtyp) > 0)
					status |= rmtyp(*argv, pdirtyp, &pathbuf);
				else if (UNDEFINE)
					status |= unpdir(&pathbuf);
				else
					status |= rmpdir(*argv, &pathbuf);
				break;
			case P_IFHOME:
			case P_IFPROOT:
				warn("%s is a project root directory", *argv);
				status = 1;
				break;
			case P_IFNEW:
			case P_IFREG:
				warn("%s: no such project directory", *argv);
				status = 1;
				break;
			}
		if (WANT_TO_EXIT)
			exit(1);
		}
	exit(status);
}



/*
 * onintr() resets interrupt, quit, and hangup signals, and sets a flag
 * which advises the process to exit at the first opportunity.
 */
onintr()
{
	signal(SIGINT, onintr);
	signal(SIGQUIT, onintr);
	signal(SIGHUP, onintr);

	WANT_TO_EXIT = 1;
}



/*
 * pbrmtyp() removes type labels from database buffer.
 */
void
pbrmtyp(ppathname, typlist)
	char *ppathname;		/* project pathname */
	SLIST *typlist;			/* type labels list */
{
	char *pbgetstring();		/* get specified string field */
	char *pdtfind();		/* find type label in buffer */
	char *slget();			/* get next key from list */
	char *tp;			/* pointer to type label */
	char typbuf[TYPBUFSIZE];	/* project directory types buffer */
	int pbaddstring();		/* add string field */
	int strlen();			/* string length */
	void slrewind();		/* rewind list */
	void pdtrm();			/* remove type label */

	pbgetstring(PDIRTYPE, typbuf);
	slrewind(typlist);
	while ((tp = slget(typlist)) != NULL)
		{
		if (pdtfind(tp, typbuf) != NULL)
			pdtrm(tp, typbuf);
		else
			warn("%s: \"%s\" type label not found", ppathname, tp);
		}
	pbaddstring(PDIRTYPE, typbuf);
}



/*
 * rmd() removes a project directory. rmd() returns the status of the rm
 * command or 1 if the user decides not to remove a project directory.
 */
rmd(pathname)
	char *pathname;			/* full pathname of directory */
{
	char cmdbuf[PATHSIZE+9];	/* command buffer */
	char *sprintf();		/* print output to string */
	int status;			/* return status */
	int yes();			/* is reply yes? */

	if (RECURSIVE)
		{
		sprintf(cmdbuf, "rm %s -r %s", RMFLAG, pathname);
		printf("%s? [yn](n): ", cmdbuf);
		if (!yes())
			return(1);
		status = system(cmdbuf);
		status >>= NBBY;
		status &= 0xff;
		}
	else	{
		status = RM_DIR(pathname);
		}
	return(status);
}



/*
 * rmpdir() removes a project directory. Returns 0 is successful, otherwise 1.
 */
rmpdir(ppathname, pb)
	char *ppathname;		/* project directory pathname */
	PATH *pb;			/* pathname struct buffer */
{
	int _closepdb();		/* close database without updating */
	int closepdb();			/* close database */
	int errpdb();			/* print database error */
	int plen;			/* length of regular pathname */
	int pputent();			/* write buffer to database */
	int status = 0;			/* return status */
	int strlen();			/* string length */
	int strncmp();			/* compare n characters */
	PATH *pd;			/* pathname struct pointer */
	PATH *readpld();		/* read project link directory entry */
	PDB *openpdb();			/* open database */
	PDB *pldp;			/* project link directory stream */

	plen = strlen(pb->p_path);
	if (strncmp(pb->p_path, CWD, plen) == 0)
		{
		warn("can't remove %s from current directory", ppathname);
		return(1);
		}
	if ((pldp = openpdb(PLDNAME, pb->p_project, "rw")) == NULL)
		return(errpdb((PDB *) NULL));
	while ((pd = readpld(pldp)) != NULL)
		if (EQUAL(pb->p_alias, pd->p_alias))
			continue;	/* the directory itself */
		else if (strncmp(pb->p_path, pd->p_path, plen) == 0)
			{		/* nest project directories */
			if (pd->p_mode == P_IFPROOT)
				{	/* don't clobber projects */
				warn("can't remove %s because project %s exists",
				     ppathname, pd->p_path);
				status = 1;
				break;
				}
			}
		else	{
			pputent(pldp);
			}
	if (status == 1)
		_closepdb(pldp);
	else	{
		if ((status = rmd(pb->p_path)) != 0)
			_closepdb(pldp);
		else
			status = closepdb(pldp);
		}
	return(status);
}



/*
 * rmtyp() removes type labels from an existing project directory.
 */
rmtyp(ppathname, pdirtyp, pb)
	char *ppathname;		/* project directory pathname */
	SLIST *pdirtyp;			/* project directory type labels list */
	PATH *pb;			/* pathname struct buffer */
{
	char *pbfndkey();		/* find key */
	int closepdb();			/* close database */
	int errpdb();			/* print database error */
	int pgetent();			/* load next entry into buffer */
	int pputent();			/* write buffer to database */
	int status = 0;			/* return status */
	PDB *openpdb();			/* open database */
	PDB *pldp;			/* project link directory stream */
	void pbrmtyp();			/* remove type labels from buffer */

	if ((pldp = openpdb(PLDNAME, pb->p_project, "rw")) == NULL)
		return(errpdb((PDB *) NULL));
	while (pgetent(pldp) != EOF)
		{
		if (pbfndkey(pb->p_alias) != NULL)
			pbrmtyp(ppathname, pdirtyp);
		pputent(pldp);
		}
	status = closepdb(pldp);
	return(status);
}



/*
 * typargtolist() prepends comma-separated type labels specified in typarg
 * to typlist.
 */
void
typargtolist(typarg, typlist)
	register char *typarg;		/* type labels argument */
	SLIST *typlist;			/* type labels list */
{
	register char *t;		/* type label argument pointer */
	char *slprepend();		/* prepend singly-linked list key */

	for (t = typarg; *t != '\0'; t++)
		continue;
	for (; t >= typarg; t--)
		if (t[0] == ',')
			{
			if (t[1] != '\0')
				slprepend(t+1, typlist);
			t[0] = '\0';
			}
	slprepend(typarg, typlist);
}



/*
 * unpdir() undefines a project directory. Returns 0 is successful, otherwise 1.
 */
unpdir(pb)
	PATH *pb;			/* pathname struct buffer */
{
	int closepdb();			/* close database */
	int errpdb();			/* print database error */
	int pputent();			/* write buffer to database */
	PATH *pd;			/* pathname struct pointer */
	PATH *readpld();		/* read project link directory entry */
	PDB *openpdb();			/* open database */
	PDB *pldp;			/* project link directory stream */

	if ((pldp = openpdb(PLDNAME, pb->p_project, "rw")) == NULL)
		return(errpdb((PDB *) NULL));
	while ((pd = readpld(pldp)) != NULL)
		if (EQUAL(pb->p_alias, pd->p_alias))
			continue;
		else
			pputent(pldp);
	return(closepdb(pldp));
}
