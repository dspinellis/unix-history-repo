static char *rcsid = "$Header$";
/*
 * rmproject - remove a project root directory
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
char *PGN = "rmproject";		/* program name */
char *RMFLAG = "";			/* rm "-f" flag */
int FORCE = 0;				/* brute force remove or undefine */
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
	int rmproject();		/* remove a project root directory */
	int rmtyp();			/* remove project dir type labels */
	int status = 0;			/* exit status */
	int unproject();		/* undefine project root directory */
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
				case 'F':
					/*
					 * 'F' option is mentioned in
					 * rmproject() and unproject()
					 */
					FORCE++;
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
		fatal("usage: rmproject [-fru] [-T type[,type...]] projectname ...");

	if ((CWP = getcwp()) == NULL)
		fatal("no project environment");
	if (getwd(CWD) == NULL)
		fatal("can't find current working directory");
	if (isfg() == YES)
		{
		signal(SIGINT, onintr);
		signal(SIGQUIT, onintr);
		signal(SIGHUP, onintr);
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
			case P_IFHOME:
			case P_IFPROOT:
				if (SLNUM(pdirtyp) > 0)
					status |= rmtyp(*argv, pdirtyp, &pathbuf);
				else if (UNDEFINE)
					status |= unproject(*argv, &pathbuf);
				else
					status |= rmproject(*argv, &pathbuf);
				break;
			case P_IFPDIR:
				warn("%s is a project directory", *argv);
				status = 1;
				break;
			case P_IFNEW:
			case P_IFREG:
				warn("%s: no such project", *argv);
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
 * Before removing a directory the project link directory is moved to
 * safe place. If the directory is removed successfully, the project link
 * directory is removed.
 */
rmd(pathname)
	char *pathname;			/* full pathname of directory */
{
	char cmdbuf[PATHSIZE+9];	/* command buffer */
	int rmpld();			/* remove project link directory */
	int savepld();			/* save project link directory */
	int status;			/* return status */
	int yes();			/* is reply yes? */
	void restorpld();		/* restore project link directory */
	void unsavepld();		/* remove saved project link dir */

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
		if ((status = savepld(pathname)) == 0)
			if ((status = rmpld(pathname)) == 0)
				if ((status = RM_DIR(pathname)) != 0)
					restorpld(pathname);
				else
					unsavepld();
		}
	return(status);
}



/*
 * rmpld() removes a project link directory. Returns 1 if file not
 * removed, otherwise zero.
 */
rmpld(pathname)
	char *pathname;			/* project root directory pathname */
{
	char *pathcat();		/* pathname concatenation */
	char pldpathname[PATHSIZE];	/* project link directory pathname */

	if (unlink(pathcat(pldpathname, pathname, PLDNAME)) != 0)
		if (!FORCE)
			{
			pperror(pldpathname);
			return(1);
			}
	return(0);
}



/*
 * rmproject() removes a project root directory. Returns 0 is successful,
 * otherwise 1.
 */
rmproject(ppathname, pb)
	char *ppathname;		/* project root directory pathname */
	PATH *pb;			/* pathname struct buffer */
{
	char *ppathcat();		/* project pathname concatenation */
	char *ppathhead();		/* remove tail of project pathname */
	char pppathname[PPATHSIZE];	/* parent project pathname */
	char *strcpy();			/* string copy */
	int _closepdb();		/* close database without updating */
	int closepdb();			/* close database */
	int errpdb();			/* print database error */
	int plen;			/* length of regular pathname */
	int pputent();			/* write buffer to database */
	int pstatus = 0;		/* project status */
	int strlen();			/* string length */
	int strncmp();			/* compare n characters */
	int subprojects();		/* check for subprojects */
	int xppath();			/* expand project pathname */
	PATH *pd;			/* pathname struct pointer */
	PATH ppathbuf;			/* parent project pathname struct buf */
	PATH *readpld();		/* read project link directory entry */
	PDB *openpdb();			/* open database */
	PDB *pldp;			/* project link directory stream */
	void resetpdb();		/* reset current database pointer */

	if (EQUAL(ppathname, CURPROJECT))
		{
		warn("%s: can't remove current project", ppathname);
		return(1);
		}
	plen = strlen(pb->p_path);
	if (strncmp(pb->p_path, CWD, plen) == 0)
		{
		warn("can't remove %s from current directory", ppathname);
		return(1);
		}
	if (FORCE)
		{
		if (*ppathname != _PDIRC && *ppathname != _HDIRC)
			{
			warn("%s must an absolute project pathname", ppathname);
			return(1);
			}
		ppathhead(strcpy(pppathname, ppathname));
		}
	else	{
		ppathcat(pppathname, ppathname, PARENTPROJECT);
		}
	if (xppath(pppathname, &ppathbuf) == -1)
		{
		patherr("");
		warn("force removal by typing `rmproject -F projectname'");
		return(1);
		}
	if ((pldp = openpdb(PLDNAME, ppathbuf.p_path, "rw")) == NULL)
		return(errpdb((PDB *) NULL));
	while ((pd = readpld(pldp)) != NULL)
		if (EQUAL(pb->p_alias, pd->p_alias))
			{
			pstatus = subprojects(pb->p_path);
			if (pstatus == 1)
				{
				warn("%s: project not empty", ppathname);
				resetpdb(pldp);
				break;
				}
			else if (pstatus == 2)
				{
				warn("can't remove %s because subprojects exist",
				     ppathname);
				resetpdb(pldp);
				break;
				}
			else if (pstatus == 3)
				{
				resetpdb(pldp);
				break;
				}
			resetpdb(pldp);
			}
		else if (strncmp(pb->p_path, pd->p_path, plen) == 0)
			{		/* don't clobber nested projects */
			if (pd->p_mode == P_IFPROOT)
				{
				warnexist(ppathname, pd->p_alias);
				pstatus = 4;
				break;
				}
			}
		else	{
			pputent(pldp);
			}
	if (pstatus != 0)
		_closepdb(pldp);
	else if ((pstatus = rmd(pb->p_path)) != 0)
		_closepdb(pldp);
	else
		pstatus = closepdb(pldp);
	return(pstatus != 0);
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
		if (pbfndkey(CURPROJECT) != NULL)
			pbrmtyp(ppathname, pdirtyp);
		pputent(pldp);
		}
	status = closepdb(pldp);
	return(status);
}



/*
 * subprojects() returns 1 if a project has project directories, 2 if any
 * of these are subprojects, and zero otherwise.
 */
subprojects(pathname)
	char *pathname;			/* project root directory pathname */
{
	char *pbfndkey();		/* find key */
	int closepdb();			/* close database */
	int errpdb();			/* print database error */
	int pbfndflag();		/* find flag field */
	int pgetent();			/* load next entry into buffer */
	int status = 0;			/* return status */
	PDB *openpdb();			/* open database */
	PDB *pldp;			/* project link directory stream */

	if ((pldp = openpdb(PLDNAME, pathname, "r")) == NULL)
		{
		if (!FORCE) return(errpdb((PDB *) NULL));
		return(status);
		}
	while (pgetent(pldp) != EOF)
		if (pbfndkey(CURPROJECT) == NULL &&
		    pbfndkey(PARENTPROJECT) == NULL)
			{
			if (!RECURSIVE) status = 1;
			if (pbfndflag(PROOTDIR) == YES)
				{
				status = 2;
				break;
				}
			}
	closepdb(pldp);
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
 * unproject() undefines a project root directory. Returns 0 is successful,
 * otherwise 1.
 */
unproject(ppathname, pb)
	char *ppathname;		/* project root directory pathname */
	PATH *pb;			/* pathname struct buffer */
{
	char *ppathcat();		/* project pathname concatenation */
	char *ppathhead();		/* remove tail of project pathname */
	char pppathname[PPATHSIZE];	/* parent project pathname */
	char *strcpy();			/* string copy */
	int _closepdb();		/* close database without updating */
	int closepdb();			/* close database */
	int errpdb();			/* print database error */
	int pputent();			/* write buffer to database */
	int pstatus = 0;		/* project status */
	int rmpld();			/* remove project link directory */
	int subprojects();		/* check for subprojects */
	int xppath();			/* expand project pathname */
	PATH *pd;			/* pathname struct pointer */
	PATH ppathbuf;			/* parent project pathname struct buf */
	PATH *readpld();		/* read project link directory entry */
	PDB *openpdb();			/* open database */
	PDB *pldp;			/* project link directory stream */
	void resetpdb();		/* reset current database pointer */

	if (EQUAL(ppathname, CURPROJECT))
		{
		warn("%s: can't undefine current project", ppathname);
		return(1);
		}
	if (FORCE)
		{
		if (*ppathname != _PDIRC && *ppathname != _HDIRC)
			{
			warn("%s must an absolute project pathname", ppathname);
			return(1);
			}
		ppathhead(strcpy(pppathname, ppathname));
		}
	else	{
		ppathcat(pppathname, ppathname, PARENTPROJECT);
		}
	if (xppath(pppathname, &ppathbuf) == -1)
		{
		patherr("");
		warn("force conversion by typing `rmproject -uF projectname'");
		return(1);
		}
	if ((pldp = openpdb(PLDNAME, ppathbuf.p_path, "rw")) == NULL)
		return(errpdb((PDB *) NULL));
	while ((pd = readpld(pldp)) != NULL)
		if (EQUAL(pb->p_alias, pd->p_alias))
			{
			pstatus = subprojects(pb->p_path);
			if (pstatus == 1)
				{
				warn("can't undefine %s: project not empty",
				     ppathname);
				resetpdb(pldp);
				break;
				}
			else if (pstatus == 2)
				{
				warn("can't undefine %s because subprojects exist",
				     ppathname);
				resetpdb(pldp);
				break;
				}
			else if (pstatus == 3)
				{
				resetpdb(pldp);
				break;
				}
			resetpdb(pldp);
			}
		else	{
			pputent(pldp);
			}
	if (pstatus != 0)
		_closepdb(pldp);
	else if ((pstatus = rmpld(pb->p_path)) != 0)
		_closepdb(pldp);
	else
		pstatus = closepdb(pldp);
	return(pstatus != 0);
}



/*
 * warnexist() warns of nested or duplicate projects.
 */
warnexist(ppathname, alias)
	char *ppathname;		/* project to be removed */
	char *alias;			/* nested or duplicate project alias */
{
	char npathname[PPATHSIZE];	/* nested project pathname */
	char *p;			/* nested project pathname pointer */
	char *strpcpy();		/* string copy and update pointer */

	p = strpcpy(npathname, ppathname);
	while (p > npathname && p[-1] != _PPSC)
		p--;
	*p = '\0';
	warn("can't remove %s because project %s%s exists", ppathname,
	     npathname, alias);
}
