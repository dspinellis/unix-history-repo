static char *rcsid = "$Header$";
/*
 * pmkdir - make a project directory
 *
 * Author: Peter J. Nicklin
 */
#include <signal.h>
#include <stdio.h>
#include <sys/types.h>
#include <sys/stat.h>
#include "getarg.h"
#include "macro.h"
#include "null.h"
#include "path.h"
#include "pdb.h"
#include "pld.h"
#include "slist.h"
#include "system.h"
#include "yesno.h"

char CWD[PATHSIZE];			/* current working directory */
char *CWP;				/* current working project */
char *PGN = "pmkdir";			/* program name */
int WANT_TO_EXIT = 0;			/* advisory exit flag */

main(argc, argv)
	int argc;
	char **argv;
{
	extern int PPDEBUG;		/* project pathname debug flag */
	char *alias = NULL;		/* alternative project directory name */
	char *getcwp();			/* get current working project */
	char *getwd();			/* get current working directory */
	int addtyp();			/* make project directory type labels */
	int chalias();			/* change project directory alias */
	int chdesc();			/* change project directory descrip */
	int isfg();			/* is process in foreground? */
	int minusdflag = YES;		/* project directory description flag */
	int mkpdir();			/* make a project directory */
	int mustexist = 0;		/* existing directories flag */
	int onintr();			/* process signals */
	int plusdflag = NO;		/* project directory description flag */
	int status = 0;			/* exit status */
	int typargtolist();		/* type labels -> pdirtyp list */
	int xppath();			/* expand project pathname */
	PATH pathbuf;			/* pathname struct buffer */
	SLIST *pdirtyp;			/* project directory type labels list */
	SLIST *slinit();		/* initialize singly-linked list */

	pdirtyp = slinit();
	{
	register char *s;		/* option pointer */
	while (--argc > 0 && (**++argv == '-' || **argv == '+'))
		{
		if (**argv == '-')
			{
			for (s = argv[0]+1; *s != '\0'; s++)
				switch (*s)
					{
					case 'D':
						PPDEBUG = YES;
						break;
					case 'N':
						alias = GETARG(s);
						if (*alias == '\0')
							status = 1;
						goto endif;
					case 'T':
						if (typargtolist(GETARG(s),pdirtyp)==NO)
							status = 1;
						else if (*s == '\0')
							status = 1;
						goto endif;
					case 'd':
						minusdflag = NO;
						break;
					default:
						warn("bad option -%c", *s);
						status = 1;
						goto endif;
					}
			}
		else	{
			mustexist = 1;
			for (s = argv[0]+1; *s != '\0'; s++)
				switch (*s)
					{
					case 'N':
						alias = GETARG(s);
						if (*alias == '\0')
							status = 1;
						goto endif;
					case 'T':
						if (typargtolist(GETARG(s),pdirtyp)==NO)
							status = 1;
						else if (*s == '\0')
							status = 1;
						goto endif;
					case 'd':
						plusdflag = YES;
						break;
					default:
						warn("bad option +%c", *s);
						status = 1;
						goto endif;
					}
			}
		endif: continue;
		}
	if (status == 1 || argc < 1)
		fatal("usage: pmkdir [{+-}d] [{+-}N alias] %s",
		      "[{+-}T type[,type...]]\n        pdirname ...");
	}

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
		if (mustexist)
			switch (pathbuf.p_mode & P_IFMT)
				{
				case P_IFPDIR:
					if (SLNUM(pdirtyp) > 0)
						status |= addtyp(*argv, pdirtyp,
							  &pathbuf);
					if (plusdflag == YES)
						status |= chdesc(*argv, &pathbuf);
					if (alias != NULL)
						status |= chalias(*argv, alias,
							  &pathbuf);
					break;
				case P_IFNEW:
				case P_IFREG:
					warn("%s: no such project directory", *argv);
					status = 1;
					break;
				case P_IFHOME:
				case P_IFPROOT:
					warn("%s is a project root directory", *argv);
					status = 1;
					break;
				}
		else
			status |= mkpdir(*argv, alias, pdirtyp, minusdflag, &pathbuf);
		if (WANT_TO_EXIT)
			exit(1);
		}
	exit(status);
}



/*
 * addtyp() adds type labels to an existing project directory.
 */
addtyp(ppathname, pdirtyp, pb)
	char *ppathname;		/* project directory pathname */
	SLIST *pdirtyp;			/* project directory type labels list */
	PATH *pb;			/* pathname struct buffer */
{
	char *pbfndkey();		/* find key */
	int closepdb();			/* close database */
	int errpdb();			/* print database error */
	int pgetent();			/* load next entry into buffer */
	int pputent();			/* write buffer to database */
	PDB *openpdb();			/* open database */
	PDB *pldp;			/* project link directory stream */
	void pbaddtyp();		/* add type labels to buffer */

	if ((pldp = openpdb(PLDNAME, pb->p_project, "rw")) == NULL)
		return(errpdb((PDB *) NULL));
	while (pgetent(pldp) != EOF)
		{
		if (pbfndkey(pb->p_alias) != NULL)
			pbaddtyp(ppathname, pdirtyp);
		pputent(pldp);
		}
	return(closepdb(pldp));
}



/*
 * badtyp() prints a bad format type label message.
 */
void
badtyp(type)
	char *type;			/* type label */
{
	warn("\"%s\" type label is badly formatted", type);
}



/*
 * chalias() changes an existing project directory alias.
 */
chalias(ppathname, newalias, pb)
	char *ppathname;		/* project directory pathname */
	char *newalias;			/* new project directory alias */
	PATH *pb;			/* pathname struct buffer */
{
	char *pbfndkey();		/* find key */
	int _closepdb();		/* close database without updating */
	int closepdb();			/* close database */
	int errpdb();			/* print database error */
	int pbchgkey();			/* change existing key */
	int pgetent();			/* load next entry into buffer */
	int pputent();			/* write buffer to database */
	PDB *openpdb();			/* open database */
	PDB *pldp;			/* project link directory stream */

	if ((pldp = openpdb(PLDNAME, pb->p_project, "rw")) == NULL)
		return(errpdb((PDB *) NULL));
	while (pgetent(pldp) != EOF)
		{
		if (pbfndkey(newalias) != NULL)
			{
			warn("%s: %s exists", ppathname, newalias);
			_closepdb(pldp);
			return(1);
			}
		pbchgkey(pb->p_alias, newalias);
		pputent(pldp);
		}
	return(closepdb(pldp));
}



/*
 * chdesc() changes an existing project directory description.
 */
chdesc(ppathname, pb)
	char *ppathname;		/* project directory pathname */
	PATH *pb;			/* pathname struct buffer */
{
	char *pbfndkey();		/* find key */
	int closepdb();			/* close database */
	int errpdb();			/* print database error */
	int pgetent();			/* load next entry into buffer */
	int pputent();			/* write buffer to database */
	PDB *openpdb();			/* open database */
	PDB *pldp;			/* project link directory stream */
	void pbadddesc();		/* add project directory description */

	if ((pldp = openpdb(PLDNAME, pb->p_project, "rw")) == NULL)
		return(errpdb((PDB *) NULL));
	while (pgetent(pldp) != EOF)
		{
		if (pbfndkey(pb->p_alias) != NULL)
			pbadddesc(ppathname);
		pputent(pldp);
		}
	return(closepdb(pldp));
}



/*
 * mkpdir() makes a project directory.
 */
mkpdir(ppathname, alias, pdirtyp, dflag, pb)
	char *ppathname;		/* project directory pathname */
	char *alias;			/* alternative project directory name */
	int dflag;			/* project directory description flag */
	SLIST *pdirtyp;			/* project directory type labels list */
	PATH *pb;			/* pathname struct buffer */
{
	char apathname[PATHSIZE];	/* absolute regular pathname */
	char *mkalias();		/* construct alias from pathname */
	char *optpath();		/* optimize pathname */
	char *pathcat();		/* pathname concatenation */
	char *pbfndkey();		/* find database key */
	char *rdp;			/* relative project directory path */
	char rpathname[PATHSIZE];	/* project root directory pathname */
	char *strcpy();			/* string copy */
	char *xorpath();		/* remove subpathname */
	int _closepdb();		/* close database without updating */
	int closepdb();			/* close database */
	int errpdb();			/* print database error message */
	int pbaddkey();			/* add key */
	int pbaddstring();		/* add string field */
	int pgetent();			/* load next entry into buffer */
	int pputent();			/* write buffer to database */
	PDB *openpdb();			/* open database */
	PDB *pldp;			/* project link directory stream */
	void pbadddesc();		/* add project directory description */
	void pbaddtyp();		/* add type labels to buffer */
	void pbclear();			/* clear buffer */

	switch (pb->p_mode & P_IFMT)
		{
		case P_IFNEW:
		case P_IFREG:
			break;
		case P_IFPDIR:
		case P_IFHOME:
		case P_IFPROOT:
			warn("%s exists", ppathname);
			return(1);
		}

	/* create pathname relative to project root directory */
	strcpy(rpathname, pb->p_project);
	if (*pb->p_path == _RDIRC)
		{
		rdp = xorpath(rpathname, pb->p_path);
		}
	else	{
		optpath(pathcat(apathname, CWD, pb->p_path));
		rdp = xorpath(rpathname, apathname);
		}

	/* open project link directory */
	if ((pldp = openpdb(PLDNAME, rpathname, "rw")) == NULL)
		return(errpdb((PDB *) NULL));

	/*
	 * check for existing aliases while preparing project link
	 * directory for new entry.
	 */
	if (alias == NULL)
		if (EQUAL(pb->p_alias, CURDIR) || EQUAL(pb->p_alias, PARENTDIR))
			alias = mkalias(rdp);
		else
			alias = pb->p_alias;
	while (pgetent(pldp) != EOF)
		{
		if (pbfndkey(alias) != NULL)
			{
			warn("%s: %s exists", ppathname, alias);
			_closepdb(pldp);
			return(1);
			}
		pputent(pldp);
		}

	/* make the directory if non-existent */
	if ((pb->p_mode & P_IFMT) == P_IFREG)
		{
		if ((pb->p_mode & S_IFMT) != S_IFDIR)
			{
			warn("%s: not a directory", ppathname);
			_closepdb(pldp);
			return(1);
			}
		}
	else if (MK_DIR(pb->p_path) != 0)
		{
		_closepdb(pldp);
		return(1);
		}

	/* update database */
	pbclear();
	pbaddkey(alias);
	pbaddstring(PDIRPATH, rdp);
	pbaddtyp(ppathname, pdirtyp);
	if (dflag == YES)
		pbadddesc(ppathname);
	else
		pbaddstring(PDIRDESC, "");
	pputent(pldp);
	return(closepdb(pldp));
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
 * pbadddesc() fetchs a project directory description from stdin and
 * adds to database buffer.
 */
void
pbadddesc(ppathname)
	char *ppathname;		/* project directory pathname */
{
	char dirdesc[DIRDESCSIZE];	/* project directory description */
	char *gets();			/* get a line from stdin */
	int pbaddstring();		/* add string field */

	printf("%s: description? (1 line): ", ppathname);
	gets(dirdesc);
	pbaddstring(PDIRDESC, dirdesc);
}



/*
 * pbaddtyp() adds type labels to database buffer.
 */
void
pbaddtyp(ppathname, typlist)
	char *ppathname;		/* project pathname */
	SLIST *typlist;			/* type labels list */
{
	char *pbgetstring();		/* get specified string field */
	char *pdtfind();		/* find type label in buffer */
	char *pfxcpy();			/* copy string prefix */
	char *slget();			/* get next key from list */
	char *tp;			/* pointer to type label */
	char type[TYPESIZE];		/* type label buffer */
	char typbuf[TYPBUFSIZE];	/* project directory types buffer */
	int pbaddstring();		/* add string field */
	void pdtinsert();		/* insert type label */
	void slrewind();		/* rewind list */

	pbgetstring(PDIRTYPE, typbuf);
	slrewind(typlist);
	while ((tp = slget(typlist)) != NULL)
		{
		if (pdtfind(pfxcpy(type, tp), typbuf) == NULL)
			pdtinsert(tp, typbuf);
		else
			warn("%s: \"%s\" type label exists", ppathname, type);
		}
	pbaddstring(PDIRTYPE, typbuf);
}



/*
 * typargtolist() prepends comma-separated type labels specified in typarg
 * to typlist. Returns NO if type labels are badly formatted, otherwise
 * YES.
 */
typargtolist(typarg, typlist)
	register char *typarg;		/* type labels argument */
	SLIST *typlist;			/* type labels list */
{
	register char *t;		/* type label argument pointer */
	char *slprepend();		/* prepend singly-linked list key */
	int ispdt();			/* is project dir type label legal? */
	int status = YES;		/* return status */
	void badtyp();			/* print bad type label message */

	for (t = typarg; *t != '\0'; t++)
		continue;
	for (; t >= typarg; t--)
		if (t[0] == ',')
			{
			if (t[1] != '\0')
				if (ispdt(t+1))
					slprepend(t+1, typlist);
				else	{
					badtyp(t+1);
					status = NO;
					}
			t[0] = '\0';
			}
	if (ispdt(typarg))
		slprepend(typarg, typlist);
	else	{
		badtyp(typarg);
		status = NO;
		}
	return(status);
}
