static char *rcsid = "$Header$";
/*
 * pdiff - differential project comparator
 *
 * Author: Peter J. Nicklin
 */
#include <stdio.h>
#include "getarg.h"
#include "macro.h"
#include "null.h"
#include "path.h"
#include "pdb.h"
#include "pdtyp.h"
#include "pld.h"
#include "ptree.h"
#include "spms.h"
#include "yesno.h"

char **NARGV;				/* new argument list */
char *PGN = "pdiff";			/* program name */
int EXECUTE = YES;			/* execute command? */
int NARGI = 0;				/* new argument list index */
int RECURSIVE = 0;			/* recursively compare projects */
PDTYP PDIRTYP;				/* project directory type labels list */

main(argc, argv)
	int argc;
	char **argv;
{
	extern int PPDEBUG;		/* project pathname debug flag */
	char p1[PPATHSIZE];		/* project pathname buffer */
	char p2[PPATHSIZE];		/* project pathname buffer */
	char *strcpy();			/* string copy */
	int balance();			/* balance PDIR structs */
	int diffdir();			/* compare directories or files */
	int diffproject();		/* compare projects */
	int pdtparse();			/* parse boolean type label expr */
	int read_path();		/* read project or regular pathname */
	int status = 0;			/* exit status */
	PATH pb1;			/* pathname struct buffer 1 */
	PATH pb2;			/* pathname struct buffer 2 */
	unsigned long pathtyp1;		/* type of pathname 1 */
	unsigned long pathtyp2;		/* type of pathname 2 */

	NARGV = argv;
	NARGV[NARGI++] = "diff";

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
					if (pdtparse(GETARG(s), &PDIRTYP) == NO)
						status = 1;
					goto endfor;
				case 'r':
					RECURSIVE++;
					break;
				case 'x':
					EXECUTE = NO;
					break;
				default:
					NARGV[NARGI++] = *argv;
					break;
				}
		endfor: continue;
		}
	}
	if (status == 1 || argc < 2)
		{
		warn("usage: pdiff [-rx] [-T typexpr] [diff options] p1 p2");
		exit(2);
		}
	
	strcpy(p1, argv[0]);
	strcpy(p2, argv[1]);
	NARGV[NARGI+2] = NULL;

	if (read_path(p1, &pb1) == NO || read_path(p2, &pb2) == NO)
		exit(2);
	
	pathtyp1 = (pb1.p_mode & P_IFMT);
	pathtyp2 = (pb2.p_mode & P_IFMT);

	if (pathtyp1 == P_IFPROOT || pathtyp1 == P_IFHOME)
		{
		if (pathtyp2 == P_IFPROOT || pathtyp2 == P_IFHOME)
			status = diffproject(p1, pb1.p_path, p2, pb2.p_path);
		else if (balance(p1, &pb1, &pb2) == NO)
			exit(2);
		else
			status = diffdir(p1, pb1.p_path, p2, pb2.p_path);
		}
	else if (pathtyp2 == P_IFPROOT || pathtyp2 == P_IFHOME)
		{
		if (balance(p2, &pb2, &pb1) == NO)
			exit(2);
		else
			status = diffdir(p1, pb1.p_path, p2, pb2.p_path);
		}
	else	{
		status = diffdir(p1, pb1.p_path, p2, pb2.p_path);
		}

	if (status > 1)
		status = 2;
	exit(status);
}



/*
 * balance() balances a PATH struct given a project and a directory or file.
 * If a regular directory or file, then it is assumed to be relative to the
 * current working project directory. Returns NO if balancing fails, otherwise
 * YES. The project pathname p1 is updated to reflect the balancing.
 */
balance(p1, pb1, pb2)
	char *p1;			/* we know this is a project root dir */
	PATH *pb1;			/* project root directory buffer */
	PATH *pb2;			/* (project) directory or file buffer */
{
	char *pathcat();		/* regular pathname concatenation */
	char *ppathcat();		/* project pathname concatenation */
	int getcpd();			/* get current working project dir */
	int read_path();		/* read project or regular pathname */
	PATH cpathbuf;			/* current pathname struct buffer */

	if ((pb2->p_mode&P_IFMT) == P_IFPDIR)
		{
		ppathcat(p1, p1, pb2->p_alias);
		return(read_path(p1, pb1));
		}
	else if (*pb2->p_path == _RDIRC || getcpd(&cpathbuf) != 1)
		{
		warn("don't know which project directory to use in %s", p1);
		return(NO);
		}
	else	{
		ppathcat(p1, p1, cpathbuf.p_alias);
		pathcat(p1, p1, pb2->p_path);
		return(read_path(p1, pb1));
		}
}



/*
 * diffdir() compares two directories or files. Returns whatever diff()
 * returns.
 */
diffdir(pp1, d1, pp2, d2)
	char *pp1;			/* project pathname */
	char *d1;			/* directory or file pathname */
	char *pp2;			/* project pathname */
	char *d2;			/* directory or file pathname */
{
	static int have_printed;	/* has printing already been done? */
	int diff();			/* fork diff program */
	int iargi;			/* argument index */

	if (have_printed)
		putchar('\n');
	have_printed = 1;
	printf("==> ");
	for (iargi = 0; iargi < NARGI; iargi++)
		printf("%s ", NARGV[iargi]);
	printf("%s %s <==\n", pp1, pp2);
	fflush(stdout);

	if (EXECUTE == NO)
		return(0);

	NARGV[NARGI] = d1;
	NARGV[NARGI+1] = d2;
	return(diff(NARGV));
}



/*
 * diffproject() compares two projects, recursively if necessary. Returns
 * 0 if no differences, 1 if some, 2 if trouble.
 */
diffproject(pp1, p1, pp2, p2)
	char *pp1;			/* project pathname */
	char *p1;			/* project root directory pathname */
	char *pp2;			/* project pathname */
	char *p2;			/* project root directory pathname */
{
	int closepdb();			/* close database */
	int diffpdtree();		/* diff project directory tree */
	int diffptree();		/* diff tree of projects */
	int errpdb();			/* print database error message */
	int pdtmatch();			/* match project dir type label expr */
	int status = 0;			/* return status */
	PATH *pd;			/* pathname struct pointer */
	PATH *readpld();		/* read project link directory entry */
	PDB *openpdb();			/* open database */
	PDB *pldp;			/* project link directory stream */
	PTREE *proot;			/* root of project tree */
	PTREE *pdroot;			/* root of project directory tree */
	PTREE *ptree();			/* search and insert in proj dir tree */
	void ptreerm();			/* remove project directory tree */

	proot = NULL;
	pdroot = NULL;

	/* read PLDNAME project link directory belonging to p1 */
	if ((pldp = openpdb(PLDNAME, p1, "r")) == NULL)
		{
		errpdb((PDB *) NULL);
		return(2);
		}
	while ((pd = readpld(pldp)) != NULL)
		{
		if (EQUAL(pd->p_alias, PARENTPROJECT))
			continue;
		else if (EQUAL(pd->p_alias, CURPROJECT))
			{
			if (PDIRTYP.pfxsize==0 || pdtmatch(&PDIRTYP,pd->p_type)==YES)
				pdroot = ptree(pdroot, "", pd->p_path, CNULL);
			}
		else if (pd->p_mode == P_IFPROOT)
			{
			if (RECURSIVE)
				proot = ptree(proot, pd->p_alias, pd->p_path, CNULL);
			}
		else if (PDIRTYP.pfxsize==0 || pdtmatch(&PDIRTYP,pd->p_type)==YES)
			pdroot = ptree(pdroot, pd->p_alias, pd->p_path, CNULL);
		}
	closepdb(pldp);

	/* read PLDNAME project link directory belonging to p2 */
	if ((pldp = openpdb(PLDNAME, p2, "r")) == NULL)
		{
		errpdb((PDB *) NULL);
		return(2);
		}
	while ((pd = readpld(pldp)) != NULL)
		{
		if (EQUAL(pd->p_alias, PARENTPROJECT))
			continue;
		else if (EQUAL(pd->p_alias, CURPROJECT))
			{
			if (PDIRTYP.pfxsize==0 || pdtmatch(&PDIRTYP,pd->p_type)==YES)
				pdroot = ptree(pdroot, "", CNULL, pd->p_path);
			}
		else if (pd->p_mode == P_IFPROOT)
			{
			if (RECURSIVE)
				proot = ptree(proot, pd->p_alias, CNULL, pd->p_path);
			}
		else if (PDIRTYP.pfxsize==0 || pdtmatch(&PDIRTYP,pd->p_type)==YES)
			pdroot = ptree(pdroot, pd->p_alias, CNULL, pd->p_path);
		}
	closepdb(pldp);

	/* diff project directory tree */
	status |= diffpdtree(pdroot, pp1, pp2);
	ptreerm(pdroot);

	/* diff subprojects */
	status |= diffptree(proot, pp1, pp2);
	ptreerm(proot);

	return(status);
}



/*
 * diffpdtree() compares project directories in a project directory tree.
 */
diffpdtree(p, pp1, pp2)
	PTREE *p;			/* current node in project dir tree */
	char *pp1;			/* project pathname */
	char *pp2;			/* project pathname */
{
	char ppathbuf1[PPATHSIZE];	/* project pathname buffer */
	char ppathbuf2[PPATHSIZE];	/* project pathname buffer */
	char *ppathcat();		/* project pathname concantenation */
	int diffdir();			/* compare directories or files */
	int status = 0;			/* return status */
	void printonly();		/* print "only in project ..." mesg */

	if (p == NULL)
		return(0);
	status |= diffpdtree(p->left, pp1, pp2);
	if (p->pd1 == NULL)
		printonly(pp2, p->alias);
	else if (p->pd2 == NULL)
		printonly(pp1, p->alias);
	else	{
		ppathcat(ppathbuf1, pp1, p->alias);
		ppathcat(ppathbuf2, pp2, p->alias);
		status |= diffdir(ppathbuf1, p->pd1, ppathbuf2, p->pd2);
		}
	status |= diffpdtree(p->right, pp1, pp2);
	return(status);
}



/*
 * diffptree() compares projects in a project tree.
 */
diffptree(p, pp1, pp2)
	PTREE *p;			/* current node in project tree */
	char *pp1;			/* project pathname */
	char *pp2;			/* project pathname */
{
	char ppathbuf1[PPATHSIZE];	/* project pathname buffer */
	char ppathbuf2[PPATHSIZE];	/* project pathname buffer */
	char *ppathcat();		/* project pathname concantenation */
	int diffproject();		/* compare projects */
	int status = 0;			/* return status */
	void printonly();		/* print "only in project ..." mesg */

	if (p == NULL)
		return(0);
	status |= diffptree(p->left, pp1, pp2);
	if (p->pd1 == NULL)
		printonly(pp2, p->alias);
	else if (p->pd2 == NULL)
		printonly(pp1, p->alias);
	else	{
		ppathcat(ppathbuf1, pp1, p->alias);
		ppathcat(ppathbuf2, pp2, p->alias);
		status |= diffproject(ppathbuf1, p->pd1, ppathbuf2, p->pd2);
		}
	status |= diffptree(p->right, pp1, pp2);
	return(status);
}



/*
 * printonly() prints "only in project ..." message.
 */
void
printonly(project, pdirname)
	char *project;			/* project pathname */
	char *pdirname;			/* unique project directory */
{
	printf("Only in project %s: %s\n", project,
	      (*pdirname == '\0') ? CURPROJECT : pdirname);
	fflush(stdout);
}



/*
 * read_path() loads a PATH struct given a regular or project pathname.
 * Returns integer NO if an invalid pathname or non-existent target,
 * otherwise YES.
 */
read_path(pathname, pb)
	char *pathname;			/* regular or project pathname */
	PATH *pb;			/* pathname struct buffer */
{
	int readpath();			/* read project or regular pathname */

	if (readpath(pathname, pb) == -1 || (pb->p_mode & P_IFMT) == P_IFNEW)
		{
		patherr(pathname);
		return(NO);
		}
	return(YES);
}
