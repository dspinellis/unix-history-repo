/*

 *      Copyright (c) 1984, 1985, 1986 AT&T
 *      All Rights Reserved

 *      THIS IS UNPUBLISHED PROPRIETARY SOURCE 
 *      CODE OF AT&T.
 *      The copyright notice above does not 
 *      evidence any actual or intended
 *      publication of such source code.

 */
/* @(#)adjust.c	1.1 */

/*
 *   ADJUST.C
 *
 *   Programmer:  D. A. Lambeth
 *
 *        Owner:  D. A. Lambeth
 *
 *         Date:  April 17, 1980
 *
 *
 *
 *   CHATTRIB (NODE, NEWATTS)
 *
 *        Give NODE the attribute(s) NEWATTS, and change its
 *        value to conform to the new attributes.
 *
 *
 *
 *   See Also:  TYPESET(I)
 */


#include     "name.h"
#include     "flags.h"

extern char *valup();
extern char *malloc();
extern char *strcpy();
extern void fassign();
extern void unassign();
extern void free();
#ifdef BSD
#define strchr	index
#endif	/* BSD */
extern char *strchr();
#ifdef NAME_SCOPE
extern struct Namnod *copy_nod();
#endif
#ifdef apollo
extern void	ev_$delete_var();
extern void	ev_$set_var();
#endif /* apollo */


/*
 *   CHATTRIB (NODE, NEWATTS)
 *
 *        struct Namnod *NODE;
 *
 *        int NEWATTS;
 *
 *	  int size;
 *
 *   Give NODE the attributes NEWATTS, and change its current
 *   value to conform to NEWATTS.  The SIZE of left and right
 *   justified fields may be given.
 */

chattrib (node, newatts, size)
struct Namnod *node;
unsigned int newatts;
{
	register char *sp;
	register char *cp = NULL;
	register struct Namnod *np = node;
	register unsigned int n;

#ifdef NAME_SCOPE
	if(np->value.namflg&C_WRITE)
		np = copy_nod(np,1);
#endif
	/* handle attributes that do not change data separately */
	n = np->value.namflg;
#ifdef apollo
	if(((n^newatts)&N_EXPORT))
	/* record changes to the environment */
	{
		short namlen = strlen(np->namid);
		if(cp = strchr(np->namid,'='))
		{
			namlen = cp - np->namid;
			cp = NULL;
		}
		if(n&N_EXPORT)
			ev_$delete_var(np->namid,&namlen);
		else
		{
			char *vp = valup(np);
			short vallen = strlen(vp);
			ev_$set_var(np->namid,&namlen,vp,&vallen);
		}
	}
#endif /* apollo */
	if(size==0 && ((n^newatts)&~(E_FLAG|N_IMPORT|N_EXPORT|N_RDONLY|T_FORM))==0)
	{
		if(((n^newatts)&N_EXPORT) && (cp=strchr(np->namid,'=')))
		{
			/* get rid of import attribute */
			*cp = 0;
			newatts &= ~N_IMPORT;
		}
		np->value.namflg = newatts;
		return;
	}
	if (sp = valup (np))
 	{
		cp = malloc ((n=strlen (sp)) + 1);
		strcpy (cp, sp);
		unassign(np);
		if(size==0 && (newatts&(L_JUST|R_JUST|Z_FILL)))
			np->namsz =  n;
	 else
			np->namsz = size;
	}
	else
		np->namsz = size;
	np->value.namflg = newatts;
	if (cp != NULL)
	{
		fassign (np, cp);
		free(cp);
	}
	return;
}
