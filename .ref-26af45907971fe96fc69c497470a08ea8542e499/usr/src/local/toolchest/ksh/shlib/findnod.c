/*

 *      Copyright (c) 1984, 1985, 1986 AT&T
 *      All Rights Reserved

 *      THIS IS UNPUBLISHED PROPRIETARY SOURCE 
 *      CODE OF AT&T.
 *      The copyright notice above does not 
 *      evidence any actual or intended
 *      publication of such source code.

 */
/* @(#)findnod.c	1.1 */

/*
 *   FINDNOD.C
 *
 *   Programmer:  D. G. Korn
 *
 *        Owner:  D. A. Lambeth
 *
 *         Date:  April 17, 1980
 *
 *
 *
 *   FINDNOD (NAME, ROOT, TYPE)
 *        
 *        Return a pointer to the Namnod in the tree given by
 *        ROOT whose namid is NAME.  If TYPE has non-zero last bit, create 
 *        a new node with namid NAME, if one does not exist, and 
 *        insert it into ROOT. 
 * 
 *   _NAMP (NAME, ROOT, TYPE)
 *
 *        Return a pointer to the Namnod in the linked list
 *        of Namnods given by ROOT whose namid is NAME.  If 
 *        TYPE is non-zero, create a new node with namid
 *        NAME, if one does not exist, and link it into the list.
 *
 *   MAK_NOD (NAME)
 *
 *        Allocate a Namnod, setting its namid to NAME and its
 *        value to VALUE to NULL.
 *
 *   COPY_NOD(NODE, TYPE)
 *
 *	  Return a pointer to a Namnod in the last Shell tree
 *	  whose name is the same as that in NODE.
 *	  If TYPE is non-zero the attributes of NODE
 *	  are also copied.
 *
 *
 *   See Also:  lookup(III), linknod(III), chkid(III)
 */

#include	"name.h"
#include        "flags.h"

#define round(a,b)	(a+b-1)&~(b-1)

struct Namnod *mak_nod();
char	*bracket_match();
struct Namnod *findnod();

extern char	*malloc(), *valup();
extern char	*strcpy();
extern unsigned	chkid();
extern void	failed();
extern void	assign();
extern void	free();
extern struct Namaray *growaray();
extern long	aeval();
#ifdef NAME_SCOPE
extern	char	*index();
struct Namnod *copy_nod();
#endif
static struct Namnod *_namp();

static int save_i;

/*
 *   FINDNOD (NAME, ROOT, TYPE)
 *
 *        char *NAME;
 *
 *        struct Amemory *ROOT;
 *   
 *        int TYPE;
 *
 *   Return a pointer to the Namnod in tree ROOT whose namid is 
 *   NAME.  If TYPE is non-zero, a Namnod of the given id will
 *   be created, if necessary.  If the RE_USE bit is set then
 *   the hash code will not be recomputed.  This is set in
 *   lookup(III) when looking up the name in several trees.
 *
 *   If TYPE is zero, and NAME is not found, NULL is returned.
 *
 *   NAME should be of a form acceptable to chkid(III).
 *
 *   Algorithm:  Memory (ROOT) is an array of linked lists of
 *               Namnods.  Hashing on NAME selects one list; a
 *               scan of this locates the node of interest.
 */

struct Namnod *findnod(name,root,type)
char *name;
struct Amemory *root;
{
	register char *cp = name;
	register int c;
	struct Namnod *np = NULL;
	{
		register int i;
		while((c = *((unsigned char*)cp++)) && c!= '[');
		if(c)
			*--cp = 0;
		if((type&RE_USE)==0)
			save_i = chkid(name);
		if((i=save_i)==0)
		{
			if(type&CHK_FOR)
				goto skip;
	        	failed (name, notid);
		}
		i &= root->memsize-1;
		np = _namp(name,&root->memhead[i],type&ADD_NOD);
	skip:
		if(c)
			*cp = c;
		if(np == NULL)
			return(np);
		if ((c == 0) && !(attest (np, ARRAY)))
			return(np);
		if(c == 0)
		{
	        	setdot (np, 0);
			return(np);
		}
	}
	{
		register struct Namaray *ap;
		register int dot;
		char *sp;
		struct Namnod *nq;
		if (attest (np, ARRAY))
			ap = arayp (np);
		else
		{
#ifdef NAME_SCOPE
			if(attest(np, C_WRITE))
			{
				np = copy_nod(np,2);
			}
#endif
			ap = growaray((struct Namaray *)NULL,0);
		}
		cp = bracket_match(sp=cp);
		c = *cp;
		*cp = 0;
		dot = (int)aeval((char*)sp+1);
		*cp = c;
		if ((dot >= ARRMAX) || (dot < 0))
			failed(name,subscript);
		else
	        	ap->adot = dot;
		if (!attest (np, ARRAY))
			if (dot == 0)
			{
				free((char *)ap);
				return(np);
			}
			else if ((cp = valup (np)) != NULL)
	        	{
		        	nq = mak_nod (np->namid);
				nq->value.namflg = np->value.namflg;
		        	assign (nq, cp);
		        	ap->val[0] = &nq->value;
		        }
		if (dot > ap->maxi)
			ap = growaray (ap, dot);
		np->value.namval.aray = ap;
		np->value.namflg |= ARRAY;
		setdot (np, dot);
		return(np);
	}
}


/*
 * skip to a matching ']' and return pointer to matched character
 * routine assumes that you are sitting on the '['
 */

char *bracket_match(string)
register char *string;
{
	register int count = 1;
	register int c;
	while(count>0 && (c= *++string))
	{
		if(c=='[')
			count++;
		else if(c==']')
			count--;
	}
	return(string);
}

/*
 *   _NAMP (NAME, ROOT, TYPE)
 *
 *        char *NAME;
 *
 *        struct Amemory *ROOT;
 *
 *        int TYPE;
 *
 *   Return a pointer to the Namnod in a linked list of
 *   Namnods (given by ROOT) whose namid is NAME.  If TYPE
 *   is non-zero, a new Namnod with the given NAME will
 *   be inserted, if none is found.
 *
 *   NAME should be of a form acceptable to chkid(III).
 */

static struct Namnod *_namp(name,root,type)
char *name;
struct Namnod **root;
{
	register char *cp,*sp;
	register struct Namnod *np;
	register struct Namnod *nq = NULL;
	struct Namnod *rp = *root;

	for(np=rp;np;nq=np,np=np->namnxt)
	{
		if((np->value.namflg&N_AVAIL)==0)
		{
			/* match even if np->name has an = in it */
			cp = np->namid;
			sp = name;
			do
			{
				if(*sp==0)
				{
					if(*cp && *cp != '=')
						break;
					if(nq==NULL)
						return(np);
					nq->namnxt = np->namnxt;
					np->namnxt = rp;
					return(*root=np);
				}
			}
			while(*sp++ == *cp++);
		}
	}
	if(type==0)
		return((struct Namnod*)NULL);
	np = mak_nod(name);
	np->namnxt = rp;
	return(*root=np);
}

/*
 *   MAKNOD (NAME)
 *
 *        char *NAME;
 *
 *   Allocate a Namnod, setting its namid to NAME and its value
 *   to VALUE to NULL.  A pointer to the allocated node is returned.
 *   NULL is returned if there is no space to be allocated
 *   for the Namnod.
 *
 *   NAME should be of a form acceptable to chkid(III).
 */

struct Namnod *mak_nod(name)
char *name;
{
	register struct Namnod *np;
	if((np=(struct Namnod *)malloc((unsigned)sizeof(struct Namnod)+strlen(name)+1)) == (struct Namnod*)NULL)
		return(np);
	np->namid = (char *)(np+1);
	strcpy (np->namid, name);
	np->value.namflg = N_DEFAULT;
	np->value.namval.cp = NULL;
	np->namnxt = NULL;
	np->namsz = 0;
	return(np);
}

#ifdef NAME_SCOPE
struct Namnod *copy_nod(node, type)
struct Namnod *node;
int type;
{
	register struct Namnod *oldnp = node;
	register struct Namnod *newnp;
	register struct Amemory *rootp=namep;
	char *cp;
	while(rootp->nexttree)
		rootp = rootp->nexttree;	/* skip to last tree */
	if(cp = index(node->namid,'='))
		*cp = 0;
	newnp = findnod(oldnp->namid,rootp,1);
	if(cp)
		*cp = '=';
	if(type==0)
		return(newnp);
	oldnp->value.namflg &= ~C_WRITE;
	newnp->value.namflg = oldnp->value.namflg&~(IN_DIR|N_FREE|N_ALLOC);
	newnp->namid = oldnp->namid;
	oldnp->value.namflg |= N_AVAIL;
	if(attest(oldnp, ARRAY))
	{
		register struct Namaray *ap1,*ap2;
		int dot;
		char *val;
		ap1 = arayp(oldnp);
		dot = ap1->adot;
		ap2 = growaray((struct Namaray*)0,ap1->maxi);
		newnp->value.namval.aray = ap2;
		for(ap1->adot=0;ap1->adot <= ap1->maxi;ap1->adot++)
			if(val=valup(oldnp))
			{
				ap2->adot = ap1->adot;
				assign(newnp,val);
			}
		ap2->adot = dot;
	}
	else if(type==2 )
		assign(newnp,valup(oldnp));
	return(newnp);
}
#endif	/* NAME_SCOPE */
