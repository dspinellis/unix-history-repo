/*
 * Copyright (c) 1980 Regents of the University of California.
 * All rights reserved.  The Berkeley software License Agreement
 * specifies the terms and conditions for redistribution.
 */

#ifndef lint
static char sccsid[] = "@(#)optcse.c	5.1 (Berkeley) 6/7/85";
#endif not lint

/*
 * optcse.c
 *
 * Common subexpression elimination routines, F77 compiler pass 1.
 *
 * University of Utah CS Dept modification history:
 *
 * $Log:	optcse.c,v $
 * Revision 2.4  84/10/29  04:40:48  donn
 * Problem with conversions -- two expressions headed by a conversion may be
 * identical in structure but different in type, thus type must be checked in
 * findnode().  This was causing a subscript to become REAL*8 type...
 * 
 * Revision 2.3  84/08/04  20:38:53  donn
 * Added fix from Jerry Berkman for an earlier fix from Alastair Fyfe --
 * samebase() should treat EQUIVALENCEd variables just as daintily as
 * COMMON variables.
 * 
 * Revision 2.2  84/08/01  16:04:33  donn
 * Changed rmcommaop so that it does subscripts too.
 * 
 * Revision 2.1  84/07/19  12:03:44  donn
 * Changed comment headers for UofU.
 * 
 * Revision 1.5  84/07/09  14:43:05  donn
 * Added changes to make OPPLUSEQ and OPSTAREQ expressions ineligible for
 * CSE, since I can't think of a simple way to handle them and they are broken
 * in the previous version, where they were treated like OPASSIGN -- this
 * fails because CSE would think that the value of the lhs and rhs were equal.
 * 
 * Revision 1.4  84/06/08  11:43:35  donn
 * Yet another way of handling the bug with COMMON -- this one is from Alastair
 * Fyfe at Sun.  I backed out the old fix.
 * 
 * Revision 1.3  84/03/07  19:25:14  donn
 * Changed method of handling COMMON bug -- COMMON variables are now treated
 * like array elements and hence are ineligible for CSE.
 * 
 * Revision 1.2  84/02/26  03:30:47  donn
 * Fixed bug in evaluation graph construction that caused two variables in
 * common to be considered identical if they were merely in the same common,
 * rather than in the same common at the same offset.
 * 
 */

#include "defs.h"
#include "optim.h"

#define FALSE	0
#define TRUE	1

LOCAL Bblockp	current_BB;
LOCAL int	cse1count;	/* count of number of cse uses eliminated */
LOCAL int	cse2count;	/* count of number of cse def's eliminated */




LOCAL dumpstacks()
{
	duplptr dl;
	valuen p;
	idlptr idl;
	idptr idp;
	nodelptr nl;
	int i;

	fprintf(diagfile,"\n *** IDblocks ***\n");
	for(idp=current_BB->headid;idp;idp=idp->next) 
	{ 
		fprintf(diagfile,
			"idp= %d idaddr= %d initval= %d assgnval= %d \n",
			idp, idp->idaddr, idp->initval, idp->assgnval);
		fprintf(diagfile,"nodes: ");
		i=0;
		for (nl=idp->headnodelist;nl;nl=nl->next) {
			if(++i>20){
				fprintf(diagfile,"\n");
				i=0;
			}
			fprintf(diagfile," %d ",nl->nodep);
		}
		fprintf(diagfile,"\n");
	}

	fprintf(diagfile,"\n *** VALUE NODES *** \n");
	for(p=current_BB->headnode;p;p=p->next) {
		fprintf(diagfile,
		   "\np= %d opp= %d lc= %d rc= %d rs= %d is_dead= %d n_dups %d",
		   p, p->opp,p->lc,p->rc, p->rs, p->is_dead, p->n_dups);
		if (p->rs){ 
			fprintf(diagfile,"tag= %d ",p->opp->tag);
			if(p->opp->tag==TEXPR)
				fprintf(diagfile,"opco= %d ",
				    p->opp->exprblock.opcode);
		}
		fprintf(diagfile,"\n");
		fprintf(diagfile,"parent= %d dups:  ",p->parent);
		i=0;
		for(dl=p->headduplist;dl;dl=dl->next) {
			if(++i>20){
				fprintf(diagfile,"\n");
				i=0;
			}
			fprintf(diagfile," %d ",dl->parent);
		}

		fprintf(diagfile,"\ndeps IDs");
		i=0;
		for(idl=p->headdeplist;idl;idl=idl->next) {
			if(++i>20){
				fprintf(diagfile,"\n");
				i=0;
			}
			fprintf(diagfile," %d ",idl->idp);
		}
	}
}



LOCAL idlptr mergedeps(lnode,rnode)
valuen lnode,rnode;
/* Given two value nodes, merge the lists of identifiers on which they
** depend to produce a new list incorporating both dependencies. Lists
** are assumed to be ordered by increasing idp address. No duplicate identifiers
** are generated in the output list.
*/
{
	register idlptr lp,lp1,lp2;
	idlptr head;

	lp = lp1 = lp2 = head = NULL;
	if(lnode) lp1 = lnode->headdeplist;
	if(rnode) lp2 = rnode->headdeplist;

	while (lp1 || lp2) {
		if (lp) { 
			lp->next = ALLOC(IDlist); 
			lp = lp->next; 
		}
		else lp = head = ALLOC(IDlist);
		lp->next = 0;
		if (lp1 == 0) {
			lp->idp = lp2->idp;
			lp2 = lp2->next;
		}
		else if (lp2 == 0) {
			lp->idp = lp1->idp;
			lp1 = lp1->next; 
		}
		else if (lp1->idp < lp2->idp) {
			lp->idp = lp1->idp;
			lp1 = lp1->next; 
		}
		else if (lp1->idp > lp2->idp) {
			lp->idp = lp2->idp;
			lp2 = lp2->next; 
		}
		else {
			lp->idp = lp1->idp;
			lp1 = lp1->next;
			lp2 = lp2->next;
		}
	}
	return(head);
}



LOCAL removenode(nodep)
valuen nodep;
/*  Removes a value node from every IDblock on the node's list of identifiers.
*/
{
	register idlptr idl;
	register nodelptr nl;
	register nodelptr *addrnl;

	if(nodep == NULL) return ;

	/* loop through all identifiers */
	for(idl=nodep->headdeplist;idl;idl=idl->next)
	{
		addrnl = &(idl->idp->headnodelist);
		/* for each identifier loop through all nodes until match is found */
		for(nl = *addrnl; nl; nl = *addrnl)
		{
			if(nl->nodep == nodep) {
				*addrnl = nl->next;
				free ( (charptr) nl );
				break;
			}
			addrnl = &nl->next;
		}
	}
	nodep->is_dead = TRUE;
}



LOCAL killid(idp)
idptr idp;
/* Kill all nodes on one identifier's list of dependent nodes, i.e. remove
** all calculations that depend on this identifier from the available 
** values stack.  Free the list of records pointing at the dependent nodes.
*/
{
	nodelptr nl1,nl2;

	for (nl1 = idp->headnodelist; nl1; nl1=nl2) 
	{
		nl2 = nl1->next;
		removenode(nl1->nodep);
	}
	/* the above call frees the node list record pointed at by nl1 since it frees
	** all the node list records that reference the value node being killed
	*/
	idp->headnodelist = NULL;

}



LOCAL killdepnodes(idp)
idptr idp;
/* Kill all value nodes that represent calculations which depend on
** this identifier. If the identifier is in COMMON or EQUIVALENCE storage,
** kill all values that depend on identifiers in COMMON or EQUIVALENCE
*/
{
	int thismemno;

	if(idp->idaddr->addrblock.vstg == STGCOMMON) 
	{
		for(idp=current_BB->headid;idp;idp=idp->next)
			if(idp->idaddr->addrblock.vstg == STGCOMMON)
				killid(idp);
	}
	else if(idp->idaddr->addrblock.vstg == STGEQUIV) 
	{           
		thismemno=idp->idaddr->addrblock.memno;
		for(idp=current_BB->headid;idp;idp=idp->next)
			if(idp->idaddr->addrblock.vstg == STGEQUIV
			    && idp->idaddr->addrblock.memno == thismemno)
				killid(idp);
	}
	else killid(idp);

}



LOCAL appendnode(nodep)
valuen nodep;
/* Append a value node to all the IDblocks on that node's list of
** dependent identifiers i.e., since this computation depends on
** all the identifiers on its list then each of those identifiers should
** include this node in their list of dependent nodes.
*/
{
	register idlptr idl;
	register nodelptr nl;

	for(idl=nodep->headdeplist;idl;idl=idl->next)
		if(idl->idp->idaddr->tag == TADDR ||
		   idl->idp->idaddr->tag == TTEMP)
			{
			nl=ALLOC(NODElist);
			nl->nodep = nodep;
			nl->next = idl->idp->headnodelist;
			idl->idp->headnodelist = nl;
			}
}



LOCAL idlptr addadep(idp,nodep) 
idptr idp;
valuen nodep;
/* Add an identifier to the dependents list of a value node.  Dependents
** lists are ordered by increasing idp value
*/
{
	register idlptr lp1,lp2; 

	lp2 = ALLOC(IDlist);
	lp2->idp = idp;
	if(nodep->headdeplist == 0) {
		lp2->next = 0;
		nodep->headdeplist = lp2;
	}
	else if(idp <= nodep->headdeplist->idp) {
		lp2->next = nodep->headdeplist;
		nodep->headdeplist = lp2;
	}
	else for(lp1 = nodep->headdeplist; lp1; lp1 = lp1->next)
		if( (lp1->next == 0) || (idp <= lp1->next->idp) )
		{
			lp2->next = lp1->next;
			lp1->next = lp2;
			break;
		}
	return(lp2);
}



LOCAL valuen newnode(expr,left,right,rslt) 
expptr expr;
valuen left,right,rslt;
/* Build a new value node            
*/
{
	register valuen p;

	p= ALLOC(VALUEnode);
	p->opp = expr ; 
	p->parent = NULL ;
	p->lc = left;
	p->rc = right;
	p->rs = rslt;
	p->n_dups = 0;
	p->is_dead = FALSE;
	p->next=NULL;
	p->headdeplist = mergedeps(left,right);
	p->headduplist=NULL;
	if(current_BB->headnode == 0) current_BB->headnode=p;
	else if(current_BB->tailnode) current_BB->tailnode->next=p;
	current_BB->tailnode=p;

	return(p);
}



LOCAL newid(idaddr,addrof_idptr)
expptr idaddr; 
idptr *addrof_idptr;
/* Build a new IDblock and hook it on the current BB's ID list
*/
{
	register idptr p;

	p= ALLOC(IDblock);

/* build a leaf value node for the identifier and put the ID on the leaf node's
** list of dependent identifiers
*/
	p->initval =  newnode(idaddr,NULL,NULL,NULL);
	p->initval->rs = p->initval;
	addadep(p,p->initval); 

	p->idaddr = idaddr;
	*addrof_idptr = p;
	p->headnodelist=NULL;
	p->next=NULL;

}



LOCAL addadup(parent,nodep)
expptr *parent;
valuen nodep;

/* A subtree has been found that duplicates the calculation represented
** by the value node referenced by nodep : add the root of the reduntant
** tree to the value node's list of duplicates.
*/

{
	register duplptr dp;
	valuen child;

	dp = ALLOC(DUPlist);
	dp->parent = parent;
	dp->next = nodep->headduplist;
	nodep->headduplist = dp;
	++nodep->n_dups;

/* Check whether either of nodep's children is also a duplicate calculation
** and if so peel off it's most recent dup record
*/

	if ( (child = nodep->lc) && (child->n_dups) )
	{
		dp = child->headduplist;
		child->headduplist = dp->next;
		free ( (charptr) dp );
		--child->n_dups;
	}
	if ( (child = nodep->rc) && (child->n_dups) )
	{
		dp = child->headduplist;
		child->headduplist = dp->next;
		free ( (charptr) dp );
		--child->n_dups;
	}

}



LOCAL samebase(ep1,ep2)
expptr ep1,ep2;
{
    if ( ep1->tag == ep2->tag  )       
	switch (ep2->tag) {
	    case TTEMP :
		if (ep1->tempblock.memalloc == ep2->tempblock.memalloc)
			return (TRUE);
		break;
	    case TADDR :  
		if (ep1->addrblock.vstg == ep2->addrblock.vstg) {
		    switch(ep1->addrblock.vstg) {
			case STGEQUIV:
			case STGCOMMON:
			    if (ep1->addrblock.memno == ep2->addrblock.memno &&
				ISCONST(ep1->addrblock.memoffset) &&
				ISCONST(ep2->addrblock.memoffset) &&
				ep1->addrblock.memoffset->constblock.const.ci ==
				ep2->addrblock.memoffset->constblock.const.ci ) {
				    return(TRUE); 
			    }
			    break;

			default:
			    if (ep1->addrblock.memno == ep2->addrblock.memno ) {
				return(TRUE); 
			    }
		    }
		}
		break;
	    case TCONST :
		if( (ep1->constblock.vtype) ==
		    (ep2->constblock.vtype)  ) 
		{
			union Constant *ap,*bp;
			ap= &ep1->constblock.const;
			bp= &ep2->constblock.const;
			switch(ep1->constblock.vtype)

			{
			case TYSHORT:
			case TYLONG:
				if(ap->ci == bp->ci) return(TRUE);
				break;
			case TYREAL:
			case TYDREAL:
				if(ap->cd[0] == bp->cd[0]) return(TRUE);
				break;
			case TYCOMPLEX:
			case TYDCOMPLEX:
				if(ap->cd[0] == bp->cd[0] &&
				    ap->cd[1] == bp->cd[1] )
					return(TRUE);
				break;
			}
		}
		break;

	    default : 
		badtag ("samebase",ep2->tag);
	}
    return(FALSE);
}



LOCAL idptr findid(idaddr)
expptr idaddr;

/* Find an identifier's IDblock given its idaddr. If the identifier has no
** IBblock build one
*/

{
	register idptr idp;
	if(current_BB->headid == 0) newid(idaddr,&current_BB->headid);
	idp=current_BB->headid;

	do {
		if (samebase(idp->idaddr,idaddr) )  break;
		if (idp->next == 0) {
			newid(idaddr,&idp->next);
			idp = idp->next;
			break;
		}
		idp = idp->next;
	}
	while(TRUE);

	return(idp);
}



LOCAL valuen findnode(ep,leftc,rightc)
expptr ep;
valuen leftc,rightc;
{
	/* Look for a matching value node in the available computations stack
	*/  
	register valuen p;

	for ( p=current_BB->headnode; p ; p=p->next)  {
		if( ( ! p->is_dead)   &&
		    (p->lc == leftc)  &&
		    (p->rc == rightc) &&
		    ( (ep->tag == TEXPR && p->opp->tag == TEXPR 
		      && p->opp->exprblock.opcode == ep->exprblock.opcode
		      && p->opp->exprblock.vtype == ep->exprblock.vtype
		      )
		    || (ep->tag == TADDR) || (ep->tag == TTEMP)
		    )
		  ) 
			return(p);
	}
	return(NULL);
}



LOCAL valuen scanchain(listp,p_parent)
expptr listp;
chainp *p_parent;

/* Make value nodes from the chain hanging off a LISTBLOCK
*/

{
	valuen lnode,rnode,new,scantree();
	chainp p;

	p= *p_parent;
	if (p == NULL) return(NULL);
	lnode = scantree( &p->datap);
	rnode = scanchain(listp, &p->nextp);
	new = newnode(listp,lnode,rnode,0);    
	new->rs = new;
	return(new->rs);
}



LOCAL valuen scantree(p_parent)
expptr *p_parent;

/* build a value node and return its address. p must point to an
** exprblock an addrblock a listblock  or a constblock.
*/

{
valuen lnode, rnode,rsltnode,new;
expptr opp,p;
Exprp ep1,ep2;
idptr idp;

p = *p_parent;
if(p == NULL) return(NULL);

switch (p->tag) {
	case TCONST :
		return( findid(p)->initval );
	
	case TTEMP :
		idp = findid(p);
		if(idp->assgnval) return(idp->assgnval);
	
		lnode = idp->initval;
		rnode = scantree( &p->tempblock.memalloc);
	
		rsltnode = findnode(p,lnode,rnode);
		if(rsltnode)
			return(rsltnode);
		else { 
			new = newnode(p,lnode,rnode,0);
			new->rs = new;
			new->parent = p_parent;
			return(new->rs);
		}

	case TADDR :    
		idp = findid(p);
		if(idp->assgnval) return(idp->assgnval);
	
		lnode = idp->initval;
		rnode = scantree( &p->addrblock.memoffset);
	
		rsltnode = findnode(p,lnode,rnode);
		if(rsltnode) {
#ifdef	notdef
			/*
			 * This code is broken until OPINDIRECT is implemented.
			 */
			if(p->addrblock.memoffset != NULL &&
			    p->addrblock.memoffset->tag == TEXPR)
				addadup(p_parent,rsltnode);
#endif	notdef
			return(rsltnode);
		} 
		else { 
			new = newnode(p,lnode,rnode,0);
			new->rs = new;
			new->parent = p_parent;
			return(new->rs);
		}
	
	case TLIST :
		return(scanchain(p->listblock.listp,&p->listblock.listp));
	
	default :    
		badtag ("scantree",p->tag);
	
	case TEXPR  :    
		lnode = scantree(&p->exprblock.leftp);
		rnode = scantree(&p->exprblock.rightp);
	
		switch (p->exprblock.opcode) {
			case OPASSIGN :
				{
				Addrp ap;

				ap = (Addrp) p->exprblock.leftp;
				idp = findid(ap);
				killdepnodes(idp);
				if( ! ap->isarray ) {
					if(rnode->is_dead)idp->assgnval=idp->initval;
					else idp->assgnval = rnode;
				}
				new = newnode(p,idp->initval,NULL,NULL);
				appendnode(new);
				new->rs = new;
				return(new->rs);
				}

			/*
			 * Don't optimize these...  they're a real hassle.
			 */
			case OPPLUSEQ :
			case OPSTAREQ :
				{
				Addrp ap;

				ap = (Addrp) p->exprblock.leftp;
				idp = findid(ap);
				killdepnodes(idp);
				idp->assgnval = NULL;
				new = newnode(p,lnode,rnode,NULL);
				new->rs = new;
				return(new->rs);
				}

			case OPCALL :                    
				{
				chainp cp;

				if(p->exprblock.rightp)

	/* pretend that all variables on the arglist have just
	** been assigned to i.e. kill of calculations that 
	** depend on them. Not necessary for CCALL(by value)
	*/

				for(cp=p->exprblock.rightp->listblock.listp;
                                cp;cp=cp->nextp) 
					if (cp->datap->tag == TADDR ||
					    cp->datap->tag == TTEMP){
						idp = findid(cp->datap);
						killdepnodes(idp);
						idp->assgnval = NULL;
				}

				new = newnode(p,lnode,rnode,NULL);
				new->rs = new;
				return(new->rs);
				}

			case OPCONCAT:
			case OPADDR:
			case OPCOLON:
			case OPINDIRECT:
		/*
		 * For now, do not optimize LSHIFT until OPINDIRECT
		 * implemented.
		 */
			case OPLSHIFT:
				new = newnode(p,lnode,rnode,NULL);
				new->rs = new;
				return(new->rs);

			case OPCOMMA:
				badop ("scantree",OPCOMMA);
				break;

			default : 
				rsltnode = findnode(p,lnode,rnode);
				if (rsltnode) {
					addadup(p_parent,rsltnode);
					return(rsltnode);
				}
				else {
					new = newnode(p,lnode,rnode,NULL);
					new->rs = new;             
					new->parent = p_parent;
					appendnode(new);
					return(new->rs);
				}
			}
	}
}



LOCAL prunetrees()

/* The only optcse.c routine that does any real work: go through the available
** computations stack and eliminate redundant subtrees.
*/

{
Addrp tempv;
register duplptr dl;
register valuen p;
expptr t;
int is_addrnode;
expptr *addr_tree1 = NULL ;
expptr tree2 = NULL ;

for(p=current_BB->headnode;p;p=p->next) 
{
	if(p->rs == NULL) { 
		if( addr_tree1 && tree2 ) 
		     *addr_tree1 = fixtype(mkexpr(OPCOMMA,tree2,*addr_tree1));
		addr_tree1 = (expptr*) p->opp;
		tree2 = NULL;
	}
	if (p->n_dups ) {

		if (p->opp->tag == TTEMP)
			fprintf(diagfile,"TTEMP in prunetrees - cbb\n");
		if(p->opp->tag == TADDR) is_addrnode = TRUE;
		else is_addrnode = FALSE;

		if (is_addrnode)
			tempv = mktemp(TYADDR,NULL);
		else
			tempv = mktemp(p->opp->exprblock.vtype,
			    p->opp->exprblock.vleng);
		cse2count++;

		if(tree2)
			tree2 = fixtype(mkexpr(OPCOMMA,tree2,
				fixtype(mkexpr(OPASSIGN,cpexpr(tempv),
				(is_addrnode ? addrof(p->opp) :  p->opp)
				))));
		else
			tree2 = fixtype(mkexpr(OPASSIGN,cpexpr(tempv),
				(is_addrnode ? addrof(p->opp) :  p->opp)
				));

		if(is_addrnode)
			*(p->parent) = fixtype(mkexpr(OPINDIRECT,cpexpr(tempv), NULL));
		else
			*(p->parent) = (expptr) cpexpr(tempv);

/* then replaces all future instances of the calculation by references to
   the temporary */

		for(dl=p->headduplist;dl->next;dl=dl->next) {
			cse1count++;
			frexpr(*dl->parent);
			if(is_addrnode)
				*(dl->parent) = fixtype(
					mkexpr(OPINDIRECT,cpexpr(tempv), NULL));
			else
				*(dl->parent) = (expptr) cpexpr(tempv);
		}

/* the last reference does not use a copy since the temporary can
   now be freed */

		cse1count++;
		frexpr(*dl->parent);
		if(is_addrnode)
			*(dl->parent) = fixtype(mkexpr(OPINDIRECT,tempv, NULL));
		else
			*(dl->parent) = (expptr) tempv;

		frtemp (tempv);
	}                  
}
if(addr_tree1 && tree2)
	*addr_tree1 = fixtype(mkexpr(OPCOMMA,tree2,*addr_tree1));
}



LOCAL rewritebb (bb)
Bblockp bb;
{
	Slotp sp; 
	expptr p;

	if (bb == NULL)
		return;
	else
		current_BB = bb;
	sp = current_BB->first;

	/* loop trough all BB slots and scan candidate expr trees when found */

	for (sp = current_BB->first; ; sp = sp->next)
		{
		switch (sp->type)
		    {
		    case SKEQ : 
		    case SKIFN : 
		    case SKCMGOTO :
		    case SKCALL :
			newnode((expptr) &sp->expr,NULL,NULL,NULL);
			scantree(&sp->expr); 
			break;

		    default  : 
			break;
		    }
		if (sp == current_BB->last) break;
		}

/* use the information built up by scantree to prune reduntant subtrees */
	prunetrees();

	current_BB = NULL;
}



/*
 *  removes all instances of OPCOMMA from the given subexpression of
 *  the given buffer slot
 */

expptr rmcommaop (p,sl)
expptr	p;
Slotp	sl;

{
expptr	leftp,rightp;
chainp	cp;

if (!p)
	return (ENULL);
switch (p->tag)
	{
	case TEXPR:
		leftp = p->exprblock.leftp;
		rightp = p->exprblock.rightp;
		leftp = rmcommaop (leftp,sl);
		if (p->exprblock.opcode == OPCOMMA)
			{
			optinsert (SKEQ,leftp,0,0,sl);
			if (p->exprblock.vleng)
				free ((charptr) p->exprblock.vleng);
			free ((charptr) p);
			p = rmcommaop (rightp,sl);
			return (p);
			}
		p->exprblock.leftp = leftp;
		p->exprblock.rightp = rmcommaop (rightp,sl);
		return (p);

	case TLIST:
		for (cp = p->listblock.listp; cp; cp = cp->nextp)
			cp->datap = (tagptr) rmcommaop (cp->datap,sl);
		return (p);

	case TADDR:
		p->addrblock.memoffset = rmcommaop (p->addrblock.memoffset,sl);
		return (p);

	default:
		return (p);
	}
}



/*
 *  scans the code buffer, performing common subexpression elimination
 */

optcse ()

{
Slotp	sl;
Bblockp	bb;

if (debugflag[13])
	return;

cse1count = 0;
cse2count = 0;
for (sl = firstslot; sl; sl = sl->next)
	sl->expr = rmcommaop (sl->expr,sl);
for (bb = firstblock; bb; bb = bb->next)
	rewritebb (bb);

if (debugflag[0])
	fprintf (diagfile,
		"%d common subexpression use%s eliminated (%d definition%s)\n",
		cse1count, (cse1count==1 ? "" : "s"),
		cse2count, (cse2count==1 ? "" : "s"));
}
