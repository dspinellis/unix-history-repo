/* Copyright (c) 1979 Regents of the University of California */
#include <stdio.h>
#include "as.h"
#include "assyms.h"

struct	allocbox	*allochead;
struct	allocbox	*alloctail;
struct	symtab		*nextsym;
struct	allocbox	*newbox;
char			*namebuffer;
int			symsleft;

symtabinit()
{
	allochead = 0;
	alloctail = 0;
	nextsym = 0;
	symsleft = 0;
}

/*
 *	Install all known instructions in the symbol table
 */
syminstall()
{
	register	struct	instab	*ip;
	register	struct	symtab	**hp;
	register	char	*p1, *p2;

	for (ip=instab; ip->name[0]!=0; ip++) {
		p1 = ip->name;
		p2 = yytext;
		while (*p2++ = *p1++);
		hp = lookup(0);		/* 0 => don't install this*/
		if (*hp==NULL) {
			*hp = (struct symtab *)ip;
			if (   (ip->tag!=INSTn)
			    && (ip->tag!=INST0)
			    && (ip->tag!=0))
				continue; /* was pseudo-op */
			itab[ip->opcode & 0xFF] = ip;
		}
	}
}	/*end of syminstall*/


/*
 *	Assign final values to symbols,
 *	and overwrite the index field with its relative position in
 *	the symbol table we give to the loader.
 */
extern struct hdr hdr;

freezesymtab()
{
	register	struct	symtab	*sp;
				long	bs;
	register	int	relpos = 0;
#ifdef SORTEDOUTPUT
	register	struct	symtab	**cosp;
#else
	register	struct	symtab		*ubsp;
	register	struct	allocbox	*allocwalk;
#endif

#ifdef SORTEDOUTPUT
	SYMITERATE(cosp, sp)
#else
	DECLITERATE(allocwalk, sp, ubsp)
#endif
	{
		if (sp->tag >= IGNOREBOUND)
			continue; 		/*totally ignore jxxx entries */
		/*
		 *	Ignore stabs, but give them a symbol table index
		 */
		if (sp->type & STABFLAG)
			goto assignindex;
		if ((sp->type&XTYPE)==XUNDEF)
			sp->type = XXTRN+XUNDEF;
		else if ((sp->type&XTYPE)==XDATA)
			sp->value += usedot[sp->index].xvalue;
		else if ((sp->type&XTYPE)==XTEXT)
			sp->value += usedot[sp->index].xvalue;
		else if ((sp->type&XTYPE)==XBSS) {
			bs = sp->value;
			sp->value = hdr.bsize + datbase;
			hdr.bsize += bs;
		}
	   assignindex:
		if (    (sp->name[0] != 'L')
		     || (sp->tag != LABELID)
		     || savelabels
		     )			/*then, we will write it later on*/
				sp->index = relpos++;
	}
}



/*
 *	For all of the stabs that had their final value undefined during pass 1
 *	and during pass 2 assign a final value.
 *	We have already given stab entrys a initial approximation
 *	when we constsructed the sorted symbol table.
 *	Iteration order doesn't matter.
 */
stabfix() {
	register struct symtab *sp, **cosp;
	register struct symtab *p;
	
	SYMITERATE(cosp, sp){
		if(sp->ptype && (sp->type & STABFLAG)) {	
			p = sp->dest;	
			sp->value = p->value;	
			sp->index = p->index;
			sp->type = p->type;
#ifdef DSTAB
			printf("STABFIX: %s (old %s) to %d offsets %d %d\n",
				sp->name, p->name, sp->value, sp, p);
#endif
		}
	}
}

char *Calloc(number, size)
	int	number, size;
{
	register	char *newstuff;
	newstuff = (char *)sbrk(number*size);
	if ((int)newstuff == -1){
		yyerror("Ran out of Memory");
		delexit();
	}
	return(newstuff);
}

struct symtab * symalloc()
{
	if (symsleft == 0){
		register	int	*p;

		newbox = (struct allocbox *)Calloc(1,ALLOCQTY);
		symsleft = SYMDALLOP;
		nextsym = &newbox->symslots[0];
		namebuffer = &newbox->symnames[0];
		p = (int *)(&newbox->symnames[SYMDALLOP * NCPS]);
		while ( p > (int *)newbox){
			*--p = 0;
		}
		if (alloctail == 0){
			allochead = alloctail = newbox;
		} else {
			alloctail->nextalloc = newbox;
			alloctail = newbox;
		}
	}
	--symsleft;
	++nsyms;
	nextsym->name = namebuffer;
	namebuffer += NCPS;
	return(nextsym++);
}

symcmp(pptr, qptr)
	struct symtab **pptr, **qptr;
{
	register struct symtab *p = *pptr;
	register struct symtab *q = *qptr;
	if (p->index < q->index)
		return(-1);
	if (p->index > q->index)
		return(1);
	if (p->value < q->value)
		return(-1);
	if (p->value > q->value)
		return(1);
	/*
	 *	Force jxxx entries to virtually preceed labels defined
	 *	to follow the jxxxx instruction, so that bumping the
	 *	jxxx instruction correctly fixes up the following labels
	 */
	if (p->tag >= IGNOREBOUND)	/*p points to a jxxx*/
		return(-1);		
	if (q->tag >= IGNOREBOUND)
		return(1);
	/*
	 *	both are now just plain labels; the relative order doesn't
	 *	matter.  Both can't be jxxxes, as they would have different
	 *	values.
	 */
	return(0);			
}	/*end of symcmp*/

/*
 *	We construct the auxiliary table of pointers, symptrs and
 *	symdelim
 *	We also assign preliminary values to stab entries that did not yet
 *	have an absolute value (because they initially referred to
 *	forward references). We don't worry about .stabds, as they
 *	already have an estimated final value
 */

sortsymtab()
{
	register	struct	symtab	*sp;
	register	struct	symtab	**cowalk;
	register	struct	allocbox	*allocwalk;
			struct	symtab	*ubsp;
				int	segno;
				int	slotno;
				int	symsin;	/*number put into symptrs*/

	symptrs =  (struct symtab **)Calloc(nsyms + 2, sizeof *symptrs);
	/*
	 *	Allocate one word at the beginning of the symptr array
	 *	so that backwards scans through the symptr array will
	 *	work correctly while scanning through the zeroth segment
	 */
	*symptrs++ = 0;
	cowalk = symptrs;
	symsin = 0;
	DECLITERATE(allocwalk, sp, ubsp) {
		if (sp->ptype && (sp->type &STABFLAG)){
			sp->value = sp->dest->value;
			sp->index = sp->dest->index;
		}
		if (symsin >= nsyms)
			yyerror("INTERNAL ERROR: overfilled symbol table indirection table");
		*cowalk++ = sp;
		symsin++;
	}
	if (symsin != nsyms)
		yyerror("INTERNAL ERROR: installed %d syms, should have installed %d",
			symsin, nsyms);
	symptrub = &symptrs[nsyms ];
	qsort(symptrs, nsyms, sizeof *symptrs, symcmp);
	symdelim[0] = symptrs;
	for (cowalk = symptrs, sp = *cowalk, segno = 0, slotno = 1;
	     segno < NLOC + NLOC;
	     segno++, slotno++){
		for (; sp && sp->index == segno; sp = *++cowalk);
		symdelim[slotno] = cowalk;	/*forms the ub delimeter*/
	}
}	/*end of sortsymtab*/

#ifdef DEBUG
dumpsymtab()
{
	register	int	segno;
	register	struct symtab *sp, **cosp, *ub;
	char		*tagstring();

	printf("Symbol Table dump:\n");
	for (segno = 0; segno < NLOC + NLOC; segno++){
		printf("Segment number: %d\n", segno);
		SEGITERATE(segno, 0, 0, cosp, sp, ub, ++){
			printf("\tSeg: %d \"%8.8s\" value: %d index: %d tag %s\n",
				segno, sp->name, sp->value, sp->index, tagstring(sp->tag));
			printf("\t\ttype: %d jxbump %d jxfear: %d\n",
				sp->type, sp->jxbump, sp->jxfear);
		}
		printf("\n\n");
	}
}

static	char tagbuff[4];

char *tagstring(tag)
	unsigned	char	tag;
{
	switch(tag){
		case JXACTIVE:		return("active");
		case JXNOTYET:		return("notyet");
		case JXALIGN:		return("align");
		case JXQUESTIONABLE:	return("jxquestionable");
		case JXINACTIVE:	return("inactive");
		case JXTUNNEL:		return("tunnel");
		case OBSOLETE:		return("obsolete");
		case IGNOREBOUND:	return("ignorebound");
		case STABFLOATING:	return("stabfloating");
		case STABFIXED:		return("stabfixed");
		case LABELID:		return("labelid");
		case OKTOBUMP:		return("oktobump");
		case ISET:		return("iset");
		case ILSYM:		return("ilsym");
		default:		sprintf(tagbuff,"%d", tag);
					return(tagbuff);
	}
}
#endif

#define 	HASHCLOGGED	(NHASH * 3 ) / 4

struct symtab **lookup(instflg)
	int	instflg;		/* 0: don't install */
{
	register int		ihash;
	register struct	symtab 	**hp;
	register char 		*p1, *p2;
	register	int	i;

#ifdef METRIC
	nhashed++;
#endif

	/*
	 *	All strings passed in in yytext had better have
	 *	a trailing null.  Strings are placed in yytext for
	 *	hashing by syminstall() and yylex()
	 */
	for (ihash = 0, p1 = yytext ; *p1; ihash <<= 2, ihash += *p1++);
	ihash += p1[-1] << 5;
	ihash %= NHASH;
	if (ihash < 0) ihash += NHASH;
	hp = &hshtab[ihash];
	ihash = 1;		/*now, it counts the number of times we rehash*/
	while (*hp) {
		p1 = yytext;
		p2 = (*hp)->name;
		for (i = 0; (i<NCPS) && *p1; i++)
			if (*p1++ != *p2++)
				goto no;
		if (i >= NCPS)		/*both symbols are maximal length*/
			return(hp);
		if (*p2 == 0)	/*assert *p1 == 0*/
			return(hp);
	    no:
#ifdef METRIC
		nhcollisions++;
#endif
		hp += ihash;
		ihash += 2;
		if (hp >= &hshtab[NHASH])
			hp -= NHASH;
	}
	if(++hshused >= HASHCLOGGED) {
		yyerror("Symbol table overflow");
		delexit();
	}
	if (instflg) {
#ifdef METRIC
		nentered++;
#endif
		*hp = symalloc();
		p1 = yytext;
		p2 = (*hp)->name;
		while (*p2++ = *p1++);
	}
	return(hp);
}	/*end of symlook*/

#ifdef vax
#define writel(p,n,f) fwrite((long)p, sizeof (long), n, f)
#else
writel(p,n,f)
	long *p;
	FILE *f;
{
	while (n--) {
		fwrite(&(*p).loword,2,1,f);
		fwrite(&(*p).hiword,2,1,f);
		p++;
	}
}
#endif

int reflen[] = {0,0,1,1,2,2,4,4,8,8};

/*
 *	Save the relocation information
 */
outrel(pval,reftype,reltype,xsym)
	long 		*pval;
	register int 	reftype,reltype;
	struct symtab 	*xsym;
{

/*
 *	reftype: PCREL or not, plus length LEN1, LEN2, LEN4, LEN8
 *	reltype: csect ("segment") number (XTEXT, XDATA, ...) associated with 'val'
 * 	xsym: symbol table pointer
 */
	long ts;
	char tc;
	long tl;
	short t;
	if (passno!=2) {
		dotp->xvalue += reflen[reftype];
		return;
	}
	if (bitoff&07)
		yyerror("Padding error");
	reltype &= ~XFORW;
	if (reltype == XUNDEF)
		yyerror("Undefined reference");
	if (reltype != XABS || reftype & PCREL) {
		/* write the address portion of a relocation datum */
		if (dotp >= &usedot[NLOC]) {
			hdr.drsize += sizeof(dotp->xvalue) + 3 + sizeof tc;
			tl = dotp->xvalue-datbase;
			writel(&tl,1,relfil);
		} else {
			hdr.trsize += sizeof(dotp->xvalue) + 3 + sizeof tc;
			writel(&dotp->xvalue,1,relfil);
		}
		/* write the properties portion of a relocation datum */
		if (reltype == XXTRN+XUNDEF) {
			ts = (xsym->index);
			tc = (XXTRN<<3) | (reftype-LEN1);
		} else if ((reltype&XTYPE) == XUNDEFO) {
			ts = (xsym->index);
			tc = ((XXTRN+2)<<3) | (reftype-LEN1);
		} else  {
			ts = (reltype);
			tc = (reftype-LEN1);
		}
		fwrite((char *)&ts, 3, 1, relfil);
		fwrite(&tc, sizeof(tc), 1, relfil);
	}
	/* write the raw ("unrelocated") value to the text file */
	t = reflen[reftype];
	dotp->xvalue += t;
	if (reftype & PCREL)
		*pval -= dotp->xvalue;
#ifdef vax
	fwrite(pval,1,t,txtfil);
#else
	if (t>2) {
		fwrite(&((*pval).loword),1,2,txtfil);
		fwrite(&((*pval).hiword),1,t-2,txtfil);
	} else fwrite(&((*pval).loword),1,t,txtfil);
#endif
}


/*
 *	Write out n symbols to file f, beginning at p
 *	ignoring symbols that are obsolete, jxxx instructions, and
 *	possibly, labels
 */

int sizesymtab()
{
	struct symtab *sp;

#define NOUTSYMS (nsyms - njxxx - nforgotten - (savelabels ? 0 : nlabels))

	return (
		(  NCPS
		 + sizeof (sp->ptype)
		 + sizeof (sp->other)
		 + sizeof (sp->desc)
		 + sizeof (sp->value)
		) 
		*	NOUTSYMS
	);
}

symwrite(f)
	FILE *f;
{
	int	symsout;			/*those actually written*/
	int	symsdesired = NOUTSYMS;
	register	struct	symtab *sp, *ub;
#ifdef SORTEDOUTPUT
	int	segno;
	register	struct	symtab 		**copointer;
#else
	register	struct	allocbox	*allocwalk;
#endif

#ifdef SORTEDOUTPUT
	for (segno = 0, symsout = 0; segno < NLOC + NLOC; segno++)
		SEGITERATE(segno, 0, 0, copointer, sp, ub, ++)
#else
	symsout = 0;
	DECLITERATE(allocwalk, sp, ub)
#endif
	{
		if (sp->tag >= IGNOREBOUND) 
			continue;
		if ((sp->name[0] == 'L') && (sp->tag == LABELID) && !savelabels)
			continue;
		symsout++;
		fwrite(sp->name, NCPS, 1, f);
		sp->type &= ~XFORW;
		fwrite((sp->ptype) ? (char *)(&(sp->ptype)) : (char *)(&(sp->type)),
			sizeof(char), 1, f);
	/*
	 *	WATCH OUT.  THIS DEPENDS THAT THE ALLOCATION OF
	 *	the four fields ptype, other, desc and value are
	 *	contiguous.  This may have to be changed!
	 *	This is safe (as of 2-Nov-79).
	 */
		fwrite(&(sp->other),
			sizeof (sp->other)
		       + sizeof (sp->desc)
		       + sizeof (sp->value), 1, f
		);
#ifdef fooie
#ifdef vax
		fwrite(&(sp->name[0]), sizeof(symtab[0].name), 1, f);
		fwrite(sp->ptype ? &(sp->ptype) : &(sp->type),
			sizeof(symtab[0].type), 1, f);
		fwrite(&(sp->other), sizeof(symtab[0].other), 1, f);
		fwrite(&(sp->desc), sizeof(symtab[0].desc), 1, f);
		fwrite(&(sp->value), sizeof(symtab[0].value), 1, f);
#else
		writel(&(p->value), 1, f);
#endif
#endif
	}
	if (symsout != symsdesired)
		yyerror("INTERNAL ERROR: Wrote %d symbols, wanted to write %d symbols\n",
			symsout, symsdesired);
}

Flushfield(n)
	register int n;
{
	while (n>0) {
		outb(bitfield);
		bitfield >>= 8;
		n -= 8;
	}
	bitoff=0;
	bitfield=0;
}
