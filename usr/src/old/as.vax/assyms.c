/* Copyright (c) 1980 Regents of the University of California */
static	char sccsid[] = "@(#)assyms.c 4.1 %G%";
#include <stdio.h>
#include <ctype.h>
#include <sys/types.h>
#include <a.out.h>
#include "as.h"
#include "asscan.h"
#include "assyms.h"

/*
 *	Managers for chunks of symbols allocated from calloc()
 *	We maintain a linked list of such chunks.
 *
 */
struct	allocbox	*allochead;	/*head of chunk list*/
struct	allocbox	*alloctail;	/*tail*/
struct	allocbox	*newbox;	/*for creating a new chunk*/
struct	symtab		*nextsym;	/*next symbol free*/
int			symsleft;	/*slots left in current chunk*/

struct	symtab		**symptrs;
struct	symtab		**symdelim[NLOC + NLOC +1];
struct	symtab		**symptrub;
/*
 *	Managers for the dynamically extendable hash table
 */
struct	hashdallop	*htab;

struct	instab		*itab[NINST];	/*maps opcodes to instructions*/
/*
 *	Counts what went into the symbol table, so that the
 *	size of the symbol table can be computed.
 */
int	nsyms;		/* total number in the symbol table */
int	njxxx;		/* number of jxxx entrys */
int	nforgotten;	/* number of symbols erroneously entered */
int	nlabels;	/* number of label entries */
int	hshused;	/* number of hash slots used */

/*
 *	Managers of the symbol literal storage.
 *	If we have flexible names, then we allocate BUFSIZ long
 *	string, and pack strings into that.  Otherwise, we allocate
 *	symbol storage in fixed hunks NCPS long when we allocate space
 *	for other symbol attributes.
 */
#ifdef	FLEXNAMES
struct	strpool		*strplhead = 0;
#else
char			*namebuffer;
#endif

symtabinit()
{
	allochead = 0;
	alloctail = 0;
	nextsym = 0;
	symsleft = 0;
#ifdef FLEXNAMES
	strpoolalloc();		/* get the first strpool storage area */
#endif FLEXNAMES
	htab = 0;
	htaballoc();		/* get the first part of the hash table */
}

/*
 *	Install all known instructions in the symbol table
 */
syminstall()
{
	register	struct	instab	*ip;
	register	struct	symtab	**hp;
	register	char	*p1, *p2;

	for (ip=instab; ip->name!=0; ip++) {
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
extern struct exec hdr;

freezesymtab()
{
	register	struct	symtab	*sp;
				long	bs;
	register	int	relpos = 0;
	register	struct	symtab		*ubsp;
	register	struct	allocbox	*allocwalk;

	DECLITERATE(allocwalk, sp, ubsp)
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
			sp->value = hdr.a_bss + datbase;
			hdr.a_bss += bs;
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

char *ClearCalloc(number, size)
	int	number, size;
{
	register	char	*newstuff;		/* r11 */
	register	int	length = number * size;	/* r10 */
	newstuff = Calloc(number, size);
	asm("movc5 $0, (r0), $0, r10, (r11)");
	return(newstuff);
}

struct symtab *symalloc()
{
	if (symsleft == 0){
		newbox = (struct allocbox *)ClearCalloc(1,ALLOCQTY);
		symsleft = SYMDALLOP;
		nextsym = &newbox->symslots[0];
#ifndef FLEXNAMES
		namebuffer = &newbox->symnames[0];
#endif not FLEXNAMES
		if (alloctail == 0){
			allochead = alloctail = newbox;
		} else {
			alloctail->nextalloc = newbox;
			alloctail = newbox;
		}
	}
	--symsleft;
	++nsyms;
#ifndef FLEXNAMES
	nextsym->name = namebuffer;
	namebuffer += NCPS;
#endif not FLEXNAMES
	return(nextsym++);
}

#ifdef FLEXNAMES
strpoolalloc()
{
	register	struct	strpool	*new;

	new = (struct strpool *)Calloc(1, sizeof (struct strpool));
	new->str_nalloc = 0;
	new->str_next = strplhead;
	strplhead = new;
}
#endif FLEXNAMES

symcmp(Pptr, Qptr)
	struct symtab **Pptr, **Qptr;
{
	register struct symtab *p = *Pptr;
	register struct symtab *q = *Qptr;
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
#ifdef FLEXNAMES
			printf("\tSeg: %d \"%s\" value: %d index: %d tag %s\n",
				segno, sp->name,
				sp->value, sp->index,
				tagstring(sp->tag));
#else not FLEXNAMES
			printf("\tSeg: %d \"%*.*s\" value: %d index: %d tag %s\n",
				segno, NCPS, NCPS, sp->name,
				sp->value, sp->index,
				tagstring(sp->tag));
#endif not FLEXNAMES
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
#endif DEBUG

htaballoc()
{
	register	struct	hashdallop	*new;
	new = (struct hashdallop *)ClearCalloc(1, sizeof (struct hashdallop));
	if (htab == 0)
		htab = new;
	else {		/* add AFTER the 1st slot */
		new->h_next = htab->h_next;
		htab->h_next = new;
	}
}

#define 	HASHCLOGGED	(NHASH / 2)

/*
 *	Lookup a symbol stored in extern yytext.
 *	All strings passed in via extern yytext had better have
 *	a trailing null.  Strings are placed in yytext for hashing by
 *	syminstall() and by yylex();
 *
 *	We take pains to avoid function calls; this functdion
 *	is called quite frequently, and the calls overhead
 *	in the vax contributes significantly to the overall
 *	execution speed of as.
 */
struct symtab **lookup(instflg)
	int	instflg;		/* 0: don't install */
{
	static	 int		initialprobe;
	register struct	symtab 	**hp;
	register char 		*from;
	register char		*to;
	register	int	len;
	register	int	nprobes;
	static	 struct hashdallop *hdallop;
	static	 struct symtab	**emptyslot;
	static 	 struct hashdallop *emptyhd;
	static	 struct	symtab	**hp_ub;

	emptyslot = 0;
	for (nprobes = 0, from = yytext;
	     *from;
	     nprobes <<= 2, nprobes += *from++)
		continue;
	nprobes += from[-1] << 5;
	nprobes %= NHASH;
	if (nprobes < 0)
		nprobes += NHASH;

	initialprobe = nprobes;
	for (hdallop = htab; hdallop != 0; hdallop = hdallop->h_next){
		for (hp = &(hdallop->h_htab[initialprobe]),
				nprobes = 1,
				hp_ub = &(hdallop->h_htab[NHASH]);
		     (*hp) && (nprobes < NHASH);
				hp += nprobes,
				hp -= (hp >= hp_ub) ? NHASH:0,
				nprobes += 2)
		{
			from = yytext;
			to = (*hp)->name;
#ifndef FLEXNAMES
			for (len = 0; (len<NCPS) && *from; len++)
				if (*from++ != *to++)
					goto nextprobe;
			if (len >= NCPS)	/*both are maximal length*/
				return(hp);
			if (*to == 0)		/*assert *from == 0*/
				return(hp);
#else FLEXNAMES
			while (*from && *to)
				if (*from++ != *to++)
					goto nextprobe;
			if (*to == *from)	/*assert both are == 0*/
				return(hp);
#endif FLEXNAMES

	nextprobe: ;
		}
		if (*hp == 0 && emptyslot == 0 &&
		    hdallop->h_nused < HASHCLOGGED) {
			emptyslot = hp;
			emptyhd = hdallop;
		}
	}
	if (emptyslot == 0) {
		htaballoc();
		hdallop = htab->h_next;		/* aren't we smart! */
		hp = &hdallop->h_htab[initialprobe];
	} else {
		hdallop = emptyhd;
		hp = emptyslot;
	}
	if (instflg) {
		*hp = symalloc();
		hdallop->h_nused++;
#ifndef FLEXNAMES
		for(len = 0, from = yytext, to = (*hp)->name; (len<NCPS); len++)
 			if ((*to++ = *from++) == '\0')
 				break;
#else FLEXNAMES
		for (from = yytext, len = 1; *from++; len++)
			continue;
		if (len >= (STRPOOLDALLOP - strplhead->str_nalloc))
			strpoolalloc();
		for ( (*hp)->name = to = strplhead->str_names + strplhead->str_nalloc, from = yytext;
		     ( (*to++ = *from++) != '\0'); )
			continue;
		strplhead->str_nalloc += len;
#endif FLEXNAMES
	}
	return(hp);
}	/*end of lookup*/

char *savestr(str)
	char *str;
{
	register int len;
	register char *from, *to;
	char *res;

	for (from = str, len = 1; *from++; len++)
		continue;
	if (len >= (STRPOOLDALLOP - strplhead->str_nalloc))
		strpoolalloc();
	for ( res = to = strplhead->str_names + strplhead->str_nalloc, from = str;
		     ( (*to++ = *from++) != '\0'); )
			continue;
	strplhead->str_nalloc += len;
	return (res);
}

/*
 *	The following two tables are indexed by
 *		{LEN1,LEN2,LEN4,LEN8} | {PCREL,0}
 *	Note that PCREL = 1
 */
int	reflen[] = 	{0,   0, 1, 1, 2, 2, 4, 4, 8, 8};	
int	lgreflen[] = 	{-1, -1, 0, 0, 1, 1, 2, 2, 3, 3};

/*
 *	The relocation information is saved internally in an array of
 *	lists of relocation buffers.  The relocation buffers are
 *	exactly the same size as a token buffer; if we use VM for the
 *	temporary file we reclaim this storage, otherwise we create
 *	them by mallocing.
 */
#define	RELBUFLG	TOKBUFLG
#define	NRELOC		((TOKBUFLG - \
			  (sizeof (int) + sizeof (struct relbufdesc *)) \
			) / (sizeof (struct relocation_info)))

struct	relbufdesc{
	int	rel_count;
	struct	relbufdesc	*rel_next;
	struct	relocation_info	rel_reloc[NRELOC];
};
extern	struct	relbufdesc	*tok_free;
#define	rel_free tok_free
static	struct	relbufdesc	*rel_temp;
struct	relocation_info r_can_1PC = {0,0,0,0,0,0};
struct	relocation_info	r_can_0PC = {0,0,0,0,0,0};

initoutrel()
{
	r_can_1PC.r_pcrel = 1;
}

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
	short	this_reflen;
	struct	relocation_info	reloc;

	this_reflen = reflen[reftype];
	if (bitoff&07)
		yyerror("Padding error");
	reltype &= ~XFORW;
	if (reltype == XUNDEF)
		yyerror("Undefined reference");

	if (reltype != XABS || reftype & PCREL) {
		reloc = (reftype & PCREL)? r_can_1PC : r_can_0PC;
		reloc.r_address = dotp->xvalue -
			( (dotp < &usedot[NLOC]) ? 0 : datbase );
		reloc.r_length = lgreflen[reftype];
		switch(reltype){
			case XXTRN | XUNDEF:
				reloc.r_symbolnum = xsym->index;
				reloc.r_extern = 1;
				break;
			default:
				reloc.r_symbolnum = reltype;
				break;
		}
		if ( (relfil == 0) || (relfil->rel_count >= NRELOC) ){
			if (rel_free){
				rel_temp = rel_free;
				rel_free = rel_temp->rel_next;
			} else {
				rel_temp = (struct relbufdesc *)
					Calloc(1,sizeof (struct relbufdesc));
			}
			rel_temp->rel_count = 0;
			rel_temp->rel_next = relfil;
			relfil = rusefile[dotp - &usedot[0]] = rel_temp;
		}
		relfil->rel_reloc[relfil->rel_count++] = reloc;
	}
	/*
	 *	write the unrelocated value to the text file
	 */
	dotp->xvalue += this_reflen;
	if (reftype & PCREL)
		*pval -= dotp->xvalue;
	bwrite((char *)pval, this_reflen, txtfil);
}
/*
 *	Flush out all of the relocation information.
 *	Note that the individual lists of buffers are in
 *	reverse order, so we must reverse them
 */
off_t closeoutrel(relocfile)
	BFILE	*relocfile;
{
	int	locindex;
	u_long	Closeoutrel();

	trsize = 0;
	for (locindex = 0; locindex < NLOC; locindex++){
		trsize += Closeoutrel(rusefile[locindex], relocfile);
	}
	drsize = 0;
	for (locindex = 0; locindex < NLOC; locindex++){
		drsize += Closeoutrel(rusefile[NLOC + locindex], relocfile);
	}
	return(trsize + drsize);
}

u_long Closeoutrel(relfil, relocfile)
	struct	relbufdesc	*relfil;
	BFILE	*relocfile;
{
	u_long	tail;
	if (relfil == 0)
		return(0L);
	tail = Closeoutrel(relfil->rel_next, relocfile);
	bwrite((char *)&relfil->rel_reloc[0],
		relfil->rel_count * sizeof (struct relocation_info),
		relocfile);
	return(tail + relfil->rel_count * sizeof (struct relocation_info));
}

int sizesymtab()
{
	struct symtab *sp;

#define NOUTSYMS (nsyms - njxxx - nforgotten - (savelabels ? 0 : nlabels))

	return (
		(
#ifndef FLEXNAMES
		 NCPS
#else FLEXNAMES
		 sizeof (long)
#endif FLEXNAMES
		 + sizeof (sp->ptype)
		 + sizeof (sp->other)
		 + sizeof (sp->desc)
		 + sizeof (sp->value)
		) 
		*	NOUTSYMS
	);
}

#ifdef FLEXNAMES
/*
 *	We write out the flexible length character strings for  names
 *	in two stages.
 *	1)	We have always! maintain a fixed sized name list entry;
 *	the string is indexed by a four byte quantity from the beginning
 *	of the string pool area.  Index 0 is reserved, and indicates
 *	that there is no associated string. The first valid index is 4.
 *	2)	 We concatenate together and write all of the strings
 *	in the string pool at the end of the name list. The first 
 *	four bytes in the string pool are indexed only by 0 (see above);
 *	they contain the total number of bytes in the string pool.
 */
#endif FLEXNAMES

/*
 *	Write out n symbols to file f, beginning at p
 *	ignoring symbols that are obsolete, jxxx instructions, and
 *	possibly, labels
 */

int symwrite(symfile)
	BFILE *symfile;
{
	int	symsout;			/*those actually written*/
	int	symsdesired = NOUTSYMS;
	register	struct	symtab *sp, *ub;
#ifdef FLEXNAMES
	register	int	len;
	long		stroff	= sizeof (stroff);
#endif FLEXNAMES

	register	struct	allocbox	*allocwalk;

	symsout = 0;
	DECLITERATE(allocwalk, sp, ub)
	{
		if (sp->tag >= IGNOREBOUND) 
			continue;
		if ((sp->name[0] == 'L') && (sp->tag == LABELID) && !savelabels)
			continue;
		symsout++;
#ifndef FLEXNAMES
		bwrite(sp->name, NCPS, symfile);
#else FLEXNAMES
		len = strlen(sp->name);
		if (len != 0) {
			bwrite(&stroff, sizeof (stroff), symfile);
			stroff += len + 1;
		} else
			bwrite("\0\0\0\0", sizeof (stroff), symfile);
#endif FLEXNAMES
		sp->type &= ~XFORW;
		bputc( ( (sp->ptype != 0) ? sp->ptype : sp->type ),
			symfile);
	/*
	 *	WATCH OUT.  THIS DEPENDS THAT THE ALLOCATION OF
	 *	the four fields ptype, other, desc and value are
	 *	contiguous, which is compiler dependent.
	 */
		bwrite((char *)&(sp->other),
			  sizeof (sp->other)
			+ sizeof (sp->desc)
		        + sizeof (sp->value),
		       symfile
		);
	}
	if (symsout != symsdesired)
		yyerror("INTERNAL ERROR: Wrote %d symbols, wanted to write %d symbols\n",
			symsout, symsdesired);
#ifdef FLEXNAMES
	/*
	 *	Pass 2 through the string pool
	 */
	symsout = 0;
	bwrite(&stroff, sizeof (stroff), symfile);
	stroff = sizeof (stroff);
	symsout = 0;
	DECLITERATE(allocwalk, sp, ub)
	{
		if (sp->tag >= IGNOREBOUND) 
			continue;
		if ((sp->name[0] == 'L') && (sp->tag == LABELID) && !savelabels)
			continue;
		len = strlen(sp->name);
		if (len)
			bwrite(sp->name, len + 1, symfile);
	}
#endif FLEXNAMES
}
