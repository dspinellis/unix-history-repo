/*
 *	Copyright (c) 1982 Regents of the University of California
 */
#ifndef lint
static char sccsid[] = "@(#)assyms.c 4.7 %G%";
#endif not lint

#include <stdio.h>
#include <ctype.h>
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

Iptr	*itab[NINST];	/*maps opcodes to instructions*/
/*
 *	Counts what went into the symbol table, so that the
 *	size of the symbol table can be computed.
 */
int	nsyms;		/* total number in the symbol table */
int	njxxx;		/* number of jxxx entrys */
int	nforgotten;	/* number of symbols erroneously entered */
int	nlabels;	/* number of label entries */

/*
 *	Managers of the symbol literal storage.
 *	If we have flexible names, then we allocate BUFSIZ long
 *	string, and pack strings into that.  Otherwise, we allocate
 *	symbol storage in fixed hunks NCPS long when we allocate space
 *	for other symbol attributes.
 */
#ifdef	FLEXNAMES
struct	strpool		*strplhead = 0;
#endif	FLEXNAMES

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
	register	Iptr	ip;
	register	struct	symtab	**hp;
	register	char	*p1, *p2;
	register	int	i;

	for (i = 0; i < NINST; i++)
		itab[i] = (Iptr*)BADPOINT;

#ifdef FLEXNAMES
	for (ip = (Iptr)instab; ip->s_name != 0; ip++) {
#else not FLEXNAMES
	for (ip = (Iptr)instab; ip->s_name[0] != '\0'; ip++){
#endif not FLEXNAMES
		p1 = ip->s_name;
		p2 = yytext;
		while (*p2++ = *p1++);
		hp = lookup(0);		/* 0 => don't install this*/
		if (*hp==NULL) {
			*hp = (struct symtab *)ip;
			if (   (ip->s_tag!=INSTn)
			    && (ip->s_tag!=INST0)
			    && (ip->s_tag!=0))
				continue; /* was pseudo-op */
			if (itab[ip->i_eopcode] == (Iptr*)BADPOINT){
				itab[ip->i_eopcode] =
					(Iptr*)ClearCalloc(256, sizeof(Iptr));
				for (i = 0; i < 256; i++)
					itab[ip->i_eopcode][i] =
						(Iptr)BADPOINT;
			}
			itab[ip->i_eopcode][ip->i_popcode] = ip;
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
		if (sp->s_tag >= IGNOREBOUND)
			continue; 		/*totally ignore jxxx entries */
		/*
		 *	Ignore stabs, but give them a symbol table index
		 */
		if (sp->s_type & STABFLAG)
			goto assignindex;
		if ((sp->s_type&XTYPE)==XUNDEF)
			sp->s_type = XXTRN+XUNDEF;
		else if ((sp->s_type&XTYPE)==XDATA)
			sp->s_value += usedot[sp->s_index].e_xvalue;
		else if ((sp->s_type&XTYPE)==XTEXT)
			sp->s_value += usedot[sp->s_index].e_xvalue;
		else if ((sp->s_type&XTYPE)==XBSS) {
			bs = sp->s_value;
			sp->s_value = hdr.a_bss + datbase;
			hdr.a_bss += bs;
		}
	   assignindex:
		if (    (sp->s_name[0] != 'L')
		     || (sp->s_tag != LABELID)
		     || savelabels
		     )			/*then, we will write it later on*/
				sp->s_index = relpos++;
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
		if(sp->s_ptype && (sp->s_type & STABFLAG)) {	
			p = sp->s_dest;	
			sp->s_value = p->s_value;	
			sp->s_index = p->s_index;
			sp->s_type = p->s_type;
		}
	}
}

char *Calloc(number, size)
	int	number, size;
{
	register	char *newstuff;
	char	*sbrk();
	newstuff = sbrk(number*size);
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
#ifdef lint
	length = length;
#endif length
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
		if (alloctail == 0){
			allochead = alloctail = newbox;
		} else {
			alloctail->nextalloc = newbox;
			alloctail = newbox;
		}
	}
	--symsleft;
	++nsyms;
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
	if (p->s_index < q->s_index)
		return(-1);
	if (p->s_index > q->s_index)
		return(1);
	if (p->s_value < q->s_value)
		return(-1);
	if (p->s_value > q->s_value)
		return(1);
	/*
	 *	Force jxxx entries to virtually preceed labels defined
	 *	to follow the jxxxx instruction, so that bumping the
	 *	jxxx instruction correctly fixes up the following labels
	 */
	if (p->s_tag >= IGNOREBOUND)	/*p points to a jxxx*/
		return(-1);		
	if (q->s_tag >= IGNOREBOUND)
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
		if (sp->s_ptype && (sp->s_type &STABFLAG)){
			sp->s_value = sp->s_dest->s_value;
			sp->s_index = sp->s_dest->s_index;
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
		for (; sp && sp->s_index == segno; sp = *++cowalk);
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
				segno, sp->s_name,
				sp->s_value, sp->s_index,
				tagstring(sp->s_tag));
#else not FLEXNAMES
			printf("\tSeg: %d \"%*.*s\" value: %d index: %d tag %s\n",
				segno, NCPS, NCPS, sp->s_name,
				sp->s_value, sp->s_index,
				tagstring(sp->s_tag));
#endif not FLEXNAMES
			printf("\t\ttype: %d jxbump %d jxfear: %d\n",
				sp->s_type, sp->s_jxbump, sp->s_jxfear);
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
			to = (*hp)->s_name;
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
		for(len = 0, from = yytext, to = (*hp)->s_name; (len<NCPS); len++)
 			if ((*to++ = *from++) == '\0')
 				break;
#else FLEXNAMES
		for (from = yytext, len = 1; *from++; len++)
			continue;
		if (len >= (STRPOOLDALLOP - strplhead->str_nalloc))
			strpoolalloc();
		for ( (*hp)->s_name = to = strplhead->str_names + strplhead->str_nalloc, from = yytext;
		     ( (*to++ = *from++) != '\0'); )
			continue;
		strplhead->str_nalloc += len;
#endif FLEXNAMES
	}
	return(hp);
}	/*end of lookup*/

#ifdef FLEXNAMES
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
#endif FLEXNAMES

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
struct	relocation_info r_can_1PC;
struct	relocation_info	r_can_0PC;

initoutrel()
{
	r_can_0PC.r_address = 0;
	r_can_0PC.r_symbolnum = 0;
	r_can_0PC.r_pcrel = 0;
	r_can_0PC.r_length = 0;
	r_can_0PC.r_extern = 0;

	r_can_1PC = r_can_0PC;
	r_can_1PC.r_pcrel = 1;
}

outrel(xp, reloc_how)
	register	struct	exp	*xp;
	int		reloc_how;	/* TYPB..TYPH + (possibly)RELOC_PCREL */
{
	struct		relocation_info	reloc;
	register	int	x_type_mask;	
	int		pcrel;

	x_type_mask = xp->e_xtype & ~XFORW;
	pcrel = reloc_how & RELOC_PCREL;
	reloc_how &= ~RELOC_PCREL;
	
	if (bitoff&07)
		yyerror("Padding error");
	if (x_type_mask == XUNDEF)
		yyerror("Undefined reference");

	if ( (x_type_mask != XABS) || pcrel ) {
		if (ty_NORELOC[reloc_how])
			yyerror("Illegal Relocation of floating or large int number.");
		reloc = pcrel ? r_can_1PC : r_can_0PC;
		reloc.r_address = dotp->e_xvalue -
		    ( (dotp < &usedot[NLOC] || readonlydata) ? 0 : datbase );
		reloc.r_length = ty_nlg[reloc_how];
		switch(x_type_mask){
			case XXTRN | XUNDEF:
				reloc.r_symbolnum = xp->e_xname->s_index;
				reloc.r_extern = 1;
				break;
			default:
				if (readonlydata && (x_type_mask&~XXTRN) == XDATA)
					x_type_mask = XTEXT | (x_type_mask&XXTRN);
				reloc.r_symbolnum = x_type_mask;
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
	dotp->e_xvalue += ty_nbyte[reloc_how];
	if (pcrel)
		xp->e_xvalue -= dotp->e_xvalue;
	switch(reloc_how){
	case TYPO:
	case TYPQ:

	case TYPF:
	case TYPD:
	case TYPG:
	case TYPH:
		bignumwrite(xp->e_number, reloc_how);
		break;

	default:
		bwrite((char *)&(xp->e_xvalue), ty_nbyte[reloc_how], txtfil);
		break;
	}
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

#define NOUTSYMS (nsyms - njxxx - nforgotten - (savelabels ? 0 : nlabels))
int sizesymtab()
{
	return (sizeof (struct nlist) * NOUTSYMS);
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
	char		*name;			/* temp to save the name */
	long		stroff	= sizeof (stroff);
	/*
	 *	We use sp->s_index to hold the length of the
	 *	name; it isn't used for anything else
	 */
#endif FLEXNAMES

	register	struct	allocbox	*allocwalk;

	symsout = 0;
	DECLITERATE(allocwalk, sp, ub)
	{
		if (sp->s_tag >= IGNOREBOUND) 
			continue;
		if ((sp->s_name[0] == 'L') && (sp->s_tag == LABELID) && !savelabels)
			continue;
		symsout++;

#ifdef FLEXNAMES
		name = sp->s_name;		/* save pointer */
		if ( (sp->s_index = strlen(sp->s_name)) != 0){
			sp->s_nmx = stroff;	/* clobber pointer */
			stroff += sp->s_index + 1;
		} else {
			sp->s_nmx = 0;		/* clobber pointer */
		}
#endif
		sp->s_type = (sp->s_ptype != 0) ? sp->s_ptype : (sp->s_type & (~XFORW));
		if (readonlydata && (sp->s_type&~N_EXT) == N_DATA)
			sp->s_type = N_TEXT | (sp->s_type & N_EXT);
		bwrite((char *)&sp->s_nm, sizeof (struct nlist), symfile);
#ifdef FLEXNAMES
		sp->s_name = name;		/* restore pointer */
#endif FLEXNAMES
	}
	if (symsout != symsdesired)
		yyerror("INTERNAL ERROR: Wrote %d symbols, wanted to write %d symbols\n",
			symsout, symsdesired);
#ifdef FLEXNAMES
	/*
	 *	Pass 2 through the string pool
	 */
	symsout = 0;
	bwrite((char *)&stroff, sizeof (stroff), symfile);
	stroff = sizeof (stroff);
	symsout = 0;
	DECLITERATE(allocwalk, sp, ub)
	{
		if (sp->s_tag >= IGNOREBOUND) 
			continue;
		if ((sp->s_name[0] == 'L') && (sp->s_tag == LABELID) && !savelabels)
			continue;
		sp->s_index = strlen(sp->s_name);
		if (sp->s_index)
			bwrite(sp->s_name, sp->s_index + 1, symfile);
	}
#endif FLEXNAMES
}
