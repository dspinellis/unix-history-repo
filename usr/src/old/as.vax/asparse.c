/* Copyright (c) 1980 Regents of the University of California */
static	char sccsid[] = "@(#)asparse.c 4.1 %G%";
#include <stdio.h>
#include <sys/types.h>
#ifdef UNIX
#  include <a.out.h>
#endif UNIX

#ifdef VMS
#  ifdef UNIXDEVEL
#	include <a.out.h>
#  else VMSDEVEL
#  	include <aout.h>
#  endif
#endif VMS
#include "as.h"
#include "asexpr.h"
#include "asscan.h"
#include "assyms.h"

int	lgensym[10];
char	genref[10];

long	bitfield;
int	bitoff;
int	curlen;			/* current length of literals */

/*
 *	The following three variables are communication between various
 *	modules to special case a number of things.  They are properly
 *	categorized as hacks.
 */
extern	struct	symtab *lastnam;/*last name seen by the lexical analyzer*/
int	exprisname;		/*last factor in an expression was a name*/
int	droppedLP;		/*one is analyzing an expression beginning with*/
				/*a left parenthesis, which has already been*/
				/*shifted. (Used to parse (<expr>)(rn)*/

char	yytext[NCPS+2];		/*the lexical image*/
int	yylval;			/*the lexical value; sloppy typing*/
/*
 *	Expression and argument managers
 */
struct	exp	*xp;		/*next free expression slot, used by expr.c*/
struct	exp	explist[NEXP];	/*max of 20 expressions in one opcode*/
struct	arg	arglist[NARG];	/*building up operands in instructions*/
/*
 *	Sets to accelerate token discrimination
 */
char	tokensets[(LASTTOKEN) - (FIRSTTOKEN) + 1];

static	char	UDotsname[32];	/*name of the assembly source*/

int	yyparse()
{
	register	struct	exp	*locxp;
			/*
			 *	loc1xp and ptrloc1xp are used in the
			 * 	expression lookahead
			 */
			struct	exp	*loc1xp;	/*must be non register*/
			struct	exp	**ptrloc1xp = & loc1xp;
			struct	exp	*pval;		/*hacking expr:expr*/

	register	struct	symtab	*np;
	register	int		argcnt;

	register	int		val;		/*what yylex gives*/
	register	int		auxval;		/*saves val*/

	register	struct 	arg	*ap;		/*first free argument*/

			struct	symtab	*p;
	register	struct	symtab	*stpt;

			struct	strdesc	*stringp;	/*handles string lists*/

			int		regno;		/*handles arguments*/
			int		*ptrregno = &regno;
			int		sawmul;		/*saw * */
			int		sawindex;	/*saw [rn]*/
			int		sawsize;
			int		seg_type; 	/*the kind of segment: data or text*/
			int		seg_number;	/*the segment number*/
			int		space_value;	/*how much .space needs*/

			int		field_width;	/*how wide a field is to be*/
			int		field_value;	/*the value to stuff in a field*/
			char		*stabname;	/*name of stab dealing with*/
			ptrall		stabstart;	/*where the stab starts in the buffer*/

	xp = explist;
	ap = arglist;

	val = yylex();

    while (val != PARSEEOF){	/* primary loop */

	while (INTOKSET(val, LINSTBEGIN)){
		if (val == INT) {
			int i = ((struct exp *)yylval)->xvalue;
			shift;
			if (val != COLON)
				goto nocolon;
			if (i < 0 || i > 9) {
				yyerror("Local labels are 0-9");
				goto errorfix;
			}
			sprintf(yytext, "L%d\001%d", i, lgensym[i]);
			lgensym[i]++;
			genref[i] = 0;
			yylval = (int)*lookup(passno == 1);
			val = NAME;
			np = (struct symtab *)yylval;
			goto restlab;
		}
		if (val == NL){
			lineno++;
			shift;
		} else
		if (val == SEMI) 
			shift;
		else {	/*its a name, so we have a label or def */
			if (val != NAME){
				ERROR("Name expected for a label");
			}
			np = (struct symtab *)yylval;
			shiftover(NAME);
nocolon:
			if (val != COLON) {
				yyerror("\"%s\" is not followed by a ':' for a label definition",
					np->name);
				goto  errorfix;
			}
restlab:
			shift;
			flushfield(NBPW/4);
			if ((np->type&XTYPE)!=XUNDEF) {
				if(  (np->type&XTYPE)!=dotp->xtype 
				   || np->value!=dotp->xvalue
				   || (  (passno==1)
				       &&(np->index != dotp->xloc)
				      )
				  ){
#ifndef DEBUG
					if (np->name[0] != 'L')
#endif not DEBUG
					{
						if (passno == 1)
						  yyerror("%.8s redefined", np->name);
						else
						  yyerror("%.8s redefined: PHASE ERROR, 1st: %d, 2nd: %d",
						   np->name,
						   np->value,
						   dotp->xvalue);
					}
				}
			}
			np->type &= ~(XTYPE|XFORW);
			np->type |= dotp->xtype;
			np->value = dotp->xvalue;
			if (passno == 1){
				np->index = dotp-usedot;
				if (np->name[0] == 'L'){
					nlabels++;
				}
				np->tag = LABELID;
			}
		}	/*end of this being a label*/
	}	/*end of to consuming all labels, NLs and SEMIS */ 

	xp = explist;
	ap = arglist;

	/*
	 *	process the INSTRUCTION body
	 */
	switch(val){

    default:
	ERROR("Unrecognized instruction or directive");

   case IABORT:
	shift;
	sawabort();
	/*NOTREACHED*/
	break;

   case PARSEEOF:
	tokptr -= sizeof(toktype);
	*tokptr++ = VOID;
	tokptr[1] = VOID;
	tokptr[2] = PARSEEOF;
	break;

   case IFILE:
	shift;
	stringp = (struct strdesc *)yylval;
	shiftover(STRING);
	dotsname = &UDotsname[0];
	movestr(dotsname, stringp->str,
		stringp->str_lg >= 32? 32 :stringp->str_lg);
	dotsname[stringp->str_lg] = '\0';
	break;

   case ILINENO:
	shift;		/*over the ILINENO*/
	expr(locxp, val);
	lineno = locxp->xvalue;
	break;

   case ISET: 	/* .set  <name> , <expr> */
	shift;
	np = (struct symtab *)yylval;
	shiftover(NAME);
	shiftover(CM);
	expr(locxp, val);
	np->type &= (XXTRN|XFORW);
	np->type |= locxp->xtype&(XTYPE|XFORW);
	np->value = locxp->xvalue;
	if (passno==1)
		np->index = locxp->xloc;
	if ((locxp->xtype&XTYPE) == XUNDEF)
		yyerror("Illegal set?");
	break;

   case ILSYM: 	/*.lsym name , expr */
	shift;
	np = (struct symtab *)yylval;
	shiftover(NAME);
	shiftover(CM);
	expr(locxp, val);
	/*
	 *	Build the unique occurance of the
	 *	symbol.
	 *	The character scanner will have
	 *	already entered it into the symbol
	 *	table, but we should remove it
	 */
	if (passno == 1){
		stpt = (struct symtab *)symalloc();
#ifdef FLEXNAMES
		stpt->name = np->name;
#else
		movestr(stpt->name, np->name, NCPS);
#endif
		np->tag = OBSOLETE;	/*invalidate original */
		nforgotten++;
		np = stpt;
		if (locxp->xtype != XABS) 
			("Illegal lsym");
		np->value=locxp->xvalue;
		np->type=XABS;
		np->tag = ILSYM;
	}
	break;

   case IGLOBAL: 	/*.globl <name> */
	shift;
	np = (struct symtab *)yylval;
	shiftover(NAME);
	np->type |= XXTRN;
	break;

   case IDATA: 	/*.data [ <expr> ] */
   case ITEXT: 	/*.text [ <expr> ] */
	seg_type = -val;
	shift;
	if (INTOKSET(val, EBEGOPS+YUKKYEXPRBEG+SAFEEXPRBEG)){
		expr(locxp, val);
		seg_type = -seg_type;   /*now, it is positive*/
	}

	if (seg_type < 0) {	/*there wasn't an associated expr*/
		seg_number = 0;
		seg_type = -seg_type;
	} else {
		if (locxp->xtype != XABS || (seg_number=locxp->xvalue) >= NLOC) {
			yyerror("illegal location counter");
			seg_number = 0;
		}
	}
	if (seg_type == IDATA)
		seg_number += NLOC;
	flushfield(NBPW/4);
	dotp = &usedot[seg_number];
#ifdef UNIX
	if (passno==2) {	/* go salt away in pass 2*/
		txtfil = usefile[seg_number];
		relfil = rusefile[seg_number];
	}
#endif UNIX
#ifdef VMS
	if (passno==2) {
		puchar(vms_obj_ptr,6);		/*  setpl  */
		puchar(vms_obj_ptr,seg_number);	/* psect # */
		plong(vms_obj_ptr,dotp->xvalue);/*  offset */
		puchar(vms_obj_ptr,80);		/*  setrb  */
		if((vms_obj_ptr-sobuf) > 400){
			write(objfil,sobuf,vms_obj_ptr-sobuf);
			vms_obj_ptr=sobuf+1;	/*flush buf*/
		}
	}
#endif VMS
	break;

	/*
	 *	Storage filler directives:
	 *
	 *	.byte	[<exprlist>]
	 *
	 *	exprlist:  empty | exprlist outexpr
	 *	outexpr:   <expr> | <expr> : <expr>
	 */
   case IBYTE:	curlen = NBPW/4; goto elist;

   case IINT:
   case ILONG:	curlen = NBPW;   goto elist;

   case IWORD:
	curlen = NBPW/2;
   elist:
	seg_type = val;
	shift;

	/*
	 *	Expression List processing
	 */
	if (INTOKSET(val, EBEGOPS+YUKKYEXPRBEG+SAFEEXPRBEG)){
	    do{
		/*
		 *	expression list consists of a list of :
		 *	<expr>
		 *	<expr> : <expr> 
		 *		(pack expr2 into expr1 bits
		 */
		expr(locxp, val);
		/*
		 *	now, pointing at the next token
		 */
		if (val == COLON){
			shiftover(COLON);
			expr(pval, val);
			if (locxp->xtype != XABS)
			  yyerror("Width not absolute");
			field_width = locxp->xvalue;
			locxp = pval;
			if (bitoff + field_width >
			  curlen)
				flushfield(curlen);
			if (field_width > curlen)
				yyerror("Expression crosses field boundary");
		} else {
			field_width = curlen;
			flushfield(curlen);
		}

		 if ((locxp->xtype&XTYPE)!=XABS) {
			if (bitoff)
				yyerror("Illegal relocation in field");
			field_width=LEN1;
			if (curlen==NBPW)
				field_width = LEN4;
			if (curlen==NBPW/2)
				field_width = LEN2;
			if (passno == 1){
				dotp->xvalue += reflen[field_width];
			} else {
				outrel(&locxp->xvalue,
					field_width,
					locxp->xtype,
					locxp->xname);
			}
		} else {
			field_value = locxp->xvalue & ( (1L << field_width)-1);
			bitfield |= field_value << bitoff;
			bitoff += field_width;
		}
		if ( auxval = (val == CM)) shift;
		xp = explist;
	    } while (auxval);
	}	/*existed an expression  at all*/

	flushfield(curlen);
	if ( ( curlen == NBPW/4) && bitoff)
		dotp->xvalue ++;
	break;
	/*end of case IBYTE, IWORD, ILONG, IINT*/

   case ISPACE: 	/* .space <expr> */
	shift;
	expr(locxp, val);
	if (locxp->xtype != XABS)
		yyerror("Space size not absolute");
	space_value = locxp->xvalue;
  ospace:
	flushfield(NBPW/4);
#ifdef UNIX
	while (space_value > 96){
		outs(strbuf[2].str, 96);
		space_value -= 96;
	}
	outs(strbuf[2].str, space_value);
#endif UNIX
#ifdef VMS
	dotp->xvalue += space_value;		/*bump pc*/
	if (passno==2){
	  if(*(strbuf[2].str)==0) {
		puchar(vms_obj_ptr,81);		/* AUGR  */
		pulong(vms_obj_ptr,space_value);/* incr  */
	  } else yyerror("VMS, encountered non-0 .space");
	  if ((vms_obj_ptr-sobuf) > 400) {
		write(objfil,sobuf,vms_obj_ptr-sobuf);
		vms_obj_ptr=sobuf+1;		/*pur buf*/
	  }
	}
#endif VMS
	break;

#ifdef UNIX
   case	IFILL:	/* .fill count, value */
		/* fill count bytes with value */
	shift;
	expr(locxp, val);
	if (locxp->xtype != XABS)
		yyerror("Fill repetition count not absolute");
	space_value = locxp->xvalue;
	shiftover(CM);
	expr(locxp, val);
	if (locxp->xtype != XABS)
		yyerror("Fill value not absolute");
	flushfield(NBPW/4);
	while(space_value-- > 0)
		outb(locxp->xvalue & 0xFF);
	break;
#endif UNIX

   case IASCII:	/* .ascii [ <stringlist> ] */
   case IASCIZ: 	/* .asciz [ <stringlist> ] */
	auxval = val;
	shift;

	/*
	 *	Code to consume a string list
	 *
	 *	stringlist: empty | STRING | stringlist STRING
	 */
	while (val ==  STRING){
		flushfield(NBPW/4);
		if (bitoff)
		  dotp->xvalue++;
		stringp = (struct strdesc *)yylval;
#ifdef UNIX
		outs(stringp->str, stringp->str_lg);
#endif UNIX
#ifdef VMS
		{
			register int i;
			for (i=0; i < stringp->str_lg; i++){
			  dotp->xvalue += 1;
			    if (passno==2){
				puchar(vms_obj_ptr,-1);
			  	puchar(vms_obj_ptr,stringp->str[i]);
			  	if (vms_obj_ptr-sobuf > 400) {
				  write(objfil,sobuf,vms_obj_ptr-sobuf);
				  vms_obj_ptr = sobuf + 1;
			  	}
			    }
			}
		}
#endif VMS
		shift;		/*over the STRING*/
		if (val == CM)	/*could be a split string*/
			shift;
	}

	if (auxval == IASCIZ){
		flushfield(NBPW/4);
#ifdef UNIX
		outb(0);
#endif UNIX
#ifdef VMS
		if (passno == 2) {
			puchar(vms_obj_ptr,-1);
			puchar(vms_obj_ptr,0);
		}
		dotp->xvalue += 1;
#endif VMS
	}
	break;
	
   case IORG: 	/* .org <expr> */
	shift;
	expr(locxp, val);

	if (locxp->xtype==XABS)
		orgwarn++;
	else if ((locxp->xtype & ~XXTRN) != dotp->xtype)
		yyerror("Illegal expression to set origin");
	space_value = locxp->xvalue - dotp->xvalue;
	if (space_value < 0)
		yyerror("Backwards 'org'");
	goto ospace;
	break;

/*
 *
 *	Process stabs.  Stabs are created only by the f77
 *	and the C compiler with the -g flag set.
 *	We only look at the stab ONCE, during pass 1, and
 *	virtually remove the stab from the intermediate file
 *	so it isn't seen during pass2.  This makes for some
 *	hairy processing to handle labels occuring in
 *	stab entries, but since most expressions in the
 *	stab are integral we save lots of time in the second
 *	pass by not looking at the stabs.
 *	A stab that is tagged floating will be bumped during
 *	the jxxx resolution phase.  A stab tagged fixed will
 *	not be be bumped.
 *
 *	.stab:	Old fashioned stabs
 *	.stabn: For stabs without names
 *	.stabs:	For stabs with string names
 *	.stabd: For stabs for line numbers or bracketing,
 *		without a string name, without
 *		a final expression.  The value of the
 *		final expression is taken to be  the current
 *		location counter, and is patched by the 2nd pass
 *
 *	.stab{<expr>,}*8,<expr>, <expr>, <expr>, <expr>
 *	.stabn		 <expr>, <expr>, <expr>, <expr>
 *	.stabs   STRING, <expr>, <expr>, <expr>, <expr>
 *	.stabd		 <expr>, <expr>, <expr> # . 
 */
   case ISTAB: 
#ifndef FLEXNAMES
	stabname = ".stab";
	if (passno == 2)	goto errorfix;
	stpt = (struct symtab *)yylval;
	/*
	 *	Make a pointer to the .stab slot.
	 *	There is a pointer in the way (stpt), and
	 *	tokptr points to the next token.
	 */
	stabstart = tokptr;
	(char *)stabstart -= sizeof(struct symtab *);
	(char *)stabstart -= sizeof(toktype);
	shift;
	for (argcnt = 0; argcnt < 8; argcnt++){
		expr(locxp, val);
		stpt->name[argcnt] = locxp->xvalue;
		xp = explist;
		shiftover(CM);
	}
	goto tailstab;
#else	FLEXNAMES
	yyerror(".stab directive not supported in; report this compiler bug to system administrator");
	goto errorfix;
#endif FLEXNAMES

  tailstab:
	expr(locxp, val);
	if (! (locxp->xvalue & STABTYPS)){
		yyerror("Invalid type in %s",stabname);
		goto errorfix;
	}
	stpt->ptype = locxp->xvalue;
	shiftover(CM);
	expr(locxp, val);
	stpt->other = locxp->xvalue;
	shiftover(CM);
	expr(locxp, val);
	stpt->desc = locxp->xvalue;
	shiftover(CM);
	exprisname = 0;
	expr(locxp, val);
	p = locxp->xname;
	if (p == NULL) {	/*absolute expr to begin with*/
		stpt->value = locxp->xvalue;
		stpt->index = dotp - usedot;
		if (exprisname){
			switch(stpt->ptype){
				case N_GSYM:
				case N_FNAME:
				case N_RSYM:
				case N_SSYM:
				case N_LSYM:
				case N_PSYM:
				case N_BCOMM:
				case N_ECOMM:
				case N_LENG:
					stpt->tag = STABFIXED;
					break;
				default:
					stpt->tag = STABFLOATING;
					break;
			}
		} else
			stpt->tag = STABFIXED;
	}
	else {		/*really have a name*/
		stpt->dest = locxp->xname;
		stpt->index = p->index;
		stpt->type = p->type | STABFLAG;
		/*
		 *	We will assign a more accruate
		 *	guess of locxp's location when
		 *	we sort the symbol table
		 *	The final value of value is
		 *	given by stabfix()
		 */
		stpt->tag = STABFLOATING;
	}
	/*
	 *	tokptr now points at one token beyond
	 *	the current token stored in val and yylval,
	 *	which are the next tokens after the end of
	 *	this .stab directive.  This next token must
	 *	be either a SEMI or NL, so is of width just
	 *	one.  Therefore, to point to the next token
	 *	after the end of this stab, just back up one..
	 */
	buildskip(stabstart, (char *)tokptr - sizeof(toktype));
	break;	/*end of the .stab*/

   case ISTABDOT:	
	stabname = ".stabd";
	stpt = (struct symtab *)yylval;
	/*
	 *	We clobber everything after the
	 *	.stabd and its pointer... we MUST
	 *	be able to get back to this .stabd
	 *	so that we can resolve its final value
	 */
	stabstart = tokptr;
	shift;		/*over the ISTABDOT*/
	if (passno == 1){
		expr(locxp, val);
		if (! (locxp->xvalue & STABTYPS)){
			yyerror("Invalid type in .stabd");
			goto errorfix;
		}
		stpt->ptype = locxp->xvalue;
		shiftover(CM);
		expr(locxp, val);
		stpt->other = locxp->xvalue;
		shiftover(CM);
		expr(locxp, val);
		stpt->desc = locxp->xvalue;
		/*
		 *
		 *	Now, clobber everything but the
		 *	.stabd pseudo and the pointer
		 *	to its symbol table entry
		 *	tokptr points to the next token,
		 *	build the skip up to this
		 */
		buildskip(stabstart, (toktype *)tokptr - sizeof(toktype));
	}
	/*
	 *	pass 1:	Assign a good guess for its position
	 *		(ensures they are sorted into right place)/
	 *	pass 2:	Fix the actual value
	 */
	stpt->value = dotp->xvalue;
	stpt->index = dotp - usedot;
	stpt->tag = STABFLOATING;	/*although it has no effect in pass 2*/
	break;

   case ISTABNONE:	stabname = ".stabn"; goto shortstab;

   case ISTABSTR: 	stabname = ".stabs";
   shortstab:
	auxval = val;
	if (passno == 2) goto errorfix;
	stpt = (struct symtab *)yylval;
	stabstart = tokptr;
	(char *)stabstart -= sizeof(struct symtab *);
	(char *)stabstart -= sizeof(toktype);
	shift;
	if (auxval == ISTABSTR){
		stringp = (struct strdesc *)yylval;
		shiftover(STRING);
#ifndef FLEXNAMES
		auxval = stringp->str_lg > NCPS ? NCPS : stringp->str_lg;
#else
		stringp->str[stringp->str_lg] = 0;
#endif
		shiftover(CM);
	} else {
		stringp = &(strbuf[2]);
#ifndef FLEXNAMES
		auxval = NCPS;
#endif
	}
#ifndef FLEXNAMES
	movestr(stpt->name, stringp->str, auxval);
#else
	stpt->name = savestr(stringp->str);
#endif
	goto tailstab;
	break;

   case ICOMM:	/* .comm  <name> , <expr> */
   case ILCOMM: 	/* .lcomm <name> , <expr> */
	auxval = val;
	shift;
	np = (struct symtab *)yylval;
	shiftover(NAME);
	shiftover(CM);
	expr(locxp, val);

	if (locxp->xtype != XABS)
		yyerror("comm size not absolute");
	if (passno==1 && (np->type&XTYPE)!=XUNDEF)
		yyerror("Redefinition of %.8s", np->name);
	if (passno==1) {
		np->value = locxp->xvalue;
		if (auxval == ICOMM)
			np->type |= XXTRN;
		else {
			np->type &= ~XTYPE;
			np->type |= XBSS;
		}
	}
	break;

   case IALIGN: 		/* .align <expr> */
	stpt = (struct symtab *)yylval;
	shift;
	expr(locxp, val);
	jalign(locxp, stpt);
	break;

   case INST0: 		/* instructions w/o arguments*/
	insout(yylval, (struct arg *)0, 0);
	shift;	
	break;

   case INSTn:		/* instructions with arguments*/
   case IJXXX: 		/* UNIX style jump instructions */
	auxval = val;
	seg_type = yylval;
	/*
	 *	Code to process an argument list
	 */
	ap = arglist;
	xp = explist;	

	shift;		/* bring in the first token for the arg list*/

	for (argcnt = 1; argcnt <= 6; argcnt++, ap++){
		/*
		 *	code to process an argument proper
		 */
	    sawindex  = sawmul = sawsize = 0;
	    {
		switch(val) {

		   default:
		     disp:
			if( !(INTOKSET(val,
				 EBEGOPS
				+YUKKYEXPRBEG
				+SAFEEXPRBEG)) ) {
				ERROR("expression expected");
			}
			expr(ap->xp,val);
		     overdisp:
			if ( val == LP || sawsize){
				shiftover(LP);
				findreg(regno);
				shiftover(RP);
				ap->atype = ADISP;
				ap->areg1 = regno;
			} else {
				ap->atype = AEXP;
				ap->areg1 = 0;
			}
			goto index;

		   case SIZESPEC: 
		     sizespec:
			sawsize = yylval;
			shift;
			goto disp;

		   case REG:
		   case REGOP: 
			findreg(regno);
			ap->atype = AREG;
			ap->areg1 = regno;
			break;
		    
		   case MUL: 
			sawmul = 1;
			shift;
			if (val == LP) goto base;
			if (val == LITOP) goto imm;
			if (val == SIZESPEC) goto sizespec;
			if (INTOKSET(val,
				 EBEGOPS
				+YUKKYEXPRBEG
				+SAFEEXPRBEG)) goto disp;
			ERROR("expression, '(' or '$' expected");
			break;

		   case LP: 
		     base:
			shift;	/*consume the LP*/
			/*
			 *	hack the ambiguity of
			 *	movl (expr) (rn), ...
			 *	note that (expr) could also
			 *	be (rn) (by special hole in the
			 *	grammar), which we ensure
			 *	means register indirection, instead
			 *	of an expression with value n
			 */
			if (val != REG && val != REGOP){
				droppedLP = 1;
				val = exprparse(val, &(ap->xp));
				droppedLP = 0;
				goto overdisp;
			}
			findreg(regno);
			shiftover(RP);
			if (val == PLUS){
				shift;
				ap->atype = AINCR;
			} else
				ap->atype = ABASE;
			ap->areg1 = regno;
			goto index;

		   case LITOP: 
		      imm:
			shift;
			expr(locxp, val);
			ap->atype = AIMM;
			ap->areg1 = 0;
			ap->xp = locxp;
			goto index;

		   case MP: 
			shift;	/* -(reg) */
			findreg(regno);
			shiftover(RP);
			ap->atype = ADECR;
			ap->areg1 = regno;
	  index:			/*look for [reg] */
			if (val == LB){
				shift;
				findreg(regno);
				shiftover(RB);
				sawindex = 1;
				ap->areg2 = regno;
			}
			break;

		}	/*end of the switch to process an arg*/
	    }	/*end of processing an argument*/

	    if (sawmul){
			/*
			 * Make a concession for *(%r)
			 * meaning *0(%r) 
			 */
			if (ap->atype == ABASE) {
				ap->atype = ADISP;
				xp->xtype = XABS;
				xp->xvalue = 0;
				xp->xloc = 0;
				ap->xp = xp++;
			}
			ap->atype |= ASTAR;
			sawmul = 0;
	    }
	    if (sawindex){
		ap->atype |= AINDX;
		sawindex = 0;
	    }
	    ap->dispsize = sawsize == 0 ? d124 : sawsize;
		if (val != CM) break;
		shiftover(CM);
	}	/*processing all the arguments*/

	if (argcnt > 6){
		yyerror("More than 6 arguments");
		goto errorfix;
	}

	insout(seg_type, arglist,
		auxval == INSTn ? argcnt : - argcnt);
	break;

   case IFLOAT:	curlen = 4;	goto floatlist;
   case IDOUBLE: 
	curlen = 8;
      floatlist:	
	/*
	 *	eat a list of floating point numbers
	 */
	shift;
	if (val == FLTNUM){
		/* KLS MOD */
		float flocal;
		do{
			if (val == CM) shift;
			if (val != FLTNUM) {
			  ERROR("floating number expected");
			}
			dotp->xvalue += curlen;
#ifdef UNIX
			if (passno == 2) {
			  if(curlen == 8)
			   bwrite((char *)&(((union Double *)yylval)->dvalue),
				curlen, txtfil);
			  else  {
			   flocal = ((union Double *)yylval)->dvalue;
			   bwrite((char *)&flocal, curlen, txtfil);
			  }
			}
#endif UNIX

#ifdef VMS
			if (passno == 2) {
			   puchar(vms_obj_ptr,-4);
			   pulong(vms_obj_ptr,
			    ((struct exp *)yylval)
				->doub_MSW);
			   if (curlen==8) {
			    puchar(vms_obj_ptr,-4);
			    pulong(vms_obj_ptr,
			    ((struct exp *)yylval)
				->doub_LSW);
			   }
			   if((vms_obj_ptr-sobuf) > 400) {
			    write(objfil,sobuf,vms_obj_ptr-sobuf);
			    vms_obj_ptr = sobuf + 1;
			   }
			}
#endif VMS
			shift;
			xp = explist;
		} while (val == CM);
	}
	break;
    }	/*end of the switch for looking at each reserved word*/

     continue;

   errorfix: 
	/*
	 *	got here by either requesting to skip to the
	 *	end of this statement, or by erroring out and
	 *	wanting to apply panic mode recovery
	 */
	while (    (val != NL) 
		&& (val != SEMI) 
		&& (val != PARSEEOF)
	      ){
		shift;
	}
	if (val == NL)
		lineno++;
	shift;

    }	/*end of the loop to read the entire file, line by line*/

}	/*end of yyparse*/
	
/*
 *	Process a register declaration of the form
 *	% <expr>
 *
 *	Note:
 *		The scanner has already processed funny registers of the form
 *	%dd[+-]*, where dd is a decimal number in the range 00 to 15 (optional
 *	preceding zero digit).  If there was any space between the % and
 *	the digit, the scanner wouldn't have recognized it, so we
 *	hack it out here.
 */
int funnyreg(val, regnoback)		/*what the read head will sit on*/
	int	val;			/*what the read head is sitting on*/
	int	*regnoback;		/*call by return*/
{
	register	struct	exp *locxp;
			struct	exp *loc1xp;
			struct	exp **ptrloc1xp = & loc1xp;

	expr(locxp, val);	/*and leave the current read head with value*/
	if ( (passno == 2) &&
	    (   locxp->xtype & XTYPE != XABS
	     || locxp->xvalue < 0
	     || locxp->xvalue >= 16 
	    )
	  ){
		yyerror("Illegal register");
		return(0);
	}
	*regnoback = locxp->xvalue;
	return(val);
} 

/*VARARGS1*/
yyerror(s, a1, a2,a3,a4,a5)
	char	*s;
{
	FILE		*sink;

#ifdef DEBUG
	sink = stdout;
#else not DEBUG
	sink = stderr;
#endif DEBUG

	if (anyerrs == 0 && ! silent) 
		fprintf(sink, "Assembler:\n");
	anyerrs++;
	if (silent) return;
	
	fprintf(sink, "\"%s\", line %d: ", dotsname, lineno);
	fprintf(sink, s, a1, a2,a3,a4,a5);
	fprintf(sink, "\n");
}
