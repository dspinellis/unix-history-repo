/* Copyright (c) 1979 Regents of the University of California */
#include <stdio.h>
#include <a.out.h>
#include "as.h"
#include "asexpr.h"
#include "asscan.h"
#include "assyms.h"

extern	struct	exp	usedot[];/*information on the dot for each seg*/
struct	exp	*dotp = &usedot[0];	/*current dot*/
int	anyerrs;

int	passno	= 1;

FILE	*tmpfil;
FILE	*relfil;		/*relocation info sent here*/
FILE	*txtfil;		/*text (for any text #) sent here*/

int	hshused;		/*hash slots consumed */
long	tsize;
long	dsize;

long	bitfield;
int	bitoff;

/*
 *	The following three variables are communication between various
 *	modules to special case a number of things.  They are properly
 *	categorized as hacks.
 */
struct	symtab *lastnam;	/*last name seen by the lexical analyzer*/
int	exprisname;		/*last factor in an expression was a name*/
int	droppedLP;		/*one is analyzing an expression beginning with*/
				/*a left parenthesis, which has already been*/
				/*shifted. (Used to parse (<expr>)(rn)*/

char	yytext[NCPS+2];
static	char	Dotsname[32];

struct	exp	*xp;		/*next free expression slot, used by expr.c*/

int	yylval;			/*the lexical value; sloppy typing*/

extern 	ptrall tokptr;	/*points to current token being eaten*/
extern	int	d124;

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
			long		space_value;	/*how much .space needs*/

			int		field_width;	/*how wide a field is to be*/
			int		field_value;	/*the value to stuff in a field*/
			char		*stabname;	/*name of stab dealing with*/
			ptrall		stabstart;	/*where the stab starts in the buffer*/

	xp = explist;
	ap = arglist;

	val = yylex();
	while (val != PARSEEOF){
		while (INTOKSET(val, LINSTBEGIN)){
			if (val == NL){
				lineno++;
				shift;
			} else
			if (val == SEMI) 
				shift;
			else {	/*its a name, so we have a label (hopefully*/
				if (val != NAME){
					ERROR("Name expected for a label");
				}
				np = (struct symtab *)yylval;
				shiftover(NAME);
				shiftover(COLON);
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
#endif
						{
							yyerror("%.8s redefined", np->name);
#ifdef DEBUG
							printf("name.value=%d, dotp->xvalue=%d\n",
							np->value, dotp->xvalue);
#endif
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
			dotsname = &Dotsname[0];
			movestr(dotsname, stringp->str,
				stringp->str_lg >= 32? 32 :stringp->str_lg);
			dotsname[stringp->str_lg] = '\0';
#ifdef DEBUG
			if (debug)
				printf("(from parser) Now considered to be in file %s\n",
					dotsname);
#endif
			break;
		
		case ILINENO:
			shift;		/*over the ILINENO*/
			expr(locxp, val);
			lineno = locxp->xvalue;
#ifdef DEBUG
			if (debug)
				printf("Now considered to be on line number %d\n",
				lineno);
#endif
			break;

		case ISET: {	/* .set  <name> , <expr> */
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
		}	/*end of case ISET*/

		case ILSYM: {	/*.lsym name , expr */
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
				movestr(stpt->name, np->name, NCPS);
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
		}	/*end of case ILSYM*/

		case IGLOBAL: {	/*.globl <name> */
			shift;
			np = (struct symtab *)yylval;
			shiftover(NAME);
			np->type |= XXTRN;
			break;
		}	/*end of case IGLOBAL*/

		case IDATA: 	/*.data [ <expr> ] */
		case ITEXT: {	/*.text [ <expr> ] */
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
			if (passno==2) {	/* go salt away in pass 2*/
				if (usefile[seg_number] == NULL) {
					tmpn2[TMPC] = 'a'+seg_number;
					if ((usefile[seg_number] =
					   fopen(tmpn2, "w"))==NULL) {
						yyerror("cannot create temp %s", tmpn2);
						delexit();
					}

					tmpn3[TMPC] = 'a'+seg_number;
					if ((rusefile[seg_number] =
					   fopen(tmpn3, "w"))==NULL) {

						yyerror("cannot create temp %s",
						   tmpn3);
					}
				}
				txtfil = usefile[seg_number];
				relfil = rusefile[seg_number];
			}

			break;
		}	/*end of case .TEXT and .DATA*/

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

		case IWORD: {	
			curlen = NBPW/2;
		   elist:
			seg_type = val;
			shift;

			/*
			 *	This processes an expression list 
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
					} /*value being colon*/
					else {
						field_width = curlen;
						flushfield(curlen);
					}

					 if ((locxp->xtype&XTYPE)!=XABS) {
						if (bitoff)
							yyerror("Illegal relocation in field");
						field_width=LEN1+!PCREL;
						if (curlen==NBPW)
							field_width = LEN4 + !PCREL;
						if (curlen==NBPW/2)
							field_width = LEN2 + !PCREL;
						/*
						 *	Save relocation information for this non absolute
						 *	symbol:
						 *	pass 1:	saves enough space for the value, and
						 *		fixes dotp.
						 *	pass 2:	writes the address info in ld compatable
						 *		format onto one of the relfiles
						 */
						outrel(&locxp->xvalue,
							field_width,
							locxp->xtype,
							locxp->xname);
					} else {
						field_value = locxp->xvalue & ( (1L << field_width)-1);
						bitfield |= field_value << bitoff;
						bitoff += field_width;
					}
					if ( auxval = (val == CM)) shift;
				} while (auxval);
			}	/*existed an expression  at all*/

			flushfield(curlen);
			if ( ( curlen == NBPW/4) && bitoff)
				dotp->xvalue ++;
			break;
		}	/*end of case IBYTE, IWORD, ILONG, IINT*/
		
		case ISPACE: {	/* .space <expr> */
			shift;
			expr(locxp, val);
			if (locxp->xtype != XABS)
				yyerror("Space size not absolute");
			space_value = locxp->xvalue;
		  ospace:
			flushfield(NBPW/4);
			while (space_value > 96){
				outs(strbuf[2].str, 96);
				space_value -= 96;
			}
			outs(strbuf[2].str, space_value);
			break;
		}	/*end of case ISPACE*/
		
		case IASCII:	/* .ascii [ <stringlist> ] */
		case IASCIZ: {	/* .asciz [ <stringlist> ] */
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
				outs(stringp->str, stringp->str_lg);
				shift;		/*over the STRING*/
				if (val == CM)	/*could be a split string*/
					shift;
			}

			if (auxval == IASCIZ){
				flushfield(NBPW/4);
				outb(0);
			}
			break;
		}	/*end of case IASCII and IASIZ*/
			
		case IORG: {	/* .org <expr> */
			shift;
			expr(locxp, val);

			if (locxp->xtype==XABS)
				orgwarn++;
			else if (locxp->xtype!=dotp->xtype)
				yyerror("Illegal expression to set origin");
			space_value = locxp->xvalue - dotp->xvalue;
			if (space_value < 0)
				yyerror("Backwards 'org'");
			goto ospace;
			break;
		}	/*end of case IORG*/
		
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
		case ISTAB: {
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
				shiftover(CM);
			}
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
				stpt->tag = STABFLOAT;
#ifdef DSTAB
				printf("FORWARD REF FOR %s...\n", stabname);
				printf("value (xname) = %x value(value(xname) = %x\n",
					stpt->dest,stpt->dest->value);
				printf("name: %.8s\n\n",
					(stpt->dest)->name);
#endif
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
		}	/*end of ISTAB*/
		
		case ISTABDOT:	{
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
			stpt->tag = STABFLOAT;	/*although it has no effect in pass 2*/
			break;
		}	/*end of case ISTABDOT*/

		case ISTABNONE:		stabname = ".stabn"; goto shortstab;

		case ISTABSTR: {	stabname = ".stabs";
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
				auxval = stringp->str_lg > NCPS ? NCPS : stringp->str_lg;
				shiftover(CM);
			} else {
				stringp = &(strbuf[2]);
				auxval = NCPS;
			}
			movestr(stpt->name, stringp->str, auxval);
			goto tailstab;
			break;
		}	/*end of case ISTABSTR and ISTABN*/
		
		case ICOMM:	/* .comm  <name> , <expr> */
		case ILCOMM: {	/* .lcomm <name> , <expr> */
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
		}	/*end of case ICOMM and ILCOMM*/
		
		case IALIGN: {		/* .align <expr> */
			stpt = (struct symtab *)yylval;
			shift;
			expr(locxp, val);
			jalign(locxp, stpt);
			break;
		}	/*end of case IALIGN*/
		
		case INST0: {		/* instructions w/o arguments*/
			insout(yylval, 0, 0);
			shift;	
			break;
		}	/*end of case INST0*/
		
		case INSTn:		/* instructions with arguments*/
		case IJXXX: {		/* UNIX style jump instructions */
			auxval = val;
			seg_type = yylval;
			/*
			 *	Code to process an argument list
			 */
			ap = arglist;
			xp = explist;	/*must be set before bring in the first token*/

			shift;		/*and bring in the first token for the arg list*/

			for (argcnt = 1; argcnt <= 6; argcnt++, ap++){
				/*
				 *	code to process an argument proper
				 */
			    sawindex  = sawmul = sawsize = 0;
			    {
				switch(val) {

				   default: {
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
				   }	/*end of the default action*/

				   case SIZESPEC: {
				     sizespec:
					sawsize = yylval;
					shift;
					goto disp;
				   }

				   case REG:
				   case REGOP: {
					findreg(regno);
					ap->atype = AREG;
					ap->areg1 = regno;
					break;
				   }	/*end of case REG*/
				    
				   case MUL: {
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
				   }	/*end of case MUL*/

				   case LP: {
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
				   }	/*end of case LP*/

				   case LITOP: {
				      imm:
					shift;
					expr(locxp, val);
					ap->atype = AIMM;
					ap->areg1 = 0;
					ap->xp = locxp;
					goto index;
				   }	/*end of case LITOP*/

				   case MP: {
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
				     }		/*end of case MP*/

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
		}	/*end of case INSTn and IJXXX*/
		
		case IFLOAT:	curlen = 4;	goto floatlist;

		case IDOUBLE: {
			curlen = 8;
		  floatlist:	
			/*
			 *	eat a list of floating point numbers
			 */
			shift;
			if (val == FLTNUM){
				do{
					if (val == CM) shift;
					if (val != FLTNUM) {
					  ERROR("floating number expected");
					}
					dotp->xvalue += curlen;
					if (passno == 2)
					   fwrite(
					     &(((struct exp *)yylval)->doubval.dvalue),
					     1, curlen, txtfil);
					shift;
				} while (val == CM);
			}
			break;
		}	/*end of case IFLOAT and IDOUBLE*/

	    }	/*end of the switch for looking at each reserved word*/

		/*
		 *	If got here, then one has no syntax errors!
		 */
		continue;

		/*
		 *	got here by either requesting to skip to the
		 *	end of this statement, or by erroring out and
		 *	wanting to apply panic mode recovery
		 */
	   errorfix: {
#ifdef DEBUG
		if (debug)
			printf("Discarding tokens from here:\n");
#endif
				while (    (val != NL) 
					&& (val != SEMI) 
					&& (val != PARSEEOF)
				      ){
#ifdef DEBUG
					if (debug)
						printf("****>>>>\t");
#endif
					shift;
				}
				if (val == NL)
					lineno++;
#ifdef DEBUG
		if (debug)
			printf("To here.\n");
#endif
			shift;
		     }

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

/* VARARGS 1*/
yyerror(s, a1, a2)
	char	*s;
{
	FILE		*sink;

#ifdef DEBUG
	sink = stdout;
#else
	sink = stderr;
#endif

	if (anyerrs == 0 && ! silent) 
		fprintf(sink, "Assembler:\n");
	anyerrs++;
	if (silent) return;
	
	fprintf(sink, "\"%s\", line %d: ", dotsname, lineno);
	fprintf(sink, s, a1, a2);
	fprintf(sink, "\n");
}
