/*
 * Copyright (c) 1982 Regents of the University of California.
 * All rights reserved.  The Berkeley software License Agreement
 * specifies the terms and conditions for redistribution.
 */

#ifndef lint
static char sccsid[] = "@(#)asjxxx.c	5.3 (Berkeley) %G%";
#endif not lint

#include	<stdio.h>
#include	"as.h"
#include	"assyms.h"

#define	JBR	0x11
#define	BRW	0x31
#define	JMP	0x17

/*
 *	The number of bytes to add if the jxxx must be "exploded"
 *	into the long form
 */
#define	JBRDELTA	1	/* brb <byte> ==> brw <byte> <byte> */
#define	JXXXDELTA	3	/* brb <byte> ==> brb <byte> brw <byte> <byte> */
#define	JBRJDELTA	d124	/* brb <byte> ==> jmp L^(pc) <byte>*d124 */
#define	JXXXJDELTA	d124+2	/* brb <byte> ==> brb <byte> jmp L^(pc) <byte>*d124 */

int	jbrfsize = JBRDELTA;
int	jxxxfsize = JXXXDELTA;

/*
 *	These variables are filled by asscan.c with the
 *	last name encountered (a pointer buried in the intermediate file),
 *	and the last jxxx symbol table entry encountered.
 */
struct 	symtab	*lastnam;
struct	symtab	*lastjxxx;

initijxxx()
{
	jbrfsize = jxxxJUMP ? JBRJDELTA : JBRDELTA;
	jxxxfsize = jxxxJUMP ? JXXXJDELTA : JXXXDELTA;
	/*
	 *	Note: ifjxxxJUMP is set, then we do NOT do any tunnelling;
	 *	this was too complicated to figure out, and in the first
	 *	version of the assembler, tunnelling proved to be the hardest
	 *	to get to work!
	 */
}
/*
 *	Handle jxxx instructions
 */
ijxout(opcode, ap, nact)
	struct	Opcode	opcode;
	struct	arg	*ap;
	int	nact;
{
	if (passno == 1){
		/*
		 *	READ THIS BEFORE LOOKING AT jxxxfix()
		 *
		 *	Record the jxxx in a special symbol table entry
		 */
		register struct symtab *jumpfrom;

		/*
		 *	We assume the MINIMAL length
		 */
		putins(opcode, ap, nact); 
		jumpfrom = lastjxxx;
		jumpfrom->s_tag = JXACTIVE;
		jumpfrom->s_jxbump = 0;
		if (opcode.Op_popcode == JBR)
			jumpfrom->s_jxfear = jbrfsize;
		else
			jumpfrom->s_jxfear = jxxxfsize;
		if (lastnam == 0)
			yyerror("jxxx destination not a label");
		jumpfrom->s_dest = lastnam;
		jumpfrom->s_type = dotp->e_xtype;	/*only TEXT or DATA*/
		jumpfrom->s_index = dotp-usedot;
#ifdef DEBUG
		jumpfrom->s_name = ITABFETCH(opcode)->i_name;
		jumpfrom->s_jxline = lineno;
#endif
		/*
		 *	value ALWAYS (ALWAYS!!!) indexes the next instruction
		 *	after the jump, even if the jump must be exploded
		 *	(bumped)
		 */
		jumpfrom->s_value = dotp->e_xvalue;
		njxxx++;
	} else {/* pass2, resolve */
		/*
		 *	READ THIS AFTER LOOKING AT jxxxfix()
		 */
		reg	long		oxvalue;
		reg	struct	exp 	*xp; 
		reg	struct	symtab	*tunnel;
		reg	struct	arg	*aplast;
			struct	Opcode	nopcode;

		aplast = ap + nact - 1;
		xp = aplast->a_xp;
		if (lastjxxx->s_tag == JXTUNNEL){
			lastjxxx->s_tag = JXINACTIVE;
			tunnel = lastjxxx->s_dest;
			xp->e_xvalue = tunnel->s_value	/*index of instruction following*/
				    - 3			/* size of brw + word*/
				    + ( ( (tunnel->s_jxfear == jbrfsize) &&
					  (tunnel->s_jxbump == 0))?1:0);
							/*non bumped branch byteis only 2 back*/
		}
		if (lastjxxx->s_jxbump == 0){	/*wasn't bumped, so is short form*/
			putins(opcode, ap, nact);
		} else {
			if (opcode.Op_popcode != JBR){
				/*
				 *	branch reverse conditional byte over
				 *	branch unconditional word
				 */
				oxvalue = xp->e_xvalue;
				xp->e_xvalue = lastjxxx->s_value;
				nopcode = opcode;
				nopcode.Op_popcode ^= 1;
				putins(nopcode, ap, nact);
				xp->e_xvalue = oxvalue;
			}
			nopcode.Op_eopcode = CORE;
			nopcode.Op_popcode = jxxxJUMP ? JMP : BRW;
			putins(nopcode, aplast, 1);
		}
	}
}

jalign(xp, sp)
	register struct exp *xp;
	register struct symtab *sp;
{
	register	int	mask;
#ifdef DEBUG
	static struct strdesc noname;
#endif
	/*
	 *	Problem with .align
	 *
	 *	When the loader constructs an executable file from
	 *	a number of objects, it effectively concatnates
	 *	together all of the text segments from all objects,
	 *	and then all of the data segments.
	 *
	 *	If we do an align by a large value, we can align
	 *	within the a.out this assembly produces, but
	 *	after the loader concatnates, the alignment can't
	 *	be guaranteed if the objects preceding this one
	 *	in the load are also aligned to the same size.
	 *
	 *	Currently, the loader guarantees full word alignment.
	 *	So, ridiculous aligns are caught here and converted
	 *	to a .align (maxalign), if possible, where maxalign
	 *	is set in the command line, and defaults to 2.
	 */
	if (   ( (xp->e_xtype & XTYPE) != XABS)
	    || (xp->e_xvalue < 0)
	    || (xp->e_xvalue > 16)
	    ) {
		yyerror("Illegal `align' argument");
		return;
	}
	if (xp->e_xvalue > maxalign){
		if (passno == 1){
			yywarning(".align %d is NOT preserved by the loader",
				xp->e_xvalue);
			yywarning(".align %d converted to .align %d",
				xp->e_xvalue, maxalign);
		}
		xp->e_xvalue = maxalign;
	}
	flushfield(NBWD/4);
	if (passno == 1) {
		sp->s_tag = JXALIGN;
		sp->s_jxfear = (1 << xp->e_xvalue) - 1;
		sp->s_type = dotp->e_xtype;
		sp->s_index = dotp-usedot;
#ifdef DEBUG
		sp->s_name = (char *)&noname;
		sp->s_jxline = lineno;
#endif
		/*
		 *	We guess that the align will take up at least one
		 *	byte in the code output.  We will correct for this
		 *	initial high guess when we explode (bump) aligns
		 *	when we fix the jxxxes.  We must do this guess
		 *	so that the symbol table is sorted correctly
		 *	and labels declared to fall before the align
		 *	really get their, instead of guessing zero size
		 *	and have the label (incorrectly) fall after the jxxx.
		 *	This is a quirk of our requirement that indices into
		 *	the code stream point to the next byte following
		 *	the logical entry in the symbol table
		 */
		dotp->e_xvalue += 1;
		sp->s_value = dotp->e_xvalue;
		njxxx++;
	} else {
		mask = (1 << xp->e_xvalue) - 1;
		while (dotp->e_xvalue & mask)
			Outb(0);
	}
}

/*
 *	Pass 1.5, resolve jxxx instructions and .align in .text
 */
jxxxfix() 
{
	register struct symtab 	*jumpfrom;
		 struct symtab	**cojumpfrom, *ubjumpfrom;
	register struct symtab 	*dest;
	register struct symtab	*intdest;	/*intermediate dest*/
	register struct symtab	**cointdest, *ubintdest;

	register struct symtab 	*tunnel;
	 	 int 		displ,nchange;
		 int		badjxalign;	/*if jump across an align*/
		 int		stillactives;	/*if still active jxxxes*/
		 int		segno;		/*current segment number*/
		 int		topono;		/*which iteration in the topo sort*/
	register unsigned char	tag;
	/*
	 *	consider each segment in turn...
	 */
	for (segno = 0; segno < NLOC + NLOC; segno++){
	    badjxalign = 0;		/*done on a per segment basis*/
	    /*
	     *	Do a lazy topological sort.
	     */
	    for (topono = 1, nchange = 1; nchange != 0; topono++){
#ifdef lint
		topno = topno;
#endif lint
#ifdef DEBUG
		if (debug)
			printf("\nSegment %d, topo iteration %d\n",
				segno, topono);
#endif
		nchange = 0;
		stillactives = 0;
		/*
		 *	We keep track of one possible tunnel location.
		 *	A tunnel will eventually be an unconditional
		 *	branch to the same place that another jxxx
		 *	will want to branch to.  We will turn a
		 *	branch conditional/unconditional (word) that would
		 *	have to get bumped because its destination is too
		 *	far away, into a branch conditional/unconditional
		 *	byte to the tunnel branch conditional/unconditional.
		 *	Of course, the tunnel must branch to the same place
		 *	as we want to go.
		 */
		tunnel = 0;	/*initially, no tunnel*/
		SEGITERATE(segno, 0, 0, cojumpfrom, jumpfrom, ubjumpfrom, ++){
			tag = jumpfrom->s_tag;
			if (tag <= IGNOREBOUND)
				continue;	/*just an ordinary symbol*/
			if (tag == JXALIGN){
				tunnel = 0;	/*avoid tunneling across a flex alocation*/
				continue;	/*we take care of these later*/
			}
			if (   jumpfrom->s_jxfear == jbrfsize	/*unconditional*/
			    || (   tag == JXINACTIVE		/*inactive bumped*/
				&& (jumpfrom->s_jxbump != 0)
			       )
			   ) tunnel = jumpfrom;
			if (tag != JXACTIVE)
				continue;
			dest = jumpfrom->s_dest;
			if (jumpfrom->s_index != dest->s_index){
				yyerror("Intersegment jxxx");
				continue;
			}
			displ = dest->s_value - jumpfrom->s_value;
			if (displ < MINBYTE || displ > MAXBYTE) {
				/*
				 *	This is an immediate lose!
				 *
				 *	We first attempt to tunnel
				 *	by finding an intervening jump that
				 *	has  the same destination.
				 *	The tunnel is always the first preceeding
				 *	jxxx instruction, so the displacement
				 *	to the tunnel is less than zero, and
				 *	its relative position will be unaffected
				 *	by future jxxx expansions.
				 *
				 *	No tunnels if doing jumps...
				 */
				if (    (!jxxxJUMP)
				     && (jumpfrom->s_jxfear > jbrfsize)
				     && (tunnel) 
				     && (tunnel->s_dest == jumpfrom->s_dest)	
				     && (tunnel->s_index == jumpfrom->s_index)
				     && (tunnel->s_value - jumpfrom->s_value >=
						MINBYTE + jxxxfsize)
				   ) {
						/*
						 *	tunnelling is OK
						 */
						jumpfrom->s_dest = tunnel;
						/*
						 * no bumping needed, this
						 * is now effectively inactive
						 * but must be remembered
						 */
						jumpfrom->s_tag = JXTUNNEL;
#ifdef DEBUG
						if(debug)
						printf("Tunnel from %s from line %d\n",
							FETCHNAME(jumpfrom),
							jumpfrom->s_jxline);
#endif
						continue;
				} else {	/*tunneling not possible*/
					/*
					 *	since this will be turned
					 *	into a bumped jump, we can
					 *	use the unconditional jump
					 *	as a tunnel
					 */
					tunnel = jumpfrom;
					jumpfrom->s_tag = JXNOTYET;
					++nchange;
					continue;
				}
			}	/*end of immediate lose*/
			/*
			 *	Do a forward search for an intervening jxxx
			 */
			if (displ >= 0) {
				SEGITERATE(segno, cojumpfrom + 1,0,cointdest,
						intdest, ubintdest, ++){
					if (intdest == dest)
						break; /* beyond destination */
					if (intdest->s_tag <= JXQUESTIONABLE)
						continue;	/*frozen solid*/
					if (intdest->s_tag == JXALIGN){
						jumpfrom->s_jxoveralign = 1;
						badjxalign++;
					}
					/*
					 *	we assume the worst case
					 *	for unfrozen jxxxxes
					 */
					displ += intdest->s_jxfear;
				}
				if (displ <= MAXBYTE){
					/*
					 *	the worst possible conditions
					 *	can't hurt us, so forget about
					 *	this jump
					 */
					jumpfrom->s_tag = JXINACTIVE;
				} else {
					stillactives++;
				}
			} else {
			/*
			 *	backward search for intervening jxxx
			 */
				SEGITERATE(segno, cojumpfrom - 1,1,cointdest,
				  intdest, ubintdest, --){
					if (intdest == dest)
						break; /* beyond destination */
					if (intdest->s_tag <= JXQUESTIONABLE)
						continue;	/*frozen solid*/
					if (intdest->s_tag == JXALIGN){
						jumpfrom->s_jxoveralign = 1;
						badjxalign++;
					}
					displ -= intdest->s_jxfear; 
				}
				if (displ >= MINBYTE) {
					jumpfrom->s_tag = JXINACTIVE;
				} else {
					stillactives++;
				}
			}	/*end of backwards search*/
		}	/*end of iterating through all symbols in this seg*/

		if (nchange == 0) {
			/*
			 *	Now, if there are still active jxxx entries,
			 *	we are partially deadlocked.  We can leave
			 *	these jxxx entries in their assumed short jump
			 *	form, as all initial displacement calcualtions
			 *	are hanging on unresolved jxxx instructions
			 *	that might explode into a long form, causing
			 *	other jxxxes jumping across the first set of
			 *	jxxxes to explode, etc.
			 *	However, if a jxxx jumps across a .align,
			 *	we assume the worst for the deadlock cycle,
			 *	and resolve all of them towards the long
			 *	jump.
			 *	Currently, the C compiler does not produce
			 *	jumps across aligns, as aligns are only used
			 *	in data segments, or in text segments to align
			 *	functions.
			 */
			if (stillactives){
				SEGITERATE(segno, 0, 0, cojumpfrom, jumpfrom,
				    ubjumpfrom, ++){
					if (jumpfrom->s_tag == JXACTIVE){
						jumpfrom->s_tag =
						  badjxalign?JXNOTYET:JXINACTIVE;
					}
				}
				if (badjxalign){
					jxxxbump(segno, (struct symtab **)0);
				}
			}
			/*
			 *	Handle  all of the .align s
			 */
			SEGITERATE(segno, 0, 0, cojumpfrom, jumpfrom,
			   ubjumpfrom, ++){
			    if (jumpfrom->s_tag == JXALIGN){
				/*
				 *	Predict the true displacement
				 *	needed, irregardless of the
				 *	fact that we guessed 1
				 */
				displ = (jumpfrom->s_value - 1) & (unsigned)jumpfrom->s_jxfear;
				if (displ == 0){	/*no virtual displacement*/
					jumpfrom->s_jxfear = -1;
				} else {
					jumpfrom->s_jxfear = (jumpfrom->s_jxfear + 1) - displ;
					/*
					 *	assert jumpfrom->s_jxfear > 0
					 */
					if (jumpfrom->s_jxfear == 1){
						/*our prediction was correct*/
						continue;
					}
					/*
					 *	assert jumpfrom->s_jxfear > 1
					 */
					jumpfrom->s_jxfear -= 1;	/*correct guess*/
				}
				/*
				 *	assert jumpfrom->s_jxfear = -1, +1...2**n-1
				 */
				jumpfrom->s_tag = JXNOTYET;	/*signal*/
				jxxxbump(segno, cojumpfrom);
				jumpfrom->s_tag = JXINACTIVE;
				/*
				 *	Assert jxfrom->jxvalue indexes the first
				 *	code byte after the added bytes, and
				 *	has n low order zeroes.
				 */
			  }
			}	/*end of walking through each segment*/
	    	}	/*end of no changes */
		else {	/*changes, and still have to try another pass*/
			jxxxbump(segno, (struct symtab **)0);
		}
	   }	/*end of doing the topologic sort*/
	}	/*end of iterating through all segments*/
}	/*end of jxxxfix*/

/*
 *	Go through the symbols in a given segment number,
 *	and see which entries are jxxx entries that have
 *	been logically "exploded" (expanded), but for which
 *	the value of textually following symbols has not been
 *	increased
 */

jxxxbump(segno, starthint)
	int	segno;
	struct	symtab **starthint;
{
	register	struct	symtab	**cosp, *sp;
	register	struct	symtab		*ub;
	register	int	cum_bump;
	register	unsigned	char	tag;

	cum_bump = 0;
	SEGITERATE(segno, starthint, 0, cosp, sp, ub, ++){
		tag = sp->s_tag;
		if (tag == JXNOTYET){
#ifdef DEBUG
			if (debug){
			if (sp->s_dest != 0)
				printf("Explode jump to %s on line %d\n",
					FETCHNAME(sp->s_dest), sp->s_jxline);
			else
				printf("Explode an align! on line %d\n",
					sp->s_jxline);
			}
#endif
			sp->s_tag = JXINACTIVE;
			sp->s_jxbump = 1;
			cum_bump += sp->s_jxfear;
		}
		/*
		 *	Only bump labels and jxxxes. Ignored entries can
		 *	be incremented, as they are thrown away later on.
		 *	Stabds are given their final value in the second 
		 *	pass.
		 */
		if (tag >= OKTOBUMP)	/*only bump labels and jxxxes and floating stabs*/
			sp->s_value += cum_bump;
	}
	usedot[segno].e_xvalue += cum_bump;
}
