/* Copyright (c) 1979 Regents of the University of California */
#include	<stdio.h>
#include	"as.h"
#include	"assyms.h"

#define JBR 0x11
#define BRW 0x31

/*
 *	The number of bytes to add if the jxxx must be "exploded"
 *	into the long form
 */
#define	JBRFSIZE	1	/*goes to brw*/
#define JXXXFSIZE	3	/*goes to brb, brw <byte> <byte> */

/*
 *	These variables are filled by asscan.c with the
 *	last name encountered (a pointer buried in the intermediate file),
 *	and the last jxxx symbol table entry encountered.
 */
struct 	symtab	*lastnam;
struct	symtab	*lastjxxx;

/*
 *	Handle jxxx instructions
 */
ijxout(op,ap,nact)
	struct arg *ap;
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
		putins(op,ap,nact); 
		jumpfrom = lastjxxx;
		jumpfrom->tag = JXACTIVE;
		jumpfrom->jxbump = 0;
		if (op == JBR)
			jumpfrom->jxfear = JBRFSIZE;
		else
			jumpfrom->jxfear = JXXXFSIZE;
#ifdef DJXXX
		jumpfrom->jxline = lineno;
#endif
		if (lastnam == 0)
			yyerror("jxxx destination not a label");
		jumpfrom->dest = lastnam;
		jumpfrom->type = dotp->xtype;	/*only TEXT or DATA*/
		jumpfrom->index = dotp-usedot;
		/*
		 *	value ALWAYS (ALWAYS!!!) indexes the next instruction
		 *	after the jump, even in the jump must be exploded
		 *	(bumped)
		 */
		jumpfrom->value = dotp->xvalue;
		njxxx++;
	} else {/* pass2, resolve */
		/*
		 *	READ THIS AFTER LOOKING AT jxxxfix()
		 */
		register long		oxvalue;
		register struct	exp 	*xp; 
		register struct symtab	*tunnel;
		register struct arg	*aplast;

		aplast = ap + nact - 1;
		xp = aplast->xp;
		if (lastjxxx->tag == JXTUNNEL){
			lastjxxx->tag = JXINACTIVE;
			tunnel = lastjxxx->dest;
			xp->xvalue = tunnel->value	/*index of instruction following*/
				    - 3			/* size of brw + word*/
				    + ( ( (tunnel->jxfear == JBRFSIZE) &&
					  (tunnel->jxbump == 0))?1:0);
							/*non bumped branch byteis only 2 back*/
		}
		if (lastjxxx->jxbump == 0){	/*wasn't bumped, so is short form*/
			putins(op, ap, nact);
		} else {
			if (op != JBR){	/*branch reverse conditional byte over 
					  branch unconditional word*/
				oxvalue = xp->xvalue;
				xp->xvalue = lastjxxx->value;
				putins(op^1, ap, nact);
				xp->xvalue = oxvalue;
			}
			putins(BRW, aplast, 1);
		}
	}
}	/*end of ijxout*/

jalign(xp, sp)
	register struct exp *xp;
	register struct symtab *sp;
{
	register	int	mask;
	if (xp->xtype != XABS || xp->xvalue < 0 || xp->xvalue > 16) {
		yyerror("Illegal `align' argument");
		return;
	}
	flushfield(NBPW/4);
	if (passno == 1) {
		sp->tag = JXALIGN;
		sp->jxfear = (1 << xp->xvalue) - 1;
#ifdef DJXXX
		sp->jxline = lineno;
#endif
		sp->type = dotp->xtype;
		sp->index = dotp-usedot;
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
		dotp->xvalue += 1;
		sp->value = dotp->xvalue;
		njxxx++;
	} else {
		mask = (1 << xp->xvalue) - 1;
		while (dotp->xvalue & mask){
			outb(0);
		}
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
			tag = jumpfrom->tag;
			if (tag <= IGNOREBOUND)
				continue;	/*just an ordinary symbol*/
			if (tag == JXALIGN){
				tunnel = 0;	/*avoid tunneling across a flex alocation*/
				continue;	/*we take care of these later*/
			}
			if (   jumpfrom->jxfear == JBRFSIZE	/*unconditional*/
			    || (   tag == JXINACTIVE		/*inactive bumped*/
				&& (jumpfrom->jxbump != 0)
			       )
			   ) tunnel = jumpfrom;
			if (tag != JXACTIVE)
				continue;
			dest = jumpfrom->dest;
			if (jumpfrom->index != dest->index){
				yyerror("Intersegment jxxx");
				continue;
			}
			displ = dest->value - jumpfrom->value;
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
				 */
				if (    (jumpfrom->jxfear > JBRFSIZE)
				     && (tunnel) 
				     && (tunnel->dest == jumpfrom->dest)	
				     && (tunnel->index == jumpfrom->index)
				     && (tunnel->value - jumpfrom->value >=
						MINBYTE + JXXXFSIZE)
				   ) {
						/*
						 *	tunnelling is OK
						 */
						jumpfrom->dest = tunnel;
						/*
						 * no bumping needed, this
						 * is now effectively inactive
						 * but must be remembered
						 */
						jumpfrom->tag = JXTUNNEL;
#ifdef DEBUG
						if(debug)
						printf("Tunnel from %s from line %d\n",
							jumpfrom->name, lineno);
#endif
#ifdef METRIC
						jxxxtunnel++;
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
					jumpfrom->tag = JXNOTYET;
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
					if (intdest->value > dest->value) 
						break; /* beyond destination */
					if (intdest->tag <= JXQUESTIONABLE)
						continue;	/*frozen solid*/
					if (intdest->tag == JXALIGN){
						jumpfrom->jxoveralign = 1;
						badjxalign++;
					}
					/*
					 *	we assume the worst case
					 *	for unfrozen jxxxxes
					 */
					displ += intdest->jxfear;
				}
				if (displ <= MAXBYTE){
					/*
					 *	the worst possible conditions
					 *	can't hurt us, so forget about
					 *	this jump
					 */
					jumpfrom->tag = JXINACTIVE;
				} else {
					stillactives++;
				}
			} else {
			/*
			 *	backward search for intervening jxxx
			 */
				SEGITERATE(segno, cojumpfrom - 1,1,cointdest,
				  intdest, ubintdest, --){
					if (intdest->value <= dest->value) 
						break; /* beyond destination */
					if (intdest->tag <= JXQUESTIONABLE)
						continue;	/*frozen solid*/
					if (intdest->tag == JXALIGN){
						jumpfrom->jxoveralign = 1;
						badjxalign++;
					}
					displ -= intdest->jxfear; 
				}
				if (displ >= MINBYTE) {
					jumpfrom->tag = JXINACTIVE;
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
#ifdef METRIC
				jxdeadlock++;
#endif
				SEGITERATE(segno, 0, 0, cojumpfrom, jumpfrom,
				    ubjumpfrom, ++){
					if (jumpfrom->tag == JXACTIVE){
						jumpfrom->tag =
						  badjxalign?JXNOTYET:JXINACTIVE;
					}
				}
				if (badjxalign){
					jxxxbump(segno, 0);
#ifdef METRIC
					nbadjxsegs++;
#endif
				}
			}
			/*
			 *	Handle  all of the .align s
			 */
			SEGITERATE(segno, 0, 0, cojumpfrom, jumpfrom,
			   ubjumpfrom, ++){
			    if (jumpfrom->tag == JXALIGN){
				/*
				 *	Predict the true displacement
				 *	needed, irregardless of the
				 *	fact that we guessed 1
				 */
				displ = (jumpfrom->value - 1) & (unsigned)jumpfrom->jxfear;
				if (displ == 0){	/*no virtual displacement*/
					jumpfrom->jxfear = -1;
				} else {
					jumpfrom->jxfear = (jumpfrom->jxfear + 1) - displ;
					/*
					 *	assert jumpfrom->jxfear > 0
					 */
					if (jumpfrom->jxfear == 1){
						/*our prediction was correct*/
						continue;
					}
					/*
					 *	assert jumpfrom->jxfear > 1
					 */
					jumpfrom->jxfear -= 1;	/*correct guess*/
				}
				/*
				 *	assert jumpfrom->jxfear = -1, +1...2**n-1
				 */
				jumpfrom->tag = JXNOTYET;	/*signal*/
				jxxxbump(segno, cojumpfrom);
				jumpfrom->tag = JXINACTIVE;
				/*
				 *	Assert jxfrom->jxvalue indexes the first
				 *	code byte after the added bytes, and
				 *	has n low order zeroes.
				 */
			  }
			}	/*end of walking through each segment*/
	    	}	/*end of no changes */
		else {	/*changes, and still have to try another pass*/
			jxxxbump(segno, 0);
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

#ifdef METRIC
	jxxxiterate++;
#endif
	cum_bump = 0;
	SEGITERATE(segno, starthint, 0, cosp, sp, ub, ++){
		tag = sp->tag;
		if (tag == JXNOTYET){
#ifdef DEBUG
			if (debug){
			if (sp->dest != 0)
				printf("Explode jump to %s on line %d\n",
					sp->dest->name, lineno);
			else
				printf("Explode an align!\n");
			}
#endif
			sp->tag = JXINACTIVE;
			sp->jxbump = 1;
			cum_bump += sp->jxfear;
		}
		/*
		 *	Only bump labels and jxxxes. Ignored entries can
		 *	be incremented, as they are thrown away later on.
		 *	Stabds are given their final value in the second 
		 *	pass.
		 */
		if (tag >= OKTOBUMP)	/*only bump labels and jxxxes and floating stabs*/
			sp->value += cum_bump;
	}
	usedot[segno].xvalue += cum_bump;
}
