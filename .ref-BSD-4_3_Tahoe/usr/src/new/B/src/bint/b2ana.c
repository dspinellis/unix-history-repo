/* Copyright (c) Stichting Mathematisch Centrum, Amsterdam, 1985. */

/* $Header: b2ana.c,v 1.4 85/08/22 16:54:05 timo Exp $ */

/* Prepare for code generation -- find out which tags are targets */

#include "b.h"
#include "b1obj.h"
#include "b2nod.h"
#include "b2gen.h" /* Must be after b2nod.h */
#include "b3err.h"
#include "b3env.h"
#include "b3sou.h" /* For get_pname */


Visible int nextvarnumber; /* Counts local targets (including formals) */

Visible value formals, locals, globals, mysteries, refinements;


Visible value *setup(t) parsetree t; {
	typenode n= Nodetype(t);
	bool in_prmnv= !Unit(n);
	nextvarnumber= 0;
	formals= mk_elt();
	mysteries= mk_elt();
	if (in_prmnv) {
		globals= copy(prmnv->tab);
		locals= Vnil;
		refinements= mk_elt();
		return Command(n) ? &globals : Pnil;
	} else {
		globals= mk_elt();
		locals= mk_elt();
		refinements=
		    copy(*Branch(t, n == HOW_TO ? HOW_R_NAMES : FPR_R_NAMES));
		unit_context(t);
		return &locals;
	}
}

Hidden Procedure unit_context(t) parsetree t; {
	cntxt= In_unit;
	release(uname); uname= get_pname(t);
}

Visible Procedure cleanup() {
	release(formals);
	release(locals);
	release(globals);
	release(mysteries);
	release(refinements);
}

/* ********************************************************************	*/

/* Analyze parse tree, finding the targets and formal parameters.
   Formal parameters of HOW'TO's are of course found in the unit heading.
   Formal parameters of YIELDs and TESTs are treated as local targets.
   Global targets are also easily found: they are mentioned in a SHARE command.
   Local targets appear on their own or in collateral forms after PUT IN,
   DRAW or CHOOSE, or as bound tags after FOR, SOME, EACH or NO.
   Note that DELETE x, REMOVE e FROM x, or PUT e IN x[k] (etc.) don't
   introduce local targets, because in all these cases x must have been
   initialized first.  This speeds up our task of finding targets,
   since we don't have to visit all nodes: only nodes that may contain
   commands or tests, and the positions mentioned here, need be visited.
   (And of course unit headings).
   We don't have to look for refinements since these are already known
   from the unit heading.
 */

Hidden Procedure a_tag(name, targs) value name; value *targs; {
	value *aa; int varnumber;
	if (locals != Vnil && envassoc(locals, name)) return;
	if (envassoc(globals, name)) return;
	if (envassoc(formals, name)) return;
	if (envassoc(refinements, name)) {
		if (targs != &mysteries)
			fixerr(MESS(4600, "a refinement may not be used as a target"));
		return;
	}
	if (aa= envassoc(mysteries, name)) {
		if (targs == &mysteries) return;
		varnumber= SmallIntVal(*aa);
		e_delete(&mysteries, name);
	}
	else if (targs != &globals) varnumber= nextvarnumber++;
	else varnumber= 0;
	e_replace(MkSmallInt(varnumber), targs, name);
}

Hidden Procedure a_fpr_formals(t) parsetree t; {
	typenode n= Nodetype(t);
	switch (n) {
	case TAG:
		break;
	case MONF: case MONPRD:
		analyze(*Branch(t, MON_RIGHT), &locals);
		break;
	case DYAF: case DYAPRD:
		analyze(*Branch(t, DYA_LEFT), &locals);
		analyze(*Branch(t, DYA_RIGHT), &locals);
		break;
	default: syserr(MESS(4601, "a_fpr_formals"));
	}
}

Visible Procedure analyze(t, targs) parsetree t; value *targs; {
	typenode nt; string s; char c; int n, k, len; value v;
	if (!Is_node(t) || !still_ok) return;
	nt= Nodetype(t);
	if (nt < 0 || nt >= NTYPES) syserr(MESS(4602, "analyze bad tree"));
	s= gentab[nt];
	if (s == NULL) return;
	n= First_fieldnr;
	while ((c= *s++) != '\0' && still_ok) {
		switch (c) {
		case '0':
		case '1':
		case '2':
		case '3':
		case '4':
		case '5':
		case '6':
		case '7':
		case '8':
		case '9':
			n= (c - '0') + First_fieldnr;
			break;
		case 'c':
			v= *Branch(t, n);
			if (v != Vnil) {
				len= Nfields(v);
				for (k= 0; k < len; ++k)
					analyze(*Field(v, k), targs);
			}
			++n;
			break;
		case '#':
			curlino= *Branch(t, n);
			/* Fall through */
		case 'l':
		case 'v':
			++n;
			break;
		case 'm':
			analyze(*Branch(t, n), &mysteries);
			++n;
			break;
		case 'g':
			analyze(*Branch(t, n), &globals);
			++n;
			break;
		case '!':
			analyze(*Branch(t, n),
				locals != Vnil ? &locals : &globals);
			++n;
			break;
		case 'x':
			curline= *Branch(t, n);
			/* Fall through */
		case 'a':
		case 'u':	
			analyze(*Branch(t, n), targs);
			++n;
			break;
		case 't':
			analyze(*Branch(t, n), Pnil);
			++n;
			break;
		case 'f':
			a_fpr_formals(*Branch(t, n));
			++n;
			break;
		case 'h':
			v= *Branch(t, n);
			if (v != Vnil && Is_text(v))
				a_tag(v, &formals);
			else
				analyze(v, &formals);
			++n;
			break;
		case '=':
			*Branch(t, n)= MkSmallInt(nextvarnumber);
			++n;
			break;
		case 'T':
			if (targs != Pnil)
				a_tag((value)*Branch(t, TAG_NAME), targs);
			break;
		}
	}
}

/* ********************************************************************	*/

/* Table describing the actions of the fixer for each node type */


/*
	LIST OF CODES AND THEIR MEANING

	char	fix		n?	analyze

	0-9			n= c-'0'

	#	set curlino	++n	set curlino
	=			++n	set to nextvarnum
	!	locate		++n	analyze; force targs= &local
	a	locate		++n	analyze
	c	collateral	++n	analyze collateral
	f	fpr_formals	++n	a_fpr_formals
	g			++n	global
	h			++n	how'to formal
	l	locate		++n
	m	actual param	++n	mystery
	t	test		++n	analyze; set targs= 0
	u	unit		++n	analyze
	v	evaluate	++n
	x	execute		++n	analyze

	?	special code for UNPARSED
	C	special code for comparison
	D	special code for DYAF
	E	special code for DYAPRD
	G	jumpto(l1)
	H	here(&l1)
	I	if (*Branch(t, n) != NilTree) jump2here(t)
	J	jump2here(t)
	K	hold(&st)
	L	let_go(&st)
	M	special code for MONF
	N	special code for MONPRD
	R	if (!reachable()) error("command cannot be reached")
	S	jumpto(Stop)
	T	special code for TAG
	U	special code for user-defined-command
	V	visit(t)
	W	visit2(t, seterr(1))
	X	visit(t) or lvisit(t) depending on flag
	Y	special code for YIELD/TEST
	Z	special code for refinement
	 
*/


Visible string gentab[]= {

	/* HOW_TO */ "1h3xSu6=",
	/* YIELD */ "2fV4xYu7=",
	/* TEST */ "2fV4xYu7=",
	/* REFINEMENT */ "H2xZSu",

	/* Commands */

	/* SUITE */ "#RVx3x",
	/* PUT */ "vaV",
	/* INSERT */ "vlV",
	/* REMOVE */ "vlV",
	/* CHOOSE */ "avV",
	/* DRAW */ "aV",
	/* SET_RANDOM */ "vV",
	/* DELETE */ "lV",
	/* CHECK */ "tV",
	/* SHARE */ "g",

	/* WRITE */ "1vV",
	/* READ */ "avV",
	/* READ_RAW */ "aV",

	/* IF */ "tV2xJ",
	/* WHILE */ "HtV2xGJ",
	/* FOR */ "avHV3xGJ",

	/* SELECT */ "1x",
	/* TEST_SUITE */ "#tW3xKIxL",
	/* ELSE */ "#2x",

	/* QUIT */ "VS",
	/* RETURN */ "vVS",
	/* REPORT */ "tVS",
	/* SUCCEED */ "VS",
	/* FAIL */ "VS",

	/* USER_COMMAND */ "1mUV",
	/* EXTENDED_COMMAND */ "1cV",

	/* Expressions, targets, tests */

	/* TAG */ "T",
	/* COMPOUND */ "a",

	/* Expressions, targets */

	/* COLLATERAL */ "cX",
	/* SELECTION */ "lvX",
	/* BEHEAD */ "lvX",
	/* CURTAIL */ "lvX",

	/* Expressions, tests */

	/* UNPARSED */ "?",

	/* Expressions */

	/* MONF */ "M1vV",
	/* DYAF */ "Dv2vV",
	/* NUMBER */ "V",
	/* TEXT_DIS */ "1v",
	/* TEXT_LIT */ "1vV",
	/* TEXT_CONV */ "vvV",
	/* ELT_DIS */ "V",
	/* LIST_DIS */ "cV",
	/* RANGE_DIS */ "vvV",
	/* TAB_DIS */ "cV",

	/* Tests */

	/* AND */ "tVtJ",
	/* OR */ "tVtJ",
	/* NOT */ "tV",
	/* SOME_IN */ "!vHVtGJ",
	/* EACH_IN */ "!vHVtGJ",
	/* NO_IN */ "!vHVtGJ",
	/* SOME_PARSING */ "!vHVtGJ",
	/* EACH_PARSING */ "!vHVtGJ",
	/* NO_PARSING */ "!vHVtGJ",
	/* MONPRD */ "N1vV",
	/* DYAPRD */ "Ev2vV",
	/* LESS_THAN */ "vvVC",
	/* AT_MOST */ "vvVC",
	/* GREATER_THAN */ "vvVC",
	/* AT_LEAST */ "vvVC",
	/* EQUAL */ "vvVC",
	/* UNEQUAL */ "vvVC",
	/* Nonode */ "",

	/* TAGformal */ "T",
	/* TAGlocal */ "T",
	/* TAGglobal */ "T",
	/* TAGmystery */ "T",
	/* TAGrefinement */ "T",
	/* TAGzerfun */ "T",
	/* TAGzerprd */ "T",

	/* ACTUAL */ "1mm",
	/* FORMAL */ "1hh",
};
