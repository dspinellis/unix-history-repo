/* Copyright (c) 1979 Regents of the University of California */

#ifndef lint
static	char sccsid[] = "@(#)pas.y 8.1 6/6/93";
#endif

#include "whoami.h"
#include "0.h"
#include "tree_ty.h"		/* must be included for yy.h */
#include "yy.h"
#include "tree.h"

#ifdef PI
#define	lineof(l)	l
#define	line2of(l)	l
#endif

extern yyerrflag;

union semstack yyval;
union semstack *yypv;
yyactr(__np__){
	register union semstack *yyYpv;
	register struct tnode *p, *q;
	yyYpv = yypv;

switch(__np__){

case 1: funcend(yyYpv[1].nl_entry, yyYpv[3].tr_entry, lineof(yyYpv[4].i_entry)); break;
case 2: segend(); break;
case 3: yyval.nl_entry = funcbody(funchdr(tree5(T_PROG, lineof(yyYpv[1].i_entry), yyYpv[2].tr_entry, fixlist(yyYpv[4].tr_entry), TR_NIL))); break;
case 4: yyval.nl_entry = funcbody(funchdr(tree5(T_PROG, lineof(yyYpv[1].i_entry),  yyYpv[2].tr_entry, TR_NIL, TR_NIL))); break;
case 5: {
			yyPerror("Malformed program statement", PPROG);
			/*
			 * Should make a program statement
			 * with "input" and "output" here.
			 */
			yyval.nl_entry = funcbody(funchdr(tree5(T_PROG, lineof(yyYpv[1].i_entry), TR_NIL, TR_NIL, TR_NIL)));
		  } break;
case 6: {
			yyval.tr_entry = tree3(T_BSTL, lineof(yyYpv[1].i_entry), fixlist(yyYpv[2].tr_entry));
			if (yyYpv[3].i_entry < 0)
				brerror(yyYpv[1].i_entry, "begin");
		  } break;
case 7: trfree(); break;
case 8: {
			constend(), typeend(), varend(), trfree();
			yyPerror("Malformed declaration", PDECL);
		  } break;
case 9: trfree(); break;
case 11: constend(); break;
case 12: typeend(); break;
case 13: varend(); break;
case 15: label(fixlist(yyYpv[2].tr_entry), lineof(yyYpv[1].i_entry)); break;
case 16: yyval.tr_entry = newlist(yyYpv[1].i_entry == NIL ? TR_NIL :
					(struct tnode *) *hash(yyYpv[1].cptr, 1)); break;
case 17: yyval.tr_entry = addlist(yyYpv[1].tr_entry, yyYpv[3].i_entry == NIL ?
				TR_NIL : (struct tnode *) *hash(yyYpv[3].cptr, 1)); break;
case 18: constbeg(yyYpv[1].i_entry, lineof(yyYpv[1].i_entry)),
		  constant(lineof(yyYpv[3].i_entry), yyYpv[2].cptr, yyYpv[4].tr_entry); break;
case 19: constant(lineof(yyYpv[3].i_entry), yyYpv[2].cptr, yyYpv[4].tr_entry); break;
case 20: {
			constbeg(yyYpv[1].i_entry);
Cerror:
			yyPerror("Malformed const declaration", PDECL);
		  } break;
case 21: goto Cerror; break;
case 22: typebeg(yyYpv[1].i_entry, line2of(yyYpv[2].i_entry)), type(lineof(yyYpv[3].i_entry), yyYpv[2].cptr, yyYpv[4].tr_entry); break;
case 23: type(lineof(yyYpv[3].i_entry), yyYpv[2].cptr, yyYpv[4].tr_entry); break;
case 24: {
			typebeg(yyYpv[1].i_entry, line2of(yyYpv[1].i_entry));
Terror:
			yyPerror("Malformed type declaration", PDECL);
		  } break;
case 25: goto Terror; break;
case 26: varbeg(yyYpv[1].i_entry, line2of(yyYpv[3].i_entry)), var(lineof(yyYpv[3].i_entry), fixlist(yyYpv[2].tr_entry), yyYpv[4].tr_entry); break;
case 27: var(lineof(yyYpv[3].i_entry), fixlist(yyYpv[2].tr_entry), yyYpv[4].tr_entry); break;
case 28: {
			varbeg(yyYpv[1].i_entry, line2of(yyYpv[1].i_entry));
Verror:
			yyPerror("Malformed var declaration", PDECL);
		  } break;
case 29: goto Verror; break;
case 30: funcfwd(yyYpv[1].nl_entry); break;
case 31: (void) funcext(yyYpv[1].nl_entry); break;
case 32: funcend(yyYpv[1].nl_entry, yyYpv[3].tr_entry, lineof(yyYpv[4].i_entry)); break;
case 34: (void) funcbody(yyYpv[1].nl_entry); break;
case 35: yyval.nl_entry = funchdr(tree5(yyYpv[1].i_entry, lineof(yyYpv[5].i_entry),
				yyYpv[2].tr_entry, yyYpv[3].tr_entry, yyYpv[4].tr_entry)); break;
case 36: yyval.i_entry = T_PDEC; break;
case 37: yyval.i_entry = T_FDEC; break;
case 38: yyval.tr_entry = fixlist(yyYpv[2].tr_entry); break;
case 39: yyval.tr_entry = TR_NIL; break;
case 40: yyval.tr_entry = tree3(T_PVAL, (int) fixlist(yyYpv[1].tr_entry), yyYpv[3].tr_entry); break;
case 41: yyval.tr_entry = tree3(T_PVAR, (int) fixlist(yyYpv[2].tr_entry), yyYpv[4].tr_entry); break;
case 42: yyval.tr_entry = tree5(T_PFUNC, (int) fixlist(yyYpv[2].tr_entry),
				yyYpv[4].tr_entry, yyYpv[3].tr_entry, 
				(struct tnode *) lineof(yyYpv[1].i_entry)); break;
case 43: yyval.tr_entry = tree5(T_PPROC, (int) fixlist(yyYpv[2].tr_entry),
				yyYpv[4].tr_entry, yyYpv[3].tr_entry, 
				(struct tnode *) lineof(yyYpv[1].i_entry)); break;
case 44: yyval = yyYpv[2]; break;
case 45: yyval.tr_entry = TR_NIL; break;
case 46: yyval.tr_entry = newlist(yyYpv[1].tr_entry); break;
case 47: yyval.tr_entry = addlist(yyYpv[1].tr_entry, yyYpv[3].tr_entry); break;
case 48: yyval.tr_entry = tree2(T_CSTRNG, yyYpv[1].i_entry); break;
case 50: yyval.tr_entry = tree2(T_PLUSC, yyYpv[2].i_entry); break;
case 51: yyval.tr_entry = tree2(T_MINUSC, yyYpv[2].i_entry); break;
case 52: yyval.tr_entry = tree2(T_ID, yyYpv[1].i_entry); break;
case 53: yyval.tr_entry = tree2(T_CINT, yyYpv[1].i_entry); break;
case 54: yyval.tr_entry = tree2(T_CBINT, yyYpv[1].i_entry); break;
case 55: yyval.tr_entry = tree2(T_CFINT, yyYpv[1].i_entry); break;
case 56: yyval.tr_entry = newlist(yyYpv[1].tr_entry); break;
case 57: yyval.tr_entry = addlist(yyYpv[1].tr_entry, yyYpv[3].tr_entry); break;
case 59: yyval.tr_entry = tree3(T_TYPTR, lineof(yyYpv[1].i_entry), tree2(T_ID,
								yyYpv[2].i_entry)); break;
case 61: yyval.tr_entry = tree3(T_TYPACK, lineof(yyYpv[1].i_entry), yyYpv[2].tr_entry); break;
case 63: yyval.tr_entry = tree3(T_TYSCAL, lineof(yyYpv[1].i_entry), fixlist(yyYpv[2].tr_entry)); break;
case 64: yyval.tr_entry = tree4(T_TYRANG, lineof(yyYpv[2].i_entry), yyYpv[1].tr_entry,
				yyYpv[3].tr_entry); break;
case 65: yyval.tr_entry = tree4(T_TYARY, lineof(yyYpv[1].i_entry),
					fixlist(yyYpv[3].tr_entry), yyYpv[6].tr_entry); break;
case 66: yyval.tr_entry = tree3(T_TYFILE, lineof(yyYpv[1].i_entry), yyYpv[3].tr_entry); break;
case 67: yyval.tr_entry = tree3(T_TYSET, lineof(yyYpv[1].i_entry), yyYpv[3].tr_entry); break;
case 68: {
			yyval.tr_entry = setuptyrec( lineof( yyYpv[1].i_entry ) , yyYpv[2].tr_entry);
			if (yyYpv[3].i_entry < 0)
				brerror(yyYpv[1].i_entry, "record");
		  } break;
case 69: yyval.tr_entry = newlist(yyYpv[1].tr_entry); break;
case 70: yyval.tr_entry = addlist(yyYpv[1].tr_entry, yyYpv[3].tr_entry); break;
case 71: yyval.tr_entry = tree4(T_FLDLST, lineof(NIL), 
				fixlist(yyYpv[1].tr_entry), yyYpv[2].tr_entry); break;
case 72: yyval.tr_entry = newlist(yyYpv[1].tr_entry); break;
case 73: yyval.tr_entry = addlist(yyYpv[1].tr_entry, yyYpv[3].tr_entry); break;
case 74: yyPerror("Malformed record declaration", PDECL); break;
case 75: yyval.tr_entry = TR_NIL; break;
case 76: yyval.tr_entry = tree4(T_RFIELD, lineof(yyYpv[2].i_entry),
				fixlist(yyYpv[1].tr_entry), yyYpv[3].tr_entry); break;
case 77: yyval.tr_entry = TR_NIL; break;
case 78: yyval.tr_entry = tree5(T_TYVARPT, lineof(yyYpv[1].i_entry), TR_NIL, 
				yyYpv[2].tr_entry, fixlist(yyYpv[4].tr_entry)); break;
case 79: yyval.tr_entry = tree5(T_TYVARPT, lineof(yyYpv[1].i_entry),
				yyYpv[2].tr_entry, yyYpv[4].tr_entry,
					fixlist(yyYpv[6].tr_entry)); break;
case 80: yyval.tr_entry = newlist(yyYpv[1].tr_entry); break;
case 81: yyval.tr_entry = addlist(yyYpv[1].tr_entry, yyYpv[3].tr_entry); break;
case 82: yyPerror("Malformed record declaration", PDECL); break;
case 83: yyval.tr_entry = TR_NIL; break;
case 84: yyval.tr_entry = tree4(T_TYVARNT,lineof(yyYpv[2].i_entry), fixlist(yyYpv[1].tr_entry),
				yyYpv[4].tr_entry); break;
case 85: yyval.tr_entry = newlist(yyYpv[1].tr_entry); break;
case 86: {
			if ((p = yyYpv[1].tr_entry) != TR_NIL && (q = p->list_node.list)->tag == T_IFX) {
				q->tag = T_IFEL;
				q->if_node.else_stmnt = yyYpv[2].tr_entry;
			} else
				yyval.tr_entry= addlist(yyYpv[1].tr_entry, yyYpv[2].tr_entry);
		  } break;
case 87: if ((q = yyYpv[1].tr_entry) != TR_NIL && (p = q->list_node.list) != TR_NIL && p->tag == T_IF) {
			if (yychar < 0)
				yychar = yylex();
			if (yyshifts >= 2 && yychar == YELSE) {
				recovered();
				copy((char *) (&Y), (char *) (&OY), sizeof Y);
				yerror("Deleted ';' before keyword else");
				yychar = yylex();
				p->tag = T_IFX;
			}
		  } break;
case 88: yyval.tr_entry = newlist(yyYpv[1].tr_entry); break;
case 89: yyval.tr_entry = addlist(yyYpv[1].tr_entry, yyYpv[3].tr_entry); break;
case 90: {
			yyval.tr_entry = TR_NIL;
Kerror:
			yyPerror("Malformed statement in case", PSTAT);
		  } break;
case 91: goto Kerror; break;
case 92: yyval.tr_entry = tree4(T_CSTAT, lineof(yyYpv[2].i_entry),
				fixlist(yyYpv[1].tr_entry), yyYpv[3].tr_entry); break;
case 93: yyval.tr_entry = tree4(T_CSTAT, lineof(yyYpv[1].i_entry), TR_NIL,
					yyYpv[2].tr_entry); break;
case 94: yyval.tr_entry = TR_NIL; break;
case 95: yyval.tr_entry = TR_NIL; break;
case 96: yyval.tr_entry = tree4(T_LABEL, lineof(yyYpv[2].i_entry),
				yyYpv[1].tr_entry == TR_NIL ? TR_NIL :
					    (struct tnode *) *hash(yyYpv[1].cptr, 1), yyYpv[3].tr_entry); break;
case 97: yyval.tr_entry = tree4(T_PCALL, lineof(yyline), yyYpv[1].tr_entry,
						TR_NIL); break;
case 98: yyval.tr_entry = tree4(T_PCALL, lineof(yyYpv[2].i_entry), yyYpv[1].tr_entry,
					fixlist(yyYpv[3].tr_entry)); break;
case 99: goto NSerror; break;
case 101: {
			yyval.tr_entry = tree3(T_BLOCK, lineof(yyYpv[1].i_entry),
						fixlist(yyYpv[2].tr_entry));
			if (yyYpv[3].i_entry < 0)
				brerror(yyYpv[1].i_entry, "begin");
		  } break;
case 102: {
			yyval.tr_entry = tree4(T_CASE, lineof(yyYpv[1].i_entry),
					yyYpv[2].tr_entry, fixlist(yyYpv[4].tr_entry));
			if (yyYpv[5].i_entry < 0)
				brerror(yyYpv[1].i_entry, "case");
		  } break;
case 103: yyval.tr_entry = tree4(T_WITH, lineof(yyYpv[1].i_entry),
				fixlist(yyYpv[2].tr_entry), yyYpv[4].tr_entry); break;
case 104: yyval.tr_entry = tree4(T_WHILE, lineof(yyYpv[1].i_entry), yyYpv[2].tr_entry,
					yyYpv[4].tr_entry); break;
case 105: yyval.tr_entry = tree4(T_REPEAT, lineof(yyYpv[3].i_entry),
				fixlist(yyYpv[2].tr_entry), yyYpv[4].tr_entry); break;
case 106: yyval.tr_entry = tree5(T_FORU, lineof(yyYpv[1].i_entry), yyYpv[2].tr_entry,
				yyYpv[4].tr_entry, yyYpv[6].tr_entry); break;
case 107: yyval.tr_entry = tree5(T_FORD, lineof(yyYpv[1].i_entry), yyYpv[2].tr_entry,
				yyYpv[4].tr_entry, yyYpv[6].tr_entry); break;
case 108: yyval.tr_entry = tree3(T_GOTO, lineof(yyYpv[1].i_entry),
				(struct tnode *) *hash(yyYpv[2].cptr, 1)); break;
case 109: yyval.tr_entry = tree5(T_IF, lineof(yyYpv[1].i_entry), yyYpv[2].tr_entry,
				yyYpv[4].tr_entry, TR_NIL); break;
case 110: yyval.tr_entry = tree5(T_IFEL, lineof(yyYpv[1].i_entry), yyYpv[2].tr_entry,
					yyYpv[4].tr_entry, yyYpv[6].tr_entry); break;
case 111: {
NSerror:
			yyval.tr_entry = TR_NIL;
			yyPerror("Malformed statement", PSTAT);
		  } break;
case 112: yyval.tr_entry = tree4(T_ASGN, lineof(yyYpv[2].i_entry), yyYpv[1].tr_entry,
				    yyYpv[4].tr_entry); break;
case 113: {
NEerror:
			yyval.tr_entry = TR_NIL;
			yyPerror("Missing/malformed expression", PEXPR);
		  } break;
case 114: yyval.tr_entry = tree4(yyYpv[2].i_entry,
			yyYpv[1].tr_entry->expr_node.const_tag == SAWCON ?
			yyYpv[3].tr_entry->expr_node.const_tag :
			yyYpv[1].tr_entry->expr_node.const_tag,
			yyYpv[1].tr_entry, yyYpv[3].tr_entry); break;
case 115: yyval.tr_entry = tree3(T_PLUS, yyYpv[2].tr_entry->expr_node.const_tag,
				yyYpv[2].tr_entry); break;
case 116: yyval.tr_entry = tree3(T_MINUS, yyYpv[2].tr_entry->expr_node.const_tag,
				yyYpv[2].tr_entry); break;
case 117: yyval.tr_entry = tree4(yyYpv[2].i_entry,
			yyYpv[1].tr_entry->expr_node.const_tag == SAWCON ?
			yyYpv[3].tr_entry->expr_node.const_tag :
			yyYpv[1].tr_entry->expr_node.const_tag, yyYpv[1].tr_entry,
			yyYpv[3].tr_entry); break;
case 118: yyval.tr_entry = tree4(yyYpv[2].i_entry,
			yyYpv[1].tr_entry->expr_node.const_tag == SAWCON ?
			yyYpv[3].tr_entry->expr_node.const_tag :
			yyYpv[1].tr_entry->expr_node.const_tag, yyYpv[1].tr_entry,
			yyYpv[3].tr_entry); break;
case 119: yyval.tr_entry = tree2(T_NIL, NOCON); break;
case 120: yyval.tr_entry = tree3(T_STRNG, SAWCON, yyYpv[1].tr_entry); break;
case 121: yyval.tr_entry = tree3(T_INT, NOCON, yyYpv[1].tr_entry); break;
case 122: yyval.tr_entry = tree3(T_BINT, NOCON, yyYpv[1].tr_entry); break;
case 123: yyval.tr_entry = tree3(T_FINT, NOCON, yyYpv[1].tr_entry); break;
case 125: goto NEerror; break;
case 126: yyval.tr_entry = tree4(T_FCALL, NOCON, yyYpv[1].tr_entry,
			fixlist(yyYpv[3].tr_entry)); break;
case 127: yyval.tr_entry = yyYpv[2].tr_entry; break;
case 128: yyval.tr_entry = tree3(T_NOT, NOCON, yyYpv[2].tr_entry); break;
case 129: yyval.tr_entry = tree3(T_CSET, SAWCON, fixlist(yyYpv[2].tr_entry)); break;
case 130: yyval.tr_entry = tree3(T_CSET, SAWCON, TR_NIL); break;
case 131: yyval.tr_entry = newlist(yyYpv[1].tr_entry); break;
case 132: yyval.tr_entry = addlist(yyYpv[1].tr_entry, yyYpv[3].tr_entry); break;
case 134: yyval.tr_entry = tree3(T_RANG, yyYpv[1].i_entry, yyYpv[3].tr_entry); break;
case 135: {
			yyval.tr_entry = setupvar(yyYpv[1].cptr, TR_NIL);
		  } break;
case 136: yyYpv[1].tr_entry->var_node.qual = 
					fixlist(yyYpv[1].tr_entry->var_node.qual); break;
case 137: yyval.tr_entry = setupvar(yyYpv[1].cptr, tree2(T_ARY, 
				(int) fixlist(yyYpv[3].tr_entry))); break;
case 138: yyYpv[1].tr_entry->var_node.qual =
				addlist(yyYpv[1].tr_entry->var_node.qual,
				tree2(T_ARY, (int) fixlist(yyYpv[3].tr_entry))); break;
case 139: yyval.tr_entry = setupvar(yyYpv[1].cptr, setupfield(yyYpv[3].tr_entry,
							TR_NIL)); break;
case 140: yyYpv[1].tr_entry->var_node.qual =
		    addlist(yyYpv[1].tr_entry->var_node.qual,
		    setupfield(yyYpv[3].tr_entry, TR_NIL)); break;
case 141: yyval.tr_entry = setupvar(yyYpv[1].cptr, tree1(T_PTR)); break;
case 142: yyYpv[1].tr_entry->var_node.qual = 
			addlist(yyYpv[1].tr_entry->var_node.qual, tree1(T_PTR)); break;
case 144: yyval.tr_entry = tree4(T_WEXP, yyYpv[1].i_entry, yyYpv[3].tr_entry, TR_NIL); break;
case 145: yyval.tr_entry = tree4(T_WEXP, yyYpv[1].i_entry, yyYpv[3].tr_entry,
						yyYpv[5].tr_entry); break;
case 146: yyval.tr_entry = tree4(T_WEXP, yyYpv[1].i_entry, TR_NIL, yyYpv[2].tr_entry); break;
case 147: yyval.tr_entry = tree4(T_WEXP, yyYpv[1].i_entry, yyYpv[3].tr_entry,
					yyYpv[4].tr_entry); break;
case 148: yyval.i_entry = OCT; break;
case 149: yyval.i_entry = HEX; break;
case 150: yyval.tr_entry = newlist(yyYpv[1].tr_entry); break;
case 151: yyval.tr_entry = addlist(yyYpv[1].tr_entry, yyYpv[3].tr_entry); break;
case 152: yyval.tr_entry = newlist(yyYpv[1].tr_entry); break;
case 153: yyval.tr_entry = addlist(yyYpv[1].tr_entry, yyYpv[3].tr_entry); break;
case 154: yyval.i_entry = T_EQ; break;
case 155: yyval.i_entry = T_LT; break;
case 156: yyval.i_entry = T_GT; break;
case 157: yyval.i_entry = T_NE; break;
case 158: yyval.i_entry = T_LE; break;
case 159: yyval.i_entry = T_GE; break;
case 160: yyval.i_entry = T_IN; break;
case 161: yyval.i_entry = T_ADD; break;
case 162: yyval.i_entry = T_SUB; break;
case 163: yyval.i_entry = T_OR; break;
case 164: yyval.i_entry = T_OR; break;
case 165: yyval.i_entry = T_MULT; break;
case 166: yyval.i_entry = T_DIVD; break;
case 167: yyval.i_entry = T_DIV; break;
case 168: yyval.i_entry = T_MOD; break;
case 169: yyval.i_entry = T_AND; break;
case 170: yyval.i_entry = T_AND; break;
case 173: yyval.tr_entry = newlist(yyYpv[1].tr_entry); break;
case 174: yyval.tr_entry = addlist(yyYpv[1].tr_entry, yyYpv[3].tr_entry); break;
case 175: yyval.tr_entry = newlist(yyYpv[1].tr_entry); break;
case 176: yyval.tr_entry = addlist(yyYpv[1].tr_entry, yyYpv[3].tr_entry); break;
case 178: {
			yyval.tr_entry = tree3(T_TYID, lineof(yyline), yyYpv[1].tr_entry);
		  } break;
}
}

int yyact[] = {0,-286,8196,12297,0,16384,4096,12297,0,12290
,-256,8199,-261,8206,-271,8212,-277,8205,-285,8213
,-293,8207,-295,8208,4096,-256,8215,-273,8214,4096
,-256,8199,-259,8217,-261,8206,-271,8212,-277,8205
,-285,8213,-293,8207,-295,8208,4096,12295,12296,12298
,0,12299,-256,8219,-259,12299,-261,12299,-271,12299
,-273,8218,-277,12299,-285,12299,-293,12299,-295,12299
,4096,0,12300,-256,8221,-259,12300,-261,12300,-271
,12300,-273,8220,-277,12300,-285,12300,-293,12300,-295
,12300,4096,0,12301,-256,8223,-259,12301,-261,12301
,-271,12301,-273,8224,-277,12301,-285,12301,-293,12301
,-295,12301,4096,12302,-276,8226,4096,-256,8228,-273
,8227,4096,-256,8230,-273,8229,4096,-256,8232,-273
,8224,4096,-256,8235,-259,12322,-261,12322,-270,8233
,-271,12322,-277,12322,-285,12322,-293,12322,-295,12322
,-303,8234,4096,-273,8237,4096,12324,12325,-40,8238
,-59,8239,4096,12293,-46,8240,4096,-256,8256,-259
,8248,-260,8249,-267,12383,-269,8253,-272,8254,-273
,8246,-274,8255,-276,8244,-288,8252,-296,8251,-297
,8250,-59,12383,4096,-61,8262,4096,12309,-61,8263
,4096,12313,-44,8265,-58,8264,4096,12317,12463,-59
,8266,-44,8267,4096,12304,-61,8268,4096,12308,-61
,8269,4096,12312,-44,8265,-58,8270,4096,12316,-59
,8271,4096,-59,8272,4096,12321,-256,8199,-259,8217
,-261,8206,-271,8212,-277,8205,-285,8213,-293,8207
,-295,8208,4096,-40,8275,12327,-273,8224,4096,12292
,12289,-267,8277,-59,8278,4096,12373,-256,8256,-259
,8248,-260,8249,-267,12383,-269,8253,-272,8254,-273
,8246,-274,8255,-276,8244,-288,8252,-294,12383,-296
,8251,-297,8250,-59,12383,4096,-58,8280,4096,-40
,8281,12385,-256,8282,-266,12472,-267,12472,-294,12472
,-46,12470,-40,12472,-59,12472,-58,12423,-94,12469
,-91,12468,4096,12388,-256,8285,-273,8294,-276,8290
,-279,8299,-280,8292,-284,8288,-290,8289,-298,8291
,-43,8286,-45,8287,-40,8296,-91,8298,-126,8300
,4096,-273,8303,4096,-256,8256,-259,8248,-260,8249
,-269,8253,-272,8254,-273,8246,-274,8255,-276,8244
,-288,8252,-294,12383,-296,8251,-297,8250,-59,12383
,4096,-273,8303,4096,-276,8307,4096,12399,-58,8309
,4096,-46,8311,-94,8312,-91,8310,12424,-91,8313
,4096,-46,8314,4096,-94,8315,4096,-273,8325,-276
,8322,-280,8324,-290,8317,-298,8323,-43,8319,-45
,8320,4096,-258,8334,-268,8335,-273,8338,-276,8322
,-280,8324,-283,8330,-287,8337,-289,8336,-290,8317
,-298,8323,-43,8319,-45,8320,-40,8332,-94,8328
,4096,-273,8340,4096,12303,-276,8341,4096,12318,12319
,-59,8345,4096,-58,8347,12333,-271,8353,-273,8224
,-285,8352,-295,8351,4096,-41,8354,-44,8265,4096
,12294,12375,12374,-256,8256,-259,8248,-260,8249,-266
,12383,-267,12383,-269,8253,-272,8254,-273,8246,-274
,8255,-276,8244,-288,8252,-294,12383,-296,8251,-297
,8250,-59,12383,4096,-256,8285,-273,8294,-276,8290
,-279,8299,-280,8292,-284,8288,-290,8289,-298,8291
,-43,8286,-45,8287,-40,8296,-91,8298,-126,8300
,4096,12387,-267,8359,-59,8278,4096,-257,8376,-262
,8374,-275,8367,-278,8375,-281,8360,-282,8370,-60
,8365,-61,8364,-62,8366,-43,8368,-45,8369,-124
,8371,-42,8372,-47,8373,-38,8377,4096,12401,12407
,12408,12409,12410,12411,12412,-256,8380,-257,12423,-262
,12423,-263,12423,-264,12423,-265,12423,-266,12423,-267
,12423,-275,12423,-278,12423,-281,12423,-282,12423,-291
,12423,-292,12423,-294,12423,-299,12423,-300,12423,-60
,12423,-61,12423,-62,12423,-43,12423,-45,12423,-124
,12423,-42,12423,-47,12423,-38,12423,-46,12470,-40
,12473,-41,12423,-59,12423,-44,12423,-58,12423,-94
,12469,-91,12468,-93,12423,4096,-40,8381,4096,-256
,8285,-273,8294,-276,8290,-279,8299,-280,8292,-284
,8288,-290,8289,-298,8291,-43,8286,-45,8287,-40
,8296,-91,8298,-93,8385,-126,8300,4096,12459,12460
,-263,8388,-44,8389,4096,12461,-46,12470,-94,12469
,-91,12468,12423,-257,8376,-262,8374,-263,8390,-275
,8367,-278,8375,-282,8370,-60,8365,-61,8364,-62
,8366,-43,8368,-45,8369,-124,8371,-42,8372,-47
,8373,-38,8377,4096,-294,8391,-59,8278,4096,-265
,8392,-292,8393,4096,12396,-257,8376,-262,8374,-275
,8367,-278,8375,-282,8370,-291,8394,-60,8365,-61
,8364,-62,8366,-43,8368,-45,8369,-124,8371,-42
,8372,-47,8373,-38,8377,4096,-61,8395,4096,-256
,8285,-273,8294,-276,8290,-279,8299,-280,8292,-284
,8288,-290,8289,-298,8291,-43,8286,-45,8287,-40
,8296,-91,8298,-126,8300,4096,-273,8399,4096,12430
,12429,-59,8402,4096,12336,12337,-273,8325,-276,8322
,-280,8324,-298,8323,4096,12340,12341,12342,12343,12465
,-59,8405,4096,12346,-273,8406,4096,12348,-258,8334
,-268,8335,-287,8337,-289,8336,4096,12350,-264,8409
,4096,-91,8410,4096,-281,8411,4096,-281,8412,4096
,-273,8224,12363,-264,12465,12466,-59,8417,4096,12464
,12305,-59,8418,4096,-59,8419,4096,-59,8420,4096
,12320,-59,8421,4096,-41,8423,-59,8424,4096,12334
,-44,8265,-58,8425,4096,-59,8429,4096,12384,-41
,8430,-44,8431,4096,12440,-257,8376,-262,8374,-275
,8367,-278,8375,-282,8370,-299,8434,-300,8435,-60
,8365,-61,8364,-62,8366,-43,8368,-45,8369,-124
,8371,-42,8372,-47,8373,-38,8377,-58,8432,12431
,12389,-256,8438,-267,12382,-273,8325,-276,8322,-280
,8324,-290,8317,-298,8323,-301,8440,-43,8319,-45
,8320,-59,12382,4096,12442,-61,8446,-62,8445,12443
,-61,8447,12444,12448,12449,12450,12451,12452,12453,12454
,12455,12456,12457,12458,-257,8376,-262,8374,-278,8375
,-42,8372,-47,8373,-38,8377,12403,-257,8376,-262
,8374,-278,8375,-42,8372,-47,8373,-38,8377,12404
,12413,-257,8376,-262,8374,-275,8367,-278,8375,-282
,8370,-60,8365,-61,8364,-62,8366,-43,8368,-45
,8369,-124,8371,-42,8372,-47,8373,-38,8377,-41
,8449,4096,12416,-44,8451,-93,8450,4096,12418,12419
,-257,8376,-262,8374,-264,8452,-275,8367,-278,8375
,-282,8370,-60,8365,-61,8364,-62,8366,-43,8368
,-45,8369,-124,8371,-42,8372,-47,8373,-38,8377
,12421,-273,8303,4096,-44,8462,-93,8461,4096,-257
,8376,-262,8374,-275,8367,-278,8375,-282,8370,-60
,8365,-61,8364,-62,8366,-43,8368,-45,8369,-124
,8371,-42,8372,-47,8373,-38,8377,12438,12428,12471
,-44,8462,-93,8463,4096,12427,12307,12338,12339,12311
,12347,12349,-41,8464,-44,8265,4096,-273,8338,-276
,8322,-280,8324,-290,8317,-298,8323,-43,8319,-45
,8320,-40,8332,4096,-273,8338,-276,8322,-280,8324
,-290,8317,-298,8323,-43,8319,-45,8320,-40,8332
,4096,-267,8470,4096,-256,8473,-260,8474,-267,12365
,-41,12365,-59,8472,4096,12360,-44,8265,-58,8475
,4096,12315,12306,12310,12314,12323,12332,12326,-271,8353
,-273,8224,-285,8352,-295,8351,4096,-44,8265,-58
,8478,4096,-40,8275,-44,8265,12327,-40,8275,-44
,8265,12327,12291,12386,-256,8285,-273,8294,-276,8290
,-279,8299,-280,8292,-284,8288,-290,8289,-298,8291
,-43,8286,-45,8287,-40,8296,-91,8298,-126,8300
,4096,12434,12436,12437,-256,8484,-267,8485,-59,8483
,4096,12376,12378,-44,8486,-58,8487,4096,-256,8256
,-259,8248,-260,8249,-267,12383,-269,8253,-272,8254
,-273,8246,-274,8255,-276,8244,-288,8252,-296,8251
,-297,8250,-59,12383,4096,12344,-257,8376,-262,8374
,-275,4096,-278,8375,-282,8370,-60,4096,-61,4096
,-62,4096,-43,8368,-45,8369,-124,8371,-42,8372
,-47,8373,-38,8377,12402,-257,8376,-262,8374,-278
,8375,-42,8372,-47,8373,-38,8377,12405,12406,12445
,12446,12447,-41,8489,-44,8431,4096,12415,12417,-256
,8285,-273,8294,-276,8290,-279,8299,-280,8292,-284
,8288,-290,8289,-298,8291,-43,8286,-45,8287,-40
,8296,-91,8298,-126,8300,4096,12391,12462,12392,-257
,8376,-262,8374,-275,8367,-278,8375,-282,8370,-60
,8365,-61,8364,-62,8366,-43,8368,-45,8369,-124
,8371,-42,8372,-47,8373,-38,8377,12393,-257,8376
,-262,8374,-263,8492,-275,8367,-278,8375,-282,8370
,-60,8365,-61,8364,-62,8366,-43,8368,-45,8369
,-124,8371,-42,8372,-47,8373,-38,8377,4096,-257
,8376,-262,8374,-263,8493,-275,8367,-278,8375,-282
,8370,-60,8365,-61,8364,-62,8366,-43,8368,-45
,8369,-124,8371,-42,8372,-47,8373,-38,8377,4096
,-266,8494,12397,-257,8376,-262,8374,-275,8367,-278
,8375,-282,8370,-60,8365,-61,8364,-62,8366,-43
,8368,-45,8369,-124,8371,-42,8372,-47,8373,-38
,8377,12400,12426,12425,12351,12352,-44,8497,-93,8496
,4096,12357,12354,12355,12356,12359,-273,8224,12363,12362
,-273,8500,4096,12335,12328,12441,-257,8376,-262,8374
,-275,8367,-278,8375,-282,8370,-299,8434,-300,8435
,-60,8365,-61,8364,-62,8366,-43,8368,-45,8369
,-124,8371,-42,8372,-47,8373,-38,8377,-58,8505
,12432,-273,8325,-276,8322,-280,8324,-290,8317,-298
,8323,-301,8440,-43,8319,-45,8320,12382,12379,12390
,12381,12414,12420,-257,8376,-262,8374,-275,8367,-278
,8375,-282,8370,-60,8365,-61,8364,-62,8366,-43
,8368,-45,8369,-124,8371,-42,8372,-47,8373,-38
,8377,12422,-257,8376,-262,8374,-275,8367,-278,8375
,-282,8370,-60,8365,-61,8364,-62,8366,-43,8368
,-45,8369,-124,8371,-42,8372,-47,8373,-38,8377
,12439,-281,8513,4096,12361,-281,8515,4096,-58,8516
,12466,12364,12329,12330,12331,12435,12377,12345,12380,12394
,12395,12398,12358,-273,8325,-276,8322,-280,8324,-290
,8317,-298,8323,-43,8319,-45,8320,12371,-273,8523
,4096,-257,8376,-262,8374,-275,8367,-278,8375,-282
,8370,-60,8365,-61,8364,-62,8366,-43,8368,-45
,8369,-124,8371,-42,8372,-47,8373,-38,8377,12433
,12353,-256,8525,-267,12366,-41,12366,-59,8524,4096
,12368,-44,8486,-58,8526,4096,-281,8527,4096,12466
,-273,8325,-276,8322,-280,8324,-290,8317,-298,8323
,-43,8319,-45,8320,12371,12370,-40,8529,4096,12369
,-256,8525,-267,12367,-41,12367,-59,8524,4096,-41
,8532,4096,12372,-1};

int yypact[] = {0,1,4,7,8,25,30,47,48,49
,50,71,92,113,114,117,122,127,132,7
,153,156,157,158,163,164,167,194,197,198
,201,202,207,208,209,214,215,218,219,222
,223,228,229,232,235,236,253,256,259,260
,261,266,267,296,299,302,323,167,324,351
,324,354,381,384,324,387,388,391,398,401
,404,407,422,422,451,454,455,407,422,422
,458,459,460,463,466,475,480,481,482,483
,514,541,542,547,578,324,324,579,580,581
,582,583,584,585,656,324,324,659,688,689
,690,695,696,703,734,739,744,745,776,779
,806,809,779,806,810,811,814,815,816,816
,825,826,827,828,829,830,833,834,837,838
,847,256,848,851,854,857,860,863,866,869
,870,871,874,877,880,881,422,884,889,890
,256,256,256,895,898,899,904,905,940,941
,324,324,324,964,965,970,973,974,975,976
,977,978,979,980,981,982,983,984,997,1010
,514,1011,1042,1043,1048,1049,1050,483,1081,483
,324,324,324,483,324,1084,1089,1118,1119,1120
,1125,1126,1127,1128,1129,1130,1131,1132,407,1137
,422,1154,1171,1174,1185,1186,1191,1192,1193,1194
,1195,1196,1197,1198,422,1207,1212,1217,1222,1223
,1224,324,1251,1252,1253,1254,1261,1262,1263,1268
,1295,1296,1325,1338,1339,1340,1341,1342,1347,1348
,1349,324,1376,1377,1378,1379,1408,1439,1470,1473
,1502,324,1503,1504,1505,1506,1511,1512,1513,1514
,1515,1516,1519,1520,422,1523,1524,422,463,463
,1525,1526,1561,1578,1579,407,1268,1580,1581,1582
,1583,483,483,483,1612,1641,1154,1644,1645,1648
,1651,1652,1653,1654,324,1655,1656,1657,1658,1659
,1660,1661,422,1662,1663,1678,1681,1710,1711,1720
,1721,1726,1729,1730,1745,1746,1663,1749,860,1750
,1759,1762,-1};

int yyr1[] = {0,1,1,2,2,2,4,3,3,3
,7,7,7,7,7,8,13,13,9,9
,9,9,10,10,10,10,11,11,11,11
,12,12,12,12,17,16,18,18,19,19
,22,22,22,22,20,20,21,21,14,14
,14,14,23,23,23,23,25,25,15,15
,15,15,26,26,26,27,27,27,27,29
,29,30,31,31,31,33,33,32,32,32
,34,34,34,35,35,6,6,37,38,38
,38,38,39,39,39,36,36,36,36,36
,36,36,36,36,36,36,36,36,36,36
,36,36,42,43,43,43,43,43,43,43
,43,43,43,43,43,43,43,43,43,43
,43,51,51,52,52,45,45,53,53,53
,53,53,53,59,59,59,59,59,60,60
,55,55,41,41,46,46,46,46,46,46
,46,47,47,47,47,48,48,48,48,48
,48,50,50,44,44,5,5,24,28,61
,54,58,56,57,40,49,-1};

int yyr2[] = {0,4,1,6,3,2,3,2,2,0
,1,1,1,1,1,3,1,3,5,5
,2,2,5,5,2,2,5,5,2,2
,3,3,4,2,1,5,1,1,3,0
,3,4,4,4,2,0,1,3,1,1
,2,2,1,1,1,1,1,3,1,2
,1,2,1,3,3,6,3,3,3,1
,3,2,1,3,2,0,3,0,4,6
,1,3,2,0,5,1,2,2,1,3
,1,2,3,2,0,0,3,1,4,2
,1,3,5,4,4,4,6,6,2,4
,6,1,4,1,3,2,2,3,3,1
,1,1,1,1,1,2,4,3,2,3
,2,1,3,1,3,1,1,4,4,3
,3,2,2,1,3,5,2,4,1,1
,1,3,1,3,1,1,1,2,2,2
,1,1,1,1,1,1,1,1,1,1
,1,1,1,1,3,1,3,1,1,1
,1,1,1,1,1,1,-1};

int yygo[] = {0,-1,1,-1,2,2,5,18,44,-1
,3,44,81,-1,24,11,30,16,39,46
,84,83,158,140,216,159,234,160,235,161
,236,232,158,-1,224,56,91,60,113,-1
,49,-1,6,-1,8,-1,9,-1,10,-1
,11,-1,12,-1,33,70,124,76,150,168
,249,217,273,291,249,294,316,323,249,332
,249,335,249,-1,141,72,147,77,151,78
,152,155,230,219,276,233,285,283,309,286
,310,321,326,-1,134,-1,17,-1,18,-1
,19,235,287,236,288,-1,82,287,311,288
,312,-1,154,-1,156,232,284,-1,157,127
,211,128,212,-1,126,-1,129,168,247,291
,247,-1,329,218,275,220,277,305,322,-1
,135,138,215,-1,137,282,307,324,330,-1
,139,-1,274,337,339,-1,221,-1,222,-1
,279,280,306,-1,223,335,338,-1,327,332
,336,-1,328,51,87,88,163,196,261,198
,263,202,267,248,296,295,317,300,318,301
,319,302,320,-1,50,-1,51,-1,244,291
,315,-1,245,-1,53,189,256,-1,164,61
,114,-1,55,57,92,59,112,63,116,94
,186,95,187,104,190,105,191,106,195,118
,205,121,205,169,250,170,251,171,252,199
,264,200,265,201,266,203,268,240,290,259
,195,260,299,270,303,313,325,-1,166,-1
,109,25,65,51,65,56,65,58,110,60
,65,61,65,88,65,196,65,197,262,198
,65,202,65,248,65,295,65,300,65,301
,65,302,65,-1,101,-1,169,-1,170,-1
,171,-1,103,-1,105,-1,192,259,298,-1
,194,-1,66,-1,67,121,208,-1,204,-1
,68,122,209,-1,206,-1,69,239,289,-1
,165,290,314,-1,241,-1,-1,-1};

int yypgo[] = {0,1,3,5,11,15,35,41,43,45
,47,49,51,53,55,75,95,97,99,101
,107,113,115,119,125,127,133,141,145,151
,153,157,159,161,165,169,173,195,197,199
,203,205,209,213,259,261,295,297,299,301
,303,305,307,311,313,315,319,321,325,327
,331,335,-1};


yyEactr(__np__, var)
int __np__;
char *var;
{
switch(__np__) {
default:
return (1);
break;
case 177:  return (identis(var, CONST)); break;
case 179:  return (identis(var, VAR)); break;
case 180:  return (identis(var, ARRAY)); break;
case 181:  return (identis(var, PTRFILE)); break;
case 182:  return (identis(var, RECORD)); break;
case 183:  return (identis(var, FIELD)); break;
case 184:  return (identis(var, PROC)); break;
case 185:  return (identis(var, FUNC)); break;
case 135: {
			 return (identis(var, VAR));
}
break;
case 178: {
			 return (identis(var, TYPE));
}
break;
}
/*NOTREACHED*/
}
