/* Copyright (c) 1979 Regents of the University of California */

#include "0.h"
#include "yy.h"
#include "tree.h"

#ifdef PI
#define	lineof(l)	l
#define	line2of(l)	l
#endif

extern yyerrflag;

int *yyval 0;
int *yypv;
yyactr(__np__){
	register int **yyYpv;
	register int *p, *q;
	yyYpv = yypv;

switch(__np__){

case 1: funcend(yyYpv[1], yyYpv[4], lineof(yyYpv[5])); break;
case 2: yyval = funcbody(funchdr(tree5(T_PROG, lineof(yyYpv[1]), yyYpv[2], fixlist(yyYpv[4]), NIL))); break;
case 3: {
			yyPerror("Malformed program statement", PPROG);
			/*
			 * Should make a program statement
			 * with "input" and "output" here.
			 */
			yyval = funcbody(funchdr(tree5(T_PROG, lineof(yyYpv[1]), NIL, NIL, NIL)));
		  } break;
case 4: {
			yyval = tree3(T_BSTL, lineof(yyYpv[1]), fixlist(yyYpv[2]));
			if (yyYpv[3].pint < 0)
				brerror(yyYpv[1], "begin");
		  } break;
case 5: trfree(); break;
case 6: {
Derror:
			constend(), typeend(), varend(), trfree();
			yyPerror("Malformed declaration", PDECL);
		  } break;
case 7: trfree(); break;
case 9: constend(); break;
case 10: typeend(); break;
case 11: varend(); break;
case 12: label(fixlist(yyYpv[2]), lineof(yyYpv[1])); break;
case 13: yyval = newlist(yyYpv[1] == NIL ? NIL : *hash(yyYpv[1], 1)); break;
case 14: yyval = addlist(yyYpv[1], yyYpv[3] == NIL ? NIL : *hash(yyYpv[3], 1)); break;
case 15: constbeg(yyYpv[1], line2of(yyYpv[2])), const(lineof(yyYpv[3]), yyYpv[2], yyYpv[4]); break;
case 16: const(lineof(yyYpv[3]), yyYpv[2], yyYpv[4]); break;
case 17: {
			constbeg(yyYpv[1], line2of(yyYpv[1]));
Cerror:
			yyPerror("Malformed const declaration", PDECL);
		  } break;
case 18: goto Cerror; break;
case 19: typebeg(yyYpv[1], line2of(yyYpv[2])), type(lineof(yyYpv[3]), yyYpv[2], yyYpv[4]); break;
case 20: type(lineof(yyYpv[3]), yyYpv[2], yyYpv[4]); break;
case 21: {
			typebeg(yyYpv[1], line2of(yyYpv[1]));
Terror:
			yyPerror("Malformed type declaration", PDECL);
		  } break;
case 22: goto Terror; break;
case 23: varbeg(yyYpv[1], line2of(yyYpv[3])), var(lineof(yyYpv[3]), fixlist(yyYpv[2]), yyYpv[4]); break;
case 24: var(lineof(yyYpv[3]), fixlist(yyYpv[2]), yyYpv[4]); break;
case 25: {
			varbeg(yyYpv[1], line2of(yyYpv[1]));
Verror:
			yyPerror("Malformed var declaration", PDECL);
		  } break;
case 26: goto Verror; break;
case 28: trfree(); break;
case 29: funcfwd(yyYpv[1]); break;
case 30: funcend(yyYpv[1], yyYpv[4], lineof(yyYpv[5])); break;
case 31: funcbody(yyYpv[1]); break;
case 32: yyval = funchdr(tree5(yyYpv[1], lineof(yyYpv[5]), yyYpv[2], yyYpv[3], yyYpv[4])); break;
case 33: yyval = T_PDEC; break;
case 34: yyval = T_FDEC; break;
case 35: yyval = fixlist(yyYpv[2]); break;
case 36: yyval = NIL; break;
case 37: yyval = tree3(T_PVAL, fixlist(yyYpv[1]), yyYpv[3]); break;
case 38: yyval = tree3(T_PVAR, fixlist(yyYpv[2]), yyYpv[4]); break;
case 39: yyval = tree3(T_PFUNC, fixlist(yyYpv[2]), yyYpv[4]); break;
case 40: yyval = tree2(T_PPROC, fixlist(yyYpv[2])); break;
case 41: yyval = yyYpv[2]; break;
case 42: yyval = NIL; break;
case 43: yyval = newlist(yyYpv[1]); break;
case 44: yyval = addlist(yyYpv[1], yyYpv[3]); break;
case 45: yyval = tree2(T_CSTRNG, yyYpv[1]); break;
case 47: yyval = tree2(T_PLUSC, yyYpv[2]); break;
case 48: yyval = tree2(T_MINUSC, yyYpv[2]); break;
case 49: yyval = tree2(T_ID, yyYpv[1]); break;
case 50: yyval = tree2(T_CINT, yyYpv[1]); break;
case 51: yyval = tree2(T_CBINT, yyYpv[1]); break;
case 52: yyval = tree2(T_CFINT, yyYpv[1]); break;
case 53: yyval = newlist(yyYpv[1]); break;
case 54: yyval = addlist(yyYpv[1], yyYpv[3]); break;
case 56: yyval = tree3(T_TYPTR, lineof(yyYpv[1]), tree2(T_ID, yyYpv[2])); break;
case 58: yyval = tree3(T_TYPACK, lineof(yyYpv[1]), yyYpv[2]); break;
case 60: yyval = tree3(T_TYSCAL, lineof(yyYpv[1]), fixlist(yyYpv[2])); break;
case 61: yyval = tree4(T_TYRANG, lineof(yyYpv[2]), yyYpv[1], yyYpv[3]); break;
case 62: yyval = tree4(T_TYARY, lineof(yyYpv[1]), fixlist(yyYpv[3]), yyYpv[6]); break;
case 63: yyval = tree3(T_TYFILE, lineof(yyYpv[1]), yyYpv[3]); break;
case 64: yyval = tree3(T_TYSET, lineof(yyYpv[1]), yyYpv[3]); break;
case 65: {
			yyval = tree3(T_TYREC, lineof(yyYpv[1]), yyYpv[2]);
			if (yyYpv[3].pint < 0)
				brerror(yyYpv[1], "record");
		  } break;
case 66: yyval = newlist(yyYpv[1]); break;
case 67: yyval = addlist(yyYpv[1], yyYpv[3]); break;
case 68: yyval = tree4(T_FLDLST, lineof(NIL), fixlist(yyYpv[1]), yyYpv[2]); break;
case 69: yyval = newlist(yyYpv[1]); break;
case 70: yyval = addlist(yyYpv[1], yyYpv[3]); break;
case 71: yyPerror("Malformed record declaration", PDECL); break;
case 72: yyval = NIL; break;
case 73: yyval = tree4(T_RFIELD, lineof(yyYpv[2]), fixlist(yyYpv[1]), yyYpv[3]); break;
case 74: yyval = NIL; break;
case 75: yyval = tree5(T_TYVARPT, lineof(yyYpv[1]), NIL, yyYpv[2], fixlist(yyYpv[4])); break;
case 76: yyval = tree5(T_TYVARPT, lineof(yyYpv[1]), yyYpv[2], yyYpv[4], fixlist(yyYpv[6])); break;
case 77: yyval = newlist(yyYpv[1]); break;
case 78: yyval = addlist(yyYpv[1], yyYpv[3]); break;
case 79: yyPerror("Malformed record declaration", PDECL); break;
case 80: yyval = NIL; break;
case 81: yyval = tree4(T_TYVARNT, lineof(yyYpv[2]), fixlist(yyYpv[1]), yyYpv[4]); break;
case 82: yyval = tree4(T_TYVARNT, lineof(yyYpv[2]), fixlist(yyYpv[1]), NIL); break;
case 83: yyval = newlist(yyYpv[1]); break;
case 84: {
			if ((p = yyYpv[1]) != NIL && (q = p[1])[0] == T_IFX) {
				q[0] = T_IFEL;
				q[4] = yyYpv[2];
			} else
				yyval = addlist(yyYpv[1], yyYpv[2]);
		  } break;
case 85: if ((q = yyYpv[1]) != NIL && (p = q[1]) != NIL && p[0] == T_IF) {
			if (yychar < 0)
				yychar = yylex();
			if (yyshifts >= 2 && yychar == YELSE) {
				recovered();
				copy(&Y, &OY, sizeof Y);
				yerror("Deleted ';' before keyword else");
				yychar = yylex();
				p[0] = T_IFX;
			}
		  } break;
case 86: yyval = newlist(yyYpv[1]); break;
case 87: yyval = addlist(yyYpv[1], yyYpv[3]); break;
case 88: {
			yyval = NIL;
Kerror:
			yyPerror("Malformed statement in case", PSTAT);
		  } break;
case 89: goto Kerror; break;
case 90: yyval = tree4(T_CSTAT, lineof(yyYpv[2]), fixlist(yyYpv[1]), yyYpv[3]); break;
case 91: yyval = tree4(T_CSTAT, lineof(yyYpv[1]), NIL, yyYpv[2]); break;
case 92: yyval = NIL; break;
case 93: yyval = NIL; break;
case 94: yyval = tree4(T_LABEL, lineof(yyYpv[2]), yyYpv[1] == NIL ? NIL : *hash(yyYpv[1], 1), yyYpv[3]); break;
case 95: yyval = tree4(T_PCALL, lineof(yyline), yyYpv[1], NIL); break;
case 96: yyval = tree4(T_PCALL, lineof(yyYpv[2]), yyYpv[1], fixlist(yyYpv[3])); break;
case 97: goto NSerror; break;
case 99: {
			yyval = tree3(T_BLOCK, lineof(yyYpv[1]), fixlist(yyYpv[2]));
			if (yyYpv[3].pint < 0)
				brerror(yyYpv[1], "begin");
		  } break;
case 100: {
			yyval = tree4(T_CASE, lineof(yyYpv[1]), yyYpv[2], fixlist(yyYpv[4]));
			if (yyYpv[5].pint < 0)
				brerror(yyYpv[1], "case");
		  } break;
case 101: yyval = tree4(T_WITH, lineof(yyYpv[1]), fixlist(yyYpv[2]), yyYpv[4]); break;
case 102: yyval = tree4(T_WHILE, lineof(yyYpv[1]), yyYpv[2], yyYpv[4]); break;
case 103: yyval = tree4(T_REPEAT, lineof(yyYpv[3]), fixlist(yyYpv[2]), yyYpv[4]); break;
case 104: yyval = tree5(T_FORU, lineof(yyYpv[1]), yyYpv[2], yyYpv[4], yyYpv[6]); break;
case 105: yyval = tree5(T_FORD, lineof(yyYpv[1]), yyYpv[2], yyYpv[4], yyYpv[6]); break;
case 106: yyval = tree3(T_GOTO, lineof(yyYpv[1]), *hash(yyYpv[2], 1)); break;
case 107: yyval = tree5(T_IF, lineof(yyYpv[1]), yyYpv[2], yyYpv[4], NIL); break;
case 108: yyval = tree5(T_IFEL, lineof(yyYpv[1]), yyYpv[2], yyYpv[4], yyYpv[6]); break;
case 109: yyval = tree5(T_IFEL, lineof(yyYpv[1]), yyYpv[2], yyYpv[4], NIL); break;
case 110: yyval = tree3(T_ASRT, lineof(yyYpv[1]), yyYpv[3]); break;
case 111: {
NSerror:
			yyval = NIL;
Serror:
			yyPerror("Malformed statement", PSTAT);
		  } break;
case 112: yyval = tree4(T_ASGN, lineof(yyYpv[2]), yyYpv[1], yyYpv[4]); break;
case 113: {
NEerror:
			yyval = NIL;
Eerror:
			yyPerror("Missing/malformed expression", PEXPR);
		  } break;
case 114: yyval = tree4(yyYpv[2], yyYpv[1][1] == SAWCON ? yyYpv[3][1] : yyYpv[1][1], yyYpv[1], yyYpv[3]); break;
case 115: yyval = tree3(T_PLUS, yyYpv[2][1], yyYpv[2]); break;
case 116: yyval = tree3(T_MINUS, yyYpv[2][1], yyYpv[2]); break;
case 117: yyval = tree4(yyYpv[2], yyYpv[1][1] == SAWCON ? yyYpv[3][1] : yyYpv[1][1], yyYpv[1], yyYpv[3]); break;
case 118: yyval = tree4(yyYpv[2], yyYpv[1][1] == SAWCON ? yyYpv[3][1] : yyYpv[1][1], yyYpv[1], yyYpv[3]); break;
case 119: yyval = tree2(T_NIL, NOCON); break;
case 120: yyval = tree3(T_STRNG, SAWCON, yyYpv[1]); break;
case 121: yyval = tree3(T_INT, NOCON, yyYpv[1]); break;
case 122: yyval = tree3(T_BINT, NOCON, yyYpv[1]); break;
case 123: yyval = tree3(T_FINT, NOCON, yyYpv[1]); break;
case 125: goto NEerror; break;
case 126: yyval = tree4(T_FCALL, NOCON, yyYpv[1], fixlist(yyYpv[3])); break;
case 127: yyval = yyYpv[2]; break;
case 128: yyval = tree3(T_NOT, NOCON, yyYpv[2]); break;
case 129: yyval = tree3(T_CSET, SAWCON, fixlist(yyYpv[2])); break;
case 130: yyval = tree3(T_CSET, SAWCON, NIL); break;
case 131: yyval = newlist(yyYpv[1]); break;
case 132: yyval = addlist(yyYpv[1], yyYpv[3]); break;
case 134: yyval = tree3(T_RANG, yyYpv[1], yyYpv[3]); break;
case 135: {
			yyval = setupvar(yyYpv[1], NIL);
		  } break;
case 136: yyYpv[1][3] = fixlist(yyYpv[1][3]); break;
case 137: yyval = setupvar(yyYpv[1], tree2(T_ARY, fixlist(yyYpv[3]))); break;
case 138: yyYpv[1][3] = addlist(yyYpv[1][3], tree2(T_ARY, fixlist(yyYpv[3]))); break;
case 139: yyval = setupvar(yyYpv[1], tree3(T_FIELD, yyYpv[3], NIL)); break;
case 140: yyYpv[1][3] = addlist(yyYpv[1][3], tree3(T_FIELD, yyYpv[3], NIL)); break;
case 141: yyval = setupvar(yyYpv[1], tree1(T_PTR)); break;
case 142: yyYpv[1][3] = addlist(yyYpv[1][3], tree1(T_PTR)); break;
case 144: yyval = tree4(T_WEXP, yyYpv[1], yyYpv[3], NIL); break;
case 145: yyval = tree4(T_WEXP, yyYpv[1], yyYpv[3], yyYpv[5]); break;
case 146: yyval = tree4(T_WEXP, yyYpv[1], NIL, yyYpv[2]); break;
case 147: yyval = tree4(T_WEXP, yyYpv[1], yyYpv[3], yyYpv[4]); break;
case 148: yyval = OCT; break;
case 149: yyval = HEX; break;
case 150: yyval = newlist(yyYpv[1]); break;
case 151: yyval = addlist(yyYpv[1], yyYpv[3]); break;
case 152: yyval = newlist(yyYpv[1]); break;
case 153: yyval = addlist(yyYpv[1], yyYpv[3]); break;
case 154: yyval = T_EQ; break;
case 155: yyval = T_LT; break;
case 156: yyval = T_GT; break;
case 157: yyval = T_NE; break;
case 158: yyval = T_LE; break;
case 159: yyval = T_GE; break;
case 160: yyval = T_IN; break;
case 161: yyval = T_ADD; break;
case 162: yyval = T_SUB; break;
case 163: yyval = T_OR; break;
case 164: yyval = T_OR; break;
case 165: yyval = T_MULT; break;
case 166: yyval = T_DIVD; break;
case 167: yyval = T_DIV; break;
case 168: yyval = T_MOD; break;
case 169: yyval = T_AND; break;
case 170: yyval = T_AND; break;
case 173: yyval = newlist(yyYpv[1]); break;
case 174: yyval = addlist(yyYpv[1], yyYpv[3]); break;
case 175: yyval = newlist(yyYpv[1]); break;
case 176: yyval = addlist(yyYpv[1], yyYpv[3]); break;
case 178: {
			yyval = tree3(T_TYID, lineof(yyline), yyYpv[1]);
		  } break;
}
}

int yyact[] {0,-286,8195,4096,0,16384,4096,12295,-256,8198
,-273,8197,4096,-256,8201,-259,12315,-261,8207,-271
,12315,-277,8206,-285,12315,-293,8208,-295,8209,4096
,-40,8210,4096,12291,-259,8213,-271,8218,-285,8217
,4096,12293,12294,12296,-256,8220,-259,12297,-261,12297
,-271,12297,-273,8219,-277,12297,-285,12297,-293,12297
,-295,12297,4096,-256,8222,-259,12298,-261,12298,-271
,12298,-273,8221,-277,12298,-285,12298,-293,12298,-295
,12298,4096,-256,8224,-259,12299,-261,12299,-271,12299
,-273,8225,-277,12299,-285,12299,-293,12299,-295,12299
,4096,-276,8227,4096,-256,8229,-273,8228,4096,-256
,8231,-273,8230,4096,-256,8233,-273,8225,4096,-273
,8225,4096,-46,8235,4096,12316,-256,8252,-259,8243
,-260,8244,-267,12381,-269,8248,-272,8249,-273,8241
,-274,8250,-276,8239,-288,8247,-296,8246,-297,8245
,-301,8251,-59,12381,4096,-270,8258,12319,-273,8260
,4096,12321,12322,-61,8261,4096,12306,-61,8262,4096
,12310,-44,8264,-58,8263,4096,12314,12463,-59,8265
,-44,8266,4096,12301,-61,8267,4096,12305,-61,8268
,4096,12309,-44,8264,-58,8269,4096,12313,-41,8270
,-44,8264,4096,12289,-267,8271,-59,8272,4096,12371
,-256,8252,-259,8243,-260,8244,-267,12381,-269,8248
,-272,8249,-273,8241,-274,8250,-276,8239,-288,8247
,-294,12381,-296,8246,-297,8245,-301,8251,-59,12381
,4096,-58,8274,4096,-40,8275,12383,-256,8276,-266
,12472,-267,12472,-294,12472,-46,12470,-40,12472,-59
,12472,-58,12423,-94,12469,-91,12468,4096,12386,-256
,8279,-273,8288,-276,8284,-279,8293,-280,8286,-284
,8282,-290,8283,-298,8285,-43,8280,-45,8281,-40
,8290,-91,8292,-126,8294,4096,-273,8297,4096,-273
,8297,4096,-276,8301,4096,-40,8303,4096,12399,-58
,8304,4096,-46,8306,-94,8307,-91,8305,12424,-91
,8308,4096,-46,8309,4096,-94,8310,4096,-59,8311
,4096,-256,8201,-259,12315,-261,8207,-271,12315,-277
,8206,-285,12315,-293,8208,-295,8209,4096,-40,8314
,12324,-273,8324,-276,8321,-280,8323,-290,8316,-298
,8322,-43,8318,-45,8319,4096,-258,8333,-268,8334
,-273,8337,-276,8321,-280,8323,-283,8329,-287,8336
,-289,8335,-290,8316,-298,8322,-43,8318,-45,8319
,-40,8331,-94,8327,4096,-273,8339,4096,12300,-276
,8340,4096,-59,8344,4096,12292,12373,12372,-256,8279
,-273,8288,-276,8284,-279,8293,-280,8286,-284,8282
,-290,8283,-298,8285,-43,8280,-45,8281,-40,8290
,-91,8292,-126,8294,4096,12385,-267,8349,-59,8272
,4096,-257,8366,-262,8364,-275,8357,-278,8365,-281
,8350,-282,8360,-60,8355,-61,8354,-62,8356,-43
,8358,-45,8359,-124,8361,-42,8362,-47,8363,-38
,8367,4096,12401,12407,12408,12409,12410,12411,12412,-256
,8370,-257,12423,-262,12423,-263,12423,-264,12423,-265
,12423,-266,12423,-267,12423,-275,12423,-278,12423,-281
,12423,-282,12423,-291,12423,-292,12423,-294,12423,-299
,12423,-300,12423,-60,12423,-61,12423,-62,12423,-43
,12423,-45,12423,-124,12423,-42,12423,-47,12423,-38
,12423,-46,12470,-40,12473,-41,12423,-59,12423,-44
,12423,-58,12423,-94,12469,-91,12468,-93,12423,4096
,-40,8371,4096,-256,8279,-273,8288,-276,8284,-279
,8293,-280,8286,-284,8282,-290,8283,-298,8285,-43
,8280,-45,8281,-40,8290,-91,8292,-93,8375,-126
,8294,4096,12459,12460,-263,8378,-44,8379,4096,12461
,-46,12470,-94,12469,-91,12468,12423,-257,8366,-262
,8364,-263,8380,-275,8357,-278,8365,-282,8360,-60
,8355,-61,8354,-62,8356,-43,8358,-45,8359,-124
,8361,-42,8362,-47,8363,-38,8367,4096,-294,8381
,-59,8272,4096,-265,8382,-292,8383,4096,12394,-257
,8366,-262,8364,-275,8357,-278,8365,-282,8360,-291
,8384,-60,8355,-61,8354,-62,8356,-43,8358,-45
,8359,-124,8361,-42,8362,-47,8363,-38,8367,4096
,-61,8386,4096,-256,8279,-273,8288,-276,8284,-279
,8293,-280,8286,-284,8282,-290,8283,-298,8285,-43
,8280,-45,8281,-40,8290,-91,8292,-126,8294,4096
,-273,8390,4096,12430,12429,12317,-259,8213,-271,8218
,-285,8217,4096,-58,8395,12330,-271,8400,-273,8225
,-285,8401,-295,8399,4096,-59,8402,4096,12333,12334
,-273,8324,-276,8321,-280,8323,-298,8322,4096,12337
,12338,12339,12340,12465,-59,8405,4096,12343,-273,8406
,4096,12345,-258,8333,-268,8334,-287,8336,-289,8335
,4096,12347,-264,8409,4096,-91,8410,4096,-281,8411
,4096,-281,8412,4096,-273,8225,12360,-264,12465,12466
,-59,8417,4096,12464,12302,-59,8418,4096,-59,8419
,4096,-59,8420,4096,12290,12382,-41,8421,-44,8422
,4096,12440,-257,8366,-262,8364,-275,8357,-278,8365
,-282,8360,-299,8425,-300,8426,-60,8355,-61,8354
,-62,8356,-43,8358,-45,8359,-124,8361,-42,8362
,-47,8363,-38,8367,-58,8423,12431,12387,-256,8429
,-267,12380,-273,8324,-276,8321,-280,8323,-290,8316
,-298,8322,-302,8431,-43,8318,-45,8319,-59,12380
,4096,12442,-61,8437,-62,8436,12443,-61,8438,12444
,12448,12449,12450,12451,12452,12453,12454,12455,12456,12457
,12458,-257,8366,-262,8364,-278,8365,-42,8362,-47
,8363,-38,8367,12403,-257,8366,-262,8364,-278,8365
,-42,8362,-47,8363,-38,8367,12404,12413,-257,8366
,-262,8364,-275,8357,-278,8365,-282,8360,-60,8355
,-61,8354,-62,8356,-43,8358,-45,8359,-124,8361
,-42,8362,-47,8363,-38,8367,-41,8440,4096,12416
,-44,8442,-93,8441,4096,12418,12419,-257,8366,-262
,8364,-264,8443,-275,8357,-278,8365,-282,8360,-60
,8355,-61,8354,-62,8356,-43,8358,-45,8359,-124
,8361,-42,8362,-47,8363,-38,8367,12421,-273,8297
,4096,-257,8366,-262,8364,-275,8357,-278,8365,-282
,8360,-60,8355,-61,8354,-62,8356,-43,8358,-45
,8359,-124,8361,-42,8362,-47,8363,-38,8367,-41
,8451,4096,-44,8454,-93,8453,4096,-257,8366,-262
,8364,-275,8357,-278,8365,-282,8360,-60,8355,-61
,8354,-62,8356,-43,8358,-45,8359,-124,8361,-42
,8362,-47,8363,-38,8367,12438,12428,12471,-44,8454
,-93,8455,4096,12427,-59,8456,4096,-59,8457,4096
,-41,8459,-59,8460,4096,12331,-44,8264,-58,8461
,4096,12304,12335,12336,12308,12344,12346,-41,8465,-44
,8264,4096,-273,8337,-276,8321,-280,8323,-290,8316
,-298,8322,-43,8318,-45,8319,-40,8331,4096,-273
,8337,-276,8321,-280,8323,-290,8316,-298,8322,-43
,8318,-45,8319,-40,8331,4096,-267,8471,4096,-256
,8474,-260,8475,-267,12362,-41,12362,-59,8473,4096
,12357,-44,8264,-58,8476,4096,12312,12303,12307,12311
,12384,-256,8279,-273,8288,-276,8284,-279,8293,-280
,8286,-284,8282,-290,8283,-298,8285,-43,8280,-45
,8281,-40,8290,-91,8292,-126,8294,4096,12434,12436
,12437,-256,8480,-267,8481,-59,8479,4096,12374,12376
,-44,8482,-58,8483,4096,12341,-257,8366,-262,8364
,-275,4096,-278,8365,-282,8360,-60,4096,-61,4096
,-62,4096,-43,8358,-45,8359,-124,8361,-42,8362
,-47,8363,-38,8367,12402,-257,8366,-262,8364,-278
,8365,-42,8362,-47,8363,-38,8367,12405,12406,12445
,12446,12447,-41,8485,-44,8422,4096,12415,12417,-256
,8279,-273,8288,-276,8284,-279,8293,-280,8286,-284
,8282,-290,8283,-298,8285,-43,8280,-45,8281,-40
,8290,-91,8292,-126,8294,4096,12389,12462,12390,-257
,8366,-262,8364,-275,8357,-278,8365,-282,8360,-60
,8355,-61,8354,-62,8356,-43,8358,-45,8359,-124
,8361,-42,8362,-47,8363,-38,8367,12391,-257,8366
,-262,8364,-263,8488,-275,8357,-278,8365,-282,8360
,-60,8355,-61,8354,-62,8356,-43,8358,-45,8359
,-124,8361,-42,8362,-47,8363,-38,8367,4096,-257
,8366,-262,8364,-263,8489,-275,8357,-278,8365,-282
,8360,-60,8355,-61,8354,-62,8356,-43,8358,-45
,8359,-124,8361,-42,8362,-47,8363,-38,8367,4096
,-266,8490,12395,12398,-257,8366,-262,8364,-275,8357
,-278,8365,-282,8360,-60,8355,-61,8354,-62,8356
,-43,8358,-45,8359,-124,8361,-42,8362,-47,8363
,-38,8367,12400,12426,12425,12318,12320,12329,12323,-271
,8400,-273,8225,-285,8401,-295,8399,4096,-44,8264
,-58,8494,4096,-44,8264,-58,8495,4096,-44,8264
,12328,12348,12349,-44,8497,-93,8496,4096,12354,12351
,12352,12353,12356,-273,8225,12360,12359,-273,8500,4096
,12441,-257,8366,-262,8364,-275,8357,-278,8365,-282
,8360,-299,8425,-300,8426,-60,8355,-61,8354,-62
,8356,-43,8358,-45,8359,-124,8361,-42,8362,-47
,8363,-38,8367,-58,8502,12432,-273,8324,-276,8321
,-280,8323,-290,8316,-298,8322,-302,8431,-43,8318
,-45,8319,12380,12377,12388,12379,12414,12420,-257,8366
,-262,8364,-275,8357,-278,8365,-282,8360,-60,8355
,-61,8354,-62,8356,-43,8358,-45,8359,-124,8361
,-42,8362,-47,8363,-38,8367,12422,-256,8252,-259
,8243,-260,8244,-266,12381,-267,12381,-269,8248,-272
,8249,-273,8241,-274,8250,-276,8239,-288,8247,-294
,12381,-296,8246,-297,8245,-301,8251,-59,12381,4096
,-257,8366,-262,8364,-275,8357,-278,8365,-282,8360
,-60,8355,-61,8354,-62,8356,-43,8358,-45,8359
,-124,8361,-42,8362,-47,8363,-38,8367,12439,12332
,12325,-281,8512,4096,12358,-281,8514,4096,-58,8515
,12466,12361,12435,12375,12342,12378,12392,12393,12396,12326
,12327,12355,-273,8324,-276,8321,-280,8323,-290,8316
,-298,8322,-43,8318,-45,8319,12368,-273,8522,4096
,-257,8366,-262,8364,-275,8357,-278,8365,-282,8360
,-60,8355,-61,8354,-62,8356,-43,8358,-45,8359
,-124,8361,-42,8362,-47,8363,-38,8367,12433,12350
,-256,8524,-267,12363,-41,12363,-59,8523,4096,12365
,-44,8482,-58,8525,4096,-281,8526,4096,12466,-273
,8324,-276,8321,-280,8323,-290,8316,-298,8322,-43
,8318,-45,8319,12368,12367,-40,8528,4096,12366,-273
,8225,-41,8531,12360,-256,8524,-267,12364,-41,12364
,-59,8523,4096,-41,8532,4096,12370,12369,-1};

int yypact[] {0,1,4,7,8,13,30,33,34,41
,42,43,44,63,82,101,104,109,114,119
,122,125,126,155,7,158,161,162,163,166
,167,170,171,176,177,178,183,184,187,188
,191,192,197,198,203,204,209,210,241,244
,247,268,126,269,296,269,126,299,302,269
,305,308,309,312,319,322,325,328,331,348
,351,366,366,395,398,399,351,366,366,402
,405,406,407,210,408,435,436,441,472,269
,269,473,474,475,476,477,478,479,550,269
,269,553,582,583,584,589,590,597,628,633
,638,639,269,670,673,700,703,673,700,704
,705,706,713,716,725,728,729,730,730,739
,740,741,742,743,744,747,748,751,752,761
,119,762,765,768,771,774,777,780,783,784
,785,788,791,794,795,796,801,802,837,838
,269,269,269,861,862,867,870,871,872,873
,874,875,876,877,878,879,880,881,894,907
,408,908,939,940,945,946,947,210,978,210
,269,269,269,210,981,269,1012,1017,1046,1047
,1048,1053,1054,1057,366,1060,1065,1066,119,119
,119,1071,1072,1073,1074,1075,1076,1077,351,1082
,366,1099,1116,1119,1130,1131,1136,1137,1138,1139
,1140,1141,269,1168,1169,1170,1171,1178,1179,1180
,210,1185,1186,1215,1228,1229,1230,1231,1232,1237
,1238,1239,269,1266,1267,1268,1269,1298,1329,1360
,1363,1364,1393,269,1394,1395,1396,1397,1398,1399
,366,1408,1413,1418,1421,1422,1423,1428,1429,1430
,1431,1432,1433,1436,1437,366,1440,1441,1476,1493
,1494,351,210,1495,1496,1497,1498,210,210,1527
,1560,1589,1590,366,366,1591,1099,1594,1595,1598
,1601,269,1602,1603,1604,1605,1606,1607,1608,1609
,1610,366,1611,1612,1627,1630,1659,1660,1669,1670
,1675,1678,1679,1694,1695,1612,1698,1699,1704,1713
,1716,1717,-1};

int yyr1[] {0,1,2,2,5,3,3,3,8,8
,8,8,9,13,13,10,10,10,10,11
,11,11,11,12,12,12,12,4,4,16
,16,18,17,19,19,20,20,23,23,23
,23,21,21,22,22,14,14,14,14,24
,24,24,24,26,26,15,15,15,15,27
,27,27,28,28,28,28,30,30,31,32
,32,32,34,34,33,33,33,35,35,35
,36,36,36,7,7,38,39,39,39,39
,40,40,40,37,37,37,37,37,37,37
,37,37,37,37,37,37,37,37,37,37
,37,37,43,44,44,44,44,44,44,44
,44,44,44,44,44,44,44,44,44,44
,44,52,52,53,53,46,46,54,54,54
,54,54,54,60,60,60,60,60,61,61
,56,56,42,42,47,47,47,47,47,47
,47,48,48,48,48,49,49,49,49,49
,49,51,51,45,45,6,6,25,29,62
,55,59,57,58,41,50,-1};

int yyr2[] {0,5,6,2,3,2,2,0,1,1
,1,1,3,1,3,5,5,2,2,5
,5,2,2,5,5,2,2,0,2,3
,5,1,5,1,1,3,0,3,4,4
,2,2,0,1,3,1,1,2,2,1
,1,1,1,1,3,1,2,1,2,1
,3,3,6,3,3,3,1,3,2,1
,3,2,0,3,0,4,6,1,3,2
,0,5,4,1,2,2,1,3,1,2
,3,2,0,0,3,1,4,2,1,3
,5,4,4,4,6,6,2,4,6,5
,4,1,4,1,3,2,2,3,3,1
,1,1,1,1,1,2,4,3,2,3
,2,1,3,1,3,1,1,4,4,3
,3,2,2,1,3,5,2,4,1,1
,1,3,1,3,1,1,1,2,2,2
,1,1,1,1,1,1,1,1,1,1
,1,1,1,1,3,1,3,1,1,1
,1,1,1,1,1,1,-1};

int yygo[] {0,-1,1,-1,2,23,67,-1,4,67
,120,-1,7,120,201,-1,19,13,31,17
,40,18,42,122,206,139,216,207,270,208
,271,209,272,268,206,-1,224,51,85,55
,107,-1,44,-1,8,-1,10,-1,11,-1
,12,-1,13,-1,34,69,123,75,149,158
,240,217,274,287,240,290,313,322,240,331
,240,334,240,-1,140,71,146,76,150,77
,151,203,266,219,277,269,301,284,309,302
,318,303,319,320,325,-1,133,-1,20,-1
,22,-1,23,-1,24,-1,121,-1,202,-1
,204,268,300,-1,205,126,211,127,212,-1
,125,-1,128,158,238,287,238,-1,328,218
,276,220,278,305,321,-1,134,137,215,-1
,136,283,307,323,329,-1,138,-1,275,336
,338,-1,221,-1,222,-1,280,281,306,-1
,223,334,337,-1,326,331,335,-1,327,46
,81,82,153,186,252,188,254,192,258,239
,292,291,314,296,315,297,316,298,317,-1
,45,-1,46,-1,235,287,312,-1,236,-1
,48,179,247,-1,154,56,108,-1,50,52
,86,54,106,58,110,88,176,89,177,98
,180,99,181,100,185,111,193,113,196,116
,196,159,241,160,242,161,243,189,255,190
,256,191,257,194,260,231,286,250,185,251
,295,262,299,310,324,-1,156,-1,103,21
,61,46,61,51,61,53,104,55,61,56
,61,82,61,186,61,187,253,188,61,192
,61,239,61,291,61,296,61,297,61,298
,61,-1,95,-1,159,-1,160,-1,161,-1
,97,-1,99,-1,182,250,294,-1,184,-1
,62,-1,63,116,199,-1,195,-1,64,117
,200,-1,197,-1,65,230,285,-1,155,286
,311,-1,232,-1,-1,-1};

int yypgo[] {0,1,3,5,9,13,17,37,43,45
,47,49,51,53,55,75,97,99,101,103
,105,107,109,111,115,121,123,129,137,141
,147,149,153,155,157,161,165,169,191,193
,195,199,201,205,209,257,259,293,295,297
,299,301,303,305,309,311,313,317,319,323
,325,329,333,-1};


yyEactr(__np__, var)
int __np__;
char *var;
{
switch(__np__) {
default:
return (1);
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
}
