/* Copyright (c) Stichting Mathematisch Centrum, Amsterdam, 1985. */

/* 
 * $Header: tabl.c,v 2.4 85/08/22 16:08:42 timo Exp $
 */

/*
 * B editor -- Grammar table.
 */

#include "b.h"
#include "node.h"
#include "gram.h"
#include "tabl.h"


/*
 * ***** DISCLAIMER *****
 *
 * This file is a mess.  There should really be a separate program (like Yacc)
 * to compile a grammar into tables.  But for the time being . . .
 */


/*
 * Values returned by function symbol(n).
 * They are used directly as index in the grammar table.
 * The NAMES of the #defined constants are of no importance outside this file.
 */

#define Put	1
#define Insert	2
#define Remove	3
#define Choose	4
#define Draw	5
#define Set_random	6
#define Delete	7
#define Check	8
#define Share	9

#define Write	10
#define Read	11
#define Read_raw	12

#define If	13
#define While	14
#define For	15

#define Select	16

#define Quit	18
#define Return	19
#define Report	20
#define Succeed	21
#define Fail	22

#define How_to	23
#define Yield	24
#define Test	25
#define Suite	26
#define Refinement	27

#define Compound	28
#define Collateral	29
#define Tag	30
#define Number	31
#define Selection	32
#define Behead	33
#define Curtail	34

#define And	35
#define Or	36
#define Not	37
#define Some_in	38
#define Each_in	39
#define No_in	40
#define Some_parsing	41
#define Each_parsing	42
#define No_parsing	43

#define Comment	44
#define Keyword	45

#define L_t_dis	46
#define List_body	47
#define Tab_body	48
#define Tab_entry	49

#define E_number	50
#define Com_target	51
#define Col_target	52
#define Sel_expr	53
#define Text1	54
#define Text2	55
#define Grouped	56
#define Blocked	57
#define Operators	58

#define Else_kw	59
#define Kw_plus	60
#define E_plus	61
#define Conversion	62
#define T1	63
#define T1_plus	64
#define T2	65
#define T2_plus	66
#define Cmt_cmd	67

#define F_kw_plus	69
#define F_e_plus	70
#define Plus_sign	71
#define Minus_sign	72

#define Long_comp	73
#define Short_comp	74
#define Cmt_comp	75

#define Long_unit	76
#define Short_unit	77
#define Cmt_head	78

#define Ref_join	79

#define And_kw	80
#define Or_kw	81

#define E_part 82

#define Unit_edit	83
#define Target_edit	84
#define Imm_cmd	85
#define Raw	86
#define Raw_input	87
#define Edit_unit	88
#define Edit_target	89
#define Colon	90
#define Equals	91
#define Test_suite	92
#define Expression	93

/*
 * The last three, `Suggestion', `Optional' and `Hole',
 * with values 97, 98 and 99, are defined in "gram.h".
 */


/*
 * Symbol values used for lexical elements.
 * Cross-reference: "lexi.c", table `chclass'.
 */

#define LEXICAL 100

#define IDENT (LEXICAL+0)
#define KEYWORD (LEXICAL+1)
#define NUMBER (LEXICAL+2)
#define COMMENT (LEXICAL+3)
#define TEXT1 (LEXICAL+4)
#define TEXT2 (LEXICAL+5)
#define OPERATORS (LEXICAL+6)
#define RAWINPUT (LEXICAL+7)
#define SUGGESTION (LEXICAL+8)


/*
 * Classes used in table initialization.
 */

Hidden classelem Asugg_body[] = {SUGGESTION, 0};
	Hidden struct classinfo sugg_body[] = {Asugg_body};

#define TARGET Tag, Com_target, Selection, Behead, Curtail
#define PRIMARY \
	Sel_expr, Tag, E_number, Number, Compound, L_t_dis, Text1, Text2
#define EXPR Blocked, Grouped, Operators, PRIMARY

Hidden classelem Atag_body[] = {IDENT, 0};
	Hidden struct classinfo tag_body[] = {Atag_body};
Hidden classelem Anum_body[] = {NUMBER, 0};
	Hidden struct classinfo num_body[] = {Anum_body};
Hidden classelem Acom_body[] = {COMMENT, 0};
	Hidden struct classinfo com_body[] = {Acom_body};
Hidden classelem Akw_body[] = {KEYWORD, 0};
	Hidden struct classinfo kw_body[] = {Akw_body};
Hidden classelem At1_body[] = {TEXT1, 0};
	Hidden struct classinfo t1_body[] = {At1_body};
Hidden classelem At2_body[] = {TEXT2, 0};
	Hidden struct classinfo t2_body[] = {At2_body};
Hidden classelem Aops_body[] = {OPERATORS, 0};
	Hidden struct classinfo ops_body[] = {Aops_body};
Hidden classelem Araw_body[] = {RAWINPUT, 0};
	Hidden struct classinfo raw_body[] = {Araw_body};
Hidden classelem Araw_input[] = {Optional, Raw, 0};
	Hidden struct classinfo raw_input[] = {Araw_input};

Hidden classelem Aid_or_kw[] = {Tag, Keyword, 0};
	Hidden struct classinfo id_or_kw[] = {Aid_or_kw};
Hidden classelem Anumber[] = {Number, 0};
	Hidden struct classinfo number[] = {Anumber};
Hidden classelem Asign[] = {Optional, Plus_sign, Minus_sign, 0};
	Hidden struct classinfo sign[] = {Asign};

Hidden classelem Ao_c_expr[] = {Optional, Collateral, EXPR, 0};
	Hidden struct classinfo o_c_expr[] = {Ao_c_expr};

#define Ac_expr (Ao_c_expr+1)
	Hidden struct classinfo c_expr[] = {Ac_expr};
#define Aexpr (Ao_c_expr+2)
	Hidden struct classinfo expr[] = {Aexpr};
#define Aprimary (Ao_c_expr+5)
	Hidden struct classinfo primary[] = {Aprimary};

Hidden classelem Ablock[] = {Operators, PRIMARY, 0};
	Hidden struct classinfo block[] = {Ablock};
Hidden classelem Agroup[] = {Blocked, Operators, PRIMARY, 0};
	Hidden struct classinfo group[] = {Agroup};

#define Ar_expr Agroup
	Hidden struct classinfo r_expr[] = {Ar_expr};

Hidden classelem Al_t_body[] =	{Optional, List_body, PRIMARY, Blocked, 
	Grouped, Operators, Tab_body, Tab_entry, 0};
	Hidden struct classinfo l_t_body[] = {Al_t_body};
Hidden classelem Alist_body[] = {List_body, EXPR, 0};
	Hidden struct classinfo list_body[] = {Alist_body};
Hidden classelem Atab_body[] = {Tab_body, Tab_entry, 0};
	Hidden struct classinfo tab_body[] = {Atab_body};
Hidden classelem Atab_entry[] = {Tab_entry, 0};
	Hidden struct classinfo tab_entry[] = {Atab_entry};

Hidden classelem Ac_target[] = {Col_target, TARGET, 0};
	Hidden struct classinfo c_target[] = {Ac_target};

#define Atarget (Ac_target+1)
	Hidden struct classinfo target[] = {Atarget};

#define SOME_ETC \
	Not, Some_in, Each_in, No_in, Some_parsing, Each_parsing, No_parsing

Hidden classelem Ae_test[] = {Else_kw, SOME_ETC, And, Or, EXPR, 0};
	Hidden struct classinfo e_test[] = {Ae_test};

#define Atest (Ae_test+1)
	Hidden struct classinfo test[] = {Atest};
#define At_test Aexpr
	Hidden struct classinfo t_test[] = {At_test};
Hidden classelem Ar_test[] = {SOME_ETC, EXPR, 0};
	Hidden struct classinfo r_test[] = {Ar_test};
Hidden classelem Aand_test[] = {SOME_ETC, And, EXPR, 0};
	Hidden struct classinfo and_test[] = {Aand_test};
Hidden classelem Aor_test[] = {SOME_ETC, Or, EXPR, 0};
	Hidden struct classinfo or_test[] = {Aor_test};
Hidden classelem Ac_test[] = {Collateral, SOME_ETC, And, Or, EXPR, 0};
	Hidden struct classinfo c_test[] = {Ac_test};
	/*
	 * This means that a compound expression may in fact
	 * contain a `collateral test', e.g. (a AND b, c AND d).
	 * Of course, this is illegal in B, but I couldn't
	 * solve the ambiguity of `(' where a test is expected
	 * otherwise (this may start a parenthesized test, or
	 * a compound expression; the latter may be followed
	 * by more expression fragments, the first may not).
	 */

Hidden classelem Acomment[] = {Comment, 0};
	Hidden struct classinfo comment[] = {Acomment};
Hidden classelem Ao_comment[] = {Optional, Comment, 0};
	Hidden struct classinfo o_comment[] = {Ao_comment};
	
#define HEAD How_to, Yield, Test
#define BODY HEAD, Cmt_head, Long_unit, Short_unit

/* The order here determines which are suggested first and is subject
   to constant change! */
#define SIMPLE_CMD SC1, SC2, SC3
#define SC1 Share, Quit, Return, Write, Read, Read_raw, Put, Delete
#define SC2 Report, Fail, Succeed, Insert, Remove, Check
#define SC3 Choose, Draw, Set_random, Suggestion, Keyword, Kw_plus

#define CONTROL_CMD If, While, For
#define COMP_CMD Short_comp, Long_comp, Cmt_comp, Select
#define CMD If, For, COMP_CMD, SIMPLE_CMD, While
/* #define SHORTCMD SIMPLE_CMD, Cmt_cmd */
#define SHORTCMD If, For, SIMPLE_CMD, While, Short_comp, Cmt_comp, Cmt_cmd

Hidden classelem Ac_head[] = {Cmt_head, HEAD, 0};
	Hidden struct classinfo c_head[] = {Ac_head};
#define Ahead (Ac_head+1)
	Hidden struct classinfo head[] = {Ahead};

Hidden classelem Aunit[] = {Optional, EXPR, BODY, Ref_join, 0};
	Hidden struct classinfo unit[] = {Aunit};
Hidden classelem Ao_refinements[] = {Optional, Refinement, 0};
	Hidden struct classinfo o_refinements[] = {Ao_refinements};
#define Arefinements (Ao_refinements+1)
	Hidden struct classinfo refinements[] = {Arefinements};
Hidden classelem Arefpred[] = {BODY, 0};
	Hidden struct classinfo refpred[] = {Arefpred};

Hidden classelem Af_cmd[] = {Keyword, F_kw_plus, 0};
	Hidden struct classinfo f_cmd[] = {Af_cmd};
#define Af_formula Aexpr /*****/
	Hidden struct classinfo f_formula[] = {Af_formula};

Hidden classelem Ao_suite[] = {Optional, Suite, 0};
	Hidden struct classinfo o_suite[] = {Ao_suite};
Hidden classelem At_suite[] = {Test_suite, 0};
	Hidden struct classinfo t_suite[] = {At_suite};
Hidden classelem Ao_t_suite[] = {Optional, Test_suite, 0};
	Hidden struct classinfo o_t_suite[] = {Ao_t_suite};

Hidden classelem Acmd[] = {Comment, CMD, Cmt_cmd, 0};
	Hidden struct classinfo cmd[] = {Acmd};
Hidden classelem Ashortcmd[] = {SHORTCMD, 0};
	Hidden struct classinfo shortcmd[] = {Ashortcmd};
Hidden classelem Ao_cmdsuite[] = {Optional, SHORTCMD, Suite, 0};
	Hidden struct classinfo o_cmdsuite[] = {Ao_cmdsuite};
Hidden classelem Asuite[] = {Suite, 0};
	Hidden struct classinfo suite[] = {Asuite};
Hidden classelem Asimple_cmd[] = {SIMPLE_CMD, 0};
	Hidden struct classinfo simple_cmd[] = {Asimple_cmd};

Hidden classelem Ac_ifforwhile[] = {CONTROL_CMD, Cmt_comp, 0};
	Hidden struct classinfo c_ifforwhile[] = {Ac_ifforwhile};
Hidden classelem Aifforwhile[] = {CONTROL_CMD, 0};
	Hidden struct classinfo ifforwhile[] = {Aifforwhile};

Hidden classelem Akeyword[] = {Keyword, 0};
	Hidden struct classinfo keyword[] = {Akeyword};
Hidden classelem Akw_next[] = {Collateral, EXPR, Keyword, E_plus, Kw_plus, 0};
	Hidden struct classinfo kw_next[] = {Akw_next};
Hidden classelem Ae_next[] = {Keyword, Kw_plus, 0};
	Hidden struct classinfo e_next[] = {Ae_next};

Hidden classelem Af_kw_next[] = {Tag, Keyword, F_kw_plus, F_e_plus, 0};
	Hidden struct classinfo f_kw_next[] = {Af_kw_next};
Hidden classelem Af_e_next[] = {Keyword, F_kw_plus, 0};
	Hidden struct classinfo f_e_next[] = {Af_e_next};
Hidden classelem Atag[] = {Tag, 0};
	Hidden struct classinfo tag[] = {Atag};

Hidden classelem Atext1[] = {Optional, T1, Conversion, T1_plus, 0};
	Hidden struct classinfo text1[] = {Atext1};
Hidden classelem At1_conv[] = {T1, Conversion, 0};
	Hidden struct classinfo t1_conv[] = {At1_conv};
Hidden classelem At1_next[] = {T1, Conversion, T1_plus, 0};
	Hidden struct classinfo t1_next[] = {At1_next};

Hidden classelem Atext2[] = {Optional, T2, Conversion, T2_plus, 0};
	Hidden struct classinfo text2[] = {Atext2};
Hidden classelem At2_conv[] = {T2, Conversion, 0};
	Hidden struct classinfo t2_conv[] = {At2_conv};
Hidden classelem At2_next[] = {T2, Conversion, T2_plus, 0};
	Hidden struct classinfo t2_next[] = {At2_next};

Hidden classelem Aand[] = {And_kw, 0};
	Hidden struct classinfo and[] = {Aand};
Hidden classelem Aor[] = {Or_kw, 0};
	Hidden struct classinfo or[] = {Aor};

Hidden classelem Ae_part[] = {E_part, 0};
	Hidden struct classinfo e_part[] = {Ae_part};

Hidden classelem Aunit_edit[] = {Optional, BODY, Ref_join, 0};
	Hidden struct classinfo unit_edit[] = {Aunit_edit};
Hidden classelem Atarget_edit[] = {Optional, EXPR, 0};
	Hidden struct classinfo target_edit[] = {Atarget_edit};
Hidden classelem Aimm_cmd[] = {Optional, Comment, HEAD, CMD, Cmt_cmd, Cmt_head,
	Edit_unit, Edit_target, 0};
	Hidden struct classinfo imm_cmd[] = {Aimm_cmd};

Hidden classelem Aed_unit[] = {Optional, Tag, Keyword, Colon, 0};
	Hidden struct classinfo ed_unit[] = {Aed_unit};
Hidden classelem Aed_target[] = {Optional, Tag, Equals, 0};
	Hidden struct classinfo ed_target[] = {Aed_target};


/*
 * WARNING: The entries in this table must correspond one by one
 * to the symbols defined earlier.  This is checked dynamically
 * by the initialization procedure (syserr "table order").
 */

#define XX(name) name, "name"

Hidden struct table b_grammar[] = {
	{XX(Rootsymbol), {0}, {unit}}, /* Start symbol of the grammar,
			may be overridden by setroot("Blabla") call. */
	{XX(Put), {"PUT ", " IN "}, {c_expr, c_target}},
	{XX(Insert), {"INSERT ", " IN "}, {c_expr, target}},
	{XX(Remove), {"REMOVE ", " FROM "}, {c_expr, target}},
	{XX(Choose), {"CHOOSE ", " FROM "}, {c_expr, expr}},
	{XX(Draw), {"DRAW "}, {target}},
	{XX(Set_random), {"SET'RANDOM "}, {c_expr}},
	{XX(Delete), {"DELETE "}, {c_target}},
	{XX(Check), {"CHECK "}, {test}},
	{XX(Share), {"SHARE "}, {c_target}},

	{XX(Write), {"WRITE "}, {c_expr}},
	{XX(Read), {"READ ", " EG "}, {c_target, c_expr}},
	{XX(Read_raw), {"READ ", " RAW"}, {target}},

	{XX(If), {"IF ", ": "}, {test}},
	{XX(While), {"WHILE ", ": "}, {test}},
	{XX(For), {"FOR ", " IN ", ": "}, {c_target, expr}},

	{XX(Select), {"SELECT: ", "\t", "\b"}, {o_comment, t_suite}},
	{0}, /* Test_suite moved to 92 */

	{XX(Quit), {"QUIT"}, {0}},
	{XX(Return), {"RETURN "}, {c_expr}},
	{XX(Report), {"REPORT "}, {test}},
	{XX(Succeed), {"SUCCEED"}, {0}},
	{XX(Fail), {"FAIL"}, {0}},

	{XX(How_to), {"HOW'TO ", ": "}, {f_cmd}},
	{XX(Yield), {"YIELD ", ": "}, {f_formula}},
	{XX(Test), {"TEST ", ": "}, {f_formula}},

	{XX(Suite), {"\n"}, {cmd, o_suite}},
	{XX(Refinement), {"\n", ": ", "\t", "\b"},
		{id_or_kw, o_comment, o_cmdsuite, o_refinements}},

	{XX(Compound), {"(", ")"}, {c_test}},
	{XX(Collateral), {0, ", "}, {expr, c_expr}},
	{XX(Tag), {0}, {tag_body}},
	{XX(Number), {0}, {num_body}},
	{XX(Selection), {0, "[", "]"}, {target, c_expr}},
	{XX(Behead), {0, "@"}, {target, r_expr}},
	{XX(Curtail), {0, "|"}, {target, r_expr}},

	{XX(And), {0, " "}, {t_test, and}},
	{XX(Or), {0, " "}, {t_test, or}},
	{XX(Not), {"NOT "}, {r_test}},
	{XX(Some_in), {"SOME ", " IN ", " HAS "}, {c_target, expr, r_test}},
	{XX(Each_in), {"EACH ", " IN ", " HAS "}, {c_target, expr, r_test}},
	{XX(No_in), {"NO ", " IN ", " HAS "}, {c_target, expr, r_test}},
	{XX(Some_parsing), {"SOME ", " PARSING ", " HAS "},
		{c_target, expr, r_test}},
	{XX(Each_parsing), {"EACH ", " PARSING ", " HAS "},
		{c_target, expr, r_test}},
	{XX(No_parsing), {"NO ", " PARSING ", " HAS "}, {c_target, expr, r_test}},

	{XX(Comment), {0}, {com_body}},
	{XX(Keyword), {0}, {kw_body}},

	{XX(L_t_dis), {"{", "}"}, {l_t_body}},
	{XX(List_body), {0, "; "}, {expr, list_body}},
	{XX(Tab_body), {0, "; "}, {tab_entry, tab_body}},
	{XX(Tab_entry), {"[", "]: "}, {c_expr, expr}},
	{XX(E_number), {0}, {number, e_part}},

	{XX(Com_target), {"(", ")"}, {c_target}},
	{XX(Col_target), {0, ", "}, {target, c_target}},
	{XX(Sel_expr), {0, "[", "]"}, {primary, c_expr}},

	{XX(Text1), {"'", "'"}, {text1}},
	{XX(Text2), {"\"", "\""}, {text2}},
	{XX(Grouped), {0, " "}, {group, expr}},
	{XX(Blocked), {0}, {block, group}},
	{XX(Operators), {0}, {ops_body}},
	{XX(Else_kw), {"ELSE"}, {0}},
	{XX(Kw_plus), {0, " "}, {keyword, kw_next}},
	{XX(E_plus), {0, " "}, {c_expr, e_next}},
	{XX(Conversion), {"`", "`"}, {o_c_expr}},
	{XX(T1), {0}, {t1_body}},
	{XX(T1_plus), {0}, {t1_conv, t1_next}},
	{XX(T2), {0}, {t2_body}},
	{XX(T2_plus), {0}, {t2_conv, t2_next}},
	{XX(Cmt_cmd), {0, " "}, {simple_cmd, comment}},
	{0},
	{XX(F_kw_plus), {0, " "}, {keyword, f_kw_next}},
	{XX(F_e_plus), {0, " "}, {tag, f_e_next}},
	{XX(Plus_sign), {"+"}, {0}},
	{XX(Minus_sign), {"-"}, {0}},

	{XX(Long_comp), {0, "\t", "\b"}, {c_ifforwhile, suite}},
	{XX(Short_comp), {0, "\t", "\b"}, {ifforwhile, shortcmd}},
	{XX(Cmt_comp), {0}, {ifforwhile, comment}},

	{XX(Long_unit), {0, "\t", "\b"}, {c_head, suite}},
	{XX(Short_unit), {0, "\t", "\b"}, {head, shortcmd}},
	{XX(Cmt_head), {0}, {head, comment}},

	{XX(Ref_join), {0}, {refpred, refinements}},

	{XX(And_kw), {"AND "}, {and_test}},
	{XX(Or_kw), {"OR "}, {or_test}},

	{XX(E_part), {"E"}, {sign, number}},

	/* Alternate root symbols */

	{XX(Unit_edit), {0}, {unit_edit}},
	{XX(Target_edit), {0}, {target_edit}},
	{XX(Imm_cmd), {0}, {imm_cmd}},
	{XX(Raw), {0}, {raw_body}},
	{XX(Raw_input), {0}, {raw_input}},
	{XX(Edit_unit), {":"}, {ed_unit}},
	{XX(Edit_target), {"="}, {ed_target}},
	{XX(Colon), {":"}, {0}},
	{XX(Equals), {"="}, {0}},
	{XX(Test_suite), {"\n", ": ", "\t", "\b"},
		{e_test, o_comment, o_cmdsuite, o_t_suite}},
	{XX(Expression), {0}, {c_expr}},

	/* Spare(s); change Optional and Hole in "gram.h" if you run out. */

	{0}, {0}, {0},

	/* Next three entries must be the last entries of the table. */
	/* (See comments in "gram.c", initgram().) */

	{XX(Suggestion), {0}, {sugg_body}},
	{XX(Optional), {0}, {0}},
	{XX(Hole), {"?"}, {0}},
};

Visible struct table *table= b_grammar;
