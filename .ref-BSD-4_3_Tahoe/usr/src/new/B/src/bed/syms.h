/* Copyright (c) Stichting Mathematisch Centrum, Amsterdam, 1985. */
/* $Header: syms.h,v 1.1 85/08/22 15:44:34 timo Exp $ */

/*
 * B editor -- Grammar symbol definitions.
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
