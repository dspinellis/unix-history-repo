/* Copyright (c) Stichting Mathematisch Centrum, Amsterdam, 1985. */

/*
  $Header: b2exp.h,v 1.4 85/08/22 16:42:25 timo Exp $
*/

/* General definitions for parsing expressions */

typedef struct { bool parsed, prop, trim;
		 intlet state, level, field;
		 value comp;
		} expadm;

typedef struct { bool prop, trim;
		 intlet level, field;
		 parsetree node;
		} unpadm;

#define Parsed(adm)	((adm)->parsed)
#define Prop(adm)   	((adm)->prop)
#define Trim(adm)	((adm)->trim)
#define State(adm)	((adm)->state)
#define Level(adm)	((adm)->level)
#define N_fld(adm)	((adm)->field)
#define Unp_comp(adm)	((adm)->comp)
#define Node(adm)	((adm)->node)

/* ********************************************************************	*/
/* Levels:								*/
/*									*/
#define L_bottom	0
#define L_term		1 /* plus, minus, join */
#define L_factor	2 /* times, over */
#define L_power		3 /* power */
#define L_number	4 /* number  */
#define L_expr		5 /* tag, repeat_text, center, (left|right)_adjust */

#define Prio \
	MESS(1900, "cannot determine priorities; use ( and ) to resolve")

/* ******************************************************************** */
/* States:								*/
/*									*/
#define S_t	1
#define S_tt	2
#define S_else	3

/* ******************************************************************** */

bool b_about();
bool b_numtor();
bool b_denomtor();
bool b_plus();
bool b_minus();
bool b_number();
bool b_behead();
bool b_curtail();
#ifdef NOT_USED
bool b_times();
bool b_over();
bool b_power();
bool b_join();
bool b_reptext();
bool b_center();
bool b_leftadj();
bool b_rightadj();
#endif
