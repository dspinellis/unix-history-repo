/* Copyright (c) Stichting Mathematisch Centrum, Amsterdam, 1985. */

/*
  $Header: b2tcP.h,v 1.4 85/08/22 16:43:20 timo Exp $
*/

/* polytype representation */

typedef value typekind;
typedef value polytype;

/* accessing, NOT giving new values */

typekind kind(); 	/* polytype u */
intlet nsubtypes(); 	/* polytype u */
polytype subtype(); 	/* polytype u, intlet i */
polytype asctype(); 	/* polytype u */
polytype keytype(); 	/* polytype u */
value ident(); 		/* polytype u */

/* MaKe Types, where subtypes are "eaten" */

polytype mkt_polytype(); /* typekind k; intlet nsub */
				/* visible only in bunif.c */
/* Procedure putsubtype(); */	/* polytype sub, *pcomp; intlet isub */
				/* to be used after mkt_polytype or
				 * mkt_compound */

polytype mkt_number();
polytype mkt_text();
polytype mkt_tn();
polytype mkt_error();
polytype mkt_list(); 	/* polytype s */
polytype mkt_table();	/* polytype k, a */
polytype mkt_lt(); 	/* polytype s */
polytype mkt_tlt(); 	/* polytype s */
/* next to be used with putsubtype() calls */
polytype mkt_compound();	/* intlet nsub */
polytype mkt_var(); 	/* value id */
polytype mkt_newvar();

polytype p_copy(); 	/* polytype u */
/* Procedure p_release(); */		/* polytype u */

/* predicates */

bool are_same_types(); 	/* polytype u, v */
bool have_same_structure();/* polytype u, v */

bool t_is_number();	/* typekind k */
bool t_is_text();	/* typekind k */
bool t_is_tn();		/* typekind k */
bool t_is_error();	/* typekind k */
bool t_is_list(); 	/* typekind k */
bool t_is_table();	/* typekind k */
bool t_is_lt(); 		/* typekind k */
bool t_is_tlt();		/* typekind k */
bool t_is_compound();	/* typekind k */
bool t_is_var();		/* typekind k */
bool has_number(); 	/* typekind k */
bool has_text(); 	/* typekind k */
bool has_lt();	 	/* typekind k */

/* typetable */

/* Procedure repl_type_of(); */ /* polytype u, tu */
bool table_has_type_of();	/* polytype u */
polytype type_of(); 		/* polytype u */
polytype bottom_var(); 		/* polytype u */

/* Procedure usetypetable(); */		/* value t */
/* Procedure deltypetable(); */

/* init */

/* Procedure initpol(); */ 	/* */
