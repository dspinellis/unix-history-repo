/* Copyright (c) Stichting Mathematisch Centrum, Amsterdam, 1985. */

/*
  $Header: b2par.h,v 1.4 85/08/22 16:42:58 timo Exp $
*/

/* General definitions for the parser */

/* contexts: */
#define In_share 's'
#define In_ranger 'q'
#define In_ref 'r'

/* Expressions: */

parsetree expr();
parsetree singexpr();
bool tag_operator();
bool is_b_tag();
/* Procedure selection(); */

/* Targets: */

parsetree targ(); 
/* Procedure tar_trimmed_text(); */

/* Tests: */

parsetree test(); 
parsetree unp_test();
extern bool dya_proposition;

/* Commands: */

parsetree cmd_suite();
/* Procedure suite_command(); */
bool simple_command();
bool control_command();
bool term_com();
bool is_comment();
value tail_line();

/* B units */

parsetree unit();
bool unit_keyword();
parsetree collateral();
parsetree compound();
parsetree idf();
extern literal idf_cntxt;
