/* Copyright (c) Stichting Mathematisch Centrum, Amsterdam, 1985. */

/*
  $Header: b3scr.h,v 1.4 85/08/22 16:44:19 timo Exp $
*/

/* screen */

extern bool interactive;
extern bool rd_interactive;
extern value iname;
extern bool filtered;
#ifndef INTEGRATION
extern string cmd_prompt, eg_prompt, raw_prompt, qn_prompt;
#endif
#ifdef INTEGRATION
extern literal unit_prompt, tar_prompt;
#endif
extern FILE *ifile;
extern FILE *sv_ifile;	/*TEMPORARY for syn*/
extern bool outeractive, Eof, at_nwl;
bool is_intended();
txptr getline();

/* Procedure redirect(); */
/* Procedure newline(); */
/* Procedure wri_space(); */
/* Procedure writ(); */
/* Procedure wri(); */
/* Procedure re_files(); */
/* Procedure vs_ifile(); */
/* Procedure initscr(); */
/* Procedure re_screen(); */

