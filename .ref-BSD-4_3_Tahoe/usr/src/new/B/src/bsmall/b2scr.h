/* Copyright (c) Stichting Mathematisch Centrum, Amsterdam, 1984. */
/* $Header: b2scr.h,v 1.1 84/06/28 00:48:49 timo Exp $ */

/* bscr.h: screen */
extern bool interactive, read_interactive, outeractive, filtered;
extern bool Eof, Eof0;
extern FILE *ifile;
extern value iname;

extern jmp_buf reading[];
#define MAX_NMB_ACT_READS 10
extern intlet active_reads;

bool is_intended();
bool at_nwl;
/* Procedure redirect(); */
/* Procedure newline(); */
/* Procedure line(); */
/* Procedure wri_space(); */
/* Procedure writ(); */
/* Procedure wri(); */

extern FILE *sv_ifile;	/*TEMPORARY for syn*/
/* Procedure re_files(); */
/* Procedure vs_ifile(); */

/* Procedure initscr(); */
/* Procedure re_screen(); */
extern string cmd_prompt, eg_prompt, raw_prompt, qn_prompt;
