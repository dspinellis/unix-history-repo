/* Copyright (c) Stichting Mathematisch Centrum, Amsterdam, 1984. */
/* $Header: b2sou.h,v 1.1 84/06/28 00:48:51 timo Exp $ */

/* bsou.h: sources */

extern value aster;

bool unit();			/* get a new unit */
bool is_unit();			/*enquire if a keyword is a unit */
/* Procedure def_unit(); */	/*enter a keyword into the list of units*/
value unit_info();		/*retrieve the details of a unit*/
/* Procedure special(); */	/*execute a special command*/
/* Procedure vs_ifile(); */	/*restore the input file*/

bool is_tloaded();		/*ensure a permanent target is loaded*/
/* Procedure getval(); */	/*get a value from a file*/
/* Procedure getprmnv(); */	/*create the permanent environment*/
/* Procedure putprmnv(); */	/*output the permanent environment*/

/* Procedure initsou(); */
