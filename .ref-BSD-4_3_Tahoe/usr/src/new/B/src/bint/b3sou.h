/* Copyright (c) Stichting Mathematisch Centrum, Amsterdam, 1985. */

/*
  $Header: b3sou.h,v 1.4 85/08/22 16:44:42 timo Exp $
*/

/* bsou.h: sources */

bool is_unit();			/* enquire if a keyword is a unit */
value permkey();		/* get key of b_perm table */
bool p_exists();		/* enquire if a unit is filed */
value get_pname();		/* get (perm) name of a unit or target */
value tarvalue();		/* value of target */

/* Procedure def_unit(); */	/* enter a keyword into the list of units */
/* Procedure rem_unit() */	/* remove a unit from the internal adm. */
/* Procedure p_name_type(); */	/* get name and type of a unit or target */
/* Procedure special(); */	/* execute a special command */
/* Procedure vs_ifile(); */	/* restore the input file */
/* Procedure getprmnv(); */	/* create the permanent environment */
/* Procedure putprmnv(); */	/* output the permanent environment */
/* Procedure initsou(); */	/* init sources */
/* Procedure lst_uhds(); */	/* listing of unit headings */
