/* Copyright (c) Stichting Mathematisch Centrum, Amsterdam, 1984. */
/* $Header: b2fil.h,v 1.1 84/06/28 00:48:48 timo Exp $ */

/* File accessing */

value f_save();			/* temporarily copy a file to somewhere safe */
/* Procedure f_edit(); */	/* call the editor for a file */
/* Procedure f_rename(); */	/* rename a file */
/* Procedure f_delete(); */	/* delete a file */
value f_uname();		/* devise a filename for a unit */
value f_tname();		/* devise a filename for a target */
unsigned f_size();		/* size of a file */
bool f_exists();		/*enquire if a file exists*/
bool f_interactive();		/*enquire if a file is the keyboard/screen*/
/* Procedure lst_uhds(); */	/*list the headings of units*/

#define FHW '\''
#define FZR '<'
#define FMN '"'
#define FDY '>'
