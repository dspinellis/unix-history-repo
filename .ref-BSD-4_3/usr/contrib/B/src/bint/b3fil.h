/* Copyright (c) Stichting Mathematisch Centrum, Amsterdam, 1985. */

/*
  $Header: b3fil.h,v 1.4 85/08/22 16:43:58 timo Exp $
*/

/* File accessing */

bool ws_writable();		/* enquire if write permission in workspace */
unsigned f_size();		/* size of a file */
bool f_exists();		/* enquire if a file exists */
value f_save();			/* temporarily copy a file to somewhere safe */
#ifndef INTEGRATION
bool f_interactive();		/* enquire if a file is the keyboard/screen */
#endif
value new_fname();		/* devise a filename for a unit or target */

extern value file_names;	/* list of file names */

/* Procedure f_edit(); */	/* call the editor for a file */
/* Procedure f_rename(); */	/* rename a file */
/* Procedure f_delete(); */	/* delete a file */
