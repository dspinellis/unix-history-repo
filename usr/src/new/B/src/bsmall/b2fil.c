/* Copyright (c) Stichting Mathematisch Centrum, Amsterdam, 1984. */
/* $Header: b2fil.c,v 1.1 84/06/28 00:49:11 timo Exp $ */

/* Facilities supplied by the file system */

#include "b.h"
#include "b0con.h"
#include <sys/types.h>
#include <sys/stat.h>
#include "b1obj.h"
#include "b2fil.h"
#include "b2scr.h"

/*This file defines the facilities needed for dealing with files,
  apart from C's standard I/O facilities which are used throughout the system.

  Units are held on files in a 'workspace', which on Unix is modelled
  using directories. The function 'f_uname' converts a unit name into a
  unique filename. On Unix this is done by prepending a character to the unit
  name to indicate the kind of unit (for how'to ', and for tests and yields
  < for zeroadic, " for monadic and > for dyadic; these have been chosen as
  characters that are not usually used in filenames), and truncating the
  name if necessary. If the name does have to be truncated, then it is
  hashed to produce a character that is appended to the filename, in an attempt
  to produce a unique filename. Even so, it is still possible for different
  unit names to produce the same filename, and in the unlikely event of this
  happening you get an error message that the unit already exists when you
  try to create the clashing unit name.

  Filenames are at most SAFEFNLEN characters long, which on standard Unix
  systems gives you one spare character for making backups or whatever.

  It would be better if the B system effectively maintained its own directories
  that mapped units onto files in the real file system, as is done for targets.
  With operating systems with a more limited file system (eg even shorter
  filenames) this is the only possibility.

*/

#define COML 60
char com_line[COML];
#define At_eos(s) ((s)+= strlen(s))

Visible Procedure f_edit(fname, errline) value fname; intlet errline; {
	/*The default editor is called with a first parameter of the line number
	  and a second parameter of the file name*/
	string cl= com_line;
	int c;
	if (filtered) {
		ignsigs();
		printf("\001: +%d %s\n", errline, strval(fname));
		fflush(stdout);
		do { c= fgetc(stdin); } while (c != '\n' && c != EOF);
		re_sigs();
		return;
	}
	if (getenv("BEDITOR") == NULL) strcpy (cl, DEDI);
	else strcpy(cl, getenv("BEDITOR"));
	if (*(cl+strlen(cl)-1) == '+') {
		if (errline != 0) sprintf(At_eos(cl), "%d", errline);
		else *(cl+strlen(cl)-1)= ' ';
	}
	app_fname(At_eos(cl), fname);
	system(com_line);
}

Visible value f_save(fname) value fname; {
	/* saves the file in a temporary file, whose name is returned */
	/* Here the OS does the copy: you may have to do this yourself */
	string cl; value sname= mk_text(SAVEFILE);
	strcpy(cl= com_line, "cp");
	app_fname(At_eos(cl), fname);
	app_fname(At_eos(cl), sname);
	system(com_line);
	return sname;
}

Visible Procedure f_rename(fname, nfname) value fname, nfname; {
	string cl;
	strcpy(cl= com_line, "mv");
	app_fname(At_eos(cl), fname);
	app_fname(At_eos(cl), nfname);
	system(com_line);
	/* what if mv fails??? */
}

Visible Procedure f_delete(fname) value fname; {
	string cl;
	strcpy(cl= com_line, "rm");
	app_fname(At_eos(cl), fname);
	system(com_line);
}

Visible bool f_exists(fname) value fname; {
	FILE *f= fopen(strval(fname), "r");
	if (f==NULL) return No;
	fclose(f);
	return Yes;
}

#define SAFEFNLEN 13

Hidden double f_hash(v) value v; {
	int len= length(v), k; double d= '"'+.404*len;
	/*That '"' is strange I know, but it's necessary for compatibility*/
	value ch;
	k_Over_len {
		ch= thof(k+1, v);
		d= .987*d+.277*charval(ch);
		release(ch);
	}
	return d;
}

Visible value f_uname(name, type) value name; literal type; {
	static char fname[SAFEFNLEN+1]; string sfn= fname;
	*sfn= type;
	if (length(name) < SAFEFNLEN) strcpy(sfn+1, strval(name));
	else {
		double hh= f_hash(name)*321.987; char h;
		strncpy(sfn+1, strval(name), SAFEFNLEN-2);
		hh= hh-floor(hh); h= (char) floor(hh*29) + '!';
		if (h >= '"') h++;
		if (h >= '\'') h++;
		if (h >= '/') h++;
		if (h >= '0') h+= 10;
		if (h >= 'A') h+= 26;
		if (h >= 'a') h+= 26;
		*(sfn+SAFEFNLEN-1)= h; *(sfn+SAFEFNLEN)= '\0';
	}
	return mk_text(fname);
}

Hidden value try(t, n) value t; intlet n; {
	value eq= mk_text("="), fn, a, b, c;
	int len= length(t);
	if (n == 0) fn= concat(eq, t);
	else { /* PUT "="^(n<<1)^t IN fn */
		fn= concat(eq, a= concat(b= convert(c= mk_integer(n), No, No), t));
		release(a); release(b); release(c);
	}
	release(eq);
	if (len > SAFEFNLEN) {
		fn= trim(a= fn, 0, len-SAFEFNLEN);
		release(a);
	}
	return(fn);
}

Visible value f_tname(t) value t; {
	value fn= try(t, 0);
	intlet i= 0;
	while (f_exists(fn)) {
		release(fn);
		fn= try(t, ++i);
	}
	return(fn);
}

Hidden Procedure app_fname(ceos, fname) string ceos; value fname; {
	string fp= strval(fname); intlet k, len= strlen(fp);
	*ceos++= ' ';
	k_Over_len {
		*ceos++= '\\';
		*ceos++= *fp++; /*should really use charval(thof(...))*/
	}
	*ceos= '\0';
}

Visible unsigned f_size(ifile) FILE *ifile; {
	struct stat sb;
	if (fstat(fileno(ifile), &sb) != 0) syserr("can't stat file");
	return( (unsigned) (sb.st_size) );
}

Visible bool f_interactive(ifile) FILE *ifile; {
	return isatty(fileno(ifile));
}

Visible Procedure lst_uhds() {
	/*List the headings of the units in this workspace*/
	system("for i in \\'* \\<* \\\"* \\>*; do head -1 $i 2>/dev/null; done");
	/*just for now, you understand*/
}
