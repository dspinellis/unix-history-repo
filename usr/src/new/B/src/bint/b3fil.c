/* Copyright (c) Stichting Mathematisch Centrum, Amsterdam, 1985. */

/*
  $Header: b3fil.c,v 1.4 85/08/27 10:56:00 timo Exp $
*/

/* Facilities supplied by the file system */

#include "b.h"
#include "b0con.h"
#include "b0fea.h"
#include "b0fil.h"
#include "b1obj.h"
#include "b3scr.h"
#include "b3err.h"
#include "b3fil.h"

#ifndef INTEGRATION

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

  The B system can operate in two ways: with the interpreter in command,
  and then the editor is called from the interpreter to edit units;
  and with the editor in command, when the editor calls the interpreter to
  execute commands. The variable 'filtered' is Yes when the editor is in
  command, and No otherwise.
*/

#define COML 60
Hidden char com_line[COML];
#define At_eos(s) ((s)+= strlen(s))

Visible Procedure f_edit(fname, errline) value fname; intlet errline; {
	/*The default editor is called with a first parameter of the line number
	  and a second parameter of the file name*/
	string cl= com_line; char c;
#ifdef unix
	if (filtered) {
		printf("\001: +%d %s\n", errline, strval(fname));
		fflush(stdout);
		do { c= fgetc(stdin); } while (c != '\n');
		still_ok= Yes; /*ignore interrupts that occurred*/
	} else {
		strcpy(cl, editorfile);
		if (*(cl+strlen(cl)-1) == '+') {
			if (errline != 0) sprintf(At_eos(cl), "%d", errline);
			else *(cl+strlen(cl)-1)= ' ';
		}
		app_fname(At_eos(cl), fname);
		system(com_line);
	}
#else !unix
	fprintf(stderr, "*** Editing units not yet implemented\n");
#endif unix
}

#else INTEGRATION

Visible Procedure
f_edit(fname, errline, prompt)
	value fname; intlet errline; literal prompt;
{
	string filename= Str(fname);
	btop(&filename, errline, prompt, 0);
	still_ok= Yes;
}

#endif

Visible bool ws_writable() {
	FILE *f= fopen(tempfile, "w");
	if (f == NULL) return No;
	fclose(f);
	return Yes;
}

Hidden bool f_copy(fname, sname) value fname, sname; {
	string fn= strval(fname), sn;
	FILE *fp= fopen(fn, "r"), *sp; int c; bool ok;
	if (fp == NULL) return No;
	sn= strval(sname);
	sp= fopen(sn, "w");
	if (sp == NULL) {
		fclose(fp);
		return No;
	}
	while ((c= getc(fp)) != EOF)
		putc(c, sp);
	fclose(fp);
	ok= fflush(sp) != EOF;
	if (fclose(sp) == EOF)
		ok= No;
	return ok;
}

Visible value f_save(fname) value fname; {
	/* saves the file in a temporary file, whose name is returned */
	value sname= mk_text(tempfile);
	VOID f_copy(fname, sname);
	return sname;
}

Visible Procedure f_rename(fname, nfname) value fname, nfname; {
	char *f1, f2[100];
	strcpy(f2, strval(nfname));
	unlink(f2);
	f1= strval(fname);
#ifndef RENAME
	link(f1, f2);
	unlink(f1);
#else
	rename(f1, f2);
#endif
	/* what if it fails??? */
}

Visible Procedure f_delete(fname) value fname; {
	unlink(strval(fname));
}

Visible bool
f_exists(file)
	string file;
{
	FILE *f= fopen(file, "r");
	if (f==NULL) return No;
	fclose(f);
	return Yes;
}

#ifndef INTEGRATION

Hidden Procedure app_fname(ceos, fname) string ceos; value fname; {
	string fp= strval(fname); intlet k, len= strlen(fp);
	*ceos++= ' ';
	k_Over_len {
		*ceos++= '\\';
		*ceos++= *fp++; /*should really use charval(thof(...))*/
	}
	*ceos= '\0';
}

#endif

Visible unsigned f_size(ifile) FILE *ifile; {
	long size, ftell();
	fseek(ifile, 0l, 2);
	size= ftell(ifile);
	fseek(ifile, 0l, 0); /* rewind */
	return size;
}

Visible Procedure f_close(ofile) FILE *ofile; {
	bool ok= fflush(ofile) != EOF;
	if (fclose(ofile) == EOF || !ok)
		error(MESS(3203, "write error (disk full?)"));
}

Visible bool f_interactive(ifile) FILE *ifile; {
#ifdef ISATTY
	return isatty(fileno(ifile));
#else
	return fileno(ifile) < 3;
#endif
}

#ifdef IBMPC

#define FNMLEN 8
#define TYPLEN 3
#define SPCLEN 1

#define FHW "how"
#define FZR "zer"
#define FMN "mon"
#define FDY "dya"
#define FTR "tar"

Hidden string
filetype(type)
	literal type;
{
	switch (type) {
		case Zer:	return FZR;
		case Mon:	return FMN;
		case Dya:	return FDY;
		case How:	return FHW;
		case Tar:	return FTR;
		default:	syserr(MESS(3200, "filetype()"));
				/* NOTREACHED */
	}
}

Hidden Procedure
cr_fname(name, type, fname, len, pname)
	value name; string fname, *pname; literal type; int len;
{
	*pname= fname;
	strncpy(*pname, strval(name), len);
	sprintf(fname + len, ".%s", filetype(type));
}

#endif IBMPC

#ifdef unix

#define FNMLEN 12
#define TYPLEN 1
#define SPCLEN 0

#define FHW '\''
#define FZR '<'
#define FMN '"'
#define FDY '>'
#define FTR '='

Hidden literal
filetype(type)
	literal type;
{
	switch (type) {
		case Zer:	return FZR;
		case Mon:	return FMN;
		case Dya:	return FDY;
		case How:	return FHW;
		case Tar:	return FTR;
		default:	syserr(MESS(3201, "filetype()"));
				/* NOTREACHED */
	}
}

Hidden Procedure
cr_fname(name, type, fname, len, pname)
	value name; string fname, *pname; literal type; int len;
{
	*fname= filetype(type);
	fname[1]= '\0';
	*pname= fname + 1;
	strncat(fname, strval(name), len);
}

#endif unix

Hidden bool
exists(name)
	string name;
{
	value v= mk_text(name);
	bool exist= in(v, file_names);
	release(v);
	return exist;
}

Visible value
new_fname(name, type)
	value name; literal type;
{
	char fname[FNMLEN + TYPLEN + SPCLEN + 1];
	intlet len= length(name);
	string pname;
	if (len > FNMLEN) len= FNMLEN;
	cr_fname(name, type, fname, len, &pname);
	while (exists(fname)) new(pname, len-1);
	return mk_text(fname);
}

#include <ctype.h>

Hidden Procedure
new(name, n)
	string name; int n;
{
	if (n < 1) error(MESS(3202, "too many units"));
	else if (!isdigit(name[n])) name[n]= '1';
	else if (name[n] != '9') ++name[n];
	else {
		name[n]= '0';
		new(name, --n);
	}
}
