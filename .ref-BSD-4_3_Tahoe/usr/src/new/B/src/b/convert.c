/* Copyright (c) Stichting Mathematisch Centrum, Amsterdam, 1985. */
/* $Header: convert.c,v 1.1 85/08/22 14:33:43 timo Exp $ */

/*
 * Utility to recover the contents of a B workspace.
 *
 * Call as: convert [\'\"\<\>=]?* (this should be done by a shell script).
 *
 * It constructs a completely new ".b_perm" file with references to all files
 * mentioned (if they exist).
 * Files whose name starts with '=' and files with an extension of ".tar"
 * or ".TAR" are taken to be targets; all others are assumed to contain
 * units (if they contain garbage, they are ignored).  (".tar" and ".TAR"
 * are used on the IBM-PC.)
 * For units, the name, type and adicity are extracted from the source;
 * for targets, the target name is taken to be the file name with all
 * illegal characters removed (upper case converted to lowercase).
 *
 * BUGS:
 * - should augment the old ".b_perm" file instead of just overwriting it;
 * - target names can get truncated when the original target name was longer
 *   than what fits in a legal file name.
 * - doesn't detect multiple files defining the same name
 * - on the IBM-PC, the file name expansion should be in the program for it to
 *   be of any use, because the MS-DOS "shell" doesn't expand * in file names
 *
 * $Log:	convert.c,v $
 * Revision 1.1  85/08/22  14:33:43  timo
 * Initial revision
 * 
 * Revision 1.3.2.1  85/06/04  13:36:04  guido
 * Create consistent branch 1.3.2.
 * 
 * Revision 1.3  85/06/04  13:35:33  guido
 * Added MS-DOS support.
 * 
 * Revision 1.2  85/04/12  22:33:33  guido
 * added treatment of targets; added warning and fatal error messages.
 * 
 * Revision 1.1  85/01/29  12:48:09  guido
 * used given file names instead of constructing file names from unit names.
 *
 * Revision --- frank
 * created.
 */

#include <stdio.h>

#ifdef MSDOS
#define BPERMFILE "PERM.BIF"
#define DELIM '\\'
#define index strchr
#define rindex strrchr
#endif

#ifdef unix
#define BPERMFILE ".b_perm"
#define DELIM '/'
#endif

char *rindex(), *index();

#ifdef MSDOS
char *defargv[]= {"convert", "*.how", "*.zer", "*.mon", "*.dya", "*.tar", 0};
#define defargc (sizeof defargv/sizeof defargv[0] - 1)
#endif

FILE *ofile;

char buffer[BUFSIZ];
char *pbuf, *first, *last, *filename;
char *progname = "convert";
int status= 0;

#define Oputc(c) putc(c, ofile)

/*VARARGS1*/
warning(fmt, a1, a2)
	char *fmt;
{
	status= 1;
	fprintf(stderr, "*** %s: ", progname);
	fprintf(stderr, fmt, a1, a2);
	fprintf(stderr, "\n");
}

/*VARARGS2*/
quit(sts, msg, a1, a2)
	int sts;
	char *msg;
{
	warning(msg, a1, a2);
	exit(sts ? sts : status);
}

main(argc, argv)
	int argc; char **argv;
{
#ifndef MSDOS
	if (argc < 2)
		quit(2, "Usage: %s file ...", progname);
	/* Don't do this under MSDOS -- program name is always 'c'... */
	progname= rindex(*argv, '/');
	if (progname)
		++progname;
	else
		progname= *argv;
#endif

	ofile= fopen(BPERMFILE, "w");
	if (!ofile)
		quit(2, "%s: can't create %s", progname, BPERMFILE);
	Oputc('{');
#ifdef defargc
	if (argc <= 1) {
		argv= defargv;
		argc= defargc;
	}
#endif
	while (argc > 1) {
		--argc; ++argv;
		treat_file(*argv);
	}
	Oputc('}');
	Oputc('\n');
	fclose(ofile);
	exit(status);
}

treat_file(file)
	char *file;
{
	FILE *ifile;
	static int recursive= 0;

	ifile= fopen(filename= file, "r");
	if (!ifile) {
		if (index(filename, '*') && !recursive) {
			/* Assume failed '*' expansion */
			/* Ignore on UNIX, expand on MSDOS */
#ifdef MSDOS
			char **list= getfiles(filename);
			if (list) {
				recursive= 1;
				for (; *list; ++list)
					treat_file(*list);
				recursive= 0;
			}
#endif
		}
		else
			warning("%s: can't read", filename);
	}
	else {
		if (!fgets(buffer, BUFSIZ, ifile))
			warning("%s: empty file", filename);
		else if (is_target(filename))
			treat_target();
		else
			treat_line();
		fclose(ifile);
	}
}

#define CapLetter ('A' <= *pbuf && *pbuf <= 'Z')
#define Letter ('a' <= *pbuf && *pbuf <= 'z')
#define Digit ('0' <= *pbuf && *pbuf <= '9')
#define Quote (*pbuf == '\'' || *pbuf == '"')
#define Colon (*pbuf == ':')
#define Open (*pbuf == '(')
#define Close (*pbuf == ')')
#define Space (*pbuf == ' ' || *pbuf == '\t')
#define Tagmark (Letter || Digit || Quote)
#define Keymark (CapLetter || Digit || Quote)

#define ToLower(c) ((c) - 'A' + 'a')

skip_tag()
{
	first= pbuf;
	while (Tagmark) pbuf++;
	last= pbuf;
}

skip_keyword()
{
	first= pbuf;
	while (Keymark) pbuf++;
	last= pbuf;
}

skip_space()
{
	while (Space) pbuf++;
}

skip_open_close()
{
	if (Open) { skip_to_close(); pbuf++; }
}

skip_to_close()
{
	while (++pbuf, !Close) if (Open) skip_to_close();
}

treat_line()
{
	pbuf= buffer;
	switch (*pbuf) {
		case 'H': howto_unit(); break;
		case 'Y':
		case 'T': funprd_unit(); break;
		default: warning("%s: not a valid B unit", filename); break;
	}
}

#define Zer 0
#define Mon 1
#define Dya 2
#define How 3
#define Tar 4

howto_unit()
{
	skip_keyword();
	skip_space();
	skip_keyword();
	put_entry(How);
}

funprd_unit()
{
	skip_keyword();
	skip_space();
	if (Letter) {
		skip_tag();
		skip_space();
		if (Colon) put_entry(Zer);
		else if (!Letter) put_entry(Mon);
		else {
			char *sv_first= first, *sv_last= last;
			skip_tag();
			skip_space();
			if (Colon) {
				first= sv_first; last= sv_last;
				put_entry(Mon);
			} else
				put_entry(Dya);
		}
	} else {
		skip_open_close(); 
		skip_space();
		skip_tag();
		put_entry(Dya);
	}		
}

treat_target()
{
	pbuf= filename;
#ifdef MSDOS
	if (index(pbuf, ':'))
		pbuf= index(pbuf, ':') + 1;
#endif
	if (rindex(pbuf, DELIM))
		pbuf= index(pbuf, DELIM) + 1;
	first= last= buffer;
	while (*pbuf && !Letter && !CapLetter)
		++pbuf;
	for (; *pbuf; ++pbuf) {
		if (CapLetter)
			*last++= ToLower(*pbuf);
		else if (Letter || Digit || Quote)
			*last++= *pbuf;
		else if (*pbuf == '.')
			break; /* Stop before extension ".tar" or ".TAR" */
	}
	if (last == first)
		warning("%s: cannot deduce target name", filename);
	else
		put_entry(Tar);
}

put_entry(type)
	int type;
{
	static int first_elem= 1;
	if (!first_elem) Oputc(';');
	else first_elem= 0;
	Oputc('[');
	put_key(type);
	Oputc(']');
	Oputc(':');
	put_assoc(type);
}

#define Text_quote '"'
#define Back_quote '`'
#define Double(c) if ((c) == Text_quote || (c) == Back_quote) Oputc(c)

put_key(type)
	int type;
{
	char *p= first;
	Oputc(Text_quote);
	switch (type) {
		case Zer: Oputc('0'); break;
		case Mon: Oputc('1'); break;
		case Dya: Oputc('2'); break;
		case How: Oputc('3'); break;
		case Tar: Oputc('4'); break;
		default: break;
	}
	while (p < last) {
		Double(*p);
		Oputc(*p++);
	}
	Oputc(Text_quote);
}

put_assoc(type)
	int type;
{
	char *p= filename;
	Oputc(Text_quote);
	while (*p != '\0') {
		Double(*p);
		Oputc(*p++);	
	}
	Oputc(Text_quote);
}

is_target(name)
	char *name;
{
	if (*name == '=')
		return 1;
	name= rindex(name, '.');
	if (!name)
		return 0;
	return strcmp(name, ".TAR") == 0 || strcmp(name, ".tar") == 0;
}
