#ifndef lint
static char sccsid[] = "@(#)main.c	4.1 (Berkeley) 7/3/83";
#endif

#include "Courier.h"
#include <errno.h>

char *input_file;
char hfile[MAXSTR], cfile1[MAXSTR], cfile2[MAXSTR];
char ufile[MAXSTR], sfile[MAXSTR];
FILE *hf, *cf1, *cf2, *uf, *sf;
list Values, Types;
int errs;
int explicit = 0;	/* if true, generate stubs with explicit bindings */

/*
 * Predefined types.
 */
struct object
	*Boolean_type,
	*Cardinal_type, *LongCardinal_type,
	*Integer_type, *LongInteger_type,
	*String_type,
	*Unspecified_type, *LongUnspecified_type;

struct object
	*Undefined_constant;

main(argc, argv)
	int argc;
	char **argv;
{
	argc--; argv++;
	if (argc == 2 && strcmp(*argv, "-x") == 0) {
		explicit = 1; argc--; argv++;
	}
	if (argc != 1) {
		fprintf(stderr, "Usage: courier [-x] input_file\n");
		exit(1);
	}
	input_file = *argv;
	if (freopen(input_file, "r", stdin) == NULL) {
		perror(input_file); exit(1);
	}
	tempname(hfile); tempname(ufile); tempname(sfile);
	tempname(cfile1); tempname(cfile2);
	if ((hf = fopen(hfile, "w")) == NULL) {
		perror(hfile); goto bad;
	}
	if ((uf = fopen(ufile, "w")) == NULL) {
		perror(ufile); goto bad;
	}
	if ((sf = fopen(sfile, "w")) == NULL) {
		perror(sfile); goto bad;
	}
	if ((cf1 = fopen(cfile1, "w")) == NULL) {
		perror(cfile1); goto bad;
	}
	if ((cf2 = fopen(cfile2, "w")) == NULL) {
		perror(cfile2); goto bad;
	}
	setup_predefs();
	(void) yyparse();
	fclose(hf); fclose(uf); fclose(sf);
	if (errs == 0) {
		int c;

		freopen(cfile2, "r", cf2);
		while ((c = getc(cf2)) != EOF)
			putc(c, cf1);
		fclose(cf1); fclose(cf2);
		unlink(cfile2);
		rename(cfile1, program_name, "_stubs.c", 0);
		rename(ufile, program_name, "_client.c", 0);
		rename(sfile, program_name, "_server.c", 0);
		rename(hfile, program_name, ".h", 1);
		exit(0);
	}
	fclose(cf1); fclose(cf2);
bad:
	unlink(hfile);
	unlink(ufile);
	unlink(sfile);
	unlink(cfile1);
	unlink(cfile2);
	exit(1);
}

/*
 * Rename the source file to be <dest>.<suffix> .
 * If we're being paranoid, we prepend # to existing files that might
 * get clobbered.
 */
rename(source, dest, suffix, paranoid)
	char *source, *dest, *suffix;
	int paranoid;
{
	char newname[MAXSTR], backup[MAXSTR];
	extern int errno;

	sprintf(newname, "%s%s", dest, suffix);
	for (;;)
		if (link(source, newname) == 0)
			if (unlink(source) != 0) {
				perror(newname);
				return (-1);
			} else
				return (0);
		else if (errno != EEXIST ||
			 (paranoid && rename(newname, "#", newname) != 0) ||
			 (! paranoid && unlink(newname) != 0))
				break;
	perror(newname);
	return (-1);
}

/* VARARGS1 */
yyerror(s, args)
	char *s;
{
	extern int yylineno;

	errs = 1;
	fprintf(stderr, "%d: ", yylineno);
	_doprnt(s, &args, stderr);
	putc('\n', stderr);
	unlink(hfile); unlink(ufile); unlink(sfile);
	unlink(cfile1); unlink(cfile2);
}

tempname(buf)
	char *buf;
{
	static int n = 0;

	sprintf(buf, "tmp%d.%d", n, getpid());
	n++;
}

/*
 * This mess is needed because C doesn't handle initialization of unions.
 */
setup_predefs()
{
	struct object *t;

	Boolean_type = make(O_SYMBOL, "Boolean");
	t = make(O_TYPE, C_PREDEF);
	t->t_pfname = "PackBoolean";
	t->t_ufname = "UnpackBoolean";
	declare(&Values, Boolean_type, t);

	Cardinal_type = make(O_SYMBOL, "Cardinal");
	t = make(O_TYPE, C_PREDEF);
	t->t_pfname = "PackCardinal";
	t->t_ufname = "UnpackCardinal";
	declare(&Values, Cardinal_type, t);

	LongCardinal_type = make(O_SYMBOL, "LongCardinal");
	t = make(O_TYPE, C_PREDEF);
	t->t_pfname = "PackLongCardinal";
	t->t_ufname = "Unpack_LongCardinal";
	declare(&Values, LongCardinal_type, t);

	Integer_type = make(O_SYMBOL, "Integer");
	t = make(O_TYPE, C_PREDEF);
	t->t_pfname = "PackInteger";
	t->t_ufname = "UnpackInteger";
	declare(&Values, Integer_type, t);

	LongInteger_type = make(O_SYMBOL, "LongInteger");
	t = make(O_TYPE, C_PREDEF);
	t->t_pfname = "PackLongInteger";
	t->t_ufname = "UnpackLongInteger";
	declare(&Values, LongInteger_type, t);

	String_type = make(O_SYMBOL, "String");
	t = make(O_TYPE, C_PREDEF);
	t->t_pfname = "PackString";
	t->t_ufname = "UnpackString";
	declare(&Values, String_type, t);

	Unspecified_type = make(O_SYMBOL, "Unspecified");
	t = make(O_TYPE, C_PREDEF);
	t->t_pfname = "PackUnspecified";
	t->t_ufname = "UnpackUnspecified";
	declare(&Values, Unspecified_type, t);

	LongUnspecified_type = make(O_SYMBOL, "LongUnspecified");
	t = make(O_TYPE, C_PREDEF);
	t->t_pfname = "PackLongUnspecified";
	t->t_ufname = "UnpackLongUnspecified";
	declare(&Values, LongUnspecified_type, t);

	Undefined_constant = make(O_SYMBOL, "?undefined?");
	declare(&Types, Undefined_constant, Unspecified_type);
	declare(&Values, Undefined_constant, make(O_CONSTANT, 0));
}
