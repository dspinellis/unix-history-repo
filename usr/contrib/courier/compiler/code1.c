#ifndef lint
static char sccsid[] = "@(#)code1.c	4.2 (Berkeley) 9/27/83";
#endif

#include "Courier.h"

char *program_name = "Unknown";
int print_level;		/* for pretty-printing C code */

/*
 * Return a printable representation of an object (number or string).
 */
char *
obj_rep(o)
	struct object *o;
{
	static char rep[MAXSTR];

	switch (class_of(o)) {
	case O_CONSTANT:
		sprintf(rep, "%d", o->o_value);
		return (rep);
	case O_SYMBOL:
		return(o->o_name);
	default:
		yyerror("Internal error in obj_rep: bad object class");
		exit(1);
		/* NOTREACHED */
	}
}

program_header(symbol)
	struct object *symbol;
{
	program_name = symbol->o_name;
	fprintf(hf,
"/*\n\
 * Declarations for Courier program %s.\n\
 */\n\
#include <courier.h>\n",
		program_name);

	fprintf(cf1,
"/*\n\
 * Routines for Courier program %s.\n\
 */\n\
#include \"%s.h\"\n",
		program_name, program_name);

	fprintf(uf,
"/*\n\
 * User access to Courier program %s.\n\
 */\n\
#include \"%s_stubs.c\"\n",
		program_name, program_name);

	fprintf(sf,
"/*\n\
 * Server for Courier program %s.\n\
 */\n\
#include \"%s_stubs.c\"\n",
		program_name, program_name);

	if (! explicit)
		generate_binding_functions();
}

char *
pack_function(type)
	struct object *type;
{
	static char buf[MAXSTR];

	if (class_of(type) == O_TYPE)
		return (type->t_pfname);
	sprintf(buf, "Pack%s", type->o_name);
	return (buf);
}

char *
unpack_function(type)
	struct object *type;
{
	static char buf[MAXSTR];

	if (class_of(type) == O_TYPE)
		return (type->t_ufname);
	sprintf(buf, "Unpack%s", type->o_name);
	return (buf);
}

/*
 * Generate definitions for types.
 */
compile_type(symbol, type)
	struct object *symbol, *type;
{
	char *name;

	if (type->t_constr == C_PROCEDURE || type->t_constr == C_ERROR)
		return;
	name = symbol->o_name;
	fprintf(hf, "\ntypedef ");
	print_decl(hf, name, type, 0);
	fprintf(cf1, "\n#define Pack%s %s\n#define Unpack%s %s\n",
		name, pack_function(type), name, unpack_function(type));
	declare(&Values, symbol, type);
}

/*
 * Generate definitions corresponding to constant declarations.
 */
compile_def(name, type, value)
	struct object *name, *type, *value;
{
	struct object *t;

	t = basetype(type);
	if (t->t_constr == C_PROCEDURE)
		proc_functions(name->o_name, t, value);
	else
		fprintf(hf, "\n#define %s %s\n", name->o_name, obj_rep(value));
	declare(&Values, name, value);
	declare(&Types, name, type);
}

/*
 * Print a C type declaration for a Courier type.
 *
 * If the nonewline flag is on, don't follow the declaration
 * by ";\n" (used for declaring the return value of a function.)
 */
print_decl(f, name, type, nonewline)
	FILE *f;
	char *name;
	struct object *type;
	int nonewline;
{
	list p, q;
	struct object *t;
	char *member;
	char newname[MAXSTR];

	if (class_of(type) == O_SYMBOL) {
		tab(f); fprintf(f, "%s %s", type->o_name, name);
		goto ret;
	}
	if (class_of(type) != O_TYPE) {
		yyerror("Internal error in print_decl: bad object class for %s",
			name);
		exit(1);
	}
	switch (type->t_constr) {

	case C_ENUMERATION:
		tab(f); fprintf(f, "enum {\n");
		print_level++;
		for (p = type->t_list; p != NIL; p = cdr(p)) {
			q = car(p);
			member = name_of(car(q));
			tab(f); fprintf(f, "%s = %s", member, obj_rep(cdr(q)));
			if (cdr(p) != NIL)
				fprintf(f, ",\n");
			else
				fprintf(f, "\n");
		}
		print_level--;
		tab(f); fprintf(f, "} %s", name);
		goto ret;

	case C_ARRAY:
		sprintf(newname, "%s[%s]", name, obj_rep(type->t_size));
		print_decl(f, newname, type->t_basetype, nonewline);
		return;

	case C_SEQUENCE:
		tab(f); fprintf(f, "struct {\n");
		print_level++;
		print_decl(f, "length", Cardinal_type, 0);
		print_decl(f, "*sequence", type->t_basetype, 0);
		print_level--;
		tab(f); fprintf(f, "} %s", name);
		goto ret;

	case C_RECORD:
		if (type->t_list == NIL) {
			/* C complains about this, but accepts it */
			tab(f); fprintf(f, "int %s[0]", name);
			goto ret;
		}
		tab(f); fprintf(f, "struct {\n");
		print_level++;
		for (p = type->t_list; p != NIL; p = cdr(p)) {
			t = (struct object *) cdr(car(p));
			for (q = car(car(p)); q != NIL; q = cdr(q))
				print_decl(f, name_of(car(q)), t, 0); 
		}
		print_level--;
		tab(f); fprintf(f, "} %s", name);
		goto ret;

	case C_CHOICE:
		tab(f); fprintf(f, "struct {\n");
		print_level++;
		print_decl(f, "designator", type->t_designator, 0);
		tab(f); fprintf(f, "union {\n");
		print_level++;
		for (p = type->t_candidates; p != NIL; p = cdr(p)) {
			t = (struct object *) cdr(car(p));
			for (q = car(car(p)); q != NIL; q = cdr(q)) {
				member = name_of(car(car(q)));
				sprintf(newname, "u_%s", member);
				print_decl(f, newname, t, 0); 
				fprintf(f, "#define %s_case u.u_%s\n",
					member, member);
			}
		}
		print_level--;
		tab(f); fprintf(f, "} u;\n");
		print_level--;
		tab(f); fprintf(f, "} %s", name);
		goto ret;

	default:
		yyerror("Internal error in print_decl: bad type constructor for %s",
			name);
		exit(1);
	}
ret:
	if (! nonewline)
		fprintf(f, ";\n");
}

char *
gensym(prefix)
	char *prefix;
{
	static int n = 0;
	char buf[MAXSTR];

	sprintf(buf, "%s%d", prefix, n);
	n++;
	return (copy(buf));
}

/*
 * Generate C functions to pack and unpack a Courier type.
 * Put their names in the type structure.
 */
type_functions(type)
	struct object *type;
{
	list p, q;
	struct object *t;
	char *pname, *uname, *format, *ref, *member, *value;

	if (class_of(type) != O_TYPE || type->t_constr == C_PREDEF)
		return;
	if (type->t_constr != C_ENUMERATION) {
		type->t_pfname = pname = gensym("Pack");
		type->t_ufname = uname = gensym("Unpack");
	}

	switch (type->t_constr) {

	case C_ENUMERATION:
		type->t_pfname = "PackCardinal";
		type->t_ufname = "UnpackCardinal";
		return;

	case C_ARRAY:
		function_heading(cf1, pname, type->t_basetype, 1);
		function_heading(cf2, uname, type->t_basetype, 0);
		format =
"{\n\
\tregister Unspecified *bp;\n\
\tregister Cardinal i;\n\
\n\
\tbp = buf;\n\
\tfor (i = 0; i < %s; i++)\n\
\t\tbp += %s(%sp[i], bp%s);\n";

		ref = refstr(type->t_basetype);
		fprintf(cf1, format, obj_rep(type->t_size),
			pack_function(type->t_basetype), ref, ", flag");
		fprintf(cf2, format, obj_rep(type->t_size),
			unpack_function(type->t_basetype), ref, "");
		break;

	case C_SEQUENCE:
		function_heading(cf1, pname, type, 1);
		function_heading(cf2, uname, type, 0);
		format =
"{\n\
\tregister Unspecified *bp;\n\
\tregister Cardinal i;\n\
\n\
\tbp = buf;\n\
\tbp += %sCardinal(&p->length, bp%s);\n";

		fprintf(cf1, format, "Pack", ", flag");
		fprintf(cf2, format, "Unpack", "");

		/*
		 * The unpack function needs to dynamically
		 * allocate space for the sequence elements.
		 */
		fprintf(cf2, "\tp->sequence = (");
		print_decl(cf2, "*", type->t_basetype);
		fprintf(cf2, ")\n\t\tAllocate(p->length * sizeof(");
		print_decl(cf2, "", type->t_basetype);
		fprintf(cf2, ")/sizeof(Unspecified));\n");

		format =
"\tfor (i = 0; i < p->length; i++)\n\
\t\tbp += %s(%sp->sequence[i], bp%s);\n";

		ref = refstr(type->t_basetype);
		fprintf(cf1, format, pack_function(type->t_basetype),
			ref, ", flag");
		fprintf(cf2, format, unpack_function(type->t_basetype),
			ref, "");
		break;

	case C_RECORD:
		function_heading(cf1, pname, type, 1);
		function_heading(cf2, uname, type, 0);
		format =
"{\n\
\tregister Unspecified *bp;\n\
\n\
\tbp = buf;\n";
		fprintf(cf1, format);
		fprintf(cf2, format);

		format = "\tbp += %s(%sp->%s, bp%s);\n";
		for (p = type->t_list; p != NIL; p = cdr(p)) {
			t = (struct object *) cdr(car(p));
			ref = refstr(t);
			for (q = car(car(p)); q != NIL; q = cdr(q)) {
				member = name_of(car(q));
				fprintf(cf1, format, pack_function(t),
					ref, member, ", flag");
				fprintf(cf2, format, unpack_function(t),
					ref, member, "");
			}
		}
		break;

	case C_CHOICE:
		function_heading(cf1, pname, type, 1);
		function_heading(cf2, uname, type, 0);
		format =
"{\n\
\tregister Unspecified *bp;\n\
\n\
\tbp = buf;\n\
\tbp += %sCardinal(&p->designator, bp%s);\n\
\tswitch (p->designator) {\n";
		fprintf(cf1, format, "Pack", ", flag");
		fprintf(cf2, format, "Unpack", "");

		format =
"\tcase %s:\n\
\t\tbp += %s(%sp->%s_case, bp%s);\n\
\t\tbreak;\n";
		for (p = type->t_candidates; p != NIL; p = cdr(p)) {
			t = (struct object *) cdr(car(p));
			ref = refstr(t);
			for (q = car(car(p)); q != NIL; q = cdr(q)) {
				member = name_of(car(car(q)));
				fprintf(cf1, format, member, pack_function(t),
					ref, member, ", flag");
				fprintf(cf2, format, member, unpack_function(t),
					ref, member, "");
			}
		}
		fprintf(cf1, "\t}\n");
		fprintf(cf2, "\t}\n");
		break;

	case C_PROCEDURE:
	case C_ERROR:
		return;

	default:
		yyerror("Internal error in type_functions: bad type constructor");
		exit(1);
	}

	format =
"\treturn (bp - buf);\n\
}\n";
	fprintf(cf1, format);
	fprintf(cf2, format);
}

/*
 * Print the heading for a type packing or unpacking function.
 */
function_heading(f, name, type, flag)
	FILE *f;
	char *name;
	struct object *type;
	int flag;
{
	fprintf(f, "\nstatic %s(p, buf%s)\n", name, flag ? ", flag" : "");
	print_level++; print_decl(f, "*p", type, 0); print_level--;
	fprintf(f, "\tregister Unspecified *buf;\n%s",
		flag ? "\tBoolean flag;\n" : "");
}

tab(f)
	FILE *f;
{
	int n;

	for (n = print_level; n > 0; n--)
		putc('\t', f);
}
