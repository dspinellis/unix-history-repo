/*
 * Copyright (C) 1984 by Eric C. Cooper.
 * All rights reserved.
 */
#ifndef lint
static char RCSid[] = "$Header: typecode.c,v 2.4 86/06/26 12:38:30 jqj Exp $";
#endif

/* $Log:	typecode.c,v $
 * Revision 2.4  86/06/26  12:38:30  jqj
 * clear_ functions should be type void.
 * 
 * Revision 2.3  86/06/06  07:28:59  jqj
 * many mods for better symbol table management:  added CurrentModule,
 *  made check_dependency, make_symbol, check_def set/use/use a symbol
 *  table instead of a module name string, etc.  Result is that we can
 *  now handle DEPENDS UPON 2 versions of same program.
 * 
 * Revision 2.2  86/05/16  11:13:59  jqj
 * various bugs in widening created by previous edit (I still don't have
 * them all fixed, but at least it's now usable).
 * 
 * Revision 2.1  86/05/16  05:47:15  jqj
 * make enumeration tags local to modules rather than global, to allow
 * DEPENDS UPON two versions of the same program.  For same reason, use
 * gensymed symbol names that include version number.
 * 
 * Revision 2.0  85/11/21  07:21:47  jqj
 * 4.3BSD standard release
 * 
 * Revision 1.6  85/05/23  06:20:12  jqj
 * *** empty log message ***
 * 
 * Revision 1.6  85/05/23  06:20:12  jqj
 * Public Beta-test version, released 24 May 1985
 * 
 * Revision 1.5  85/05/06  08:13:43  jqj
 * Almost Beta-test version.
 * 
 * Revision 1.4  85/03/26  06:10:42  jqj
 * Revised public alpha-test version, released 26 March 1985
 * 
 * Revision 1.3  85/03/11  16:40:21  jqj
 * Public alpha-test version, released 11 March 1985
 * 
 * Revision 1.2  85/02/21  11:06:11  jqj
 * alpha test version
 * 
 * Revision 1.1  85/02/15  13:55:45  jqj
 * Initial revision
 * 
 */

#include "compiler.h"

#define candidate_name(str)	(str)

/*
 * This function is used to cope with the fact that C passes arrays
 * by reference but all other types by value.
 * The argument should be a base type.
 */
char *
refstr(typtr)
	struct type *typtr;
{
/*	if (typtr->o_class != O_TYPE)
		error(FATAL, "internal error (refstr): not a type");
 */
	return (typtr->type_constr == C_ARRAY ? "" : "&");
}

/*
 * Names of translation functions for types.
 * Warning: returns pointer to a static buffer.
 */
char *
xfn(kind, typtr)
	enum translation kind;
	struct type *typtr;
{
	static char buf[MAXSTR];
	char *name;

	switch (kind) {
	    case EXTERNALIZE:
		name = "externalize";
		break;
	    case INTERNALIZE:
		name = "internalize";
		break;
	}
	(void) sprintf(buf, "%s_%s", name, typtr->type_name);
	return (buf);
}

/*
 * Print the heading for a type externalizing or internalizing function.
 */
xfn_header(kind, typtr, ptr_type)
	enum translation kind;
	struct type *typtr, *ptr_type;
{
	FILE *f;

	switch (kind) {
	    case EXTERNALIZE:
		f = support1; break;
	    case INTERNALIZE:
		f = support2; break;
	}
	fprintf(f,
"\n\
int\n\
%s(p, buf)\n\
\tregister %s *p;\n\
\tregister Unspecified *buf;\n",
		xfn(kind, typtr), typename(ptr_type));
}

/*
 * create an alias for a type's datastructures.  Note that caller must
 * create the alias for the typedef name itself.
 */
copy_typefns(headerfile,new,old)
	FILE *headerfile; 
	char *new, *old;
{
	fprintf(headerfile,
"#define sizeof_%s sizeof_%s\n\
#define clear_%s clear_%s\n\
#define externalize_%s externalize_%s\n\
#define internalize_%s internalize_%s\n\n",
			new, old, new, old, new, old, new, old);
}


widen_enumeration_typedef(header1,fullname,tpname,typtr)
	FILE *header1;
	char *fullname;
	char *tpname;
	struct type *typtr;
{
	list p,q;

	fprintf(header1, "\n#define %s %s\ntypedef enum {\n",
			fullname,tpname);
	for (p = typtr->type_list; p != NIL; p = cdr(p)) {
		q=car(p);
		fprintf(header1, "\t%s = %s", name_of(car(q)),
				((char *) cdr(q)) );
		if (cdr(p) != NIL)
			fprintf(header1, ",\n");
		else
			fprintf(header1, "\n");
	}
	fprintf(header1, "} %s;\n", tpname);
}

define_enumeration_type(typtr)
	struct type *typtr;
{
	list p,q;
	struct object *optr;

	typtr->type_xsize = 1;
	if (recursive_flag)
		return;
	/*
	 * Print a C definition for the enumeration.
	 */
	fprintf(header, "\ntypedef enum {\n");
	for (p = typtr->type_list; p != NIL; p = cdr(p)) {
		q=car(p);
		optr = (struct object *) car(q);
		fprintf(header, "\t%s = %s",enumname_of(car(q)),
/*				make_full_name(optr->o_module,		      */
/*					optr->o_modversion,name_of(car(q))),  */
				((char *) cdr(q)) );
		if (cdr(p) != NIL)
			fprintf(header, ",\n");
		else
			fprintf(header, "\n");
	}
	fprintf(header, "} %s;\n", typename(typtr));
	/*
	 * We use the same sizeof and translation functions
	 * for all enumerated types.
	 */
	copy_typefns(header,typename(typtr),"enumeration");
}


define_record_type(typtr)
	struct type *typtr;
{
	struct type *bt;
	list p, q;
	int fixed_size;
	char *format, *ref, *member;

	/*
	 * Make sure all subtypes are defined and have sizes
	 */
	for (p = typtr->type_list; p != NIL; p = cdr(p)) {
		bt = (struct type *) cdar(p);
		if (typename(bt) == NULL) {
			struct object *name;
			name = make_symbol(gensym("T_r"),CurrentModule);
			define_type(name,bt);
		}
	}
	/*
	 * Generate size field.
	 * The size is equal to the sum of the sizes of each field.
	 */
	fixed_size = 0;
	typtr->type_xsize = 0;
	for (p = typtr->type_list; p != NIL; p = cdr(p)) {
		bt = (struct type *) cdar(p);
		if (bt->type_xsize == -1)
			typtr->type_xsize = -1;
		else
			fixed_size += bt->type_xsize;
	}
	if (typtr->type_xsize != -1)
		typtr->type_xsize = fixed_size;
	if (recursive_flag)
		return;
	/*
	 * Print a C definition for the record.
	 */
	fprintf(header, "\ntypedef struct {\n");
	for (p = typtr->type_list; p != NIL; p = cdr(p)) {
		bt = (struct type *) cdar(p);
		q = caar(p);
		member = (char *) car(q);
		fprintf(header, "\t%s %s;\n", typename(bt), member);
	}
	fprintf(header, "} %s;\n", typename(typtr));
	/*
	 * Generate sizeof and free functions for the record.
	 */
	if (typtr->type_xsize != -1) {
		/*
		 * The record is fixed-size, so just define a macro.
		 */
		fprintf(header,
"\n\
#define sizeof_%s(p) %d\n\
\n\
#define clear_%s(p)\n",
			typename(typtr), typtr->type_xsize,
			typename(typtr));
	} else {
		/*
		 * There are some variable-size fields, so define functions.
		 */
		fprintf(support1,
"\n\
int\n\
sizeof_%s(p)\n\
\tregister %s *p;\n\
{\n\
\tregister int size = %d;\n\
\n",
			typename(typtr), typename(typtr), fixed_size);
		for (p = typtr->type_list; p != NIL; p = cdr(p)) {
			bt = (struct type *) cdar(p);
			if (bt->type_xsize != -1)
				continue;
			ref = refstr(bt);
			q = caar(p);
			member = (char *) car(q);
			fprintf(support1,
"\tsize += sizeof_%s(%sp->%s);\n",
				typename(bt), ref, member);
		}
		fprintf(support1,
"\treturn (size);\n\
}\n"
			);
		fprintf(support1,
"\n\
void\n\
clear_%s(p)\n\
\tregister %s *p;\n\
{\n\
\n",
			typename(typtr), typename(typtr));
		for (p = typtr->type_list; p != NIL; p = cdr(p)) {
			bt = (struct type *) cdar(p);
			if (bt->type_xsize != -1)
				continue;
			ref = refstr(bt);
			q = caar(p);
			member = (char *) car(q);
			fprintf(support1,
"\tclear_%s(%sp->%s);\n",
				typename(bt), ref, member);
		}
		fprintf(support1, "}\n" );
	}
	/*
	 * Define translation functions.
	 */
	xfn_header(EXTERNALIZE, typtr, typtr);
	xfn_header(INTERNALIZE, typtr, typtr);
	format =
"{\n\
\tregister Unspecified *bp;\n\
\n\
\tbp = buf;\n";
	fprintf(support1, format);
	fprintf(support2, format);
	format =
"\tbp += %s(%sp->%s, bp);\n";
	for (p = typtr->type_list; p != NIL; p = cdr(p)) {
		bt = (struct type *) cdar(p);
		ref = refstr(bt);
		q = caar(p);
		member = (char *) car(q);
		fprintf(support1, format,
			xfn(EXTERNALIZE, bt), ref, member);
		fprintf(support2, format,
			xfn(INTERNALIZE, bt), ref, member);
	}
	format =
"\treturn (bp - buf);\n\
}\n";
	fprintf(support1, format);
	fprintf(support2, format);
}

define_array_type(typtr)
	struct type *typtr;
{
	struct type *bt;
	int true_size;
	char *ref, *format;

	bt = typtr->type_basetype;
	/*
	 * Make sure the component type is defined and sized
	 */
	if (typename(bt) == NULL) {
		struct object *name;
		name = make_symbol(gensym("T_a"),CurrentModule);
		define_type(name,bt);
	}
	ref = refstr(bt);
	true_size = typtr->type_size;
	if (bt->type_xsize != -1)
		typtr->type_xsize = true_size * bt->type_xsize;
	else
		typtr->type_xsize = -1;
	if (recursive_flag)
		return;
	/*
	 * Print a C definition for the array.
	 */
	fprintf(header, "\ntypedef %s %s[%d];\n",
		typename(bt), typename(typtr), true_size);
	/*
	 * Generate a sizeof and free functions for the array.
	 * The size is equal to the sum of the sizes of each element.
	 */
	if (bt->type_xsize != -1) {
		/*
		 * The element type, and hence the array, is fixed-size,
		 * so just define a macro.
		 */
		fprintf(header,
"\n\
#define sizeof_%s(p) %d\n\
\n\
#define clear_%s(p)\n",
			typename(typtr), typtr->type_xsize,
			typename(typtr));
	} else {
		/*
		 * The element type is variable-size, so define a function.
		 */
		fprintf(support1,
"\n\
int\n\
sizeof_%s(p)\n\
\tregister %s *p;\n\
{\n\
\tregister int size = 0;\n\
\tregister int i;\n\
\n\
\tfor (i = 0; i < %d; i += 1)\n\
\t\tsize += sizeof_%s(%sp[i]);\n\
\treturn (size);\n\
}\n",
			typename(typtr), typename(bt), true_size,
			typename(bt), ref);
		fprintf(support1,
"\n\
void\n\
clear_%s(p)\n\
\t%s *p;\n\
{\n\
\tregister int i;\n\
\n\
\tfor (i = 0; i < %d; i += 1)\n\
\t\tclear_%s(%sp[i]);\n\
}\n",
			typename(typtr), typename(bt), true_size,
			typename(bt), ref);
	}
	/*
	 * Define translation functions.
	 */
	xfn_header(EXTERNALIZE, typtr, bt);
	xfn_header(INTERNALIZE, typtr, bt);
	format =
"{\n\
\tregister Unspecified *bp;\n\
\tregister int i;\n\
\n\
\tbp = buf;\n\
\tfor (i = 0; i < %d; i += 1)\n\
\t\tbp += %s(%sp[i], bp);\n\
\treturn (bp - buf);\n\
}\n";
	fprintf(support1, format,
		true_size, xfn(EXTERNALIZE, bt), ref);
	fprintf(support2, format,
		true_size, xfn(INTERNALIZE, bt), ref);
}

define_sequence_type(typtr)
	struct type *typtr;
{
	struct type *bt;
	char *ref, *format;

	typtr->type_xsize = -1;
	bt = typtr->type_basetype;
	/*
	 * Make sure the component type is defined
	 */
	if (typename(bt) == NULL) {
		struct object *name;
		name = make_symbol(gensym("T_s"),CurrentModule);
		define_type(name,bt);
	}
	if (recursive_flag)
		return;
	/*
	 * Print a C definition for the sequence.
	 */
	fprintf(header,
"\n\
typedef struct {\n\
\tCardinal length;\n\
\t%s *sequence;\n\
} %s;\n",
		typename(bt), typename(typtr));
	/*
	 * Generate sizeof and free functions for the sequence.
	 * The size is equal to 1 (for the length word)
	 * plus the sum of the sizes of each element.
	 */
	bt = typtr->type_basetype;
	ref = refstr(bt);
	if (bt->type_xsize != -1) {
		/*
		 * The element type is fixed-size, so just define a macro.
		 */
		fprintf(header,
"\n\
#define sizeof_%s(p) (1 + (p)->length * %d)\n",
			typename(typtr), bt->type_xsize);
		fprintf(support1,
"\n\
void\n\
clear_%s(p)\n\
\tregister %s *p;\n\
{\n\
\tDeallocate((Unspecified*) p->sequence);\n\
\tp->length = 0;  p->sequence = (%s*) 0;\n\
}\n",
			typename(typtr), typename(typtr),
			typename(bt) );
	} else {
		/*
		 * The element type is variable-size, so define a function.
		 */
		fprintf(support1,
"\n\
int\n\
sizeof_%s(p)\n\
\tregister %s *p;\n\
{\n\
\tregister int size = 1;\n\
\tregister int i;\n\
\n\
\tif (p->sequence == (%s*) 0) return(size);\n\
\tfor (i = 0; i < p->length; i += 1)\n\
\t\tsize += sizeof_%s(%sp->sequence[i]);\n\
\treturn (size);\n\
}\n",
			typename(typtr), typename(typtr), typename(bt),
			typename(bt), ref);
		fprintf(support1,
"\n\
void\n\
clear_%s(p)\n\
\tregister %s *p;\n\
{\n\
\tregister int i;\n\
\n\
\tif (p->sequence != (%s*) 0) for (i = 0; i < p->length; i += 1)\n\
\t\tclear_%s(%sp->sequence[i]);\n\
\tDeallocate((Unspecified*) p->sequence);\n\
\tp->length = 0;  p->sequence = (%s*) 0;\n\
}\n",
			typename(typtr), typename(typtr), typename(bt),
			typename(bt), ref,
			typename(bt) );

	}
	/*
	 * Define translation functions.
	 */
	xfn_header(EXTERNALIZE, typtr, typtr);
	xfn_header(INTERNALIZE, typtr, typtr);
	/*
	 * The externalize function (trivially) checks its pointer
	 * for consistency.
	 */
	fprintf(support1,
"{\n\
\tregister Unspecified *bp;\n\
\tregister int i;\n\
\n\
\tif (p->sequence == (%s*)0) p->length = 0;\n\
\tbp = buf + %s(&p->length, buf);\n",
		typename(bt),
		xfn(EXTERNALIZE, Cardinal_type));
	/*
	 * The internalize function needs to allocate space
	 * for the sequence elements dynamically.
	 */
	fprintf(support2,
"{\n\
\tregister Unspecified *bp;\n\
\tregister int i;\n\
\n\
\tbp = buf + %s(&p->length, buf);\n\
\tp->sequence = (%s *)\n\
\t\tAllocate(p->length * sizeof(%s)/sizeof(Cardinal));\n",
		xfn(INTERNALIZE, Cardinal_type),
		typename(bt), typename(bt));
	format =
"\tfor (i = 0; i < p->length; i++)\n\
\t\tbp += %s(%sp->sequence[i], bp);\n\
\treturn (bp - buf);\n\
}\n";
	fprintf(support1, format, xfn(EXTERNALIZE, bt), ref);
	fprintf(support2, format, xfn(INTERNALIZE, bt), ref);
}

define_choice_type(typtr)
	struct type *typtr;
{
	struct type *designator, *bt;
	list p,q,candidates;
	char *format, *ref, *member;

	typtr->type_xsize = -1;

	designator = typtr->type_designator;
	candidates = typtr->type_candidates;
	if (! recursive_flag)
		fprintf(header,
"\n\
extern struct %s;\n\
typedef struct %s %s;\n",
			typename(typtr), typename(typtr), typename(typtr));
	/*
	 * Make sure each arm type is defined
	 */
	for (p = candidates; p != NIL; p = cdr(p)) {
		bt = (struct type *) cdar(p);
		if (typename(bt) == NULL) {
			struct object *name;
			name = make_symbol(gensym("T_c"),CurrentModule);
			define_type(name,bt);
		}
	}
	if (recursive_flag)
		return;
	/*
	 * Print a C definition for the choice.
	 * First, be prepared for recursive references of the SEQUENCE OF form
	 */
	fprintf(header,
"\n\
struct %s {\n\
\t%s designator;\n\
\tunion {\n",
		typename(typtr), typename(designator));
	for (p = candidates; p != NIL; p = cdr(p)) {
		bt = (struct type *) cdar(p);
		for (q = caar(p); q != NIL; q = cdr(q)) {
			member = enumname_of(caar(q));
			fprintf(header,
"\t\t%s u_%s;\n\
#define %s_case u.u_%s\n",
				typename(bt), member,
				candidate_name(member), member);
			if (strcmp(name_of(caar(q)),candidate_name(member)) != 0)
				fprintf(header1,"#define %s_case %s_case\n",
					name_of(caar(q)),
					candidate_name(member) );
		}
	}
	fprintf(header,
"\t} u;\n\
};\n" );
	/*
	 * Generate a sizeof function for the choice.
	 * The size is equal to 1 (for the designator word)
	 * plus the size of the corresponding candidate.
	 * We could check if all the candidates happen to be the same size,
	 * but we don't bother and always call it variable-size.
	 */
	fprintf(support1,
"\n\
int\n\
sizeof_%s(p)\n\
\tregister %s *p;\n\
{\n\
\tregister int size = 1;\n\
\n\
\tswitch (p->designator) {\n",
		typename(typtr), typename(typtr));
	for (p = candidates; p != NIL; p = cdr(p)) {
		bt = (struct type *) cdar(p);
		ref = refstr(bt);
		for (q = caar(p); q != NIL; q = cdr(q)) {
			member = enumname_of(caar(q));
			fprintf(support1,
"\t    case %s:\n\
\t\tsize += sizeof_%s(%sp->%s_case);\n\
\t\tbreak;\n",
				member, typename(bt), ref, 
				candidate_name(member));
		}
	}
	fprintf(support1,
"\t}\n\
\treturn (size);\n\
}\n"
		);
	/*
	 * Now generate the freeing function.  Here we do bother
	 * not to free constant-sized structures, just for kicks.
	 * However, we always generate a freeing function, even if
	 * all the arms of the choice are constant sized.
	 */
	fprintf(support1,
"\n\
void\n\
clear_%s(p)\n\
\tregister %s *p;\n\
{\n\
\tswitch (p->designator) {\n",
		typename(typtr), typename(typtr));
	for (p = candidates; p != NIL; p = cdr(p)) {
		bt = (struct type *) cdar(p);
		ref = refstr(bt);
		for (q = caar(p); q != NIL; q = cdr(q)) {
			member = enumname_of(caar(q));
			if (bt->type_xsize == -1)
				fprintf(support1,
"\t    case %s:\n\
\t\tbreak;\n",
					member);
			else
				fprintf(support1,
"\t    case %s:\n\
\t\tclear_%s(%sp->%s_case);\n\
\t\tbreak;\n",
					member, typename(bt), ref, 
					candidate_name(member));
		}
	}
	fprintf(support1,
"\t}\n\
}\n"
		);
	/*
	 * Define translation functions.
	 */
	xfn_header(EXTERNALIZE, typtr, typtr);
	xfn_header(INTERNALIZE, typtr, typtr);
	format =
"{\n\
\tregister Unspecified *bp;\n\
\n\
\tbp = buf + %s(&p->designator, buf);\n\
\tswitch (p->designator) {\n";
	fprintf(support1, format, xfn(EXTERNALIZE, designator));
	fprintf(support2, format, xfn(INTERNALIZE, designator));
	format =
"\t    case %s:\n\
\t\tbp += %s(%sp->%s_case, bp);\n\
\t\tbreak;\n";
	for (p = candidates; p != NIL; p = cdr(p)) {
		bt = (struct type *) cdar(p);
		ref = refstr(bt);
		for (q = caar(p); q != NIL; q = cdr(q)) {
			member = enumname_of(caar(q));
			fprintf(support1, format,
				member, xfn(EXTERNALIZE, bt),
				ref, candidate_name(member));
			fprintf(support2, format,
				member, xfn(INTERNALIZE, bt),
				ref, candidate_name(member));
		}
	}
	format =
"\t}\n\
\treturn (bp - buf);\n\
}\n";
	fprintf(support1, format);
	fprintf(support2, format);
}

/*
 * Generate a new full name of the form <module><version>_<name>
 */
char *
make_full_name(module,version,name)
	char *module;
	int version;
	char *name;
{
	char buf[MAXSTR];
	if (module == NULL || *module == '\0')
		return(copy(name));
	sprintf(buf,"%s%d_%s",module,version,name);
	return(copy(buf));
}

/*
 * Generate defininitions for named types
 * and their size and translation functions.
 * We assume that each type with a type_name field has already been
 * generated.
 */
define_type(name, typtr)
	struct object *name;
	struct type *typtr;
{
	char *fullname;
	/*
	 * create the symbol -- it has already been made via make_symbol()
	 * which, along with allocating an object, set o_name
	 */
	name->o_class = O_TYPE;
	name->o_type = typtr;
	fullname = make_full_name(name->o_module, name->o_modversion,
			name_of(name));
	code_type(fullname, typtr);
	if (!recursive_flag) {
		/* widen scope */
		if (typtr->type_constr == C_ENUMERATION)
			/* special scope-widening */
			widen_enumeration_typedef(header1,fullname,
					name_of(name),typtr);
		else
			fprintf(header1, "typedef %s %s;\n",
				fullname, name_of(name));
		copy_typefns(header1,name_of(name),fullname);
	}
}

/*
 * Actually generate some code.  This routine may be called recursively
 * if subtypes have no name.
 */
code_type(name, typtr)
	char *name;
	struct type *typtr;
{
	/*
	 * check for simple case of "foo: TYPE = bar;" rename
	 */
	if (typename(typtr) != NULL) {
		if (!recursive_flag) {
			/* create alias for typedef */
			fprintf(header,"typedef %s %s;\n",
				typename(typtr), name);
			copy_typefns(header,name, typename(typtr));
		}
		return;
	}
	/*
	 * general case:  "foo: TYPE = <type>;"
	 * actually generate some code
	 */
	switch (typtr->type_constr) {
	case C_PROCEDURE:
		/* no code gets generated for these Types */
		typename(typtr) = name;
		break;
	case C_NUMERIC:
	case C_BOOLEAN:
	case C_STRING:
		/* create alias for typedef */
		fprintf(header,"typedef %s %s;\n",
			typename(typtr), name);
		copy_typefns(header,name,typename(typtr));
		typename(typtr) = name;
		break;
	case C_ENUMERATION:
		typename(typtr) = name;
		define_enumeration_type(typtr);
		break;
	case C_ARRAY:
		typename(typtr) = name;
		define_array_type(typtr);
		break;
	case C_SEQUENCE:
		typename(typtr) = name;
		define_sequence_type(typtr);
		break;
	case C_RECORD:
		typename(typtr) = name;
		define_record_type(typtr);
		break;
	case C_CHOICE:
		typename(typtr) = name;
		define_choice_type(typtr);
		break;
	case C_ERROR:
		typename(typtr) = name;
		if (typtr->type_list != NIL)
			define_record_type(typtr);
		break;
	}
	return;
}
