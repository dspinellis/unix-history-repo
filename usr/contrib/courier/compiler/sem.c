#ifndef lint
static char sccsid[] = "@(#)sem.c	4.1 (Berkeley) 7/3/83";
#endif

#include "Courier.h"

/*
 * String allocation.
 */
char *
copy(s)
	char *s;
{
	char *p;
	extern char *malloc();

	if ((p = malloc(strlen(s) + 1)) == NULL) {
		fprintf(stderr, "Out of string space.\n");
		exit(1);
	}
	strcpy(p, s);
	return (p);
}

/*
 * Object allocation.
 */
struct object *
make(class, value)
	enum class class;
	int value;
{
	struct object *o;

	o = New(struct object);
	o->o_class = class;
	switch (class) {
	case O_TYPE:
		o->o_type = New(struct type);
		o->t_constr = (enum constr) value;
		break;
	case O_SYMBOL:
		o->o_name = copy(value);
		break;
	case O_CONSTANT:
		o->o_value = value;
		break;
	default:
		yyerror("Internal error: bad object class %d", class);
		exit(1);
	}
	return (o);
}

/*
 * Lisp operations.
 */
list
cons(a, b)
	list a, b;
{
	list p;

	if ((p = New(struct cons)) == NIL) {
		yyerror("Out of cons space.");
		exit(1);
	}
	car(p) = a;
	cdr(p) = b;
	return (p);
}

length(p)
	list p;
{
	int n;

	for (n = 0; p != NIL; p = cdr(p), n++)
		;
	return (n);
}

list
nconc(p, q)
	list p, q;
{
	list pp;

	pp = p;
	if (p == NIL)
		return (q);
	while (cdr(p) != NIL)
		p = cdr(p);
	cdr(p) = q;
	return (pp);
}

struct object *
construct_type1(constructor, items)
	enum constr constructor;
	list items;
{
	struct object *t;

	t = make(O_TYPE, constructor);
	t->t_list = items;
	return (t);
}

struct object *
construct_type2(constructor, size, base)
	enum constr constructor;
	struct object *size, *base;
{
	struct object *t;

	t = make(O_TYPE, constructor);
	t->t_basetype = base;
	t->t_size = size;
	return (t);
}

struct object *
construct_procedure(args, results, errors)
	list args, results, errors;
{
	struct object *t;

	t = make(O_TYPE, C_PROCEDURE);
	t->t_args = args;
	t->t_results = results;
	t->t_errors = errors;
	return (t);
}

/*
 * Look up the value corresponding to a member of an enumeration type.
 * Print an error message if it's not found.
 */
struct object *
designator_value(symbol, enumtype)
	struct object *symbol, *enumtype;
{
	list p;
	char *name;

	name = symbol->o_name;
	for (p = enumtype->t_list; p != NIL; p = cdr(p))
		if (streq(name, name_of(car(car(p)))))
			return ((struct object *) cdr(car(p)));
	yyerror("%s not a member of specified enumeration type", name);
	return (0);
}

/*
 * Construct a choice type.
 * There are two ways a choice can be specified:
 * with an explicit designator enumeration type,
 * or implicitly by specifying values for each designator.
 * Convert the second form into the first by creating
 * an enumeration type on the fly.
 */
struct object *
construct_choice(designator, candidates)
	struct object *designator;
	list candidates;
{
	struct object *t;
	list p, q, dlist;
	int bad = 0;

	if (designator != 0) {
		t = basetype(designator);
		if (t->t_constr != C_ENUMERATION) {
			yyerror("Designator type %s is not an enumeration type",
				designator->o_name);
			return (Unspecified_type);
		}
		/* check that designators don't specify values */
		for (p = candidates; p != NIL; p = cdr(p))
			for (q = car(car(p)); q != NIL; q = cdr(q)) {
				if (cdr(car(q)) != NIL) {
					yyerror("Value cannot be specified for designator %s",
						name_of(car(car(q))));
					bad = 1;
					continue;
				}
				if (designator_value(car(car(q)), t) == 0) {
					bad = 1;
					continue;
				}
			}
	} else {
		/* check that designators do specify values */
		dlist = NIL;
		for (p = candidates; p != NIL; p = cdr(p))
			for (q = car(car(p)); q != NIL; q = cdr(q)) {
				if (cdr(car(q)) == NIL) {
					yyerror("Value must be specified for designator %s",
						name_of(car(car(q))));
					bad = 1;
					continue;
				}
				dlist = cons(car(q), dlist);
			}
		if (! bad)
			designator = construct_type1(C_ENUMERATION, dlist);
	}
	if (bad)
		return (Unspecified_type);
	t = make(O_TYPE, C_CHOICE);
	t->t_designator = designator;
	t->t_candidates = candidates;
	return (t);
}

/*
 * Symbol table management.
 */
struct object *
lookup(symlist, symbol)
	list symlist;
	struct object *symbol;
{
	char *name;
	list p, q;

	name = symbol->o_name;
	for (p = symlist; p != NIL; p = cdr(p)) {
		q = car(p);
		if (streq(name_of(car(q)), name))
			return ((struct object *) cdr(q));
	}
	return (0);
}

check_def(symbol)
	struct object *symbol;
{
	if (lookup(Values, symbol) == 0) {
		yyerror("%s undefined", symbol->o_name);
		return (0);
	}
	return (1);
}

declare(symlist, name, value)
	list *symlist;
	struct object *name, *value;
{
	if (lookup(*symlist, name) != 0) {
		yyerror("%s redeclared", name->o_name);
		return;
	}
	*symlist = cons(cons(name, value), *symlist);
}

/*
 * Find the underlying type of a type.
 */
struct object *
basetype(type)
	struct object *type;
{
	while (type != 0 && class_of(type) == O_SYMBOL)
		type = lookup(Values, type);
	if (type == 0 || class_of(type) != O_TYPE) {
		yyerror("Internal error: bad class in basetype\n");
		exit(1);
	}
	return (type);
}

/*
 * Make sure a number is a valid constant for this type.
 */
type_check(type, value)
	struct object *type, *value;
{
	struct object *t, *v;

	if (class_of(type) != O_SYMBOL)
		return (type->t_constr == C_PROCEDURE ||
			type->t_constr == C_ERROR);
	/*
	 * Type is a symbol.
	 * Track down the actual type, and its closest name.
	 */
	while (type != 0 && class_of(type) == O_SYMBOL) {
		t = type;
		type = lookup(Values, type);
	}
	if (type == 0 || class_of(type) != O_TYPE) {
		yyerror("Internal error: bad class in type_check\n");
		exit(1);
	}
	if (type->t_constr != C_PREDEF)
		return (type->t_constr == C_PROCEDURE ||
			type->t_constr == C_ERROR);
	/*
	 * Here we know that t is either a type
	 * or a symbol defined as a predefined type.
	 * Now find the type of the constant, if possible.
	 * If it is just a number, we don't check any further.
	 */
	if (class_of(value) == O_SYMBOL)
		v = basetype(lookup(Types, value));
	else
		v = 0;
	return ((t == Cardinal_type || t == LongCardinal_type ||
		 t == Integer_type || t == LongInteger_type ||
		 t == Unspecified_type) && (v == 0 || v == type));
}

/*
 * Debugging routines.
 */
symtabs()
{
	printf("Values:\n"); prsymtab(Values);
	printf("Types:\n"); prsymtab(Types);
}

prsymtab(symlist)
	list symlist;
{
	list p;
	char *s;

	for (p = symlist; p != NIL; p = cdr(p)) {
		switch (class_of(cdr(car(p)))) {
		case O_TYPE:
			s = "type"; break;
		case O_CONSTANT:
			s = "constant"; break;
		case O_SYMBOL:
			s = "symbol"; break;
		default:
			s = "unknown class"; break;
		}
		printf("%s = [%s]\n", name_of(car(car(p))), s);
	}
}
