#ifndef lint
static char RCSid[] = "$Header: types.c,v 2.1 86/05/16 11:14:30 jqj Exp $";
#endif

/* $Log:	types.c,v $
 * Revision 2.1  86/05/16  11:14:30  jqj
 * various bugs in widening created by previous edit (I still don't have
 * them all fixed, but at least it's now usable).
 * 
 * Revision 2.0  85/11/21  07:21:51  jqj
 * 4.3BSD standard release
 * 
 * Revision 1.3  85/03/11  16:40:43  jqj
 * *** empty log message ***
 * 
 * Revision 1.3  85/03/11  16:40:43  jqj
 * Public alpha-test version, released 11 March 1985
 * 
 * Revision 1.2  85/02/21  11:06:29  jqj
 * alpha test version
 * 
 * Revision 1.1  85/02/15  13:55:49  jqj
 * Initial revision
 * 
 */

#include "compiler.h"

/*
 * Object allocation.
 */
struct type *
make_type(constr)
	enum constr constr;
{
	struct type *typtr;

	typtr = New(struct type);
	typtr->type_constr = constr;
	return(typtr);
}

struct type *
enumeration_type(items)
	list items;
{
	struct type *typtr;

	typtr = make_type(C_ENUMERATION);
	typtr->type_list = items;
	return(typtr);
}


struct type *
record_type(fields)
	list fields;
{
	struct type *typtr;

	if (fields == NIL)
		return (NilRecord_type);
	typtr = make_type(C_RECORD);
	typtr->type_list = fields;
	return(typtr);
}


struct type *
error_type(arguments)
	list arguments;
{
	struct type *typtr;

	typtr = make_type(C_ERROR);
	typtr->type_list = arguments;
	return (typtr);
}

struct type *
array_type(size, bt)
	char *size;
	struct type *bt;
{
	struct type *typtr;

	typtr = make_type(C_ARRAY);
	typtr->type_basetype = bt;
	typtr->type_size = stringtocard(size);
	return(typtr);
}

struct type *
sequence_type(size, bt)
	char *size;
	struct type *bt;
{
	struct type *typtr;

	typtr = make_type(C_SEQUENCE);
	typtr->type_basetype = bt;
	typtr->type_size = stringtocard(size);
	return(typtr);
}

struct type *
procedure_type(args, results, errors)
	list args, results, errors;
{
	struct type *typtr;

	typtr = make_type(C_PROCEDURE);
	typtr->type_args = args;
	typtr->type_results = results;
	typtr->type_errors = errors;
	return (typtr);
}

/*
 * Construct a choice type.
 * There are two ways a choice can be specified:
 * with an explicit enumeration type as a designator,
 * or with an implicit enumeration type,
 * by specifying values as well as names for each designator.
 * Convert the second form into the first by creating
 * an enumeration type on the fly.
 */
struct type *
choice_type(designator, candidates)
	struct type *designator;
	list candidates;
{
	struct type *typtr;
	list p, q, dlist;
	int bad = 0;
	char *tpname;

	if (designator != TNIL) {
		if (designator->type_constr != C_ENUMERATION) {
			error(ERROR, "designator type %s is not an enumeration type",
				typename(designator));
			return (Unspecified_type);
		}
		/*
		 * Check that designators don't specify conflicting values.
		 */
		for (p = candidates; p != NIL; p = cdr(p))
			for (q = caar(p); q != NIL; q = cdr(q))
				if (cdar(q) != NIL &&
				    stringtocard((char *)cdar(q)) != enumvalue_of(caar(q))) {
					error(ERROR, "conflicting value specified for designator %s",
						name_of(caar(q)));
					bad = 1;
					continue;
				}
	} else {
		/*
		 * Check that designators do specify values.
		 */
		dlist = NIL;
		for (p = candidates; p != NIL; p = cdr(p)) {
			for (q = caar(p); q != NIL; q = cdr(q)) {
				if (cdar(q) == NIL) {
					error(ERROR, "value must be specified for designator %s",
						name_of(caar(q)));
					bad = 1;
					continue;
				}
				dlist = nconc(dlist, cons(car(q), NIL));
			}
		}
		if (!bad) {
			designator = enumeration_type(dlist);
			tpname = gensym("T_d");
			code_type(tpname,designator);
			widen_enumeration_typedef(header1, tpname,
						gensym("T_dw"), designator);
		}
	}
	if (bad)
		return (Unspecified_type);
	typtr = make_type(C_CHOICE);
	typtr->type_designator = designator;
	typtr->type_candidates = candidates;
	return(typtr);
}
