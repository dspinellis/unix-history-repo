#ifndef lint
static char RCSid[] = "$Header: checktype.c,v 2.1 86/06/06 07:26:32 jqj Exp $";
#endif

/* $Log:	checktype.c,v $
 * Revision 2.1  86/06/06  07:26:32  jqj
 * many mods for better symbol table management:  added CurrentModule,
 *  made check_dependency, make_symbol, check_def set/use/use a symbol
 *  table instead of a module name string, etc.  Result is that we can
 *  now handle DEPENDS UPON 2 versions of same program.
 * 
 * Revision 2.0  85/11/21  07:21:27  jqj
 * 4.3BSD standard release
 * 
 * Revision 1.5  85/11/20  13:01:59  root
 * Gould bugfixes, I guess
 * 
 * Revision 1.4  85/05/06  08:12:54  jqj
 * Almost Beta-test version.
 * 
 * Revision 1.3  85/03/11  16:38:34  jqj
 * Public alpha-test version, released 11 March 1985
 * 
 * Revision 1.2  85/02/21  11:04:43  jqj
 * alpha test version
 * 
 * Revision 1.1  85/02/15  13:55:13  jqj
 * Initial revision
 * 
 */

#include "compiler.h"

static int 
type_check_list(typtr, p)
	struct type *typtr;
	list p;
{
	for ( ; p != NIL ; p = cdr(p))
		if (! type_check(typtr, (struct constant *) car(p)) )
			return(0);
	return(1);
}

static int
type_check_enumeration(typtr, value)
	struct type *typtr;
	struct constant *value;
{
	list p;

	if (typtr->type_constr != value->cn_constr)
		return(0);
	for (p = typtr->type_list; p != NIL; p = cdr(p))
		if (streq(value->cn_value, enumname_of(caar(p))))
		/* name_of((struct object *) caar(p)))) */
			return(1);
	return(0);
}

static int
type_check_record(typtr, value)
	struct type *typtr;
	struct constant *value;
{
	if (typtr->type_constr != value->cn_constr)
		return(0);
	/* ### not yet implemented */
	return(1);
}

/*
 * Make sure a number is a valid constant for this type.
 */
int
type_check(typtr, value)
	struct type *typtr;
	struct constant *value;
{

	switch (typtr->type_constr) {
	case C_NUMERIC:
	case C_BOOLEAN:
	case C_STRING:
		return(typtr->type_constr == value->cn_constr);
	case C_ENUMERATION:
		return( type_check_enumeration(typtr, value) );
	case C_ARRAY:
		if (value->cn_constr == C_RECORD && value->cn_list == NIL &&
		    typtr->type_size == 0)
			return(1);
		return( (typtr->type_constr == value->cn_constr) &&
			typtr->type_size == length(value->cn_list) &&
			type_check_list(typtr->type_basetype, value->cn_list));
	case C_SEQUENCE:
		if (value->cn_constr == C_ARRAY) {
			value->cn_constr = C_SEQUENCE;
		}
		if (value->cn_constr == C_RECORD && value->cn_list == NIL)
			return(1);
		return( (typtr->type_constr == value->cn_constr) &&
			type_check_list(typtr->type_basetype, value->cn_list));
	case C_RECORD:
		return( type_check_record(typtr, value) );
	case C_PROCEDURE:
	case C_ERROR:
		return(value->cn_constr == C_NUMERIC);
	case C_CHOICE:
		return(1);	/* should do better check here */
	}
	/* oops.  Should be an error here */
	return(0);
}
