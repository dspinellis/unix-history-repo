#ifndef lint
static char RCSid[] = "$Header: constants.c,v 2.0 85/11/21 07:21:34 jqj Exp $";
#endif

/* $Log:	constants.c,v $
 * Revision 2.0  85/11/21  07:21:34  jqj
 * 4.3BSD standard release
 * 
 * Revision 1.3  85/03/11  16:39:09  jqj
 * *** empty log message ***
 * 
 * Revision 1.3  85/03/11  16:39:09  jqj
 * Public alpha-test version, released 11 March 1985
 * 
 * Revision 1.2  85/02/21  11:05:02  jqj
 * alpha test version
 * 
 * Revision 1.1  85/02/15  13:55:22  jqj
 * Initial revision
 * 
 */

/*
 * Generate build symbol tables, etc. for constant declarations.
 */

#include "compiler.h"

/*
 * Allocate storage for constants
 */
struct constant *
make_constant(constr)
	enum constr constr;
{
	struct constant *c;

	c = New(struct constant);
	c->cn_constr = constr;
	return(c);
}


/*
 * Generate parse tree for simple constants -- Booleans
 */
struct constant *
Boolean_constant(value)
	char *value;
{
	struct constant *c;

	c = make_constant(C_BOOLEAN);
	c->cn_value = value;
	return(c);
}

/*
 * Generate parse tree for simple constants -- strings
 */
struct constant *
String_constant(value)
	char *value;
{
	struct constant *c;

	c = make_constant(C_STRING);
	c->cn_value = value;
	return(c);
}

/*
 * Generate parse tree for simple constants -- numeric values
 * Note that we don't know the actual type of such constants;
 * they are type-compatible with any numeric type.
 */
struct constant *
Numeric_constant(value)
	char *value;
{
	struct constant *c;

	c = make_constant(C_NUMERIC);
	c->cn_value = value;
	return(c);
}
/*
 * Generate parse tree for simple constants -- enumerations
 * Note that we don't know the actual type of such constants;
 * they are type-compatible with any numeric type.
 */
struct constant *
enumeration_constant(value)
	char *value;
{
	struct constant *c;

	c = make_constant(C_ENUMERATION);
	c->cn_value = value;
	return(c);
}

/*
 * Generate parse tree for complex constants -- arrays and sequences
 * Note that we treat them all as arrays, and allow sequences to
 * be type-compatible with arrays at declaration time.
 */
struct constant *
array_constant(values)
	list values;
{
	struct constant *c;

	c = make_constant(C_ARRAY);
	c->cn_list = values;
	return(c);
}

/*
 * Generate parse tree for complex constants -- records
 * As a special case, NIL record constants are also type-compatible
 * with some arrays and all sequences.
 */
struct constant *
record_constant(values)
	list values;
{
	struct constant *c;

	c = make_constant(C_RECORD);
	c->cn_list = values;
	return(c);
}

/*
 * Generate parse tree for complex constants -- choices
 */
struct constant *
choice_constant(values)
	list values;
{
	struct constant *c;

	c = make_constant(C_CHOICE);
	c->cn_list = values;
	return(c);
}
