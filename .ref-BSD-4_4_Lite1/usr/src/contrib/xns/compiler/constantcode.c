#ifndef lint
static char RCSid[] = "$Header: constantcode.c,v 2.0 85/11/21 07:21:32 jqj Exp $";
#endif

/* $Log:	constantcode.c,v $
 * Revision 2.0  85/11/21  07:21:32  jqj
 * 4.3BSD standard release
 * 
 * Revision 1.4  85/05/23  06:19:32  jqj
 * *** empty log message ***
 * 
 * Revision 1.4  85/05/23  06:19:32  jqj
 * Public Beta-test version, released 24 May 1985
 * 
 * Revision 1.3  85/03/26  06:09:41  jqj
 * Revised public alpha-test version, released 26 March 1985
 * 
 * Revision 1.2  85/03/11  16:38:56  jqj
 * Public alpha-test version, released 11 March 1985
 * 
 * Revision 1.1  85/02/15  13:55:18  jqj
 * Initial revision
 * 
 */

/*
 * Generate code for constant declarations.
 */

#include "compiler.h"

/*
 * Generate code for constant declarations
 */
define_constant(name, typtr, value)
	struct object *name;
	struct type *typtr;
	struct constant *value;
{
	char *fullname;

	name->o_class = O_CONSTANT;
	name->o_constant = value;
	fullname = make_full_name( name->o_module,
				name->o_modversion, name_of(name));
	/*
	 * Check for simple case of Foo: TypeBaz = Mumble;
	 * where Mumble is another constant.  In this case,
	 * just use the existing declaration
	 */
	if (value->cn_name != NULL) {
		if (!recursive_flag) {
			fprintf(header,"#define %s %s\n",
				fullname, value->cn_name);
			/* open scope */
			fprintf(header1,"#define %s %s\n",
				name_of(name), value->cn_name);
		}
		return;
	}
	/*
	 * We have to generate some code for this one.  We'll generate
	 * the declaration in the header file of a static variable
	 * initialized to the appropriate values.
	 */
	value->cn_name = fullname;
	if (recursive_flag)
		return;		/* it's already been expanded elsewhere */
	/* open scope */
	fprintf(header1,"#define %s %s\n", name_of(name), fullname);
	/* make sure the type is defined */
	if (typename(typtr) == NULL) {
		/* create an anonymous (not in symboltable) type and subtypes */
		char * typenam;
		typenam = gensym("T_cn");
		code_type(typenam, typtr);
		typename(typtr) = typenam;
		}
	/* depending on the type, generate appropriate initializer */
	switch (typtr->type_constr) {
	case C_PROCEDURE:
		define_procedure_constant(name, typtr, value);
		break;
	case C_ERROR:
		define_error_constant(name, typtr, value);
		break;
	case C_NUMERIC:
	case C_BOOLEAN:
	case C_STRING:
	case C_ENUMERATION:
		/* these are simple, since they can't include sequences */
		fprintf(header, "\nstatic %s %s = {%s};\n",
			typename(typtr), value->cn_name, value->cn_value);
		break;
	default:
		/* the general case */
		scan_for_sequences(typtr, value);	/* kludge */
		fprintf(header, "\nstatic %s %s = ",
				typename(typtr), value->cn_name);
		code_constant(typtr, value);
		fprintf(header,";\n");
		break;
	}
	return;
}


/*
 * Generate client and server code for error constants
 */
define_error_constant(symbol,typtr,value)
	struct object *symbol;
	struct type *typtr;
	struct constant *value;
{
	char *errvalue;

	if (recursive_flag)
		return;		/* can't happen */
	if (typtr->type_constr != C_ERROR)
		error(FATAL, "internal error (define_error_constant): not an error");
	if (value->cn_constr != C_NUMERIC) {
		error(ERROR,"Values of ERRORs must be numeric");
		errvalue = "-1";
	}
	else
		errvalue = value->cn_value;
	fprintf(header,"\n#define %s (ERROR_OFFSET+%s)\n\
#define %sArgs %s\n",
			value->cn_name, errvalue,
			value->cn_name, typename(typtr));
	fprintf(header1,"#define %sArgs %sArgs\n",
			symbol->o_name, value->cn_name);
	value->cn_constr = C_ERROR;
	/* put this error in the constant's data structure */
	/* also store this error on the global list */
	if (typtr->type_list == NIL) {
		value->cn_list = cons((list) errvalue, NIL);
		Errors = cons( cons((list) value, NIL), Errors);
	}
	else {
		value->cn_list = cons((list) errvalue, (list) typtr);
		Errors = cons( cons((list) value, (list) typtr), Errors);
	}
}

/*
 * recursively generate the code for a constant
 */
code_constant(typtr, value)
	struct type *typtr;
	struct constant *value;
{
	switch (typtr->type_constr) {
	case C_NUMERIC:
	case C_BOOLEAN:
	case C_STRING:
	case C_ENUMERATION:
		if (value == (struct constant*) 0)
		  fprintf(header, "0");
		else
		  fprintf(header, "%s", value->cn_value);
		break;
	case C_ARRAY:
		code_array_constant(typtr,value);
		break;
	case C_SEQUENCE:
		code_sequence_constant(typtr,value);
		break;
	case C_RECORD:
		code_record_constant(typtr,value);
		break;
	case C_CHOICE:
		code_choice_constant(typtr,value);
		break;
	case C_ERROR:
		error(ERROR,"Error constants may not be part of a structure");
		break;
	case C_PROCEDURE:
		error(ERROR,"Procedures may not be part of a structure");
	}
}

/*
 * Given the name of a record field and a record constant, return
 * the corresponding component of the record constant.
 */
static struct constant *
findcomponent(name,recvalue)
	char *name;
	struct constant *recvalue;
{
	list p;

	if (recvalue->cn_constr != C_RECORD)
		error(FATAL,"internal error (findcomponent): constant is of type %d",
			recvalue->cn_constr);
	for (p = recvalue->cn_list; p != NIL; p = cdr(p))
		if (streq((char *) caar(p), name))
			return((struct constant *) cdar(p));
	return((struct constant *) 0);
}


/*
 * kludge since PCC doesn't like initializations of the form
 *   struct {int length; Seqtype *sequence} seq = {3,{1,2,3}};
 * instead, use:
 *   Seqtype anonymous[3] = {1,2,3};
 *   struct {int length; Seqtype *sequence} seq = {3,anonymous};
 * We have to generate the sequence value before we walk the constant.
 */
scan_for_sequences(typtr, value)
	struct type *typtr;
	struct constant *value;
{
	list p;

	switch (typtr->type_constr) {
	case C_ARRAY:
		for (p = value->cn_list; p != NIL; p = cdr(p))
			scan_for_sequences(typtr->type_basetype, 
					   (struct constant *) car(p));
		break;
	case C_RECORD:
		scan_record_for_sequences(typtr, value);
		break;
	case C_CHOICE:
		scan_choice_for_sequences(typtr, value);
		break;
	case C_SEQUENCE:
		for (p = value->cn_list; p != NIL; p = cdr(p))
			scan_for_sequences(typtr->type_basetype, 
					   (struct constant *) car(p));
		value->cn_seqvalname = gensym("S_v");
		fprintf(header,"\nstatic %s %s[%d] = {\n\t",
			typename(typtr->type_basetype), value->cn_seqvalname,
			length(value->cn_list));
		for (p = value->cn_list ; p != NIL ; p = cdr(p)) {
			code_constant(typtr->type_basetype,
				      (struct constant *) car(p));
			if (cdr(p) != NIL)
				fprintf(header,",\n\t");
		}
		fprintf(header,"\n};\n");
		break;
	default:	/* other types don't have embedded sequences */
		break;
	}
}

scan_record_for_sequences(typtr, value)
	struct type *typtr;
	struct constant *value;
{
	list p, q;
	struct constant *component;

	for (p = typtr->type_list; p != NIL; p = cdr(p)) {
		q = car(p);
		component = findcomponent((char *) caar(q),value);
		if (component != (struct constant *) 0)
			scan_for_sequences((struct type *) cdr(q), component);
	}
}

/*ARGSUSED*/
scan_choice_for_sequences(typtr, value)
	struct type *typtr;
	struct constant *value;
{
	/* constants of type CHOICE are not implemented */
}


code_array_constant(typtr, value)
	struct type *typtr;
	struct constant *value;
{
	list p;
	int i;

	if (value == (struct constant *) 0) {
		fprintf(header,"{0");
		for (i = 1; i < typtr->type_size; i++)
		  fprintf(header,",0");
		fprintf(header,"}");
		return;
	}
	if (typtr->type_size != length(value->cn_list))
		error(WARNING,"wrong number of constant elements specified for array");
	fprintf(header,"\n\t{");
	for (p = value->cn_list; p != NIL; p = cdr(p)) {
		code_constant(typtr->type_basetype,(struct constant *) car(p));
		if (cdr(p) != NIL)
			fprintf(header,",");
	}
	fprintf(header,"}");
}

code_choice_constant(typtr, value)
	struct type *typtr;
	struct constant *value;
{
	list p,q;
	struct type *bt;
	char *desigval;
	struct object *name;

	if (value == (struct constant *)0)
	  desigval = "?";	/* caar(typtr->type_designator->type_list); */
	else
	  desigval = (char *) car(value->cn_list);
	fprintf(header,"\n\t{ %s", desigval);
	/* find the corresponding arm of the choice */
	bt = TNIL;
	for (p = typtr->type_candidates; bt==TNIL && p!=NIL; p = cdr(p)) {
		for (q = caar(p); bt==TNIL && q!=NIL; q = cdr(q)) {
			name = (struct object *) caar(q);
			if (streq(name->o_enum->en_name,desigval))
				bt = (struct type *) cdar(p);
		}
	}
	if (bt == TNIL)
		error(WARNING,"CHOICE designator %s is invalid here",desigval);
	else if (bt != NilRecord_type)
		error(WARNING,"Constants of type CHOICE are not supported");

	fprintf(header,"\n\t}");
}

code_sequence_constant(typtr, value)
	struct type *typtr;
	struct constant *value;
{
	list p;
	int l;

	if (value == (struct constant *)0 ||
	    (p = value->cn_list) == NIL) {
		fprintf(header,"{0, 0}");
		return;
	}
	l = length(p);
	if (typtr->type_size < l)
		error(WARNING,"too many constant elements specified for sequence");
	fprintf(header,"{%d, %s}",l,value->cn_seqvalname);
}

code_record_constant(typtr, value)
	struct type *typtr;
	struct constant *value;
{
	list p, q;
	struct constant *component;

	fprintf(header,"{");
	for (p = typtr->type_list; p != NIL; p = cdr(p)) {
		q = car(p);
		if (value == (struct constant *) 0)
		  component = value;
		else
		  component = findcomponent((char *) caar(q), value);
		code_constant((struct type *) cdr(q), component);
		if (cdr(p) != NIL)
			fprintf(header,",");
	}
	fprintf(header,"\n\t}");
}

