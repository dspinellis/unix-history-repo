#ifndef lint
static char RCSid[] = "$Header: procedures.c,v 2.2 86/11/11 09:49:06 jqj Exp $";
#endif

/* $Log:	procedures.c,v $
 * Revision 2.2  86/11/11  09:49:06  jqj
 * Per Ed Flint, if a Courier procedure returns void, the server should still
 * do a SendReturnMessage(0,0), so generate the proper code for this case.
 * Note that this eventually implies (in xnslib/readwrite.c) that a writev()
 * has an iovec with 0 length, which might tickle a kernel bug in some
 * implementations.
 * 
 * Revision 2.1  86/06/06  07:28:49  jqj
 * many mods for better symbol table management:  added CurrentModule,
 *  made check_dependency, make_symbol, check_def set/use/use a symbol
 *  table instead of a module name string, etc.  Result is that we can
 *  now handle DEPENDS UPON 2 versions of same program.
 * 
 * Revision 2.0  85/11/21  07:21:43  jqj
 * 4.3BSD standard release
 * 
 * Revision 1.5  85/05/06  08:13:31  jqj
 * *** empty log message ***
 * 
 * Revision 1.5  85/05/06  08:13:31  jqj
 * Almost Beta-test version.
 * 
 * Revision 1.4  85/03/26  06:10:21  jqj
 * Revised public alpha-test version, released 26 March 1985
 * 
 * Revision 1.3  85/03/11  16:39:55  jqj
 * Public alpha-test version, released 11 March 1985
 * 
 * Revision 1.2  85/02/21  11:05:39  jqj
 * alpha test version
 * 
 * Revision 1.1  85/02/15  13:55:36  jqj
 * Initial revision
 * 
 */

#define argname(p)	((char *) car(caar(p)))
#define argtype(p)	((struct type *) cdar(p))

/*
 * routines for generating procedures and errors
 */

#include "compiler.h"

/*
 * Generate client and server functions for procedure declarations.
 */
define_procedure_constant(symbol,typtr,value)
	struct object *symbol;
	struct type *typtr;
	struct constant *value;
{
	struct type *resulttype;
	char *procvalue;
	char * resultname;
	char buf[MAXSTR];
	list p, q;

	if (recursive_flag)	/* don't bother to do anything for procs */
		return;		/* in DEPENDS UPON modules */
	if (typtr->type_constr != C_PROCEDURE)
		error(FATAL, "internal error (define_procedure): not a procedure");
	if (value->cn_constr != C_NUMERIC) {
		error(ERROR,"Values of procedure constants must be numeric");
		procvalue = "-1";
	}
	else
		procvalue = value->cn_value;
	/*
	 * RETURNS stuff:  coerce the result to be a single record
	 */
	if (length(typtr->type_results) > 0) {
		struct object *resultobj;

		resulttype = record_type(typtr->type_results);
		sprintf(buf,"%sResults",name_of(symbol));
		resultname = copy(buf);
		resultobj = make_symbol(resultname,CurrentModule);
		define_type(resultobj, resulttype);
		/* replaces define_record_type(resulttype); */
		typtr->type_results = cons( cons( cons((list)resultname, NIL),
						  (list)resulttype), 
					    NIL);
	}
	/*
	 * REPORTS stuff:  check here to make sure the errors are all defined
	 */
	for (p = typtr->type_errors, q = NIL; p != NIL; q = p, p = cdr(p)) {
		struct object *sym;
		sym = check_def((char *)car(p),CurrentModule);
		if (sym == (struct object *)0) {
			error(ERROR,"Error constant %s not defined",
				(char*)car(p));
			if (q == NIL) typtr->type_errors = cdr(p);
			else cdr(q) = cdr(p);
		}
		else if (sym->o_class != O_CONSTANT
		    || sym->o_constant->cn_constr != C_ERROR) {
			error(ERROR,"Symbol %s is not of appropriate type",
				name_of(sym));
			if (q == NIL) typtr->type_errors = cdr(p);
			else cdr(q) = cdr(p);
		}
	}
	/*
	 * Argument stuff:  make sure all the argument types are defined
	 */
	for (p = typtr->type_args; p != NIL; p = cdr(p)) {
		if (typename(argtype(p)) == NULL) {
			struct object *name;
			name = make_symbol(gensym("T_p"),CurrentModule);
			define_type(name,argtype(p));
		}
	}
	/*
	 * Actually generate code for this procedure
	 */
	proc_functions(symbol->o_constant->cn_name, typtr, procvalue);
	/*
	 * Save this procedure on the global procs for wrapup (server 
	 * dispatch code)
	 */
	Procedures = cons(cons( (list)symbol->o_constant->cn_name,
				(list)procvalue ),
			  Procedures);
}


/*
 * Generate funcions for client and server calls to a procedure.
 */
proc_functions(proc_name, type, proc_number)
	char *proc_name;
	struct type *type;
	char *proc_number;
{
	list p;
	int nresults, fixed_size, variable_size;
	struct type *t, *bt, *result_type;
	char *result_name, *ref, *rtname;

	/*
	 * Make sure there is at most one result returned.
	 */
	nresults = length(type->type_results);
	if (nresults > 1) {
		error(ERROR, "procedures that return multiple results are not supported");
		return;
	}
	if (nresults) {
		result_name = "_Results";
		result_type = argtype(type->type_results);
		rtname = typename(result_type);
	} else {
		rtname = "void";
	}

	/*
	 * Server routine.
	 */

	fprintf(server, "\nextern %s %s();\n", rtname, proc_name);
	fprintf(server,
"\nserver_%s(_buf)\n\
\tregister Unspecified *_buf;\n\
{\n\
\tregister Unspecified *_bp = _buf;\n\
\tregister LongCardinal _n;\n",
		proc_name);
	for (p = type->type_args; p != NIL; p = cdr(p)) {
		t = argtype(p);
		fprintf(server, "\t%s %s;\n", typename(t), argname(p));
	}
	if (nresults)
		fprintf(server, "\t%s %s;\n", rtname, result_name);
	fprintf(server, "\n");
	/*
	 * Generate code to internalize the arguments.
	 */
	for (p = type->type_args; p != NIL; p = cdr(p)) {
		t = argtype(p);
		ref = refstr(t);
		fprintf(server, "\t_bp += %s(%s%s, _bp);\n",
			xfn(INTERNALIZE, t), ref, argname(p));
	}
	/*
	 * Generate code to call the procedure.
	 */
	if (nresults)
		fprintf(server, "\t%s = %s(_serverConnection, 0",
			result_name, proc_name);
	else
		fprintf(server, "\t%s(_serverConnection, 0", proc_name);
	for (p = type->type_args; p != NIL; p = cdr(p)) {
		fprintf(server, ", %s", argname(p));
	}
	fprintf(server, ");\n");
	/*
	 * Generate code to externalize the result.
	 */
	if (nresults) {
		ref = refstr(result_type);
		fprintf(server,
"\t_n = sizeof_%s(%s%s);\n\
\t_bp = Allocate(_n);\n\
\t%s(%s%s, _bp);\n\
\tSendReturnMessage(_n, _bp);\n\
\tDeallocate(_bp);\n\
}\n",
			rtname, ref, result_name,
			xfn(EXTERNALIZE, result_type), ref, result_name);
	} else
		fprintf(server,
"\tSendReturnMessage( 0, _bp);\n\
}\n"
			);

	/*
	 * Stub routine for client.
	 */

	fprintf(header, "\nextern %s %s();\n",
		rtname, proc_name);
	fprintf(client,
"\n\
%s\n\
%s(_Connection, _BDTprocptr",
		rtname, proc_name);
	for (p = type->type_args; p != NIL; p = cdr(p))
		fprintf(client, ", %s", argname(p));
	fprintf(client, ")\n\
\tCourierConnection *_Connection;\n\
\tint (*_BDTprocptr)();\n\
"
		);
	for (p = type->type_args; p != NIL; p = cdr(p)) {
		t = argtype(p);
		fprintf(client, "\t%s %s;\n", typename(t), argname(p));
	}
	fprintf(client, "{\n");
	if (nresults)
		fprintf(client, "\t%s %s;\n", rtname, result_name);
	fprintf(client,
"\tregister Unspecified *_buf, *_bp;\n\
\tBoolean _errorflag;\n\
\tCardinal _errtype;\n"
		);
	/*
	 * Determine the size of the arguments.
	 * This is like the code in record_type().
	 */
	fixed_size = 0;
	variable_size = 0;
	for (p = type->type_args; p != NIL; p = cdr(p)) {
		bt = argtype(p);
		if (bt->type_xsize == -1) {
			variable_size = 1;
		} else {
			fixed_size += bt->type_xsize;
		}
	}
	if (!variable_size) {
		/*
		 * The argument list is fixed-size.
		 */
		fprintf(client,
"\n\
\t_buf = Allocate(%d);\n",
			fixed_size);
	} else {
		/*
		 * There are some variable-size arguments.
		 */
		fprintf(client,
"\tregister LongCardinal _n = %d;\n\
\n",
			fixed_size);
		for (p = type->type_args; p != NIL; p = cdr(p)) {
			t = argtype(p);
			bt = t;
			if (bt->type_xsize != -1)
				continue;
			ref = refstr(bt);
			fprintf(client,
"\t_n += sizeof_%s(%s%s);\n",
				typename(t), ref, argname(p));
		}
		fprintf(client,
"\t_buf = Allocate(_n);\n"
			);
	}
	fprintf(client,
"\t_bp = _buf;\n"
		);
	/*
	 * Generate code to externalize the arguments.
	 */
	for (p = type->type_args; p != NIL; p = cdr(p)) {
		t = argtype(p);
		ref = refstr(t);
		fprintf(client, "\t_bp += %s(%s%s, _bp);\n",
			xfn(EXTERNALIZE, t), ref, argname(p));
	}
	if (!variable_size) {
		fprintf(client,
"\tSendCallMessage(_Connection, %d, %d, %s, %d, _buf);\n",
			CurrentNumber, CurrentVersion,
			proc_number, fixed_size);
	} else {
		fprintf(client,
"\tSendCallMessage(_Connection, %d, %d, %s, _n, _buf);\n",
			CurrentNumber, CurrentVersion,
			proc_number);
	}
	fprintf(client,
"\tDeallocate(_buf);\n\
\tMaybeCallBDTHandler(_Connection, _BDTprocptr);\n"
		);
	/*
	 * Generate code to receive the results and interpret them
	 * as errors
	 */
	fprintf(client,
"\t_bp = ReceiveReturnMessage(_Connection, &_errorflag);\n\
\t_buf = _bp;\n\
\tif (_errorflag) {\n\
\t\t_bp += %s(&_errtype, _bp);\n\
\t\tswitch (ERROR_OFFSET+_errtype) {\n",
		xfn(INTERNALIZE, Cardinal_type)
			);
	for (p = type->type_errors; p != NIL; p = cdr(p)) {
		struct constant *errconst;
		struct type *errtype;
		errconst = (check_def((char *)car(p),CurrentModule))->o_constant;
		errtype = (struct type *) cdr(errconst->cn_list);
		if (errtype == TNIL)
			fprintf(client,
"\t\tcase %s:\n\
\t\t\traise(ERROR_OFFSET+_errtype, 0);\n\
\t\t\t/*NOTREACHED*/\n",
				errconst->cn_name);
		else
			fprintf(client,
"\t\tcase %s: {\n\
\t\t\tstatic %s _result;\n\
\t\t\t_bp += %s(%s_result, _bp);\n\
\t\t\traise(ERROR_OFFSET+_errtype, (char *) &_result);\n\
\t\t\t/*NOTREACHED*/\n\
\t\t\t}\n",
				errconst->cn_name,
				typename(errtype),
				xfn(INTERNALIZE, errtype), refstr(errtype)
				);
	}
	fprintf(client,
"\t\tdefault:\n\
\t\t\t/* don't know how to unpack this */\n\
\t\t\traise(ERROR_OFFSET+_errtype, 0);\n\
\t\t\t/*NOTREACHED*/\n\
\t\t}\n"
		);
	/*
	 * Code to unpack results and return
	 */
	if (nresults)
		fprintf(client,
"\t} else\n\
\t\t_bp += %s(%s%s, _bp);\n\
\tDeallocate(_buf);\n\
\treturn (%s);\n\
}\n",
			xfn(INTERNALIZE, result_type),
			refstr(result_type), result_name, result_name);
	else
		fprintf(client,
"\t}\n\
\tDeallocate(_buf);\n\
}\n");
}
