#ifndef lint
static char sccsid[] = "@(#)code2.c	4.1 (Berkeley) 7/3/83";
#endif

#include "Courier.h"

/*
 * Generate functions for user and server calls to a procedure.
 */
proc_functions(proc_name, type, value)
	char *proc_name;
	struct object *type, *value;
{
	list p, q;
	int nresults;
	struct object *t, *result_type;
	char *result_name, *func, *ref;

	/*
	 * Make sure there is at most one result returned.
	 */
	nresults = length(type->t_results);
	if (nresults == 1)
		/* could be multiple names with one type */
		nresults = length(car(car(type->t_results)));
	if (nresults > 1) {
		yyerror("Procedures that return multiple results are not supported");
		return;
	}
	if (nresults) {
		result_name = name_of(car(car(car(type->t_results))));
		result_type = (struct object *) cdr(car(type->t_results));
	}

	/*
	 * Server routine.
	 */
	if (nresults) {
		fprintf(sf, "\nextern ");
		print_decl(sf, proc_name, result_type, 1);
		fprintf(sf, "();\n");
	} else
		fprintf(sf, "\nextern void %s();\n", proc_name);
	fprintf(sf,
"\nServer_%s(_buf)\n\
\tregister Unspecified *_buf;\n\
{\n\
\tregister Unspecified *_bp;\n\
\tregister LongCardinal _n;\n",
		proc_name);
	print_level++;
	for (p = type->t_args; p != NIL; p = cdr(p)) {
		t = (struct object *) cdr(car(p));
		for (q = car(car(p)); q != NIL; q = cdr(q))
			print_decl(sf, name_of(car(q)), t, 0);
	}
	if (nresults)
		print_decl(sf, result_name, result_type, 0);
	print_level--;
	fprintf(sf, "\n\t_bp = _buf;\n");
	for (p = type->t_args; p != NIL; p = cdr(p)) {
		t = (struct object *) cdr(car(p));
		ref = refstr(t);
		for (q = car(car(p)); q != NIL; q = cdr(q))
			fprintf(sf, "\t_bp += %s(%s%s, _bp);\n",
				unpack_function(t), ref, name_of(car(q)));
	}
	if (nresults)
		fprintf(sf, "\t%s = %s(", result_name, proc_name);
	else
		fprintf(sf, "\t%s(", proc_name);
	for (p = type->t_args; p != NIL; p = cdr(p)) {
		for (q = car(car(p)); q != NIL; q = cdr(q)) {
			fprintf(sf, "%s", name_of(car(q)));
			if (cdr(q) != NIL)
				fprintf(sf, ", ");
		}
		if (cdr(p) != NIL)
			fprintf(sf, ", ");
	}
	fprintf(sf, ");\n");
	if (nresults) {
		func = pack_function(result_type);
		ref = refstr(result_type);
		fprintf(sf,
"\t_n = %s(%s%s, 0, 0);\n\
\t_bp = Allocate(_n);\n\
\t%s(%s%s, _bp, 1);\n\
\tSendReturnMessage(_n, _bp);\n\
\tDeallocate(_bp);\n",
			func, ref, result_name, func, ref, result_name);
	}
	fprintf(sf, "}\n");

	/*
	 * Remote access routine.
	 */
	if (nresults) {
		fprintf(hf, "\nextern ");
		print_decl(hf, proc_name, result_type, 1);
		fprintf(hf, "();\n");

		fprintf(uf, "\n");
		print_decl(uf, proc_name, result_type, 1);
		fprintf(uf, "(");
	} else {
		fprintf(hf, "\nextern void %s();\n", proc_name);
		fprintf(uf, "\nvoid %s(", proc_name);
	}
	if (explicit) {
		fprintf(uf, "_machine");
		if (type->t_args != NIL)
			fprintf(uf, ", ");
	}
	for (p = type->t_args; p != NIL; p = cdr(p)) {
		for (q = car(car(p)); q != NIL; q = cdr(q)) {
			fprintf(uf, "%s", name_of(car(q)));
			if (cdr(q) != NIL)
				fprintf(uf, ", ");
		}
		if (cdr(p) != NIL)
			fprintf(uf, ", ");
	}
	fprintf(uf, ")\n");
	if (explicit)
		fprintf(uf, "\tString _machine;\n");
	print_level++;
	for (p = type->t_args; p != NIL; p = cdr(p)) {
		t = (struct object *) cdr(car(p));
		for (q = car(car(p)); q != NIL; q = cdr(q))
			print_decl(uf, name_of(car(q)), t, 0);
	}
	fprintf(uf, "{\n");
	if (nresults)
		print_decl(uf, result_name, result_type, 0);
	fprintf(uf,
"\tregister Unspecified *_buf, *_bp;\n\
\tregister LongCardinal _n;\n\
\n\
\t_n = 0;\n");
	print_level--;
	for (p = type->t_args; p != NIL; p = cdr(p)) {
		t = (struct object *) cdr(car(p));
		ref = refstr(t);
		for (q = car(car(p)); q != NIL; q = cdr(q))
			fprintf(uf, "\t_n += %s(%s%s, 0, 0);\n",
				pack_function(t), ref, name_of(car(q)));
	}
	fprintf(uf,
"\t_buf = Allocate(_n);\n\
\t_bp = _buf;\n");
	for (p = type->t_args; p != NIL; p = cdr(p)) {
		t = (struct object *) cdr(car(p));
		ref = refstr(t);
		for (q = car(car(p)); q != NIL; q = cdr(q))
			fprintf(uf, "\t_bp += %s(%s%s, _bp, 1);\n",
				pack_function(t), ref, name_of(car(q)));
	}
	if (explicit)
		fprintf(uf,
"\tSendCallMessage(CourierProgram(\"%s\", _machine), %s, _n, _buf);\n",
			program_name, obj_rep(value));
	else
		fprintf(uf,
"\tSendCallMessage(_%sConnection, %s, _n, _buf);\n",
			program_name, obj_rep(value));
	fprintf(uf, "\tDeallocate(_buf);\n");
	if (nresults) {
		if (explicit)
			fprintf(uf,
"\t_bp = ReceiveReturnMessage(CourierProgram(\"%s\", _machine));\n",
				program_name);
		else
			fprintf(uf,
"\t_bp = ReceiveReturnMessage(_%sConnection);\n",
				program_name);
		fprintf(uf,
"\t%s(%s%s, _bp);\n\
\tDeallocate(_bp);\n\
\treturn (%s);\n",
			unpack_function(result_type), refstr(result_type),
			result_name, result_name);
	}
	fprintf(uf, "}\n");
}

program(prog)
	struct object *prog;
{
	/*
	 * Program_name should have been set by now,
	 * but a little paranoia never hurt anyone.
	 */
	if (! streq(name_of(prog), program_name)) {
		yyerror("Internal error: conflicting program names %s and %s\n",
			name_of(prog), program_name);
		exit(1);
	}
	generate_server();
}

/*
 * Generate main loop for server program.
 */
generate_server()
{
	list p;
	struct object *t, *proc, *v;

	fprintf(sf,
"\nServer()\n\
{\n\
\tCardinal procedure;\n\
\tregister Unspecified *buf;\n\
\n\
\tServerInit();\n\
\tfor (;;) {\n\
\t\tbuf = ReceiveCallMessage(&procedure);\n\
\t\tswitch (procedure) {\n"
		);
	/*
	 * Find all the procedures declared in the program.
	 */
	for (p = Types; p != NIL; p = cdr(p)) {
		t = (struct object *) cdr(car(p));
		if (t->t_constr == C_PROCEDURE) {
			proc = (struct object *) car(car(p));
			v = lookup(Values, proc);
			fprintf(sf,
"\t\tcase %s:\n\
\t\t\tServer_%s(buf);\n\
\t\t\tbreak;\n",
				obj_rep(v), name_of(proc));
		}
	}
	fprintf(sf,
"\t\tdefault:\n\
\t\t\tNoSuchProcedureValue(\"%s\", procedure);\n\
\t\t\tbreak;\n\
\t\t}\n\
\t\tDeallocate(buf);\n\
\t}\n\
}\n",
		program_name);
}

/*
 * When implicit binding is used, this routine generates functions to
 * bind the remote Courier program to a machine by setting the global
 * connection variable for the program, and to remove the binding by
 * closing the connection.
 */
generate_binding_functions()
{
	fprintf(uf,
"\nint _%sConnection = -1;\n\
\n\
Bind%sToMachine(machine)\n\
\tString machine;\n\
{\n\
\tclose(_%sConnection);\n\
\t_%sConnection = CourierActivate(\"%s\", machine);\n\
}\n\
\n\
Unbind%s()\n\
{\n\
\tclose(_%sConnection);\n\
\t_%sConnection = -1;\n\
}\n",
		program_name, program_name, program_name, program_name,
		program_name, program_name, program_name, program_name);
}
