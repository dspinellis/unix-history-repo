#ifndef lint
static char RCSid[] = "$Header: code.c,v 2.4 87/03/17 09:32:16 ed Exp $";
#endif

/* $Log:	code.c,v $
 * Revision 2.4  87/03/17  09:32:16  ed
 * changes from Webster integrated
 * 
 * Revision 2.4  87/03/17  09:32:16  ed
 * Added -I switch to establish search path for DEPENDS UPON files.
 * 
 * Revision 2.3  86/06/30  12:50:28  jqj
 * bugfix to Server() code generation from Jack Callahan -- add "break;" if
 * new Courier procedure call is for a different module.  I'm not convinced
 * this is the right fix, but have not had time to examine it in detail.
 * 
 * Revision 2.2  86/06/06  07:28:31  jqj
 * many mods for better symbol table management:  added CurrentModule,
 *  made check_dependency, make_symbol, check_def set/use/use a symbol
 *  table instead of a module name string, etc.  Result is that we can
 *  now handle DEPENDS UPON 2 versions of same program.
 * 
 * Revision 2.1  86/01/21  10:04:02  jqj
 * changes from Dan Chernikoff:  dereferencing null pointer in code
 * generation for Abort messages from servers.
 * 
 * Revision 2.0  85/11/21  07:21:28  jqj
 * 4.3BSD standard release
 * 
 * Revision 1.6  85/05/23  06:19:16  jqj
 * *** empty log message ***
 * 
 * Revision 1.6  85/05/23  06:19:16  jqj
 * Public Beta-test version, released 24 May 1985
 * 
 * Revision 1.5  85/05/06  08:12:58  jqj
 * Almost Beta-test version.
 * 
 * Revision 1.4  85/03/26  06:09:24  jqj
 * Revised public alpha-test version, released 26 March 1985
 * 
 * Revision 1.3  85/03/11  16:38:39  jqj
 * Public alpha-test version, released 11 March 1985
 * 
 * Revision 1.2  85/02/21  11:04:47  jqj
 * alpha test version
 * 
 * Revision 1.1  85/02/15  13:55:15  jqj
 * Initial revision
 * 
 */

#include "compiler.h"
#include <xnscourier/courierdb.h>

char *CurrentProgram = "unknown";
struct object *CurrentModule = (struct object *) 0;
int CurrentNumber = 0;
int CurrentVersion = 0;

extern char *dirs[];
extern int ndirs;

/*
 * Generate comments, #includes, and client binding.
 * This action is called before the module body has been reduced.
 * (jqj)
 */
program_header(symbol, number, version)
	char *symbol;
	char *number, *version;
{
	if (check_dependency(symbol)) {
		error(ERROR,"Module %s already seen.", symbol);
		return;
	}
	CurrentModule = make_module(symbol,number,version);
	CurrentProgram = symbol;
	CurrentVersion = stringtocard(version);
	CurrentNumber = stringtocard(number);

	/*
	 * only do this stuff for the main Courier program -- generate
	 * minimal code for DEPENDS UPON inclusions.
	 */
	if (recursive_flag) {
		fprintf(header1,"/* DEPENDS UPON %s NUMBER %d VERSION %d */\n",
			CurrentProgram, CurrentNumber, CurrentVersion);
		return;
	}

	/*
	 * Generate initial contents of all sorts of stuff:
	 * (1) set up gensym counter,
	 * (2) generate beginning of header files, support file, 
	 *	client file, server file
	 */
	setgensym(number,version);
	fprintf(header1,
"/*\n\
 * This header file contains inclusions for the main definitions and for\n\
 * any DEPENDS UPON modules.  It also contains #define commands to open\n\
 * the scope of the main definitons module.\n\
 *\n\
 * main inclusion:\n\
 */\n\
#include \"%s%d.h\"\n\n",
		CurrentProgram, CurrentVersion);

	/*
	 * In the definitions for this module, make sure we don't
	 * compile things twice by wrapping everything inside of a
	 * #ifndef ... #endif (the #endif is in wrapup_program() ).
	 */
	fprintf(header,
"/*\n\
 * Definitions for %s VERSION %s NUMBER %s.\n\
 */\n\
#ifndef __%s%d\n\
#define __%s%d\n\
#include <xnscourier/courier.h>\n\
#include <xnscourier/courierconnection.h>\n\n",
		CurrentProgram, version, number,
		CurrentProgram,	CurrentVersion,
		CurrentProgram, CurrentVersion);

	fprintf(support1,
"/*\n\
 * Support routines for %s.\n\
 */\n\
#include \"%s%d.h\"\n",
		CurrentProgram, CurrentProgram, CurrentVersion);

	fprintf(client,
"/*\n\
 * Client routines for %s.\n\
 */\n\
#include \"%s%d.h\"\n",
		CurrentProgram, CurrentProgram, CurrentVersion);
	fprintf(server,
"/*\n\
 * Server for %s.\n\
 */\n\
#include \"%s%d.h\"\n\
#include <xnscourier/except.h>\n\n\
extern CourierConnection *_serverConnection;\n",
		CurrentProgram, CurrentProgram, CurrentVersion);
}

/*
 * Recursively parse a program to get types and constants.
 * as a side effect, enters the program name into the SymbolTables 
 * symbol table.
 */
ref_program(name, number, version)
	char *name, *number, *version;
{
	long save_offset;
	char *save_input_file;
	char buf[MAXSTR];
	int *p;
	int intversion, i;
	extern int *save_parser_state();
	struct courierdbent *dbent;

	intversion = stringtocard(version);

	/*
	 * in a DEPENDS UPON inclusion, generate minimal code, making
	 * sure we don't do it twice.  The included program is wrapped in
	 * an #ifdef ... #endif
	 */
	if (!recursive_flag) {
		fprintf(header,"\n\
/*\n\
 * Definitions from DEPENDS UPON %s inclusion\n\
 * (must be linked with %s%d_support.c also)\n\
 */\n\
#include <xnscourier/%s%d.h>\n",
			name,
			name, intversion,
			name, intversion);
	}
	if (check_module_def(name,number,version)) {
		/* we've already parsed this one, so don't bother to redo */
		/* as a side effect, check_module_def adds this module to */
		/* the dependency list for CurrentModule */
		return;
	}

	save_offset = ftell(stdin);
	save_input_file = input_file;
	sprintf(buf, "%s%d.cr", name, intversion);
	input_file = buf;
	if (freopen(input_file, "r", stdin) == NULL) {
		/*
		 * attempt to find file from include directories
		 *	-I switch on command line
		 */
		for ( i= 0; i < ndirs ; i++ ) {
			sprintf(buf, "%s%s%s%d.cr", dirs[i],
				(strcmp(dirs[i], "/") == 0 ? "" : "/"),
				name, intversion);
			input_file= buf;
			if (freopen(input_file, "r", stdin) != NULL)
				break;
		}
		/*
		 * if all else fails, look in courier description file
		 */
		if ( i >= ndirs ) {
			dbent = getcourierservice(stringtocard(number),intversion);
			if (dbent == NULL ||
			    dbent->cr_description == NULL ||
			    (input_file = copy(dbent->cr_description)) == NULL ||
			    freopen(input_file, "r", stdin) == NULL) {
				error(ERROR, "file %s not found", input_file);
				input_file = save_input_file;
				freopen(input_file, "r", stdin);
				fseek(stdin, save_offset, 0);
				return;
			}
		}
	}
	p = save_parser_state();
	/* note:  recursive_flag is now set */
	(void) yyparse();	/* recursively parse */
	restore_parser_state(p);
	input_file = save_input_file;
	freopen(input_file, "r", stdin);
	fseek(stdin, save_offset, 0);
	return;
}

/*
 * Generate any code needed after DEPENDS UPON modules have been
 * parsed, but before declarations.
 */
program_body()
{
	if (recursive_flag)
		return;
	fprintf(header1,
"/*\n\
 * Widen scope to include all symbols defined in main inclusion:\n\
 */\n");
 }

/*
 * Generate code relating to binding.
 * This action is called after the entire module has been reduced.
 */
wrapup_program(prog)
	char *prog;
{
	if (recursive_flag)
		return;
	fprintf(header,"\n#endif __%s\n\n", CurrentProgram);
	if (!streq(prog,CurrentProgram))
		error(FATAL,"internal error (module): conflicting module names, %s and %s",
			prog,CurrentProgram);
	generate_server_binding();
	generate_client_binding();
}

/*
 * Generate export function for server.
 */
generate_server_binding()
{
	list p;

	fprintf(server,
"\nServer(skipcount,skippedwords)\n\
\tint skipcount;\n\
\tUnspecified skippedwords[];\n\
{\n\
\tCardinal _procedure;\n\
\tregister Unspecified *_buf;\n\
\tLongCardinal programnum;\n\
\tCardinal versionnum;\n\
\tCardinal _n;\n\
\n\
\tfor (;;) {\n\
\t\t_buf = ReceiveCallMessage(&_procedure, skipcount, skippedwords);\n\
\t\tDURING switch (_procedure) {\n");
	/*
	 * Find all the procedures declared in the program.
	 */
	for (p = Procedures; p != NIL; p = cdr(p)) {
		fprintf(server,
"\t\tcase %s:\n\
\t\t\tserver_%s(_buf);\n\
\t\t\tbreak;\n",
			(char *)cdar(p), (char *)caar(p));
	}
	fprintf(server,
"\t\tdefault:\n\
\t\t\tNoSuchProcedureValue(\"%s\", _procedure);\n\
\t\t\tbreak;\n\
\t\t} HANDLER {\n\
\t\t    Deallocate(_buf);\n\
\t\t    switch (Exception.Code) {\n",
		CurrentProgram);
	for (p = Errors; p != NIL; p = cdr(p)) {
		struct constant *errconst;
		struct type *errtype;
		errconst = (struct constant *) caar(p);
		errtype = (struct type *) cdar(p);
		if (errtype == TNIL)
			fprintf(server,
"\t\t    case %s:\n\
\t\t\t_buf = Allocate(0);\n\
\t\t\tSendAbortMessage(Exception.Code-ERROR_OFFSET, 0, _buf);\n\
\t\t\tbreak;\n",
				errconst->cn_name);
		/* errtype != TNIL */
		else if (typename(errtype) == NULL) {
			error(ERROR,"Internal error (server): unexpanded type for %s",
				errconst->cn_name);
			break;
		}
		/* errtype != TNIL && typename(errtype) != NULL */
		else fprintf(server,
"\t\t    case %s:\n\
\t\t\t_n = sizeof_%s((%s *)Exception.Message);\n\
\t\t\t_buf = Allocate(_n);\n\
\t\t\t(void) %s((%s*)Exception.Message, _buf);\n\
\t\t\tSendAbortMessage(Exception.Code-ERROR_OFFSET, _n, _buf);\n\
\t\t\tbreak;\n",
				errconst->cn_name,
				typename(errtype), typename(errtype),
				xfn(EXTERNALIZE,errtype), typename(errtype));
	}
	fprintf(server,
"\t\t    default:\n\
\t\t\t_buf = Allocate(0);\n\
\t\t\tSendRejectMessage(unspecifiedError, 0, _buf);\n\
\t\t\tbreak;\n\
\t\t    }\n\
\t\t} END_HANDLER;\n\
\t\tDeallocate(_buf);\n\
\t\tfor (;;) {\n\
\t\t\tskipcount = LookAheadCallMsg(&programnum, &versionnum,\n\
\t\t\t\t\tskippedwords);\n\
\t\t\tif (skipcount < 0) return(0);\t/* timed out */\n\
\t\t\tif (programnum != %d || versionnum != %d)\n\
\t\t\t\tExecCourierProgram(programnum, versionnum,\n\
\t\t\t\t\t\tskipcount, skippedwords);\n\
\t\t\telse break;\n\
\t\t\}  /* loop if can't exec that program */\n\
\t}\n\
}\n",
		CurrentNumber, CurrentVersion
		);
}

/*
 * Generate function for importing server module.
 */
generate_client_binding()
{
}
