#ifndef lint
static char RCSid[] = "$Header: main.c,v 2.0 85/11/21 07:21:40 jqj Exp $";
#endif

/* $Log:	main.c,v $
 * Revision 2.0  85/11/21  07:21:40  jqj
 * 4.3BSD standard release
 * 
 * Revision 1.5  85/05/23  06:19:55  jqj
 * *** empty log message ***
 * 
 * Revision 1.5  85/05/23  06:19:55  jqj
 * Public Beta-test version, released 24 May 1985
 * 
 * Revision 1.4  85/03/26  06:10:07  jqj
 * Revised public alpha-test version, released 26 March 1985
 * 
 * Revision 1.3  85/03/11  16:39:42  jqj
 * Public alpha-test version, released 11 March 1985
 * 
 * Revision 1.2  85/02/21  11:05:24  jqj
 * alpha test version
 * 
 * Revision 1.1  85/02/15  13:55:31  jqj
 * Initial revision
 * 
 */

#include "compiler.h"
#include <errno.h>
#include <signal.h>

char *input_file;
static char header_file[MAXSTR];
FILE *header;
static char *header1_file[MAXSTR];
FILE *header1;
static char support_file1[MAXSTR];
FILE *support1;
static char support_file2[MAXSTR];
FILE *support2;
static char client_file[MAXSTR];
FILE *client;
static char server_file[MAXSTR];
FILE *server;

list Procedures, Errors;
int recursive_flag, errs;

struct type *Boolean_type, *Cardinal_type, *LongCardinal_type,
	*Integer_type, *LongInteger_type, *String_type,
	*Unspecified_type, *LongUnspecified_type, *NilRecord_type,
	*StreamEnum_type;

#ifdef DEBUG
int DebugFlag = 0;
#endif


interrupt()
{
	errs = 1;
	goodbye();
}

main(argc,argv)
	int argc;
	char **argv;
{
	if (argc != 2) {
		fprintf(stderr, "Usage: %s input_file\n",argv[0]);
		exit(1);
	}
	input_file = argv[1];
	if (freopen(input_file, "r",stdin) == NULL) {
		perror(input_file); exit(1);
	}
	tempname(header_file);
	tempname(header1_file);
	tempname(client_file); 
	tempname(server_file);
	tempname(support_file1); 
	tempname(support_file2);
	if ((header = fopen(header_file,"w")) == NULL) {
		perror(header_file); 
		goodbye();
	}
	if ((header1 = fopen(header1_file,"w")) == NULL) {
		perror(header1_file); 
		goodbye();
	}
	if ((client = fopen(client_file,"w")) == NULL) {
		perror(client_file); 
		goodbye();
	}
	if ((server = fopen(server_file,"w")) == NULL) {
		perror(server_file); 
		goodbye();
	}
	if ((support1 = fopen(support_file1,"w")) == NULL) {
		perror(support_file1); 
		goodbye();
	}
	if ((support2 = fopen(support_file2,"w")) == NULL) {
		perror(support_file2); 
		goodbye();
	}
	setup_predefs();
	(void) yyparse();
	(void) fclose(header1);
	(void) fclose(header); 
	(void) fclose(client); 
	(void) fclose(server);
	if (errs == 0) {
		register int c;

		freopen(support_file2, "r", support2);
		while ((c = getc(support2)) != EOF)
			(void) putc(c, support1);
		(void) fclose(support1); 
		(void) fclose(support2);
		(void) unlink(support_file2);
		changename(support_file1, "_support.c");
		if (Procedures != NIL) {
			changename(client_file, "_client.c");
			changename(server_file, "_server.c");
		} else {
			(void) unlink(client_file);
			(void) unlink(server_file);
		}
		changename(header_file, ".h");
		changename(header1_file, "_defs.h");
	}
	goodbye();

}


goodbye()
{
	if(errs) {
		(void) unlink(header_file);
		(void) unlink(header1_file);
		(void) unlink(client_file);
		(void) unlink(server_file);
		(void) unlink(support_file1);
		(void) unlink(support_file2);
	}
	exit(errs);
}

/*
 * Rename the source file to be <CurrentProgram><CurrentVersion><suffix> .
 */
changename(source, suffix)
	char *source, *suffix;
{
	char newname[MAXSTR];

	(void) sprintf(newname, "%s%d%s", 
			CurrentProgram, CurrentVersion, suffix);
	if (rename(source, newname) == -1)
		perror(newname);
}

/* VARARGS1 */
error(level, s, args)
	enum severity level;
	char *s;
{
	extern int yylineno;

	fprintf(stderr, "%s: %d: ", input_file, yylineno);
	if (level == WARNING)
		fprintf(stderr, "Warning: ");
	_doprnt(s, &args, stderr);
	(void) putc('\n', stderr);
	if (level == ERROR)
		errs++;
	if (level == FATAL)
		goodbye();
}

yyerror(s)
	char *s;
{
	error(ERROR, s);
}

tempname(bclient)
	char *bclient;
{
	static int n = 0;

	sprintf(bclient, "tmp%d.%d", n, getpid());
	n++;
}

struct type *
predefine_enum_type(name,elements)
	char *name;
	char **elements;
{
	struct object *symbol;
	list dlist;
	struct type *resulttype;
	char *id, *value;
	
	dlist = NIL;
	for ( ; *elements != (char*)NULL; elements += 2) {
		id = *elements;
		value = *(elements+1);
		if (check_def(id, (char*)NULL))
		  error(FATAL,"in predefine_enum_type, %s already declared",
			id);
		symbol = make_symbol(id, (char*)NULL);
		define_enumeration_symbol(symbol, value);
		dlist = cons(cons((list) symbol, (list) value),
			     dlist);
	}
	resulttype = enumeration_type(dlist);
	resulttype->type_name = name;
	resulttype->type_xsize = 1;
	return(resulttype);
}

#define PREDEFINE(xtype, xname, nwords, constr) { \
	xtype = make_type(constr); \
	xtype->type_name = xname; \
	xtype->type_xsize = nwords; \
}

/*
 * This mess is needed because C doesn't handle initialization of unions.
 * Note that all of these must correspond to declarations, plus sizeof_,
 * externalize_, and internalize_ functions, in courier.h
 */
setup_predefs()
{
	static char *streamvals[] = {"nextSegment","0","lastSegment","1",
				     (char*)0};
#ifndef lint
	PREDEFINE(Boolean_type, "Boolean", 1, C_BOOLEAN);
	PREDEFINE(Cardinal_type, "Cardinal", 1, C_NUMERIC);
	PREDEFINE(LongCardinal_type, "LongCardinal", 2, C_NUMERIC);
	PREDEFINE(Integer_type, "Integer", 1, C_NUMERIC);
	PREDEFINE(LongInteger_type, "LongInteger", 2, C_NUMERIC);
	PREDEFINE(String_type, "String", -1, C_STRING);
	PREDEFINE(Unspecified_type, "Unspecified", 1, C_NUMERIC);
	PREDEFINE(LongUnspecified_type, "LongUnspecified", 2, C_NUMERIC);
	PREDEFINE(NilRecord_type, "NilRecord", 0, C_RECORD);
	StreamEnum_type = predefine_enum_type("StreamEnumerator", streamvals);
#endif
}
