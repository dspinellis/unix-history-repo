/*	Header file for XNS Courier compiler	*/

/* $Header: compiler.h,v 2.2 86/06/06 07:28:35 jqj Exp $ */
/* $Log:	compiler.h,v $
 * Revision 2.2  86/06/06  07:28:35  jqj
 * many mods for better symbol table management:  added CurrentModule,
 *  made check_dependency, make_symbol, check_def set/use/use a symbol
 *  table instead of a module name string, etc.  Result is that we can
 *  now handle DEPENDS UPON 2 versions of same program.
 * 
 * Revision 2.1  86/05/16  05:44:42  jqj
 * make enumeration tags local to modules rather than global, to allow
 * DEPENDS UPON two versions of the same program.  For same reason, use
 * gensymed symbol names that include version number.
 * 
 * Revision 2.0  85/11/21  07:21:30  jqj
 * 4.3BSD standard release
 * 
 * Revision 1.5  85/05/23  06:19:24  jqj
 * *** empty log message ***
 * 
 * Revision 1.5  85/05/23  06:19:24  jqj
 * Public Beta-test version, released 24 May 1985
 * 
 * Revision 1.4  85/03/26  06:09:31  jqj
 * Revised public alpha-test version, released 26 March 1985
 * 
 * Revision 1.3  85/03/11  16:38:47  jqj
 * Public alpha-test version, released 11 March 1985
 * 
 * Revision 1.2  85/02/21  11:04:54  jqj
 * alpha test version
 * 
 * Revision 1.1  85/02/15  13:58:15  jqj
 * Initial revision
 * 
 */

#include <stdio.h>

#define MAXSTR		200
#define streq(s, t)	(strcmp(s, t) == 0)
#define New(t)		((t *) calloc(1, sizeof(t)))

/*
 * error message severity types
 */
enum severity {
	WARNING,
	ERROR,
	FATAL,
};

/*
 * Cons cell for lisp operations.
 */
struct cons {
	struct cons *c_cdr;
	struct cons *c_car;
};
typedef struct cons *list;

#define NIL	((list) 0)
#define car(x)	((x)->c_car)
#define cdr(x)	((x)->c_cdr)
#define caar(x)	((x)->c_car->c_car)
#define cdar(x)	((x)->c_car->c_cdr)
#define cadr(x)	((x)->c_cdr->c_car)
#define cddr(x)	((x)->c_cdr->c_cdr)

#define ONIL	((struct object *) 0)
#define ocar(x)	((x)->o_car)
#define ocdr(x)	((x)->o_cdr)

#define TNIL	((struct type *) 0)

/*
 * Object classes.
 */
enum class {
	O_UNKNOWN = 0,		/* make_symbol starts with this */
	O_TYPE,			/* a typename symbol */
	O_CONSTANT,		/* a constantname symbol */
	O_ENUMTAG,		/* an enumeration tag */
	O_SYMBOLTABLE		/* a symbol table */
};

/*
 * Type constructors.
 */
enum constr {
	C_NUMERIC = 1,
	C_BOOLEAN,
	C_STRING,
	C_ENUMERATION,
	C_ARRAY,
	C_SEQUENCE,
	C_RECORD,
	C_CHOICE,
	C_PROCEDURE,
	C_ERROR
};

/*
 * Object structure, for types, and constants.
 * These are the symbol table entries!
 */
struct object {
	struct object *o_cdr, *o_car;	/* for a binary tree symbol table */
	enum class o_class;
	char *o_name;			/* Courier name of this symbol */
	char *o_module;			/* name of module it appears in */
	int o_modnumber;		/* number of module it appears in */
	int o_modversion;		/* version of module it appears in */
	union {
		struct type *u_type;
		struct constant *u_constant;
		struct enumtag *u_enum;
		struct symtab *u_symboltable;
	} o_union;
};
#define o_type		o_union.u_type
#define o_constant	o_union.u_constant
#define o_enum		o_union.u_enum
#define o_symboltable	o_union.u_symboltable

#define class_of(x)	(((struct object *) x)->o_class)
#define name_of(x)	(((struct object *) x)->o_name)
#define value_of(x)	(((struct object *) x)->o_value->cn_value)
#define enumvalue_of(x)	(((struct object *) x)->o_enum->en_value)
#define enumname_of(x)	(((struct object *) x)->o_enum->en_name)


/*
 * Type structure.
 *
 * Formats of various lists are as follows.
 * a-b is an a of type b.
 * . indicates a cons operation.
 * ... indicates a list of elements of the preceding form.
 *
 * enumeration:
 *	((name-objectptr . value-numericstring) ...)
 *	[ the value is irrelevant, since it is stored in the enumtag]
 * record, error, procedure arguments, procedure results:
 *	(((name-string) . type-typeptr) ...)
 * choice:
 *	((((name-symbol . value-numericstring) ...) . type-typeptr) ...)
 *		[value may be nil if it is inherited from someplace]
 */
struct type {
	enum constr type_constr;	/* constructor */
	char *type_pfname;		/* name of pack function */
	char *type_ufname;		/* name of unpack function */
	char *type_name;		/* name of C type */
	int type_xsize;			/* size of external form in 16-bit
					   words, or -1 if variable */
	union {
		list u_list;		/* enumeration, record, error */
		struct {
			int u_size;
			struct type *u_basetype;
		} u_array;		/* array, sequence */
		struct {
			struct type *u_designator;
			list u_candidates;
		} u_choice;		/* choice */
		struct {
			list u_args, u_results, u_errors;
		} u_procedure;		/* procedure */
	} type_u;
};
#define typename(tp) (tp->type_name)

#define type_list type_u.u_list
#define type_array type_u.u_array
#define type_size type_array.u_size
#define type_basetype type_array.u_basetype
#define type_choice type_u.u_choice
#define type_designator type_choice.u_designator
#define type_candidates type_choice.u_candidates
#define type_procedure type_u.u_procedure
#define type_args type_procedure.u_args
#define type_results type_procedure.u_results
#define type_errors type_procedure.u_errors

/*
 * These definitions allow access from an object pointer
 * known to be a type.
 */
#define t_constr	o_type->type_constr
#define t_pfname	o_type->type_pfname
#define t_ufname	o_type->type_ufname
#define t_name		o_type->type_name
#define t_xsize		o_type->type_xsize
#define t_list		o_type->type_u.u_list
#define t_size		o_type->type_u.u_array.u_size
#define t_basetype	o_type->type_u.u_array.u_basetype
#define t_designator	o_type->type_u.u_choice.u_designator
#define t_candidates	o_type->type_u.u_choice.u_candidates
#define t_args		o_type->type_u.u_procedure.u_args
#define t_results	o_type->type_u.u_procedure.u_results
#define t_errors	o_type->type_u.u_procedure.u_errors


/*
 * constant definition structure.
 *
 *   Formats for cn_value follow:
 * numeric constants, error constants
 *	e.g.:  34
 * string constants
 *	e.g.:  "abc\"def"
 * enumeration constants
 *	e.g.:  red
 *
 *   Formats for cn_list follow:
 * record
 *	((name-string . constant) ...)
 * choice:
 *	(name-symbol . constant)
 * array, sequence
 *	(constant ...)
 * error
 *	(errorvalue-string . argsrecord-typtr)
 * note that procedure and error constants are special.
 */
struct constant {
	enum constr cn_constr;
	char *cn_name;			/* name of the C constant */
	char *cn_seqvalname;		/* only for sequence constants */
	union {
		list u_list;		/* ConstructedConstant */
		char *u_value;		/* PredefinedConstant */
					/* EnumerationConstant */
	} cn_union;
};
#define cn_list cn_union.u_list
#define cn_value cn_union.u_value


/*
 * enumeration definition structure.
 */
struct enumtag {
	char * en_name;			/* C name for this tag */
	unsigned short en_value;
};

/*
 * symbol table structure
 */
struct symtab {
	struct object *s_syms;
	list s_dependencies;		/* a list of modulews */
};
	
/*
 * Kinds of translation functions
 */
enum translation {
	EXTERNALIZE,
	INTERNALIZE,
};

/*
 * Predefined types.
 */
extern struct type
	*Boolean_type,
	*Cardinal_type, *LongCardinal_type,
	*Integer_type, *LongInteger_type,
	*String_type,
	*Unspecified_type, *LongUnspecified_type,
	*NilRecord_type,
	*StreamEnum_type;

/*
 * symbol table management routines
 */
extern struct object
	*check_def(), *make_symbol(), *make_module(), *check_dependency();
/*
 * routines for entering types in the parse tree
 */
extern struct type
	*make_type(),
	*enumeration_type(), *array_type(),
	*sequence_type(), *record_type(), *choice_type(),
	*procedure_type(), *error_type();
/*
 * routines for entering constants in the parse tree
 */
extern struct constant
	*Boolean_constant(), *Numeric_constant(),
	*String_constant(),
	*array_constant(), *choice_constant(), 
	*record_constant(), *enumeration_constant();
/*
 * list headers for all the procedures and errors seen in this module
 */
extern list Procedures, Errors;
/*
 * basic lispish functions
 */
extern list cons(), nconc();
/*
 * files we'll be using
 */
extern FILE *header, *header1, *support1, *support2, *client, *server;

/*
 * random global variables
 */
extern char *CurrentProgram;
extern struct object *CurrentModule;
extern int CurrentVersion, CurrentNumber;
extern char *input_file;
extern int recursive_flag;

/*
 * more functions
 */
extern char *refstr(), *xfn(), *copy(), *gensym(), *make_full_name();
extern char *malloc(), *calloc(), *strcpy();
