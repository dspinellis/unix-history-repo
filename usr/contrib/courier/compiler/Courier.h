/*	Courier.h	4.1	83/07/03	*/

#include <stdio.h>

#define NIL		0
#define MAXSTR		200
#define streq(s, t)	(strcmp(s, t) == 0)
#define New(t)		((t *) calloc(1, sizeof(t)))

/*
 * Cons cell for lisp operations.
 */
struct cons {
	struct cons *c_cdr;
	struct cons *c_car;
};
typedef struct cons *list;
#define car(x)	((x)->c_car)
#define cdr(x)	((x)->c_cdr)

/*
 * Object classes.
 */
enum class {
	O_TYPE = 1,
	O_CONSTANT,
	O_SYMBOL
};

/*
 * Type constructors.
 */
enum constr {
	C_PREDEF = 1,
	C_ENUMERATION,
	C_ARRAY,
	C_SEQUENCE,
	C_RECORD,
	C_CHOICE,
	C_PROCEDURE,
	C_ERROR
};

/*
 * Object structure, for types, numbers, and strings.
 */
struct object {
	enum class o_class;
	union {
		struct type *u_type;
		char *u_name;
		int u_value;
	} o_union;
};
#define o_type		o_union.u_type
#define o_name		o_union.u_name
#define o_value		o_union.u_value

#define class_of(x)	(((struct object *) x)->o_class)
#define name_of(x)	(((struct object *) x)->o_name)
#define value_of(x)	(((struct object *) x)->o_value)

/*
 * Type structure.
 *
 * Formats of various lists are as follows.
 * . indicates a cons operation.
 * ... indicates a list of elements of the preceding form.
 *
 * enumeration:
 *	((name . value) ...)
 * record, error, procedure arguments, procedure results:
 *	(((name ...) . type) ...)
 * choice:
 *	((((name . value) ...) . type) ...)
 */
struct type {
	enum constr type_constr;	/* constructor */
	char *type_pfname;		/* name of pack function */
	char *type_ufname;		/* name of unpack function */
	union {
		list u_list;		/* enumeration, record, error */
		struct {
			struct object *u_size, *u_basetype;
		} u_array;		/* array, sequence */
		struct {
			struct object *u_designator;
			list u_candidates;
		} u_choice;		/* choice */
		struct {
			list u_args, u_results, u_errors;
		} u_procedure;		/* procedure */
	} type_u;
};
/*
 * These definitions allow access from an object pointer
 * known to be a type.
 */
#define t_constr	o_type->type_constr
#define t_pfname	o_type->type_pfname
#define t_ufname	o_type->type_ufname
#define t_list		o_type->type_u.u_list
#define t_size		o_type->type_u.u_array.u_size
#define t_basetype	o_type->type_u.u_array.u_basetype
#define t_designator	o_type->type_u.u_choice.u_designator
#define t_candidates	o_type->type_u.u_choice.u_candidates
#define t_args		o_type->type_u.u_procedure.u_args
#define t_results	o_type->type_u.u_procedure.u_results
#define t_errors	o_type->type_u.u_procedure.u_errors

/*
 * This macro is to cope with the fact that C passes arrays
 * by reference but all other constructed types by value.
 */
#define refstr(t)	(basetype(t)->t_constr == C_ARRAY ? "" : "&")

/*
 * Predefined types.
 */
extern struct object
	*Boolean_type,
	*Cardinal_type, *LongCardinal_type,
	*Integer_type, *LongInteger_type,
	*String_type,
	*Unspecified_type, *LongUnspecified_type;

extern struct object
	*Undefined_constant;

extern struct object *make(), *lookup(), *basetype();
extern struct object *construct_type1(), *construct_type2();
extern struct object *construct_choice(), *construct_procedure();
extern list Values, Types;
extern list cons(), nconc();
extern FILE *hf, *cf1, *cf2, *uf, *sf;
extern int explicit, print_level;
extern char *program_name;
extern char *copy(), *gensym();
extern char *obj_rep(), *pack_function(), *unpack_function();
