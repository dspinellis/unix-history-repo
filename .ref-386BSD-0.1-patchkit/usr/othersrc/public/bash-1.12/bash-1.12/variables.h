/* variables.h -- data structures for shell variables. */

/* Shell variables and functions are stored in hash tables. */
#include "hash.h"

/* What a shell variable looks like. */

typedef struct variable *DYNAMIC_FUNC ();

typedef struct variable {
  char *name;			/* Symbol that the user types. */
  char *value;			/* Value that is returned. */
  DYNAMIC_FUNC *dynamic_value;	/* Function called to return a `dynamic'
				   value for a variable, like $SECONDS
				   or $RANDOM. */
  DYNAMIC_FUNC *assign_func; 	/* Function called when this `special
				   variable' is assigned a value in
				   bind_variable. */
  int attributes;		/* export, readonly, array, invisible... */
  int context;			/* Which context this variable belongs to. */
  struct variable *prev_context; /* Value from previous context or NULL. */
} SHELL_VAR;

/* The various attributes that a given variable can have.
   We only reserve one byte of the INT. */
#define att_exported  0x01	/* %00000001 (export to environment) */
#define att_readonly  0x02	/* %00000010 (cannot change)	     */
#define att_invisible 0x04	/* %00000100 (cannot see)	     */
#define att_array     0x08	/* %00001000 (value is an array)     */
#define att_nounset   0x10	/* %00010000 (cannot unset)	     */
#define att_function  0x20	/* %00100000 (value is a function)   */
#define att_integer   0x40	/* %01000000 (internal rep. is int)  */

#define exported_p(var)		((((var)->attributes) & (att_exported)))
#define readonly_p(var)		((((var)->attributes) & (att_readonly)))
#define invisible_p(var)	((((var)->attributes) & (att_invisible)))
#define array_p(var)		((((var)->attributes) & (att_array)))
#define function_p(var)		((((var)->attributes) & (att_function)))
#define integer_p(var)		((((var)->attributes) & (att_integer)))

#define value_cell(var) ((var)->value)
#define function_cell(var) (COMMAND *)((var)->value)

/* Stuff for hacking variables. */
extern HASH_TABLE *shell_variables, *shell_functions;
extern SHELL_VAR *find_function (), *find_variable (), *variable_lookup ();
extern SHELL_VAR *copy_variable (), *bind_variable (), *bind_function ();
extern char *get_string_value (), *dollar_vars[];
extern char **export_env;
extern SHELL_VAR **map_over ();
extern SHELL_VAR **all_shell_variables (), **all_shell_functions ();
extern int variable_in_context ();
extern int variable_context;
