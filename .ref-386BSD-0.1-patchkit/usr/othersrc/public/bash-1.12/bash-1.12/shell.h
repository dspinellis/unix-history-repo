/* shell.h -- The data structures used by the shell */

#include "config.h"
#include "general.h"
#include "error.h"
#include "variables.h"
#include "quit.h"
#include "maxpath.h"
#include "unwind_prot.h"
#include "command.h"

extern int EOF_Reached;

#define NO_PIPE -1
#define REDIRECT_BOTH -2
#define IS_DESCRIPTOR -1

#define NO_VARIABLE -1

/* A bunch of stuff for flow of control using setjmp () and longjmp (). */
#include <setjmp.h>
extern jmp_buf top_level, catch;

#define NOT_JUMPED 0		/* Not returning from a longjmp. */
#define FORCE_EOF 1		/* We want to stop parsing. */
#define DISCARD 2		/* Discard current command. */
#define EXITPROG 3		/* Unconditionally exit the program now. */

/* Values that can be returned by execute_command (). */
#define EXECUTION_FAILURE 1
#define EXECUTION_SUCCESS 0

/* Special exit status used when the shell is asked to execute a
   binary file as a shell script. */
#define EX_BINARY_FILE 126

/* The list of characters that are quoted in double-quotes with a
   backslash.  Other characters following a backslash cause nothing
   special to happen. */
#define slashify_in_quotes "\\`$\""
#define slashify_in_here_document "\\`$"

/* Constants which specify how to handle backslashes and quoting in
   expand_word_internal ().  Q_DOUBLE_QUOTES means to use the function
   slashify_in_quotes () to decide whether the backslash should be
   retained.  Q_HERE_DOCUMENT means slashify_in_here_document () to
   decide whether to retain the backslash.  Q_KEEP_BACKSLASH means
   to unconditionally retain the backslash. */
#define Q_DOUBLE_QUOTES  0x1
#define Q_HERE_DOCUMENT  0x2
#define Q_KEEP_BACKSLASH 0x4

extern char **shell_environment;
extern WORD_LIST *rest_of_args;

/* Generalized global variables. */
extern int executing, login_shell;

/* Structure to pass around that holds a bitmap of file descriptors
   to close, and the size of that structure.  Used in execute_cmd.c. */
struct fd_bitmap {
  long size;
  char *bitmap;
};

#define FD_BITMAP_SIZE 32
