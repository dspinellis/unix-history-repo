/* error.h -- External declarations of functions appearing in error.c. */

/* Report an error having to do with FILENAME. */
extern void file_error ();

/* Report a programmer's error, and abort.  Pass REASON, and ARG1 ... ARG5. */
extern void programming_error ();

/* General error reporting.  Pass FORMAT and ARG1 ... ARG5. */
extern void report_error ();

/* Report an unrecoverable error and exit.  Pass FORMAT and ARG1 ... ARG5. */
extern void fatal_error ();
