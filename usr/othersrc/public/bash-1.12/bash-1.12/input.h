/* input.h -- Structures and unions used for reading input. */

#if !defined (_INPUT_H)
#define _INPUT_H

/* Function pointers can be declared as (Function *)foo. */
#if !defined (__FUNCTION_DEF)
#  define __FUNCTION_DEF
typedef int Function ();
typedef void VFunction ();
#endif /* _FUNCTION_DEF */

/* Some stream `types'. */
#define st_stream 1
#define st_string 2

typedef union {
  FILE *file;
  char *string;
} INPUT_STREAM;

typedef struct {
  int type;
  char *name;
  INPUT_STREAM location;
  Function *getter;
  Function *ungetter;
} BASH_INPUT;

extern BASH_INPUT bash_input;

#endif /* _INPUT_H */

