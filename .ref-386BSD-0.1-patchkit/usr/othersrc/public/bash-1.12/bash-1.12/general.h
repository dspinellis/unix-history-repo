/* general.h -- defines that everybody likes to use. */

#if !defined (_GENERAL_)
#define _GENERAL_

#if !defined (NULL)
#define NULL 0x0
#endif

#ifndef savestring
#define savestring(x) (char *)strcpy (xmalloc (1 + strlen (x)), (x))
#endif

#ifndef whitespace
#define whitespace(c) (((c) == ' ') || ((c) == '\t'))
#endif

#ifndef digit
#define digit(c)  ((c) >= '0' && (c) <= '9')
#endif

#ifndef isletter
#define isletter(c) (((c) >= 'A' && (c) <= 'Z') || ((c) >= 'a' && (c) <= 'z'))
#endif

#ifndef digit_value
#define digit_value(c) ((c) - '0')
#endif

#if !defined (__STDC__)
char *index (), *rindex ();
#endif

#ifndef member
#define member(c, s) (int)((c) ? index ((s), (c)) : 0)
#endif

/* All structs which contain a `next' field should have that field
   as the first field in the struct.  This means that functions
   can be written to handle the general case for linked lists. */
typedef struct g_list {
  struct g_list *next;
} GENERIC_LIST;
  
/* Here is a generic structure for associating character strings
   with integers.  It is used in the parser for shell tokenization. */
typedef struct {
  char *word;
  int token;
} STRING_INT_ALIST;

/* String comparisons that possibly save a function call each. */
#define STREQ(a, b) ((a)[0] == (b)[0] && strcmp(a, b) == 0)
#define STREQN(a, b, n) ((a)[0] == (b)[0] && strncmp(a, b, n) == 0)

/* Function pointers can be declared as (Function *)foo. */
#if !defined (__FUNCTION_DEF)
#  define __FUNCTION_DEF
typedef int Function ();
typedef void VFunction ();
#endif /* _FUNCTION_DEF */

#if defined (VOID_SIGHANDLER)
#define sighandler void
#else
#define sighandler int
#endif

typedef sighandler SigHandler ();

#define NOW	((time_t) time ((time_t *) 0))

/* Some defines for calling file status functions. */
#define FS_EXISTS	  0x1
#define FS_EXECABLE	  0x2
#define FS_EXEC_PREFERRED 0x4
#define FS_EXEC_ONLY	  0x8

extern char *xmalloc (), *malloc (), *xrealloc (), *realloc ();
extern char *itos ();

#endif	/* _GENERAL_ */
