/* This is just like the default gvarargs.h
   except for differences decribed below.  */

/* va_list is a structure instead of a char*.  */
typedef struct __va_ctl
{
  char *__stack;   /* Current pointer for fetching args.  */
  char *__beg;     /* Pointer to position of first saved register arg.  */
} va_list;

#define va_alist  __builtin_va_alist
#define va_dcl    int __builtin_va_alist;

/* The difference is to store the stack address in both components
   instead of in AP itself.  */
#define va_start(AP) 						\
 (__builtin_saveregs (),					\
 (AP).__beg = (AP).__stack = ((char *) &__builtin_va_alist))
#define va_end(pvar)

#define __va_rounded_size(TYPE)  \
  (((sizeof (TYPE) + sizeof (int) - 1) / sizeof (int)) * sizeof (int))

/* The difference is that, for an aggregate that is not word-aligned,
   we advance (pvar).__stack to the first non-reg slot.  */
#define va_arg(pvar,TYPE)					\
({ TYPE __va_temp;						\
   ((__builtin_classify_type (__va_temp) < 12			\
     || __alignof__ __va_temp >= 4)				\
    ? ((pvar).__stack += __va_rounded_size (TYPE),		\
       *((TYPE *) ((pvar).__stack - __va_rounded_size (TYPE))))	\
    : ((((pvar).__stack - (pvar).__beg < 24)			\
	? (pvar).__stack = (pvar).__beg + 24 : 0),		\
       (pvar).__stack += __va_rounded_size (TYPE),		\
       *((TYPE *) ((pvar).__stack - __va_rounded_size (TYPE)))));})
