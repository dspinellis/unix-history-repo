#ifndef _STDARG_H
#define _STDARG_H

/* The macro _VA_LIST_ is the same thing used by this file in Ultrix.  */
#ifndef _VA_LIST_
#define _VA_LIST_
typedef char *va_list;
#endif

/* Amount of space required in an argument list for an arg of type TYPE.
   TYPE may alternatively be an expression whose type is used.  */

#define __va_rounded_size(TYPE)  \
  (((sizeof (TYPE) + sizeof (int) - 1) / sizeof (int)) * sizeof (int))

#ifndef __sparc__
#define va_start(AP, LASTARG) 						\
 (AP = ((char *) __builtin_next_arg ()))
#else
#define va_start(AP, LASTARG) 						\
 (__builtin_saveregs (),						\
  AP = ((char *) __builtin_next_arg ()))
#endif

void va_end (va_list);		/* Defined in gnulib */
#define va_end(AP)

#ifdef __mips__
#define va_arg(AP, mode) ((mode *)(AP = \
	(char *) (sizeof(mode) > 4 ? ((int)AP + 2*8 - 1) & -8 \
				   : ((int)AP + 2*4 - 1) & -4)))[-1]
#else /* not __mips__ */
#define va_arg(AP, TYPE)						\
 (*((TYPE *) (AP += __va_rounded_size (TYPE),				\
	      AP - (sizeof (TYPE) < 4 ? sizeof (TYPE)			\
		    : __va_rounded_size (TYPE)))))
#endif /* not __mips__ */

#endif /* _STDARG_H */
