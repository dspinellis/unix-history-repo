/* "varargs.h" -- old style variable argument list manipulation (for VAX) */
#ifndef __GNUC__

 /* Use the system's macros with the system's compiler. */
#include <varargs.h>

#else	/*__GNUC__*/

# if defined(__VAX__) || defined(__vax__) || defined(VAX) || defined(vax)
 /* These macros implement traditional (non-ANSI) varargs for GNU C on VAX */

#  if !defined(_VA_LIST) && !defined(_VA_LIST_)
#   define _VA_LIST
#   define _VA_LIST_
typedef char *va_list;
#  endif

#  define va_alist	_varargs
#  define va_dcl	int va_alist;
#  define va_start(AP)	AP = (va_list) &va_alist
#  define va_end(AP)

#  define _va_rounded_size(TYPE)	\
	(((sizeof (TYPE) + sizeof (int) - 1) / sizeof (int)) * sizeof (int))

#  define va_arg(AP,TYPE)		\
	(AP += _va_rounded_size(TYPE),	\
		*((TYPE *) (AP - _va_rounded_size(TYPE))))

#  if defined(__VMS__) || defined(__vms__) || defined(VMS) || defined(vms)
 /* VAX C compatability macros */
#   define va_count(CNT)  vaxc$va_count(&CNT)	/* rtl routine */
#   define va_start_1(AP,OFFSET)  AP = (va_list) (&va_alist + (OFFSET))
#  endif /* VMS */

# endif /* VAX */

#endif	/*__GNUC__*/
