#ifndef _VA_LIST
#define _VA_LIST
struct __va_struct { int fixed[12]; int floating[8]; };

typedef struct __va_ctl
{
  struct __va_struct *__regs;
  void *__stack;
  int __nfixed, __nfloating;
} va_list;
#endif /* _VA_LIST */

#define va_alist

#define va_dcl

#define va_start(pvar) \
 (memcpy (&(pvar), (struct __va_ctl *) __builtin_saveregs (), 16))
#define va_end(pvar)

#define va_arg(pvar,type)					\
({ type __va_temp;						\
   *((__builtin_classify_type (__va_temp) < 8			\
      && sizeof __va_temp < 8)					\
     ? ((pvar).__nfixed < 12					\
	? (type *) &(pvar).__regs->fixed[(pvar).__nfixed++]	\
	: ({							\
	     int temp						\
	       = ((int) ((pvar).__stack + __alignof__ (type) - 1) \
		  & ~(__alignof__ (type) - 1));			\
	     (pvar).__stack = (void *) (temp + sizeof (type));	\
	     (type *) temp; 					\
	   }))							\
     : __builtin_classify_type (__va_temp) < 9			\
     ? ((pvar).__nfloating < 8					\
	? ((pvar).__nfloating					\
	     = (((pvar).__nfloating + 2 * (sizeof __va_temp / 4) - 1) \
		& ~(sizeof __va_temp / 4 - 1)),			\
	   (type *) &(pvar).__regs->floating[(pvar).__nfloating - (sizeof __va_temp / 4)]) \
	: ({							\
	     int temp						\
	       = ((int) ((pvar).__stack + __alignof__ (type) - 1) \
		  & ~(__alignof__ (type) - 1));			\
	     (pvar).__stack = (void *) (temp + sizeof (type));	\
	     (type *) temp; 					\
	   }))							\
     : ({							\
	  int temp						\
	    = ((int) ((pvar).__stack + __alignof__ (type) - 1)	\
	       & ~(__alignof__ (type) - 1));			\
	  (pvar).__stack = (void *) (temp + sizeof (type));	\
	  (type *) temp; 					\
	})); })
