/* ---------------------------------------- */
/*           VARARGS  for MIPS/GNU CC       */
/*                                          */
/*                                          */
/*                                          */
/*                                          */
/* ---------------------------------------- */


/* These macros implement traditional (non-ANSI) varargs
   for GNU C.  */

#define va_alist  __builtin_va_alist
#define va_dcl    int __builtin_va_alist;
#ifndef _VA_LIST_
#define _VA_LIST_
#define va_list   char *
#endif

#define va_start(AP)  AP = (char *) &__builtin_va_alist
#define va_end(AP)

#ifdef lint	/* complains about constant in conditional context */
#define va_arg(list, mode) ((mode *)(list += sizeof(mode)))[-1]

#else		/* !lint */
#define va_arg(AP, mode) ((mode *)(AP = \
	(char *) (sizeof(mode) > 4 ? ((int)AP + 2*8 - 1) & -8 \
				   : ((int)AP + 2*4 - 1) & -4)))[-1]
#endif		/* lint */
