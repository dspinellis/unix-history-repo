/* config.h manually constructed for VMS */

/* Define to empty if the keyword does not work.  */
#undef const

/* Define to `unsigned' if <sys/types.h> doesn't define.  */
#undef size_t

/* Define if you have the ANSI C header files.  */
#define STDC_HEADERS

/* Define if you have the <malloc.h> header file.  */
#undef HAVE_MALLOC_H

/* Define if you have the <string.h> header file.  */
#define HAVE_STRING_H

/* Define if you have the <sys/types.h> header file.  */
#ifndef __GNUC__
#undef HAVE_SYS_TYPES_H
#else
#define HAVE_SYS_TYPES_H
#endif

/* Define if you have <alloca.h> and it should be used (not on Ultrix).  */
#undef HAVE_ALLOCA_H

/* Extra platform-specific command line handling.  */
#define NEED_ARGV_FIXUP

/* Override default exit behavior.  */
#define exit vms_exit
