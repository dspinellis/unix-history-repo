#include "sparc.h"

#undef LIB_SPEC
#define LIB_SPEC	"%{!p:%{!pg:-lc}}%{p:-lc_p}%{pg:-lc_p}"

#undef STARTFILE_SPEC
#define STARTFILE_SPEC	"%{pg:gcrt0.o%s}%{!pg:%{p:gcrt0.o%s}%{!p:crt0.o%s}}"

/* SunOS size_t is broken; ours works */
#undef SIZE_TYPE
#define SIZE_TYPE "unsigned int"

/* We have atexit() and should use it in libgcc2 */
#define	HAVE_ATEXIT

/*
 * This one is pretty minor, but what the heck.  Simplify the output,
 * and use a minimal size.
 */
#undef ASM_OUTPUT_COMMON
#define ASM_OUTPUT_COMMON(FILE, NAME, SIZE, ROUNDED)  \
( fputs ("\t.comm ", (FILE)),		\
  assemble_name ((FILE), (NAME)),	\
  fprintf ((FILE), ",%u\n", (SIZE ? SIZE : 1)))

/*
 * Want to do the same for .lcomm, but have to use the rounded size
 * since gas 1.38 will not align local BSS offsets for us.
 */
#undef ASM_OUTPUT_LOCAL
#define ASM_OUTPUT_LOCAL(FILE, NAME, SIZE, ROUNDED)  \
( fputs ("\t.lcomm ", (FILE)),		\
  assemble_name ((FILE), (NAME)),	\
  fprintf ((FILE), ",%u\n", (ROUNDED)))

/* we are not SunOS; we will try not defining `sun' */
#undef CPP_PREDEFINES
#define CPP_PREDEFINES "-Dsparc -Dunix"
