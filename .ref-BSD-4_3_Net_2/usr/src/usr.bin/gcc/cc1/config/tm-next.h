/* tm-next.h:  Definitions for Next as target machine for GNU C compiler.  */

#include "tm-m68k.h"

/* Enable recent gcc to compile under the old gcc in Next release 1.0.  */
#define __inline inline

/* See tm-m68k.h.  7 means 68020/030 with 68881/882.  */

#define TARGET_DEFAULT 7

/* These compiler options take an argument.  */

#define WORD_SWITCH_TAKES_ARG(STR)	\
  (!strcmp (STR, "Ttext") || !strcmp (STR, "Tdata"))

/* Names to predefine in the preprocessor for this target machine.  */

#define CPP_PREDEFINES "-Dmc68000 -DNeXT -Dunix -D__MACH__"

/* Machine dependent ccp options.  */

#define CPP_SPEC "%{bsd:-D__STRICT_BSD__}"

/* Machine dependent ld options.  */

#define LINK_SPEC "%{Z} %{M} %{Mach} %{segcreate*} %{seglinkedit}"

/* Machine dependent libraries.  */

#define LIB_SPEC "%{!p:%{!pg:-lsys_s}} %{pg:-lsys_p}"
 
/* We specify crt0.o as -lcrt0.o so that ld will search the library path. */
#define STARTFILE_SPEC  \
  "%{pg:-lgcrt0.o}%{!pg: \
     %{p:%e-p profiling is no longer supported.  Use -pg instead.} \
     %{!p:-lcrt0.o}}"

/* Every structure or union's size must be a multiple of 2 bytes.  */

#define STRUCTURE_SIZE_BOUNDARY 16

/* We want C++ style comments to be supported for Objective-C */

#define CPLUSPLUS

/* Why not? */

#define DOLLARS_IN_IDENTIFIERS 1

#if 0 /* These pertain to code changes that are not present in 1.36.  */

/* Allow Mach -MD and -MMD make depend switches. */

#define MACH_MAKE_DEPEND

/* These options take an argument.  Note that we don't support -Ttext or -Tdata.  */

#define WORD_SWITCH_TAKES_ARG(STR) (!strcmp (STR, "MD") || !strcmp (STR,  
"MMD"))

#endif /* 0 */

/* Allow #sscs (but don't do anything). */

#define SCCS_DIRECTIVE

/* We use Dbx symbol format.  */

#define DBX_DEBUGGING_INFO

/* This is how to output an assembler line defining a `double' constant.  */

#undef ASM_OUTPUT_DOUBLE
#define ASM_OUTPUT_DOUBLE(FILE,VALUE)					\
  (isinf ((VALUE))							\
   ? fprintf (FILE, "\t.double 0r%s99e999\n", ((VALUE) > 0 ? "" : "-")) \
   : fprintf (FILE, "\t.double 0r%.20e\n", (VALUE)))

/* This is how to output an assembler line defining a `float' constant.  */

#undef ASM_OUTPUT_FLOAT
#define ASM_OUTPUT_FLOAT(FILE,VALUE)					\
  (isinf ((VALUE))							\
   ? fprintf (FILE, "\t.single 0r%s99e999\n", ((VALUE) > 0 ? "" : "-")) \
   : fprintf (FILE, "\t.single 0r%.20e\n", (VALUE)))

#undef ASM_OUTPUT_FLOAT_OPERAND
#define ASM_OUTPUT_FLOAT_OPERAND(FILE,VALUE)				\
  (isinf ((VALUE))							\
   ? fprintf (FILE, "#0r%s99e999", ((VALUE) > 0 ? "" : "-")) \
   : fprintf (FILE, "#0r%.9g", (VALUE)))

#undef ASM_OUTPUT_DOUBLE_OPERAND
#define ASM_OUTPUT_DOUBLE_OPERAND(FILE,VALUE)				\
  (isinf ((VALUE))							\
   ? fprintf (FILE, "#0r%s99e999", ((VALUE) > 0 ? "" : "-")) \
   : fprintf (FILE, "#0r%.20g", (VALUE)))
