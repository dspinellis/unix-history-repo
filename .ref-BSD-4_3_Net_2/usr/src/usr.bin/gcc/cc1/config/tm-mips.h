/* Definitions of target machine for GNU compiler.  MIPS version.
   Contributed by   A. Lichnewsky, lich@inria.inria.fr
   Changes by	    Michael Meissner, meissner@osf.org
   Copyright (C) 1989, 1990 Free Software Foundation, Inc.

This file is part of GNU CC.

GNU CC is free software; you can redistribute it and/or modify
it under the terms of the GNU General Public License as published by
the Free Software Foundation; either version 1, or (at your option)
any later version.

GNU CC is distributed in the hope that it will be useful,
but WITHOUT ANY WARRANTY; without even the implied warranty of
MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
GNU General Public License for more details.

You should have received a copy of the GNU General Public License
along with GNU CC; see the file COPYING.  If not, write to
the Free Software Foundation, 675 Mass Ave, Cambridge, MA 02139, USA.  */

/* ??? This file needs to be reformatted so that it looks like the
   rest of GCC. ???  */

/*----------------------------------------------------------------------

SWITCHES:

    -O    optimization. Implies -mgpOPT
    -O1	  Same as -O, mips compatibility
    -O2   Implies -O -fomit-frame-pointer -fstrength-reduce
    -O3   Implies -O2 + -finline-functions

    -mG0 -mG1 -mG2
          Construct a size to be passed to GCC for Data / Sdata selection.

          Value is ( (i=G0 + 2 G1 + 4 G2) , (i < 6) ? ( 1<<i) :(1 <<(i+3)))
          Same value should be passed to as + ld using -G.  Use -G instead
	  since it is now supported.

	  Default = -mG1 -mG0 (Value = 8).

    -G32  Implies -G 32 -mG2 -mnG1 -mG0.


    -bestGnum
          Pass -bestGnum flag to ld. This helps setting best value for
          the -G parameter.

    -SSYSV  for RISC-OS: use the System V environment
    -SBSD43 for RISC-OS: use the BSD 4.3  environment
----------------------------------------------------------------------*/



/* Suppression of libg.a when debugging */
#define NO_LIBG


/* Switch  Recognition by gcc.c   */

#ifdef SWITCH_TAKES_ARG
#undef SWITCH_TAKES_ARG
#endif

#define SWITCH_TAKES_ARG(CHAR)      \
  ((CHAR) == 'D' || (CHAR) == 'U' || (CHAR) == 'o' \
   || (CHAR) == 'e' || (CHAR) == 'T' || (CHAR) == 'u' \
   || (CHAR) == 'I' || (CHAR) == 'Y' || (CHAR) == 'm' \
   || (CHAR) == 'L' || (CHAR) == 'i' || (CHAR) == 'A' \
   || (CHAR) == 'G')

/* Process -mGxx switches  */

extern void overide_options ();

#define OVERRIDE_OPTIONS overide_options ()


/* Names to predefine in the preprocessor for this target machine.  */

#ifndef CPP_PREDEFINES
#define CPP_PREDEFINES "-Dmips -Dunix -Dhost_mips -DMIPSEB -DR3000 -DLANGUAGE_C"
#endif

/* Extra switches sometimes passed to the assembler.  */

#ifndef ASM_SPEC
#ifndef OSF_OS			/* normal MIPS system */
#ifndef DECSTATION		/* big endian MIPS (MIPS, SGI) */
#ifndef SGI_TARGET		/* not Silicon Graphics (ie, MIPSco) */

#define ASM_SPEC	"%{!mrnames:-nocpp}				\
			 %{!mgas:					\
				%{pipe: %e-pipe is not supported.}	\
				%{EB} %{!EB:-EB}			\
				%{EL: %e-EL not supported}		\
				%{O:-O2} %{O1:-O2} %{O2:-O2} %{O3:-O3}	\
				%{g} %{g1} %{g2} %{g3} %{g0}}		\
			 %{G*}						\
			 %{!G:%{!G32:	%{mpic:-G 0} %{pic:-G 0}	\
					%{mgas:-G 0}			\
					%{!pic:%{!mpic:%{!mgas:-G 8}}}}	\
				%{G32: -G 32}}				\
			 %{v} %{K}"

#else				/* Silicon Graphics */
#define ASM_SPEC	"%{!mrnames:-nocpp}				\
			 %{!mgas:					\
				%{pipe: %e-pipe is not supported.}	\
				%{O:-O2} %{O1:-O2} %{O2:-O2} %{O3:-O3}	\
				%{g} %{g1} %{g2} %{g3} %{g0}}		\
			 %{G*}						\
			 %{!G:%{!G32:	%{mpic:-G 0} %{pic:-G 0}	\
					%{mgas:-G 0}			\
					%{!pic:%{!mpic:%{!mgas:-G 8}}}}	\
				%{G32: -G 32}}				\
			 %{v} %{K}"

#endif				/* Silicon Graphics */
#else				/* Ultrix Decstation (little endian) */
#define ASM_SPEC	"%{!mrnames:-nocpp}				\
			 %{!mgas:					\
				%{pipe:%e:-pipe not supported}		\
				%{EL} %{!EL:-EL}			\
				%{EB: %e-EB not supported}		\
				%{O:-O2} %{O1:-O2} %{O2:-O2} %{O3:-O3}	\
				%{g} %{g1} %{g2} %{g3} %{g0}}		\
			 %{G*}						\
			 %{!G:%{!G32:	%{mpic:-G 0} %{pic:-G 0}	\
					%{mgas:-G 0}			\
					%{!pic:%{!mpic:%{!mgas:-G 8}}}}	\
				%{G32: -G 32}}				\
			 %{v} %{K}"
#endif				/* DECstation running Ultrix */
#else				/* OSF/1 of some sort */
#ifndef DECSTATION
				/* Big endian MIPS running OSF/1 */
#define ASM_SPEC	"%{mmips-as:					\
				%{pipe:%e:-pipe not supported}		\
				%{EB} %{!EB:-EB}			\
				%{EL: %e-EL not supported}		\
				%{!mrnames:-nocpp}			\
				%{O:-O2} %{O1:-O2} %{O2:-O2} %{O3:-O3}	\
				%{g} %{g1} %{g2} %{g3} %{g0}		\
				%{v} %{K}}				\
			 %{G*}						\
			 %{!G:%{!G32:	%{mpic:-G 0} %{pic:-G 0}	\
					%{!mmips-as:-G 0}		\
					%{!pic:%{!mpic:%{mmips-as:-G 8}}}} \
				%{G32: -G 32}}"
#else
				/* Little endian OSF/1 Decstation */
#define ASM_SPEC	"%{mmips-as:					\
				%{pipe:%e:-pipe not supported}		\
				%{EL} %{!EL:-EL}			\
				%{EB: %e-EB not supported}		\
				%{!mrnames:-nocpp}			\
				%{O:-O2} %{O1:-O2} %{O2:-O2} %{O3:-O3}	\
				%{g} %{g1} %{g2} %{g3} %{g0}		\
				%{v} %{K}}				\
			 %{G*}						\
			 %{!G:%{!G32:	%{mpic:-G 0} %{pic:-G 0}	\
					%{!mmips-as:-G 0}		\
					%{!pic:%{!mpic:%{mmips-as:-G 8}}}} \
				%{G32: -G 32}}"

#endif				/* little endian OSF/1 DECstation */
#endif				/* OSF/1 */
#endif				/* ASM_SPEC */

/* Redefinition of libraries used.  Mips doesn't support normal
   UNIX style profiling via calling _mcount.  It does offer
   profiling that samples the PC, so do what we can... */

#ifndef LIB_SPEC
#define LIB_SPEC "%{pg:%e-pg is not supported on the MIPS}%{p:-lprof1} -lc"
#endif

/* Extra switches sometimes passed to the loader.  */


#ifndef LINK_SPEC
#ifdef MIPS_SYSV		/* RISC-OS SYSTEM V */

#ifndef STARTFILE_SPEC
#define STARTFILE_SPEC						\
  "%{pg:gcrt0.o%s}%{!pg:%{p:mcrt0.o%s}%{!p:crt1.o%s crtn.o%s}}"
#endif

#define LINK_SPEC  "%{G*}						\
		    %{!G:%{!G32:	%{mpic:-G 0} %{pic:-G 0}	\
					%{mgas:-G 0}			\
					%{!pic:%{!mpic:%{!mgas:-G 8}}}}	\
			%{G32:-G 32}}					\
		    %{!G:%{!G32:					\
			%{mG0:%eYou should include ld/as option -G}	\
			%{mG1:%eYou should include ld/as option -G}	\
			%{mG2:%eYou should include ld/as option -G}}}	\
		    %{bestGnum}						\
		    %{!ZBSD43:-systype /sysv/}%{ZBSD43:-systype /bsd43/} \
		    %{EB} %{!EB:-EB} %{EL:%e-EL not supported}"

#else				/* RISC-OS SYSTEM V */
#ifdef MIPS_BSD43		/* RISC-OS BSD */

#ifndef STARTFILE_SPEC
#define STARTFILE_SPEC							\
  "%{pg:gcrt0.o%s}%{!pg:%{p:mcrt0.o%s}%{!p:crt1.o%s crtn.o%s}}"
#endif

#define LINK_SPEC  "%{G*}						\
		    %{!G:%{!G32:	%{mpic:-G 0} %{pic:-G 0}	\
					%{mgas:-G 0}			\
					%{!pic:%{!mpic:%{!mgas:-G 8}}}}	\
			%{G32:-G 32}}					\
		    %{!G:%{!G32:					\
			%{mG0:%eYou should include ld/as option -G}	\
			%{mG1:%eYou should include ld/as option -G}	\
			%{mG2:%eYou should include ld/as option -G}}}	\
		    %{bestGnum}						\
		    %{!ZSYSV:-systype /bsd43/}%{ZSYSV:-systype /sysv/}	\
		    %{EB} %{!EB:-EB} %{EL:%e-EL not supported}"

#else

#ifndef DECSTATION		/* Big endian BSD or OSF/1 system */
#ifndef OSF_OS			/* Big endian BSD system */
#ifndef SGI_TARGET		/* Big endian non Silicon Graphics system */

#define LINK_SPEC  "%{G*}						\
		    %{!G:%{!G32:	%{mpic:-G 0} %{pic:-G 0}	\
					%{mgas:-G 0}			\
					%{!pic:%{!mpic:%{!mgas:-G 8}}}}	\
			%{G32:-G 32}}					\
		    %{!G:%{!G32:					\
			%{mG0:%eYou should include ld/as option -G}	\
			%{mG1:%eYou should include ld/as option -G}	\
			%{mG2:%eYou should include ld/as option -G}}}	\
		    %{!mgas: %{EB} %{!EB:-EB} %{EL:%e-EL not supported}	\
			     %{bestGnum}}"

#else				/* Silicon graphics system */
#define LINK_SPEC  "%{G*}						\
		    %{!G:%{!G32:	%{mpic:-G 0} %{pic:-G 0}	\
					%{mgas:-G 0}			\
					%{!pic:%{!mpic:%{!mgas:-G 8}}}}	\
			%{G32:-G 32}}					\
		    %{!G:%{!G32:					\
			%{mG0:%eYou should include ld/as option -G}	\
			%{mG1:%eYou should include ld/as option -G}	\
			%{mG2:%eYou should include ld/as option -G}}}	\
		    %{!mgas: %{bestGnum}}"

#endif				/* Silicon Graphics system */
#else				/* Big endian OSF/1 system */
#define LINK_SPEC  "%{G*}						\
		    %{!G:%{!G32:	%{mpic:-G 0} %{pic:-G 0}	\
					%{!mmips-as:-G 0}		\
					%{!pic:%{!mpic:%{mmips-as:-G 8}}}} \
			%{G32:-G 32}}					\
		    %{!G:%{!G32:					\
			%{mG0:%eYou should include ld/as option -G}	\
			%{mG1:%eYou should include ld/as option -G}	\
			%{mG2:%eYou should include ld/as option -G}}}	\
		    %{mmips-as: %{EB} %{!EB:-EB} %{EL:%e-EL not supported} \
			%{bestGnum}}					\
		    %{nostdlib}"
#endif				/* Big endian BSD or OSF/1 system */

#else				/* Little endian Ultrix or OSF/1 */
#ifndef OSF_OS			/* Little endian Ultrix system */
#define LINK_SPEC  "%{G*}						\
		    %{!G:%{!G32:	%{mpic:-G 0} %{pic:-G 0}	\
					%{mgas:-G 0}			\
					%{!pic:%{!mpic:%{!mgas:-G 8}}}}	\
			%{G32:-G 32}}					\
		    %{!G:%{!G32:					\
			%{mG0:%eYou should include ld/as option -G}	\
			%{mG1:%eYou should include ld/as option -G}	\
			%{mG2:%eYou should include ld/as option -G}}}	\
		    %{!mgas: %{EL} %{!EL:-EL} %{EB:%e-EB not supported}	\
			     %{bestGnum}}"

#else				/* Little endian OSF/1 system */
#define LINK_SPEC  "%{G*}						\
		    %{!G:%{!G32:	%{mpic:-G 0} %{pic:-G 0}	\
					%{!mmips-as:-G 0}		\
					%{!pic:%{!mpic:%{mmips-as:-G 8}}}} \
			%{G32:-G 32}}					\
		    %{!G:%{!G32:					\
			%{mG0:%eYou should include ld/as option -G}	\
			%{mG1:%eYou should include ld/as option -G}	\
			%{mG2:%eYou should include ld/as option -G}}}	\
		    %{mmips-as: %{EL} %{!EL:-EL} %{EB:%e-EB not supported} \
			%{bestGnum}}					\
		    %{nostdlib}"

#endif				/* Little endian OSF/1 system */
#endif				/* Little endian BSD or OSF/1 system */
#endif				/* RISC-OS BSD */
#endif				/* RISC-OS SYSTEM V */
#endif				/* LINK_SPEC defined */

/* CC1 SPECS */

#define CC1_SPEC   "%{O: %{!mngpOPT:-mgpOPT}}				\
                    %{O1:-O %{!mngpOPT:-mgpOPT}}			\
		    %{O2:-O %{!fnostrength-reduce:-fstrength-reduce}	\
			    %{!fnoomit-frame-pointer:-fomit-frame-pointer} \
			    %{!mngpOPT:-mgpOPT}}			\
		    %{O3:-O %{!fnostrength-reduce:-fstrength-reduce}	\
			    %{!fnoomit-frame-pointer:-fomit-frame-pointer} \
			    %{!fnoinline-functions:-finline-functions}	\
			    %{!mngpOPT:-mgpOPT}}			\
                    %{O4:%eGCC does not support -O4}			\
		    %{!g: %{g1:-g} %{g2:-g} %{g3:-g}}			\
		    %{G32: -mG2 -mnG1 }"

/* CPP SPECS */

#ifndef DECSTATION

#ifdef SGI_TARGET		/* Silicon Graphics */
#define CPP_SPEC " %{!ansi:-D__EXTENSIONS__}				\
		   -D_MIPSEB -D_SYSTYPE_SYSV -D_LANGUAGE_C		\
		   %{O1:-D__OPTIMIZE__}					\
		   %{O2:-D__OPTIMIZE__}					\
		   %{O3:-D__OPTIMIZE__}"

#else
#if defined(MIPS_SYSV) || defined(MIPS_BSD43)
				/* MIPS RISC-OS environments */

#ifdef MIPS_SYSV
#define CPP_SPEC " %{!ansi:%{!ZBSD43:-DSYSTYPE_SYSV}%{ZBSD43:-DSYSTYPE_BSD43}}\
		   %{!ZBSD43:-D__SYSTYPE_SYSV__}%{ZBSD43:-D__SYSTYPE_BSD43__} \
		   %{!nostdinc:%{!ZBSD43:-I/sysv/usr/include}		\
			       %{ZBSD43:-I/bsd43/usr/include}}		\
		   %{O1:-D__OPTIMIZE__}					\
		   %{O2:-D__OPTIMIZE__}					\
		   %{O3:-D__OPTIMIZE__}"
#else /* not MIPS_SYSV */
#define CPP_SPEC " %{!ansi:%{!ZSYSV:-DSYSTYPE_BSD43}%{ZSYSV:-DSYSTYPE_SYSV}}\
		   %{!ZSYSV:-D__SYSTYPE_BSD43__}%{ZSYSV:-D__SYSTYPE_SYSV__}\
		   %{!nostdinc:%{!ZSYSV:-I/bsd43/usr/include}		\
			       %{ZSYSV:-I/sysv/usr/include}}		\
		   %{O1:-D__OPTIMIZE__}					\
		   %{O2:-D__OPTIMIZE__}					\
		   %{O3:-D__OPTIMIZE__}"

#endif /* not MIPS_SYSV */

#else /* not MIPS_SYSV and not MIPS_BSD43 */
				/* default MIPS Bsd environment */
#define CPP_SPEC "%{!ansi:-DSYSTYPE_BSD} -D__SYSTYPE_BSD__		\
		   %{O1:-D__OPTIMIZE__}					\
		   %{O2:-D__OPTIMIZE__}					\
		   %{O3:-D__OPTIMIZE__}"

#endif /* not MIPS_SYSV and not MIPS_BSD43 */
#endif /* not Silicon Graphics */

#else	/* DECSTATION */
#define CPP_SPEC  "%{O1:-D__OPTIMIZE__}					\
		   %{O2:-D__OPTIMIZE__}					\
		   %{O3:-D__OPTIMIZE__}"

#endif /* not DECSTATION */

/* Print subsidiary information on the compiler version in use.  */

#ifndef __DATE__
#define __DATE__ "[unknown date]"
#endif

#define MIPS_VERSION "AL-MIPS 1.1"

#ifdef DECSTATION
#ifdef OSF_OS
#define MACHINE_TYPE "OSF/1 Dec Mips"
#else
#define MACHINE_TYPE "Ultrix Dec Mips"
#endif

#else
#ifdef SGI_TARGET
#define MACHINE_TYPE "Sgi Mips"

#else
#if defined(MIPS_SYSV) || defined(MIPS_BSD43)
				/* MIPS RISC-OS environments */
#ifdef MIPS_SYSV
#define MACHINE_TYPE "RISC-OS System V Mips"

#else /* not MIPS_SYSV */
#define MACHINE_TYPE "RISC-OS BSD Mips"

#endif /* not MIPS_SYSV */
#else /* not MIPS_SYSV and not MIPS_BSD43 */
				/* default MIPS Bsd environment */
#define MACHINE_TYPE "BSD Mips"
#endif /* not SGI iris */
#endif /* not MIPS_SYSV and not MIPS_BSD43 */
#endif /* not DECSTATION */

#define TARGET_VERSION							\
{									\
  fprintf (stderr, " %s %s %s", MIPS_VERSION, MACHINE_TYPE, __DATE__);	\
}


#define SDB_DEBUGGING_INFO	/* generate debug info inside of comments */
#define MIPS_DEBUGGING_INFO	/* MIPS specific debugging info */

/* On Sun 4, this limit is 2048.  We use 1500 to be safe,
   since the length can run past this up to a continuation point.  */
#define DBX_CONTIN_LENGTH 1500


/* How to renumber registers for dbx and gdb.
   MIPS needs no change in the numeration.  */

#define DBX_REGISTER_NUMBER(REGNO) (REGNO)


/* Overides for the COFF debug format.  */
#define PUT_SDB_SCL(a)					\
do {							\
  extern FILE *asm_out_text_file;			\
  fprintf (asm_out_text_file, "\t.scl\t%d;", (a));	\
} while (0)

#define PUT_SDB_INT_VAL(a)				\
do {							\
  extern FILE *asm_out_text_file;			\
  fprintf (asm_out_text_file, "\t.val\t%d;", (a));	\
} while (0)

#define PUT_SDB_VAL(a)					\
do {							\
  extern FILE *asm_out_text_file;			\
  fputs ("\t.val\t", asm_out_text_file);		\
  output_addr_const (asm_out_text_file, (a));		\
  fputc (';', asm_out_text_file);			\
} while (0)

#define PUT_SDB_DEF(a)					\
do {							\
  extern FILE *asm_out_text_file;			\
  fprintf (asm_out_text_file, "\t#.def\t");		\
  ASM_OUTPUT_LABELREF (asm_out_text_file, a); 		\
  fputc (';', asm_out_text_file);			\
} while (0)

#define PUT_SDB_PLAIN_DEF(a)				\
do {							\
  extern FILE *asm_out_text_file;			\
  fprintf (asm_out_text_file, "\t#.def\t.%s;", (a));	\
} while (0)

#define PUT_SDB_ENDEF					\
do {							\
  extern FILE *asm_out_text_file;			\
  fprintf (asm_out_text_file, "\t.endef\n");		\
} while (0)

#define PUT_SDB_TYPE(a)					\
do {							\
  extern FILE *asm_out_text_file;			\
  fprintf (asm_out_text_file, "\t.type\t0x%x;", (a));	\
} while (0)

#define PUT_SDB_SIZE(a)					\
do {							\
  extern FILE *asm_out_text_file;			\
  fprintf (asm_out_text_file, "\t.size\t%d;", (a));	\
} while (0)

#define PUT_SDB_DIM(a)					\
do {							\
  extern FILE *asm_out_text_file;			\
  fprintf (asm_out_text_file, "\t.dim\t%d;", (a));	\
} while (0)

#ifndef PUT_SDB_START_DIM
#define PUT_SDB_START_DIM				\
do {							\
  extern FILE *asm_out_text_file;			\
  fprintf (asm_out_text_file, "\t.dim\t");		\
} while (0)
#endif

#ifndef PUT_SDB_NEXT_DIM
#define PUT_SDB_NEXT_DIM(a)				\
do {							\
  extern FILE *asm_out_text_file;			\
  fprintf (asm_out_text_file, "%d,", a);		\
} while (0)
#endif

#ifndef PUT_SDB_LAST_DIM
#define PUT_SDB_LAST_DIM(a)				\
do {							\
  extern FILE *asm_out_text_file;			\
  fprintf (asm_out_text_file, "%d;", a);		\
} while (0)
#endif

#define PUT_SDB_TAG(a)					\
do {							\
  extern FILE *asm_out_text_file;			\
  fprintf (asm_out_text_file, "\t.tag\t");		\
  ASM_OUTPUT_LABELREF (asm_out_text_file, a); 		\
  fputc (';', asm_out_text_file);			\
} while (0)

/* For block start and end, we create labels, so that
   later we can figure out where the correct offset is.
   The normal .ent/.end serve well enough for functions,
   so those are just commented out.  */

#define PUT_SDB_BLOCK_START(LINE)			\
do {							\
  extern FILE *asm_out_text_file;			\
  fprintf (asm_out_text_file,				\
	   "$Lb%d:\n\t#.begin\t$Lb%d\t%d\n",		\
	   sdb_label_count,				\
	   sdb_label_count,				\
	   (LINE));					\
  sdb_label_count++;					\
} while (0)

#define PUT_SDB_BLOCK_END(LINE)				\
do {							\
  extern FILE *asm_out_text_file;			\
  fprintf (asm_out_text_file,				\
	   "$Le%d:\n\t#.bend\t$Le%d\t%d\n",		\
	   sdb_label_count,				\
	   sdb_label_count,				\
	   (LINE));					\
  sdb_label_count++;					\
} while (0)

#define PUT_SDB_FUNCTION_START(LINE)

#define PUT_SDB_FUNCTION_END(LINE)

#define PUT_SDB_EPILOGUE_END(NAME)

#define SDB_GENERATE_FAKE(BUFFER, NUMBER) \
  sprintf ((BUFFER), ".%dfake", (NUMBER));


/* Run-time compilation parameters selecting different hardware subsets.  */

extern int target_flags;

/* Macros used in the machine description to test the flags.  */

					/* Mips vs. GNU assembler */
#define TARGET_UNIX_ASM		(target_flags & 0x00000001)
#define TARGET_MIPS_AS		TARGET_UNIX_ASM
#define TARGET_GAS		(TARGET_UNIX_ASM == 0)

					/* Debug Mode */
#define TARGET_DEBUG_MODE	(target_flags & 0x00000002)
#define TARGET_DEBUGA_MODE	(target_flags & 0x00000004)
#define TARGET_DEBUGB_MODE	(target_flags & 0x00000010)
#define TARGET_DEBUGC_MODE	(target_flags & 0x00000020)
#define TARGET_DEBUGD_MODE	(target_flags & 0x00000040)
#define TARGET_DEBUGE_MODE	(target_flags & 0x00008000)

					/* Reg. Naming in .s ($21 vs. $a0) */
#define TARGET_NAME_REGS	(target_flags & 0x00000008)

					/* addu/subbu vs. add/sub */
#define TARGET_NOFIXED_OVFL	(target_flags & 0x00000080)

					/* Optimize for Sdata/Sbss */
#define TARGET_GP_OPT		(target_flags & 0x00001000)
#define TARGET_GVALUE_MASK	(target_flags & 0x00000f00)
#define TARGET_GVALUE		(TARGET_GVALUE_MASK >> 8)

					/* Position independent code */
#define TARGET_PIC		(target_flags & 0x00002000)
#define TARGET_PIC_LARGE_OBJECT (target_flags & 0x00004000)



/* Macro to define tables used to set the flags.
   This is a list in braces of pairs in braces,
   each pair being { "NAME", VALUE }
   where VALUE is the bits to set or minus the bits to clear.
   An empty string NAME is used to identify the default VALUE.  */

#define TARGET_SWITCHES							\
{ {"mips-as",		  0x00000001},	/* MIPS assembler */		\
  {"gas",		 -0x00000001},	/* GNU  assembler */		\
  {"debug",		  0x00000002},	/* Eliminate version in output*/ \
  {"nodebug",		 -0x00000002},					\
  {"debuga",		  0x00000004},	/* don't fold SP pushes into frame */ \
  {"nodebuga",		 -0x00000004},					\
  {"debugb",		  0x00000010},	/* GO_IF_LEGITIMATE_ADDRESS debug */ \
  {"nodebugb",		 -0x00000010},					\
  {"debugc",		  0x00000020},	/* fix frame ptr debug */	\
  {"nodebugc",		 -0x00000020},					\
  {"debugd",		  0x00000040},	/* branch/cc0 debug */		\
  {"nodebugd",		 -0x00000040},					\
  {"rnames",		  0x00000008},	/* Register names like $a0 */	\
  {"nornames",		 -0x00000008},	/* Register names like $21 */	\
  {"nofixed-ovfl",	  0x00000080},	/* Use addu and subu */		\
  {"fixed-ovfl",	 -0x00000080},	/* Use add  and sub */		\
  {"G0",		  0x00000100},	/* Bit 1 of sdata size */	\
  {"nG0",		 -0x00000100},					\
  {"noG0",		 -0x00000100},					\
  {"G1",		  0x00000200},	/* Bit 2 of sdata size */	\
  {"nG1",		 -0x00000200},					\
  {"noG1",		 -0x00000200},					\
  {"G2",		  0x00000400},	/* Bit 3 of sdata size */	\
  {"nG2",		 -0x00000400},					\
  {"noG2",		 -0x00000400},					\
  {"gpOPT",		  0x00001000},	/* Optimize for global ptr */	\
  {"ngpOPT",		 -0x00001000},					\
  {"nogpOPT",		 -0x00001000},					\
  {"pic",		  0x00002000},	/* Position independent code */	\
  {"npic",		 -0x00002000},					\
  {"nopic",		 -0x00002000},					\
  {"pic-large-object",	  0x00004000},	/* Don't opt pic local funcs */	\
  {"nopic-large-object", -0x00004000},					\
  {"debuge",		  0x00008000},	/* FUNCTION_ARG debug */	\
  {"nodebuge",		 -0x00008000},					\
  {"",			 TARGET_DEFAULT}}

/* Default target_flags if no switches specified (-mmips-as, -mnofixed-ovfl,
   -G0, -G1 [same as -G 8]).  OSF/1 does not set -mmips-as, and sets -G 0. */

#ifndef OSF_OS
#define TARGET_DEFAULT 0x00000381
#else
#define TARGET_DEFAULT 0x00000080
#endif

/* Default GVALUE  (data item size threshold for selection of Sdata/data)
   is computed : GVALUE ==  ( ((i=G0+2*G1+4*G2) < 6)
				        ? 1<<i
					: 1<< (i+3))
*/

/* Target machine storage layout */

/* Define this if most significant bit is lowest numbered
   in instructions that operate on numbered bit-fields.
*/
/* #define BITS_BIG_ENDIAN */

/* Define this if most significant byte of a word is the lowest numbered. */
#ifndef DECSTATION
#define BYTES_BIG_ENDIAN
#endif

/* Define this if most significant word of a multiword number is numbered. */
#ifndef DECSTATION
#define WORDS_BIG_ENDIAN
#endif

/* Define macros to easily access the most and least significant words
   without a lot of #ifdef's.  */

#ifdef WORDS_BIG_ENDIAN
#define MOST_SIGNIFICANT_WORD	0
#define LEAST_SIGNIFICANT_WORD	1

#else
#define MOST_SIGNIFICANT_WORD	1
#define LEAST_SIGNIFICANT_WORD	0
#endif

/* Number of bits in an addressible storage unit */
#define BITS_PER_UNIT 8

/* Width in bits of a "word", which is the contents of a machine register.
   Note that this is not necessarily the width of data type `int';
   if using 16-bit ints on a 68000, this would still be 32.
   But on a machine with 16-bit registers, this would be 16.  */
#define BITS_PER_WORD 32

/* Width of a word, in units (bytes).  */
#define UNITS_PER_WORD 4

/* Width in bits of a pointer.
   See also the macro `Pmode' defined below.  */
#define POINTER_SIZE 32

/* Allocation boundary (in *bits*) for storing pointers in memory.  */
#define POINTER_BOUNDARY 32

/* Allocation boundary (in *bits*) for storing arguments in argument list.  */
#define PARM_BOUNDARY 32

/* Give parms extra alignment, up to this much, if their types want it.  */
#define MAX_PARM_BOUNDARY 64

/* Allocation boundary (in *bits*) for the code of a function.  */
#define FUNCTION_BOUNDARY 32

/* Alignment of field after `int : 0' in a structure.  */
#define EMPTY_FIELD_BOUNDARY 32

/* Every structure's size must be a multiple of this.  */
#define STRUCTURE_SIZE_BOUNDARY 8

/* There is no point aligning anything to a rounder boundary than this.  */
#define BIGGEST_ALIGNMENT 64

/* Define this if move instructions will actually fail to work
   when given unaligned data.  */
#define STRICT_ALIGNMENT

/* Define this macro if an argument declared as `char' or `short' in a
   prototype should actually be passed as an `int'.  In addition to
   avoiding errors in certain cases of mismatch, it also makes for
   better code on certain machines. */
#define PROMOTE_PROTOTYPES


/* Standard register usage.  */

/* Number of actual hardware registers.
   The hardware registers are assigned numbers for the compiler
   from 0 to just below FIRST_PSEUDO_REGISTER.
   All registers that the compiler knows about must be given numbers,
   even those that are not normally considered general registers.  */
#define FIRST_PSEUDO_REGISTER 64

/* 1 for registers that have pervasive standard uses
   and are not available for the register allocator.

   On the MIPS, see conventions, page D-2

   I have chosen not to  take Multiply/Divide HI,LO or PC into
   account.  */

#define FIXED_REGISTERS {1, 1, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0,\
		         0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 1, 1, 1, 1, 0, 1,\
		         0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0,\
		         0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0	\
}


/* 1 for registers not available across function calls.
   These must include the FIXED_REGISTERS and also any
   registers that can be used without being saved.
   The latter must include the registers where values are returned
   and the register where structure-value addresses are passed.
   Aside from that, you can include as many other registers as you like.  */

#define CALL_USED_REGISTERS {1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1,\
		             0, 0, 0, 0, 0, 0, 0, 0, 1, 1, 1, 1, 1, 1, 0, 1,\
		             1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1,\
		             1, 1, 1, 1, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0\
}


/* Internal macros to classify a register number as to whether it's a
   general purpose register or a floating point register.  The macro
   FP_CALL_REG_P also allows registers $4 and $6 as floating point
   registers to pass floating point as per MIPS spec. */

#define GP_REG_FIRST 0
#define GP_REG_LAST  31
#define GP_REG_NUM   (GP_REG_LAST - GP_REG_FIRST + 1)

#define FP_REG_FIRST 32
#define FP_REG_LAST  63
#define FP_REG_NUM   (FP_REG_LAST - FP_REG_FIRST + 1)

#define GP_REG_P(REGNO) ((unsigned) ((REGNO) - GP_REG_FIRST) < GP_REG_NUM)
#define FP_REG_P(REGNO) ((unsigned) ((REGNO) - FP_REG_FIRST) < FP_REG_NUM)

#define FP_CALL_REG_P(REGNO)					\
  (FP_REG_P (REGNO)						\
   || (REGNO) == (4 + GP_REG_FIRST)				\
   || (REGNO) == (6 + GP_REG_FIRST))


/* Return number of consecutive hard regs needed starting at reg REGNO
   to hold something of mode MODE.
   This is ordinarily the length in words of a value of mode MODE
   but can be less for certain modes in special long registers.

   On the MIPS, all general registers are one word long. I have chosen to
   use Floating point register pairs.  */

#define HARD_REGNO_NREGS(REGNO, MODE)					\
 ((MODE == SFmode) ? 2 :						\
  ((GET_MODE_SIZE (MODE) + UNITS_PER_WORD - 1) / UNITS_PER_WORD))

/* Value is 1 if hard register REGNO can hold a value of machine-mode MODE.
   On the MIPS, all general registers can hold all  modes, except
   FLOATING POINT.  */

#define HARD_REGNO_MODE_OK(REGNO, MODE)					\
  ((GET_MODE_CLASS (MODE) == MODE_INT || MODE == VOIDmode)		\
	? (GP_REG_P (REGNO))						\
	: (GET_MODE_CLASS (MODE) == MODE_FLOAT)				\
		?  (((REGNO) & 1) == 0 && FP_CALL_REG_P (REGNO))	\
		: 0)							\


/* Value is 1 if it is a good idea to tie two pseudo registers
   when one has mode MODE1 and one has mode MODE2.
   If HARD_REGNO_MODE_OK could produce different values for MODE1 and MODE2,
   for any hard reg, then this must be 0 for correct output.  */
#define MODES_TIEABLE_P(MODE1, MODE2)					\
  (   ((MODE1) == SFmode || (MODE1) == DFmode)				\
   == ((MODE2) == SFmode || (MODE2) == DFmode))

/* MIPS pc is apparently not overloaded on a register.  */
/* #define PC_REGNUM 15                                 */

/* Register to use for pushing function arguments.  */
#define STACK_POINTER_REGNUM 29

/* Offset from the stack pointer to the first available location.  */
#define STACK_POINTER_OFFSET 0

/* Base register for access to local variables of the function.  */
#define FRAME_POINTER_REGNUM 30

/* Value should be nonzero if functions must have frame pointers.
   Zero means the frame pointer need not be set up (and parms
   may be accessed via the stack pointer) in functions that seem suitable.
   This is computed in `reload', in reload1.c.

   At present this is required if we are not a leaf procedure.  This
   is because the .frame directive requires a register that does not
   change throughout the procedure call, and until stack pushes are
   folded into the initial stack allocation, we need an unvarying fp.  */
#define FRAME_POINTER_REQUIRED (stack_args_pushed > 0)

/* Base register for access to arguments of the function.  */
#define ARG_POINTER_REGNUM FRAME_POINTER_REGNUM

/* Register in which static-chain is passed to a function.  */
#define STATIC_CHAIN_REGNUM 2

/* Register in which address to store a structure value
   is passed to a function.  */
#define STRUCT_VALUE_REGNUM 4

/* Mips registers used in prologue/epilogue code when the stack frame
   is larger than 32K bytes.  These registers must come from the
   scratch register set, and not used for passing and returning
   arguments and any other information used in the calling sequence
   (such as pic).  */
#define MIPS_TEMP1_REGNUM 8
#define MIPS_TEMP2_REGNUM 9

/* Define NO_FUNCTION_CSE if it is as good or better to call a constant
   function address than to call an address kept in a register.  */
#define NO_FUNCTION_CSE


/* Define the classes of registers for register constraints in the
   machine description.  Also define ranges of constants.

   One of the classes must always be named ALL_REGS and include all hard regs.
   If there is more than one class, another class must be named NO_REGS
   and contain no registers.

   The name GENERAL_REGS must be the name of a class (or an alias for
   another name such as ALL_REGS).  This is the class of registers
   that is allowed by "g" or "r" in a register constraint.
   Also, registers outside this class are allocated only when
   instructions express preferences for them.

   The classes must be numbered in nondecreasing order; that is,
   a larger-numbered class must never be contained completely
   in a smaller-numbered class.

   For any two classes, it is very desirable that there be another
   class that represents their union.  */

/* The MIPS has general and floating point registers.  */


enum reg_class  { NO_REGS, GR_REGS, FP_REGS, ALL_REGS, LIM_REG_CLASSES } ;

#define N_REG_CLASSES (int) LIM_REG_CLASSES

#define GENERAL_REGS GR_REGS

/* Give names of register classes as strings for dump file.   */

#define REG_CLASS_NAMES							\
 {"NO_REGS", "GR_REGS", "FP_REGS", "ALL_REGS" }

/* Define which registers fit in which classes.
   This is an initializer for a vector of HARD_REG_SET
   of length N_REG_CLASSES.  */

#define REG_CLASS_CONTENTS {{0x00000000, 0x00000000},			\
                            {0xffffffff, 0x00000000},			\
                            {0x00000000, 0xffffffff},			\
			    {0xffffffff, 0xffffffff}}


/* The same information, inverted:
   Return the class number of the smallest class containing
   reg number REGNO.  This could be a conditional expression
   or could index an array.  */

#define REGNO_REG_CLASS(REGNO) ((FP_REG_P (REGNO)) ? FP_REGS : GR_REGS)

/* Define a table that lets us find quickly all the reg classes
   containing a given one.  This is the initializer for an
   N_REG_CLASSES x N_REG_CLASSES array of reg class codes.
   Row N is a sequence containing all the class codes for
   classes that contain all the regs in class N.  Each row
   contains no duplicates, and is terminated by LIM_REG_CLASSES.  */

/* We give just a dummy for the first element, which is for NO_REGS.  */
/* #define REG_CLASS_SUPERCLASSES  {{LIM_REG_CLASSES},			\
  {GR_REGS,ALL_REGS,LIM_REG_CLASSES},					\
  {FP_REGS,ALL_REGS,LIM_REG_CLASSES},					\
  {ALL_REGS,LIM_REG_CLASSES}						\
}
*/
/* We give just a dummy for the first element, which is for NO_REGS.  */
#define REG_CLASS_SUPERCLASSES  {{LIM_REG_CLASSES},			\
  {ALL_REGS,LIM_REG_CLASSES},						\
  {ALL_REGS,LIM_REG_CLASSES},						\
  {LIM_REG_CLASSES}							\
}

/* The inverse relationship:
   for each class, a list of all reg classes contained in it.  */
#define REG_CLASS_SUBCLASSES						\
{{LIM_REG_CLASSES},							\
  {GR_REGS,LIM_REG_CLASSES},						\
  {FP_REGS,LIM_REG_CLASSES},\
  {GR_REGS, FP_REGS, ALL_REGS, LIM_REG_CLASSES}\
}

/* Define a table that lets us find quickly the class
   for the subunion of any two classes.

   We say "subunion" because the result need not be exactly
   the union; it may instead be a subclass of the union
   (though the closer to the union, the better).
   But if it contains anything beyond union of the two classes,
   you will lose!

   This is an initializer for an N_REG_CLASSES x N_REG_CLASSES
   array of reg class codes.  The subunion of classes C1 and C2
   is just element [C1, C2].  */

#define REG_CLASS_SUBUNION						\
{{NO_REGS,  GR_REGS,   FP_REGS,  ALL_REGS},				\
 {GR_REGS,  GR_REGS,   ALL_REGS, ALL_REGS},				\
 {FP_REGS,  ALL_REGS,  FP_REGS,  ALL_REGS},				\
 {ALL_REGS, ALL_REGS,  ALL_REGS, ALL_REGS}}

/* The class value for index registers, and the one for base regs.  */

#define INDEX_REG_CLASS GR_REGS
#define BASE_REG_CLASS  GR_REGS


				/* REGISTER AND CONSTANT CLASSES
				 */

/* Get reg_class from a letter such as appears in the machine
description.  */
				/* DEFINED REGISTER CLASSES:
				**
				** 'f'     : Floating point registers
				** 'y'     : General register when used to
				**           transfer chunks of Floating point
				**           with mfc1 mtc1 insn
				 */

#define REG_CLASS_FROM_LETTER(C)					\
   ((C) == 'f' ? FP_REGS:						\
     (C) == 'y' ? GR_REGS:NO_REGS)

/* The letters I, J, K, L and M in a register constraint string
   can be used to stand for particular ranges of immediate operands.
   This macro defines what the ranges are.
   C is the letter, and VALUE is a constant value.
   Return 1 if VALUE is in the range specified by C.  */

/*   For MIPS, `I' is used for the range of constants an arithmetic insn
                   can actually contain (16 bits signed integers).
               `J' is used for the range which is just zero (since that is
	           available as $R0).
	       `K' is used for the range of constants a logical insn
	           can actually contain (16 bit zero-extended integers).
*/

#define SMALL_INT(X) ((unsigned) (INTVAL (X) + 0x8000) < 0x10000)
#define SMALL_INT_UNSIGNED(X) ((unsigned) (INTVAL (X)) < 0x10000)

#define CONST_OK_FOR_LETTER_P(VALUE, C)					\
  ((C) == 'I' ? (unsigned) ((VALUE) + 0x8000) < 0x10000			\
   : (C) == 'J' ? (VALUE) == 0						\
   : (C) == 'K' ? (unsigned) (VALUE) < 0x10000				\
   : 0)

/* Similar, but for floating constants, and defining letters G and H.
   Here VALUE is the CONST_DOUBLE rtx itself.  */

				/* DEFINED FLOATING CONSTANT CLASSES:
				**
				** 'G'     : Floating point 0
				 */
#define CONST_DOUBLE_OK_FOR_LETTER_P(VALUE, C)				\
  ((C) == 'G' && XINT (VALUE, 0) == 0 && XINT (VALUE, 1) == 0)

/* Given an rtx X being reloaded into a reg required to be
   in class CLASS, return the class of reg to actually use.
   In general this is just CLASS; but on some machines
   in some cases it is preferable to use a more restrictive class.  */

#define PREFERRED_RELOAD_CLASS(X,CLASS)					\
    (((GET_MODE(X) == SFmode) || (GET_MODE(X) == DFmode))? FP_REGS  :	\
     ((GET_MODE(X) == VOIDmode) ? GR_REGS :(CLASS)))

/* Same but Mode has been extracted already
*/

#define PREFERRED_RELOAD_CLASS_FM(X,CLASS)				\
    ((((X) == SFmode) || ((X) == DFmode))? FP_REGS  :			\
     (((X) == VOIDmode) ? GR_REGS :(CLASS)))

/* Return the maximum number of consecutive registers
   needed to represent mode MODE in a register of class CLASS.  */

#define CLASS_MAX_NREGS(CLASS, MODE)					\
 ((((MODE) == DFmode) || ((MODE) == SFmode)) ? 2			\
  : ((MODE) == VOIDmode)? ((CLASS) == FP_REGS ? 2 :1)			\
  : ((GET_MODE_SIZE (MODE) + UNITS_PER_WORD - 1) / UNITS_PER_WORD))


/* Stack layout; function entry, exit and calling.  */

/* Define this if pushing a word on the stack
   makes the stack pointer a smaller address.  */
#define STACK_GROWS_DOWNWARD

/* Define this if the nominal address of the stack frame
   is at the high-address end of the local variables;
   that is, each additional local variable allocated
   goes at a more negative offset in the frame.  */
#define FRAME_GROWS_DOWNWARD

/* Offset within stack frame to start allocating local variables at.
   If FRAME_GROWS_DOWNWARD, this is the offset to the END of the
   first local allocated.  Otherwise, it is the offset to the BEGINNING
   of the first local allocated.  */
#define STARTING_FRAME_OFFSET -8

/* If we generate an insn to push BYTES bytes,
   this says how many the stack pointer really advances by.
   On the vax, sp@- in a byte insn really pushes a word.  */

/* #define PUSH_ROUNDING(BYTES) 0 */

/* Offset of first parameter from the argument pointer register value.  */
#define FIRST_PARM_OFFSET(FNDECL) 0

/* Offset from top-of-stack address to location to store the
   function parameter if it can't go in a register.
   Addresses for following parameters are computed relative to this one.

   It also has the effect of counting register arguments in the total
   argument size. */
#define FIRST_PARM_CALLER_OFFSET(FNDECL) 0

/* When a parameter is passed in a register, stack space is still
   allocated for it.  For the MIPS, stack space must be allocated, cf
   Asm Lang Prog Guide page 7-8.

   BEWARE that some space is also allocated for non existing arguments
   in register. In case an argument list is of form GF used registers
   are a0 (a2,a3), but we should push over a1...  */
#define REG_PARM_STACK_SPACE

/* Align stack frames on 64 bits (Double Word ).  */
#define STACK_BOUNDARY 64


/* Standard GCC stack related variables that we reference.  */

extern int optimize;
extern int may_call_alloca;
extern int current_function_calls_alloca;
extern int frame_pointer_needed;
extern int flag_omit_frame_pointer;

/* MIPS external variables defined in out-mips.c.  */

extern char *reg_numchar[];		/* register names as $r2, etc. */
extern char *current_function_name;	/* current function being compiled */
extern int num_source_filenames;	/* current .file # */
extern int inside_function;		/* != 0 if inside of a function */
extern int stack_args_pushed;		/* max bytes pushed for calls */
extern int stack_args_preallocated;	/* # bytes for args preallocated */
extern int sdb_label_count;		/* block start/end next label # */
extern int mips_section_threshold;	/* # bytes of data/sdata cutoff */
extern int sym_lineno;			/* sgi next label # for each stmt */


/* Make sure 16 bytes are always allocated on the stack.  */
#ifndef STACK_ARGS_ADJUST
#define STACK_ARGS_ADJUST(SIZE)						\
{									\
  if (SIZE.constant < 16)						\
    SIZE.constant = 16;							\
}
#endif

/* Value is 1 if returning from a function call automatically
   pops the arguments described by the number-of-args field in the call.
   FUNTYPE is the data type of the function (as a tree),
   or for a library call it is an identifier node for the subroutine name.  */

#define RETURN_POPS_ARGS(FUNTYPE) 0


/* Symbolic macros for the registers used to return integer and floating
   point values.  */

#define GP_RETURN 2
#define FP_RETURN 32

/* Symbolic macros for the first/last argument registers.  */

#define GP_ARG_FIRST 4
#define GP_ARG_LAST  7
#define FP_ARG_FIRST 44
#define FP_ARG_LAST  47

#define MAX_ARGS_IN_REGISTERS	4

/* Define how to find the value returned by a library function
   assuming the value has mode MODE.  */

#define LIBCALL_VALUE(MODE)						\
  gen_rtx (REG, MODE,							\
	   (GET_MODE_CLASS (MODE) == MODE_FLOAT)			\
		? FP_RETURN						\
		: GP_RETURN)

/* Define how to find the value returned by a function.
   VALTYPE is the data type of the value (as a tree).
   If the precise function being called is known, FUNC is its FUNCTION_DECL;
   otherwise, FUNC is 0.  */

#define FUNCTION_VALUE(VALTYPE, FUNC) LIBCALL_VALUE (TYPE_MODE (VALTYPE))


/* 1 if N is a possible register number for a function value.
   On the MIPS, R2 R3 and F0 F2 are the only register thus used.
   Currently, R2 and F0 are only implemented  here (C has no complex type)  */

#define FUNCTION_VALUE_REGNO_P(N) ((N) == GP_RETURN || (N) == FP_RETURN)

/* 1 if N is a possible register number for function argument passing.  */

#define FUNCTION_ARG_REGNO_P(N) (((N) >= GP_ARG_FIRST && (N) <= GP_ARG_LAST)   \
				 || ((N) >= FP_ARG_FIRST && (N) <= FP_ARG_LAST \
				     && (0 == (N) % 2)))

/* A C expression which can inhibit the returning of certain function
   values in registers, based on the type of value.  A nonzero value says
   to return the function value in memory, just as large structures are
   always returned.  Here TYPE will be a C expression of type
   `tree', representing the data type of the value.

   Note that values of mode `BLKmode' are returned in memory
   regardless of this macro.  Also, the option `-fpcc-struct-return'
   takes effect regardless of this macro.  On most systems, it is
   possible to leave the macro undefined; this causes a default
   definition to be used, whose value is the constant 0.

   GCC normally converts 1 byte structures into chars, 2 byte
   structs into shorts, and 4 byte structs into ints, and returns
   them this way.  Defining the following macro overides this,
   to give us MIPS cc compatibility.  */

#define RETURN_IN_MEMORY(TYPE)	\
  ((TREE_CODE (TYPE) == RECORD_TYPE) || (TREE_CODE (TYPE) == UNION_TYPE))


/* Define a data type for recording info about an argument list
   during the scan of that argument list.  This data type should
   hold all necessary information about the function itself
   and about the args processed so far, enough to enable macros
   such as FUNCTION_ARG to determine where the next arg should go.
*/

typedef struct mips_args {
  int gp_reg_found;
  int arg_number;
  int arg_words;
} *CUMULATIVE_ARGS;

/* Initialize a variable CUM of type CUMULATIVE_ARGS
   for a call to a function whose data type is FNTYPE.
   For a library call, FNTYPE is 0.

*/

extern void init_cumulative_args ();

#define INIT_CUMULATIVE_ARGS(CUM,FNTYPE)				\
do {									\
  CUM = (CUMULATIVE_ARGS) alloca (sizeof (*CUM));			\
  init_cumulative_args (CUM, FNTYPE);					\
} while (0)

/* Update the data in CUM to advance over an argument
   of mode MODE and data type TYPE.
   (TYPE is null for libcalls where that information may not be available.)  */

#define FUNCTION_ARG_ADVANCE(CUM, MODE, TYPE, NAMED)			\
  (function_arg_advance(CUM, MODE, TYPE, NAMED))

extern void function_arg_advance();

/* Determine where to put an argument to a function.
   Value is zero to push the argument on the stack,
   or a hard register in which to store the argument.

   MODE is the argument's machine mode.
   TYPE is the data type of the argument (as a tree).
    This is null for libcalls where that information may
    not be available.
   CUM is a variable of type CUMULATIVE_ARGS which gives info about
    the preceding args and about the function being called.
   NAMED is nonzero if this argument is a named parameter
    (otherwise it is an extra parameter matching an ellipsis).  */

extern struct rtx_def *function_arg ();

#define FUNCTION_ARG(CUM, MODE, TYPE, NAMED) \
  (function_arg(CUM, MODE, TYPE, NAMED))

/* For an arg passed partly in registers and partly in memory,
   this is the number of registers used.
   For args passed entirely in registers or entirely in memory, zero.
*/

extern int function_arg_partial_nregs ();

#define FUNCTION_ARG_PARTIAL_NREGS(CUM, MODE, TYPE, NAMED) \
  (function_arg_partial_nregs (CUM, MODE, TYPE, NAMED))


/* This macro generates the assembly code for function entry.
   FILE is a stdio stream to output the code to.
   SIZE is an int: how many units of temporary storage to allocate.
   Refer to the array `regs_ever_live' to determine which registers
   to save; `regs_ever_live[I]' is nonzero if register number I
   is ever used in the function.  This macro is responsible for
   knowing which registers should not be saved even if used.  */

extern void function_prologue ();

#define FUNCTION_PROLOGUE(FILE, SIZE) function_prologue(FILE, SIZE)

/* This macro generates the assembly code for function exit,
   on machines that need it.  If FUNCTION_EPILOGUE is not defined
   then individual return instructions are generated for each
   return statement.  Args are same as for FUNCTION_PROLOGUE.  */

extern void function_epilogue ();

#define FUNCTION_EPILOGUE(FILE, SIZE) function_epilogue(FILE, SIZE)

/* Tell prologue and epilogue if Register containing return
   address should be saved / restored.  */

#define MUST_SAVE_REGISTER(regno) \
 ((regs_ever_live[regno] && !call_used_regs[regno]) || \
  (regno == FRAME_POINTER_REGNUM && frame_pointer_needed) || \
  (regno == 31 && regs_ever_live[31]))

/* ALIGN FRAMES on double word boundaries */

#define AL_ADJUST_ALIGN(LOC) (((LOC)+7) & 0xfffffff8)


/* If the memory Address ADDR is relative to the frame pointer,
   correct it to be relative to the stack pointer. This is for
   when we don't use a frame pointer.
   ADDR should be a variable name.  */

#define FIX_FRAME_POINTER_ADDRESS(ADDR,DEPTH)				\
{ ADDR = mips_fix_frame_pointer(ADDR, DEPTH); }

extern struct rtx_def *mips_fix_frame_pointer ();

/* Output assembler code to FILE to increment profiler label # LABELNO
   for profiling a function entry.  */

#define FUNCTION_PROFILER(FILE, LABELNO)				\
{									\
  register char **reg_ptr = (TARGET_NAME_REGS) ? reg_names : reg_numchar; \
									\
  fprintf (FILE, "\t.set\tnoreorder\n");				\
  fprintf (FILE, "\t.set\tnoat\n");					\
  fprintf (FILE, "\tmove\t%s,%s\t\t# save current return address\n",	\
	   reg_ptr[1], reg_ptr[31]);					\
  fprintf (FILE, "\tjal\t_mcount\n");					\
  fprintf (FILE, "\tsubu\t%s,%s,8\t\t# _mcount pops 2 words from  stack\n", \
	   reg_ptr[STACK_POINTER_REGNUM], reg_ptr[STACK_POINTER_REGNUM]); \
  fprintf (FILE, "\t.set\treorder\n");					\
  fprintf (FILE, "\t.set\tat\n");					\
}

/* EXIT_IGNORE_STACK should be nonzero if, when returning from a function,
   the stack pointer does not matter.  The value is tested only in
   functions that have frame pointers.
   No definition is equivalent to always zero.  */

#define EXIT_IGNORE_STACK 1


/* Addressing modes, and classification of registers for them.  */

/* #define HAVE_POST_INCREMENT */
/* #define HAVE_POST_DECREMENT */

/* #define HAVE_PRE_DECREMENT */
/* #define HAVE_PRE_INCREMENT */

/* These assume that REGNO is a hard or pseudo reg number.
   They give nonzero only if REGNO is a hard reg of the suitable class
   or a pseudo reg currently allocated to a suitable hard reg.
   These definitions are NOT overridden anywhere.  */

#define REGNO_OK_FOR_INDEX_P(regno)					\
((regno) < FIRST_PSEUDO_REGISTER || reg_renumber[regno] >= 0)

#define REGNO_OK_FOR_BASE_P(regno)					\
((regno) < FIRST_PSEUDO_REGISTER || reg_renumber[regno] >= 0)

/* The macros REG_OK_FOR..._P assume that the arg is a REG rtx
   and check its validity for a certain class.
   We have two alternate definitions for each of them.
   The usual definition accepts all pseudo regs; the other rejects them all.
   The symbol REG_OK_STRICT causes the latter definition to be used.

   Most source files want to accept pseudo regs in the hope that
   they will get allocated to the class that the insn wants them to be in.
   Some source files that are used after register allocation
   need to be strict.  */

#ifndef REG_OK_STRICT

#define REG_OK_FOR_INDEX_P(X) 1		/* ok if index or pseudo reg */
#define REG_OK_FOR_BASE_P(X)  1		/* ok if base reg. of pseudo reg */

#else

#define REG_OK_FOR_INDEX_P(X) REGNO_OK_FOR_INDEX_P (REGNO (X))
#define REG_OK_FOR_BASE_P(X)  REGNO_OK_FOR_BASE_P  (REGNO (X))

#endif


/* Maximum number of registers that can appear in a valid memory address.  */

#define MAX_REGS_PER_ADDRESS 1

/* GO_IF_LEGITIMATE_ADDRESS recognizes an RTL expression
   that is a valid memory address for an instruction.
   The MODE argument is the machine mode for the MEM expression
   that wants to use this address.

   The other macros defined here are used only in GO_IF_LEGITIMATE_ADDRESS,
   except for CONSTANT_ADDRESS_P which is actually machine-independent.  */

/* 1 if X is an address that we could indirect through.  */
#define INDIRECTABLE_ADDRESS_P(X)					\
  (CONSTANT_ADDRESS_P (X)						\
   || (GET_CODE (X) == REG && REG_OK_FOR_BASE_P (X))			\
   || (GET_CODE (X) == PLUS						\
       && ((xplus0 = XEXP (X, 0)),					\
	   (xplus1 = XEXP (X, 1)),					\
	   ((GET_CODE (xplus0) != REG && GET_CODE (xplus1) == REG)	\
	    ? ((xplus0 = XEXP (X, 1)), (xplus1 = XEXP (X, 0)))		\
	    : 0),							\
	   GET_CODE (xplus0) == REG)					\
       && REG_OK_FOR_BASE_P (xplus0)					\
       && ((GET_CODE (xplus1) == CONST_INT && SMALL_INT (xplus1))	\
	   || (GET_CODE (xplus1) == LABEL_REF)				\
	   || (GET_CODE (xplus1) == SYMBOL_REF)				\
	   || (GET_CODE (xplus1) == CONST)				\
	   || (xplus0 == stack_pointer_rtx				\
	       && (GET_CODE (xplus1) == CONST || (GET_CODE (xplus1) == SYMBOL_REF))))))


#if 1
extern void trace ();
#define GO_PRINTF(x)	trace(x)
#define GO_DEBUG_RTX(x) debug_rtx(x)

#else
#define GO_PRINTF(x)
#define GO_DEBUG_RTX(x)
#endif

/* Go to ADDR if X is a valid address not using indexing.
   (This much is the easy part.)  */
#define GO_IF_LEGITIMATE_ADDRESS(MODE, X, ADDR)				\
{									\
  register rtx xinsn = (X);						\
  register rtx xplus0, xplus1;						\
									\
  if (TARGET_DEBUGB_MODE)						\
    {									\
      GO_PRINTF ("\n==================== GO_IF_LEGITIMATE_ADDRESS\n");	\
      GO_DEBUG_RTX (xinsn);						\
    }									\
									\
  if (GET_CODE (xinsn) == REG)		goto ADDR;			\
  if (INDIRECTABLE_ADDRESS_P (xinsn))   goto ADDR;			\
									\
  if (TARGET_DEBUGB_MODE)						\
    GO_PRINTF ("Not a legitimate address\n");				\
}


#define CONSTANT_ADDRESS_P(X) CONSTANT_P (X)


/* Nonzero if the constant value X is a legitimate general operand.
   It is given that X satisfies CONSTANT_P or is a CONST_DOUBLE.

   At present, GAS doesn't understand li.[sd], so don't allow it
   to be generated at present.  Also, the MIPS assembler does not
   grok li.d Infinity.  */

#define LEGITIMATE_CONSTANT_P(X) (GET_CODE (X) != CONST_DOUBLE)

/* Try machine-dependent ways of modifying an illegitimate address
   to be legitimate.  If we find one, return the new, valid address.
   This macro is used in only one place: `memory_address' in explow.c.

   OLDX is the address as it was before break_out_memory_refs was called.
   In some cases it is useful to look at this to decide what needs to be done.

   MODE and WIN are passed so that this macro can use
   GO_IF_LEGITIMATE_ADDRESS.

   It is always safe for this macro to do nothing.  It exists to recognize
   opportunities to optimize the output.

   For the MIPS (so far ..), nothing needs to be done.

   ACHTUNG this is actually used by the FLOW analysis to get rid
   of statements....

*/

#define LEGITIMIZE_ADDRESS(X,OLDX,MODE,WIN) {}

/* Go to LABEL if ADDR (a legitimate address expression)
   has an effect that depends on the machine mode it is used for.  */

#define GO_IF_MODE_DEPENDENT_ADDRESS(ADDR,LABEL) {}


/* Specify the machine mode that this machine uses
   for the index in the tablejump instruction.  */
#define CASE_VECTOR_MODE SImode

/* Define this if the tablejump instruction expects the table
   to contain offsets from the address of the table.
   Do not define this if the table should contain absolute addresses.  */
/* #define CASE_VECTOR_PC_RELATIVE */

/* Specify the tree operation to be used to convert reals to integers.  */
#define IMPLICIT_FIX_EXPR FIX_ROUND_EXPR

/* This is the kind of divide that is easiest to do in the general case.  */
#define EASY_DIV_EXPR TRUNC_DIV_EXPR

/* Define this as 1 if `char' should by default be signed; else as 0.  */
#define DEFAULT_SIGNED_CHAR 1

/* Max number of bytes we can move from memory to memory
   in one reasonably fast instruction.  */
#define MOVE_MAX 4

/* Nonzero if access to memory by bytes is slow and undesirable.  */
#define SLOW_BYTE_ACCESS 0

/* We assume that the store-condition-codes instructions store 0 for false
   and some other value for true.  This is the value stored for true.  */

#define STORE_FLAG_VALUE 1

/* Declarations for condition code stuff.  */
extern void compare_collect ();
extern void compare_restore ();

/* Define this if zero-extension is slow (more than one real instruction).  */
#define SLOW_ZERO_EXTEND

/* Define if shifts truncate the shift count
   which implies one can omit a sign-extension or zero-extension
   of a shift count.

   Only 5 bits are used in SLLV and SRLV
*/
#define SHIFT_COUNT_TRUNCATED


/* Value is 1 if truncating an integer of INPREC bits to OUTPREC bits
   is done just by pretending it is already truncated.  */
#define TRULY_NOOP_TRUNCATION(OUTPREC, INPREC) 1

/* Specify the machine mode that pointers have.
   After generation of rtl, the compiler makes no further distinction
   between pointers and any other objects of this machine mode.  */
#define Pmode SImode

/* A function address in a call instruction
   is a word address (for indexing purposes)
   so give the MEM rtx a words's mode.  */

#define FUNCTION_MODE SImode

/* Compute the cost of computing a constant rtl expression RTX
   whose rtx-code is CODE.  The body of this macro is a portion
   of a switch statement.  If the code is computed here,
   return it with a return statement.  Otherwise, break from the switch.  */

#define CONST_COSTS(RTX,CODE)						\
  case CONST_INT:							\
    /* Constant zero is super cheap due to register 0.  */		\
    if (RTX == const0_rtx) return 0;					\
    if ((INTVAL (RTX) < 0x7fff) && (- INTVAL(RTX) < 0x7fff)) return 1;	\
  case CONST:								\
  case LABEL_REF:							\
  case SYMBOL_REF:							\
    return 3;								\
  case CONST_DOUBLE:							\
    return 5;

/* Used in by the peephole code.  */
#define additive_op(op,mode) (GET_CODE (op) == PLUS || GET_CODE (op) == MINUS)


/* Tell final.c how to eliminate redundant test instructions.  */

/* Here we define machine-dependent flags and fields in cc_status
   (see `conditions.h').  No extra ones are needed for the vax.  */
/* Tell final.c how to eliminate redundant test instructions.  */

/* Tell final.c how to eliminate redundant test instructions.  */

/* Here we define machine-dependent flags and fields in cc_status
   (see `conditions.h').  No extra ones are needed for the vax.  */

/* Store in cc_status the expressions
   that the condition codes will describe
   after execution of an instruction whose pattern is EXP.
   Do not alter them if the instruction would not alter the cc's.  */

#define NOTICE_UPDATE_CC(EXP, INSN)					\
  CC_STATUS_INIT;


/* Here we define machine-dependent flags and fields in cc_status
   (see `conditions.h').   */


/* Control the assembler format that we output.  */

/* Output at beginning of assembler file.
   If we are optimizing to use the global pointer, create a temporary
   file to hold all of the text stuff, and write it out to the end.
   This is needed because the MIPS assembler is evidently one pass,
   and if it hasn't seen the relevant .comm/.lcomm/.extern/.sdata
   declaration when the code is processed, it generates a two
   instruction sequence.  */

#define ASM_FILE_START(STREAM)						\
{									\
  extern FILE *asm_out_text_file, *asm_out_data_file;			\
  extern FILE *tmpfile ();						\
  if (TARGET_NAME_REGS)							\
    fprintf (STREAM, "#include <regdef.h>\n");				\
  ASM_OUTPUT_SOURCE_FILENAME (STREAM, main_input_filename);		\
  print_options(STREAM);						\
  data_section ();		/* put gcc_compiled. in data, not text*/\
  if (TARGET_GP_OPT)							\
    {									\
      asm_out_data_file = STREAM;					\
      asm_out_text_file = tmpfile ();					\
      if (!asm_out_text_file)						\
	pfatal_with_name ("Can't open temporary file with tmpfile");	\
    }									\
  else									\
    asm_out_data_file = asm_out_text_file = STREAM;			\
}

/* Output to assembler file text saying following lines
   may contain character constants, extra white space, comments, etc.  */

#define ASM_APP_ON " #APP\n"

/* Output to assembler file text saying following lines
   no longer contain unusual constructs.  */

#define ASM_APP_OFF " #NO_APP\n"

/* How to refer to registers in assembler output.
   This sequence is indexed by compiler's hard-register-number (see above).  */

#define REGISTER_NAMES							\
{"$0", "at", "v0", "v1", "a0", "a1", "a2", "a3", "t0",			\
 "t1", "t2", "t3", "t4", "t5", "t6", "t7","s0",				\
 "s1","s2","s3","s4","s5","s6","s7","t8","t9",				\
 "k0","k1","gp","sp","fp","ra",						\
 "$f0","$f1","$f2","$f3","$f4","$f5","$f6","$f7","$f8","$f9",		\
"$f10","$f11","$f12","$f13","$f14","$f15","$f16","$f17","$f18","$f19",	\
"$f20","$f21","$f22","$f23","$f24","$f25","$f26","$f27","$f28","$f29",	\
"$f30","$f31"								\
}
#define REGISTER_NUMCHAR						\
{									\
"$0","$1","$2","$3","$4","$5","$6","$7","$8","$9",			\
"$10","$11","$12","$13","$14","$15","$16","$17","$18","$19",		\
"$20","$21","$22","$23","$24","$25","$26","$27","$28","$sp",		\
"$fp","$31",								\
"$f0","$f1","$f2","$f3","$f4","$f5","$f6","$f7","$f8","$f9",		\
"$f10","$f11","$f12","$f13","$f14","$f15","$f16","$f17","$f18","$f19",	\
"$f20","$f21","$f22","$f23","$f24","$f25","$f26","$f27","$f28","$f29",	\
"$f30","$f31"								\
}

#define REG_NAME(reg) (TARGET_NAME_REGS ? reg_names[reg] : reg_numchar[reg])


/* Define results of standard character escape sequences.  */
#define TARGET_BELL 007
#define TARGET_BS 010
#define TARGET_TAB 011
#define TARGET_NEWLINE 012
#define TARGET_VT 013
#define TARGET_FF 014
#define TARGET_CR 015


/* Print an instruction operand X on file FILE.
   CODE is the code from the %-spec that requested printing this operand;
   if `%z3' was used to print operand 3, then CODE is 'z'.
   CODE is used as follows:

    LIST OF PRINT OPERAND CODES:

	'x'  X is CONST_INT, prints 16 bits in hex format.
	'd'  output integer constant in decimal,
	':'  Prints an 'u' if flag -mnofixed-ovfl (for addu vs. add)  */

#define PRINT_OPERAND_PUNCT_VALID_P(CODE)				\
  ((CODE) == ':')

#define PRINT_OPERAND(FILE, X, CODE)					\
{									\
  if ((CODE) == ':')							\
    {									\
      if (TARGET_NOFIXED_OVFL)						\
	fprintf(FILE,"u");						\
    }									\
									\
  else if (GET_CODE (X) == REG)						\
    {									\
      int regnum = REGNO (X);						\
									\
      if (CODE == 'M')							\
	regnum += MOST_SIGNIFICANT_WORD;				\
      else if (CODE == 'L')						\
	regnum += LEAST_SIGNIFICANT_WORD;				\
      else if (CODE == 'D')						\
	regnum++;							\
									\
      fprintf (FILE, "%s",						\
	       ((TARGET_NAME_REGS) ? reg_names : reg_numchar)[regnum]); \
    }									\
									\
  else if (GET_CODE (X) == MEM)						\
    output_address (XEXP (X, 0));					\
									\
  else if (GET_CODE (X) == CONST_DOUBLE)				\
    {									\
      union { double d; int i[2]; } u;					\
      u.i[0] = CONST_DOUBLE_LOW (X);					\
      u.i[1] = CONST_DOUBLE_HIGH (X);					\
      if (GET_MODE (X) == SFmode)					\
	{								\
	  float f;							\
	  f = u.d;							\
	  u.d = f;							\
	}								\
      fprintf (FILE, "%.20e", u.d);					\
    }									\
									\
  else if ((CODE == 'x') && (GET_CODE(X) == CONST_INT))			\
    fprintf(FILE,"0x%x", 0xffff & (INTVAL(X)));				\
									\
  else if ((CODE == 'd') && (GET_CODE(X) == CONST_INT))			\
    fprintf(FILE,"%d", (INTVAL(X)));					\
									\
  else if ((CODE) == 'd')						\
    fatal ("Code d was found & insn was not CONST_INT");		\
									\
  else									\
    output_addr_const (FILE, X);					\
}


/* Print a memory operand whose address is X, on file FILE.  */

#define PRINT_OPERAND_ADDRESS(FILE, ADDR)				\
{									\
  register rtx addr	 = ADDR;					\
  register char **reg_ptr = (TARGET_NAME_REGS) ? reg_names : reg_numchar; \
									\
  switch (GET_CODE (addr))						\
    {									\
    default:								\
      abort_with_insn (addr, "PRINT_OPERAND_ADDRESS, illegal insn #1");	\
      break;								\
									\
    case REG:								\
      fprintf (FILE, "0(%s)", reg_ptr [REGNO (addr)]);			\
      break;								\
									\
    case PLUS:								\
      {									\
	register rtx reg    = (rtx)0;					\
	register rtx offset = (rtx)0;					\
	register rtx arg0   = XEXP (addr, 0);				\
	register rtx arg1   = XEXP (addr, 1);				\
									\
	if (GET_CODE (arg0) == REG)					\
	  {								\
	    reg = arg0;							\
	    offset = arg1;						\
	    if (GET_CODE (offset) == REG)				\
	      abort_with_insn (addr, "PRINT_OPERAND_ADDRESS, 2 regs");	\
	  }								\
	else if (GET_CODE (arg1) == REG)				\
	  {								\
	    reg = arg1;							\
	    offset = arg0;						\
	  }								\
	else if (CONSTANT_P (arg0) && CONSTANT_P (arg1))		\
	  {								\
	    output_addr_const (FILE, addr);				\
	    break;							\
	  }								\
	else								\
	  abort_with_insn (addr, "PRINT_OPERAND_ADDRESS, no regs");	\
									\
	if (!CONSTANT_P (offset))					\
	  abort_with_insn (addr, "PRINT_OPERAND_ADDRESS, illegal insn #2"); \
									\
	output_addr_const (FILE, offset);				\
	fprintf (FILE, "(%s)", reg_ptr [REGNO (reg)]);			\
      }									\
      break;								\
									\
    case LABEL_REF:							\
    case SYMBOL_REF:							\
    case CONST_INT:							\
    case CONST:								\
      output_addr_const (FILE, addr);					\
      break;								\
    }									\
}


/* How to tell the debugger about changes of source files.  Note, the
   mips ECOFF format cannot deal with changes of files inside of
   functions, which means the output of parser generators like bison
   is generally not debuggable without using the -l switch.  Lose,
   lose, lose.  Silicon graphics seems to want all .file's hardwired
   to 1.  */

#ifndef SET_FILE_NUMBER
#define SET_FILE_NUMBER() ++num_source_filenames
#endif

#define ASM_OUTPUT_SOURCE_FILENAME(STREAM, NAME)			\
{									\
  SET_FILE_NUMBER ();							\
  fprintf (STREAM, "\t%s.file\t%d \"%s\"\n",				\
	   (TARGET_GAS || !inside_function) ? "" : "#",			\
	   num_source_filenames, NAME);					\
}

/* This is how to output a note the debugger telling it the line number
   to which the following sequence of instructions corresponds.
   Silicon graphics puts a label after each .loc.  */

#ifndef LABEL_AFTER_LOC
#define LABEL_AFTER_LOC(STREAM)
#endif

#define ASM_OUTPUT_SOURCE_LINE(STREAM, LINE)				\
{									\
  fprintf (STREAM, "\n\t.loc\t%d %d\n", num_source_filenames, LINE);	\
  LABEL_AFTER_LOC (STREAM);						\
}

/* The MIPS implementation uses some labels for it's own purposed.  The
   following lists what labels are created, and are all formed by the
   pattern $L[a-z].*.  The machine independent portion of GCC creates
   labels matching:  $L[A-Z][0-9]+ and $L[0-9]+.

	LM[0-9]+	Sillicon graphics label before each stmt.
	$Lb[0-9]+	Begin blocks for MIPS debug support
	$Ldtable	Beginning of the PIC data table
	$Le[0-9]+	End blocks for MIPS debug support
	$Ls[0-9]+	FP-SP difference if -fomit-frame-pointer  */

/* This is how to output the definition of a user-level label named NAME,
   such as the label on a static function or variable NAME.

   If we are optimizing the gp, remember that this label has been put
   out, so we know not to emit an .extern for it in mips_asm_file_end.
   We use one of the common bits in the IDENTIFIER tree node for this,
   since those bits seem to be unused, and we don't have any method
   of getting the decl nodes from the name.  */

#ifndef COLLECT
#define ASM_OUTPUT_LABEL(STREAM,NAME)					\
do {									\
  assemble_name (STREAM, NAME);						\
  fputs (":\n", STREAM);						\
									\
  if (TARGET_GP_OPT && mips_section_threshold != 0)			\
    {									\
      tree name_tree = get_identifier (NAME);				\
      TREE_ADDRESSABLE (name_tree) = 1;					\
    }									\
} while (0)

#else
#define ASM_OUTPUT_LABEL(STREAM,NAME)					\
do {									\
  assemble_name (STREAM, NAME);						\
  fputs (":\n", STREAM);						\
} while (0)
#endif

/* This is how to output a command to make the user-level label named NAME
   defined for reference from other files.  */

#define ASM_GLOBALIZE_LABEL(STREAM,NAME)				\
  do {									\
    fputs ("\t.globl\t", STREAM);					\
    assemble_name (STREAM, NAME);					\
    fputs ("\n", STREAM);						\
  } while (0)

/* This says how to output an assembler line
   to define a global common symbol.  */

#define ASM_OUTPUT_COMMON(STREAM, NAME, SIZE, ROUNDED)			\
do {									\
  fputs ("\n\t.comm\t", (STREAM));					\
  assemble_name ((STREAM), (NAME));					\
  fprintf ((STREAM), ",%u\n", (ROUNDED));				\
									\
  if (TARGET_GP_OPT && mips_section_threshold != 0)			\
    {									\
      tree name_tree = get_identifier (NAME);				\
      TREE_ADDRESSABLE (name_tree) = 1;					\
    }									\
} while (0)

/* This says how to output an assembler line
   to define a local common symbol.  */

#define ASM_OUTPUT_LOCAL(STREAM, NAME, SIZE, ROUNDED)			\
do {									\
  fputs ("\n\t.lcomm\t", (STREAM));					\
  assemble_name ((STREAM), (NAME));					\
  fprintf ((STREAM), ",%u\n", (ROUNDED));				\
									\
  if (TARGET_GP_OPT && mips_section_threshold != 0)			\
    {									\
      tree name_tree = get_identifier (NAME);				\
      TREE_ADDRESSABLE (name_tree) = 1;					\
    }									\
} while (0)


/* This says how to output an external.  It would be possible not to
   output anything and let undefined symbol become external. However
   the assembler uses length information on externals to allocate in
   data/sdata bss/sbss, thereby saving exec time.  */

#define ASM_OUTPUT_EXTERNAL(STREAM,DECL,NAME) \
  mips_output_external(STREAM,DECL,NAME)

/* This says what to print at the end of the assembly file */
#define ASM_FILE_END(STREAM) mips_asm_file_end(STREAM)


/* This is how to declare a function name.  The actual work of
   emitting the label is moved to function_prologue, so that we can
   get the line number correctly emitted before the .ent directive,
   and after any .file directives.

   Also, switch files if we are optimizing the global pointer.  */

#define ASM_DECLARE_FUNCTION_NAME(STREAM,NAME,DECL)			\
{									\
  extern FILE *asm_out_text_file;					\
  if (TARGET_GP_OPT)							\
    STREAM = asm_out_text_file;						\
									\
  current_function_name = NAME;						\
}

/* This is how to output a reference to a user-level label named NAME.
   `assemble_name' uses this.  */

#define ASM_OUTPUT_LABELREF(STREAM,NAME)				\
  fprintf (STREAM, "%s", NAME)

/* This is how to output an internal numbered label where
   PREFIX is the class of label and NUM is the number within the class.  */

#define ASM_OUTPUT_INTERNAL_LABEL(STREAM,PREFIX,NUM)			\
  fprintf (STREAM, "$%s%d:\n", PREFIX, NUM)

/* This is how to store into the string LABEL
   the symbol_ref name of an internal numbered label where
   PREFIX is the class of label and NUM is the number within the class.
   This is suitable for output with `assemble_name'.  */

#define ASM_GENERATE_INTERNAL_LABEL(LABEL,PREFIX,NUM)			\
  sprintf (LABEL, "*$%s%d", PREFIX, NUM)

/* This is how to output an assembler line defining a `double' constant.  */

#define ASM_OUTPUT_DOUBLE(STREAM,VALUE)					\
{									\
  union { double d; long l[2]; } u2;					\
  u2.d = VALUE;								\
  fprintf (STREAM, "\t.word\t0x%08lx\t\t# %.20g\n\t.word\t0x%08lx\n",	\
	   u2.l[0], u2.d, u2.l[1]);					\
}

/* This is how to output an assembler line defining a `float' constant.  */

#define ASM_OUTPUT_FLOAT(STREAM,VALUE)					\
{									\
  union { float f; long l; } u2;					\
  u2.f = VALUE;								\
  fprintf (STREAM, "\t.word\t0x%08lx\t\t# %.12g\n", u2.l, u2.f);	\
}

/* This is how to output an assembler line defining an `int' constant.  */

#define ASM_OUTPUT_INT(STREAM,VALUE)					\
{									\
  fprintf (STREAM, "\t.word\t");					\
  output_addr_const (STREAM, (VALUE));					\
  fprintf (STREAM, "\n");						\
}

/* Likewise for `char' and `short' constants.  */

#define ASM_OUTPUT_SHORT(STREAM,VALUE)					\
{									\
  fprintf (STREAM, "\t.half\t");					\
  output_addr_const (STREAM, (VALUE));					\
  fprintf (STREAM, "\n");						\
}

#define ASM_OUTPUT_CHAR(STREAM,VALUE)					\
{									\
  fprintf (STREAM, "\t.byte\t");					\
  output_addr_const (STREAM, (VALUE));					\
  fprintf (STREAM, "\n");						\
}

/* This is how to output an assembler line defining an `int' constant,
   which is not in tree format (for collect.c).  */

#define ASM_OUTPUT_INT_CONST(STREAM,VALUE) 				\
  fprintf(STREAM, "\t.word\t%d\n", VALUE)

/* This is how to output an assembler line defining an external/static
   address which is not in tree format (for collect.c).  */

#define ASM_OUTPUT_PTR_INT_SUM(STREAM, NAME, VALUE)			\
do {									\
  fprintf (STREAM, "\t.word\t");					\
  ASM_OUTPUT_LABELREF (STREAM, NAME);					\
  fprintf (STREAM, "+%d\n", VALUE);					\
} while (0)

#define ASM_OUTPUT_LABELREF_AS_INT(STREAM, NAME)			\
do {									\
  fprintf (STREAM, "\t.word\t");					\
  ASM_OUTPUT_LABELREF (STREAM, NAME);					\
  fprintf (STREAM, "\n");						\
} while (0)

/* This is how to output an assembler line for a numeric constant byte.  */

#define ASM_OUTPUT_BYTE(STREAM,VALUE)					\
{									\
  fprintf (STREAM, "\t.byte\t0x%x\n", (VALUE));				\
}

/* This is how to output an element of a case-vector that is absolute.  */

#define ASM_OUTPUT_ADDR_VEC_ELT(STREAM, VALUE)				\
{									\
  fprintf (STREAM, "\t.word\t$L%d\n", VALUE);				\
}

/* This is how to output an element of a case-vector that is relative.
   (We  do not use such vectors,
   but we must define this macro anyway.)  */

#define ASM_OUTPUT_ADDR_DIFF_ELT(STREAM, VALUE, REL)			\
{									\
  fprintf (STREAM, "\t.word\t$L%d-$L%d\n", VALUE, REL);			\
}

/* This is how to emit the initial label for switch statements.  We
   need to put the switch labels somewhere else from the text section,
   because the MIPS assembler gets real confused about line numbers if
   .word's appear in the text section.  */

#define ASM_OUTPUT_CASE_LABEL(STREAM, PREFIX, NUM, JUMPTABLE)		\
{									\
  rdata_section ();							\
  ASM_OUTPUT_ALIGN (STREAM, 2);						\
  ASM_OUTPUT_INTERNAL_LABEL (STREAM, PREFIX, NUM);			\
}

/* Output at the end of a switch's jump table.  */

#define ASM_OUTPUT_CASE_END(STREAM, NUM, INSN)				\
{									\
  text_section ();							\
}

/* This is how to output an assembler line
   that says to advance the location counter
   to a multiple of 2**LOG bytes.  */

#define ASM_OUTPUT_ALIGN(STREAM,LOG)					\
{									\
  int mask = (1 << (LOG)) - 1;						\
  fprintf (STREAM, "\t.align\t%d\n", (LOG));				\
}

/* This is how to output an assembler line to to advance the location
   counter by SIZE bytes.  */
#define ASM_OUTPUT_SKIP(STREAM,SIZE)					\
{									\
  fprintf (STREAM, "\t.space\t%u\n", (SIZE));				\
}

/* This is how to output a string.  */
#define ASM_OUTPUT_ASCII(STREAM, STRING, LEN)				\
do {									\
  register int i, c, len = LEN, cur_pos = 17;				\
  register unsigned char *string = (unsigned char *)STRING;		\
  fprintf (STREAM, "\t.ascii\t\"");					\
  for (i = 0; i < len; i++)						\
    {									\
      register int c = string[i];					\
									\
      switch (c)							\
	{								\
	case '\"':							\
	case '\\':							\
	  putc ('\\', STREAM);						\
	  putc (c, STREAM);						\
	  cur_pos += 2;							\
	  break;							\
									\
	case TARGET_NEWLINE:						\
	  fputs ("\\n", STREAM);					\
	  if (i+1 < len							\
	      && (((c = string[i+1]) >= '\040' && c <= '~')		\
		  || c == TARGET_TAB))					\
	    cur_pos = 32767;		/* break right here */		\
	  else								\
	    cur_pos += 2;						\
	  break;							\
									\
	case TARGET_TAB:						\
	  fputs ("\\t", STREAM);					\
	  cur_pos += 2;							\
	  break;							\
									\
	case TARGET_FF:							\
	  fputs ("\\f", STREAM);					\
	  cur_pos += 2;							\
	  break;							\
									\
	case TARGET_BS:							\
	  fputs ("\\b", STREAM);					\
	  cur_pos += 2;							\
	  break;							\
									\
	case TARGET_CR:							\
	  fputs ("\\r", STREAM);					\
	  cur_pos += 2;							\
	  break;							\
									\
	  default:							\
	  if (c >= ' ' && c < 0177)					\
	    {								\
	      putc (c, STREAM);						\
	      cur_pos++;						\
	    }								\
	  else								\
	    {								\
	      fprintf (STREAM, "\\%03o", c);				\
	      cur_pos += 4;						\
	    }								\
	}								\
									\
      if (cur_pos > 72 && i+1 < len)					\
	{								\
	  cur_pos = 17;							\
	  fprintf (STREAM, "\"\n\t.ascii\t\"");				\
	}								\
    }									\
  fprintf (STREAM, "\"\n");						\
} while (0)

/* Handle certain cpp directives used in header files on sysV.  */
#define SCCS_DIRECTIVE

/* Output #ident as a in the read-only data section.  */
#define ASM_OUTPUT_IDENT(FILE, STRING)					\
{									\
  char *p = STRING;							\
  int size = strlen (p) + 1;						\
  rdata_section ();							\
  assemble_string (p, size);						\
}


/* Output before read-only data.  */

#define TEXT_SECTION_ASM_OP "\t.text"

/* Output before writable data.  */

#define DATA_SECTION_ASM_OP "\t.data"

/* Output before writable  short data.  */

#define SDATA_SECTION_ASM_OP "\t.sdata"

/* Output before read-only data.  */

#define RDATA_SECTION_ASM_OP "\t.rdata"

/* What other sections we support other than the normal .data/.text.  */

#define EXTRA_SECTIONS in_sdata, in_rdata, in_last_p1

/* Define the additional functions to select our additional sections.  */

/* on the MIPS it is not a good idea to put constants in the text
   section, since this defeats the sdata/data mechanism. This is
   especially true when -O is used. In this case an effort is made to
   address with faster (gp) register relative addressing, which can
   only get at sdata and sbss items (there is no stext !!)  However,
   if the constant is too large for sdata, and it's readonly, it
   will go into the .rdata section. */

#define EXTRA_SECTION_FUNCTIONS						\
void									\
sdata_section ()							\
{									\
  if (in_section != in_sdata)						\
    {									\
      fprintf (asm_out_file, "%s\n", SDATA_SECTION_ASM_OP);		\
      in_section = in_sdata;						\
    }									\
}									\
									\
void									\
rdata_section ()							\
{									\
  if (in_section != in_rdata)						\
    {									\
      fprintf (asm_out_file, "%s\n", RDATA_SECTION_ASM_OP);		\
      in_section = in_rdata;						\
    }									\
}

/* Given a decl node or constant node, choose the section to output it in
   and select that section.  */

#define SELECT_SECTION_MODE(MODE,RTX)					\
{									\
  extern int mips_section_threshold;					\
  if ((GET_MODE_SIZE(MODE) / BITS_PER_UNIT) <= mips_section_threshold	\
      && mips_section_threshold > 0)					\
    sdata_section ();							\
  else									\
    rdata_section ();							\
}									\

#define SELECT_SECTION(DECL)						\
{									\
  extern int mips_section_threshold;					\
  if (int_size_in_bytes (TREE_TYPE (DECL)) <= mips_section_threshold	\
      && mips_section_threshold > 0)					\
    sdata_section ();							\
  else if (TREE_CODE (DECL) == STRING_CST)				\
    {									\
      if (flag_writable_strings)					\
	data_section ();						\
      else								\
	rdata_section ();						\
    }									\
  else if (TREE_CODE (DECL) != VAR_DECL)				\
    rdata_section ();							\
  else if (!TREE_READONLY (DECL) || TREE_VOLATILE (DECL))		\
    data_section ();							\
  else									\
    rdata_section ();							\
}


/* Store in OUTPUT a string (made with alloca) containing
   an assembler-name for a local static variable named NAME.
   LABELNO is an integer which is different for each call.  */

#define ASM_FORMAT_PRIVATE_NAME(OUTPUT, NAME, LABELNO)			\
( (OUTPUT) = (char *) alloca (strlen ((NAME)) + 10),			\
  sprintf ((OUTPUT), "%s.%d", (NAME), (LABELNO)))

#define ASM_OUTPUT_REG_POP(STREAM,REGNO)				\
do {									\
  extern char *reg_numchar[];						\
  char **reg_name_ptr = (TARGET_NAME_REGS) ? reg_names : reg_numchar;	\
  fprintf (STREAM, "\tsubu\t%s,%s,4\n\tsw\t%s,0(%s)\n",			\
	   reg_name_ptr[STACK_POINTER_REGNUM],				\
	   reg_name_ptr[STACK_POINTER_REGNUM],				\
	   reg_name_ptr[REGNO],						\
	   reg_name_ptr[STACK_POINTER_REGNUM]);				\
} while (0)

#define ASM_OUTPUT_REG_PUSH(STREAM,REGNO)				\
do {									\
  extern char *reg_numchar[];						\
  char **reg_name_ptr = (TARGET_NAME_REGS) ? reg_names : reg_numchar;	\
  fprintf (STREAM, "\tlw\t%s,0(%s)\n\taddu\t%s,%s,4\n",			\
	   reg_name_ptr[REGNO],						\
	   reg_name_ptr[STACK_POINTER_REGNUM],				\
	   reg_name_ptr[STACK_POINTER_REGNUM],				\
	   reg_name_ptr[STACK_POINTER_REGNUM]);				\
} while (0)


/* Define the parentheses used to group arithmetic operations
   in assembler code.  */

#define ASM_OPEN_PAREN "("
#define ASM_CLOSE_PAREN ")"


/* Tell G++'s collect that MIPS' based ports do not have leading
   underscores.  */

#ifndef NO_UNDERSCORES
#define NO_UNDERSCORES
#endif  NO_UNDERSCORES

/* Tell G++ that we need to run collect.  */

#ifndef USE_COLLECT
#define USE_COLLECT
#endif

#ifndef EXTENDED_COFF
#define EXTENDED_COFF
#endif

/* The following are for collect.c which has it's own idea of
   which macros should be used.  */

#define ASM_INT_OP ".word "
#define ASM_SHORT_OP ".half "
#define ASM_CHAR_OP ".byte "
