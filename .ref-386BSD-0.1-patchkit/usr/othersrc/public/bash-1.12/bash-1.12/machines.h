/* machines.h --
   Included file in the makefile that gets run through Cpp.  This file
   tells which machines have what features based on the unique machine
   identifier present in Cpp. */

/* **************************************************************** */
/*                                                                  */
/*                Global Assumptions (true for most systems).       */
/*                                                                  */
/* **************************************************************** */

/* We make some global assumptions here.  This can be #undef'ed in
   various machine specific entries. */

/* If this file is being processed with Gcc, then the user has Gcc. */
#if defined (__GNUC__)
#  if !defined (HAVE_GCC)
#    define HAVE_GCC
#  endif /* HAVE_GCC */
#endif /* __GNUC__ */

/* Assume that all machines have the getwd () system call.  We unset it
   for USG systems. */
#define HAVE_GETWD

/* Assume that all systems have a working getcwd () call.  We unset it for
   ISC systems. */
#define HAVE_GETCWD

/* Most (but not all) systems have a good, working version of dup2 ().
   For systems that don't have the call (HP/UX), and for systems
   that don't set the open-on-exec flag for the dup'ed file descriptors,
   (Sequents running Dynix, Ultrix), #undef HAVE_DUP2 in the machine
   description. */
#define HAVE_DUP2

/* Every machine that has Gcc has alloca as a builtin in Gcc.  If you are
   compiling Bash without Gcc, then you must have alloca in a library,
   in your C compiler, or be able to assemble or compile the alloca source
   that we ship with Bash. */
#define HAVE_ALLOCA

/* We like most machines to use the GNU Malloc routines supplied in the
   source code because they provide high quality error checking.  On
   some machines, our malloc () cannot be used (because of library
   conflicts, for example), and for those, you should specifically
   #undef USE_GNU_MALLOC in the machine description. */
#define USE_GNU_MALLOC

/* **************************************************************** */
/*								    */
/*			Sun Microsystems Machines	      	    */
/*								    */
/* **************************************************************** */

#if defined (sun)

/* We aren't currently using GNU Malloc on Suns because of a bug in Sun's
   YP which bites us when Sun free ()'s an already free ()'ed address.
   When Sun fixes their YP, we can start using our winning malloc again. */
#  undef USE_GNU_MALLOC

/* Most Sun systems have signal handler functions that are void. */
#  define VOID_SIGHANDLER

#  if defined (mc68010)
#    define sun2
#  endif
#  if defined (mc68020)
#    define sun3
#  endif
#  if defined (sparc)
#    define sun4
#  endif
#  if defined (i386)
#    define Sun386i
#  endif
#if defined (HAVE_SHARED_LIBS)
#  define M_OS SunOS4
#  define SunOS4_SYSDEP_CFLAGS -DBSD_GETPGRP
#else
#  if !defined (sparc)
#     undef VOID_SIGHANDLER
#  endif
#  define M_OS SunOS3
#endif
#endif /* sun */

/* ************************ */
/*			    */
/*	    Sun2	    */
/*			    */
/* ************************ */
#if defined (sun2)
#define M_MACHINE "sun2"
#define HAVE_SYS_SIGLIST
#define HAVE_SETLINEBUF
#define HAVE_VFPRINTF
#define HAVE_GETGROUPS
#endif /* sun2 */

/* ************************ */
/*			    */
/*	    Sun3	    */
/*			    */
/* ************************ */
#if defined (sun3)
#define M_MACHINE "sun3"
#if defined (SunOS4_SYSDEP_CFLAGS)
#  define SYSDEP_CFLAGS SunOS4_SYSDEP_CFLAGS
#endif /* SunOS4 */
#define HAVE_SYS_SIGLIST
#define HAVE_SETLINEBUF
#define HAVE_VFPRINTF
#define HAVE_GETGROUPS
#endif /* sun3 */

/* ************************ */
/*			    */
/*	    Sparc	    */
/*			    */
/* ************************ */
#if defined (sun4)
#define M_MACHINE "sparc"
#if defined (SunOS4_SYSDEP_CFLAGS)
#  define SYSDEP_CFLAGS SunOS4_SYSDEP_CFLAGS
#endif /* SunOS4 */
#define HAVE_SYS_SIGLIST
#define HAVE_SETLINEBUF
#define HAVE_VFPRINTF
#define HAVE_GETGROUPS
#endif /* sparc */

/* ************************ */
/*			    */
/*	    Sun386i	    */
/*			    */
/* ************************ */
#if defined (Sun386i)
#define done386
#define M_MACHINE "Sun386i"
#if defined (SunOS4_SYSDEP_CFLAGS)
#  define SYSDEP_CFLAGS SunOS4_SYSDEP_CFLAGS
#endif /* SunOS4 */
#define HAVE_SYS_SIGLIST
#define HAVE_SETLINEBUF
#define HAVE_VFPRINTF
#define HAVE_GETGROUPS
#endif /* Sun386i */

/* **************************************************************** */
/*								    */
/*			DEC Machines (vax, decstations)   	    */
/*								    */
/* **************************************************************** */

/* ************************ */
/*			    */
/*	    Ultrix	    */
/*			    */
/* ************************ */
#if defined (ultrix)
#if defined (MIPSEL)
#  undef HAVE_ALLOCA_H
#  define M_MACHINE "MIPSEL"
#  if !defined (HAVE_GCC)
#    define MIPS_CFLAGS -Wf,-XNl3072
#  endif
#else /* !MIPSEL */
#  define M_MACHINE "vax"
#endif /* MIPSEL */
#if defined (MIPS_CFLAGS)
#  define SYSDEP_CFLAGS -DBSD_GETPGRP -DTERMIOS_MISSING MIPS_CFLAGS
#else
#  define SYSDEP_CFLAGS -DBSD_GETPGRP -DTERMIOS_MISSING
#endif
#define M_OS Ultrix
#define VOID_SIGHANDLER
#define HAVE_SYS_SIGLIST
#define HAVE_SETLINEBUF
#define HAVE_VFPRINTF
#define HAVE_GETGROUPS
#undef HAVE_DUP2
#endif /* ultrix */

/* ************************ */
/*			    */
/*	VAX 4.3 BSD	    */
/*			    */
/* ************************ */
#if defined (vax) && !defined (ultrix)
#define M_MACHINE "vax"
#define M_OS Bsd
#define HAVE_SETLINEBUF
#define HAVE_SYS_SIGLIST
#define HAVE_GETGROUPS
#endif /* vax && !ultrix */

/* **************************************** */
/*					    */
/*		SGI Iris/IRIX	    	    */
/*					    */
/* **************************************** */
#if defined (sgi)
#  if defined (Irix3)
#    define M_OS "Irix3"
#    define MIPS_CFLAGS -real_frameptr -Wf,-XNl3072
#    undef HAVE_ALLOCA
#  endif /* Irix3 */
#  if defined (Irix4)
#    define M_OS "Irix4"
#    define MIPS_CFLAGS -Wf,-XNl3072
#  endif /* Irix4 */
#define M_MACHINE "sgi"
#define HAVE_GETGROUPS
#define VOID_SIGHANDLER
#define HAVE_SYS_SIGLIST
#define HAVE_SETLINEBUF
#define HAVE_VFPRINTF
#define REQUIRED_LIBRARIES -lsun
#if defined (HAVE_GCC) || !defined (mips)
#  undef MIPS_CFLAGS
#  define MIPS_CFLAGS
#endif /* HAVE_GCC || !mips */
#define SYSDEP_CFLAGS -DUSG -DPGRP_PIPE MIPS_CFLAGS
#undef HAVE_GETWD
#endif  /* sgi */

/* ************************ */
/*			    */
/*	    Sony	    */
/*			    */
/* ************************ */
#if defined (sony)
#if defined (MIPSEB)
#  define M_MACHINE "MIPSEB"
#else
#  define M_MACHINE "sony"
#endif

#if defined (SYSTYPE_SYSV) || defined (USGr4)
#  define M_OS USG
#  undef HAVE_GETWD
#  define HAVE_VFPRINTF
#  define VOID_SIGHANDLER
   /* Alloca requires either Gcc or cc with -lucb. */
#  if !defined (HAVE_GCC)
#    define EXTRA_LIB_SEARCH_PATH /usr/ucblib
#    define REQUIRED_LIBRARIES -lc -lucb
#  endif /* !HAVE_GCC */
#  if defined (MIPSEB)
#    if !defined (HAVE_GCC)
#      define SYSDEP_CFLAGS -Wf,-XNl3072 -DUSGr4
#    else
#      define SYSDEP_CFLAGS -DUSGr4
#    endif /* HAVE_GCC */
#  else
#    define SYSDEP_CFLAGS -DUSGr4
#  endif /* MIPSEB */
#else
#  define M_OS Bsd
#endif /* SYSTYPE_SYSV */
#define HAVE_SYS_SIGLIST
#define HAVE_SETLINEBUF
#define HAVE_GETGROUPS
#endif /* sony */

/* ******************************** */
/*				    */
/*	   MIPS RISC/os		    */
/*				    */
/* ******************************** */

/* Notes on compiling with "make":

   * Place /bsd43/bin in your PATH before /bin.
   * Use `$(CC) -E' instead of `/lib/cpp' in Makefile.
*/
#if defined (mips) && !defined (M_MACHINE)

#if defined (MIPSEB)
#  define M_MACHINE "MIPSEB"
#else
#  if defined (MIPSEL)
#    define M_MACHINE "MIPSEL"
#  else
#    define M_MACHINE "mips"
#  endif /* MIPSEL */
#endif /* MIPSEB */

#define M_OS Bsd

/* Special things for machines from MIPS Co. */
#define mips_CFLAGS -DOPENDIR_NOT_ROBUST -DPGRP_PIPE

#if defined (HAVE_GCC)
#  define SYSDEP_CFLAGS mips_CFLAGS
#else
#  define SYSDEP_CFLAGS -Wf,-XNl3072 -systype bsd43 mips_CFLAGS
#endif /* !HAVE_GCC */
#define HAVE_SYS_SIGLIST
#define HAVE_SETLINEBUF
#define HAVE_VFPRINTF
#define HAVE_GETGROUPS
/* This is actually present but unavailable in the BSD universe? */
#undef HAVE_UNISTD_H
#if !defined (HAVE_RESOURCE)
#  define HAVE_RESOURCE
#endif
/* Appears not to work correctly, so why use it? */
#undef HAVE_WAIT_H
#endif /* mips */

/* ************************ */
/*			    */
/*	  Pyramid	    */
/*			    */
/* ************************ */
#if defined (pyr)
#define M_MACHINE "Pyramid"
#define M_OS Bsd
#if !defined (HAVE_GCC)
#  undef HAVE_ALLOCA
#endif /* HAVE_GCC */
#define HAVE_SYS_SIGLIST
#define HAVE_SETLINEBUF
/* #define HAVE_VFPRINTF */
#define HAVE_GETGROUPS
#endif /* pyr */

/* ************************ */
/*			    */
/*	    IBMRT	    */
/*			    */
/* ************************ */
/* Notes:  Compiling with pcc seems to work better than compiling with
   the hc compiler.  I had problems when compiling with hc with alloca,
   even though the -ma flag was defined.  (bfox) */
#if defined (ibm032)
#define M_MACHINE "IBMRT"
#define M_OS Bsd
#define HAVE_SYS_SIGLIST
#define HAVE_SETLINEBUF
/* Some systems cannot find vfprintf at load time. */
/* #define HAVE_VFPRINTF */
/* Alloca requires either gcc or pcc with -ma in SYSDEP_CFLAGS. */
#if !defined (HAVE_GCC)
#  define SYSDEP_CFLAGS -ma -U__STDC__
#endif
#define HAVE_GETGROUPS
/* #define USE_GNU_TERMCAP */
#endif /* ibm032 */


/* **************************************************************** */
/*								    */
/*	  All Intel 386 Processor Machines are Defined Here!	    */
/*								    */
/* **************************************************************** */

#if defined (i386)

/* 386BSD */
/* Use `$(CC) -E' instead of `/lib/cpp' in Makefile. */
#  if !defined (done386) && defined (__386BSD__)
#    define done386
#    define M_MACHINE "i386"
#    define M_OS Bsd
#    define HAVE_SETLINEBUF
#    define HAVE_SYS_SIGLIST
#    define HAVE_VFPRINTF
#    define HAVE_GETGROUPS
/*#    define SYSDEP_CFLAGS -traditional ? */
#    define HAVE_STRERROR
#    define HAVE_RESOURCE
#    define VOID_SIGHANDLER
#  endif /* i386 && __386BSD__ */

/* **************************************************************** */
/*                                                                  */
/*                       Sequent Machines                           */
/*                                                                  */
/* **************************************************************** */

/* Sequent Symmetry running Dynix/ptx (System V.3.2) */
#  if !defined (done386) && defined (_SEQUENT_)
#    define done386
#    define M_MACHINE "Symmetry"
#    define M_OS USG
#    define SYSDEP_CFLAGS -DUSGr3
#    define HAVE_VFPRINTF
#    define VOID_SIGHANDLER
#    define HAVE_ALLOCA
#    define REQUIRED_LIBRARIES -lPW -lseq
#    undef HAVE_GETWD
#    undef HAVE_RESOURCE
#  endif /* _SEQUENT_ */

/* Sequent Symmetry running Dynix (4.2 BSD) */
#  if !defined (done386) && defined (sequent)
#    define done386
#    define M_MACHINE "Symmetry"
#    define M_OS Bsd
#    define SYSDEP_CFLAGS -DCPCC -DHAVE_SETDTABLESIZE
#    define HAVE_SETLINEBUF
#    define HAVE_SYS_SIGLIST
#    define HAVE_GETGROUPS
#    undef HAVE_DUP2
#  endif /* Sequent 386 */

/* Generic 386 clone running Mach (4.3 BSD-compatible). */
#  if !defined (done386) && defined (MACH)
#    define done386
#    define M_MACHINE "i386"
#    define M_OS Bsd
#    define HAVE_SETLINEBUF
#    define HAVE_SYS_SIGLIST
#    define HAVE_GETGROUPS
#  endif /* i386 && MACH */

/* AIX/PS2 1.2 for the 386. */
#  if !defined (done386) && defined (aixpc)
#    define done386
#    define M_MACHINE "aixpc"
#    define M_OS AIX
#    define HAVE_VFPRINTF
#    define VOID_SIGHANDLER
#    define SYSDEP_CFLAGS -D_BSD
#    define REQUIRED_LIBRARIES -lbsd
#    define HAVE_GETGROUPS
#    if !defined (HAVE_GCC)
#      undef HAVE_ALLOCA
#      undef HAVE_ALLOCA_H
#    endif /* !HAVE_GCC */
#  endif /* AIXPC i386 */

/* System V Release 4 on the 386 */
#  if !defined (done386) && defined (USGr4)
#    define done386
#    define M_MACHINE "i386"
#    define M_OS USG
#    define HAVE_SYS_SIGLIST
#    define HAVE_VFPRINTF
#    define VOID_SIGHANDLER
     /* Alloca requires either Gcc or cc with -lucb. */
#    if !defined (HAVE_GCC)
#      define EXTRA_LIB_SEARCH_PATH /usr/ucblib
#      define REQUIRED_LIBRARIES -lc -lucb
#    endif /* !HAVE_GCC */
#    define HAVE_GETGROUPS
#    define SYSDEP_CFLAGS -DUSGr4
#    undef HAVE_GETWD
#  endif /* System V Release 4 on i386 */

/* 386 box running Interactive Unix 2.2 or greater. */
#  if !defined (done386) && defined (isc386)
#    define done386
#    define M_MACHINE "isc386"
#    define M_OS USG
#    define HAVE_VFPRINTF
#    define VOID_SIGHANDLER
#    define HAVE_GETGROUPS
#    if !defined (HAVE_GCC)
#      define REQUIRED_LIBRARIES -lPW -lc_s
#      define SYSDEP_LDFLAGS -Xp
       /* ISC's wait.h requires lots of POSIX junk.  Forget it. */
#      undef HAVE_WAIT_H
#    endif
#    if defined (NOTDEF)
       /* libcposix.a contains putc, getc, fileno. */
#      define REQUIRED_LIBRARIES -lcposix
#    endif /* NOTDEF */
#    undef HAVE_GETWD
#    undef HAVE_GETCWD
     /* <sys/types.h> uses mode_t, but doesn't define it unless
	_POSIX_SOURCE is defined.  But when _POSIX_SOURCE is defined,
	<signal.h> tries to use pid_t without including <sys/types.h>!
	What a mess.

	ISC's <sys/fcntl.h> doesn't want to define O_NDELAY if __STDC__
	is defined.  We fix that here also.  */
#    if defined (__STDC__) || defined (HAVE_GCC)
#      define SYSDEP_CFLAGS -DUSGr3 -D_POSIX_SOURCE \
	-Dmode_t="unsigned short" -DO_NDELAY=O_NONBLOCK -DPGRP_PIPE
#    else
#      define SYSDEP_CFLAGS -DUSGr3 -D_POSIX_SOURCE -DPGRP_PIPE
#    endif /* __STDC__ || HAVE_GCC */
#  endif /* isc386 */

/* Xenix386 machine. */
#if !defined (done386) && defined (Xenix386)
#  define done386
#  define M_MACHINE "i386"
#  define M_OS Xenix
#  define SYSDEP_CFLAGS -DUSGr3 -DREVERSED_SETVBUF_ARGS
#  define HAVE_VFPRINTF
#  define VOID_SIGHANDLER
#  define ALLOCA_ASM x386-alloca.s
#  define REQUIRED_LIBRARIES -lx
#  undef HAVE_ALLOCA
#endif /* Xenix386 */

/* SCO UNIX 3.2 chip@count.tct.com (Chip Salzenberg) */
#  if !defined (done386) && defined (M_UNIX)
#    define done386
#    define M_MACHINE "i386"
#    define M_OS SCO
#    define SYSDEP_CFLAGS -DUSG -DUSGr3
#    define HAVE_VFPRINTF
#    define VOID_SIGHANDLER
#    define HAVE_GETGROUPS
#    undef HAVE_GETWD
#    undef HAVE_RESOURCE
#  endif /* SCO Unix on 386 boxes. */

/* Assume a generic 386 running Sys V Release 3. */
#  if !defined (done386)
#    define done386
#    define M_MACHINE "i386"
#    define M_OS USG
#    define SYSDEP_CFLAGS -DUSGr3
#    define HAVE_VFPRINTF
#    define VOID_SIGHANDLER
     /* Alloca requires either Gcc or cc with libPW.a */
#    if !defined (HAVE_GCC)
#      define REQUIRED_LIBRARIES -lPW
#    endif /* !HAVE_GCC */
#    undef HAVE_GETWD
#  endif /* Generic i386 Box running Sys V release 3. */
#endif /* All i386 Machines with an `i386' define in cpp. */


/* **************************************************************** */
/*								    */
/*		      Gould 9000 - UTX/32 R2.1A			    */
/*								    */
/* **************************************************************** */
#if defined (gould)		/* Maybe should be GOULD_PN ? */
#define M_MACHINE "gould"
#define M_OS Bsd
#define HAVE_SYS_SIGLIST
#define HAVE_SETLINEBUF
#define HAVE_GETGROUPS
#endif /* gould */

/* ************************ */
/*			    */
/*	    NeXT	    */
/*			    */
/* ************************ */
#if defined (NeXT)
#define M_MACHINE "NeXT"
#define M_OS Bsd
#define HAVE_VFPRINTF
#define HAVE_SYS_SIGLIST
#define HAVE_GETGROUPS
#define HAVE_STRERROR
#define VOID_SIGHANDLER
#undef USE_GNU_MALLOC
#endif

/* ************************ */
/*			    */
/*	hp9000 4.3 BSD	    */
/*			    */
/* ************************ */
#if defined (hp9000) && !defined (hpux)
#define M_MACHINE "hp9000"
#define M_OS Bsd
#undef HAVE_ALLOCA
#define HAVE_SYS_SIGLIST
#define HAVE_SETLINEBUF
#define HAVE_GETGROUPS
#endif /* hp9000 && !hpux */

/* ************************ */
/*			    */
/*	    hpux	    */
/*			    */
/* ************************ */
#if defined (hpux)
#define M_MACHINE "hpux"

/* This is for 6.2+ systems with job control. */
#define M_OS HPUX

/* For HP-UX systems before 6.2, we don't have job control. */
/* #undef M_OS */
/* #define M_OS USG */

/* For HP-UX 7.0, we don't need the -lBSD. */
#if defined (__hpux)
#  define HPUX_70
#endif

#if defined (HPUX_70)
#  define SYSDEP_CFLAGS -DHPUX_70
#  define REQUIRED_LIBRARIES -lPW
#  undef HAVE_GETWD
#else /* Not 7.0 OS version. */
#  define REQUIRED_LIBRARIES -lPW -lBSD
#endif /* __hpux */

#define HAVE_VFPRINTF
#define VOID_SIGHANDLER
#define HAVE_GETGROUPS
#define HAVE_STRERROR
#define SEARCH_LIB_NEEDS_SPACE
#endif /* hpux */

/* ************************ */
/*			    */
/*	    Xenix286	    */
/*			    */
/* ************************ */
#if defined (Xenix286)
#define M_MACHINE "i286"
#define M_OS Xenix
#undef HAVE_ALLOCA
#define REQUIRED_LIBRARIES -lx
#define SYSDEP_CFLAGS -DREVERSED_SETVBUF_ARGS
#endif

/* Xenix 386 box not caught in i386 case above. */
#if !defined (M_MACHINE) && defined (Xenix386)
#  define M_MACHINE "i386"
#  define M_OS Xenix
#  define SYSDEP_CFLAGS -DUSGr3 -DREVERSED_SETVBUF_ARGS
#  define HAVE_VFPRINTF
#  define VOID_SIGHANDLER
#  define ALLOCA_ASM x386-alloca.s
#  define REQUIRED_LIBRARIES -lx
#  undef HAVE_ALLOCA
#endif /* Xenix386 */

/* ************************ */
/*			    */
/*	    convex	    */
/*			    */
/* ************************ */
#if defined (convex)
#define M_MACHINE "convex"
#define M_OS Bsd
#undef HAVE_ALLOCA
#define HAVE_SETLINEBUF
#define HAVE_SYS_SIGLIST
#define HAVE_GETGROUPS
#endif /* convex */

/* ************************ */
/*			    */
/*	    AIX/RT	    */
/*			    */
/* ************************ */
#if defined (aix) && !defined (aixpc)
#define M_MACHINE "AIX"
#define M_OS Bsd
#undef HAVE_ALLOCA
#define HAVE_VFPRINTF
#define HAVE_SYS_SIGLIST
#define VOID_SIGHANDLER
#define HAVE_GETGROUPS
#define USE_TERMCAP_EMULATION
#endif /* AIX */

/* **************************************** */
/*					    */
/*		IBM RISC 6000		    */
/*					    */
/* **************************************** */
#if defined (RISC6000) || defined (_IBMR2)
#define M_MACHINE "RISC6000"
#define M_OS "AIX"
#undef HAVE_GETWD
#undef HAVE_ALLOCA
#define HAVE_SYS_SIGLIST
#define HAVE_SETLINEBUF
#define HAVE_VFPRINTF
#define VOID_SIGHANDLER
#define USE_TERMCAP_EMULATION
#define HAVE_GETGROUPS
#define SYSDEP_CFLAGS -DNLS -DUSG
#undef USE_GNU_MALLOC
#endif /* RISC6000 */

/* **************************************** */
/*					    */
/*	u370 IBM AIX/370		    */
/*					    */
/* **************************************** */
#if defined (u370)
#  if defined (_AIX370)
#    define M_MACHINE "AIX370"
#    define M_OS Bsd
#    define REQUIRED_LIBRARIES -lbsd
#    define HAVE_SETLINEBUF
#    define HAVE_VFPRINTF
#    define SYSDEP_CFLAGS -D_BSD
#    define HAVE_GETGROUPS
#    define USE_TERMCAP_EMULATION
#    undef USE_GNU_MALLOC
#  endif /* _AIX370 */
#  if defined (USGr4) /* System V Release 4 on 370 series architecture. */
#    define M_MACHINE "uxp"
#    define M_OS USG
#    define HAVE_SYS_SIGLIST
#    define HAVE_VPRINTF
#    define USE_GNU_MALLOC
#    define VOID_SIGHANDLER
#    if !defined (HAVE_GCC)
#      undef HAVE_ALLOCA
#      define EXTRA_LIB_SEARCH_PATH /usr/ucblib
#      define REQUIRED_LIBRARIES -lc -lucb
#    endif /* !HAVE_GCC */
#    define HAVE_GETGROUPS
#    define HAVE_RESOURCE
#    define SYSDEP_CFLAGS -DUSGr4
#    endif /* USGr4 */
#endif /* u370 */  

/* ************************ */
/*			    */
/*	    ATT 3B	    */
/*			    */
/* ************************ */
#if defined (att3b) || defined (u3b2)
#if defined (att3b)
#  define M_MACHINE "att3b"
#  define HAVE_SYS_SIGLIST
#else
#  define M_MACHINE "u3b2"
#endif
#define M_OS USG
#undef HAVE_GETWD
#define HAVE_VFPRINTF
#define VOID_SIGHANDLER
/* For an AT&T Unix before V.3 take out the -DUSGr3 */
#define SYSDEP_CFLAGS -DUSGr3
/* Alloca requires either Gcc or cc with libPW.a. */
#if !defined (HAVE_GCC)
#  define REQUIRED_LIBRARIES -lPW
#endif /* !HAVE_GCC */
#endif /* att3b */

/* ************************ */
/*			    */
/*	    ATT 386	    */
/*			    */
/* ************************ */
#if defined (att386)
#define M_MACHINE "att386"
#define M_OS USG
#undef HAVE_GETWD
/* Alloca requires either Gcc or cc with libPW.a. */
#if !defined (HAVE_GCC)
#  define REQUIRED_LIBRARIES -lPW
#endif /* HAVE_GCC */
#define HAVE_SYS_SIGLIST
#define HAVE_VFPRINTF
#define VOID_SIGHANDLER
/* For an AT&T Unix before V.3 take out the -DUSGr3 */
#define SYSDEP_CFLAGS -DUSGr3
#endif /* att386 */

/* ************************ */
/*			    */
/*	    Encore	    */
/*			    */
/* ************************ */
#if defined (MULTIMAX)
#  if defined (n16)
#    define M_MACHINE "Multimax32k"
#  else
#    define M_MACHINE "Multimax"
#  endif /* n16 */
#  if defined (UMAXV)
#    define M_OS USG
#    define REQUIRED_LIBRARIES -lPW
#    define SYSDEP_CFLAGS -DUSGr3
#    define HAVE_VFPRINTF
#    define USE_TERMCAP_EMULATION
#    define VOID_SIGHANDLER
#  else
#    if defined (CMU)
#      define M_OS Mach
#    else
#      define M_OS Bsd
#    endif /* CMU */
#    define HAVE_SYS_SIGLIST
#    define HAVE_STRERROR
#    define HAVE_SETLINEBUF
#  endif /* UMAXV */
#  define HAVE_GETGROUPS
#endif  /* MULTIMAX */

/* ******************************************** */
/*						*/
/*   Encore Series 91 (88K BCS w Job Control)	*/
/*						*/
/* ******************************************** */
#if defined (__m88k) && defined (__UMAXV__)
#define M_MACHINE "Gemini"
#define M_OS USG
#define REQUIRED_LIBRARIES -lPW
#define USE_TERMCAP_EMULATION
#define HAVE_VFPRINTF
#define HAVE_GETGROUPS
#define VOID_SIGHANDLER
#define SYSDEP_CFLAGS -q ext=pcc -D_POSIX_JOB_CONTROL -D_POSIX_VERSION \
		      -Dmalloc=_malloc -Dfree=_free -Drealloc=_realloc
#endif  /* m88k */

/* ******************************************** */
/*						*/
/*    System V Release 4 on the ICL DRS6000     */
/*						*/
/* ******************************************** */
#if defined (drs6000)
#define M_MACHINE "drs6000"
#define M_OS USG
#define SYSDEP_CFLAGS -Xa -DUSGr4 -Dsys_siglist=_sys_siglist
#define EXTRA_LIB_SEARCH_PATH /usr/ucblib
#define SEARCH_LIB_NEEDS_SPACE
#define REQUIRED_LIBRARIES -lc -lucb
#define HAVE_SYS_SIGLIST
#define HAVE_SETLINEBUF
#define HAVE_VFPRINTF
#define HAVE_GETGROUPS
#define HAVE_STRERROR
#define VOID_SIGHANDLER
#undef  HAVE_ALLOCA
#undef	HAVE_ALLOCA_H
#undef	USE_GNU_MALLOC
#endif /* drs6000 */

/* ******************************************** */
/*						*/
/*  System V Release 4 on the Commodore Amiga   */
/*						*/
/* ******************************************** */
#if defined (amiga)
#define M_MACHINE "amiga"
#define M_OS USG
#define SYSDEP_CFLAGS -DUSGr4
#if !defined (HAVE_GCC)
#  define EXTRA_LIB_SEARCH_PATH /usr/ucblib
#  define REQUIRED_LIBRARIES -lc -lucb
#endif /* !HAVE_GCC */
#define HAVE_SYS_SIGLIST
#define HAVE_VFPRINTF
#define VOID_SIGHANDLER
#define HAVE_GETGROUPS
#define HAVE_STRERROR
#undef HAVE_GETWD
#undef USE_GNU_MALLOC
#endif /* System V Release 4 on amiga */

/* ************************ */
/*			    */
/*	    clipper	    */
/*			    */
/* ************************ */
/* This is for the Orion 1/05 (A BSD 4.2 box based on a Clipper processor) */
#if defined (clipper)
#define M_MACHINE "clipper"
#define M_OS Bsd
#define HAVE_SETLINEBUF
#define HAVE_GETGROUPS
#endif  /* clipper */


/* ******************************** */
/*				    */
/*    Integrated Solutions 68020?   */
/*				    */
/* ******************************** */
#if defined (is68k)
#define M_MACHINE "is68k"
#define M_OS Bsd
#undef HAVE_ALLOCA
#define HAVE_SYS_SIGLIST
#define HAVE_SETLINEBUF
#define HAVE_GETGROUPS
#endif /* is68k */

/* ******************************** */
/*				    */
/*	   Omron Luna/Mach 2.5	    */
/*				    */
/* ******************************** */
#if defined (luna88k)
#define M_MACHINE "Luna88k"
#define M_OS Bsd
#define HAVE_SYS_SIGLIST
#define USE_GNU_MALLOC
#define HAVE_SETLINEBUF
#define HAVE_VFPRINTF
#define HAVE_GETGROUPS
#endif  /* luna88k */

/* **************************************** */
/*					    */
/*	    Apollo/SR10.2/BSD4.3	    */
/*					    */
/* **************************************** */
/* This is for the Apollo DN3500 running SR10.2 BSD4.3 */
#if defined (apollo)
#define M_MACHINE "apollo"
#define M_OS Bsd
#define SYSDEP_CFLAGS -D_POSIX_VERSION -D_INCLUDE_BSD_SOURCE -D_INCLUDE_POSIX_SOURCE -DTERMIOS_MISSING -DBSD_GETPGRP -Dpid_t=int
#define HAVE_SYS_SIGLIST
#define HAVE_SETLINEBUF
#define HAVE_GETGROUPS
#endif  /* apollo */

/* ************************ */
/*			    */
/*	DG AViiON	    */
/*			    */
/* ************************ */
/* This is for the DG AViiON box (runs DG/UX with both AT&T & BSD features.) */
#if defined (__DGUX__) || defined (DGUX)
#define M_MACHINE "AViiON"
#define M_OS USG
#undef HAVE_GETWD
#define SYSDEP_CFLAGS -D_DGUX_SOURCE -DPGRP_PIPE /* -D_M88K_SOURCE */
/* DG/UX comes standard with gcc. */
#define HAVE_GCC
#define HAVE_FIXED_INCLUDES
#define HAVE_STRERROR
#define HAVE_GETGROUPS
#define VOID_SIGHANDLER
#undef USE_GNU_MALLOC

/* If you want to build bash for M88K BCS compliance on a DG/UX 5.4
   or above system, do the following:

     - Add -D_M88K_SOURCE to SYSDEP_CFLAGS above.
     - Before running "make" type: "eval `sde-target m88kbcs`" to set
       the software development environment to build BCS objects. */
#endif /* __DGUX__ */

/* ************************ */
/*                          */
/*          XD88            */
/*                          */
/* ************************ */
#if defined (m88k) && !defined (M_MACHNE)
#define M_MACHINE "XD88"
#define M_OS USG
#define HAVE_VPRINTF
#undef HAVE_GETWD
#undef HAVE_ALLOCA
#endif /* XD88 && ! M_MACHINE */

/* ************************ */
/*			    */
/*    Harris Night Hawk	    */
/*			    */
/* ************************ */
/* This is for the Harris Night Hawk family. */
#if defined (_CX_UX)
#if defined (_M88K)
# define M_MACHINE "nh4000"
#else
#  if defined (hcx)
#    define M_MACHINE "nh2000"
#  else
#    if defined (gcx)
#      define M_MACHINE "nh3000"
#    endif
#  endif
#endif
#define M_OS USG
#define SYSDEP_CFLAGS -g -Xa -v -Dgetwd=bash_getwd -D_POSIX_SOURCE -D_POSIX_JOB_CONTROL
#define USE_TERMCAP_EMULATION
#define HAVE_VFPRINTF
#define HAVE_GETGROUPS
#define VOID_SIGHANDLER
#undef USE_GNU_MALLOC
#undef HAVE_GETWD
#endif

/* **************************************** */
/*					    */
/*	    	Tektronix	    	    */
/*					    */
/* **************************************** */
/* These are unproven as yet. */
#if defined (Tek4132)
#define M_MACHINE "Tek4132"
#define M_OS Bsd
#define HAVE_SYS_SIGLIST
#define HAVE_SETLINEBUF
#define HAVE_GETGROUPS
#endif /* Tek4132 */

#if defined (Tek4300)
#define M_MACHINE "Tek4300"
#define M_OS Bsd
#define HAVE_SYS_SIGLIST
#define HAVE_SETLINEBUF
#define HAVE_GETGROUPS
#endif /* Tek4300 */

/* ************************ */
/*			    */
/*     Sequent Balances     */
/*       (Dynix 3.x)	    */
/* ************************ */
#if defined (sequent) && !defined (M_MACHINE)
#define M_MACHINE "Sequent"
#define M_OS Bsd
#undef HAVE_DUP2
#define HAVE_SYS_SIGLIST
#define HAVE_SETLINEBUF
#define HAVE_GETGROUPS
#endif /* sequent */

/* ****************************************** */
/*					      */
/*    NCR Tower 32, System V Release 3	      */
/*					      */
/* ****************************************** */
#if defined (tower32)
#define M_MACHINE "tower32"
#define M_OS USG
#if !defined (HAVE_GCC)
#  define REQUIRED_LIBRARIES -lPW
   /* Disable stack/frame-pointer optimization, incompatible with alloca */
#  define SYSDEP_CFLAGS -DUSGr3 -W2,-aat
#else
#  define SYSDEP_CFLAGS -DUSGr3
#endif /* !HAVE_GCC */
#define HAVE_VFPRINTF
#define USE_TERMCAP_EMULATION
#define VOID_SIGHANDLER
#undef HAVE_GETWD
#endif /* tower32 */

/* ************************ */
/*			    */
/*    Ardent Titan OS v2.2  */
/*			    */
/* ************************ */
#if defined (ardent)
#define M_MACHINE "Ardent Titan"
#define M_OS Bsd
#if !defined (titan)
#  define HAVE_GETGROUPS
#endif
#define HAVE_SYS_SIGLIST
#define HAVE_SETLINEBUF
#define SYSDEP_CFLAGS -43 -w
#define SYSDEP_LDFLAGS -43
#undef HAVE_ALLOCA
#undef USE_GNU_MALLOC
#undef HAVE_VFPRINTF
#endif /* ardent */

/* ************************ */
/*			    */
/*	  Stardent	    */
/*			    */
/* ************************ */
#if defined (stardent) && !defined (M_MACHINE)
#define M_MACHINE "Stardent"
#define M_OS USG
#undef HAVE_GETWD
#undef HAVE_ALLOCA
#define HAVE_SYS_SIGLIST
#define USE_TERMCAP_EMULATION
#define VOID_SIGHANDLER
#endif /* stardent */

/* ************************ */
/*			    */
/*	Concurrent	    */
/*			    */
/* ************************ */
#if defined (concurrent)
/* Use the BSD universe (`universe ucb') */
#define M_MACHINE "Concurrent"
#define M_OS Bsd
#define HAVE_SYS_SIGLIST
#define HAVE_SETLINEBUF
#define HAVE_GETGROUPS
#endif /* concurrent */


/* **************************************************************** */
/*                                                                  */
/*             Honeywell Bull X20 (lele@idea.sublink.org)	    */
/*                                                                  */
/* **************************************************************** */
#if defined (hbullx20)
#define M_MACHINE "Honeywell"
#define M_OS USG
#define SYSDEP_CFLAGS -DUSG
/* Bull x20 needs -lposix for struct dirent. */
#define REQUIRED_LIBRARIES -lPW -lposix
#define HAVE_VFPRINTF
#define VOID_SIGHANDLER
#define USE_TERMCAP_EMULATION
#undef HAVE_GETWD
#endif  /* hbullx20 */

/* ************************ */
/*			    */
/*    Cadmus (tested once)  */
/*			    */
/* ************************ */
#if defined (cadmus) && !defined (M_MACHINE)
#define M_MACHINE "cadmus"
#define M_OS BrainDeath		/* By Far, the worst yet. */
#define SYSDEP_CFLAGS -DUSG
#define HAVE_VFPRINTF
#define VOID_SIGHANDLER
#define USE_TERMCAP_EMULATION
#undef HAVE_GETWD
#undef HAVE_ALLOCA
#endif  /* cadmus */

/* ************************ */
/*			    */
/*	MagicStation	    */
/*			    */
/* ************************ */
#if defined (MagicStation)
#define M_MACHINE "MagicStation"
#define M_OS USG
#define SYSDEP_CFLAGS -DUSGr4
#define HAVE_GETGROUPS
#define HAVE_STRERROR
#define VOID_SIGHANDLER
#undef HAVE_ALLOCA
#undef HAVE_GETWD
#endif /* MagicStation */

/* **************************************************************** */
/*								    */
/*			Generic Entry   			    */
/*								    */
/* **************************************************************** */

/* Use this entry for your machine if it isn't represented here.  It
   is loosely based on a Vax running Bsd. */

#if !defined (M_MACHINE)
#define UNKNOWN_MACHINE
#endif

#ifdef UNKNOWN_MACHINE
#define M_MACHINE "UNKNOWN_MACHINE"
#define M_OS UNKNOWN_OS

/* Required libraries for building on this system. */
#define REQUIRED_LIBRARIES

/* Define HAVE_SYS_SIGLIST if your system has sys_siglist[]. */
#define HAVE_SYS_SIGLIST

/* Undef HAVE_ALLOCA if you are not using Gcc, and neither your library
   nor compiler has a version of alloca ().  In that case, we will use
   our version of alloca () in alloca.c */
/* #undef HAVE_ALLOCA */

/* Undef USE_GNU_MALLOC if there appear to be library conflicts, or if you
   especially desire to use your OS's version of malloc () and friends.  We
   reccommend against this because GNU Malloc has debugging code built in. */
#define USE_GNU_MALLOC

/* Define USE_GNU_TERMCAP if you want to use the GNU termcap library
   instead of your system termcap library. */
/* #define USE_GNU_TERMCAP */

/* Define HAVE_SETLINEBUF if your machine has the setlinebuf ()
   stream library call.  Otherwise, setvbuf () will be used.  If
   neither of them work, you can edit in your own buffer control
   based upon your machines capabilities. */
#define HAVE_SETLINEBUF

/* Define HAVE_VFPRINTF if your machines has the vfprintf () library
   call.  Otherwise, printf will be used.  */
#define HAVE_VFPRINTF

/* Define HAVE_GETGROUPS if your OS allows you to be in multiple
   groups simultaneously by supporting the `getgroups' system call. */
/* #define HAVE_GETGROUPS */

/* Define SYSDEP_CFLAGS to be the flags to cc that make your compiler
   work.  For example, `-ma' on the RT makes alloca () work. */
#define SYSDEP_CFLAGS

/* Define HAVE_STRERROR if your system supplies a definition for strerror ()
   in the C library, or a macro in a header file. */
/* #define HAVE_STRERROR */

/* If your system does not supply /usr/lib/libtermcap.a, but includes
   the termcap routines as a part of the curses library, then define
   this.  This is the case on some System V machines. */
/* #define USE_TERMCAP_EMULATION */

/* Define VOID_SIGHANDLER if your system's signal () returns a pointer to
   a function returning void. */
/* #define VOID_SIGHANDLER */

/* Define EXTRA_LIB_SEARCH_PATH if your required libraries (or standard)
   ones for that matter) are not normally in the ld search path.  For
   example, some machines require /usr/ucblib in the ld search path so
   that they can use -lucb. */
/* #define EXTRA_LIB_SEARCH_PATH /usr/ucblib */

/* Define SEARCH_LIB_NEEDS_SPACE if your native ld requires a space after
   the -L argument, which gives the name of an alternate directory to search
   for libraries specified with -llib.  For example, the HPUX ld requires
   this:
   	-L lib/readline -lreadline
   instead of:
	-Llib/readline -lreadline
 */
/* #define SEARCH_LIB_NEEDS_SPACE */

#endif  /* UNKNOWN_MACHINE */
