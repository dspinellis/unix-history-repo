/* $Header: /home/hyperion/mu/christos/src/sys/tcsh-6.01/RCS/tc.vers.c,v 3.12 1991/12/19 22:34:14 christos Exp $ */
/*
 * tc.vers.c: Version dependent stuff
 */
/*-
 * Copyright (c) 1980, 1991 The Regents of the University of California.
 * All rights reserved.
 *
 * Redistribution and use in source and binary forms, with or without
 * modification, are permitted provided that the following conditions
 * are met:
 * 1. Redistributions of source code must retain the above copyright
 *    notice, this list of conditions and the following disclaimer.
 * 2. Redistributions in binary form must reproduce the above copyright
 *    notice, this list of conditions and the following disclaimer in the
 *    documentation and/or other materials provided with the distribution.
 * 3. All advertising materials mentioning features or use of this software
 *    must display the following acknowledgement:
 *	This product includes software developed by the University of
 *	California, Berkeley and its contributors.
 * 4. Neither the name of the University nor the names of its contributors
 *    may be used to endorse or promote products derived from this software
 *    without specific prior written permission.
 *
 * THIS SOFTWARE IS PROVIDED BY THE REGENTS AND CONTRIBUTORS ``AS IS'' AND
 * ANY EXPRESS OR IMPLIED WARRANTIES, INCLUDING, BUT NOT LIMITED TO, THE
 * IMPLIED WARRANTIES OF MERCHANTABILITY AND FITNESS FOR A PARTICULAR PURPOSE
 * ARE DISCLAIMED.  IN NO EVENT SHALL THE REGENTS OR CONTRIBUTORS BE LIABLE
 * FOR ANY DIRECT, INDIRECT, INCIDENTAL, SPECIAL, EXEMPLARY, OR CONSEQUENTIAL
 * DAMAGES (INCLUDING, BUT NOT LIMITED TO, PROCUREMENT OF SUBSTITUTE GOODS
 * OR SERVICES; LOSS OF USE, DATA, OR PROFITS; OR BUSINESS INTERRUPTION)
 * HOWEVER CAUSED AND ON ANY THEORY OF LIABILITY, WHETHER IN CONTRACT, STRICT
 * LIABILITY, OR TORT (INCLUDING NEGLIGENCE OR OTHERWISE) ARISING IN ANY WAY
 * OUT OF THE USE OF THIS SOFTWARE, EVEN IF ADVISED OF THE POSSIBILITY OF
 * SUCH DAMAGE.
 */
#include "sh.h"

RCSID("$Id: tc.vers.c,v 3.12 1991/12/19 22:34:14 christos Exp $")

#include "patchlevel.h"


Char *
gethosttype()
{
    Char *hosttype;
    
#ifdef HOSTTYPE	/* Override any system determined hosttypes */
    hosttype = str2short(HOSTTYPE);
#else
# if defined(vax) || defined(__vax)
#  define _havehosttype_
    hosttype = str2short("vax");
# endif /* vax || __vax */

# ifdef hp9000 /* hp9000 running MORE/bsd */
#  ifdef hp300
#   define _havehosttype_
    hosttype = str2short("hp300");
#  endif 
#  ifdef hp800
#   define _havehosttype_
    hosttype = str2short("hp800");
#  endif 
#  ifndef _havehosttype_
#   define _havehosttype_
    hosttype = str2short("hp9000");	
#  endif 
# endif /* hp9000 */

# ifdef sun
#  ifdef mc68010
#   define _havehosttype_
    hosttype = str2short("sun2");
#  endif /* mc68010 */
#  ifdef mc68020
#   define _havehosttype_
    hosttype = str2short("sun3");
#  endif /* mc68020 */
#  ifdef sparc
#   define _havehosttype_
    hosttype = str2short("sun4");
#  endif /* sparc */
#  ifdef i386
#   define _havehosttype_
    hosttype = str2short("sun386i");
#  endif /* i386 */
#  ifndef _havehosttype_
#   define _havehosttype_
    hosttype = str2short("sun");	
#  endif 
# endif /* sun */

# ifdef pyr /* pyramid */
#  define _havehosttype_
    hosttype = str2short("pyramid");
# endif /* pyr */

# ifdef ibm032 /* from Jak Kirman */
#  define _havehosttype_
    hosttype = str2short("rt");
# endif /* ibm032 */

# ifdef aiws /* not to be confused with the above */
#  define _havehosttype_
    hosttype = str2short("rtpc");
# endif /* aiws */

# ifdef _AIX370
#  define _havehosttype_
    hosttype = str2short("aix370");
# endif /* _AIX370 */

# ifdef _IBMESA
#  define _havehosttype_
    hosttype = str2short("aixESA");
# endif /* _IBMESA */

# ifdef _IBMR2
#  define _havehosttype_
    hosttype = str2short("rs6000");
# endif /* _IBMR2 */

# ifdef _AIXPS2 /* AIX on a PS/2 */
#  define _havehosttype_
    hosttype = str2short("ps2");
# endif /* _AIXPS2 */

# ifdef OREO
#  define _havehosttype_
    hosttype = str2short("mac2");
# endif /* OREO */

# ifdef hpux
#  if defined(__hp9000s700) && !defined(_havehosttype_)
#   define _havehosttype_
   hosttype = str2short("hp9000s700");
#  endif /* __hp9000s700 */
#  if defined(hp9000s800) && !defined(_havehosttype_)
#   define _havehosttype_
   hosttype = str2short("hp9000s800");	/* maybe "spectrum" */
#  endif /* hp9000s800 */
#  if defined(hp9000s300) && !defined(_havehosttype_)
#   define _havehosttype_
   hosttype = str2short("hp9000s300");
#  endif /* hp9000s300 */
# if defined(hp9000s500) && !defined(_havehosttype_)
#  define _havehosttype_
   hosttype = str2short("hp9000s500");
# endif /* hp9000s500 */
#  ifndef _havehosttype_
#   define _havehosttype_
   hosttype = str2short("hp");
#  endif /* _havehosttype_ */
# endif /* hpux */

# ifdef apollo
#  define _havehosttype_
    hosttype = str2short("apollo");
# endif 

# ifdef u3b20d
#  define _havehosttype_
    hosttype = str2short("att3b20");
# endif /* u3b20d */

# ifdef u3b15
#  define _havehosttype_
    hosttype = str2short("att3b15");
# endif /* u3b15 */

# ifdef u3b5
#  define _havehosttype_
    hosttype = str2short("att3b5");
# endif /* u3b5 */

# ifdef u3b2
#  define _havehosttype_
    hosttype = str2short("att3b2");
# endif /* u3b2 */

#ifdef _MINIX
# define _havehosttype_
# ifdef i386
    hosttype = str2short("minix386");
# else /* minix ? amoeba or mac? */
    hosttype = str2short("minix");
# endif /* i386 */
#endif /* _MINIX */

# if defined(i386) && SVID > 0

#  if !defined(_havehosttype_) && (defined(ISC) || defined(ISC202))
#   define _havehosttype_
    hosttype = str2short("isc386");
#  endif /* !_havehosttype_ && (ISC || ISC202) */

#  if !defined(_havehosttype_) && defined(SCO)
#   define _havehosttype_
    hosttype = str2short("sco386");
#  endif /* !_havehosttype_ && SCO */

#  if !defined(_havehosttype_) && defined(INTEL)
#   define _havehosttype_
    hosttype = str2short("intel386");
#  endif /* !_havehosttype_ && INTEL */

#  ifndef _havehosttype_
#   define _havehosttype_
    hosttype = str2short("i386");
#  endif /* _havehosttype_ */

# endif 

#ifdef UNIXPC
# define _havehosttype_
    hosttype = str2short("unixpc");
#endif /* UNIXPC/att3b1/att7300 */

# ifdef alliant
#  define _havehosttype_
    hosttype = str2short("alliant");	/* for Alliant FX Series */
# endif 

# if defined(i386) && defined(MACH)
#  define _havehosttype_
    hosttype = str2short("i386-mach");
# endif 

# if defined(sequent) || defined(_SEQUENT_)
#  define _havehosttype_
#  ifdef i386
#   ifdef sequent
    hosttype = str2short("symmetry");	/* Sequent Symmetry Dynix/3 */
#    ifndef LOCALSTR
#     define LOCALSTR	" (Dynix/3)"
#    endif /* LOCALSTR */
#   else
    hosttype = str2short("ptx");	/* Sequent Symmetry Dynix/ptx */
#    ifndef LOCALSTR
#     define LOCALSTR	" (Dynix/ptx)"
#    endif /* LOCALSTR */
#   endif 
#  else
    hosttype = str2short("balance");	/* for Sequent Balance Series */
#   ifndef LOCALSTR
#    define LOCALSTR	" (Dynix/3)"
#   endif /* LOCALSTR */
#  endif 
# else /* !sequent */
#  ifdef ns32000
#   define _havehosttype_
#   ifdef CMUCS			/* hack for Mach (in the true spirit of CMU) */
    hosttype = str2short("multimax");
#   else /* CMUCS */
    hosttype = str2short((!access("/Umax.image", F_OK) ? 
			 "multimax" : "ns32000"));
#   endif /* CMUCS */
#  endif /* ns32000 */
# endif /* sequent */

# if defined(convex) || defined(__convex__)
#  define _havehosttype_
    /* From: Brian Allison <uiucdcs!convex!allison@RUTGERS.EDU> */
    hosttype = str2short("convex");
# endif /* convex */

# ifdef butterfly
#  define _havehosttype_
    /* this will work _until_ the bfly with 88000s comes out */
    hosttype = str2short("butterfly");	/* BBN Butterfly 1000 */
# endif /* butterfly */

# ifdef NeXT
#  define _havehosttype_
    hosttype = str2short("next");
# endif /* NeXT */

/* From Kazuhiro Honda <honda@mt.cs.keio.ac.jp> */
# ifdef sony_news
#  define _havehosttype_
#  ifdef mips /* Sony NEWS based on a r3000 */
    hosttype = str2short("news_mips");
#  else
    hosttype = str2short("news");
#  endif 
# endif /* sony_news */

# if defined(mips) || defined(__mips)
#  define _havehosttype_
#  if defined(MIPSEL) || defined(__MIPSEL)
#   if defined(ultrix) || defined(__ultrix)
    hosttype = str2short("decstation");
#   else
    hosttype = str2short("mips");
#   endif /* ultrix || __ultrix */
#  endif /* MIPSEL || __MIPSEL */
#  if defined(MIPSEB) || defined(__MIPSEB)
#   if defined(ultrix) || defined(__ultrix)
    hosttype = str2short("decmips");
#   else
#    ifdef sgi /* sgi iris 4d */
    hosttype = str2short("iris4d");
#    else
#     ifdef sony_news
    hosttype = str2short("news_mips");
#     else
    hosttype = str2short("mips");
#     endif /* sony_news */
#    endif /* sgi */
#   endif /* ultrix || __ultrix */
#  endif /* MIPSEB || __MIPSEB */
# endif /* mips || __mips */

# ifdef m88k
#  define _havehosttype_
    hosttype = str2short("m88k");	/* Motorola 88100 system */
# endif 

# ifdef masscomp			/* Added, DAS DEC-90. */
#  define _havehosttype_
    hosttype = str2short("masscomp");/* masscomp == concurrent */
# endif /* masscomp */

# ifdef GOULD_NP1
#  define _havehosttype_
    hosttype = str2short("gould_np1");
# endif /* GOULD_NP1 */

# ifdef SXA
#  define _havehosttype_
    hosttype = str2short("pfa50");
#  ifdef  _BSDX_
#   ifndef LOCALSTR
#    define LOCALSTR	" (SX/A E60+BSDX)"
#   endif /* LOCALSTR */
#  else
#   ifndef LOCALSTR
#    define LOCALSTR	" (SX/A E60)"
#   endif /* LOCALSTR */
#  endif 
# endif /* PFU/Fujitsu A-xx computer */

# ifdef titan
#  define _havehosttype_
    /* Ken Laprade <laprade@trantor.harris-atd.com> */
    hosttype = str2short("titan");
# endif /* titan */

# ifdef stellar
#  define _havehosttype_
    hosttype = str2short("stellar");
# endif /* stellar */

# ifdef sgi
/* Iris 4D is in the mips section; these are the 68k machines. */
#  ifdef m68000
#   define _havehosttype_
    /* Vince Del Vecchio <vd09@andrew.cmu.edu> */
    hosttype = str2short("iris3d");
#  endif
# endif /* sgi */

# ifdef uts
#  define _havehosttype_
    hosttype = str2short("amdahl");
# endif /* uts */
  
# ifdef OPUS
#  define _havehosttype_
    hosttype = str2short("opus");
# endif /* OPUS */

# ifdef eta10
#  define _havehosttype_
   /* Bruce Woodcock <woodcock@mentor.cc.purdue.edu> */
   hosttype = str2short("eta10");
# endif /* eta10 */

# ifdef cray
#  define _havehosttype_
   hosttype = str2short("cray");
# endif /* cray */

# ifdef NDIX
#  define _havehosttype_
   /* B|rje Josefsson <bj@dc.luth.se> */
   hosttype = str2short("nd500");
# endif /* NDIX */

# ifndef _havehosttype_
#  define _havehosttype_
    /* Default to something reasonable */
    hosttype = str2short("unknown");
# endif 
# undef _havehosttype_
#endif /* HOSTTYPE */
    return hosttype;
} /* end gethosttype */


/* fix_version():
 *	Print a reasonable version string, printing all compile time
 *	options that might affect the user.
 */
void
fix_version()
{
    char    version[BUFSIZE];

#ifdef SHORT_STRINGS
# define SSSTR "8b"
#else
# define SSSTR "7b"
#endif 
#ifdef NLS
# define NLSSTR ",nls"
#else
# define NLSSTR ""
#endif 
#ifdef LOGINFIRST
# define LFSTR ",lf"
#else
# define LFSTR ""
#endif 
#ifdef DOTLAST
# define DLSTR ",dl"
#else
# define DLSTR ""
#endif 
#ifdef VIDEFAULT
# define VISTR ",vi"
#else
# define VISTR ""
#endif 
#ifdef TESLA
# define DTRSTR ",dtr"
#else
# define DTRSTR ""
#endif 
#ifdef KAI
# define BYESTR ",bye"
#else
# define BYESTR ""
#endif 
#ifdef AUTOLOGOUT
# define ALSTR ",al"
#else
# define ALSTR ""
#endif 
#ifdef CSHDIRS
# define DIRSTR ",dir"
#else
# define DIRSTR ""
#endif 
#ifdef KANJI
# define KANSTR ",kan"
#else
# define KANSTR ""
#endif 
#ifdef SYSMALLOC
# define SMSTR	",sm"
#else
# define SMSTR  ""
#endif 
/* if you want your local version to say something */
#ifndef LOCALSTR
# define LOCALSTR ""
#endif /* LOCALSTR */

    xsprintf(version,
	       "tcsh %d.%.2d.%.2d (%s) %s options %s%s%s%s%s%s%s%s%s%s%s%s",
	       REV, VERS, PATCHLEVEL, ORIGIN, DATE,
	       SSSTR, NLSSTR, LFSTR, DLSTR, VISTR, DTRSTR,
	       BYESTR, ALSTR, DIRSTR, KANSTR, SMSTR, LOCALSTR);
    set(STRversion, SAVE(version));
    xsprintf(version, "%d.%.2d.%.2d", REV, VERS, PATCHLEVEL);
    set(STRtcsh, SAVE(version));
}
