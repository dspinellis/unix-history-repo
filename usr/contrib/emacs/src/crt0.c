/* C code startup routine.
   Copyright (C) 1985 Richard M. Stallman

This program is distributed in the hope that it will be useful,
but without any warranty.  No author or distributor
accepts responsibility to anyone for the consequences of using it
or for whether it serves any particular purpose or works at all,
unless he says so in writing.

   Permission is granted to anyone to distribute verbatim copies
   of this program's source code as received, in any medium, provided that
   the copyright notice, the nonwarraty notice above
   and this permission notice are preserved,
   and that the distributor grants the recipient all rights
   for further redistribution as permitted by this notice,
   and informs him of these rights.

   Permission is granted to distribute modified versions of this
   program's source code, or of portions of it, under the above
   conditions, plus the conditions that all changed files carry
   prominent notices stating who last changed them and that the
   derived material, including anything packaged together with it and
   conceptually functioning as a modification of it rather than an
   application of it, is in its entirety subject to a permission
   notice identical to this one.

   Permission is granted to distribute this program (verbatim or
   as modified) in compiled or executable form, provided verbatim
   redistribution is permitted as stated above for source code, and
    A.  it is accompanied by the corresponding machine-readable
      source code, under the above conditions, or
    B.  it is accompanied by a written offer, with no time limit,
      to distribute the corresponding machine-readable source code,
      under the above conditions, to any one, in return for reimbursement
      of the cost of distribution.   Verbatim redistribution of the
      written offer must be permitted.  Or,
    C.  it is distributed by someone who received only the
      compiled or executable form, and is accompanied by a copy of the
      written offer of source code which he received along with it.

   Permission is granted to distribute this program (verbatim or as modified)
   in executable form as part of a larger system provided that the source
   code for this program, including any modifications used,
   is also distributed or offered as stated in the preceding paragraph.

In other words, you are welcome to use, share and improve this program.
You are forbidden to forbid anyone else to use, share and improve
what you give them.   Help stamp out software-hoarding!  */


/* The standard Vax 4.2 Unix crt0.c cannot be used for Emacs
   because it makes `envron' an initialized variable.
   It is easiest to have a special crt0.c on all machines
   though I don't know whether other machines actually need it.  */

/* On the vax and 68000, in BSD4.2 and USG5.2,
   this is the data format on startup:
  (vax) ap and fp are unpredictable as far as I know; don't use them.
  sp ->  word containing argc
         word pointing to first arg string
	 [word pointing to next arg string]... 0 or more times
	 0
Optionally:
	 [word pointing to environment variable]... 1 or more times
	 ...
	 0
And always:
	 first arg string
	 [next arg string]... 0 or more times
*/

/* On the 16000, at least in the one 4.2 system I know about,
  the initial data format is
  sp ->  word containing argc
         word containing argp
         word pointing to first arg string, and so on as above
*/

#include "config.h"

/*		********  WARNING ********
    Do not insert any data definitions before data_start!
    Since this is the first file linked, the address of the following
    variable should correspond to the start of initialized data space.
    On some systems this is a constant that is independent of the text
    size for shared executables.  On others, it is a function of the
    text size. In short, this seems to be the most portable way to
    discover the start of initialized data space dynamically at runtime,
    for either shared or unshared executables, on either swapping or
    virtual systems.  It only requires that the linker allocate objects
    in the order encountered, a reasonable model for most Unix systems.
    Similarly, note that the address of _start() should be the start
    of text space.   Fred Fish, UniSoft Systems Inc.  */
 
int data_start = 0;
   
#ifdef NEED_ERRNO
int errno = 0;
#endif
 
char **environ;

#if defined(orion) || defined(pyramid) || defined (celerity)

_start (argc, argv, envp)
     int argc;
     char **argv, **envp;
{
  environ = envp;

  exit (main (argc, argv, envp));
}

#endif /* orion or pyramid or celerity */

#if defined (ns16000) && !defined (sequent) && !defined (UMAX)

_start ()
{
/* On 16000, _start pushes fp onto stack */
  start1 ();
}

/* ignore takes care of skipping the fp value pushed in start.  */
static
start1 (ignore, argc, argv)
     int ignore;
     int argc;
     register char **argv;
{
  environ = argv + argc + 1;

  if (environ == *argv)
    environ--;
  exit (main (argc, argv, environ));
}
#endif /* ns16000, not sequent and not UMAX */

#ifdef UMAX
_start()
{
	asm("	exit []			# undo enter");
	asm("	.set	exitsc,1");
	asm("	.set	sigcatchall,0x400");

	asm("	.globl	_exit");
	asm("	.globl	start");
	asm("	.globl	__start");
	asm("	.globl	_main");
	asm("	.globl	_environ");
	asm("	.globl	_sigvec");
	asm("	.globl	sigentry");

	asm("start:");
	asm("	br	.xstart");
	asm("	.org	0x20");
	asm("	.double	p_glbl,0,0xf00000,0");
	asm("	.org	0x30");
	asm(".xstart:");
	asm("	adjspb	$8");
	asm("	movd	8(sp),0(sp)	# argc");
	asm("	addr	12(sp),r0");
	asm("	movd	r0,4(sp)	# argv");
	asm("L1:");
	asm("	movd	r0,r1");
	asm("	addqd	$4,r0");
	asm("	cmpqd	$0,0(r1)	# null args term ?");
	asm("	bne	L1");
	asm("	cmpd	r0,0(4(sp))	# end of 'env' or 'argv' ?");
	asm("	blt	L2");
	asm("	addqd	$-4,r0		# envp's are in list");
	asm("L2:");
	asm("	movd	r0,8(sp)	# env");
	asm("	movd	r0,@_environ	# indir is 0 if no env ; not 0 if env");
	asm("	movqd	$0,tos		# setup intermediate signal handler");
	asm("	addr	@sv,tos");
	asm("	movzwd	$sigcatchall,tos");
	asm("	jsr	@_sigvec");
	asm("	adjspb	$-12");
	asm("	jsr	@_main");
	asm("	adjspb	$-12");
	asm("	movd	r0,tos");
	asm("	jsr	@_exit");
	asm("	adjspb	$-4");
	asm("	addr	@exitsc,r0");
	asm("	svc");
	asm("	.align	4		# sigvec arg");
	asm("sv:");
	asm("	.double	sigentry");
	asm("	.double	0");
	asm("	.double	0");

	asm("	.comm	p_glbl,1");
}
#endif /* UMAX */

#if defined(vax) || defined(tahoe) || defined (sequent) || defined (BOGUS)

#ifdef sequent
#define BOGUS bogus_fp,
#endif /* sequent */

#ifdef vax
#define BOGUS
#endif /* vax */

#ifdef tahoe
#define BOGUS
#endif /* tahoe */

/* Define symbol "start": here; some systems want that symbol.  */
#ifdef tower32
asm("	text		");
asm("	global start	");
#else
asm("	.text		");
asm("	.globl start	");
#endif
asm("	start:		");

_start ()
{
/* On vax, nothing is pushed here  */
/* On sequent, bogus fp is pushed here  */
  start1 ();
}

static
start1 (BOGUS argc, xargv)
     int argc;
     char *xargv;
{
  register char **argv = &xargv;
  environ = argv + argc + 1;

  if ((char *)environ == xargv)
    environ--;
  exit (main (argc, argv, environ));
}
#else /* not vax or tahoe or sequent or BOGUS */

/* "m68k" and "m68000" both stand for m68000 processors,
   but with different program-entry conventions.
   This is a kludge.  Now that the BOGUS mechanism above exists,
   most of these machines could use the vax code above
   with some suitable definition of BOGUS.
   Then the symbol m68k could be flushed.
   But I don't want to risk breaking these machines
   in a version 17 patch release, so that change is being put off.  */

#ifdef m68k			/* Can't do it all from C */
#ifdef STRIDE
	asm ("	comm	havefpu%,2");
#else /* m68k, not STRIDE */
	asm ("	data");
	asm ("	even");
	asm ("	global	splimit%");
	asm ("splimit%:");
	asm ("	space 4");
#endif /* STRIDE */
	asm ("	global	_start");
	asm ("	global	exit");
	asm ("	text");
	asm ("_start:");
#ifdef STRIDE
	asm ("	trap	&3");
	asm ("	mov.w	%d0,havefpu%");
#else /* m68k, not STRIDE */
  	asm ("	mov.l	%d0,splimit%");
#endif /* STRIDE */
	asm ("	jsr	start1");
	asm ("	mov.l	%d0,(%sp)");
	asm ("	jsr	exit");
	asm ("	mov.l	&1,%d0");	/* d0 = 1 => exit */
	asm ("	trap	&0");
#else /* m68000, not m68k */

#ifdef m68000

_start ()
{
/* On 68000, _start pushes a6 onto stack  */
  start1 ();
}
#endif /* m68000 */
#endif /* m68k */

#if defined(m68k) || defined(m68000)
/* ignore takes care of skipping the a6 value pushed in start.  */
static
#ifdef m68k
start1 (argc, xargv)
#else
start1 (ignore, argc, xargv)
#endif
     int argc;
     char *xargv;
{
  register char **argv = &xargv;
  environ = argv + argc + 1;

  if ((char *)environ == xargv)
    environ--;
#ifdef sun2
  hack_sky();
#endif /* sun2 */
  exit (main (argc, argv, environ));
}

#endif /* m68k or m68000 */

#endif /* not vax or tahoe or sequent or BOGUS */

#ifdef hp9000s200
int argc_value;
char **argv_value;

	asm("   text");
	asm("	globl __start");
	asm("	globl _exit");
	asm("	globl _main");
	asm("__start");
	asm("	dc.l	0");
	asm("	subq.w	#0x1,d0");
	asm("	move.w	d0,float_soft");
	asm("	move.l	0x4(a7),d0");
	asm("	beq.s	skip_1");
	asm("	move.l	d0,a0");
	asm("	clr.l	-0x4(a0)");
	asm("skip_1");
	asm("	move.l	a7,a0");
	asm("	subq.l	#0x8,a7");
	asm("	move.l	(a0),(a7)");
	asm("	move.l	(a0),_argc_value");
	asm("	addq.l	#0x4,a0");
	asm("	move.l	a0,0x4(a7)");
	asm("	move.l	a0,_argv_value");
	asm("incr_loop");
	asm("	tst.l	(a0)+");
	asm("	bne.s	incr_loop");
	asm("	move.l	0x4(a7),a1");
	asm("	cmp.l	(a1),a0");
	asm("	blt.s	skip_2");
	asm("	subq.l	#0x4,a0");
	asm("skip_2");
	asm("	move.l	a0,0x8(a7)");
	asm("	move.l	a0,_environ");
	asm("	jsr	_main");
	asm("	addq.l	#0x8,a7");
	asm("	move.l	d0,-(a7)");
	asm("	jsr	_exit");
	asm("	move.w	#0x1,d0");
	asm("	trap	#0x0");
	asm("	comm	float_soft,4");
/* float_soft is allocated in this way because C would
   put an underscore character in its name otherwise. */ 
#endif /* hp9000s200 */
