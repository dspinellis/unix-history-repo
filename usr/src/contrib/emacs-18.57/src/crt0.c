/* C code startup routine.
   Copyright (C) 1985, 1986 Free Software Foundation, Inc.

    This program is free software; you can redistribute it and/or modify
    it under the terms of the GNU General Public License as published by
    the Free Software Foundation; either version 1, or (at your option)
    any later version.

    This program is distributed in the hope that it will be useful,
    but WITHOUT ANY WARRANTY; without even the implied warranty of
    MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
    GNU General Public License for more details.

    You should have received a copy of the GNU General Public License
    along with this program; if not, write to the Free Software
    Foundation, Inc., 675 Mass Ave, Cambridge, MA 02139, USA.

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
int errno;
#endif

#ifndef DONT_NEED_ENVIRON
char **environ;
#endif

#if defined(orion) || defined(pyramid) || defined(celerity) || defined(ALLIANT) || defined(clipper)

#ifdef ALLIANT
/* _start must initialize _curbrk and _minbrk on the first startup;
   when starting up after dumping, it must initialize them to what they were
   before the dumping, since they are in the shared library and
   are not dumped.  See ADJUST_EXEC_HEADER in m-alliant.h.  */
extern unsigned char *_curbrk, *_minbrk;
extern unsigned char end;
unsigned char *_setbrk = &end;
#endif

#ifndef DUMMIES
#define DUMMIES
#endif

_start (DUMMIES argc, argv, envp)
     int argc;
     char **argv, **envp;
{
#ifdef ALLIANT
  _curbrk = _setbrk;
  _minbrk = _setbrk;
#endif

  environ = envp;

  exit (main (argc, argv, envp));
}

#endif /* orion or pyramid or celerity or alliant or clipper */

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

#ifdef CRT0_DUMMIES

/* Define symbol "start": here; some systems want that symbol.  */
#ifdef DOT_GLOBAL_START
asm("	.text		");
asm("	.globl start	");
asm("	start:		");
#endif /* DOT_GLOBAL_START */

#ifdef NODOT_GLOBAL_START
asm("	text		");
asm("	global start	");
asm("	start:		");
#endif /* NODOT_GLOBAL_START */

_start ()
{
/* On vax, nothing is pushed here  */
/* On sequent, bogus fp is pushed here  */
  start1 ();
}

static
start1 (CRT0_DUMMIES argc, xargv)
     int argc;
     char *xargv;
{
  register char **argv = &xargv;
  environ = argv + argc + 1;

  if ((char *)environ == xargv)
    environ--;
  exit (main (argc, argv, environ));
}
#else /* not CRT0_DUMMIES */

/* "m68k" and "m68000" both stand for m68000 processors,
   but with different program-entry conventions.
   This is a kludge.  Now that the CRT0_DUMMIES mechanism above exists,
   most of these machines could use the vax code above
   with some suitable definition of CRT0_DUMMIES.
   Then the symbol m68k could be flushed.
   But I don't want to risk breaking these machines
   in a version 17 patch release, so that change is being put off.  */

#ifdef m68k			/* Can't do it all from C */
	asm ("	global	_start");
	asm ("	text");
	asm ("_start:");
#ifndef NU
#ifdef STRIDE
	asm ("	comm	havefpu%,2");
#else /* m68k, not STRIDE */
	asm ("  comm	splimit%,4");
#endif /* STRIDE */
	asm ("	global	exit");
	asm ("	text");
#ifdef STRIDE
	asm ("	trap	&3");
	asm ("	mov.w	%d0,havefpu%");
#else /* m68k, not STRIDE */
  	asm ("	mov.l	%d0,splimit%");
#endif /* STRIDE */
#endif /* not NU */
	asm ("	jsr	start1");
	asm ("	mov.l	%d0,(%sp)");
	asm ("	jsr	exit");
	asm ("	mov.l	&1,%d0");	/* d0 = 1 => exit */
	asm ("	trap	&0");
#else /* m68000, not m68k */

#ifdef m68000

#ifdef ISI68K
/* Added by ESM Sun May 24 12:44:02 1987 to get new ISI library to work */
#ifdef BSD4_3
static foo () {
#endif
	asm ("	.globl  is68020");
	asm ("is68020:");
#ifndef BSD4_3
	asm ("	.long   0x00000000");
	asm ("	.long   0xffffffff");
/* End of stuff added by ESM */
#endif
	asm ("	.text");
	asm ("	.globl	__start");
	asm ("__start:");
	asm ("	.word 0");
	asm ("	link	fp,#0");
	asm ("	jbsr	_start1");
	asm ("	unlk	fp");
	asm ("	rts");
#ifdef BSD4_3
      }
#endif
#else /* not ISI68K */

_start ()
{
/* On 68000, _start pushes a6 onto stack  */
  start1 ();
}
#endif /* not ISI68k */
#endif /* m68000 */
#endif /* m68k */

#if defined(m68k) || defined(m68000)
/* ignore takes care of skipping the a6 value pushed in start.  */
static
#if defined(m68k)
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
  exit (main (argc, argv, environ));
}

#endif /* m68k or m68000 */

#endif /* not CRT0_DUMMIES */

#ifdef hp9000s300
int argc_value;
char **argv_value;
#ifdef OLD_HP_ASSEMBLER
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

#else /* new hp assembler */

	asm("	text");
        asm("   global  float_loc");
        asm("   set     float_loc,0xFFFFB000");
 	asm("	global	fpa_loc");
	asm("	set	fpa_loc,0xfff08000");
	asm("	global	__start");
	asm("	global	_exit");
	asm("	global	_main");
	asm("__start:");
	asm("	byte	0,0,0,0");
	asm("	subq.w	&1,%d0");
	asm("	mov.w	%d0,float_soft");
	asm("	mov.w	%d1,flag_68881");
#ifndef HPUX_68010
	asm("	beq.b	skip_float");
	asm("	fmov.l	&0x7400,%fpcr");
/*	asm("	fmov.l	&0x7480,%fpcr"); */
#endif /* HPUX_68010 */
	asm("skip_float:");
	asm("	mov.l	%a0,%d0");
	asm("	add.l	%d0,%d0");
	asm("	subx.w	%d1,%d1");
	asm("	mov.w	%d1,flag_68010");
	asm("	add.l	%d0,%d0");
	asm("	subx.w	%d1,%d1");
	asm("	mov.w	%d1,flag_fpa");
	asm("	tst.l	%d2");
	asm("	ble.b	skip_3");
	asm("	lsl	flag_68881");
	asm("	lsl	flag_fpa");
	asm("skip_3:");
	asm("	mov.l	4(%a7),%d0");
	asm("	beq.b	skip_1");
	asm("	mov.l	%d0,%a0");
	asm("	clr.l	-4(%a0)");
	asm("skip_1:");
	asm("	mov.l	%a7,%a0");
	asm("	subq.l	&8,%a7");
	asm("	mov.l	(%a0),(%a7)");
	asm("	mov.l	(%a0),_argc_value");
	asm("	addq.l	&4,%a0");
	asm("	mov.l	%a0,4(%a7)");
	asm("	mov.l	%a0,_argv_value");
	asm("incr_loop:");
	asm("	tst.l	(%a0)+");
	asm("	bne.b	incr_loop");
	asm("	mov.l	4(%a7),%a1");
	asm("	cmp.l	%a0,(%a1)");
	asm("	blt.b	skip_2");
	asm("	subq.l	&4,%a0");
	asm("skip_2:");
	asm("	mov.l	%a0,8(%a7)");
	asm("	mov.l	%a0,_environ");
	asm("	jsr	_main");
	asm("	addq.l	&8,%a7");
	asm("	mov.l	%d0,-(%a7)");
	asm("	jsr	_exit");
	asm("	mov.w	&1,%d0");
	asm("	trap	&0");
	asm("	comm	float_soft, 4");
	asm("	comm	flag_68881, 4");
	asm("	comm	flag_68010, 4");
	asm("	comm	flag_fpa, 4");

#endif /* new hp assembler */
#endif /* hp9000s300 */

#ifdef GOULD

/* startup code has to be in near text rather
   than fartext as allocated by the C compiler. */
	asm("	.text");
	asm("	.align	2");
	asm("	.globl	__start");
	asm("	.text");
	asm("__start:");
/* setup base register b1 (function base). */
	asm("	.using	b1,.");
	asm("	tpcbr	b1");
/* setup base registers b3 through b7 (data references). */
	asm("	file	basevals,b3");
/* setup base register b2 (stack pointer); it should be
   aligned on a 8-word boundary; but because it is pointing
   to argc, its value should be remembered (in r5). */
	asm("	movw	b2,r4");
	asm("	movw	b2,r5");
	asm("	andw	#~0x1f,r4");
	asm("	movw	r4,b2");
/* allocate stack frame to do some work. */
	asm("	subea	16w,b2");
/* initialize signal catching for UTX/32 1.2; this is
   necessary to make restart from saved image work. */
	asm("	movea	sigcatch,r1");
	asm("	movw	r1,8w[b2]");
	asm("	svc	#1,#150");
/* setup address of argc for start1. */
	asm("	movw	r5,8w[b2]");
	asm("   func	#1,_start1");
	asm("	halt");
/* space for ld to store base register initial values. */
	asm("	.align	5");
	asm("basevals:");
	asm("	.word	__base3,__base4,__base5,__base6,__base7");

static
start1 (xargc)
     int *xargc;
{
  register int	argc;
  register char **argv;

  argc = *xargc;
  argv = (char **)(xargc) + 1;
  environ = argv + argc + 1;

  if (environ == argv)
    environ--;
  exit (main (argc, argv, environ));

}

#endif /* GOULD */

#ifdef elxsi
extern int errno;
extern char **environ;

_start()
{
  register int r;

  errno = 0;
  environ = *(&environ + 8);
  _stdinit();
  r = main(*(&environ + 6), *(&environ + 7), environ);
  exit(r);
  _exit(r);
}
#endif /* elxsi */


#ifdef sparc
asm (".global __start");
asm (".text");
asm ("__start:");
asm ("	mov	0, %fp");
asm ("	ld	[%sp + 64], %o0");
asm ("	add	%sp, 68, %o1");
asm ("	sll	%o0, 2,	%o2");
asm ("	add	%o2, 4,	%o2");
asm ("	add	%o1, %o2, %o2");
asm ("	sethi	%hi(_environ), %o3");
asm ("	st	%o2, [%o3+%lo(_environ)]");
asm ("	andn	%sp, 7,	%sp");
asm ("	call	_main");
asm ("	sub	%sp, 24, %sp");
asm ("	call	__exit");
asm ("	nop");

#endif /* sparc */
