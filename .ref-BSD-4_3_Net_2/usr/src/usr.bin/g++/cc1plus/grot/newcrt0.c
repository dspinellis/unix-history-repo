/* C++ code startup routine.
   Copyright (C) 1985, 1986 Free Software Foundation, Inc.
   Hacked by Michael Tiemann (tiemann@mcc.com)

		       NO WARRANTY

  BECAUSE THIS PROGRAM IS LICENSED FREE OF CHARGE, WE PROVIDE ABSOLUTELY
NO WARRANTY, TO THE EXTENT PERMITTED BY APPLICABLE STATE LAW.  EXCEPT
WHEN OTHERWISE STATED IN WRITING, FREE SOFTWARE FOUNDATION, INC,
RICHARD M. STALLMAN AND/OR OTHER PARTIES PROVIDE THIS PROGRAM "AS IS"
WITHOUT WARRANTY OF ANY KIND, EITHER EXPRESSED OR IMPLIED, INCLUDING,
BUT NOT LIMITED TO, THE IMPLIED WARRANTIES OF MERCHANTABILITY AND
FITNESS FOR A PARTICULAR PURPOSE.  THE ENTIRE RISK AS TO THE QUALITY
AND PERFORMANCE OF THE PROGRAM IS WITH YOU.  SHOULD THE PROGRAM PROVE
DEFECTIVE, YOU ASSUME THE COST OF ALL NECESSARY SERVICING, REPAIR OR
CORRECTION.

 IN NO EVENT UNLESS REQUIRED BY APPLICABLE LAW WILL RICHARD M.
STALLMAN, THE FREE SOFTWARE FOUNDATION, INC., AND/OR ANY OTHER PARTY
WHO MAY MODIFY AND REDISTRIBUTE THIS PROGRAM AS PERMITTED BELOW, BE
LIABLE TO YOU FOR DAMAGES, INCLUDING ANY LOST PROFITS, LOST MONIES, OR
OTHER SPECIAL, INCIDENTAL OR CONSEQUENTIAL DAMAGES ARISING OUT OF THE
USE OR INABILITY TO USE (INCLUDING BUT NOT LIMITED TO LOSS OF DATA OR
DATA BEING RENDERED INACCURATE OR LOSSES SUSTAINED BY THIRD PARTIES OR
A FAILURE OF THE PROGRAM TO OPERATE WITH ANY OTHER PROGRAMS) THIS
PROGRAM, EVEN IF YOU HAVE BEEN ADVISED OF THE POSSIBILITY OF SUCH
DAMAGES, OR FOR ANY CLAIM BY ANY OTHER PARTY.

		GENERAL PUBLIC LICENSE TO COPY

  1. You may copy and distribute verbatim copies of this source file
as you receive it, in any medium, provided that you conspicuously and
appropriately publish on each copy a valid copyright notice "Copyright
(C) 1986 Free Software Foundation, Inc."; and include following the
copyright notice a verbatim copy of the above disclaimer of warranty
and of this License.

  2. You may modify your copy or copies of this source file or
any portion of it, and copy and distribute such modifications under
the terms of Paragraph 1 above, provided that you also do the following:

    a) cause the modified files to carry prominent notices stating
    that you changed the files and the date of any change; and

    b) cause the whole of any work that you distribute or publish,
    that in whole or in part contains or is a derivative of this
    program or any part thereof, to be licensed at no charge to all
    third parties on terms identical to those contained in this
    License Agreement (except that you may choose to grant more extensive
    warranty protection to some or all third parties, at your option).

    c) You may charge a distribution fee for the physical act of
    transferring a copy, and you may at your option offer warranty
    protection in exchange for a fee.

Mere aggregation of another unrelated program with this program (or its
derivative) on a volume of a storage or distribution medium does not bring
the other program under the scope of these terms.

  3. You may copy and distribute this program (or a portion or derivative
of it, under Paragraph 2) in object code or executable form under the terms
of Paragraphs 1 and 2 above provided that you also do one of the following:

    a) accompany it with the complete corresponding machine-readable
    source code, which must be distributed under the terms of
    Paragraphs 1 and 2 above; or,

    b) accompany it with a written offer, valid for at least three
    years, to give any third party free (except for a nominal
    shipping charge) a complete machine-readable copy of the
    corresponding source code, to be distributed under the terms of
    Paragraphs 1 and 2 above; or,

    c) accompany it with the information you received as to where the
    corresponding source code may be obtained.  (This alternative is
    allowed only for noncommercial distribution and only if you
    received the program in object code or executable form alone.)

For an executable file, complete source code means all the source code for
all modules it contains; but, as a special exception, it need not include
source code for modules which are standard libraries that accompany the
operating system on which the executable file runs.

  4. You may not copy, sublicense, distribute or transfer this program
except as expressly provided under this License Agreement.  Any attempt
otherwise to copy, sublicense, distribute or transfer this program is void and
your rights to use the program under this License agreement shall be
automatically terminated.  However, parties who have received computer
software programs from you with this License Agreement will not have
their licenses terminated so long as such parties remain in full compliance.

  5. If you wish to incorporate parts of this program into other free
programs whose distribution conditions are different, write to the Free
Software Foundation at 675 Mass Ave, Cambridge, MA 02139.  We have not yet
worked out a simple rule that can be stated here, but we will often permit
this.  We will be guided by the two goals of preserving the free status of
all derivatives of our free software and of promoting the sharing and reuse of
software.


In other words, you are welcome to use, share and improve this program.
You are forbidden to forbid anyone else to use, share and improve
what you give them.   Help stamp out software-hoarding!  */


/* The standard Vax 4.2 Unix crt0.c cannot be used for Emacs
   because it makes `envron' an initialized variable.
   It is easiest to have a special crt0.c on all machines
   though I don't know whether other machines actually need it.  */

/* Also, the standard crt0.c cannot be used for C++, since C++
   guarantees to call global constructors before main () is
   called, and to call global destructors after control has left from
   main ().  */

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

/* On Suns, and possibly on other machines, a routine called
   `on_exit' is used to call termination handling routines.
   If your system provides this feature, define the macro
   ON_EXIT to perform this task.  It is called after a  program
   calls exit(3)  or  returns  normally,  and before its process
   terminates.  The routine named is called as

        (*procp)(status, arg);

   where status is the argument with which exit was called,  or
   zero  if  main returns.  Typically, arg is the address of an
   argument vector to (*procp), but may be  an  integer  value.
   Several  calls  may  be  made to ON_EXIT, specifying several
   termination handlers. The order in which they are called  is
   the reverse of that in which they were given to ON_EXIT.

   Some systems may impose a limit on the number of termination
   handlers which can be specified.  Users near that limit are
   warned that GNU C++ will take one of those slots.

   Note that proper GNU C++ code will not need to rely on this
   mechanism at the application level, because thoughtfully
   constructed classes will take care of this automatically.
   There is no limit to the number of classes which GNU C++
   can destroy.  :-) */

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

#ifndef DONT_NEED_ENVIRON 
char **environ;
#endif

/* These are for GNU C++.  */
#include "crt0.h"
extern void exit ();
extern void __do_global_cleanup ();

#if defined(sequent)
	asm("	.globl _387_flt");
	asm("	.set	_387_flt,0");
#endif

#if defined(orion) || defined(pyramid) || defined(celerity) || defined(ALLIANT)

#ifdef ALLIANT
/* _start must initialize _curbrk and _minbrk on the first startup;
   when starting up after dumping, it must initialize them to what they were
   before the dumping, since they are in the shared library and
   are not dumped.  See ADJUST_EXEC_HEADER in m-alliant.h.  */
extern unsigned char *_curbrk, *_minbrk;
extern unsigned char end;
unsigned char *_setbrk = &end;
#endif

_start (argc, argv, envp)
     int argc;
     char **argv, **envp;
{
#ifdef ALLIANT
  _curbrk = _setbrk;
  _minbrk = _setbrk;
#endif

  environ = envp;

  __do_global_init ();
  exit (main (argc, argv, envp));
}

#endif /* orion or pyramid or celerity or alliant */

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

  __do_global_init ();
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
	asm("	jsr	@___do_global_init");
	asm("	jsr	@_main");
	asm("	adjspb	$-12");
	asm("	movd	r0,tos");
	asm("	jsr	@___do_global_cleanup");
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

  __do_global_init ();
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
	asm ("	data");
	asm ("	even");
	asm ("	global	splimit%");
	asm ("splimit%:");
	asm ("	space 4");
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

#if defined(m68000) || defined(mc68000) || defined(mc68020)
  
#ifdef ISI68K
/* Added by ESM Sun May 24 12:44:02 1987 to get new ISI library to work */
	asm ("	.globl  is68020");
	asm ("is68020:");
	asm ("	.long   0x00000000");
	asm ("	.long   0xffffffff");
/* End of stuff added by ESM */
	asm ("	.text");
	asm ("	.globl	__start");
	asm ("__start:");
	asm ("	.word 0");
	asm ("	link	fp,#0");
	asm ("	jbsr	_start1");
	asm ("	unlk	fp");
	asm ("	rts");
#else /* not ISI68K */
_start ()
{
/* On 68000, _start pushes a6 onto stack  */
  start1 ();
}
#endif /* not ISI68k */
#endif /* m68000 || mc68000 || mc68020 */
#endif /* m68k */

#if defined(sun)
#define ON_EXIT(PROCP, ARG) \
  do { extern void PROCP (); on_exit (PROCP, ARG); } while (0)
#endif

#if defined(m68k) || defined(m68000) || defined(mc68000) || defined(mc68020)
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

#ifdef ON_EXIT

#ifdef sun
  ON_EXIT (_cleanup, 0);
#endif

  ON_EXIT (__do_global_cleanup, 0);

#endif

  __do_global_init ();
  exit (main (argc, argv, environ));
}

#endif /* m68k or m68000 or mc68000 or mc68020 */

#endif /* not CRT0_DUMMIES */

/* Should "hp9000" be completely removed?  */
#if defined(hp9000) || defined(hp9000s300)
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
	asm("	jsr	___do_global_init");
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
	asm("	global	__start");
	asm("	global	_exit");
	asm("	global	_main");
        asm("   global  flag_fpa,flag_68010,fpa_loc,float_loc");
	asm("__start:");
	asm("	byte	0,0,0,0");
	asm("   sub.l   %a6,%a6");
	asm("	subq.w	&1,%d0");
	asm("	mov.w	%d0,float_soft");
	asm("	mov.w	%d1,flag_68881");
	asm("	beq.b	skip_float"); 
	asm("	fmov.l	&0x7400,%fpcr");
	asm("skip_float:");
	asm("   move.l  %a0,%d0");
	asm("   add.l   %d0,%d0");
	asm("   subx.w  %d1,%d1");
	asm("   move.w  %d1,flag_68010");
	asm("   add.l   %d0,%d0");
	asm("   subx.w  %d1,%d1");
	asm("   move.w  %d1,flag_fpa");
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
	asm("	jsr	___do_global_init");
	asm("	jsr	_main");
	asm("	addq.l	&8,%a7");
	asm("	mov.l	%d0,-(%a7)");
	asm("	jsr	_exit");
	asm("	mov.w	&1,%d0");
	asm("	trap	&0");
	asm("	comm	float_soft, 4");
	asm("	comm	flag_68881, 4");
	asm("	comm	flag_68010, 4");
	asm("	comm	flag_fpa,   4");
	asm("   set     float_loc,0xffffb000");
	asm("   set     fpa_loc,0xfff08000");

#endif /* new hp assembler */
#endif /* hp9000 */

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

  __do_global_init ();
  exit (main (argc, argv, environ));
}

#endif /* GOULD */

#ifdef elxsi
extern int errno;
extern char **environ;

_start()
{
  errno = 0;
  environ = *(&environ + 8);
  _stdinit();

  __do_global_init ();
  exit (main(*(&environ + 6), *(&environ + 7), environ));
}
#endif /* elxsi */

#ifdef sparc
asm (".global start");
asm (".text");
asm ("start:");

/* Set up `argc', `argv', and `envp' into local registers.  */
asm ("	mov	0, %fp");
asm ("	ld	[%sp + 64], %l0");
asm ("	add	%sp, 68, %l1");
asm ("	sll	%l0, 2,	%l2");
asm ("	add	%l2, 4,	%l2");
asm ("	add	%l1, %l2, %l2");
asm ("	sethi	%hi(_environ), %l3");
asm ("	st	%l2, [%l3+%lo(_environ)]");

#ifdef ON_EXIT

#ifdef sun
asm ("	set __cleanup,%o0");
asm ("	call _on_exit,0");
asm ("	mov %g0,%o1");
#endif

asm ("	set ___do_global_cleanup,%o0");
asm ("	call _on_exit,0");
asm ("	mov %g0,%o1");

#endif

/* Finish diddling with stack and call `__do_global_init'.  */
asm ("	andn	%sp, 7,	%sp");
asm ("	sub	%sp, 24, %sp");
asm ("	call	___do_global_init");

/* Move `argc', `argv', and `envp' from locals to parameters for `main'.  */
asm ("	mov	%l0,%o0");
asm ("	mov	%l1,%o1");
asm ("	call	_main");
asm ("	mov	%l2,%o2");

#ifndef ON_EXIT
/* Save return value from `main', and call `__do_global_cleanup',
   if necessary.  In any event, get return value from `main' into
   a safe register.  */
asm ("	call	___do_global_cleanup");
#endif
asm ("	mov	%o0,%l0");

#ifdef ON_EXIT
/* Call `exit' or `_exit' with return value from `main'.  */
asm ("	call	_exit");
#else
asm ("	call	__exit");
#endif
asm ("	mov	%l0,%o0");
#endif /* sparc */

#ifndef sun
/* For C++, calls to exit(3) must perform their cleanup duties.
   This means calling destructors on all of the items which
   need to have their destructors called.  After calling these
   destructors, a call is made to _exit (2), which is serious
   business.  */
void
exit (status)
     int status;
{
  __do_global_cleanup ();
#ifdef hp9000s300
  _cleanup ();
#endif
  _exit (status);
}
#endif

static int _end_crt0 ();

void
__do_global_init ()
{
  extern void (*__CTOR_LIST__)();
  register void (**ppf)() = &__CTOR_LIST__;
  register int *pi = (int *)ppf;

#ifdef VIRTUAL_FUNCTION_MASK
  if ((int)_end_crt0 < VINDEX_MAX)
    {
      printf ("virtual function index too large--encroaches text space at address 0x%x\n", _end_crt0);
      abort ();
    }
#else
  /* @@What to do for losing segmented architectures?  */
#endif

  while (*pi++)
    {
      /* Losing PCC cannot handle this statement as (*ppf++)().  Pity.  */
      (*ppf)();
      ppf++;
    }
}

typedef struct dtor_table_entry
{
  struct dtor_table_entry *next;
  void (*fp)();
} dtor_table_entry;

extern dtor_table_entry *__DTOR_LIST__;

void
__do_global_cleanup ()
{
  register int i;

  while (__DTOR_LIST__)
    {
      /* Prevent problems if a destructor should mistakenly call
	 exit() by always consuming one entry per round.  */
      void (*fp)() = __DTOR_LIST__->fp;
      __DTOR_LIST__ = __DTOR_LIST__->next;
      (*fp)();
    }
}

static int
_end_crt0 ()
{
}
