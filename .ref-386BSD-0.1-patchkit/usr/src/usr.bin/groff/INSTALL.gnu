Groff has been compiled on a Sun 4 under SunOS 4.0.3 with g++ 1.37.1
and with AT&T C++ 2.0, and on a 386 PC under 386/ix 2.0.1 with g++
1.37.1 using Michael Bloom's GNU COFF patches.  You may encounter
problems on other machines that I cannot anticipate.

If you are using g++, you will need to install the header files from
libg++.  The only other parts of libg++ used by groff are contained in
the files xyzzy.c and gnulib3.c; the libg++.a that I use contains only
xyzzy.o and gnulib3.o.  You don't need xyzzy.o unless you're using GNU
ld.

If you are using g++ 1.37.2 alpha, you'll need the following fix (from
Michael Tiemann):

*** cplus-tree.c~	Sat Jun 23 16:10:41 1990
--- cplus-tree.c	Sat Jun 30 23:45:09 1990
***************
*** 685,688 ****
--- 685,689 ----
  }
  
+ /* Constructor for hashed lists.  */
  tree
  hash_tree_chain (value, chain)
***************
*** 701,705 ****
--- 702,721 ----
  }
  
+ /* Similar, but used for concatenating two lists.  */
  tree
+ hash_chainon (list1, list2)
+      tree list1, list2;
+ {
+   if (list2 == 0)
+     return list1;
+   if (list1 == 0)
+     return list2;
+   if (TREE_CHAIN (list1) == NULL_TREE)
+     return hash_tree_chain (TREE_VALUE (list1), list2);
+   return hash_tree_chain (TREE_VALUE (list1),
+ 			  hash_chainon (TREE_CHAIN (list1), list2));
+ }
+ 
+ tree
  build_decl_list_1 (value)
       tree value;
***************
*** 926,930 ****
      {
        if (DECL_NAME (decl))
! 	return IDENTIFIER_POINTER (DECL_NAME (decl));
        return "((anonymous))";
      }
--- 942,950 ----
      {
        if (DECL_NAME (decl))
! 	{
! 	  if (THIS_NAME_P (DECL_NAME (decl)))
! 	    return "this";
! 	  return IDENTIFIER_POINTER (DECL_NAME (decl));
! 	}
        return "((anonymous))";
      }
*** cplus-parse.y~	Mon Jun  4 23:52:34 1990
--- cplus-parse.y	Sat Jun 30 23:45:09 1990
***************
*** 501,505 ****
  		    error ("no base initializers given following ':'");
  		  setup_vtbl_ptr ();
! 		 }
  	;
  
--- 501,505 ----
  		    error ("no base initializers given following ':'");
  		  setup_vtbl_ptr ();
! 		}
  	;
  
***************
*** 1274,1278 ****
  		{ $$ = hash_tree_chain ($1, $2); }
  	| declmods typespec reserved_declspecs
! 		{ $$ = hash_tree_chain ($2, chainon ($3, $1)); }
  	;
  
--- 1274,1278 ----
  		{ $$ = hash_tree_chain ($1, $2); }
  	| declmods typespec reserved_declspecs
! 		{ $$ = hash_tree_chain ($2, hash_chainon ($3, $1)); }
  	;
  
***************
*** 1319,1323 ****
  		{ $$ = decl_tree_cons (NULL_TREE, $1, $2); }
  	| nonempty_type_quals typespec reserved_typespecquals
! 		{ $$ = decl_tree_cons (NULL_TREE, $2, chainon ($3, $1)); }
  	;
  
--- 1319,1323 ----
  		{ $$ = decl_tree_cons (NULL_TREE, $1, $2); }
  	| nonempty_type_quals typespec reserved_typespecquals
! 		{ $$ = decl_tree_cons (NULL_TREE, $2, hash_chainon ($3, $1)); }
  	;
  

If you're using g++ 1.39 on a sparc you'll probably want to apply the
following fix (from Casper H.S. Dik):

*** config/out-sparc.c.org	Wed Dec 12 03:13:57 1990
--- config/out-sparc.c	Sat Feb 23 23:21:26 1991
***************
*** 908,925 ****
  	else if (GET_CODE (XEXP (operands[1], 0)) == PLUS)
  	  {
  	    rtx inc_reg = XEXP (XEXP (operands[1], 0), 0);
  	    if (inc_reg == frame_pointer_rtx
  		&& GET_CODE (XEXP (XEXP (operands[1], 0), 1)) == REG
! 		&& XEXP (XEXP (operands[1], 0), 0) != frame_pointer_rtx)
  	      inc_reg = XEXP (XEXP (operands[1], 0), 1);
  	    if (inc_reg == frame_pointer_rtx)
  	      {
  		output_asm_insn ("mov %%fp,%%g1", xoperands);
  		inc_reg = gen_rtx (REG, SImode, 1);
  	      }
  	    xoperands[1] = inc_reg;
  	    output_asm_insn ("add 4,%1,%1", xoperands);
! 	    xoperands[1] = operands[1];
  	    output_asm_insn ("ld %1,%0", xoperands);
  	    xoperands[1] = inc_reg;
  	    output_asm_insn ("add -4,%1,%1", xoperands);
--- 908,931 ----
  	else if (GET_CODE (XEXP (operands[1], 0)) == PLUS)
  	  {
  	    rtx inc_reg = XEXP (XEXP (operands[1], 0), 0);
+ 	    rtx from = operands[1];
  	    if (inc_reg == frame_pointer_rtx
  		&& GET_CODE (XEXP (XEXP (operands[1], 0), 1)) == REG
! 		&& XEXP (XEXP (operands[1], 0), 1) != frame_pointer_rtx)
  	      inc_reg = XEXP (XEXP (operands[1], 0), 1);
  	    if (inc_reg == frame_pointer_rtx)
  	      {
  		output_asm_insn ("mov %%fp,%%g1", xoperands);
  		inc_reg = gen_rtx (REG, SImode, 1);
+ 		from = gen_rtx (GET_CODE (operands[1]),
+ 				GET_MODE (operands[1]),
+ 				gen_rtx (PLUS, GET_MODE (XEXP (operands[1], 0)),
+ 					       inc_reg,
+ 					       XEXP (XEXP (operands[1], 0), 1)));
  	      }
  	    xoperands[1] = inc_reg;
  	    output_asm_insn ("add 4,%1,%1", xoperands);
! 	    xoperands[1] = from;
  	    output_asm_insn ("ld %1,%0", xoperands);
  	    xoperands[1] = inc_reg;
  	    output_asm_insn ("add -4,%1,%1", xoperands);
***************
*** 989,1006 ****
  	else if (GET_CODE (XEXP (operands[0], 0)) == PLUS)
  	  {
  	    rtx inc_reg = XEXP (XEXP (operands[0], 0), 0);
  	    if (inc_reg == frame_pointer_rtx
  		&& GET_CODE (XEXP (XEXP (operands[0], 0), 1)) == REG
! 		&& XEXP (XEXP (operands[0], 0), 0) != frame_pointer_rtx)
  	      inc_reg = XEXP (XEXP (operands[0], 0), 1);
  	    if (inc_reg == frame_pointer_rtx)
  	      {
  		output_asm_insn ("mov %%fp,%%g1", xoperands);
  		inc_reg = gen_rtx (REG, SImode, 1);
  	      }
  	    xoperands[0] = inc_reg;
  	    output_asm_insn ("add 4,%0,%0", xoperands);
! 	    xoperands[0] = operands[0];
  	    output_asm_insn ("st %r1,%0", xoperands);
  	    xoperands[0] = inc_reg;
  	    output_asm_insn ("add -4,%0,%0", xoperands);
--- 995,1018 ----
  	else if (GET_CODE (XEXP (operands[0], 0)) == PLUS)
  	  {
  	    rtx inc_reg = XEXP (XEXP (operands[0], 0), 0);
+ 	    rtx to = operands[0];
  	    if (inc_reg == frame_pointer_rtx
  		&& GET_CODE (XEXP (XEXP (operands[0], 0), 1)) == REG
! 		&& XEXP (XEXP (operands[0], 0), 1) != frame_pointer_rtx)
  	      inc_reg = XEXP (XEXP (operands[0], 0), 1);
  	    if (inc_reg == frame_pointer_rtx)
  	      {
  		output_asm_insn ("mov %%fp,%%g1", xoperands);
  		inc_reg = gen_rtx (REG, SImode, 1);
+ 		to = gen_rtx (GET_CODE (operands[0]),
+ 				GET_MODE (operands[0]),
+ 				gen_rtx (PLUS, GET_MODE (XEXP (operands[0], 0)),
+ 					       inc_reg,
+ 					       XEXP (XEXP (operands[0], 0), 1)));
  	      }
  	    xoperands[0] = inc_reg;
  	    output_asm_insn ("add 4,%0,%0", xoperands);
! 	    xoperands[0] = to;
  	    output_asm_insn ("st %r1,%0", xoperands);
  	    xoperands[0] = inc_reg;
  	    output_asm_insn ("add -4,%0,%0", xoperands);

On a Sun 3 and other 68k machines, using libg++ 1.37.0 you will need
to apply the following change to g++-include/math.h:

*** math.h-	Sat Jan  6 14:09:52 1990
--- math.h	Tue Mar 13 02:07:01 1990
***************
*** 32,39 ****
  
  
  #ifdef __HAVE_68881__		/* MC68881/2 Floating-Point Coprocessor */
- #include <math-68881.h>
  extern "C" {			/* fill in what we've left out */
  
  double  acosh(double);
  double  asinh(double);
--- 32,39 ----
  
  
  #ifdef __HAVE_68881__		/* MC68881/2 Floating-Point Coprocessor */
  extern "C" {			/* fill in what we've left out */
+ #include <math-68881.h>
  
  double  acosh(double);
  double  asinh(double);

If you have bison 1.11, you will need to apply the following fix to
bison.simple if you want change any of the grammars:

*** bison.simple.~1~	Fri Aug 10 12:13:41 1990
--- bison.simple	Fri Aug 10 12:24:46 1990
***************
*** 20,26 ****
--- 20,28 ----
  
  
  #ifdef __GNUC__
+ #ifndef alloca
  #define alloca __builtin_alloca
+ #endif /* Not alloca. */
  #else /* Not GNU C.  */
  #if (!defined (__STDC__) && defined (sparc)) || defined (__sparc__)
  #include <alloca.h>
***************
*** 114,123 ****
--- 116,129 ----
  /* This is the most reliable way to avoid incompatibilities
     in available built-in functions on various systems.  */
  static void
+ #ifdef __cplusplus
+ __yy_bcopy (char *from, char *to, int count)
+ #else
  __yy_bcopy (from, to, count)
       char *from;
       char *to;
       int count;
+ #endif
  {
    register char *f = from;
    register char *t = to;
***************
*** 127,133 ****
      *t++ = *f++;
  }
  
! #line 131 "/usr/local/lib/bison.simple"
  int
  yyparse()
  {
--- 133,139 ----
      *t++ = *f++;
  }
  
! #line 137 "/usr/local/lib/bison.simple"
  int
  yyparse()
  {

For gas 1.36 on a Sun 4, the following fix is desirable:

*** sparc.c.~1~	Mon May 21 19:06:18 1990
--- sparc.c	Sat Aug 11 11:09:12 1990
***************
*** 56,65 ****
--- 56,67 ----
  static struct hash_control *op_hash = NULL;
  
  static void s_seg(), s_proc(), s_data1(), s_reserve(), s_common();
+ static void s_sparc_align();
  extern void s_globl(), s_long(), s_short(), s_space(), cons();
  
  pseudo_typeS
  md_pseudo_table[] = {
+     { "align",	    s_sparc_align, 0 },
      { "common",     s_common,   0 },
      { "global",     s_globl,    0 },
      { "half",       cons,       2 },

*** read.c.~1~	Tue Mar  6 21:08:29 1990
--- read.c	Sat Aug 11 11:07:23 1990
***************
*** 175,181 ****
--- 175,183 ----
  potable[] =
  {
    { "abort",	s_abort,	0	},
+ #ifndef SPARC
    { "align",	s_align,	0	},
+ #endif
    { "ascii",	stringer,	0	},
    { "asciz",	stringer,	1	},
    { "byte",	cons,		1	},


On a Sequent Symmetry S27 running Dynix 3.0.17, you'll need to use GNU
make or add 'MAKE=make' to Makefiles which use $(MAKE).  You'll also
need to change

  dev=${GROFF_TYPESETTER:-@DEVICE@}

to

  dev=$GROFF_TYPESETTER

in groff.sh.  You should use gcc to compile xditview.

You should only have to edit the top-level Makefile.  The comments
should make it clear what has to be changed.  If you don't have a
separate directory tree for local manual pages you can make
MAN[157]EXT be l (that's an ell) or n, and MANROOT be /usr/man.  The
changes you make to the top-level Makefile will be propagated to
sub-makes, but this won't happen if you invoke make in the
sub-directories.

You might also need to edit groff.sh.  This is a shell-script that
runs gtroff, an appropriate postprocessor and optionally various
preprocessors.  (Actually, the shell-script is created from groff.sh
by substituting for some variables surrounded by @s).  If your kernel
doesn't understand #!, you will need to arrange for the script to be
run by /bin/sh in some other way.

If you want to use existing troff drivers you should change groff.sh
so that it recognises them.  It is also a good idea to copy over the
dev* directory for the device into a directory that's only searched by
groff (eg /usr/local/lib/groff/font), so that you can take advantage
of the groff extensions to the DESC and font formats.  Groff only uses
the ASCII versions of the device files so you only need copy them.  If
you want to use GNU eqn, it is essential that the font files contain
correct height and depth information.  The format for this information
is described in the groff_font(5) page.  The best way to add this
information is to modify the program that generates the font files.
As a last resort you could try using the program addftinfo: it
attempts to guess plausible heights and depths.  To obtain good
results you would probably have to do more work on addftinfo.

To compile everything, just do a `make'.  If that works, then do a
`make install'.

If you have problems compiling pic/pic.tab.c or eqn/eqn.tab.c, you might
want to try using your system's yacc.  Set YACC=yacc in the top-level
Makefile, and also do

  mv pic/pic.tab.c pic/pic.tab.c.dist
  mv eqn/eqn.tab.c eqn/eqn.tab.c.dist

so that the parsers will be regenerated using yacc (the supplied
*.tab.[ch] files were generated by bison.)

If you want to install xditview, you'll need to do that separately:
change directory to xditview, edit the Makefile, do a make and a make
install. You'll need to be running X11R4.

The dvi files produced by grodvi can use fonts at non-standard
magnifications.  You may need to compile fonts with Metafont at these
magnifications. The CompileFonts script in the dvi/devdvi directory
may help you to do this. (It will take a *long* time.)

If you have problems printing existing troff documents, read the
section on `Incompatbilities' in gtroff(1).  If you have existing
macro packages that are in the habit of omitting the space between a
macro or request and its arguments, it is good idea to produce a
version with spaces so that you can use it with groff (without having
to use the -C flag).  The file macros/fixmacros.sed is a sed script
which will attempt to edit a file of macros so that it can be used
with groff without the -C flag.  If you have the DWB 2.0 mm macros
installed on your machine, you might want to do a `make install.mm';
this will copy the mm macros to groff's macro directory and fix a few
problems that occur when using the mm macros with groff; this requires
the `patch' program.  If the patch in macros/mm.diff is rejected,
carefully apply it by hand.

You can share groff with a friend who has the same type of machine as
you, but does not have a C++ compiler.  First do `make bindist'; this
will create a subdirectory `bindist' containing a set of binaries, a
Makefile and a README.  If you want to strip the binaries, now do a
`make strip' in the bindist directory.  Rename the bindist directory
to something more meaningful, tar it up, and give to your friend along
with the original groff source distribution.  Your friend can then
install groff just by editing the Makefile in the bindist directory
and doing a make there; this will automatically install the non-binary
parts of the groff source distribution as well as the binaries from
the bindist directory.
