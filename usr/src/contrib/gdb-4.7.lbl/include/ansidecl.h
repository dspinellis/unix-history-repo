/* ANSI and traditional C compatability macros
   Copyright 1991 Free Software Foundation, Inc.
   This file is part of the GNU C Library.

This program is free software; you can redistribute it and/or modify
it under the terms of the GNU General Public License as published by
the Free Software Foundation; either version 2 of the License, or
(at your option) any later version.

This program is distributed in the hope that it will be useful,
but WITHOUT ANY WARRANTY; without even the implied warranty of
MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
GNU General Public License for more details.

You should have received a copy of the GNU General Public License
along with this program; if not, write to the Free Software
Foundation, Inc., 675 Mass Ave, Cambridge, MA 02139, USA.  */

/* ANSI and traditional C compatibility macros

   ANSI C is assumed if __STDC__ is #defined.

   Macro	ANSI C definition	Traditional C definition
   -----	---- - ----------	----------- - ----------
   PTR		`void *'		`char *'
   LONG_DOUBLE	`long double'		`double'
   CONST	`const'			`'
   VOLATILE	`volatile'		`'
   SIGNED	`signed'		`'
   PTRCONST	`void *const'		`char *'

   DEFUN(name, arglist, args)

	Defines function NAME.

	ARGLIST lists the arguments, separated by commas and enclosed in
	parentheses.  ARGLIST becomes the argument list in traditional C.

	ARGS list the arguments with their types.  It becomes a prototype in
	ANSI C, and the type declarations in traditional C.  Arguments should
	be separated with `AND'.  For functions with a variable number of
	arguments, the last thing listed should be `DOTS'.

   DEFUN_VOID(name)

	Defines a function NAME, which takes no arguments.

   EXFUN(name, prototype)

	Is used in an external function declaration.
	In ANSI C it is `NAMEPROTOTYPE' (so PROTOTYPE should be enclosed in
	parentheses).  In traditional C it is `NAME()'.
	For a function that takes no arguments, PROTOTYPE should be `(NOARGS)'.

    For example:
	extern int EXFUN(printf, (CONST char *format DOTS));
	int DEFUN(fprintf, (stream, format),
		  FILE *stream AND CONST char *format DOTS) { ... }
	void DEFUN_VOID(abort) { ... }
*/

#ifndef	_ANSIDECL_H

#define	_ANSIDECL_H	1


/* Every source file includes this file,
   so they will all get the switch for lint.  */
/* LINTLIBRARY */


#ifdef	__STDC__

#define	PTR		void *
#define	PTRCONST	void *CONST
#define	LONG_DOUBLE	long double

#define	AND		,
#define	NOARGS		void
#define	CONST		const
#define	VOLATILE	volatile
#define	SIGNED		signed
#define	DOTS		, ...

#define	EXFUN(name, proto)		name proto
#define	DEFUN(name, arglist, args)	name(args)
#define	DEFUN_VOID(name)		name(NOARGS)

#define PROTO(type, name, arglist) type name arglist

/* We could use the EXFUN macro to handle prototypes, but
   the name is misleading and the result is ugly.  So just define a
   simple macro to handle the parameter lists, as in:

	static int foo PARAMS ((int, char));

   EXFUN would do it like this:

	static int EXFUN (foo, (int, char));

   but the function is not external...  EXFUN should be considered
   obsolete, and new code written to use PARAMS.  */

#define PARAMS(paramlist) paramlist

#else	/* Not ANSI C.  */

#define	PTR		char *
#define	PTRCONST	PTR
#define	LONG_DOUBLE	double

#define	AND		;
#define	NOARGS
#define	CONST
#define	VOLATILE
#define	SIGNED
#define	DOTS

#define	EXFUN(name, proto)		name()
#define	DEFUN(name, arglist, args)	name arglist args;
#define	DEFUN_VOID(name)		name()
#define PROTO(type, name, arglist) type name ()
#define PARAMS(paramlist)		()

#endif	/* ANSI C.  */

#endif	/* ansidecl.h	*/
