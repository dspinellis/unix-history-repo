/* Everything you wanted to know about your machine and C compiler,
   but didn't know who to ask.
   Author: Steven Pemberton, CWI, Amsterdam; steven@cwi.nl
   Bugfixes and upgrades gratefully received.

   Name changed to `hard-params' by Richard Stallman, April 89.
   xmalloc function defined, Richard Stallman, June 89.
   Avoid macro in #include, Richard Stallman, Jan 90.
   Undef CHAR_BIT, etc., if defined in stdio.h, Richard Stallman, Aug 90.

   Copyright (c) 1988, 1989 Steven Pemberton, CWI, Amsterdam.
   All rights reserved.

   COMPILING
   With luck and a following wind, just the following will work:
	cc hard-params.c -o hard-params

   If your compiler doesn't support:		add flag:
	signed char (eg pcc)			-DNO_SC
	unsigned char				-DNO_UC
	unsigned short and long			-DNO_UI
	signal(), or setjmp/longjmp()		-DNO_SIG

   Try it first with no flags, and see if you get any errors - you might be
   surprised. (Most non-ANSI compilers need -DNO_SC, though.)
   Some compilers need a -f flag for floating point.

   Don't use any optimisation flags: the program may not work if you do.
   Though "while (a+1.0-a-1.0 == 0.0)" may look like "while(1)" to an
   optimiser, to a floating-point unit there's a world of difference.

   Some compilers offer various flags for different floating point
   modes; it's worth trying all possible combinations of these.

   Add -DID=\"name\" if you want the machine/flags identified in the output.

   SYSTEM DEPENDENCIES
   You may possibly need to add some calls to signal() for other sorts of
   exception on your machine than SIGFPE, and SIGOVER.  See lines beginning
   #ifdef SIGxxx in main() (and communicate the differences to me!).

   If your C preprocessor doesn't have the predefined __FILE__ macro, and
   you want to call this file anything other than hard-params.c, change the
   #define command for __FILE__ accordingly.  If it doesn't accept macro
   names at all in #include lines, order a new C compiler. While you're
   waiting for it to arrive, change the last #include in this file (the
   last but one line) accordingly.

   OUTPUT
   Run without argument to get the information as English text.  If run
   with argument -l (e.g. hard-params -l), output is a series of #define's for
   the ANSI standard limits.h include file, excluding MB_MAX_CHAR.  If run
   with argument -f, output is a series of #define's for the ANSI standard
   float.h include file.  Flag -v gives verbose output: output includes the
   English text above as C comments.  The program exit(0)'s if everything
   went ok, otherwise it exits with a positive number, telling how many
   problems there were.

   VERIFYING THE COMPILER
   If, having produced the float.h and limits.h header files, you want to
   verify that the compiler reads them back correctly (there are a lot of
   boundary cases, of course, like minimum and maximum numbers), you can
   recompile hard-params.c with -DVERIFY set (plus the other flags that you used
   when compiling the version that produced the header files).  This then
   recompiles the program so that it #includes "limits.h" and "float.h",
   and checks that the constants it finds there are the same as the
   constants it produces. Run the resulting program with hard-params -fl.  As of
   this writing, of 21 compiler/flags combinations only 1 compiler has
   passed without error! (The honour goes to 'pcc' on an IBM RT.)

   You can also use this option if your compiler already has both files,
   and you want to confirm that this program produces the right results.

   TROUBLE SHOOTING.
   This program is now quite trustworthy, and suspicious and wrong output
   may well be caused by bugs in the compiler, not in the program (however
   of course, this is not guaranteed, and no responsibility can be
   accepted, etc.)

   The program only works if overflows are ignored by the C system or
   are catchable with signal().

   If the program fails to run to completion (often with the error message
   "Unexpected signal at point x"), this often turns out to be a bug in the
   C compiler's run-time system. Check what was about to be printed, and
   try to narrow the problem down.

   Another possible problem is that you have compiled the program to produce
   loss-of-precision arithmetic traps. The program cannot cope with these,
   and you should re-compile without them. (They should never be the default).

   Make sure you compiled with optimisation turned off.

   Output preceded by *** WARNING: identifies behaviour of the C system
   deemed incorrect by the program. Likely problems are that printf or
   scanf don't cope properly with certain boundary numbers.  For each float
   and double that is printed, the printed value is checked that it is
   correct by using sscanf to read it back.  Care is taken that numbers are
   printed with enough digits to uniquely identify them, and therefore that
   they can be read back identically. If the number read back is different,
   the program prints a warning message. If the two numbers in the warning
   look identical, then printf is more than likely rounding the last
   digit(s) incorrectly.  To put you at ease that the two really are
   different, the bit patterns of the two numbers are also printed.  The
   difference is very likely in the last bit.  Many scanf's read the
   minimum double back as 0.0, and similarly cause overflow when reading
   the maximum double.  The program quite ruthlessly declares all these
   behaviours faulty.

   The warning that "a cast didn't work" refers to cases like this:

      float f;
      #define C 1.234567890123456789
      f= C;
      if (f != (float) C) printf ("Wrong!");

   A faulty compiler will widen f to double and ignore the cast to float,
   and because there is more accuracy in a double than a float, fail to
   recognise that they are the same. In the actual case in point, f and C
   are passed as parameters to a function that discovers they are not equal,
   so it's just possible that the error was in the parameter passing,
   not in the cast (see function Validate()).
   For ANSI C, which has float constants, the error message is "constant has
   wrong precision".

   REPORTING PROBLEMS
   If the program doesn't work for you for any reason that can't be
   narrowed down to a problem in the C compiler, or it has to be changed in
   order to get it to compile, or it produces suspicious output (like a very
   low maximum float, for instance), please mail the problem and an example
   of the incorrect output to steven@cwi.nl or mcvax!steven.uucp, so that
   improvements can be worked into future versions; mcvax/cwi.nl is the
   European backbone, and is connected to uunet and other fine hosts.

   This version of the program is the first to try to catch and diagnose
   bugs in the compiler/run-time system. I would be especially pleased to
   have reports of failures so that I can improve this service.

   I apologise unreservedly for the contorted use of the preprocessor...

   THE SMALL PRINT
   You may copy and distribute verbatim copies of this source file.

   You may modify this source file, and copy and distribute such
   modified versions, provided that you leave the copyright notice
   at the top of the file and also cause the modified file to carry
   prominent notices stating that you changed the files and the date
   of any change; and cause the whole of any work that you distribute
   or publish, that in whole or in part contains or is a derivative of
   this program or any part thereof, to be licensed at no charge to
   all third parties on terms identical to those here.

   If you do have a fix to any problem, please send it to me, so that
   other people can have the benefits.

   While every effort has been taken to make this program as reliable as
   possible, no responsibility can be taken for the correctness of the
   output, or suitability for any particular use.

   ACKNOWLEDGEMENTS
   Many people have given time and ideas to making this program what it is.
   To all of them thanks, and apologies for not mentioning them by name.
*/

#ifndef __FILE__
#define __FILE__ "hard-params.c"
#endif

#ifndef PASS
#define PASS 1
#define PASS1 1
#define VERSION "4.1"

/* Procedure just marks the functions that don't return a result */
#ifdef Procedure
#undef Procedure
#endif
#define Procedure

#define Vprintf if (V) printf

/* stdc is used in tests like if (stdc) */
#ifdef __STDC__
#define stdc 1
#else
#define stdc 0
#endif

/* volatile is used to reduce the chance of optimisation,
   and to prevent variables being put in registers (when setjmp/longjmp
   wouldn't work as we want)  */
#ifndef __STDC__
#define volatile static
#endif

#include <stdio.h>

/* Kludge around the possiblity that <stdio.h> includes <limits.h> */
#ifdef CHAR_BIT
#undef CHAR_BIT
#undef CHAR_MAX
#undef CHAR_MIN
#undef SCHAR_MAX
#undef SCHAR_MIN
#undef UCHAR_MAX
#undef UCHAR_MIN
#endif

#ifdef VERIFY
#include "limits.h"
#include "float.h"
#endif

#ifdef NO_SIG  /* There's no signal(), or setjmp/longjmp() */

	/* Dummy routines instead */
	int lab=1;
	int setjmp(lab) int lab; { return(0); }
	signal(i, p) int i, (*p)(); {}

#else

#include <signal.h>
#include <setjmp.h>

	jmp_buf lab;
	overflow(sig) int sig; { /* what to do on overflow/underflow */
		signal(sig, overflow);
		longjmp(lab, 1);
	}

#endif /*NO_SIG*/

#define Unexpected(place) if (setjmp(lab)!=0) croak(place)

int V= 0,	/* verbose */
    L= 0,	/* produce limits.h */
    F= 0,	/* produce float.h  */
    bugs=0;	/* The number of (possible) bugs in the output */

char co[4], oc[4]; /* Comment starter and ender symbols */

int bits_per_byte; /* the number of bits per unit returned by sizeof() */

#ifdef TEST
/* Set the fp modes on a SUN with 68881 chip, to check that different
   rounding modes etc. get properly detected.
   Compile with additional flag -DTEST, and run with additional parameter
   +hex-number, to set the 68881 mode register to hex-number
*/

/* Bits 0x30 = rounding mode: */
#define ROUND_BITS	0x30
#define TO_NEAREST	0x00
#define TO_ZERO		0x10
#define TO_MINUS_INF	0x20
#define TO_PLUS_INF	0x30 /* The SUN FP user's guide seems to be wrong here */

/* Bits 0xc0 = extended rounding: */
#define EXT_BITS	0xc0
#define ROUND_EXTENDED	0x00
#define ROUND_SINGLE 	0x40
#define ROUND_DOUBLE	0x80

/* Enabled traps: */
#define EXE_INEX1  0x100
#define EXE_INEX2  0x200
#define EXE_DZ	   0x400
#define EXE_UNFL   0x800
#define EXE_OVFL  0x1000
#define EXE_OPERR 0x2000
#define EXE_SNAN  0x4000
#define EXE_BSUN  0x8000

printmode(new) unsigned new; {
	fpmode_(&new);
	printf("New fp mode:\n");
	printf("  Round toward ");
	switch (new & ROUND_BITS) {
	      case TO_NEAREST:   printf("nearest"); break;
	      case TO_ZERO:      printf("zero"); break;
	      case TO_MINUS_INF: printf("minus infinity"); break;
	      case TO_PLUS_INF:  printf("plus infinity"); break;
	      default: printf("???"); break;
	}

	printf("\n  Extended rounding precision: ");

	switch (new & EXT_BITS) {
	      case ROUND_EXTENDED: printf("extended"); break;
	      case ROUND_SINGLE:   printf("single"); break;
	      case ROUND_DOUBLE:   printf("double"); break;
	      default: printf("???"); break;
	}

	printf("\n  Enabled exceptions:");
	if (new & (unsigned) EXE_INEX1) printf(" inex1");
	if (new & (unsigned) EXE_INEX2) printf(" inex2");
	if (new & (unsigned) EXE_DZ)    printf(" dz"); 
	if (new & (unsigned) EXE_UNFL)  printf(" unfl"); 
	if (new & (unsigned) EXE_OVFL)  printf(" ovfl"); 
	if (new & (unsigned) EXE_OPERR) printf(" operr"); 
	if (new & (unsigned) EXE_SNAN)  printf(" snan"); 
	if (new & (unsigned) EXE_BSUN)  printf(" bsun"); 
	printf("\n");
}

int setmode(s) char *s; {
	unsigned mode=0, dig;
	char c;

	while (*s) {
		c= *s++;
		if  (c>='0' && c<='9') dig= c-'0';
		else if (c>='a' && c<='f') dig= c-'a'+10;
		else if (c>='A' && c<='F') dig= c-'A'+10;
		else return 1;
		mode= mode<<4 | dig;
	}
	printmode(mode);
	return 0;
}
#else
int setmode(s) char *s; {
	fprintf(stderr, "Can't set mode: not compiled with TEST\n");
	return(1);
}
#endif

croak(place) int place; {
	printf("*** Unexpected signal at point %d\n", place);
	exit(bugs+1); /* An exit isn't essential here, but avoids loops */
}

/* This is here in case alloca.c is used.  That wants to call this.  */

char *
xmalloc(size) unsigned size; {
	char *malloc();
	char *value = malloc(size);
	if (value == 0) {
		fprintf(stderr, "Virtual memory exceeded\n");
		exit(bugs+1);
	}
	return value;
}

main(argc, argv) int argc; char *argv[]; {
	int dprec, fprec, lprec, basic(), fprop(), dprop(), efprop(), edprop();
	char *malloc();
	unsigned int size;
	long total;
	int i; char *s; int bad;

#ifdef SIGFPE
	signal(SIGFPE, overflow);
#endif
#ifdef SIGOVER
	signal(SIGOVER, overflow);
#endif
/* Add more calls as necessary */

	Unexpected(1);

	bad=0;
	for (i=1; i < argc; i++) {
		s= argv[i];
		if (*s == '-') {
			s++;
			while (*s) {
				switch (*(s++)) {
				      case 'v': V=1; break;
				      case 'l': L=1; break;
				      case 'f': F=1; break;
				      default: bad=1; break;
				}
			}
		} else if (*s == '+') {
			s++;
			bad= setmode(s);
		} else bad= 1;
	}
	if (bad) {
		fprintf(stderr,
			"Usage: %s [-vlf]\n  v=Verbose l=Limits.h f=Float.h\n",
			argv[0]);
		exit(1);
	}
	if (L || F) {
		co[0]= '/'; oc[0]= ' ';
		co[1]= '*'; oc[1]= '*';
		co[2]= ' '; oc[2]= '/';
		co[3]= '\0'; oc[3]= '\0';
	} else {
		co[0]= '\0'; oc[0]= '\0';
		V=1;
	}

	if (L) printf("%slimits.h%s\n", co, oc);
	if (F) printf("%sfloat.h%s\n", co, oc);
#ifdef ID
	printf("%sProduced on %s by hard-params version %s, CWI, Amsterdam%s\n",
	       co, ID, VERSION, oc);
#else
	printf("%sProduced by hard-params version %s, CWI, Amsterdam%s\n",
	       co, VERSION, oc);
#endif

#ifdef VERIFY
	printf("%sVerification phase%s\n", co, oc);
#endif

#ifdef NO_SIG
	Vprintf("%sCompiled without signal(): %s%s\n",
		co,
		"there's nothing that can be done if overflow occurs",
		oc);
#endif
#ifdef NO_SC
	Vprintf("%sCompiled without signed char%s\n", co, oc);
#endif
#ifdef NO_UC
	Vprintf("%Compiled without unsigned char%s\n", co, oc);
#endif
#ifdef NO_UI
	Vprintf("%Compiled without unsigned short or long%s\n", co, oc);
#endif
#ifdef __STDC__
	Vprintf("%sCompiler claims to be ANSI C level %d%s\n",
		co, __STDC__, oc);
#else
	Vprintf("%sCompiler does not claim to be ANSI C%s\n", co, oc);
#endif
	printf("\n");
	bits_per_byte= basic();
	Vprintf("\n");
	if (F||V) {
		fprec= fprop(bits_per_byte);
		dprec= dprop(bits_per_byte);
		lprec= ldprop(bits_per_byte);
		efprop(fprec, dprec, lprec);
		edprop(fprec, dprec, lprec);
		eldprop(fprec, dprec, lprec);
	}
	if (V) {
		/* An extra goody: the approximate amount of data-space */
		/* Allocate store until no more available */
		size=1<<((bits_per_byte*sizeof(int))-2);
		total=0;
		while (size!=0) {
			while (malloc(size)!=(char *)NULL) total+=(size/2);
			size/=2;
		}

		Vprintf("%sMemory mallocatable ~= %ld Kbytes%s\n",
			co, (total+511)/512, oc);
	}
	exit(bugs);
}

Procedure eek_a_bug(problem) char *problem; {
	printf("\n%s*** WARNING: %s%s\n", co, problem, oc);
	bugs++;
}

Procedure i_define(sort, name, val, req) char *sort, *name; long val, req; {
	if (val >= 0) {
		printf("#define %s%s %ld\n", sort, name, val);
	} else {
		printf("#define %s%s (%ld)\n", sort, name, val);
	}
	if (val != req) {
		printf("%s*** Verify failed for above #define!\n", co);
		printf("       Compiler has %ld for value%s\n\n", req, oc);
		bugs++;
	}
	Vprintf("\n");
}

#ifndef NO_UI

#ifdef __STDC__
#define U "U"
#else
#define U ""
#endif

Procedure u_define(sort, name, val, req) char *sort, *name; unsigned long val, req; {
	printf("#define %s%s %lu%s\n", sort, name, val, U);
	if (val != req) {
		printf("%s*** Verify failed for above #define!\n", co);
		printf("       Compiler has %lu for value%s\n\n", req, oc);
		bugs++;
	}
	Vprintf("\n");
}
#endif

/* Long_double is the longest floating point type available: */
#ifdef __STDC__
#define Long_double long double
#else
#define Long_double double
#endif

char *f_rep();

Procedure f_define(sort, name, precision, val, mark)
     char *sort, *name; int precision; Long_double val; char *mark; {
	if (stdc) {
		printf("#define %s%s %s%s\n",
		       sort, name, f_rep(precision, val), mark);
	} else if (*mark == 'F') {
		/* non-ANSI C has no float constants, so cast the constant */
		printf("#define %s%s ((float)%s)\n",
		       sort, name, f_rep(precision, val));
	} else {
		printf("#define %s%s %s\n", sort, name, f_rep(precision, val));
	}
	Vprintf("\n");
}

int floor_log(base, x) int base; Long_double x; { /* return floor(log base(x)) */
	int r=0;
	while (x>=base) { r++; x/=base; }
	return r;
}

int ceil_log(base, x) int base; Long_double x; {
	int r=0;
	while (x>1.0) { r++; x/=base; }
	return r;
}

int exponent(x, fract, exp) Long_double x; double *fract; int *exp; {
	/* Split x into a fraction and a power of ten;
	   returns 0 if x is unusable, 1 otherwise.
	   Only used for error messages about faulty output.
	*/
	int r=0, neg=0;
	Long_double old;
	*fract=0.0; *exp=0;
	if (x<0.0) {
		x= -x;
		neg= 1;
	}
	if (x==0.0) return 1;
	if (x>=10.0) {
		while (x>=10.0) {
			old=x; r++; x/=10.0;
			if (old==x) return 0;
		}
	} else {
		while (x<1.0) {
			old=x; r--; x*=10.0;
			if (old==x) return 0;
		}
	}
	if (neg) *fract= -x;
	else *fract=x;
	*exp=r;
	return 1;
}

#define fabs(x) (((x)<0.0)?(-x):(x))

char *f_rep(precision, val) int precision; Long_double val; {
	static char buf[1024];
	char *f1;
	if (sizeof(double) == sizeof(Long_double)) {
		/* Assume they're the same, and use non-stdc format */
		/* This is for stdc compilers using non-stdc libraries */
		f1= "%.*e";
	} else {
		/* It had better support Le then */
		f1= "%.*Le";
	}
	sprintf(buf, f1, precision, val);
	return buf;
}

Procedure bitpattern(p, size) char *p; int size; {
	char c;
	int i, j;

	for (i=1; i<=size; i++) {
		c= *p;
		p++;
		for (j=bits_per_byte-1; j>=0; j--)
			printf("%c", (c>>j)&1 ? '1' : '0');
		if (i!=size) printf(" ");
	}
}

#define Order(x, px, mode)\
   printf("%s    %s ", co, mode); for (i=0; i<sizeof(x); i++) px[i]= c[i]; \
   for (i=1; i<=sizeof(x); i++) { putchar((char)((x>>(bits_per_byte*(sizeof(x)-i)))&mask)); }\
   printf("%s\n", oc);

Procedure endian(bits_per_byte) int bits_per_byte; {
	/*unsigned*/ short s=0;
	/*unsigned*/ int j=0;
	/*unsigned*/ long l=0;

	char *ps= (char *) &s,
	     *pj= (char *) &j,
	     *pl= (char *) &l,
	     *c= "ABCDEFGHIJKLMNOPQRSTUVWXYZ";
	unsigned int mask, i;

	mask=0;
	for (i=1; i<=bits_per_byte; i++) mask= (mask<<1)|1;

	if (V) {
		printf("%sCharacter order:%s\n", co, oc);
		Order(s, ps, "short:");
		Order(j, pj, "int:  ");
		Order(l, pl, "long: ");
	}
}

#ifdef VERIFY
#ifndef SCHAR_MAX
#define SCHAR_MAX char_max
#define SCHAR_MIN char_min
#endif
#ifndef UCHAR_MAX
#define UCHAR_MAX char_max
#endif
#else
#define CHAR_BIT char_bit
#define CHAR_MAX char_max
#define CHAR_MIN char_min
#define SCHAR_MAX char_max
#define SCHAR_MIN char_min
#define UCHAR_MAX char_max
#endif /* VERIFY */

int cprop() { /* Properties of character */
	volatile char c, char_max, char_min;
	volatile int bits_per_byte, is_signed;
	long char_bit;

	Unexpected(2);

	/* Calculate number of bits per character *************************/
	c=1; bits_per_byte=0;
	do { c=c<<1; bits_per_byte++; } while(c!=0);
	c= (char)(-1);
	if (((int)c)<0) is_signed=1;
	else is_signed=0;
	Vprintf("%sChar = %d bits, %ssigned%s\n",
		co, (int)sizeof(c)*bits_per_byte, (is_signed?"":"un"), oc);
	char_bit=(long)(sizeof(c)*bits_per_byte);
	if (L) i_define("CHAR", "_BIT", char_bit, (long) CHAR_BIT);

	c=0; char_max=0;
	c++;
	if (setjmp(lab)==0) { /* Yields char_max */
		while (c>char_max) {
			char_max=c;
			c++;
		}
	} else {
		Vprintf("%sCharacter overflow generates a trap!%s\n", co, oc);
	}
	c=0; char_min=0;
	c--;
	if (setjmp(lab)==0) { /* Yields char_min */
		while (c<char_min) {
			char_min=c;
			c--;
		}
	}
	Unexpected(3);

	if (L) {
		i_define("CHAR", "_MAX", (long) char_max, (long) CHAR_MAX);
		i_define("CHAR", "_MIN", (long) char_min, (long) CHAR_MIN);
		if (is_signed) {
			i_define("SCHAR", "_MAX", (long) char_max,
				 (long) SCHAR_MAX);
			i_define("SCHAR", "_MIN", (long) char_min,
				 (long) SCHAR_MIN);
		} else {
			i_define("UCHAR", "_MAX", (long) char_max,
				 (long) UCHAR_MAX);
		}

		if (is_signed) {
#ifndef NO_UC
			volatile unsigned char c, char_max;
			c=0; char_max=0;
			c++;
			if (setjmp(lab)==0) { /* Yields char_max */
				while (c>char_max) {
					char_max=c;
					c++;
				}
			}
			Unexpected(4);
			i_define("UCHAR", "_MAX", (long) char_max,
				 (long) UCHAR_MAX);
#endif
		} else {
#ifndef NO_SC /* Define NO_SC if the next line gives a syntax error */
			volatile signed char c, char_max, char_min;
			c=0; char_max=0;
			c++;
			if (setjmp(lab)==0) { /* Yields char_max */
				while (c>char_max) {
					char_max=c;
					c++;
				}
			}
			c=0; char_min=0;
			c--;
			if (setjmp(lab)==0) { /* Yields char_min */
				while (c<char_min) {
					char_min=c;
					c--;
				}
			}
			Unexpected(5);
			i_define("SCHAR", "_MIN", (long) char_min,
				 (long) SCHAR_MIN);
			i_define("SCHAR", "_MAX", (long) char_max,
				 (long) SCHAR_MAX);
#endif /* NO_SC */
		}
	}
	return bits_per_byte;
}

int basic() {
	/* The properties of the basic types.
	   Returns number of bits per sizeof unit */
	volatile int bits_per_byte;

	bits_per_byte= cprop();

	/* Shorts, ints and longs *****************************************/
	Vprintf("%sShort=%d int=%d long=%d float=%d double=%d bits %s\n",
		co,
		(int) sizeof(short)*bits_per_byte,
		(int) sizeof(int)*bits_per_byte,
		(int) sizeof(long)*bits_per_byte,
		(int) sizeof(float)*bits_per_byte,
		(int) sizeof(double)*bits_per_byte, oc);
	if (stdc) {
		Vprintf("%sLong double=%d bits%s\n",
			co, (int) sizeof(Long_double)*bits_per_byte, oc);
	}
	Vprintf("%sChar pointers = %d bits%s%s\n",
		co, (int)sizeof(char *)*bits_per_byte,
		sizeof(char *)>sizeof(int)?" BEWARE! larger than int!":"",
		oc);
	Vprintf("%sInt pointers = %d bits%s%s\n",
		co, (int)sizeof(int *)*bits_per_byte,
		sizeof(int *)>sizeof(int)?" BEWARE! larger than int!":"",
		oc);
	sprop();
	iprop();
	lprop();
	usprop();
	uiprop();
	ulprop();

	Unexpected(6);

	/* Alignment constants ********************************************/
	Vprintf("%sAlignments used for char=%d short=%d int=%d long=%d%s\n",
		co,
		(int)sizeof(struct{char i1; char c1;})-(int)sizeof(char),
		(int)sizeof(struct{short i2; char c2;})-(int)sizeof(short),
		(int)sizeof(struct{int i3; char c3;})-(int)sizeof(int),
		(int)sizeof(struct{long i4; char c4;})-(int)sizeof(long),
		oc);

	/* Ten little endians *********************************************/

	endian(bits_per_byte);

	/* Pointers *******************************************************/
	if (V) {
		if ("abcd"=="abcd")
			printf("%sStrings are shared%s\n", co, oc);
		else printf("%sStrings are not shared%s\n", co, oc);
	}

	return bits_per_byte;
}

#endif /* ifndef PASS */

/* As I said, I apologise for the contortions below. The functions are
   expanded by the preprocessor twice or three times (for float and double,
   and maybe for long double, and for short, int and long). That way,
   I never make a change to one that I forget to make to the other.
   You can look on it as C's fault for not supporting multi-line macro's.
   This whole file is read 3 times by the preprocessor, with PASSn set for
   n=1, 2 or 3, to decide which parts to reprocess.
*/

/* #undef on an already undefined thing is (wrongly) flagged as an error
   by some compilers, therefore the #ifdef that follows: 
*/
#ifdef Number
#undef Number
#undef THING
#undef Thing
#undef thing
#undef FPROP
#undef Fname
#undef Store
#undef Sum
#undef Diff
#undef Mul
#undef Div
#undef Self
#undef F_check
#undef Validate
#undef EPROP
#undef MARK

#undef F_RADIX
#undef F_MANT_DIG
#undef F_DIG
#undef F_ROUNDS
#undef F_EPSILON
#undef F_MIN_EXP
#undef F_MIN
#undef F_MIN_10_EXP
#undef F_MAX_EXP
#undef F_MAX
#undef F_MAX_10_EXP
#endif

#ifdef Integer
#undef Integer
#undef INT
#undef IPROP
#undef Iname
#undef UPROP
#undef Uname
#undef OK_UI

#undef I_MAX
#undef I_MIN
#undef U_MAX
#endif

#ifdef PASS1

#define Number float
#define THING "FLOAT"
#define Thing "Float"
#define thing "float"
#define Fname "FLT"
#define FPROP fprop
#define Store fStore
#define Sum fSum
#define Diff fDiff
#define Mul fMul
#define Div fDiv
#define Self fSelf
#define F_check fCheck
#define Validate fValidate
#define MARK "F"

#define EPROP efprop

#define Integer short
#define INT "short"
#define IPROP sprop
#define Iname "SHRT"
#ifndef NO_UI
#define OK_UI 1
#endif

#define UPROP usprop
#define Uname "USHRT"

#ifdef VERIFY
#define I_MAX SHRT_MAX
#define I_MIN SHRT_MIN
#define U_MAX USHRT_MAX

#define F_RADIX FLT_RADIX
#define F_MANT_DIG FLT_MANT_DIG
#define F_DIG FLT_DIG
#define F_ROUNDS FLT_ROUNDS
#define F_EPSILON FLT_EPSILON
#define F_MIN_EXP FLT_MIN_EXP
#define F_MIN FLT_MIN
#define F_MIN_10_EXP FLT_MIN_10_EXP
#define F_MAX_EXP FLT_MAX_EXP
#define F_MAX FLT_MAX
#define F_MAX_10_EXP FLT_MAX_10_EXP
#endif /* VERIFY */

#endif /* PASS1 */

#ifdef PASS2

#define Number double
#define THING "DOUBLE"
#define Thing "Double"
#define thing "double"
#define Fname "DBL"
#define FPROP dprop
#define Store dStore
#define Sum dSum
#define Diff dDiff
#define Mul dMul
#define Div dDiv
#define Self dSelf
#define F_check dCheck
#define Validate dValidate
#define MARK ""

#define EPROP edprop

#define Integer int
#define INT "int"
#define IPROP iprop
#define Iname "INT"
#define OK_UI 1 /* Unsigned int is always possible */

#define UPROP uiprop
#define Uname "UINT"

#ifdef VERIFY
#define I_MAX INT_MAX
#define I_MIN INT_MIN
#define U_MAX UINT_MAX

#define F_MANT_DIG DBL_MANT_DIG
#define F_DIG DBL_DIG
#define F_EPSILON DBL_EPSILON
#define F_MIN_EXP DBL_MIN_EXP
#define F_MIN DBL_MIN
#define F_MIN_10_EXP DBL_MIN_10_EXP
#define F_MAX_EXP DBL_MAX_EXP
#define F_MAX DBL_MAX
#define F_MAX_10_EXP DBL_MAX_10_EXP
#endif /* VERIFY */

#endif /* PASS2 */

#ifdef PASS3

#ifdef __STDC__
#define Number long double
#endif

#define THING "LONG DOUBLE"
#define Thing "Long double"
#define thing "long double"
#define Fname "LDBL"
#define FPROP ldprop
#define Store ldStore
#define Sum ldSum
#define Diff ldDiff
#define Mul ldMul
#define Div ldDiv
#define Self ldSelf
#define F_check ldCheck
#define Validate ldValidate
#define MARK "L"

#define EPROP eldprop

#define Integer long
#define INT "long"
#define IPROP lprop
#define Iname "LONG"
#ifndef NO_UI
#define OK_UI 1
#endif

#define UPROP ulprop
#define Uname "ULONG"

#ifdef VERIFY
#define I_MAX LONG_MAX
#define I_MIN LONG_MIN
#define U_MAX ULONG_MAX

#define F_MANT_DIG LDBL_MANT_DIG
#define F_DIG LDBL_DIG
#define F_EPSILON LDBL_EPSILON
#define F_MIN_EXP LDBL_MIN_EXP
#define F_MIN LDBL_MIN
#define F_MIN_10_EXP LDBL_MIN_10_EXP
#define F_MAX_EXP LDBL_MAX_EXP
#define F_MAX LDBL_MAX
#define F_MAX_10_EXP LDBL_MAX_10_EXP
#endif /* VERIFY */

#endif /* PASS3 */

#ifndef VERIFY
#define I_MAX int_max
#define I_MIN int_min
#define U_MAX int_max

#define F_RADIX f_radix
#define F_MANT_DIG f_mant_dig
#define F_DIG f_dig
#define F_ROUNDS f_rounds
#define F_EPSILON f_epsilon
#define F_MIN_EXP f_min_exp
#define F_MIN f_min
#define F_MIN_10_EXP f_min_10_exp
#define F_MAX_EXP f_max_exp
#define F_MAX f_max
#define F_MAX_10_EXP f_max_10_exp
#endif

Procedure IPROP() { /* for short, int, and long */
	volatile Integer newi, int_max, maxeri, int_min, minneri;
	volatile int ibits, ipower, two=2;

	/* Calculate max short/int/long ***********************************/
	/* Calculate 2**n-1 until overflow - then use the previous value  */

	newi=1; int_max=0;

	if (setjmp(lab)==0) { /* Yields int_max */
		for(ipower=0; newi>int_max; ipower++) {
			int_max=newi;
			newi=newi*two+1;
		}
		Vprintf("%sOverflow of a%s %s does not generate a trap%s\n",
			co, INT[0]=='i'?"n":"", INT, oc);
	} else {
		Vprintf("%sOverflow of a%s %s generates a trap%s\n",
			co, INT[0]=='i'?"n":"", INT, oc);
	}
	Unexpected(7);

	/* Minimum value: assume either two's or one's complement *********/
	int_min= -int_max;
	if (setjmp(lab)==0) { /* Yields int_min */
		if (int_min-1 < int_min) int_min--;
	}
	Unexpected(8);

	/* Now for those daft Cybers: */

	maxeri=0; newi=int_max;

	if (setjmp(lab)==0) { /* Yields maxeri */
		for(ibits=ipower; newi>maxeri; ibits++) {
			maxeri=newi;
			newi=newi+newi+1;
		}
	}
	Unexpected(9);

	minneri= -maxeri;
	if (setjmp(lab)==0) { /* Yields minneri */
		if (minneri-1 < minneri) minneri--;
	}
	Unexpected(10);

	Vprintf("%sMaximum %s = %ld (= 2**%d-1)%s\n",
		co, INT, (long)int_max, ipower, oc);
	Vprintf("%sMinimum %s = %ld%s\n", co, INT, (long)int_min, oc);

	if (L) i_define(Iname, "_MAX", (long) int_max, (long) I_MAX);
	if (L) i_define(Iname, "_MIN", (long) int_min, (long) I_MIN);

	if (maxeri>int_max) {
		Vprintf("%sThere is a larger %s, %ld (= 2**%d-1), %s %s%s\n",
			co, INT, (long)maxeri, ibits, 
			"but only for addition, not multiplication",
			"(I smell a Cyber!)",
			oc);
	}

	if (minneri<int_min) {
		Vprintf("%sThere is a smaller %s, %ld, %s %s%s\n",
			co, INT, (long)minneri, 
			"but only for addition, not multiplication",
			"(I smell a Cyber!)",
			oc);
	}
}

Procedure UPROP () { /* unsigned short/int/long */
#ifdef OK_UI
	volatile unsigned Integer int_max, newi, two;
	newi=1; int_max=0; two=2;

	if (setjmp(lab)==0) { /* Yields int_max */
		while(newi>int_max) {
			int_max=newi;
			newi=newi*two+1;
		}
	}
	Unexpected(11);
	Vprintf("%sMaximum unsigned %s = %lu%s\n",
		co, INT, (unsigned long) int_max, oc);
	if (L) u_define(Uname, "_MAX", (unsigned long) int_max,
			(unsigned long) U_MAX);
#endif
}


#ifdef Number

/* These routines are intended to defeat any attempt at optimisation
   or use of extended precision, and to defeat faulty narrowing casts:
*/
Procedure Store(a, b) Number a, *b; { *b=a; }
Number Sum(a, b) Number a, b; { Number r; Store(a+b, &r); return (r); }
Number Diff(a, b) Number a, b; { Number r; Store(a-b, &r); return (r); }
Number Mul(a, b) Number a, b; { Number r; Store(a*b, &r); return (r); }
Number Div(a, b) Number a, b; { Number r; Store(a/b, &r); return (r); }
Number Self(a) Number a; { Number r; Store(a, &r); return (r); }

Procedure F_check(precision, val1) int precision; Long_double val1; {
	/* You don't think I'm going to go to all the trouble of writing
	   a program that works out what all sorts of values are, only to
	   have printf go and print the wrong values out, do you?
	   No, you're right, so this function tries to see if printf
	   has written the right value, by reading it back again.
	   This introduces a new problem of course: suppose printf writes
	   the correct value, and scanf reads it back wrong... oh well.
	   But I'm adamant about this: the precision given is enough
	   to uniquely identify the printed number, therefore I insist
	   that sscanf read the number back identically. Harsh yes, but
	   sometimes you've got to be cruel to be kind.
	*/
	Long_double new1;
	Number val, new, diff;
	double rem;
	int e;
	char *rep;
	char *f2;

	if (sizeof(double) == sizeof(Long_double)) {
		/* Assume they're the same, and use non-stdc format */
		/* This is for stdc compilers using non-stdc libraries */
		f2= "%le";   /* Input */
	} else {
		/* It had better support Le then */
		f2= "%Le";
	}
	val= val1;
	rep= f_rep(precision, (Long_double) val);
	if (setjmp(lab)==0) {
		sscanf(rep, f2, &new1);
	} else {
		eek_a_bug("sscanf caused a trap");
		printf("%s    scanning: %s format: %s%s\n\n", co, rep, f2, oc);
		Unexpected(12);
		return;
	}

	if (setjmp(lab)==0) { /* See if new is usable */
		new= new1;
		if (new != 0.0) {
			diff= val/new - 1.0;
			if (diff < 0.1) diff= 1.0;
			/* That should be enough to generate a trap */
		}
	} else {
		eek_a_bug("sscanf returned an unusable number");
		printf("%s    scanning: %s with format: %s%s\n\n",
		       co, rep, f2, oc);
		Unexpected(13);
		return;
	}

	Unexpected(14);
	if (new != val) {
		eek_a_bug("Possibly bad output from printf above");
		if (!exponent(val, &rem, &e)) {
			printf("%s    but value was an unusable number%s\n\n",
			       co, oc);
			return;
		}
		printf("%s    expected value around %.*fe%d, bit pattern:\n    ",
		       co, precision, rem, e);
		bitpattern((char *) &val, sizeof(val));
		printf ("%s\n", oc);
		printf("%s    sscanf gave           %s, bit pattern:\n    ",
		       co, f_rep(precision, (Long_double) new));
		bitpattern((char *) &new, sizeof(new));
		printf ("%s\n", oc);
		printf("%s    difference= %s%s\n\n", 
		       co, f_rep(precision, (Long_double) (val-new)), oc);
	}
}

Procedure Validate(prec, val, req, same) int prec, same; Long_double val, req; {
	Unexpected(15);
	if (!same) {
		printf("%s*** Verify failed for above #define!\n", co);
		if (setjmp(lab) == 0) { /* for the case that req == nan */
			printf("       Compiler has %s for value%s\n", 
			       f_rep(prec, req), oc);
		} else {
			printf("       Compiler has %s for value%s\n",
			       "an unusable number", oc);
		}
		if (setjmp(lab) == 0) {
			F_check(prec, (Long_double) req);
		} /*else forget it*/
		if (setjmp(lab) == 0) {		
			if (req > 0.0 && val > 0.0) {
				printf("%s    difference= %s%s\n",
				       co, f_rep(prec, val-req), oc);
			}
		} /*else forget it*/
		Unexpected(16);
		printf("\n");
		bugs++;
	} else if (val != req) {
		if (stdc) {
			printf("%s*** Verify failed for above #define!\n", co);
			printf("       Constant has the wrong precision%s\n",
			       oc);
			bugs++;
		} else eek_a_bug("the cast didn't work");
		printf("\n");
	}
}

int FPROP(bits_per_byte) int bits_per_byte; {
	/* Properties of floating types, using algorithms by Cody and Waite
	   from MA Malcolm, as modified by WM Gentleman and SB Marovich.
	   Further extended by S Pemberton.

	   Returns the number of digits in the fraction.
	*/

	volatile int i, f_radix, iexp, irnd, mrnd, f_rounds, f_mant_dig,
	    iz, k, inf, machep, f_max_exp, f_min_exp, mx, negeps,
	    mantbits, digs, f_dig, trap,
	    hidden, normal, f_min_10_exp, f_max_10_exp;
	volatile Number a, b, base, basein, basem1, f_epsilon, epsneg,
	       f_max, newxmax, f_min, xminner, y, y1, z, z1, z2;

	Unexpected(17);

	Vprintf("%sPROPERTIES OF %s:%s\n", co, THING, oc);

	/* Base and size of mantissa **************************************/
	/* First repeatedly double until adding 1 has no effect.	  */
	/* For instance, if base is 10, with 3 significant digits	  */
	/* it will try 1, 2, 4, 8, ... 512, 1024, and stop there,	  */
	/* since 1024 is only representable as 1020.			  */
	a=1.0;
	if (setjmp(lab)==0) { /* inexact trap? */
		do { a=Sum(a, a); }
		while (Diff(Diff(Sum(a, 1.0), a), 1.0) == 0.0);
	} else {
		fprintf(stderr, "*** Program got loss-of-precision trap!\n");
		/* And supporting those is just TOO much trouble! */
		exit(bugs+1);
	}
	Unexpected(18);
	/* Now double until you find a number that can be added to the	  */
	/* above number. For 1020 this is 8 or 16, depending whether the  */
	/* result is rounded or truncated.				  */
	/* In either case the result is 1030. 1030-1020= the base, 10.	  */
	b=1.0;
	do { b=Sum(b, b); } while ((base=Diff(Sum(a, b), a)) == 0.0);
	f_radix=base;
	Vprintf("%sBase = %d%s\n", co, f_radix, oc);

	/* Sanity check; if base<2, I can't guarantee the rest will work  */
	if (f_radix < 2) {
		eek_a_bug("Function return or parameter passing faulty? (This is a guess.)");
		printf("\n");
		return(0);
	}

#ifdef PASS1 /* only for FLT */
	if (F) i_define("FLT", "_RADIX", (long) f_radix, (long) F_RADIX);
#endif

	/* Now the number of digits precision: */
	f_mant_dig=0; b=1.0;
	do { f_mant_dig++; b=Mul(b, base); }
	while (Diff(Diff(Sum(b,1.0),b),1.0) == 0.0);
	f_dig=floor_log(10, (Long_double)(b/base)) + (base==10?1:0);
	Vprintf("%sSignificant base digits = %d %s %d %s%s\n",
		co, f_mant_dig, "(= at least", f_dig, "decimal digits)", oc);
	if (F) i_define(Fname, "_MANT_DIG", (long) f_mant_dig,
			(long) F_MANT_DIG);
	if (F) i_define(Fname, "_DIG", (long) f_dig, (long) F_DIG);
	digs= ceil_log(10, (Long_double)b); /* the number of digits to printf */

	/* Rounding *******************************************************/
	basem1=Diff(base, 0.5);
	if (Diff(Sum(a, basem1), a) != 0.0) {
		if (f_radix == 2) basem1=0.375;
		else basem1=1.0;
		if (Diff(Sum(a, basem1), a) != 0.0) irnd=2; /* away from 0 */
		else irnd=1; /* to nearest */
	} else irnd=0; /* towards 0 */

	basem1=Diff(base, 0.5);

	if (Diff(Diff(-a, basem1), -a) != 0.0) {
		if (f_radix == 2) basem1=0.375;
		else basem1=1.0;
		if (Diff(Diff(-a, basem1), -a) != 0.0) mrnd=2; /* away from 0*/
		else mrnd=1; /* to nearest */
	} else mrnd=0; /* towards 0 */

	f_rounds=4; /* Unknown rounding */
	if (irnd==0 && mrnd==0) f_rounds=0; /* zero = chops */
	if (irnd==1 && mrnd==1) f_rounds=1; /* nearest */
	if (irnd==2 && mrnd==0) f_rounds=2; /* +inf */
	if (irnd==0 && mrnd==2) f_rounds=3; /* -inf */

	if (f_rounds != 4) {
		Vprintf("%sArithmetic rounds towards ", co);
		switch (f_rounds) {
		      case 0: Vprintf("zero (i.e. it chops)"); break;
		      case 1: Vprintf("nearest"); break;
		      case 2: Vprintf("+infinity"); break;
		      case 3: Vprintf("-infinity"); break;
		      default: Vprintf("???"); break;
		}
		Vprintf("%s\n", oc);
	} else { /* Hmm, try to give some help here: */
		Vprintf("%sArithmetic rounds oddly: %s\n", co, oc);
		Vprintf("%s    Negative numbers %s%s\n",
			co, mrnd==0 ? "towards zero" :
			    mrnd==1 ? "to nearest" :
				      "away from zero",
			oc);
		Vprintf("%s    Positive numbers %s%s\n",
			co, irnd==0 ? "towards zero" :
			    irnd==1 ? "to nearest" :
				      "away from zero",
			oc);
	}
	/* An extra goody */
	if (f_radix == 2 && f_rounds == 1) {
		if (Diff(Sum(a, 1.0), a) != 0.0) {
			Vprintf("%s   Tie breaking rounds up%s\n", co, oc);
		} else if (Diff(Sum(a, 3.0), a) == 4.0) {
			Vprintf("%s   Tie breaking rounds to even%s\n", co, oc);
		} else {
			Vprintf("%s   Tie breaking rounds down%s\n", co, oc);
		}
	}
#ifdef PASS1 /* only for FLT */
	if (F) i_define("FLT", "_ROUNDS", (long) f_rounds, (long) F_ROUNDS);
#endif

	/* Various flavours of epsilon ************************************/
	negeps=f_mant_dig+f_mant_dig;
	basein=1.0/base;
	a=1.0;
	for(i=1; i<=negeps; i++) a*=basein;

	b=a;
	while (Diff(Diff(1.0, a), 1.0) == 0.0) {
		a*=base;
		negeps--;
	}
	negeps= -negeps;
	Vprintf("%sSmallest x such that 1.0-base**x != 1.0 = %d%s\n",
		co, negeps, oc);

	epsneg=a;
	if ((f_radix!=2) && irnd) {
	/*	a=(a*(1.0+a))/(1.0+1.0); => */
		a=Div(Mul(a, Sum(1.0, a)), Sum(1.0, 1.0));
	/*	if ((1.0-a)-1.0 != 0.0) epsneg=a; => */
		if (Diff(Diff(1.0, a), 1.0) != 0.0) epsneg=a;
	}
	Vprintf("%sSmall x such that 1.0-x != 1.0 = %s%s\n",
		co, f_rep(digs, (Long_double) epsneg), oc);
	/* it may not be the smallest */
	if (V) F_check(digs, (Long_double) epsneg);
	Unexpected(19);

	machep= -f_mant_dig-f_mant_dig;
	a=b;
	while (Diff(Sum(1.0, a), 1.0) == 0.0) { a*=base; machep++; }
	Vprintf("%sSmallest x such that 1.0+base**x != 1.0 = %d%s\n",
		co, machep, oc);

	f_epsilon=a;
	if ((f_radix!=2) && irnd) {
	/*	a=(a*(1.0+a))/(1.0+1.0); => */
		a=Div(Mul(a, Sum(1.0, a)), Sum(1.0, 1.0));
	/*	if ((1.0+a)-1.0 != 0.0) f_epsilon=a; => */
		if (Diff(Sum(1.0, a), 1.0) != 0.0) f_epsilon=a;
	}
	Vprintf("%sSmallest x such that 1.0+x != 1.0 = %s%s\n",
		co, f_rep(digs, (Long_double) f_epsilon), oc);
	/* Possible loss of precision warnings here from non-stdc compilers: */
	if (F) f_define(Fname, "_EPSILON", digs, (Long_double) f_epsilon, MARK);
	if (V || F) F_check(digs, (Long_double) f_epsilon);
	Unexpected(20);
	if (F) Validate(digs, (Long_double) f_epsilon, (Long_double) F_EPSILON,
			f_epsilon == Self(F_EPSILON));
	Unexpected(21);

	/* Extra chop info *************************************************/
	if (f_rounds == 0) {
		if (Diff(Mul(Sum(1.0,f_epsilon),1.0),1.0) !=  0.0) {
			Vprintf("%sAlthough arithmetic chops, it uses guard digits%s\n", co, oc);
		}
	}

	/* Size of and minimum normalised exponent ************************/
	y=0; i=0; k=1; z=basein; z1=(1.0+f_epsilon)/base;

	/* Coarse search for the largest power of two */
	if (setjmp(lab)==0) { /* for underflow trap */ /* Yields i, k, y, y1 */
		do {
			y=z; y1=z1;
			z=Mul(y,y); z1=Mul(z1, y);
			a=Mul(z,1.0);
			z2=Div(z1,y);
			if (z2 != y1) break;
			if ((Sum(a,a) == 0.0) || (fabs(z) >= y)) break;
			i++;
			k+=k;
		} while(1);
	} else {
		Vprintf("%s%s underflow generates a trap%s\n", co, Thing, oc);
	}
	Unexpected(22);

	if (f_radix != 10) {
		iexp=i+1; /* for the sign */
		mx=k+k;
	} else {
		iexp=2;
		iz=f_radix;
		while (k >= iz) { iz*=f_radix; iexp++; }
		mx=iz+iz-1;
	}

	/* Fine tune starting with y and y1 */
	if (setjmp(lab)==0) { /* for underflow trap */ /* Yields k, f_min */
		do {
			f_min=y; z1=y1;
			y=Div(y,base); y1=Div(y1,base);
			a=Mul(y,1.0);
			z2=Mul(y1,base);
			if (z2 != z1) break;
			if ((Sum(a,a) == 0.0) || (fabs(y) >= f_min)) break;
			k++;
		} while (1);
	}
	Unexpected(23);

	f_min_exp=(-k)+1;

	if ((mx <= k+k-3) && (f_radix != 10)) { mx+=mx; iexp+=1; }
	Vprintf("%sNumber of bits used for exponent = %d%s\n", co, iexp, oc);
	Vprintf("%sMinimum normalised exponent = %d%s\n", co, f_min_exp, oc);
	if (F) i_define(Fname, "_MIN_EXP", (long) f_min_exp, (long) F_MIN_EXP);

	if (setjmp(lab)==0) {
		Vprintf("%sMinimum normalised positive number = %s%s\n",
			co, f_rep(digs, (Long_double) f_min), oc);
	} else {
		eek_a_bug("printf can't print the smallest normalised number");
		printf("\n");
	}
	Unexpected(24);
	/* Possible loss of precision warnings here from non-stdc compilers: */
	if (setjmp(lab) == 0) {
		if (F) f_define(Fname, "_MIN", digs, (Long_double) f_min, MARK);
		if (V || F) F_check(digs, (Long_double) f_min);
	} else {
		eek_a_bug("xxx_MIN caused a trap");
		printf("\n");
	}

	if (setjmp(lab) == 0) {
		if (F) Validate(digs, (Long_double) f_min, (Long_double) F_MIN,
				f_min == Self(F_MIN));
	} else {
		printf("%s*** Verify failed for above #define!\n    %s %s\n\n",
		       co, "Compiler has an unusable number for value", oc);
		bugs++;
	}
	Unexpected(25);

	a=1.0; f_min_10_exp=0;
	while (a > f_min*10.0) { a/=10.0; f_min_10_exp--; }
	if (F) i_define(Fname, "_MIN_10_EXP", (long) f_min_10_exp,
			(long) F_MIN_10_EXP);

	/* Minimum exponent ************************************************/
	if (setjmp(lab)==0) { /* for underflow trap */ /* Yields xminner */
		do {
			xminner=y;
			y=Div(y,base);
			a=Mul(y,1.0);
			if ((Sum(a,a) == 0.0) || (fabs(y) >= xminner)) break;
		} while (1);
	}
	Unexpected(26);

	if (xminner != 0.0 && xminner != f_min) {
		normal= 0;
		Vprintf("%sThe smallest numbers are not kept normalised%s\n",
			co, oc);
		if (setjmp(lab)==0) {
		    Vprintf("%sSmallest unnormalised positive number = %s%s\n",
			    co, f_rep(digs, (Long_double) xminner), oc);
		    if (V) F_check(digs, (Long_double) xminner);
		} else {
			eek_a_bug("printf can't print the smallest unnormalised number.");
			printf("\n");
		}
		Unexpected(27);
	} else {
		normal= 1;
		Vprintf("%sThe smallest numbers are normalised%s\n", co, oc);
	}

	/* Maximum exponent ************************************************/
	f_max_exp=2; f_max=1.0; newxmax=base+1.0;
	inf=0; trap=0;
	while (f_max<newxmax) {
		f_max=newxmax;
		if (setjmp(lab) == 0) { /* Yields inf, f_max_exp */
			newxmax=Mul(newxmax, base);
		} else {
			trap=1;
			break;
		}
		if (Div(newxmax, base) != f_max) {
			inf=1; /* ieee infinity */
			break;
		}
		f_max_exp++;
	}
	Unexpected(28);
	if (trap) {
		Vprintf("%s%s overflow generates a trap%s\n", co, Thing, oc);
	}

	if (inf) Vprintf("%sThere is an 'infinite' value%s\n", co, oc);
	Vprintf("%sMaximum exponent = %d%s\n", co, f_max_exp, oc);
	if (F) i_define(Fname, "_MAX_EXP", (long) f_max_exp, (long) F_MAX_EXP);

	/* Largest number ***************************************************/
	f_max=Diff(1.0, epsneg);
	if (Mul(f_max,1.0) != f_max) f_max=Diff(1.0, Mul(base,epsneg));
	for (i=1; i<=f_max_exp; i++) f_max=Mul(f_max, base);

	if (setjmp(lab)==0) {
		Vprintf("%sMaximum number = %s%s\n",
			co, f_rep(digs, (Long_double) f_max), oc);
	} else {
		eek_a_bug("printf can't print the largest double.");
		printf("\n");
	}
	if (setjmp(lab)==0) {
	/* Possible loss of precision warnings here from non-stdc compilers: */
		if (F) f_define(Fname, "_MAX", digs, (Long_double) f_max, MARK);
		if (V || F) F_check(digs, (Long_double) f_max);
	} else {
		eek_a_bug("xxx_MAX caused a trap");
		printf("\n");
	}
	if (setjmp(lab)==0) {
		if (F) Validate(digs, (Long_double) f_max, (Long_double) F_MAX,
				f_max == Self(F_MAX));
	} else {
		printf("%s*** Verify failed for above #define!\n    %s %s\n\n",
		       co, "Compiler has an unusable number for value", oc);
		bugs++;
	}
	Unexpected(29);

	a=1.0; f_max_10_exp=0;
	while (a < f_max/10.0) { a*=10.0; f_max_10_exp++; }
	if (F) i_define(Fname, "_MAX_10_EXP", (long) f_max_10_exp,
			(long) F_MAX_10_EXP);

	/* Hidden bit + sanity check ****************************************/
	if (f_radix != 10) {
		hidden=0;
		mantbits=floor_log(2, (Long_double)f_radix)*f_mant_dig;
		if (mantbits+iexp == (int)sizeof(Number)*bits_per_byte) {
			hidden=1;
			Vprintf("%sArithmetic uses a hidden bit%s\n", co, oc);
		} else if (mantbits+iexp+1 == (int)sizeof(Number)*bits_per_byte) {
			Vprintf("%sArithmetic doesn't use a hidden bit%s\n",
				co, oc);
		} else {
			printf("\n%s%s\n    %s %s %s!%s\n\n",
			       co,
			       "*** Something fishy here!",
			       "Exponent size + mantissa size doesn't match",
			       "with the size of a", thing,
			       oc);
		}
		if (hidden && f_radix == 2 && f_max_exp+f_min_exp==3) {
			Vprintf("%sIt looks like %s length IEEE format%s\n",
				co, f_mant_dig==24 ? "single" :
				    f_mant_dig==53 ? "double" :
				    f_mant_dig >53 ? "extended" :
						"some", oc);
			if (f_rounds != 1 || normal) {
				Vprintf("%s   though ", co);
				if (f_rounds != 1) {
					Vprintf("the rounding is unusual");
					if (normal) Vprintf(" and ");
				}
				if (normal) Vprintf("the normalisation is unusual");
				Vprintf("%s\n", oc);
			}
		} else {
			Vprintf("%sIt doesn't look like IEEE format%s\n",
				co, oc);
		}
	}
	printf("\n"); /* regardless of verbosity */
	return f_mant_dig;
}

Procedure EPROP(fprec, dprec, lprec) int fprec, dprec, lprec; {
	/* See if expressions are evaluated in extended precision.
	   Some compilers optimise even if you don't want it,
	   and then this function fails to produce the right result.
	   We try to diagnose this if it happens.
	*/
	volatile int eprec;
	volatile double a, b, base, old;
	volatile Number d, oldd, dbase, one, zero;
	volatile int bad=0;

	/* Size of mantissa **************************************/
	a=1.0;
	if (setjmp(lab) == 0) { /* Yields nothing */
		do { old=a; a=a+a; }
		while ((((a+1.0)-a)-1.0) == 0.0 && a>old);
	} else bad=1;
	if (a <= old) bad=1;

	if (!bad) {
		b=1.0;
		if (setjmp(lab) == 0) { /* Yields nothing */
			do { old=b; b=b+b; }
			while ((base=((a+b)-a)) == 0.0 && b>old);
			if (b <= old) bad=1;
		} else bad=1;
	}

	if (!bad) {
		eprec=0; d=1.0; dbase=base; one=1.0; zero=0.0;
		if (setjmp(lab) == 0) { /* Yields nothing */
			do { eprec++; oldd=d; d=d*dbase; }
			while ((((d+one)-d)-one) == zero && d>oldd);
			if (d <= oldd) bad=1;
		} else bad=1;
	}

	Unexpected(30);

	if (bad) {
	  Vprintf("%sCan't determine precision for %s expressions:\n%s%s\n", 
		 co, thing, "   check that you compiled without optimisation!",
		 oc);
	} else if (eprec==dprec) {
	  Vprintf("%s%s expressions are evaluated in double precision%s\n",
		  co, Thing, oc);
	} else if (eprec==fprec) {
	  Vprintf("%s%s expressions are evaluated in float precision%s\n",
		  co, Thing, oc);
	} else if (eprec==lprec) {
	  Vprintf("%s%s expressions are evaluated in long double precision%s\n",
		  co, Thing, oc);
	} else {
		Vprintf("%s%s expressions are evaluated in a %s %s %d %s%s\n",
			co, Thing, eprec>dprec ? "higher" : "lower",
			"precision than double,\n   using",
			eprec, "base digits",
		        oc);
	}
}

#else /* Number */

#ifdef FPROP
/* ARGSUSED */
int FPROP(bits_per_byte) int bits_per_byte; {
	return 0;
}
#endif
#ifdef EPROP
/* ARGSUSED */
Procedure EPROP(fprec, dprec, lprec) int fprec, dprec, lprec; {}
#endif

#endif /* ifdef Number */

#ifdef PASS3
#undef PASS
#endif

#ifdef PASS2
#undef PASS2
#define PASS3 1
#endif

#ifdef PASS1
#undef PASS1
#define PASS2 1
#endif

/* If your C compiler doesn't accept the next #include,
   replace __FILE__ with the file name - and get a new C compiler... */

#ifdef PASS
#include "hard-params.c"
#endif

