/*
 * Copyright (c) 1994 David I. Bell
 * Permission is granted to use, distribute, or modify this source,
 * provided that this copyright notice remains intact.
 *
 * Built-in functions implemented here
 */

#include <sys/types.h>
#include <sys/times.h>
#include <time.h>

#include "calc.h"
#include "opcodes.h"
#include "token.h"
#include "func.h"
#include "string.h"
#include "symbol.h"


/* if HZ & CLK_TCK are not defined, pick typical values, hope for the best */
#if !defined(HZ)
#  define HZ 60
#endif
#if !defined(CLK_TCK)
# undef CLK_TCK
# define CLK_TCK HZ
#endif

extern int errno;


/*
 * Totally numeric functions.
 */
static NUMBER *f_cfsim();	/* simplify number using continued fractions */
static NUMBER *f_ilog();	/* return log of one number to another */
static NUMBER *f_faccnt();	/* count of divisions */
static NUMBER *f_min();		/* minimum of several arguments */
static NUMBER *f_max();		/* maximum of several arguments */
static NUMBER *f_hmean();	/* harmonic mean */
static NUMBER *f_trunc();	/* truncate number to specified decimal places */
static NUMBER *f_btrunc();	/* truncate number to specified binary places */
static NUMBER *f_gcd();		/* greatest common divisor */
static NUMBER *f_lcm();		/* least common multiple */
static NUMBER *f_xor();		/* xor of several arguments */
static NUMBER *f_ceil();	/* ceiling of a fraction */
static NUMBER *f_floor();	/* floor of a fraction */
static NUMBER *f_meq();		/* numbers are same modular value */
static NUMBER *f_isrel();	/* two numbers are relatively prime */
static NUMBER *f_ismult();	/* whether one number divides another */
static NUMBER *f_mne();		/* whether a and b are not equal modulo c */
static NUMBER *f_isset();	/* tests if a bit of a num (base 2) is set */
static NUMBER *f_highbit();	/* high bit number in base 2 representation */
static NUMBER *f_lowbit();	/* low bit number in base 2 representation */
static NUMBER *f_near();	/* whether two numbers are near each other */
static NUMBER *f_legtoleg();	/* positive form of leg to leg */
static NUMBER *f_ilog10();	/* integer log of number base 10 */
static NUMBER *f_ilog2();	/* integer log of number base 2 */
static NUMBER *f_digits();	/* number of digits of number */
static NUMBER *f_digit();	/* digit at specified decimal place of number */
static NUMBER *f_places();	/* number of decimal places of number */
static NUMBER *f_primetest();	/* primality test */
static NUMBER *f_issquare();	/* whether number is a square */
static NUMBER *f_runtime();	/* user runtime in seconds */
static NUMBER *f_base();	/* set default output base */


/*
 * General functions.
 */
static VALUE f_hash();		/* produce hash from values */
static VALUE f_bround();	/* round number to specified binary places */
static VALUE f_round();		/* round number to specified decimal places */
static VALUE f_det();		/* determinant of matrix */
static VALUE f_mattrans();	/* return transpose of matrix */
static VALUE f_matdim();	/* dimension of matrix */
static VALUE f_matmax();	/* maximum index of matrix dimension */
static VALUE f_matmin();	/* minimum index of matrix dimension */
static VALUE f_matfill();	/* fill matrix with values */
static VALUE f_listpush();	/* push element onto front of list */
static VALUE f_listpop();	/* pop element from front of list */
static VALUE f_listappend();	/* append element to end of list */
static VALUE f_listremove();	/* remove element from end of list */
static VALUE f_listinsert();	/* insert element into list */
static VALUE f_listdelete();	/* delete element from list */
static VALUE f_strlen();	/* length of string */
static VALUE f_char();		/* character value of integer */
static VALUE f_substr();	/* extract substring */
static VALUE f_strcat();	/* concatenate strings */
static VALUE f_ord();		/* get ordinal value for character */
static VALUE f_avg();		/* average of several arguments */
static VALUE f_ssq();		/* sum of squares */
static VALUE f_poly();		/* result of evaluating polynomial */
static VALUE f_sqrt();		/* square root of a number */
static VALUE f_root();		/* number taken to root of another */
static VALUE f_exp();		/* complex exponential */
static VALUE f_ln();		/* complex natural logarithm */
static VALUE f_power();		/* one value to another power */
static VALUE f_cos();		/* complex cosine */
static VALUE f_sin();		/* complex sine */
static VALUE f_polar();		/* polar representation of complex number */
static VALUE f_arg();		/* argument of complex number */
static VALUE f_list();		/* create a list */
static VALUE f_size();		/* number of elements in object */
static VALUE f_search();	/* search matrix or list for match */
static VALUE f_rsearch();	/* search matrix or list backwards for match */
static VALUE f_cp();		/* cross product of vectors */
static VALUE f_dp();		/* dot product of vectors */
static VALUE f_prompt();	/* prompt for input line */
static VALUE f_eval();		/* evaluate string into value */
static VALUE f_str();		/* convert value to string */
static VALUE f_fopen();		/* open file for reading or writing */
static VALUE f_fprintf();	/* print data to file */
static VALUE f_strprintf();	/* return printed data as a string */
static VALUE f_fgetline();	/* read next line from file */
static VALUE f_fgetc();		/* read next char from file */
static VALUE f_fflush();	/* flush output to file */
static VALUE f_printf();	/* print data to stdout */
static VALUE f_fclose();	/* close file */
static VALUE f_ferror();	/* whether error occurred */
static VALUE f_feof();		/* whether end of file reached */
static VALUE f_files();		/* return file handle or number of files */
static VALUE f_assoc();		/* return a new association value */


#define IN 100		/* maximum number of arguments */
#define	FE 0x01		/* flag to indicate default epsilon argument */
#define	FA 0x02		/* preserve addresses of variables */


/*
 * List of primitive built-in functions
 */
static struct builtin {
	char *b_name;		/* name of built-in function */
	short b_minargs;	/* minimum number of arguments */
	short b_maxargs;	/* maximum number of arguments */
	short b_flags;		/* special handling flags */
	short b_opcode;		/* opcode which makes the call quick */
	NUMBER *(*b_numfunc)();	/* routine to calculate numeric function */
	VALUE (*b_valfunc)();	/* routine to calculate general values */
	char *b_desc;		/* description of function */
} builtins[] = {
	"abs", 1, 2, 0, OP_ABS, 0, 0, "absolute value within accuracy b",
	"acos", 1, 2, FE, OP_NOP, qacos, 0, "arccosine of a within accuracy b",
	"acosh", 1, 2, FE, OP_NOP, qacosh, 0, "hyperbolic arccosine of a within accuracy b",
	"append", 2, 2, FA, OP_NOP, 0, f_listappend, "append value to end of list",
	"appr", 1, 2, FE, OP_NOP, qbappr, 0, "approximate a with simpler fraction to within b",
	"arg", 1, 2, 0, OP_NOP, 0, f_arg, "argument (the angle) of complex number",
	"asin", 1, 2, FE, OP_NOP, qasin, 0, "arcsine of a within accuracy b",
	"asinh", 1, 2, FE, OP_NOP, qasinh, 0, "hyperbolic arcsine of a within accuracy b",
	"assoc", 0, 0, 0, OP_NOP, 0, f_assoc, "create new association array",
	"atan", 1, 2, FE, OP_NOP, qatan, 0, "arctangent of a within accuracy b",
	"atan2", 2, 3, FE, OP_NOP, qatan2, 0, "angle to point (b,a) within accuracy c",
	"atanh", 1, 2, FE, OP_NOP, qatanh, 0, "hyperbolic arctangent of a within accuracy b",
	"avg", 1, IN, 0, OP_NOP, 0, f_avg, "arithmetic mean of values",
	"base", 0, 1, 0, OP_NOP, f_base, 0, "set default output base",
	"bround", 1, 2, 0, OP_NOP, 0, f_bround, "round value a to b number of binary places",
	"btrunc", 1, 2, 0, OP_NOP, f_btrunc, 0, "truncate a to b number of binary places",
	"ceil", 1, 1, 0, OP_NOP, f_ceil, 0, "smallest integer greater than or equal to number",
	"cfappr", 1, 2, FE, OP_NOP, qcfappr, 0, "approximate a within accuracy b using\n\t\t    continued fractions",
	"cfsim", 1, 1, 0, OP_NOP, f_cfsim, 0, "simplify number using continued fractions",
	"char", 1, 1, 0, OP_NOP, 0, f_char, "character corresponding to integer value",
	"cmp", 2, 2, 0, OP_CMP, 0, 0, "compare values returning -1, 0, or 1",
	"comb", 2, 2, 0, OP_NOP, qcomb, 0, "combinatorial number a!/b!(a-b)!",
	"config", 1, 2, 0, OP_SETCONFIG, 0, 0, "set or read configuration value",
	"conj", 1, 1, 0, OP_CONJUGATE, 0, 0, "complex conjugate of value",
	"cos", 1, 2, 0, OP_NOP, 0, f_cos, "cosine of value a within accuracy b",
	"cosh", 1, 2, FE, OP_NOP, qcosh, 0, "hyperbolic cosine of a within accuracy b",
	"cp", 2, 2, 0, OP_NOP, 0, f_cp, "Cross product of two vectors",
	"delete", 2, 2, FA, OP_NOP, 0, f_listdelete, "delete element from list a at position b",
	"den", 1, 1, 0, OP_DENOMINATOR, qden, 0, "denominator of fraction",
	"det", 1, 1, 0, OP_NOP, 0, f_det, "determinant of matrix",
	"digit", 2, 2, 0, OP_NOP, f_digit, 0, "digit at specified decimal place of number",
	"digits", 1, 1, 0, OP_NOP, f_digits, 0, "number of digits in number",
	"dp", 2, 2, 0, OP_NOP, 0, f_dp, "Dot product of two vectors",
	"epsilon", 0, 1, 0, OP_SETEPSILON, 0, 0, "set or read allowed error for real calculations",
	"eval", 1, 1, 0, OP_NOP, 0, f_eval, "Evaluate expression from string to value",
	"exp", 1, 2, 0, OP_NOP, 0, f_exp, "exponential of value a within accuracy b",
	"fcnt", 2, 2, 0, OP_NOP, f_faccnt, 0, "count of times one number divides another",
	"fib", 1, 1, 0, OP_NOP, qfib, 0, "Fibonacci number F(n)",
	"frem", 2, 2, 0, OP_NOP, qfacrem, 0, "number with all occurrences of factor removed",
	"fact", 1, 1, 0, OP_NOP, qfact, 0, "factorial",
	"fclose", 1, 1, 0, OP_NOP, 0, f_fclose, "close file",
	"feof", 1, 1, 0, OP_NOP, 0, f_feof, "whether EOF reached for file",
	"ferror", 1, 1, 0, OP_NOP, 0, f_ferror, "whether error occurred for file",
	"fflush", 1, 1, 0, OP_NOP, 0, f_fflush, "flush output to file",
	"fgetc", 1, 1, 0, OP_NOP, 0, f_fgetc, "read next char from file",
	"fgetline", 1, 1, 0, OP_NOP, 0, f_fgetline, "read next line from file",
	"files", 0, 1, 0, OP_NOP, 0, f_files, "return opened file or max number of opened files",
	"floor", 1, 1, 0, OP_NOP, f_floor, 0, "greatest integer less than or equal to number",
	"fopen", 2, 2, 0, OP_NOP, 0, f_fopen, "open file name a in mode b",
	"fprintf", 2, IN, 0, OP_NOP, 0, f_fprintf, "print formatted output to opened file",
	"frac", 1, 1, 0, OP_FRAC, qfrac, 0, "fractional part of value",
	"gcd", 1, IN, 0, OP_NOP, f_gcd, 0, "greatest common divisor",
	"gcdrem", 2, 2, 0, OP_NOP, qgcdrem, 0, "a divided repeatedly by gcd with b",
	"hash", 1, IN, 0, OP_NOP, 0, f_hash, "return non-negative hash value for one or\n\t\t    more values",
	"highbit", 1, 1, 0, OP_NOP, f_highbit, 0, "high bit number in base 2 representation",
	"hmean", 1, IN, 0, OP_NOP, f_hmean, 0, "harmonic mean of values",
	"hypot", 2, 3, FE, OP_NOP, qhypot, 0, "hypotenuse of right triangle within accuracy c",
	"ilog", 2, 2, 0, OP_NOP, f_ilog, 0, "integral log of one number with another",
	"ilog10", 1, 1, 0, OP_NOP, f_ilog10, 0, "integral log of a number base 10",
	"ilog2", 1, 1, 0, OP_NOP, f_ilog2, 0, "integral log of a number base 2",
	"im", 1, 1, 0, OP_IM, 0, 0, "imaginary part of complex number",
	"insert", 3, 3, FA, OP_NOP, 0, f_listinsert, "insert value c into list a at position b",
 	"int", 1, 1, 0, OP_INT, qint, 0, "integer part of value",
	"inverse", 1, 1, 0, OP_INVERT, 0, 0, "multiplicative inverse of value",
	"iroot", 2, 2, 0, OP_NOP, qiroot, 0, "integer b'th root of a",
	"isassoc", 1, 1, 0, OP_ISASSOC, 0, 0, "whether a value is an association",
	"iseven", 1, 1, 0, OP_ISEVEN, 0, 0, "whether a value is an even integer",
	"isfile", 1, 1, 0, OP_ISFILE, 0, 0, "whether a value is a file",
	"isint", 1, 1, 0, OP_ISINT, 0, 0, "whether a value is an integer",
	"islist", 1, 1, 0, OP_ISLIST, 0, 0, "whether a value is a list",
	"ismat", 1, 1, 0, OP_ISMAT, 0, 0, "whether a value is a matrix",
	"ismult", 2, 2, 0, OP_NOP, f_ismult, 0, "whether a is a multiple of b",
	"isnull", 1, 1, 0, OP_ISNULL, 0, 0, "whether a value is the null value",
	"isnum", 1, 1, 0, OP_ISNUM, 0, 0, "whether a value is a number",
	"isobj", 1, 1, 0, OP_ISOBJ, 0, 0, "whether a value is an object",
	"isodd", 1, 1, 0, OP_ISODD, 0, 0, "whether a value is an odd integer",
	"isqrt", 1, 1, 0, OP_NOP, qisqrt, 0, "integer part of square root",
	"isreal", 1, 1, 0, OP_ISREAL, 0, 0, "whether a value is a real number",
	"isset", 2, 2, 0, OP_NOP, f_isset, 0, "whether bit b of abs(a) (in base 2) is set",
	"isstr", 1, 1, 0, OP_ISSTR, 0, 0, "whether a value is a string",
	"isrel", 2, 2, 0, OP_NOP, f_isrel, 0, "whether two numbers are relatively prime",
	"issimple", 1, 1, 0, OP_ISSIMPLE, 0, 0, "whether value is a simple type",
	"issq", 1, 1, 0, OP_NOP, f_issquare, 0, "whether or not number is a square",
 	"istype", 2, 2, 0, OP_ISTYPE, 0, 0, "whether the type of a is same as the type of b",
	"jacobi", 2, 2, 0, OP_NOP, qjacobi, 0, "-1 => a is not quadratic residue mod b\n\t\t  1 => b is composite, or a is quad residue of b",
	"lcm", 1, IN, 0, OP_NOP, f_lcm, 0, "least common multiple",
	"lcmfact", 1, 1, 0, OP_NOP, qlcmfact, 0, "lcm of all integers up till number",
	"lfactor", 2, 2, 0, OP_NOP, qlowfactor, 0, "lowest prime factor of a in first b primes",
	"list", 0, IN, 0, OP_NOP, 0, f_list, "create list of specified values",
	"ln", 1, 2, 0, OP_NOP, 0, f_ln, "natural logarithm of value a within accuracy b",
	"lowbit", 1, 1, 0, OP_NOP, f_lowbit, 0, "low bit number in base 2 representation",
	"ltol", 1, 2, FE, OP_NOP, f_legtoleg, 0, "leg-to-leg of unit right triangle (sqrt(1 - a^2))",
	"matdim", 1, 1, 0, OP_NOP, 0, f_matdim, "number of dimensions of matrix",
	"matfill", 2, 3, FA, OP_NOP, 0, f_matfill, "fill matrix with value b (value c on diagonal)",
	"matmax", 2, 2, 0, OP_NOP, 0, f_matmax, "maximum index of matrix a dim b",
	"matmin", 2, 2, 0, OP_NOP, 0, f_matmin, "minimum index of matrix a dim b",
	"mattrans", 1, 1, 0, OP_NOP, 0, f_mattrans, "transpose of matrix",
	"max", 1, IN, 0, OP_NOP, f_max, 0, "maximum value",
	"meq", 3, 3, 0, OP_NOP, f_meq, 0, "whether a and b are equal modulo c",
	"min", 1, IN, 0, OP_NOP, f_min, 0, "minimum value",
	"minv", 2, 2, 0, OP_NOP, qminv, 0, "inverse of a modulo b",
	"mmin", 2, 2, 0, OP_NOP, qminmod, 0, "a mod b value with smallest abs value",
	"mne", 3, 3, 0, OP_NOP, f_mne, 0, "whether a and b are not equal modulo c",
	"near", 2, 3, 0, OP_NOP, f_near, 0, "sign of (abs(a-b) - c)",
	"norm", 1, 1, 0, OP_NORM, 0, 0, "norm of a value (square of absolute value)",
	"null", 0, 0, 0, OP_UNDEF, 0, 0, "null value",
	"num", 1, 1, 0, OP_NUMERATOR, qnum, 0, "numerator of fraction",
	"ord", 1, 1, 0, OP_NOP, 0, f_ord, "integer corresponding to character value",
	"param", 1, 1, 0, OP_ARGVALUE, 0, 0, "value of parameter n (or parameter count if n\n\t\t    is zero)",
	"perm", 2, 2, 0, OP_NOP, qperm, 0, "permutation number a!/(a-b)!",
	"pfact", 1, 1, 0, OP_NOP, qpfact, 0, "product of primes up till number",
	"pi", 0, 1, FE, OP_NOP, qpi, 0, "value of pi accurate to within epsilon",
	"places", 1, 1, 0, OP_NOP, f_places, 0, "places after decimal point (-1 if infinite)",
	"pmod", 3, 3, 0, OP_NOP, qpowermod,0, "mod of a power (a ^ b (mod c))",
	"polar", 2, 3, 0, OP_NOP, 0, f_polar, "complex value of polar coordinate (a * exp(b*1i))",
	"poly", 2, IN, 0, OP_NOP, 0, f_poly, "(a1,a2,...,an,x) = a1*x^n+a2*x^(n-1)+...+an",
	"pop", 1, 1, FA, OP_NOP, 0, f_listpop, "pop value from front of list",
	"power", 2, 3, 0, OP_NOP, 0, f_power, "value a raised to the power b within accuracy c",
	"ptest", 2, 2, 0, OP_NOP, f_primetest, 0, "probabilistic primality test",
	"printf", 1, IN, 0, OP_NOP, 0, f_printf, "print formatted output to stdout",
	"prompt", 1, 1, 0, OP_NOP, 0, f_prompt, "prompt for input line using value a",
	"push", 2, 2, FA, OP_NOP, 0, f_listpush, "push value onto front of list",
	"quomod", 4, 4, 0, OP_QUOMOD, 0, 0, "set c and d to quotient and remainder of a\n\t\t    divided by b",
	"rcin", 2, 2, 0, OP_NOP, qredcin, 0, "convert normal number a to REDC number mod b",
	"rcmul", 3, 3, 0, OP_NOP, qredcmul, 0, "multiply REDC numbers a and b mod c",
	"rcout", 2, 2, 0, OP_NOP, qredcout, 0, "convert REDC number a mod b to normal number",
	"rcpow", 3, 3, 0, OP_NOP, qredcpower, 0, "raise REDC number a to power b mod c",
	"rcsq", 2, 2, 0, OP_NOP, qredcsquare, 0, "square REDC number a mod b",
	"re", 1, 1, 0, OP_RE, 0, 0, "real part of complex number",
	"remove", 1, 1, FA, OP_NOP, 0, f_listremove, "remove value from end of list",
	"root", 2, 3, 0, OP_NOP, 0, f_root, "value a taken to the b'th root within accuracy c",
	"round", 1, 2, 0, OP_NOP, 0, f_round, "round value a to b number of decimal places",
	"rsearch", 2, 3, 0, OP_NOP, 0, f_rsearch, "reverse search matrix or list for value b\n\t\t    starting at index c",
	"runtime", 0, 0, 0, OP_NOP, f_runtime, 0, "user mode cpu time in seconds",
	"scale", 2, 2, 0, OP_SCALE, 0, 0, "scale value up or down by a power of two",
	"search", 2, 3, 0, OP_NOP, 0, f_search, "search matrix or list for value b starting\n\t\t    at index c",
	"sgn", 1, 1, 0, OP_SGN, qsign, 0, "sign of value (-1, 0, 1)",
	"sin", 1, 2, 0, OP_NOP, 0, f_sin, "sine of value a within accuracy b",
	"sinh", 1, 2, FE, OP_NOP, qsinh, 0, "hyperbolic sine of a within accuracy b",
	"size", 1, 1, 0, OP_NOP, 0, f_size, "total number of elements in value",
	"sqrt", 1, 2, 0, OP_NOP, 0, f_sqrt, "square root of value a within accuracy b",
	"ssq", 1, IN, 0, OP_NOP, 0, f_ssq, "sum of squares of values",
	"str", 1, 1, 0, OP_NOP, 0, f_str, "simple value converted to string",
	"strcat", 1,IN, 0, OP_NOP, 0, f_strcat, "concatenate strings together",
	"strlen", 1, 1, 0, OP_NOP, 0, f_strlen, "length of string",
	"strprintf", 1, IN, 0, OP_NOP, 0, f_strprintf, "return formatted output as a string",
	"substr", 3, 3, 0, OP_NOP, 0, f_substr, "substring of a from position b for c chars",
	"swap", 2, 2, 0, OP_SWAP, 0, 0, "swap values of variables a and b (can be dangerous)",
	"tan", 1, 2, FE, OP_NOP, qtan, 0, "tangent of a within accuracy b",
	"tanh", 1, 2, FE, OP_NOP, qtanh, 0, "hyperbolic tangent of a within accuracy b",
	"trunc", 1, 2, 0, OP_NOP, f_trunc, 0, "truncate a to b number of decimal places",
	"xor", 1, IN, 0, OP_NOP, f_xor, 0, "logical xor",
	NULL, 0, 0, 0, OP_NOP, 0, 0, NULL /* end of table */
};


/*
 * Call a built-in function.
 * Arguments to the function are on the stack, but are not removed here.
 * Functions are either purely numeric, or else can take any value type.
 */
VALUE
builtinfunc(index, argcount, stck)
	int argcount;
	long index;
	VALUE *stck;		/* arguments on the stack */
{
	VALUE *sp;		/* pointer to stack entries */
	VALUE **vpp;		/* pointer to current value address */
	struct builtin *bp;	/* builtin function to be called */
	long i;			/* index */
	NUMBER *numargs[IN];	/* numeric arguments for function */
	VALUE *valargs[IN];	/* addresses of actual arguments */
	VALUE result;		/* general result of function */

	if ((unsigned long)index >= (sizeof(builtins) / sizeof(builtins[0])) - 1)
		math_error("Bad built-in function index");
	bp = &builtins[index];
	if (argcount < bp->b_minargs)
		math_error("Too few arguments for builtin function \"%s\"", bp->b_name);
	if ((argcount > bp->b_maxargs) || (argcount > IN))
		math_error("Too many arguments for builtin function \"%s\"", bp->b_name);
	/*
	 * If an address was passed, then point at the real variable,
	 * otherwise point at the stack value itself (unless the function
	 * is very special).
	 */
	sp = stck - argcount + 1;
	vpp = valargs;
	for (i = argcount; i > 0; i--) {
		if ((sp->v_type != V_ADDR) || (bp->b_flags & FA))
			*vpp = sp;
		else
			*vpp = sp->v_addr;
		sp++;
		vpp++;
	}
	/*
	 * Handle general values if the function accepts them.
	 */
	if (bp->b_valfunc) {
		vpp = valargs;
		if ((bp->b_minargs == 1) && (bp->b_maxargs == 1))
			result = (*bp->b_valfunc)(vpp[0]);
		else if ((bp->b_minargs == 2) && (bp->b_maxargs == 2))
			result = (*bp->b_valfunc)(vpp[0], vpp[1]);
		else if ((bp->b_minargs == 3) && (bp->b_maxargs == 3))
			result = (*bp->b_valfunc)(vpp[0], vpp[1], vpp[2]);
		else
			result = (*bp->b_valfunc)(argcount, vpp);
		return result;
	}
	/*
	 * Function must be purely numeric, so handle that.
	 */
	vpp = valargs;
	for (i = 0; i < argcount; i++) {
		if ((*vpp)->v_type != V_NUM)
			math_error("Non-real argument for builtin function %s", bp->b_name);
		numargs[i] = (*vpp)->v_num;
		vpp++;
	}
	result.v_type = V_NUM;
	if (!(bp->b_flags & FE) && (bp->b_minargs != bp->b_maxargs)) {
		result.v_num = (*bp->b_numfunc)(argcount, numargs);
		return result;
	}
	if ((bp->b_flags & FE) && (argcount < bp->b_maxargs))
		numargs[argcount++] = _epsilon_;

	switch (argcount) {
		case 0:
			result.v_num = (*bp->b_numfunc)();
			break;
		case 1:
			result.v_num = (*bp->b_numfunc)(numargs[0]);
			break;
		case 2:
			result.v_num = (*bp->b_numfunc)(numargs[0], numargs[1]);
			break;
		case 3:
			result.v_num = (*bp->b_numfunc)(numargs[0], numargs[1], numargs[2]);
			break;
		default:
			math_error("Bad builtin function call");
	}
	return result;
}


static VALUE
f_eval(vp)
	VALUE *vp;
{
	FUNC	*oldfunc;
	FUNC	*newfunc;
	VALUE	result;

	if (vp->v_type != V_STR)
		math_error("Evaluating non-string argument");
	(void) openstring(vp->v_str);
	oldfunc = curfunc;
	enterfilescope();
	if (evaluate(TRUE)) {
		exitfilescope();
		freevalue(stack--);
		newfunc = curfunc;
		curfunc = oldfunc;
		result = newfunc->f_savedvalue;
		newfunc->f_savedvalue.v_type = V_NULL;
		if (newfunc != oldfunc)
			free(newfunc);
		return result;
	}
	exitfilescope();
	newfunc = curfunc;
	curfunc = oldfunc;
	freevalue(&newfunc->f_savedvalue);
	newfunc->f_savedvalue.v_type = V_NULL;
	if (newfunc != oldfunc)
		free(newfunc);
	math_error("Evaluation error");
	/*NOTREACHED*/
	abort ();
}


static VALUE
f_prompt(vp)
	VALUE *vp;
{
	VALUE result;
	char *cp;
	char *newcp;

	if (inputisterminal()) {
		printvalue(vp, PRINT_SHORT);
		math_flush();
	}
	cp = nextline();
	if (cp == NULL)
		math_error("End of file while prompting");
	if (*cp == '\0') {
		result.v_type = V_STR;
		result.v_subtype = V_STRLITERAL;
		result.v_str = "";
		return result;
	}
	newcp = (char *)malloc(strlen(cp) + 1);
	if (newcp == NULL)
		math_error("Cannot allocate string");
	strcpy(newcp, cp);
	result.v_str = newcp;
	result.v_type = V_STR;
	result.v_subtype = V_STRALLOC;
	return result;
}


static VALUE
f_str(vp)
	VALUE *vp;
{
	VALUE result;
	static char *cp;

	switch (vp->v_type) {
		case V_STR:
			copyvalue(vp, &result);
			return result;
		case V_NULL:
			result.v_str = "";
			result.v_type = V_STR;
			result.v_subtype = V_STRLITERAL;
			return result;
		case V_NUM:
			math_divertio();
			qprintnum(vp->v_num, MODE_DEFAULT);
			cp = math_getdivertedio();
			break;
		case V_COM:
			math_divertio();
			comprint(vp->v_com);
			cp = math_getdivertedio();
			break;
		default:
			math_error("Non-simple type for string conversion");
	}
	result.v_str = cp;
	result.v_type = V_STR;
	result.v_subtype = V_STRALLOC;
	return result;
}


static VALUE
f_poly(count, vals)
	int count;
	VALUE **vals;
{
	VALUE *x;
	VALUE result, tmp;

	x = vals[--count];
	copyvalue(*vals++, &result);
	while (--count > 0) {
		mulvalue(&result, x, &tmp);
		freevalue(&result);
		addvalue(*vals++, &tmp, &result);
		freevalue(&tmp);
	}
	return result;
}


static NUMBER *
f_mne(val1, val2, val3)
	NUMBER *val1, *val2, *val3;
{
	return itoq((long) qcmpmod(val1, val2, val3));
}


static NUMBER *
f_isrel(val1, val2)
	NUMBER *val1, *val2;
{
	if (qisfrac(val1) || qisfrac(val2))
		math_error("Non-integer for isrel");
	return itoq((long) zrelprime(val1->num, val2->num));
}


static NUMBER *
f_issquare(vp)
	NUMBER *vp;
{
	return itoq((long) qissquare(vp));
}


static NUMBER *
f_primetest(val1, val2)
	NUMBER *val1, *val2;
{
	return itoq((long) qprimetest(val1, val2));
}


static NUMBER *
f_isset(val1, val2)
	NUMBER *val1, *val2;
{
	if (qisfrac(val2))
		math_error("Non-integral bit position");
	if (qiszero(val1) || (qisint(val1) && qisneg(val2)))
		return qlink(&_qzero_);
	if (zisbig(val2->num)) {
		if (qisneg(val2))
			math_error("Very large bit position");
		return qlink(&_qzero_);
	}
	return itoq((long) qisset(val1, qtoi(val2)));
}


static NUMBER *
f_digit(val1, val2)
	NUMBER *val1, *val2;
{
	if (qisfrac(val2))
		math_error("Non-integral digit position");
	if (qiszero(val1) || (qisint(val1) && qisneg(val2)))
		return qlink(&_qzero_);
	if (zisbig(val2->num)) {
		if (qisneg(val2))
			math_error("Very large digit position");
		return qlink(&_qzero_);
	}
	return itoq((long) qdigit(val1, qtoi(val2)));
}


static NUMBER *
f_digits(val)
	NUMBER *val;
{
	return itoq((long) qdigits(val));
}


static NUMBER *
f_places(val)
	NUMBER *val;
{
	return itoq((long) qplaces(val));
}


static NUMBER *
f_xor(count, vals)
	int count;
	NUMBER **vals;
{
	NUMBER *val, *tmp;

	val = qlink(*vals);
	while (--count > 0) {
		tmp = qxor(val, *++vals);
		qfree(val);
		val = tmp;
	}
	return val;
}


static NUMBER *
f_min(count, vals)
	int count;
	NUMBER **vals;
{
	NUMBER *val, *tmp;

	val = qlink(*vals);
	while (--count > 0) {
		tmp = qmin(val, *++vals);
		qfree(val);
		val = tmp;
	}
	return val;
}


static NUMBER *
f_max(count, vals)
	int count;
	NUMBER **vals;
{
	NUMBER *val, *tmp;

	val = qlink(*vals);
	while (--count > 0) {
		tmp = qmax(val, *++vals);
		qfree(val);
		val = tmp;
	}
	return val;
}


static NUMBER *
f_gcd(count, vals)
	int count;
	NUMBER **vals;
{
	NUMBER *val, *tmp;

	val = qabs(*vals);
	while (--count > 0) {
		tmp = qgcd(val, *++vals);
		qfree(val);
		val = tmp;
	}
	return val;
}


static NUMBER *
f_lcm(count, vals)
	int count;
	NUMBER **vals;
{
	NUMBER *val, *tmp;

	val = qabs(*vals);
	while (--count > 0) {
		tmp = qlcm(val, *++vals);
		qfree(val);
		val = tmp;
		if (qiszero(val))
			break;
	}
	return val;
}


static VALUE
f_hash(count, vals)
	int count;
	VALUE **vals;
{
	HASH hash;
	long lhash;
	VALUE result;

	hash = 0;
	while (count-- > 0)
		hash = hash * 947369 + hashvalue(*vals++);
	lhash = (long) hash;
	if (lhash < 0)
		lhash = -lhash;
	if (lhash < 0)
		lhash = 0;
	result.v_num = itoq(lhash);
	result.v_type = V_NUM;
	return result;
}


static VALUE
f_avg(count, vals)
	int count;
	VALUE **vals;
{
	int i;
	VALUE result;
	VALUE tmp;
	VALUE div;

	result.v_num = qlink(&_qzero_);
	result.v_type = V_NUM;
	for (i = count; i > 0; i--) {
		addvalue(&result, *vals++, &tmp);
		freevalue(&result);
		result = tmp;
	}
	if (count <= 1)
		return result;
	div.v_num = itoq((long) count);
	div.v_type = V_NUM;
	divvalue(&result, &div, &tmp);
	qfree(div.v_num);
	return tmp;
}


static NUMBER *
f_hmean(count, vals)
	int count;
	NUMBER **vals;
{
	NUMBER *val, *tmp, *tmp2, *num;

	num = itoq(count);
	val = qinv(*vals);
	while (--count > 0) {
		tmp2 = qinv(*++vals);
		tmp = qadd(val, tmp2);
		qfree(tmp2);
		qfree(val);
		val = tmp;
	}
	tmp = qdiv(num, val);
	qfree(num);
	qfree(val);
	return tmp;
}


static VALUE
f_ssq(count, vals)
	int count;
	VALUE **vals;
{
	VALUE result, tmp1, tmp2;

	squarevalue(*vals++, &result);
	while (--count > 0) {
		squarevalue(*vals++, &tmp1);
		addvalue(&tmp1, &result, &tmp2);
		freevalue(&tmp1);
		freevalue(&result);
		result = tmp2;
	}
	return result;
}


static NUMBER *
f_ismult(val1, val2)
	NUMBER *val1, *val2;
{
	return itoq((long) qdivides(val1, val2));
}


static NUMBER *
f_meq(val1, val2, val3)
	NUMBER *val1, *val2, *val3;
{
	NUMBER *tmp, *res;

	tmp = qsub(val1, val2);
	res = itoq((long) qdivides(tmp, val3));
	qfree(tmp);
	return res;
}


static VALUE
f_exp(count, vals)
	int count;
	VALUE **vals;
{
	VALUE result;
	NUMBER *err;

	err = _epsilon_;
	if (count == 2) {
		if (vals[1]->v_type != V_NUM)
			math_error("Non-real epsilon value for exp");
		err = vals[1]->v_num;
	}
	switch (vals[0]->v_type) {
		case V_NUM:
			result.v_num = qexp(vals[0]->v_num, err);
			result.v_type = V_NUM;
			break;
		case V_COM:
			result.v_com = cexp(vals[0]->v_com, err);
			result.v_type = V_COM;
			break;
		default:
			math_error("Bad argument type for exp");
	}
	return result;
}


static VALUE
f_ln(count, vals)
	int count;
	VALUE **vals;
{
	VALUE result;
	COMPLEX ctmp;
	NUMBER *err;

	err = _epsilon_;
	if (count == 2) {
		if (vals[1]->v_type != V_NUM)
			math_error("Non-real epsilon value for ln");
		err = vals[1]->v_num;
	}
	switch (vals[0]->v_type) {
		case V_NUM:
			if (!qisneg(vals[0]->v_num) && !qiszero(vals[0]->v_num)) {
				result.v_num = qln(vals[0]->v_num, err);
				result.v_type = V_NUM;
				break;
			}
			ctmp.real = vals[0]->v_num;
			ctmp.imag = &_qzero_;
			ctmp.links = 1;
			result.v_com = cln(&ctmp, err);
			result.v_type = V_COM;
			break;
		case V_COM:
			result.v_com = cln(vals[0]->v_com, err);
			result.v_type = V_COM;
			break;
		default:
			math_error("Bad argument type for ln");
	}
	return result;
}


static VALUE
f_cos(count, vals)
	int count;
	VALUE **vals;
{
	VALUE result;
	COMPLEX *c;
	NUMBER *err;

	err = _epsilon_;
	if (count == 2) {
		if (vals[1]->v_type != V_NUM)
			math_error("Non-real epsilon value for cos");
		err = vals[1]->v_num;
	}
	switch (vals[0]->v_type) {
		case V_NUM:
			result.v_num = qcos(vals[0]->v_num, err);
			result.v_type = V_NUM;
			break;
		case V_COM:
			c = ccos(vals[0]->v_com, err);
			result.v_com = c;
			result.v_type = V_COM;
			if (cisreal(c)) {
				result.v_num = qlink(c->real);
				result.v_type = V_NUM;
				comfree(c);
			}
			break;
		default:
			math_error("Bad argument type for cos");
	}
	return result;
}


static VALUE
f_sin(count, vals)
	int count;
	VALUE **vals;
{
	VALUE result;
	COMPLEX *c;
	NUMBER *err;

	err = _epsilon_;
	if (count == 2) {
		if (vals[1]->v_type != V_NUM)
			math_error("Non-real epsilon value for sin");
		err = vals[1]->v_num;
	}
	switch (vals[0]->v_type) {
		case V_NUM:
			result.v_num = qsin(vals[0]->v_num, err);
			result.v_type = V_NUM;
			break;
		case V_COM:
			c = csin(vals[0]->v_com, err);
			result.v_com = c;
			result.v_type = V_COM;
			if (cisreal(c)) {
				result.v_num = qlink(c->real);
				result.v_type = V_NUM;
				comfree(c);
			}
			break;
		default:
			math_error("Bad argument type for sin");
	}
	return result;
}


static VALUE
f_arg(count, vals)
	int count;
	VALUE **vals;
{
	VALUE result;
	COMPLEX *c;
	NUMBER *err;

	err = _epsilon_;
	if (count == 2) {
		if (vals[1]->v_type != V_NUM)
			math_error("Non-real epsilon value for arg");
		err = vals[1]->v_num;
	}
	result.v_type = V_NUM;
	switch (vals[0]->v_type) {
		case V_NUM:
			if (qisneg(vals[0]->v_num))
				result.v_num = qpi(err);
			else
				result.v_num = qlink(&_qzero_);
			break;
		case V_COM:
			c = vals[0]->v_com;
			if (ciszero(c))
				result.v_num = qlink(&_qzero_);
			else
				result.v_num = qatan2(c->imag, c->real, err);
			break;
		default:
			math_error("Bad argument type for arg");
	}
	return result;
}


static NUMBER *
f_legtoleg(val1, val2)
	NUMBER *val1, *val2;
{
	return qlegtoleg(val1, val2, FALSE);
}


static NUMBER *
f_trunc(count, vals)
	int count;
	NUMBER **vals;
{
	NUMBER *val;

	val = &_qzero_;
	if (count == 2)
		val = vals[1];
	return qtrunc(*vals, val);
}


static VALUE
f_bround(count, vals)
	int count;
	VALUE **vals;
{
	VALUE *vp, tmp, res;

	if (count > 1)
		vp = vals[1];
	else {
		tmp.v_type = V_INT;
		tmp.v_num = 0;
		vp = &tmp;
	}
	broundvalue(vals[0], vp, &res);
	return res;
}


static VALUE
f_round(count, vals)
	int count;
	VALUE **vals;
{
	VALUE *vp, tmp, res;

	if (count > 1)
		vp = vals[1];
	else {
		tmp.v_type = V_INT;
		tmp.v_num = 0;
		vp = &tmp;
	}
	roundvalue(vals[0], vp, &res);
	return res;
}


static NUMBER *
f_btrunc(count, vals)
	int count;
	NUMBER **vals;
{
	NUMBER *val;

	val = &_qzero_;
	if (count == 2)
		val = vals[1];
	return qbtrunc(*vals, val);
}


static NUMBER *
f_near(count, vals)
	int count;
	NUMBER **vals;
{
	NUMBER *val;

	val = _epsilon_;
	if (count == 3)
		val = vals[2];
	return itoq((long) qnear(vals[0], vals[1], val));
}


static NUMBER *
f_cfsim(val)
	NUMBER *val;
{
	return qcfappr(val, NULL);
}


static NUMBER *
f_ceil(val)
	NUMBER *val;
{
	NUMBER *val2;

	if (qisint(val))
		return qlink(val);
	val2 = qint(val);
	if (qisneg(val))
		return val2;
	val = qinc(val2);
	qfree(val2);
	return val;
}


static NUMBER *
f_floor(val)
	NUMBER *val;
{
	NUMBER *val2;

	if (qisint(val))
		return qlink(val);
	val2 = qint(val);
	if (!qisneg(val))
		return val2;
	val = qdec(val2);
	qfree(val2);
	return val;
}


static NUMBER *
f_highbit(val)
	NUMBER *val;
{
	if (qiszero(val))
		math_error("Highbit of zero");
	if (qisfrac(val))
		math_error("Highbit of non-integer");
	return itoq(zhighbit(val->num));
}


static NUMBER *
f_lowbit(val)
	NUMBER *val;
{
	if (qiszero(val))
		math_error("Lowbit of zero");
	if (qisfrac(val))
		math_error("Lowbit of non-integer");
	return itoq(zlowbit(val->num));
}


static VALUE
f_sqrt(count, vals)
	int count;
	VALUE **vals;
{
	VALUE *vp, err, result;

	if (count > 1)
		vp = vals[1];
	else {
		err.v_num = _epsilon_;
		err.v_type = V_NUM;
		vp = &err;
	}
	sqrtvalue(vals[0], vp, &result);
	return result;
}


static VALUE
f_root(count, vals)
	int count;
	VALUE **vals;
{
	VALUE *vp, err, result;

	if (count > 2)
		vp = vals[3];
	else {
		err.v_num = _epsilon_;
		err.v_type = V_NUM;
		vp = &err;
	}
	rootvalue(vals[0], vals[1], vp, &result);
	return result;
}


static VALUE
f_power(count, vals)
	int count;
	VALUE **vals;
{
	VALUE *vp, err, result;

	if (count > 2)
		vp = vals[2];
	else {
		err.v_num = _epsilon_;
		err.v_type = V_NUM;
		vp = &err;
	}
	powervalue(vals[0], vals[1], vp, &result);
	return result;
}


static VALUE
f_polar(count, vals)
	int count;
	VALUE **vals;
{
	VALUE *vp, err, result;
	COMPLEX *c;

	if (count > 2)
		vp = vals[2];
	else {
		err.v_num = _epsilon_;
		err.v_type = V_NUM;
		vp = &err;
	}
	if ((vals[0]->v_type != V_NUM) || (vals[1]->v_type != V_NUM))
		math_error("Non-real argument for polar");
	if ((vp->v_type != V_NUM) || qisneg(vp->v_num) || qiszero(vp->v_num))
		math_error("Bad epsilon value for polar");
	c = cpolar(vals[0]->v_num, vals[1]->v_num, vp->v_num);
	result.v_com = c;
	result.v_type = V_COM;
	if (cisreal(c)) {
		result.v_num = qlink(c->real);
		result.v_type = V_NUM;
		comfree(c);
	}
	return result;
}


static NUMBER *
f_ilog(val1, val2)
	NUMBER *val1, *val2;
{
	return itoq(qilog(val1, val2));
}


static NUMBER *
f_ilog2(val)
	NUMBER *val;
{
	return itoq(qilog2(val));
}


static NUMBER *
f_ilog10(val)
	NUMBER *val;
{
	return itoq(qilog10(val));
}


static NUMBER *
f_faccnt(val1, val2)
	NUMBER *val1, *val2;
{
	return itoq(qdivcount(val1, val2));
}


static VALUE
f_matfill(count, vals)
	int count;
	VALUE **vals;
{
	VALUE *v1, *v2, *v3;
	VALUE result;

	v1 = vals[0];
	v2 = vals[1];
	v3 = (count == 3) ? vals[2] : NULL;
	if (v1->v_type != V_ADDR)
		math_error("Non-variable argument for matfill");
	v1 = v1->v_addr;
	if (v1->v_type != V_MAT)
		math_error("Non-matrix for matfill");
	if (v2->v_type == V_ADDR)
		v2 = v2->v_addr;
	if (v3 && (v3->v_type == V_ADDR))
		v3 = v3->v_addr;
	matfill(v1->v_mat, v2, v3);
	result.v_type = V_NULL;
	return result;
}


static VALUE
f_mattrans(vp)
	VALUE *vp;
{
	VALUE result;

	if (vp->v_type != V_MAT)
		math_error("Non-matrix argument for mattrans");
	result.v_type = V_MAT;
	result.v_mat = mattrans(vp->v_mat);
	return result;
}


static VALUE
f_det(vp)
	VALUE *vp;
{
	if (vp->v_type != V_MAT)
		math_error("Non-matrix argument for det");
	return matdet(vp->v_mat);
}


static VALUE
f_matdim(vp)
	VALUE *vp;
{
	VALUE result;

	if (vp->v_type != V_MAT)
		math_error("Non-matrix argument for matdim");
	result.v_type = V_NUM;
	result.v_num = itoq((long) vp->v_mat->m_dim);
	return result;
}


static VALUE
f_matmin(v1, v2)
	VALUE *v1, *v2;
{
	VALUE result;
	NUMBER *q;
	long i;

	if ((v1->v_type != V_MAT) || (v2->v_type != V_NUM))
		math_error("Bad argument type for matmin");
	q = v2->v_num;
	i = qtoi(q);
	if (qisfrac(q) || qisneg(q) || (i <= 0) || (i > v1->v_mat->m_dim))
		math_error("Bad dimension value for matmin");
	result.v_type = V_NUM;
	result.v_num = itoq(v1->v_mat->m_min[i - 1]);
	return result;
}


static VALUE
f_matmax(v1, v2)
	VALUE *v1, *v2;
{
	VALUE result;
	NUMBER *q;
	long i;

	if ((v1->v_type != V_MAT) || (v2->v_type != V_NUM))
		math_error("Bad argument type for matmax");
	q = v2->v_num;
	i = qtoi(q);
	if (qisfrac(q) || qisneg(q) || (i <= 0) || (i > v1->v_mat->m_dim))
		math_error("Bad dimension value for matmax");
	result.v_type = V_NUM;
	result.v_num = itoq(v1->v_mat->m_max[i - 1]);
	return result;
}


static VALUE
f_cp(v1, v2)
	VALUE *v1, *v2;
{
	VALUE result;

	if ((v1->v_type != V_MAT) || (v2->v_type != V_MAT))
		math_error("Non-matrix argument for cross product");
	result.v_type = V_MAT;
	result.v_mat = matcross(v1->v_mat, v2->v_mat);
	return result;
}


static VALUE
f_dp(v1, v2)
	VALUE *v1, *v2;
{
	if ((v1->v_type != V_MAT) || (v2->v_type != V_MAT))
		math_error("Non-matrix argument for dot product");
	return matdot(v1->v_mat, v2->v_mat);
}


static VALUE
f_strlen(vp)
	VALUE *vp;
{
	VALUE result;

	if (vp->v_type != V_STR)
		math_error("Non-string argument for strlen");
	result.v_type = V_NUM;
	result.v_num = itoq((long) strlen(vp->v_str));
	return result;
}


static VALUE
f_strcat(count, vals)
	int count;
	VALUE **vals;
{
	register VALUE **vp;
	register char *cp;
	int i;
	long len;
	long lengths[IN];
	VALUE result;

	len = 1;
	vp = vals;
	for (i = 0; i < count; i++) {
		if ((*vp)->v_type != V_STR)
			math_error("Non-string argument for strcat");
		lengths[i] = strlen((*vp)->v_str);
		len += lengths[i];
		vp++;
	}
	cp = (char *)malloc(len);
	if (cp == NULL)
		math_error("No memory for strcat");
	result.v_str = cp;
	result.v_type = V_STR;
	result.v_subtype = V_STRALLOC;
	i = 0;
	for (vp = vals; count-- > 0; vp++) {
		strcpy(cp, (*vp)->v_str);
		cp += lengths[i++];
	}
	return result;
}


static VALUE
f_substr(v1, v2, v3)
	VALUE *v1, *v2, *v3;
{
	NUMBER *q1, *q2;
	long i1, i2, len;
	char *cp;
	VALUE result;

	if (v1->v_type != V_STR)
		math_error("Non-string argument for substr");
	if ((v2->v_type != V_NUM) || (v3->v_type != V_NUM))
		math_error("Non-numeric positions for substr");
	q1 = v2->v_num;
	q2 = v3->v_num;
	if (qisfrac(q1) || qisneg(q1) || qisfrac(q2) || qisneg(q2))
		math_error("Illegal positions for substr");
	i1 = qtoi(q1);
	i2 = qtoi(q2);
	cp = v1->v_str;
	len = strlen(cp);
	result.v_type = V_STR;
	if (i1 > 0)
		i1--;
	if (i1 >= len) {	/* indexing off of end */
		result.v_subtype = V_STRLITERAL;
		result.v_str = "";
		return result;
	}
	cp += i1;
	len -= i1;
	if ((i2 >= len) && (v1->v_subtype == V_STRLITERAL)) {
		result.v_subtype = V_STRLITERAL;
		result.v_str = cp;
		return result;
	}
	if (len > i2)
		len = i2;
	if (len == 1) {
		result.v_subtype = V_STRLITERAL;
		result.v_str = charstr(*cp);
		return result;
	}
	result.v_subtype = V_STRALLOC;
	result.v_str = (char *)malloc(len + 1);
	if (result.v_str == NULL)
		math_error("No memory for substr");
	strncpy(result.v_str, cp, len);
	result.v_str[len] = '\0';
	return result;
}


static VALUE
f_char(vp)
	VALUE *vp;
{
	long num;
	NUMBER *q;
	VALUE result;

	if (vp->v_type != V_NUM)
		math_error("Non-numeric argument for char");
	q = vp->v_num;
	num = qtoi(q);
	if (qisneg(q) || qisfrac(q) || zisbig(q->num) || (num > 255))
		math_error("Illegal number for char");
	result.v_type = V_STR;
	result.v_subtype = V_STRLITERAL;
	result.v_str = charstr((int) num);
	return result;
}


static VALUE
f_ord(vp)
	VALUE *vp;
{
	char *str;
	VALUE result;

	if (vp->v_type != V_STR)
		math_error("Non-string argument for ord");
	str = vp->v_str;
	if (str[0] && str[1])
		math_error("Multi-character string given for ord");
	result.v_type = V_NUM;
	result.v_num = itoq((long) (*str & 0xff));
	return result;
}


static VALUE
f_size(vp)
	VALUE *vp;
{
	long count;
	VALUE result;

	switch (vp->v_type) {
		case V_NULL:	count = 0; break;
		case V_MAT:	count = vp->v_mat->m_size; break;
		case V_LIST:	count = vp->v_list->l_count; break;
		case V_ASSOC:	count = vp->v_assoc->a_count; break;
		case V_OBJ:	count = vp->v_obj->o_actions->count; break;
		default:	count = 1; break;
	}
	result.v_type = V_NUM;
	result.v_num = itoq(count);
	return result;
}


static VALUE
f_search(count, vals)
	int count;
	VALUE **vals;
{
	VALUE *v1, *v2;
	NUMBER *q;
	long start;
	long index = -1;
	VALUE result;

	v1 = *vals++;
	v2 = *vals++;
	start = 0;
	if (count == 3) {
		if ((*vals)->v_type != V_NUM)
			math_error("Non-numeric start index for search");
		q = (*vals)->v_num;
		if (qisfrac(q) || qisneg(q))
			math_error("Bad start index for search");
		start = qtoi(q);
	}
	switch (v1->v_type) {
		case V_MAT:
			index = matsearch(v1->v_mat, v2, start);
			break;
		case V_LIST:
			index = listsearch(v1->v_list, v2, start);
			break;
		case V_ASSOC:
			index = assocsearch(v1->v_assoc, v2, start);
			break;
		default:
			math_error("Bad argument type for search");
	}
	result.v_type = V_NULL;
	if (index >= 0) {
		result.v_type = V_NUM;
		result.v_num = itoq(index);
	}
	return result;
}


static VALUE
f_rsearch(count, vals)
	int count;
	VALUE **vals;
{
	VALUE *v1, *v2;
	NUMBER *q;
	long start;
	long index = -1;
	VALUE result;

	v1 = *vals++;
	v2 = *vals++;
	start = MAXFULL;
	if (count == 3) {
		if ((*vals)->v_type != V_NUM)
			math_error("Non-numeric start index for rsearch");
		q = (*vals)->v_num;
		if (qisfrac(q) || qisneg(q))
			math_error("Bad start index for rsearch");
		start = qtoi(q);
	}
	switch (v1->v_type) {
		case V_MAT:
			index = matrsearch(v1->v_mat, v2, start);
			break;
		case V_LIST:
			index = listrsearch(v1->v_list, v2, start);
			break;
		case V_ASSOC:
			index = assocrsearch(v1->v_assoc, v2, start);
			break;
		default:
			math_error("Bad argument type for rsearch");
	}
	result.v_type = V_NULL;
	if (index >= 0) {
		result.v_type = V_NUM;
		result.v_num = itoq(index);
	}
	return result;
}


static VALUE
f_list(count, vals)
	int count;
	VALUE **vals;
{
	VALUE result;

	result.v_type = V_LIST;
	result.v_list = listalloc();
	while (count-- > 0)
		insertlistlast(result.v_list, *vals++);
	return result;
}


/*ARGSUSED*/
static VALUE
f_assoc(count, vals)
	int count;
	VALUE **vals;
{
	VALUE result;

	result.v_type = V_ASSOC;
	result.v_assoc = assocalloc(0L);
	return result;
}


static VALUE
f_listinsert(v1, v2, v3)
	VALUE *v1, *v2, *v3;
{
	VALUE result;

	if ((v1->v_type != V_ADDR) || (v1->v_addr->v_type != V_LIST))
		math_error("Inserting into non-list variable");
	if (v2->v_type == V_ADDR)
		v2 = v2->v_addr;
	if ((v2->v_type != V_NUM) || qisfrac(v2->v_num))
		math_error("Non-integral index for list insert");
	if (v3->v_type == V_ADDR)
		v3 = v3->v_addr;
	insertlistmiddle(v1->v_addr->v_list, qtoi(v2->v_num), v3);
	result.v_type = V_NULL;
	return result;
}


static VALUE
f_listpush(v1, v2)
	VALUE *v1, *v2;
{
	VALUE result;

	if ((v1->v_type != V_ADDR) || (v1->v_addr->v_type != V_LIST))
		math_error("Pushing onto non-list variable");
	if (v2->v_type == V_ADDR)
		v2 = v2->v_addr;
	insertlistfirst(v1->v_addr->v_list, v2);
	result.v_type = V_NULL;
	return result;
}


static VALUE
f_listappend(v1, v2)
	VALUE *v1, *v2;
{
	VALUE result;

	if ((v1->v_type != V_ADDR) || (v1->v_addr->v_type != V_LIST))
		math_error("Appending to non-list variable");
	if (v2->v_type == V_ADDR)
		v2 = v2->v_addr;
	insertlistlast(v1->v_addr->v_list, v2);
	result.v_type = V_NULL;
	return result;
}


static VALUE
f_listdelete(v1, v2)
	VALUE *v1, *v2;
{
	VALUE result;

	if ((v1->v_type != V_ADDR) || (v1->v_addr->v_type != V_LIST))
		math_error("Deleting from non-list variable");
	if (v2->v_type == V_ADDR)
		v2 = v2->v_addr;
	if ((v2->v_type != V_NUM) || qisfrac(v2->v_num))
		math_error("Non-integral index for list delete");
	removelistmiddle(v1->v_addr->v_list, qtoi(v2->v_num), &result);
	return result;
}


static VALUE
f_listpop(vp)
	VALUE *vp;
{
	VALUE result;

	if ((vp->v_type != V_ADDR) || (vp->v_addr->v_type != V_LIST))
		math_error("Popping from non-list variable");
	removelistfirst(vp->v_addr->v_list, &result);
	return result;
}


static VALUE
f_listremove(vp)
	VALUE *vp;
{
	VALUE result;

	if ((vp->v_type != V_ADDR) || (vp->v_addr->v_type != V_LIST))
		math_error("Removing from non-list variable");
	removelistlast(vp->v_addr->v_list, &result);
	return result;
}


/*
 * Return the current runtime of calc in seconds.
 * This is the user mode time only.
 */
static NUMBER *
f_runtime()
{
	struct tms buf;

	times(&buf);
	return iitoq((long) buf.tms_utime, (long) CLK_TCK);
}


static VALUE
f_fopen(v1, v2)
	VALUE *v1, *v2;
{
	VALUE result;
	FILEID id;

	if (v1->v_type != V_STR)
		math_error("Non-string filename for fopen");
	if (v2->v_type != V_STR)
		math_error("Non-string mode for fopen");
	id = openid(v1->v_str, v2->v_str);
	if (id == FILEID_NONE) {
		result.v_type = V_NUM;
		result.v_num = itoq((long) errno);
	} else {
		result.v_type = V_FILE;
		result.v_file = id;
	}
	return result;
}


static VALUE
f_fclose(vp)
	VALUE *vp;
{
	VALUE result;

	if (vp->v_type != V_FILE)
		math_error("Non-file for fclose");
	if (closeid(vp->v_file)) {
		result.v_type = V_NUM;
		result.v_num = itoq((long) errno);
	} else
		result.v_type = V_NULL;
	return result;
}


static VALUE
f_ferror(vp)
	VALUE *vp;
{
	VALUE result;

	if (vp->v_type != V_FILE)
		math_error("Non-file for ferror");
	result.v_type = V_NUM;
	result.v_num = itoq((long) errorid(vp->v_file));
	return result;
}


static VALUE
f_feof(vp)
	VALUE *vp;
{
	VALUE result;

	if (vp->v_type != V_FILE)
		math_error("Non-file for feof");
	result.v_type = V_NUM;
	result.v_num = itoq((long) eofid(vp->v_file));
	return result;
}


static VALUE
f_fflush(vp)
	VALUE *vp;
{
	VALUE result;

	if (vp->v_type != V_FILE)
		math_error("Non-file for fflush");
	flushid(vp->v_file);
	result.v_type = V_NULL;
	return result;
}


static VALUE
f_fprintf(count, vals)
	int count;
	VALUE **vals;
{
	VALUE result;

	if (vals[0]->v_type != V_FILE)
		math_error("Non-file for fprintf");
	if (vals[1]->v_type != V_STR)
		math_error("Non-string format for fprintf");
	idprintf(vals[0]->v_file, vals[1]->v_str, count - 2, vals + 2);
	result.v_type = V_NULL;
	return result;
}


static VALUE
f_printf(count, vals)
	int count;
	VALUE **vals;
{
	VALUE result;

	if (vals[0]->v_type != V_STR)
		math_error("Non-string format for printf");
	idprintf(FILEID_STDOUT, vals[0]->v_str, count - 1, vals + 1);
	result.v_type = V_NULL;
	return result;
}


static VALUE
f_strprintf(count, vals)
	int count;
	VALUE **vals;
{
	VALUE result;

	if (vals[0]->v_type != V_STR)
		math_error("Non-string format for strprintf");
	math_divertio();
	idprintf(FILEID_STDOUT, vals[0]->v_str, count - 1, vals + 1);
	result.v_str = math_getdivertedio();
	result.v_type = V_STR;
	result.v_subtype = V_STRALLOC;
	return result;
}


static VALUE
f_fgetc(vp)
	VALUE *vp;
{
	VALUE result;
	int ch;

	if (vp->v_type != V_FILE)
		math_error("Non-file for fgetc");
	ch = getcharid(vp->v_file);
	result.v_type = V_NULL;
	if (ch != EOF) {
		result.v_type = V_STR;
		result.v_subtype = V_STRLITERAL;
		result.v_str = charstr(ch);
	}
	return result;
}


static VALUE
f_fgetline(vp)
	VALUE *vp;
{
	VALUE result;
	char *str;

	if (vp->v_type != V_FILE)
		math_error("Non-file for fgetline");
	readid(vp->v_file, &str);
	result.v_type = V_NULL;
	if (str) {
		result.v_type = V_STR;
		result.v_subtype = V_STRALLOC;
		result.v_str = str;
	}
	return result;
}


static VALUE
f_files(count, vals)
	int count;
	VALUE **vals;
{
	VALUE result;

	if (count == 0) {
		result.v_type = V_NUM;
		result.v_num = itoq((long) MAXFILES);
		return result;
	}
	if ((vals[0]->v_type != V_NUM) || qisfrac(vals[0]->v_num))
		math_error("Non-integer for files");
	result.v_type = V_NULL;
	result.v_file = indexid(qtoi(vals[0]->v_num));
	if (result.v_file != FILEID_NONE)
		result.v_type = V_FILE;
	return result;
}


/*
 * return a numerical 'value' of the mode/base
 */
static NUMBER *
base_value(mode)
	long mode;	/* a MODE_XYZ value */
{
	NUMBER *result;

	/* return the old base */
	switch (mode) {
	case MODE_DEFAULT:
		if (_outmode_ == MODE_DEFAULT) {
			result = itoq(10); /* firewall */
		} else {
			result = base_value(_outmode_);
		}
		break;
	case MODE_FRAC:
		result = qalloc();
		itoz(3, &result->den);
		break;
	case MODE_INT:
		result = itoq(-10);
		break;
	case MODE_REAL:
		result = itoq(10);
		break;
	case MODE_EXP:
		result = qalloc();
		ztenpow(20, &result->num);
		break;
	case MODE_HEX:
		result = itoq(16);
		break;
	case MODE_OCTAL:
		result = itoq(8);
		break;
	case MODE_BINARY:
		result = itoq(2);
		break;
	default:
		result = itoq(0);
		break;
	}
	return result;
}


/*
 * set the default output base/mode
 */
static NUMBER *
f_base(count, vals)
	int count;
	NUMBER **vals;
{
	long base;	/* output base/mode */
	long oldbase=0;	/* output base/mode */

	/* deal with just a query */
	if (count != 1) {
		return base_value(_outmode_);
	}

	/* deal with the specal modes first */
	if (qisfrac(vals[0])) {
		return base_value(math_setmode(MODE_FRAC));
	}
	if (vals[0]->num.len > 64/BASEB) {
		return base_value(math_setmode(MODE_EXP));
	}

	/* set the base, if possible */
	base = qtoi(vals[0]);
	switch (base) {
	case -10:
		oldbase = math_setmode(MODE_INT);
		break;
	case 2:
		oldbase = math_setmode(MODE_BINARY);
		break;
	case 8:
		oldbase = math_setmode(MODE_OCTAL);
		break;
	case 10:
		oldbase = math_setmode(MODE_REAL);
		break;
	case 16:
		oldbase = math_setmode(MODE_HEX);
		break;
	default:
		math_error("Unsupported base");
		break;
	}

	/* return the old base */
	return base_value(oldbase);
}


/*
 * Show the list of primitive built-in functions
 */
void
showbuiltins()
{
	register struct builtin *bp;	/* current function */

	printf("\nName\tArgs\tDescription\n\n");
	for (bp = builtins; bp->b_name; bp++) {
		printf("%-9s ", bp->b_name);
		if (bp->b_maxargs == IN)
			printf("%d+    ", bp->b_minargs);
		else if (bp->b_minargs == bp->b_maxargs)
			printf("%-6d", bp->b_minargs);
		else
			printf("%d-%-4d", bp->b_minargs, bp->b_maxargs);
		printf(" %s\n", bp->b_desc);
	}
	printf("\n");
}


/*
 * Return the index of a built-in function given its name.
 * Returns minus one if the name is not known.
 */
int
getbuiltinfunc(name)
	char *name;
{
	register struct builtin *bp;

	for (bp = builtins; bp->b_name; bp++) {
		if ((*name == *bp->b_name) && (strcmp(name, bp->b_name) == 0))
		return (bp - builtins);
	}
	return -1;
}


/*
 * Given the index of a built-in function, return its name.
 */
char *
builtinname(index)
	long index;
{
	if ((unsigned long)index >= (sizeof(builtins) / sizeof(builtins[0])) - 1)
		return "";
	return builtins[index].b_name;
}


/*
 * Given the index of a built-in function, and the number of arguments seen,
 * determine if the number of arguments are legal.  This routine is called
 * during parsing time.
 */
void
builtincheck(index, count)
	int count;
	long index;
{
	register struct builtin *bp;

	if ((unsigned long)index >= (sizeof(builtins) / sizeof(builtins[0])) - 1)
		math_error("Unknown built in index");
	bp = &builtins[index];
	if (count < bp->b_minargs)
		scanerror(T_NULL, "Too few arguments for builtin function \"%s\"",
	bp->b_name);
	if (count > bp->b_maxargs)
		scanerror(T_NULL, "Too many arguments for builtin function \"%s\"",
			bp->b_name);
}


/*
 * Return the opcode for a built-in function that can be used to avoid
 * the function call at all.
 */
int
builtinopcode(index)
	long index;
{
	if ((unsigned long)index >= (sizeof(builtins) / sizeof(builtins[0])) - 1)
		return OP_NOP;
	return builtins[index].b_opcode;
}

/* END CODE */
