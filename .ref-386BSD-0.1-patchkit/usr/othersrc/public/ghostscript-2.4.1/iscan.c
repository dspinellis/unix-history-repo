/* Copyright (C) 1989, 1992 Aladdin Enterprises.  All rights reserved.
   Distributed by Free Software Foundation, Inc.

This file is part of Ghostscript.

Ghostscript is distributed in the hope that it will be useful, but
WITHOUT ANY WARRANTY.  No author or distributor accepts responsibility
to anyone for the consequences of using it or for whether it serves any
particular purpose or works at all, unless he says so in writing.  Refer
to the Ghostscript General Public License for full details.

Everyone is granted permission to copy, modify and redistribute
Ghostscript, but only under the conditions described in the Ghostscript
General Public License.  A copy of this license is supposed to have been
given to you along with Ghostscript so you can know your rights and
responsibilities.  It should be in a file named COPYING.  Among other
things, the copyright notice and this notice must be preserved on all
copies.  */

/* iscan.c */
/* Token scanner for Ghostscript interpreter */
#include <ctype.h>
#include "memory_.h"
#include "ghost.h"
#include "alloc.h"
#include "dict.h"			/* for //name lookup */
#include "dstack.h"			/* ditto */
#include "errors.h"
#include "iutil.h"
#include "name.h"
#include "ostack.h"			/* for accumulating proc bodies */
#include "packed.h"
#include "store.h"
#include "stream.h"
#include "scanchar.h"

/* Array packing flag */
ref array_packing;			/* t_boolean */
/* Binary object format flag. This will never be set non-zero */
/* unless the binary token feature is enabled. */
ref binary_object_format;		/* t_integer */
#define recognize_btokens() (binary_object_format.value.intval != 0)

/* Procedure for binary tokens.  Set at initialization if the binary token */
/* option is included; only called if recognize_btokens() is true. */
/* Returns 0 on success, <0 on failure. */
int (*scan_btoken_proc)(P3(stream *, ref *, int)) = NULL;

/*
 * Level 2 includes some changes in the scanner:
 *	- \ is always recognized in strings, regardless of the data source;
 *	- << and >> are legal tokens;
 *	- <~ introduces an ASCII-85 encoded string (terminated by ~>)
 *		(not implemented yet);
 *	- Character codes above 127 introduce binary objects.
 * We explicitly enable or disable these changes here.
 */
int scan_enable_level2 = 1;

/* Forward references */
private	int	scan_hex_string(P2(stream *, ref *)),
		scan_int(P6(byte **, byte *, int, int, long *, double *)),
		scan_number(P3(byte *, byte *, ref *)),
		scan_string(P3(stream *, int, ref *));

/* Define the character scanning table (see scanchar.h). */
byte scan_char_array[258];

/* A structure for dynamically growable objects */
typedef struct dynamic_area_s {
	byte *base;
	byte *next;
	uint num_elts;
	uint elt_size;
	int is_dynamic;			/* false if using fixed buffer */
	byte *limit;
} dynamic_area;
typedef dynamic_area _ss *da_ptr;

/* Begin a dynamic object. */
/* dynamic_begin returns the value of alloc, which may be 0: */
/* the invoker of dynamic_begin must test the value against 0. */
#define dynamic_begin(pda, dnum, desize)\
	((pda)->base = (byte *)alloc((pda)->num_elts = (dnum),\
				     (pda)->elt_size = (desize), "scanner"),\
	 (pda)->limit = (pda)->base + (dnum) * (desize),\
	 (pda)->is_dynamic = 1,\
	 (pda)->next = (pda)->base)

/* Free a dynamic object. */
private void
dynamic_free(da_ptr pda)
{	if ( pda->is_dynamic )
		alloc_free((char *)(pda->base), pda->num_elts, pda->elt_size,
			   "scanner");
}

/* Grow a dynamic object. */
/* If the allocation fails, free the old contents, and return NULL; */
/* otherwise, return the new `next' pointer. */
private byte *
dynamic_grow(register da_ptr pda, byte *next)
{	if ( next != pda->limit ) return next;
	pda->next = next;
	   {	uint num = pda->num_elts;
		uint old_size = num * pda->elt_size;
		uint pos = pda->next - pda->base;
		uint new_size = (old_size < 10 ? 20 :
				 old_size >= (max_uint >> 1) ? max_uint :
				 old_size << 1);
		uint new_num = new_size / pda->elt_size;
		if ( pda->is_dynamic )
		   {	byte *base = alloc_grow(pda->base, num, new_num, pda->elt_size, "scanner");
			if ( base == 0 )
			   {	dynamic_free(pda);
				return NULL;
			   }
			pda->base = base;
			pda->num_elts = new_num;
			pda->limit = pda->base + new_size;
		   }
		else
		   {	byte *base = pda->base;
			if ( !dynamic_begin(pda, new_num, pda->elt_size) ) return NULL;
			memcpy(pda->base, base, old_size);
			pda->is_dynamic = 1;
		   }
		pda->next = pda->base + pos;
	   }
	return pda->next;
}

/* Initialize the scanner. */
void
scan_init()
{	/* Initialize decoder array */
	register byte _ds *decoder = scan_char_decoder;
	static char stop_chars[] = "()<>[]{}/%";
	static char space_chars[] = " \f\t\n\r";
	decoder[ERRC] = ctype_eof;	/* ****** FIX THIS? ****** */
	decoder[EOFC] = ctype_eof;
	memset(decoder, ctype_name, 256);
	memset(decoder + 128, ctype_btoken, 32);
	   {	register char _ds *p;
		for ( p = space_chars; *p; p++ )
		  decoder[*p] = ctype_space;
		decoder[char_NULL] = decoder[char_VT] =
		  decoder[char_DOS_EOF] = ctype_space;
		for ( p = stop_chars; *p; p++ )
		  decoder[*p] = ctype_other;
	   }
	   {	register int i;
		for ( i = 0; i < 10; i++ )
		  decoder['0' + i] = i;
		for ( i = 0; i < max_radix - 10; i++ )
		  decoder['A' + i] = decoder['a' + i] = i + 10;
	   }
	/* Other initialization */
	make_false(&array_packing);
	make_int(&binary_object_format, 0);
}

/* Read a token from a stream. */
/* Return 1 for end-of-stream, 0 if a token was read, */
/* or a (negative) error code. */
/* If the token required a terminating character (i.e., was a name or */
/* number) and the next character was whitespace, read and discard */
/* that character: see the description of the 'token' operator on */
/* p. 232 of the Red Book. */
/* from_string indicates reading from a string vs. a file, */
/* because \ escapes are not recognized in the former case. */
/* (See the footnote on p. 23 of the Red Book.) */
int
scan_token(register stream *s, int from_string, ref *pref)
{	ref *myref = pref;
	dynamic_area proc_da;	/* (not actually dynamic) */
	int pstack = 0;		/* offset from proc_da.base */
	int retcode = 0;
	register int c;
	int name_type;		/* number of /'s preceding */
	int try_number;
	byte s1[2];
	register byte _ds *decoder = scan_char_decoder;
	/* Only old P*stScr*pt interpreters use from_string.... */
	from_string &= !scan_enable_level2;
top:	c = sgetc(s);
#ifdef DEBUG
if ( gs_debug['s'] )
	fprintf(gs_debug_out, (c >= 32 && c <= 126 ? "`%c'" : "`%03o'"), c);
#endif
	switch ( c )
	   {
	case ' ': case '\f': case '\t': case '\n': case '\r':
	case char_NULL: case char_VT: case char_DOS_EOF:
		goto top;
	case '[':
	case ']':
		s1[0] = (byte)c;
		name_ref(s1, 1, myref, 1);
		r_set_attrs(myref, a_executable);
		break;
	case '<':
		if ( scan_enable_level2 )
		   {	c = sgetc(s);
			if ( char_is_data(c) ) sputback(s);
			switch ( c )
			   {
			case '<':
				name_type = try_number = 0;
				goto try_funny_name;
			/****** Check for <~ here ******/
			   }
		   }
		retcode = scan_hex_string(s, myref);
		break;
	case '(':
		retcode = scan_string(s, from_string, myref);
		break;
	case '{':
		if ( pstack == 0 )
		   {	/* Use the operand stack to accumulate procedures. */
			myref = osp + 1;
			proc_da.base = (byte *)myref;
			proc_da.limit = (byte *)(ostop + 1);
			proc_da.is_dynamic = 0;
			proc_da.elt_size = sizeof(ref);
			proc_da.num_elts = ostop - osp;
		   }
		if ( proc_da.limit - (byte *)myref < 2 * sizeof(ref) )
		  return e_limitcheck; /* ****** SHOULD GROW OSTACK ****** */
		r_set_size(myref, pstack);
		myref++;
		pstack = (byte *)myref - proc_da.base;
		goto top;
	case '>':
		if ( scan_enable_level2 )
		   {	name_type = try_number = 0;
			goto try_funny_name;
		   }
		/* falls through */
	case ')':
		retcode = e_syntaxerror;
		break;
	case '}':
		if ( pstack == 0 )
		   {	retcode = e_syntaxerror;
			break;	
		   }
		   {	ref *ref0 = (ref *)(proc_da.base + pstack);
			uint size = myref - ref0;
			ref *aref;
			myref = ref0 - 1;
			pstack = r_size(myref);
			if ( pstack == 0 ) myref = pref;
			if ( array_packing.value.index )
			   {	retcode = make_packed_array(ref0, size, myref,
							    "scanner(packed)");
				if ( retcode < 0 ) return retcode;
				r_set_attrs(myref, a_executable);
			   }
			else
			  {	aref = alloc_refs(size, "scanner(proc)");
				if ( aref == 0 ) return e_VMerror;
				refcpy_to_new(aref, ref0, size);
				make_tasv_new(myref, t_array, a_executable + a_all, size, refs, aref);
			  }
		   }
		break;
	case '/':
		c = sgetc(s);
		if ( c == '/' )
		   {	name_type = 2;
			c = sgetc(s);
		   }
		else
			name_type = 1;
		try_number = 0;
		switch ( decoder[c] )
		   {
		case ctype_name:
		default:
			goto do_name;
		case ctype_btoken:
			if ( !recognize_btokens() ) goto do_name;
			/* otherwise, an empty name */
			sputback(s);
		case ctype_eof:
			/* Empty name: bizarre but legitimate. */
			name_ref((byte *)0, 0, myref, 1);
			goto have_name;
		case ctype_other:
			switch ( c )
			   {
			case '[':	/* only special as first character */
			case ']':	/* ditto */
				s1[0] = (byte)c;
				name_ref(s1, 1, myref, 1);
				goto have_name;
			case '<':	/* legal in Level 2 */
			case '>':
				if ( scan_enable_level2 ) goto try_funny_name;
			default:
				/* Empty name: bizarre but legitimate. */
				name_ref((byte *)0, 0, myref, 1);
				sputback(s);
				goto have_name;
			   }
		case ctype_space:
			/* Empty name: bizarre but legitimate. */
			name_ref((byte *)0, 0, myref, 1);
			/* Check for \r\n */
			if ( c == '\r' && (c = sgetc(s)) != '\n' && char_is_data(c) )
				sputback(s);
			goto have_name;
		   }
		/* NOTREACHED */
	case '%':
	   {	for ( ; ; )
		  switch ( sgetc(s) )
		   {
		case '\r':
			if ( (c = sgetc(s)) != '\n' && char_is_data(c) )
				sputback(s);
			/* falls through */
		case '\n': case '\f':
			goto top;
		case EOFC:
			goto ceof;
		case ERRC:
			goto cerr;
		   }
ceof:		;
	   }	/* falls through */
	case EOFC:
		retcode = (pstack != 0 ? e_syntaxerror : 1);
		break;
	case ERRC:
cerr:		retcode = e_ioerror;
		break;

	/* Check for a Level 2 funny name (<< or >>). */
	/* c is '<' or '>'. */
try_funny_name:
	   {	int c1 = sgetc(s);
		if ( c1 == c )
		   {	s1[0] = s1[1] = c;
			retcode = name_ref(s1, 2, myref, 1);
			goto have_name;
		   }
		if ( char_is_data(c1) ) sputback(s);
	   }	retcode = e_syntaxerror;
		break;

	/* Handle separately the names that might be a number */
	case '0': case '1': case '2': case '3': case '4':
	case '5': case '6': case '7': case '8': case '9':
	case '.': case '+': case '-':
		name_type = 0;
		try_number = 1;
		goto do_name;

	/* Check for a binary object */
#define case4(c) case c: case c+1: case c+2: case c+3
	case4(128): case4(132): case4(136): case4(140):
	case4(144): case4(148): case4(152): case4(156):
#undef case4
		if ( recognize_btokens() )
		   {	retcode = (*scan_btoken_proc)(s, myref, c);
			break;
		   }
	/* Not a binary object, fall through. */

	/* The default is a name. */
	default:
	/* Handle the common cases (letters and _) explicitly, */
	/* rather than going through the default test. */
	case 'a': case 'b': case 'c': case 'd': case 'e': case 'f':
	case 'g': case 'h': case 'i': case 'j': case 'k': case 'l': case 'm':
	case 'n': case 'o': case 'p': case 'q': case 'r': case 's':
	case 't': case 'u': case 'v': case 'w': case 'x': case 'y': case 'z':
	case 'A': case 'B': case 'C': case 'D': case 'E': case 'F':
	case 'G': case 'H': case 'I': case 'J': case 'K': case 'L': case 'M':
	case 'N': case 'O': case 'P': case 'Q': case 'R': case 'S':
	case 'T': case 'U': case 'V': case 'W': case 'X': case 'Y': case 'Z':
	case '_':
		/* Common code for scanning a name. */
		/* try_number and name_type are already set. */
		/* We know c has ctype_name (or maybe ctype_btoken) */
		/* or is a digit. */
		name_type = 0;
		try_number = 0;
do_name:
	   {	dynamic_area da;
		int max_name_ctype =
			(recognize_btokens() ? ctype_name : ctype_btoken);
		/* Try to scan entirely within the stream buffer. */
		/* We stop 1 character early, so we don't switch buffers */
		/* looking ahead if the name is terminated by \r\n. */
		register byte *ptr = sbufptr(s);
		byte *end = sbufend(s) - 1;
		da.base = ptr - 1;
		da.is_dynamic = 0;
		do
		   {	if ( ptr >= end )
			   {	ssetbufptr(s, ptr);
				/* Initialize the dynamic area. */
				/* We have to do this before the next */
				/* sgetc, which will overwrite the buffer. */
				da.limit = ptr;
				da.num_elts = ptr - da.base;
				da.elt_size = 1;
				ptr = dynamic_grow(&da, ptr);
				if ( !ptr ) return e_VMerror;
				goto dyn_name;
			   }
			c = *ptr++;
		   }
		while ( decoder[c] <= max_name_ctype );	/* digit or name */
		/* Name ended within the buffer. */
		ssetbufptr(s, ptr);
		ptr--;
		goto nx;
		/* Name overran buffer. */
dyn_name:	while ( decoder[c = sgetc(s)] <= max_name_ctype )
		  {	if ( ptr == da.limit )
			   {	ptr = dynamic_grow(&da, ptr);
				if ( !ptr ) return e_VMerror;
			   }
			*ptr++ = c;
		   }
nx:		switch ( decoder[c] )
		  {
		  case ctype_btoken:
		  case ctype_other:
			sputback(s);
			break;
		  case ctype_space:
			/* Check for \r\n */
			if ( c == '\r' && (c = sgetc(s)) != '\n' && char_is_data(c) )
				sputback(s);
		  case ctype_eof: ;
		  }
		/* Check for a number */
		if ( try_number )
		   {	retcode = scan_number(da.base, ptr, myref);
			if ( retcode != e_syntaxerror )
			   {	dynamic_free(&da);
				if ( name_type == 2 ) return e_syntaxerror;
				break;	/* might be e_limitcheck */
			   }
		   }
		retcode = name_ref(da.base, (uint)(ptr - da.base), myref, 1);
		dynamic_free(&da);
	   }
		/* Done scanning.  Check for preceding /'s. */
have_name:	if ( retcode < 0 ) return retcode;
		switch ( name_type )
		   {
		case 0:			/* ordinary executable name */
			if ( r_has_type(myref, t_name) )	/* i.e., not a number */
			  r_set_attrs(myref, a_executable);
		case 1:			/* quoted name */
			break;
		case 2:			/* immediate lookup */
		   {	ref *pvalue;
			if ( !r_has_type(myref, t_name) )
				return e_undefined;
			if ( (pvalue = dict_find_name(myref)) == 0 )
				return e_undefined;
			ref_assign_new(myref, pvalue);
		   }
		   }
	   }
	/* If we are at the top level, return the object, */
	/* otherwise keep going. */
	if ( pstack == 0 || retcode < 0 )
	  return retcode;
	if ( proc_da.limit - (byte *)myref < 2 * sizeof(ref) )
	  return e_limitcheck; /* ****** SHOULD GROW OSTACK ****** */
	myref++;
	goto top;
}

/* The internal scanning procedures return 0 on success, */
/* or a (negative) error code on failure. */

/* Scan a number for cvi or cvr. */
/* The first argument is a t_string.  This is just like scan_number, */
/* but allows leading or trailing whitespace. */
int
scan_number_only(ref *psref, ref *pnref)
{	byte *str = psref->value.bytes;
	byte *end = str + r_size(psref);
	if ( !r_has_attr(psref, a_read) ) return e_invalidaccess;
	while ( str < end && scan_char_decoder[*str] == ctype_space )
	  str++;
	while ( str < end && scan_char_decoder[end[-1]] == ctype_space )
	  end--;
	return scan_number(str, end, pnref);
}

/* Note that the number scanning procedures use a byte ** and a byte * */
/* rather than a stream.  (It makes quite a difference in performance.) */
#define ngetc(sp) (sp < end ? *sp++ : EOFC)
#define nputback(sp) (--sp)
#define nreturn(v) return (*pstr = sp, v)

/* Procedure to scan a number. */
private int
scan_number(byte *str, byte *end, ref *pref)
{	/* Powers of 10 up to 6 can be represented accurately as */
	/* a single-precision float. */
#define num_powers_10 6
	static float powers_10[num_powers_10+1] =
	   {	1e0, 1e1, 1e2, 1e3, 1e4, 1e5, 1e6	};
	static double neg_powers_10[num_powers_10+1] =
	   {	1e0, 1e-1, 1e-2, 1e-3, 1e-4, 1e-5, 1e-6	};
	byte *sp = str;		/* can't be register because of & */
	int sign = 0;
	long ival;
	double dval;
	int exp10 = 0;
	int code;
	register int c;
	switch ( c = ngetc(sp) )
	   {
	case '+': sign = 1; c = ngetc(sp); break;
	case '-': sign = -1; c = ngetc(sp); break;
	   }
	if ( !isdigit(c) )
	   {	if ( c != '.' ) return(e_syntaxerror);
		c = ngetc(sp);
		if ( !isdigit(c) ) return(e_syntaxerror);
		ival = 0;
		goto fi;
	   }
	nputback(sp);
	if ( (code = scan_int(&sp, end, 10, 0, &ival, &dval)) != 0 )
	   {	if ( code < 0 ) return(code);	/* e_syntaxerror */
		/* Code == 1, i.e., the integer overflowed. */
		switch ( c = ngetc(sp) )
		   {
		default:
			return(e_syntaxerror); /* not terminated properly */
		case '.':
			c = ngetc(sp); goto fd;
		case 'e': case 'E':
			goto fsd;
		case EOFC:		/* return a float */
			make_real_new(pref, (float)(sign < 0 ? -dval : dval));
			return 0;
		case ERRC:
			return e_ioerror;
		   }
	   }
	switch ( c = ngetc(sp) )
	   {
	case EOFC:
		break;
	case ERRC:
		return e_ioerror;
	case '.':
		c = ngetc(sp); goto fi;
	default:
		return(e_syntaxerror);	/* not terminated properly */
	case 'e': case 'E':
		goto fsi;
	case '#':
		if ( sign || ival < min_radix || ival > max_radix )
			return(e_syntaxerror);
		code = scan_int(&sp, end, (int)ival, 1, &ival, NULL);
		if ( code ) return(code);
		switch ( ngetc(sp) )
		   {
			case EOFC:
				break;
			case ERRC:
				return(e_ioerror);
			default:
				return(e_syntaxerror);
		   }
	   }
	/* Return an integer */
	make_int_new(pref, (sign < 0 ? -ival : ival));
	return(0);
	/* Handle a real.  We just saw the decimal point. */
	/* Enter here if we are still accumulating an integer in ival. */
fi:	while ( isdigit(c) )
	   {	/* Check for overflowing ival */
		if ( ival >= (max_ulong >> 1) / 10 - 1 )
		   {	dval = ival;
			goto fd;
		   }
		ival = ival * 10 + (c - '0');
		c = ngetc(sp);
		exp10--;
	   }
fsi:	if ( sign < 0 ) ival = -ival;
	/* Take a shortcut for the common case */
	if ( !(c == 'e' || c == 'E' || exp10 < -num_powers_10) )
	   {	make_real_new(pref, (float)(ival * neg_powers_10[-exp10]));
		return(0);
	   }
	dval = ival;
	goto fe;
	/* Now we are accumulating a double in dval. */
fd:	while ( isdigit(c) )
	   {	dval = dval * 10 + (c - '0');
		c = ngetc(sp);
		exp10--;
	   }
fsd:	if ( sign < 0 ) dval = -dval;
fe:	/* dval contains the value, negated if necessary */
	if ( c == 'e' || c == 'E' )
	   {	/* Check for a following exponent. */
		int esign = 0;
		long eexp;
		switch ( c = ngetc(sp) )
		   {
		case '+': break;
		case '-': esign = 1; break;
		default: nputback(sp);
		   }
		code = scan_int(&sp, end, 10, 0, &eexp, NULL);
		if ( code < 0 ) return(code);
		if ( code > 0 || eexp > 999 )
			return(e_limitcheck);	/* semi-arbitrary */
		if ( esign )
			exp10 -= (int)eexp;
		else
			exp10 += (int)eexp;
		c = ngetc(sp);
	   }
	if ( c != EOFC ) return(c == ERRC ? e_ioerror : e_syntaxerror);
	/* Compute dval * 10^exp10. */
	if ( exp10 > 0 )
	   {	while ( exp10 > num_powers_10 )
			dval *= powers_10[num_powers_10],
			exp10 -= num_powers_10;
		if ( exp10 > 0 )
			dval *= powers_10[exp10];
	   }
	else if ( exp10 < 0 )
	   {	while ( exp10 < -num_powers_10 )
			dval /= powers_10[num_powers_10],
			exp10 += num_powers_10;
		if ( exp10 < 0 )
			dval /= powers_10[-exp10];
	   }
	make_real_new(pref, (float)dval);
	return(0);
}
/* Internal subroutine to scan an integer. */
/* Return 0, e_limitcheck, or e_syntaxerror. */
/* (The only syntax error is no digits encountered.) */
/* Put back the terminating character. */
/* If nosign is true, the integer is scanned as unsigned; */
/* overflowing a ulong returns e_limitcheck.  If nosign is false, */
/* the integer is scanned as signed; if the integer won't fit in a long, */
/* then: */
/*   if pdval == NULL, return e_limitcheck; */
/*   if pdval != NULL, return 1 and store a double value in *pdval. */
private int
scan_int(byte **pstr, byte *end, int radix, int nosign,
  long *pval, double *pdval)
{	register byte *sp = *pstr;
	uint ival = 0, imax, irem;
#if arch_ints_are_short
	ulong lval, lmax;
	uint lrem;
#else
#  define lval ival			/* for overflowing into double */
#endif
	double dval;
	register int c, d;
	register byte _ds *decoder = scan_char_decoder;
	/* Avoid the long divisions when radix = 10 */
#define set_max(vmax, vrem, big)\
  if ( radix == 10 )	vmax = (big) / 10, vrem = (big) % 10;\
  else			vmax = (big) / radix, vrem = (big) % radix
	set_max(imax, irem, max_uint);
#define convert_digit_fails(c, d)\
  (d = decoder[c]) >= radix
	while ( 1 )
	   {	c = ngetc(sp);
		if ( convert_digit_fails(c, d) )
		   {	if ( char_is_data(c) ) nputback(sp);
			if ( (int)ival < 0 && !nosign )
			   {	d = ival % radix;
				ival /= radix;
				break;
			   }
			*pval = ival;
			nreturn(0);
		   }
		if ( ival >= imax && (ival > imax || d > irem) )
			break;		/* overflow */
		ival = ival * radix + d;
	   }
#if arch_ints_are_short
	/* Short integer overflowed.  Accumulate in a long. */
	lval = (ulong)ival * radix + d;
	set_max(lmax, lrem, max_ulong);
	while ( 1 )
	   {	c = ngetc(sp);
		if ( convert_digit_fails(c, d) )
		   {	if ( char_is_data(c) ) nputback(sp);
			if ( (long)lval < 0 && !nosign )
			   {	d = lval % radix;
				lval /= radix;
				break;
			   }
			*pval = lval;
			nreturn(0);
		   }
		if ( lval >= lmax && (lval > lmax || d > lrem) )
			break;		/* overflow */
		lval = lval * radix + d;
	   }
#endif
	/* Integer overflowed.  Accumulate the result as a double. */
	if ( pdval == NULL ) nreturn(e_limitcheck);
	dval = (double)lval * radix + d;
	while ( 1 )
	   {	c = ngetc(sp);
		if ( convert_digit_fails(c, d) )
		   {	if ( char_is_data(c) ) nputback(sp);
			*pdval = dval;
			nreturn(1);
		   }
		dval = dval * radix + d;
	   }
	/* Control doesn't get here */
}

/* Make a string.  If the allocation fails, release any dynamic storage. */
private int
mk_string(ref *pref, da_ptr pda, byte *next)
{	uint size = (pda->next = next) - pda->base;
	byte *body = alloc_shrink(pda->base, pda->num_elts, size, 1, "scanner(string)");
	if ( body == 0 )
	   {	dynamic_free(pda);
		return e_VMerror;
	   }
	make_tasv_new(pref, t_string, a_all, size, bytes, body);
	return 0;
}

/* Internal procedure to scan a string. */
private int
scan_string(register stream *s, int from_string, ref *pref)
{	dynamic_area da;
	register int c;
	register byte *ptr = dynamic_begin(&da, 100, 1);
	int plevel = 0;
	if ( ptr == 0 ) return e_VMerror;
top:	while ( 1 )
	   {	switch ( (c = sgetc(s)) )
		   {
		case EOFC:
			dynamic_free(&da);
			return e_syntaxerror;
		case ERRC:
			dynamic_free(&da);
			return e_ioerror;
		case '\\':
			if ( from_string ) break;
			switch ( (c = sgetc(s)) )
			   {
			case 'n': c = '\n'; break;
			case 'r': c = '\r'; break;
			case 't': c = '\t'; break;
			case 'b': c = '\b'; break;
			case 'f': c = '\f'; break;
			case '\r':	/* ignore, check for following \n */
				c = sgetc(s);
				if ( c != '\n' && char_is_data(c) )
					sputback(s);
				goto top;
			case '\n': goto top;	/* ignore */
			case '0': case '1': case '2': case '3':
			case '4': case '5': case '6': case '7':
			   {	int d = sgetc(s);
				c -= '0';
				if ( d >= '0' && d <= '7' )
				   {	c = (c << 3) + d - '0';
					d = sgetc(s);
					if ( d >= '0' && d <= '7' )
					   {	c = (c << 3) + d - '0';
						break;
					   }
				   }
				if ( char_is_signal(d) )
				   {	dynamic_free(&da);
					return (d == ERRC ? e_ioerror : e_syntaxerror);
				   }
				sputback(s);
			   }
				break;
			default: ;	/* ignore the \ */
			   }
			break;
		case '(':
			plevel++; break;
		case ')':
			if ( --plevel < 0 ) goto out; break;
		case '\r':		/* convert to \n */
			c = sgetc(s);
			if ( c != '\n' && char_is_data(c) )
				sputback(s);
			c = '\n';
		   }
		if ( ptr == da.limit )
		   {	ptr = dynamic_grow(&da, ptr);
			if ( !ptr ) return e_VMerror;
		   }
		*ptr++ = c;
	   }
out:	return mk_string(pref, &da, ptr);
}

/* Internal procedure to scan a hex string. */
private int
scan_hex_string(stream *s, ref *pref)
{	dynamic_area da;
	int c1, c2, val1, val2;
	byte *ptr = dynamic_begin(&da, 100, 1);
	register byte _ds *decoder = scan_char_decoder;
	if ( ptr == 0 ) return e_VMerror;
l1:	do
	   {	c1 = sgetc(s);
		if ( (val1 = decoder[c1]) < 0x10 )
		   {	do
			   {	c2 = sgetc(s);
				if ( (val2 = decoder[c2]) < 0x10 )
				   {	if ( ptr == da.limit )
					   {	ptr = dynamic_grow(&da, ptr);
						if ( !ptr ) return e_VMerror;
					   }
					*ptr++ = (val1 << 4) + val2;
					goto l1;
				   }
			   }
			while ( val2 == ctype_space );
			if ( c2 != '>' )
			   {	dynamic_free(&da);
				return e_syntaxerror;
			   }
			if ( ptr == da.limit )
			   {	ptr = dynamic_grow(&da, ptr);
				if ( !ptr ) return e_VMerror;
			   }
			*ptr++ = val1 << 4;	/* no 2nd char */
			goto lx;
		   }
	   }
	while ( val1 == ctype_space );
	if ( c1 != '>' )
	   {	dynamic_free(&da);
		return e_syntaxerror;
	   }
lx:	return mk_string(pref, &da, ptr);
}
