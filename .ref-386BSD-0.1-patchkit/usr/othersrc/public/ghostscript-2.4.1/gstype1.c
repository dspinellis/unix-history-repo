/* Copyright (C) 1990, 1992 Aladdin Enterprises.  All rights reserved.
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

/* gstype1.c */
/* Adobe Type 1 font routines for Ghostscript library */
#include "math_.h"
#include "memory_.h"
#include "gx.h"
#include "gserrors.h"
#include "gxarith.h"
#include "gxfixed.h"
#include "gxmatrix.h"
#include "gzstate.h"
#include "gzdevice.h"			/* for gxchar */
#include "gxdevmem.h"			/* ditto */
#include "gzpath.h"
#include "gxchar.h"
#include "gxfont.h"
#include "gxtype1.h"
#include "gxop1.h"

/* Define the amount of thickening to be applied to characters. */
#define type1_fill_adjust 0.25

/* Encrypt a string. */
int
gs_type1_encrypt(byte *dest, byte *src, uint len, crypt_state *pstate)
{	register crypt_state state = *pstate;
	register byte *from = src;
	register byte *to = dest;
	register uint count = len;
	while ( count )
	   {	encrypt_next(*from, state, *to);
		from++, to++, count--;
	   }
	*pstate = state;
	return 0;
}
/* Decrypt a string. */
int
gs_type1_decrypt(byte *dest, byte *src, uint len, crypt_state *pstate)
{	register crypt_state state = *pstate;
	register byte *from = src;
	register byte *to = dest;
	register uint count = len;
	while ( count )
	   {	/* If from == to, we can't use the obvious */
		/*	decrypt_next(*from, state, *to);	*/
		register byte ch = *from++;
		decrypt_next(ch, state, *to);
		to++, count--;
	   }
	*pstate = state;
	return 0;
}

/* Define the structures for the state of a Type 1 interpreter. */
/* This is the interpreter state that must be saved and restored */
/* when calling a CharString subroutine. */
typedef struct {
	byte *ip;
	crypt_state dstate;
} ip_state;
/* Define the stem hint tables. */
/* Each stem hint table is kept sorted. */
#define max_stems 3			/* arbitrary */
typedef struct {
	fixed v0, v1;			/* coordinates (widened a little) */
	gs_fixed_point adjust_lower, adjust_upper;	/* adjustments */
} stem_hint;
typedef struct {
	int count;
	stem_hint *current;		/* cache cursor for search */
	stem_hint data[max_stems];
} stem_hint_table;
/* This is the full state of the Type 1 interpreter. */
#define ostack_size 24			/* per documentation */
#define ipstack_size 10			/* per documentation */
struct gs_type1_state_s {
		/* The following are set at initialization */
	gs_show_enum *penum;		/* show enumerator */
	gs_state *pgs;			/* graphics state */
	gs_type1_data *pdata;		/* font-specific data */
	int charpath_flag;		/* 0 for show, 1 for false */
					/* charpath, 2 for true charpath */
	int paint_type;			/* 0/3 for fill, 1/2 for stroke */
	fixed_coeff fc;			/* cached fixed coefficients */
	float flatness;			/* flatness for character curves */
		/* The following are updated dynamically */
	fixed ostack[ostack_size];	/* the Type 1 operand stack */
	int os_count;			/* # of occupied stack entries */
	ip_state ipstack[ipstack_size+1];	/* control stack */
	int ips_count;			/* # of occupied entries */
	gs_fixed_point lsb;		/* left side bearing */
	gs_fixed_point width;		/* character width */
	int seac_base;			/* base character code for seac, */
					/* or -1 */
		/* The following are set dynamically, */
		/* but not actually used yet. */
	int in_dotsection;		/* true if inside dotsection */
	stem_hint_table hstem_hints;	/* horizontal stem hints */
	stem_hint_table vstem_hints;	/* vertical stem hints */
};

/* Export the size of the structure */
const uint gs_type1_state_sizeof = sizeof(gs_type1_state);

/* Imported procedures */
extern int gx_matrix_to_fixed_coeff(P3(gs_matrix *, fixed_coeff *, int));

/* Initialize a Type 1 interpreter. */
/* The caller must supply a string to the first call of gs_type1_interpret. */
int
gs_type1_init(register gs_type1_state *pis, gs_show_enum *penum,
  int charpath_flag, int paint_type, gs_type1_data *pdata)
{	gs_state *pgs = penum->pgs;
	pis->penum = penum;
	pis->pgs = pgs;
	pis->pdata = pdata;
	pis->charpath_flag = charpath_flag;
	pis->paint_type = paint_type;
	pis->os_count = 0;
	pis->ips_count = 1;
	pis->seac_base = -1;
	pis->in_dotsection = 0;
	pis->hstem_hints.count = 0;
	pis->hstem_hints.current = &pis->hstem_hints.data[0];
	pis->vstem_hints.count = 0;
	pis->vstem_hints.current = &pis->vstem_hints.data[0];
	gx_matrix_to_fixed_coeff(&ctm_only(pgs), &pis->fc, max_coeff_bits);
	/* Set the current point of the path to the origin, */
	/* in anticipation of the initial [h]sbw. */
	{ gx_path *ppath = pgs->path;
	  ppath->position.x = pgs->ctm.tx_fixed;
	  ppath->position.y = pgs->ctm.ty_fixed;
	}
	/* Set the flatness to a value that is likely to produce */
	/* reasonably good-looking curves, regardless of its */
	/* current value in the graphics state. */
	if ( (pis->flatness = pgs->flatness) > 1.0 )
	   {	/* With the conventional 1000-unit characters, */
		/* a small "o" will be about 250 units, */
		/* so set the flatness to 10 units. */
		float cxx = pgs->ctm.xx, cyy = pgs->ctm.yy;
		if ( cxx < 0 ) cxx = -cxx;
		if ( cyy < 0 ) cyy = -cyy;
		if ( cyy > cxx ) cxx = cyy;
		if ( is_skewed(&pgs->ctm) )
		   {	float cxy = pgs->ctm.xy, cyx = pgs->ctm.yx;
			if ( cxy < 0 ) cxy = -cxy;
			if ( cyx < 0 ) cyx = -cyx;
			if ( cxy > cxx ) cxx = cxy;
			if ( cyx > cxx ) cxx = cyx;
		   }
		pis->flatness = (cxx > 0.1 ? cxx * 10 : 1.0);
	   }
	return 0;
}

/* Tracing for type 1 interpreter */
#ifdef DEBUG
#  define dc(str) if ( gs_debug['1'] ) type1_trace(cip, c, str);
private void near
type1_trace(byte *cip, byte c, char _ds *str)
{	dprintf3("[1]%lx: %02x %s\n", (ulong)(cip - 1), c, (char *)str);
}
#else
#  define dc(str)
#endif

/* Define the state used by operator procedures. */
/* These macros refer to a current instance (s) of gs_op1_state. */
#define sppath s.ppath
#define sfc s.fc
#define ptx s.px
#define pty s.py
#define sctx s.ctx
#define scty s.cty

/* Accumulate relative coordinates */
/****** THESE ARE NOT ACCURATE FOR NON-INTEGER DELTAS. ******/
/* This probably doesn't make any difference in practice. */
#define c_fixed(d, c) m_fixed(arg2int(d), c, sfc)
#define accum_x(dx)\
    ptx += c_fixed(dx, sfc.xx);\
    if ( sfc.skewed ) pty += c_fixed(dx, sfc.xy)
#define accum_y(dy)\
    pty += c_fixed(dy, sfc.yy);\
    if ( sfc.skewed ) ptx += c_fixed(dy, sfc.yx)
#define accum_xy(dx,dy)\
    accum_xy_proc(&s, dx, dy)

#define s (*ps)
#define arg2int(f) fixed2int_var(f)

private void near
accum_xy_proc(register is_ptr ps, fixed dx, fixed dy)
{	int idx, idy;
	ptx += m_fixed((idx = arg2int(dx)), sfc.xx, sfc),
	pty += m_fixed((idy = arg2int(dy)), sfc.yy, sfc);
	if ( sfc.skewed )
		ptx += m_fixed(idy, sfc.yx, sfc),
		pty += m_fixed(idx, sfc.xy, sfc);
}

/* We round all endpoints of lines or curves */
/* to the center of the nearest quarter-pixel, and suppress null lines. */
/* (Rounding to the half-pixel causes too many dropouts.) */
/* This saves a lot of rendering work for small characters. */
#define pixel_rounded(fx)\
  (((fx) | float2fixed(0.125)) & float2fixed(-0.125))
#define must_draw_to(lpx, lpy, px, py)\
  ((lpx = pixel_rounded(px)), (lpy = pixel_rounded(py)),\
   (psub = sppath->current_subpath) == 0 ||\
   (pseg = psub->last)->type == s_line_close ||\
   lpx != pseg->pt.x || lpy != pseg->pt.y)

/* ------ Operator procedures ------ */

/* We put these before the interpreter to save having to write */
/* prototypes for all of them. */

int
gs_op1_closepath(register is_ptr ps)
{	/* Note that this does NOT reset the current point! */
	int code = gx_path_close_subpath(sppath);
	if ( code < 0 ) return code;
	return gx_path_add_point(sppath, ptx, pty);	/* put the point where it was */
}

int
gs_op1_sbw(register is_ptr ps, fixed sbx, fixed sby, fixed wx, fixed wy)
{	register gs_type1_state *pis = ps->pis;
	gs_show_enum *penum = pis->penum;
	if ( penum->sb_set )
		pis->lsb = penum->metrics_sb;
	else
		pis->lsb.x = sbx, pis->lsb.y = sby;
	if ( penum->width_set )
		pis->width = penum->metrics_width;
	else
		pis->width.x = wx, pis->width.y = wy;
#ifdef DEBUG
if ( gs_debug['1'] )
	dprintf4("[1]sb=(%g,%g) w=(%g,%g)\n",
		 fixed2float(pis->lsb.x), fixed2float(pis->lsb.y),
		 fixed2float(pis->width.x), fixed2float(pis->width.y));
#endif
	accum_xy(pis->lsb.x, pis->lsb.y);
	return 0;
}

int
gs_op1_hsbw(register is_ptr ps, fixed sbx, fixed wx)
{	return gs_op1_sbw(ps, sbx, (fixed)0, wx, (fixed)0);
}

int
gs_op1_rrcurveto(register is_ptr ps, fixed dx1, fixed dy1,
  fixed dx2, fixed dy2, fixed dx3, fixed dy3)
{	fixed ptx1, pty1, ptx2, pty2;
	/* Following declarations are only for must_draw_to */
	fixed lpx, lpy;
	subpath *psub;
	segment *pseg;
	accum_xy(dx1, dy1);
	ptx1 = ptx, pty1 = pty;
	accum_xy(dx2, dy2);
	ptx2 = ptx, pty2 = pty;
	accum_xy(dx3, dy3);
	if ( must_draw_to(lpx, lpy, ptx, pty) )
	  return gx_path_add_flattened_curve(sppath, ptx1, pty1, ptx2, pty2, lpx, lpy, ps->pis->flatness);
	return 0;
}

#undef s

/* ------ Main interpreter ------ */

/* Continue interpreting a Type 1 CharString. */
/* If str != 0, it is taken as the byte string to interpret. */
/* Return 0 on successful completion, <0 on error, */
/* or >0 when client intervention is required. */
/* The int * argument is where the character is stored for seac, */
/* or the othersubr # for callothersubr. */
private void near type1_hstem(P3(gs_type1_state *, fixed, fixed));
private void near type1_vstem(P3(gs_type1_state *, fixed, fixed));
private stem_hint *near type1_stem(P3(stem_hint_table *, fixed, fixed));
private int near type1_endchar(P3(gs_type1_state *, gs_state *, gx_path *));
int
gs_type1_interpret(register gs_type1_state *pis, byte *str, int *pindex)
{	gs_state *pgs = pis->pgs;
	gs_type1_data *pdata = pis->pdata;
	gs_op1_state s;
	fixed cstack[ostack_size];
#define cs0 cstack[0]
#define ics0 fixed2int_var(cs0)
#define cs1 cstack[1]
#define ics1 fixed2int_var(cs1)
#define cs2 cstack[2]
#define ics2 fixed2int_var(cs2)
#define cs3 cstack[3]
#define ics3 fixed2int_var(cs3)
#define cs4 cstack[4]
#define ics4 fixed2int_var(cs4)
#define cs5 cstack[5]
#define ics5 fixed2int_var(cs5)
	register fixed _ss *csp;
#define clear csp = cstack - 1
	ip_state *ipsp = &pis->ipstack[pis->ips_count - 1];
	register byte *cip;
	register crypt_state state;
	register int c;
	int code = 0;
	fixed ftx = pgs->ctm.tx_fixed, fty = pgs->ctm.ty_fixed;
	fixed lpx, lpy;
	subpath *psub;
	segment *pseg;

	sppath = pgs->path;
	s.pis = pis;
	sfc = pis->fc;
	ptx = sppath->position.x;
	pty = sppath->position.y;

	/* Copy the operand stack out of the saved state. */
	if ( pis->os_count == 0 )
	   {	clear;
	   }
	else
	   {	memcpy(cstack, pis->ostack, pis->os_count * sizeof(fixed));
		csp = &cstack[pis->os_count - 1];
	   }

	if ( str == 0 ) goto cont;
	cip = str;
call:	state = crypt_charstring_seed;
	   {	int skip = pdata->lenIV;
		/* Skip initial random bytes */
		for ( ; skip > 0; --skip )
		   {	decrypt_skip_next(*cip, state); ++cip;
		   }
	   }
	goto top;
cont:	cip = ipsp->ip;
	state = ipsp->dstate;
top:	while ( 1 )
	 { uint c0;
	   c = decrypt_this((c0 = *cip++), state);
	   decrypt_skip_next(c0, state);
	   switch ( (char_command)c )
	   {
#define cnext clear; break
	case c_hstem: dc("hstem")
		type1_hstem(pis, cs0, cs1);
		cnext;
	case c_vstem: dc("vstem")
		type1_vstem(pis, cs0, cs1);
		cnext;
	case c_vmoveto: dc("vmoveto")
		accum_y(cs0);
move:		/* Round to the nearest center of a quarter-pixel. */
		if ( must_draw_to(lpx, lpy, ptx, pty) )
			code = gx_path_add_point(sppath, lpx, lpy);
		goto cc;
	case c_rlineto: dc("rlineto")
		accum_xy(cs0, cs1);
line:		/* Round to the nearest center of a quarter-pixel. */
		if ( must_draw_to(lpx, lpy, ptx, pty) )
			code = gx_path_add_line(sppath, lpx, lpy);
cc:		if ( code < 0 ) return code;
pp:
#ifdef DEBUG
if ( gs_debug['1'] )
		dprintf2("[1]pt=(%g,%g)\n",
			 fixed2float(ptx), fixed2float(pty));
#endif
		cnext;
	case c_hlineto: dc("hlineto")
		accum_x(cs0);
		goto line;
	case c_vlineto: dc("vlineto")
		accum_y(cs0);
		goto line;
	case c_rrcurveto: dc("rrcurveto")
		code = gs_op1_rrcurveto(&s, cs0, cs1, cs2, cs3, cs4, cs5);
		goto cc;
	case c_closepath: dc("closepath")
		code = gs_op1_closepath(&s);
		goto cc;
	case c_callsubr: dc("callsubr")
	   {	int index = fixed2int_var(*csp);
		byte *nip;
		code = (*pdata->subr_proc)(pdata, index, &nip);
		if ( code < 0 ) return_error(code);
		--csp;
		ipsp->ip = cip, ipsp->dstate = state;
		++ipsp;
		cip = nip;
	   }
		goto call;
	case c_return: dc("return")
		--ipsp;
		goto cont;
	case c_escape: dc("escape:")
		decrypt_next(*cip, state, c); ++cip;
		switch ( (char_extended_command)c )
		   {
		case ce_dotsection: dc("  dotsection")
			pis->in_dotsection = !pis->in_dotsection;
			cnext;
		case ce_vstem3: dc("  vstem3")
			type1_vstem(pis, cs0, cs1);
			type1_vstem(pis, cs2, cs3);
			type1_vstem(pis, cs4, cs5);
			cnext;
		case ce_hstem3: dc("  hstem3")
			type1_hstem(pis, cs0, cs1);
			type1_hstem(pis, cs2, cs3);
			type1_hstem(pis, cs4, cs5);
			cnext;
		case ce_seac: dc("  seac")
			/* Do the accent now.  When it finishes */
			/* (detected in endchar), do the base character. */
			pis->seac_base = ics3;
			/* Adjust the origin of the coordinate system */
			/* for the accent (endchar puts it back). */
			ptx = ftx, pty = fty;
			cs1 -= cs0;	 /* subtract off asb */
			accum_xy(cs1, cs2);
			sppath->position.x = ptx;
			sppath->position.y = pty;
			pis->os_count = 0;	/* clear */
			/* Give control back to the caller, who must */
			/* re-invoke the interpreter with the seac string. */
			*pindex = ics4;
			return type1_result_seac;
		case ce_sbw: dc("  sbw")
			code = gs_op1_sbw(&s, cs0, cs1, cs2, cs3);
			goto cc;
		case ce_div: dc("  div")
			csp[-1] = float2fixed((float)csp[-1] / (float)*csp);
			--csp; goto pushed;
		case ce_undoc15: dc("  undoc15")
			/*
			 * NOTE: this opcode is not documented by Adobe,
			 * but is used in some Adobe fonts.  I have no idea
			 * what it is supposed to do.
			 */
			cnext;
		case ce_callothersubr: dc("  callothersubr")
		   {	int scount = csp - cstack;
			*pindex = fixed2int_var(*csp);
			/* Update path position so it will be right */
			/* when we come back in. */
			sppath->position.x = ptx;
			sppath->position.y = pty;
			/* Exit to caller */
			ipsp->ip = cip, ipsp->dstate = state;
			pis->os_count = scount;
			pis->ips_count = ipsp - &pis->ipstack[0] + 1;
			if ( scount )
				memcpy(pis->ostack, cstack,
				       scount * sizeof(fixed));
			return type1_result_callothersubr;
		   }
		case ce_pop: dc("  pop")
			++csp;
			code = (*pdata->pop_proc)(pdata, csp);
			if ( code < 0 ) return_error(code);
			goto pushed;
		case ce_setcurrentpoint: dc("  setcurrentpoint")
			ptx = ftx, pty = fty;
			accum_xy(cs0, cs1);
			goto pp;
		default:
			return_error(gs_error_invalidfont);
		   }
		break;
	case c_hsbw: dc("hsbw")
		code = gs_op1_hsbw(&s, cs0, cs1);
		goto cc;
	case c_endchar: dc("endchar")
		if ( pis->seac_base >= 0 )
		   {	/* We just finished the accent of a seac. */
			/* Do the base character. */
			*pindex = pis->seac_base;
			pis->seac_base = -1;
			/* Restore the coordinate system origin */
			sppath->position.x = ftx;
			sppath->position.y = fty;
			pis->os_count = 0;	/* clear */
			/* Clear the ipstack, in case the accent ended */
			/* inside a subroutine. */
			pis->ips_count = 1;
			/* Give control back to the caller, who must */
			/* re-invoke the interpreter with the */
			/* base character string. */
			return type1_result_seac;
		   }
		/* This is a real endchar.  Handle it below. */
		return type1_endchar(pis, pgs, sppath);
	case c_undoc15: dc("  undoc15")
		/*
		 * NOTE: this opcode is not documented by Adobe,
		 * but is used in some Adobe fonts.  I have no idea
		 * what it is supposed to do.
		 */
		cnext;
	case c_rmoveto: dc("rmoveto")
		accum_xy(cs0, cs1);
		goto move;
	case c_hmoveto: dc("hmoveto")
		accum_x(cs0);
		goto move;
	case c_vhcurveto: dc("vhcurveto")
		code = gs_op1_rrcurveto(&s, (fixed)0, cs0, cs1, cs2, cs3, (fixed)0);
		goto cc;
	case c_hvcurveto: dc("hvcurveto")
		code = gs_op1_rrcurveto(&s, cs0, (fixed)0, cs1, cs2, (fixed)0, cs3);
		goto cc;
	/* Fill up the dispatch up to 32. */
	case c_undef0: case c_undef2:
	case c_undef16: case c_undef17: case c_undef18: case c_undef19:
	case c_undef20: case c_undef23:
	case c_undef24: case c_undef25: case c_undef26: case c_undef27:
	case c_undef28: case c_undef29:
		return_error(gs_error_invalidfont);
	/* Fill up the dispatch for 1-byte numbers. */
#define icase(n) case n:
#define ncase(n) case n: *++csp = int2fixed(c_value_num1(n)); goto pushed;
#define icase10(n)\
  icase(n) icase(n+1) icase(n+2) icase(n+3) icase(n+4)\
  icase(n+5) icase(n+6) icase(n+7) icase(n+8) icase(n+9)
#define ncase10(n)\
  ncase(n) ncase(n+1) ncase(n+2) ncase(n+3) ncase(n+4)\
  ncase(n+5) ncase(n+6) ncase(n+7) ncase(n+8) ncase(n+9)
	icase(32) icase(33) icase(34)
	icase(35) icase(36) icase(37) icase(38) icase(39)
	icase10(40)
	icase10(50) icase10(60) icase10(70) icase10(80) icase10(90)
	icase10(100) icase10(110) goto pi; ncase10(120) ncase10(130) ncase10(140)
	ncase10(150) icase10(160) icase10(170) icase10(180) icase10(190)
	icase10(200) icase10(210) icase10(220) icase10(230)
	icase(240) icase(241) icase(242) icase(243) icase(244)
	icase(245) icase(246)
pi:		*++csp = int2fixed(c_value_num1(c));
pushed:
#ifdef DEBUG
if ( gs_debug['1'] )
		dprintf3("[1]%d: (%d) %f\n",
			 (int)(csp - cstack), c, fixed2float(*csp));
#endif
		break;
	/* Handle 2-byte positive numbers. */
#define case_num2(n)\
  case c_num2+n: *++csp = int2fixed(c_value_num2(c_num2+n, 0))
	case_num2(0); goto pos2;
	case_num2(1); goto pos2;
	case_num2(2); goto pos2;
	case_num2(3);
#undef case_num2
pos2:	   {	c0 = *cip++;
		*csp += int2fixed(decrypt_this(c0, state));
		decrypt_skip_next(c0, state);
	   }	goto pushed;
	/* Handle 2-byte negative numbers. */
#define case_num3(n)\
  case c_num3+n: *++csp = int2fixed(c_value_num3(c_num3+n, 0))
	case_num3(0); goto neg2;
	case_num3(1); goto neg2;
	case_num3(2); goto neg2;
	case_num3(3);
#undef case_num3
neg2:	   {	c0 = *cip++;
		*csp -= int2fixed(decrypt_this(c0, state));
		decrypt_skip_next(c0, state);
	   }	goto pushed;
	/* Handle 5-byte numbers. */
	case c_num4:
	   {	uint c1, c2;
		long lw;
		decrypt_next(*cip, state, c0);
		decrypt_next(cip[1], state, c1);
		decrypt_next(cip[2], state, c2);
		decrypt_next(cip[3], state, lw);
		cip += 4;
		lw += (ulong)c0 << 24;
		lw += (ulong)c1 << 16;
		lw += c2 << 8;
		*++csp = int2fixed(lw);
		if ( lw != fixed2long(*csp) )
			return_error(gs_error_rangecheck);
	   }	goto pushed;
	   }
	 }
}

/* Add a horizontal stem hint. */
private void near
type1_hstem(gs_type1_state *pis, fixed y, fixed dy)
{	stem_hint *psh;
	if ( dy < 0 ) y += dy, dy = -dy;
	psh = type1_stem(&pis->hstem_hints, y, dy);
	if ( psh == 0 ) return;
	/* Compute adjustments here */
}

/* Add a vertical stem hint. */
private void near
type1_vstem(gs_type1_state *pis, fixed x, fixed dx)
{	stem_hint *psh;
	if ( dx < 0 ) x += dx, dx = -dx;
	psh = type1_stem(&pis->vstem_hints, x, dx);
	if ( psh == 0 ) return;
	/* Compute adjustments here */
}

/* Add a stem hint, keeping the table sorted. */
/* Return the stem hint pointer, or 0 if the table is full. */
private stem_hint *near
type1_stem(stem_hint_table *psht, fixed v0, fixed d)
{	stem_hint *bot = &psht->data[0];
	stem_hint *top = bot + psht->count;
	if ( psht->count >= max_stems ) return 0;
	while ( top > bot && v0 < top[-1].v0 )
	   {	*top = top[-1];
		top--;
	   }
	top->v0 = v0, top->v1 = v0 + d;
	psht->count++;
	return top;
}

/* Handle the end of a character.  We break this out into a separate */
/* procedure so as not to overwhelm the optimizing compilers. */
private int near
type1_endchar(gs_type1_state *pis, gs_state *pgs, gx_path *ppath)
{	int use_stroke = pis->paint_type == 1 || pis->paint_type == 2;
	fixed ftx = pgs->ctm.tx_fixed, fty = pgs->ctm.ty_fixed;
	int code;
	/* Set the current point to the character origin: */
	/* the 'show' loop will take care of adding in */
	/* the width we supply to setcharwidth/cachedevice. */
	gx_path_add_point(ppath, ftx, fty);
	if ( pis->charpath_flag )
	   {	code = gs_setcharwidth(pis->penum, pgs,
				fixed2float(pis->width.x),
				fixed2float(pis->width.y));
		if ( code < 0 ) return code;
		/* Merge the path into its parent */
		return gx_path_add_path(pgs->saved->path, ppath);
	   }
	   {	gs_rect bbox;
		code = gs_pathbbox(pgs, &bbox);
		if ( code < 0 )		/* must be a null path */
		   {	bbox.p.x = bbox.p.y = bbox.q.x = bbox.q.y = 0;
		   }
#ifdef DEBUG
if ( gs_debug['1'] )
		dprintf4("[1]bbox=(%g,%g),(%g,%g)\n",
			 bbox.p.x, bbox.p.y, bbox.q.x, bbox.q.y);
#endif
		   {	/* Expand the bounding box to encompass */
			/* the width of the stroke (if stroking). */
			/* setcachedevice also adds 1 or 2 pixels, */
			/* so we don't have to worry about rounding. */
			if ( use_stroke )
			   {	float adjust = gs_currentlinewidth(pgs);
				if ( adjust < 1 ) adjust = 1;
				/****** SHOULD SCALE ******/
				bbox.p.x -= adjust;
				bbox.p.y -= adjust;
				bbox.q.x += adjust;
				bbox.q.y += adjust;
			   }
		   }
		code = gs_setcachedevice(pis->penum, pgs,
				fixed2float(pis->width.x),
				fixed2float(pis->width.y),
				bbox.p.x, bbox.p.y,
				bbox.q.x, bbox.q.y);
		if ( code < 0 ) return code;
	   }
	/* We've already constructed the path: */
	/* translate it so it matches the cache device. */
	gx_path_translate(pgs->path, pgs->ctm.tx_fixed - ftx,
			  pgs->ctm.ty_fixed - fty);
	if ( code < 0 ) return code;
	/******
	 ****** The adjust parameter is a hack to make
	 ****** characters come out more bold, since we
	 ****** don't look at the hints.
	 ******/
	gx_color_load(pgs->dev_color, pgs);
	return (use_stroke ? gs_stroke(pgs) :
		gs_fill_adjust(pgs, float2fixed(type1_fill_adjust)));
}

/* Pop a (fixed) number off the internal stack. */
/* The client uses this to get the arguments for an OtherSubr. */
int
gs_type1_pop(gs_type1_state *pis, fixed *pf)
{	*pf = pis->ostack[--(pis->os_count)];
	return 0;
}
