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

/* zchar.c */
/* Character operators for Ghostscript */
#include "ghost.h"
#include "errors.h"
#include "oper.h"
#include "gxfixed.h"			/* for gstype1.h */
#include "gxmatrix.h"			/* for font.h */
#include "gschar.h"
#include "gstype1.h"
#include "gxdevice.h"			/* for gxfont.h */
#include "gxfont.h"
#include "gzpath.h"			/* for type1addpath: see below */
#include "gzstate.h"
#include "alloc.h"
#include "dict.h"
#include "dstack.h"			/* for systemdict */
#include "font.h"
#include "estack.h"
#include "state.h"
#include "store.h"

/* All the character rendering operators use the execution stack */
/* for loop control -- see estack.h for details. */
/* The information pushed by these operators is as follows: */
/*	the enumerator (t_string, but points to a gs_show_enum); */
/*	a slot for the procedure for kshow, unused otherwise; */
/*	the procedure to be called at the end of the enumeration */
/*		(t_operator, but called directly, not by the interpreter); */
/*	the usual e-stack mark (t_null). */
#define snumpush 4
#define senum (gs_show_enum *)(esp->value.bytes)
#define sslot esp[-1]
#define seproc esp[-2]

/* Imports */
extern int array_get(P3(ref *, long, ref *));
extern ref name_StandardEncoding;

/* Forward references */
private int setup_show(P3(ref *, op_proc_p, int));
private int show_continue(P1(os_ptr));
private int i_show_continue;
private int finish_show(P1(os_ptr));
private int i_finish_show;
private int finish_stringwidth(P1(os_ptr));
private int i_finish_stringwidth;
private gs_show_enum *find_show(P0());
private void free_show(P0());

/* show */
int
zshow(register os_ptr op)
{	int code = setup_show(op, finish_show, i_finish_show);
	if ( code < 0 ) return code;
	if ( (code = gs_show_n_init(senum, igs, (char *)op->value.bytes, r_size(op))) < 0 )
	   {	free_show();
		return code;
	   }
	pop(1);  op--;
	return show_continue(op);
}

/* ashow */
int
zashow(register os_ptr op)
{	int code;
	float axy[2];
	if (	(code = num_params(op - 1, 2, axy)) < 0 ||
		(code = setup_show(op, finish_show, i_finish_show)) < 0
	   )
		return code;
	if ( (code = gs_ashow_n_init(senum, igs, axy[0], axy[1], (char *)op->value.bytes, r_size(op))) < 0 )
	   {	free_show();
		return code;
	   }
	pop(3);  op -= 3;
	return show_continue(op);
}

/* widthshow */
int
zwidthshow(register os_ptr op)
{	int code;
	float cxy[2];
	check_type(op[-1], t_integer);
	if ( (ulong)(op[-1].value.intval) > 255 ) return e_rangecheck;
	if (	(code = num_params(op - 2, 2, cxy)) < 0 ||
		(code = setup_show(op, finish_show, i_finish_show)) < 0
	   )
		return code;
	if ( (code = gs_widthshow_n_init(senum, igs, cxy[0], cxy[1],
					 (char)op[-1].value.intval,
					 (char *)op->value.bytes,
					 r_size(op))) < 0 )
	   {	free_show();
		return code;
	   }
	pop(4);  op -= 4;
	return show_continue(op);
}

/* awidthshow */
int
zawidthshow(register os_ptr op)
{	int code;
	float cxy[2], axy[2];
	check_type(op[-3], t_integer);
	if ( (ulong)(op[-3].value.intval) > 255 ) return e_rangecheck;
	if (	(code = num_params(op - 4, 2, cxy)) < 0 ||
		(code = num_params(op - 1, 2, axy)) < 0 ||
		(code = setup_show(op, finish_show, i_finish_show)) < 0
	   )
		return code;
	if ( (code = gs_awidthshow_n_init(senum, igs, cxy[0], cxy[1],
					  (char)op[-3].value.intval,
					  axy[0], axy[1],
					  (char *)op->value.bytes,
					  r_size(op))) < 0 )
	   {	free_show();
		return code;
	   }
	pop(6);  op -= 6;
	return show_continue(op);
}

/* kshow */
int
zkshow(register os_ptr op)
{	int code;
	check_proc(op[-1]);
	if ( (code = setup_show(op, finish_show, i_finish_show)) < 0 ) return code;
	if ( (code = gs_kshow_n_init(senum, igs, (char *)op->value.bytes, r_size(op))) < 0 )
	   {	free_show();
		return code;
	   }
	sslot = op[-1];		/* save kerning proc */
	pop(2);  op -= 2;
	return show_continue(op);
}

/* Common finish procedure for all show operations. */
/* Doesn't have to do anything. */
private int
finish_show(os_ptr op)
{	return 0;
}

/* stringwidth */
int
zstringwidth(register os_ptr op)
{	int code = setup_show(op, finish_stringwidth, i_finish_stringwidth);
	if ( code < 0 ) return code;
	if ( (code = gs_stringwidth_n_init(senum, igs, (char *)op->value.bytes, r_size(op))) < 0 )
	   {	free_show();
		return code;
	   }
	pop(1);  op--;
	return show_continue(op);
}
/* Finishing procedure for stringwidth. */
/* Pushes the accumulated width. */
private int
finish_stringwidth(register os_ptr op)
{	gs_point width;
	gs_show_width(senum, &width);
	push(2);
	make_real(op - 1, width.x);
	make_real(op, width.y);
	return 0;
}

/* charpath */
int
zcharpath(register os_ptr op)
{	int code;
	check_type(*op, t_boolean);
	code = setup_show(op - 1, finish_show, i_finish_show);
	if ( code < 0 ) return code;
	if ( (code = gs_charpath_n_init(senum, igs, (char *)op[-1].value.bytes, r_size(op - 1), op->value.index)) < 0 )
	   {	free_show();
		return code;
	   }
	pop(2);  op -= 2;
	return show_continue(op);
}

/* setcachedevice */
int
zsetcachedevice(register os_ptr op)
{	float wbox[6];
	int npop = 6;
	gs_show_enum *penum = find_show();
	int code = num_params(op, 6, wbox);
	if ( penum == 0 ) return e_undefined;
	if ( code < 0 )
	   {	/* P*stScr*pt implementations apparently allow the */
		/* bounding box to be specified as a 4-element array. */
		/* Check for this here. */
		check_array(*op);
		if ( r_size(op) != 4 ||
		     num_params(op - 1, 2, wbox) < 0 ||
		     num_params(op->value.refs + 3, 4, wbox + 2) < 0
		   )
			return code;
		npop = 3;
	   }
	if ( (code = gs_setcachedevice(penum, igs, wbox[0], wbox[1], wbox[2], wbox[3], wbox[4], wbox[5])) < 0 )
		return code;
	pop(npop);
	return 0;
}

/* setcharwidth */
int
zsetcharwidth(register os_ptr op)
{	float width[2];
	gs_show_enum *penum = find_show();
	int code = num_params(op, 2, width);
	if ( penum == 0 ) return e_undefined;
	if (	code < 0 || 
		(code = gs_setcharwidth(penum, igs, width[0], width[1])) < 0
	   )
		return code;
	pop(2);
	return 0;
}

/* setmetrics */
int
zsetmetrics(register os_ptr op)
{	float params[4];
	gs_point sb, w;
	gs_show_enum *penum = find_show();
	int code, size;
	if ( penum == 0 ) return e_undefined;
	switch ( r_type(op) )
	   {
	case t_array:
		switch ( (size = r_size(op)) )
		   {
		case 2: case 4: break;
		default: return e_invalidfont;
		   }
		code = num_params(op->value.refs + size - 1, size, params);
		if ( code < 0 ) return code;
		sb.x = params[0];
		if ( size == 4 )
			sb.y = params[1], w.x = params[2], w.y = params[3];
		else
			sb.y = 0, w.x = params[1], w.y = 0;
		code = gs_setmetrics(penum, igs, &sb, &w);
		break;
	default:
		code = real_param(op, params);
		if ( code < 0 ) return code;
		w.x = params[0];
		w.y = 0;
		code = gs_setmetrics(penum, igs, 0, &w);
	   }
	if ( code >= 0 ) pop(1);
	return code;
}

/* type1addpath */
typedef struct {
	gs_font *pfont;
	fixed *osptr;			/* fake interpreter operand stack */
	fixed ostack[2];
} z1_data;
int
ztype1addpath(register os_ptr op)
{	int code, value;
	gs_show_enum *penum = find_show();
	gs_font *pfont = gs_currentfont(igs);
	font_data *pfdata = (font_data *)pfont->client_data;
	gs_type1_state *pis;
	fixed discard;
	gs_fixed_point spt, ept;
	int flex_path_was_open;
	gs_type1_data tdata;
	z1_data zdata;
	byte *charstring = 0;
	ref enc_entry;
	if ( penum == 0 ) return e_undefined;
	check_type(*op, t_string);
	tdata = pfont->data.base.type1_data;
	zdata.pfont = pfont;
	zdata.osptr = zdata.ostack;
	tdata.proc_data = (char *)&zdata;
	if ( r_size(op) <= tdata.lenIV )
	   {	/* String is empty, or too short.  Just ignore it. */
		pop(1);
		return 0;
	   }
	pis = (gs_type1_state *)alloc(1, gs_type1_state_sizeof, "type1addpath");
	if ( pis == 0 ) return e_VMerror;
	code = gs_type1_init(pis, penum,
			     gs_show_in_charpath(penum), tdata.PaintType,
			     &tdata);
	if ( code < 0 )
	   {	alloc_free((char *)pis, 1, gs_type1_state_sizeof, "type1addpath");
		return code;
	   }
	charstring = op->value.bytes;
more:	code = gs_type1_interpret(pis, charstring, &value);
	charstring = 0;
	switch ( code )
	   {
	case type1_result_seac:
	   {	ref *pstdenc, *pcstr;
		if ( dict_find(&systemdict,
			       &name_StandardEncoding, &pstdenc) <= 0 )
			return e_undefined;
		code = array_get(pstdenc, (long)value, &enc_entry);
		if ( code < 0 ) return code;
		if ( dict_find(&pfdata->CharStrings,
			       &enc_entry, &pcstr) <= 0 )
			return e_undefined;
		if ( !r_has_type(pcstr, t_string) )
			return e_invalidfont;
		charstring = pcstr->value.bytes;
	   }	goto more;
	case type1_result_callothersubr:
	   {	/* We aren't prepared to call the interpreter here, */
		/* so we fake the Flex feature. */
		gx_path *ppath = igs->path;
		gs_type1_pop(pis, &discard);	/* pop # of args */
		switch ( value )
		   {
		case 0:
			/* We have to do something really sleazy here, */
			/* namely, make it look as though the rmovetos */
			/* never really happened, because we don't want */
			/* to interrupt the current subpath. */
			gx_path_current_point(ppath, &ept);
			gx_path_add_point(ppath, spt.x, spt.y);
			ppath->subpath_open = flex_path_was_open;
					/* ^--- sleaze */
			gx_path_add_line(ppath, ept.x, ept.y);
			/* Transfer endpoint coordinates to 'ostack' */
			gs_type1_pop(pis, &zdata.ostack[0]);
			gs_type1_pop(pis, &zdata.ostack[1]);
			gs_type1_pop(pis, &discard);
			zdata.osptr = &zdata.ostack[2];
			goto more;
		case 1:
			gx_path_current_point(ppath, &spt);
			flex_path_was_open = ppath->subpath_open;
					/* ^--- more sleaze */
			goto more;
		case 2:
			goto more;
		case 3:
			gs_type1_pop(pis, &discard);	/* pop subr# */
			zdata.ostack[0] = int2fixed(3);
			zdata.osptr = &zdata.ostack[1];
			goto more;
		   }
		/* Unrecognized othersubr */
		code = e_rangecheck;
	   }	break;
	   }
	alloc_free((char *)pis, 1, gs_type1_state_sizeof, "type1addpath");
	if ( code >= 0 ) pop(1);
	return code;
}

/* type1imagepath */
int
ztype1imagepath(register os_ptr op)
{	float woxy[4];
	int code;
	check_type(op[-7], t_string);
	check_type(op[-6], t_integer);
	check_type(op[-5], t_integer);
	if ( (code = num_params(op - 1, 4, woxy)) < 0 ) return code;
	check_write_type(*op, t_string);
	code = gs_type1imagepath(igs, op[-7].value.bytes,
		(int)op[-6].value.intval, (int)op[-5].value.intval,
		woxy[0], woxy[1], woxy[2], woxy[3],
		op->value.bytes, r_size(op));
	if ( code < 0 ) return code;
	op[-7] = *op;
	r_set_size(op - 7, code);
	pop(7);
	return 0;
}

/* ------ Auxiliary procedures for type 1 fonts ------ */

int
z1_subr_proc(gs_type1_data *pdata, int index, byte **pstr)
{	gs_font *pfont = ((z1_data *)(pdata->proc_data))->pfont;
	font_data *pfdata = (font_data *)(pfont->client_data);
	ref *psubr;
	if ( index < 0 || index >= r_size(&pfdata->Subrs) )
		return e_rangecheck;
	psubr = pfdata->Subrs.value.refs + index;
	check_type(*psubr, t_string);
	*pstr = psubr->value.bytes;
	return 0;
}

int
z1_pop_proc(gs_type1_data *pdata, fixed *pf)
{	*pf = *--(((z1_data *)(pdata->proc_data))->osptr);
	return 0;
}

/* ------ Initialization procedure ------ */

op_def zchar_op_defs[] = {
	{"3ashow", zashow},
	{"6awidthshow", zawidthshow},
	{"2charpath", zcharpath},
	{"2kshow", zkshow},
	{"3setcachedevice", zsetcachedevice},
	{"2setcharwidth", zsetcharwidth},
	{"1.setmetrics", zsetmetrics},
	{"1show", zshow},
	{"1stringwidth", zstringwidth},
	{"1type1addpath", ztype1addpath},
	{"8type1imagepath", ztype1imagepath},
	{"4widthshow", zwidthshow},
		/* Internal operators */
	{"0%finish_show", finish_show, &i_finish_show},
	{"0%finish_stringwidth", finish_stringwidth, &i_finish_stringwidth},
	{"0%show_continue", show_continue, &i_show_continue},
	op_def_end(0)
};

/* ------ Internal routines ------ */

/* Set up for a show operator. */
/* The top stack element must be the string to be scanned. */
/* The caller has already done all other argument checking. */
private int
setup_show(ref *op, op_proc_p endproc /* end procedure */, int proc_index)
{	gs_show_enum *penum;
	check_read_type(*op, t_string);
	check_estack(snumpush + 2);
	if ( (penum = (gs_show_enum *)alloc(1, gs_show_enum_sizeof, "setup_show")) == 0 )
		return e_VMerror;
	mark_estack(es_show);
	push_op_estack(endproc, proc_index);
	++esp;
	make_tv(esp, t_null, index, 0);		/* reserve slot */
	++esp;
	make_tasv(esp, t_string, 0, gs_show_enum_sizeof, bytes, (byte *)penum);
	return o_push_estack;
}

/* Continuation operator for character rendering. */
private int
show_continue(register os_ptr op)
{	gs_show_enum *penum = senum;
	int code = gs_show_next(penum);
	switch ( code )
	   {
	case 0:				/* all done */
		code = (*real_opproc(&seproc))(op);
		free_show();
		return (code >= 0 ? o_pop_estack : code);
	case gs_show_kern:
	   {	ref *pslot = &sslot;
		push(2);
		make_int(op - 1, gs_kshow_previous_char(penum));
		make_int(op, gs_kshow_next_char(penum));
		push_op_estack(show_continue, i_show_continue);		/* continue after kerning */
		*++esp = *pslot;	/* kerning procedure */
	   }
		return o_push_estack;
	case gs_show_render:
	   {	font_data *pfont = (font_data *)gs_currentfont(igs)->client_data;
		push(2);
		op[-1] = pfont->dict;	/* push the font */
		make_int(op, gs_show_current_char(penum));
		push_op_estack(show_continue, i_show_continue);
		*++esp = pfont->BuildChar;
	   }
		return o_push_estack;
	default:			/* error */
		free_show();
		return code;
	   }
}

/* Find the current show enumerator on the e-stack. */
private gs_show_enum *
find_show()
{	es_ptr ep = esp;
	while ( !(r_has_type(ep, t_null) && ep->value.index == es_show) )
	   {	if ( --ep < esbot ) return 0;	/* no mark */
	   }
	return (gs_show_enum *)ep[snumpush - 1].value.bytes;
}

/* Discard the show record (after an error, or at the end). */
private void
free_show()
{	alloc_free((char *)senum, 1, gs_show_enum_sizeof, "free_show");
	esp -= snumpush;
}
