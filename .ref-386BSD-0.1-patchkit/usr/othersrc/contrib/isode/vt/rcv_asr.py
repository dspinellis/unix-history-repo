-- VTPM: decode ASR PDU

-- $Header: /f/osi/vt/RCS/rcv_asr.py,v 7.1 91/02/22 09:48:04 mrose Interim $
--
--
-- $Log:	rcv_asr.py,v $
-- Revision 7.1  91/02/22  09:48:04  mrose
-- Interim 6.8
-- 
-- Revision 7.0  89/11/23  22:31:36  mrose
-- Release 6.0
-- 

--
--				  NOTICE
--
--    Acquisition, use, and distribution of this module and related
--    materials are subject to the restrictions of a license agreement.
--    Consult the Preface in the User's Manual for the full terms of
--    this agreement.
--
--


ASRPDU DEFINITIONS ::=

%{
#include <stdio.h>
#include "sector1.h"

#undef PEPYPARM
#define PEPYPARM int *

#undef	PEPYTEST

static int l,m,n,q;

#ifdef PEPYTEST

ASR_MSG udr;

rcv_asr(pe)
PE pe;
{

	int i;

	if(unbuild_ASRPDU_ASRpdu(pe,1,NULLIP,NULLVP,&udr) == NOTOK) 
	{
		(void)printf("Can't unbuild ASR PDU\n");
		return;
	}

	(void)printf("Result = %d\n",udr.result);
	(void)printf("Version.bitcount = %d; Version = %x\n",
		udr.version.bitcount,udr.version.bitstring);
	(void)printf("F.U. bitcount = %d; F.U. = %x\n",
		udr.func_units.bitcount,udr.func_units.bitstring);
	if(udr.valid_coll)
		(void)printf("valid_coll = %d; coll_winner = %d\n",
			udr.valid_coll,udr.coll_winner);
	(void)printf("Valid_reason = %d\n",udr.valid_reason);
	(void)printf("Sp Obj = %d  CDS Obj = %d   CSS Obj = %d   DEV Obj = %d\n",
		udr.arg_list.num_sp_param,udr.arg_list.num_cds_objects,
		udr.arg_list.num_css_objects,udr.arg_list.num_dev_objects);

	for(l=0; l<udr.arg_list.num_sp_param; l++)
	{
		(void)printf("\n");
		(void)printf("Special Num = %d  Special Type = %d",
			udr.arg_list.sp_val[l].param_num,
			udr.arg_list.sp_val[l].param_type);
		if(udr.arg_list.sp_val[l].param_type == 0)
		{
			(void)printf("Boolean Type = %d\n",
				udr.arg_list.sp_val[l].args.bool_arg);
		}
		else if(udr.arg_list.sp_val[l].param_type == 1)
		{
			(void)printf("Integer Type = %d\n",
				udr.arg_list.sp_val[l].args.int_arg);
		}
		else 
			(void)printf("Bad Special Param Type\n");
	}

	for(l=0;l<udr.arg_list.num_cds_objects; l++)
	{
		(void)printf("\n\nObject Name = %s\n",udr.arg_list.cds_val[l].obj_name);
		(void)printf("Valid:\n");
		(void)printf("\tdimen(%d)   x_dim(%d)   y_dim(%d)   z_dim(%d)\n",
			udr.arg_list.cds_val[l].dimensions,
			udr.arg_list.cds_val[l].valid_x_dim,
			udr.arg_list.cds_val[l].valid_y_dim,
			udr.arg_list.cds_val[l].valid_z_dim);
		(void)printf("\terase(%d)   repertoire(%d)   emph(%d)   fore(%d)\n",
			udr.arg_list.cds_val[l].valid_erasure,
			udr.arg_list.cds_val[l].valid_rep_list,
			udr.arg_list.cds_val[l].valid_emp_list,
			udr.arg_list.cds_val[l].valid_fore_color);
		(void)printf("\tback(%d)   access right(%d)\n",
			udr.arg_list.cds_val[l].valid_back_color,
			udr.arg_list.cds_val[l].valid_access_right);
		(void)printf("X Dimension:\n");
		(void)printf("\tBound(%d)  addressing(%d)  absolute(%d)  window(%d)\n",
			udr.arg_list.cds_val[l].x_dim.bound_type,
			udr.arg_list.cds_val[l].x_dim.valid_addr,
			udr.arg_list.cds_val[l].x_dim.valid_abs,
			udr.arg_list.cds_val[l].x_dim.window_type);
		(void)printf("\twindow size = %d\n",udr.arg_list.cds_val[l].x_dim.window);
		(void)printf("Repertoires: (Number = %d)\n",
			udr.arg_list.cds_val[l].rep_value.num_reps);
		for(i=0;i<udr.arg_list.cds_val[l].rep_value.num_reps;i++)
		{
			(void)printf("\ttype(%d)   rep(%s)   capability(%d)   fonts(%d)\n",
				udr.arg_list.cds_val[l].rep_value.repertoire[i].rep_type,
				udr.arg_list.cds_val[l].rep_value.repertoire[i].rep_assign,
				udr.arg_list.cds_val[l].rep_value.repertoire[i].valid_font_cap,
				udr.arg_list.cds_val[l].rep_value.repertoire[i].num_fonts);
		}
	}

}

#endif


#define	bitstr2int(arg,val,cnt) \
{ \
    char   *cp; \
 \
    cp = bitstr2strb (arg, &cnt); \
    val = strb2int (cp, cnt); \
    free (cp); \
}

%}

BEGIN

SECTIONS none unbuild none

ASRpdu ::= CHOICE

{
	asrpdu [1] IMPLICIT ASRcontent [[p (PEPYPARM)parm]]
}

ASRcontent 
%{
	ASR_MSG	*arg = (ASR_MSG *)parm;
%}
	::= SEQUENCE
%{
	arg->valid_reason = 0;
	arg->valid_imp = 0;
	arg->valid_coll = 0;
	arg->version.bitcount = 0;
	arg->func_units.bitcount = 0;
	arg->arg_list.num_sp_param = 0;
%}
{
	
	userReason [0] IMPLICIT PrintableString [[s arg->reason.usr_reason]]
	%{arg->reason.type = 0;
	  arg->valid_reason = 1;%}
	OPTIONAL,

	provReason [1] IMPLICIT INTEGER [[i arg->reason.provider_reason]]
	%{arg->reason.type = 1;
	arg->valid_reason = 1;%}
	OPTIONAL,

	[2] IMPLICIT INTEGER [[i arg->result]],

	[3] IMPLICIT ImplemIdent [[p (PEPYPARM)&(arg->imp_id)]]
	%{arg->valid_imp = 1;%}
	OPTIONAL,

	[4] IMPLICIT BITSTRING
	%{ bitstr2int ($$, arg->version.bitstring, arg->version.bitcount); %},

	[5] IMPLICIT ArgumValueList [[p (PEPYPARM)&(arg->arg_list)]],

	[6] IMPLICIT BITSTRING
	%{ bitstr2int ($$, arg->func_units.bitstring, arg->func_units.bitcount) ;%},

	[7] IMPLICIT INTEGER [[i arg->coll_winner]]
	%{arg->valid_coll = 1;%}
	OPTIONAL
}

ImplemIdent 
%{
	IMPLEM_ID	*arg = (IMPLEM_ID *)parm;
%}
	::= SEQUENCE
%{
	arg->oid_true = 0;
	arg->name_true = 0;
	arg->version_true = 0;
%}
{
	impIdent [0] IMPLICIT OBJECT IDENTIFIER
	[[O arg->imp_oid]]
	%{arg->oid_true = 1;%},

	impName [1] IMPLICIT PrintableString
	[[s arg->name]]
	%{arg->name_true = 1;%},

	impVersion [2] IMPLICIT PrintableString
	[[s arg->version]]
	%{arg->version_true = 1;%}
}

ArgumValueList 
%{
	ARG_VAL_LIST	*arg = (ARG_VAL_LIST *)parm;
%}
	::= SET OF %{q = arg->num_sp_param;%} Squat [[p parm]]


Squat ::= CHOICE
{
	specArgs	[0] IMPLICIT SpecialArgs [[p parm]],
	vteParams	[1] IMPLICIT ParamValueList [[p parm]]
}

SpecialArgs 
%{
	ARG_VAL_LIST	*arg = (ARG_VAL_LIST *)parm;
%}
	::= SEQUENCE
{
	identifier INTEGER [[i arg->sp_val[q].param_num]],
	value CHOICE
	{
		BOOLEAN [[b arg->sp_val[q].args.bool_arg]]
		%{arg->sp_val[q].param_type = 0;%},
		INTEGER [[i arg->sp_val[q].args.int_arg]]
		%{arg->sp_val[q].param_type = 1;%},
		PrintableString[[s arg->sp_val[q].args.string_arg]]
		%{arg->sp_val[q].param_type = 2;%}
	}

	%{++arg->num_sp_param;
	  if(arg->num_sp_param >= MAXSPARGS) return(OK);
	%}
}

ParamValueList 
%{
	ARG_VAL_LIST	*arg = (ARG_VAL_LIST *)parm;
%}
	::= SEQUENCE
%{
/*	arg->num_cds_objects = 0;*/
%}
{
	displayObjects 	[0] IMPLICIT CDSValues [[p parm]]
	OPTIONAL,

--	controlObjects	[1] IMPLICIT CSSValues [[p parm]]
--	OPTIONAL,

--	deviceObjects	[2] IMPLICIT DEVValues [[p parm]]
--	OPTIONAL,

	deliveryControl	[3] IMPLICIT INTEGER
	[[i arg->del_ctrl]]
	OPTIONAL
}

--Note Problem with IMPLICIT SEQUENCE Definition below.  PEPY does not accept
--it as defined in 9041 and in fact that definition is ridiculous.  At the
--moment it is not clear if even hand coding available in ISODE 3.0 can
--produce the requirement of 9041.

CDSValues 
%{
	ARG_VAL_LIST	*arg = (ARG_VAL_LIST *)parm;
%}
	::= 
SET OF %{l = arg->num_cds_objects;%} SEQUENCE
{
	objectName PrintableString
		[[s arg->cds_val[l].obj_name]],

	ObjectOffer [[p (PEPYPARM)&(arg->cds_val[l])]]
	%{ ++arg->num_cds_objects;
	   if(arg->num_cds_objects >= MAXCDSOBJ) return(OK);
	%}
}

CSSValues ::= NULL --Unused for now--

DEVValues ::= NULL --Unused for now--

ObjectOffer 
%{
	CDS_VALUE	*arg = (CDS_VALUE *)parm;
%}
	::= SEQUENCE
%{
	arg->dimensions = 0;
	arg->valid_x_dim = 0;
	arg->valid_y_dim = 0;
	arg->valid_z_dim = 0;
	arg->valid_erasure = 0;
	arg->valid_rep_list = 0;
	arg->valid_emp_list = 0;
	arg->valid_fore_color = 0;
	arg->valid_back_color = 0;
	arg->valid_access_right = 0;
%}
{
	dimensionValue [0]	IMPLICIT INTEGER
	[[i arg->dimensions]]
	OPTIONAL,

	xParamValue [1]		IMPLICIT DimValue [[p (PEPYPARM)&(arg->x_dim)]]
	%{arg->valid_x_dim = 1;%}
	OPTIONAL,

	yParamValue [2]		IMPLICIT DimValue [[p (PEPYPARM)&(arg->y_dim)]]
	%{arg->valid_y_dim = 1;%}
	OPTIONAL,

	zParamValue [3]		IMPLICIT DimValue [[p (PEPYPARM)&(arg->z_dim)]]
	%{arg->valid_z_dim = 1;%}
	OPTIONAL,

--	erasurevalue [4]	IMPLICIT BOOLEAN
--	[[b arg->erasure]]
--	%{arg->valid_erasure = 1;%}
--	OPTIONAL,

	repValueList [5]	IMPLICIT CompRepValue [[p (PEPYPARM)&(arg->rep_value)]]
	%{arg->valid_rep_list = 1;%}
	OPTIONAL,

--	empValueList [6]	IMPLICIT CompEmpValue [[p (PEPYPARM)&(arg->emp_value)]]
--	%{arg->valid_emp_list = 1;%}
--	OPTIONAL,

--	foreColorVal [7]	IMPLICIT ColorValue [[p (PEPYPARM)&(arg->fore_color_list)]]
--	%{arg->valid_fore_color = 1;%}
--	OPTIONAL,

--	backColorVal [8]	IMPLICIT ColorValue [[p (PEPYPARM)&(arg->back_color_list)]]
--	%{arg->valid_back_color = 1;%}
--	OPTIONAL,

	objectAccRight [9]	IMPLICIT INTEGER
	[[i arg->access_right]]
	%{arg->valid_access_right = 1;%}
	OPTIONAL
}

DimValue 
%{
	DIMEN_VALUE	*arg = (DIMEN_VALUE *)parm;
%}
	::= SEQUENCE
%{
	arg->bound_type = 0;
	arg->valid_addr = 0;
	arg->valid_abs = 0;
	arg->window_type = 0;
%}
{
	bound		[0] CHOICE
	{
		unbounded NULL
		%{arg->bound_type = 1;%},

		limit INTEGER
		[[i arg->bound]]
		%{arg->bound_type = 2;%}
	}
	OPTIONAL,

	addressing	[1] IMPLICIT INTEGER
	[[i arg->addressing]]
	OPTIONAL,

	absolute	[2] IMPLICIT INTEGER
	[[i arg->absolute]]
	OPTIONAL,

	window		[3] CHOICE
	{
		unbounded NULL
		%{arg->window_type = 1;%},

		limit INTEGER
		[[i arg->window]]
		%{arg->window_type = 2;%}
	}
	OPTIONAL
}

CompRepValue 
%{
	REP_VALUE	*arg = (REP_VALUE *)parm;
%}
	::= SEQUENCE
%{
	arg->valid_cap = 0;
	arg->num_reps = 0;
%}
{
	repCapability	[0] IMPLICIT INTEGER
	[[i arg->capability]]
	%{arg->valid_cap = 1;%}
	OPTIONAL,
			[1] IMPLICIT SEQUENCE OF 
	%{m =  arg->num_reps;%}
	RepFontValue [[p (PEPYPARM)&(arg->repertoire[m])]]
	%{++arg->num_reps;
	  if(arg->num_reps >= MAXREPS) return(OK);
	%}
	OPTIONAL
}

RepFontValue 
%{
	FONT_VALUE	*arg = (FONT_VALUE *)parm;
%}
	::= 
CHOICE
%{
	arg->valid_font_cap = 0;
	arg->num_fonts = 0;
	arg->rep_assign = 0;
%}
{
	NULL
	%{arg->rep_type = 1;%},

	SEQUENCE
	{
		repertoire	[0] IMPLICIT PrintableString
		[[s arg->rep_assign]]
		OPTIONAL,

		fontCapability	[1] IMPLICIT INTEGER
		[[i arg->capability]]
		%{++arg->valid_font_cap;%}
		OPTIONAL,

		[2] IMPLICIT SEQUENCE OF
		%{n = arg->num_fonts;%}
		PrintableString
		[[s arg->font_names[n] ]]
		%{
			++arg->num_fonts;
			if(arg->num_fonts >= MAXFONTS) return(OK);
		%}
		OPTIONAL
	}
}

--CompEmpValue 
--%{
--		EMP_VALUE	*arg = (EMP_VALUE *)parm;
--%}
--	::= SEQUENCE
--%{arg->valid_cap = 0;
--  arg->num_emps = 0;
--%}
--{
--	empCap		[0] IMPLICIT INTEGER
--			[[i arg->capability]]
--			%{arg->valid_cap = 1;%}
--			OPTIONAL,

--			SEQUENCE OF %{m = arg->num_emps;%}
--				PrintableString
--				[[s arg->emp_string[m] ]]
--				%{++arg->num_emps;
--				  if(arg->num_emps >= MAXEMPS) return(OK);
--				%}
--			OPTIONAL
--}

--ColorValue 
--%{
--		COLOR_VALUE	*arg = (COLOR_VALUE *)parm;
--%}
--	::= SEQUENCE
--%{
--	arg->valid_cap = 0;
--	arg->num_colors = 0;
--%}
--{
--	colorCap	[0] IMPLICIT INTEGER
--			[[i arg->capability]]
--			%{arg->valid_cap = 1;%}
--			OPTIONAL,

--	colorNames	SEQUENCE OF %{m = arg->num_colors;%}
--				PrintableString
--				[[s arg->color_string[m] ]]
--				%{++arg->num_colors;
--				  if(arg->num_colors >= MAXCOLORS) return(OK);
--				%}
--}

END

%{


%}
