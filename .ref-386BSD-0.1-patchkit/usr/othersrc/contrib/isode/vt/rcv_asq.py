-- VTPM: decode ASQ PDU

-- $Header: /f/osi/vt/RCS/rcv_asq.py,v 7.1 91/02/22 09:48:02 mrose Interim $
--
--
-- $Log:	rcv_asq.py,v $
-- Revision 7.1  91/02/22  09:48:02  mrose
-- Interim 6.8
-- 
-- Revision 7.0  89/11/23  22:31:34  mrose
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


ASQPDU DEFINITIONS ::=

%{
#include <stdio.h>
#include "sector1.h"

#undef PEPYPARM
#define PEPYPARM int *

#undef	PEPYTEST

static int l,m,n,q;

#ifdef PEPYTEST

char *myname;
ASQ_MSG ud;

rcv_asq(pe)
PE pe;
{
	int i,j;

	if(unbuild_ASQPDU_ASQpdu(pe,1,NULLIP,NULLVP,&ud) == NOTOK)return;

	(void)printf("\n\n\nASQ Message:\n");
	(void)printf("Class = %d   Valid_Imp = %d   Functional Units(%d) = %x\n",
		ud.class,ud.valid_imp,ud.func_units.bitcount,ud.func_units.bitstring);
	(void)printf("Version = %x   Valid_coll = %d   Valid_Prof = %d\n",
		ud.version.bitstring,ud.valid_coll,ud.valid_prof);
	if(ud.valid_prof)printf("OID_true = %d\n",
		ud.asq_profile.oid_true);

	if(!ud.valid_prof) 
		return;
	(void)printf("Specials=%d   CDS=%d    CSS=%d    DEV=%d\n",
		ud.asq_profile.num_sp_param,
		ud.asq_profile.num_cds_objects,ud.asq_profile.num_css_objects,
		ud.asq_profile.num_dev_objects);
	
	for(i=0; i < ud.asq_profile.num_sp_param; i++)
	{
		(void)printf("\n");
		(void)printf("Special Num = %d   Special Type = %d  ",
			ud.asq_profile.sp_offer_list[i].param_num,
			ud.asq_profile.sp_offer_list[i].param_type);
		if(ud.asq_profile.sp_offer_list[i].param_type == 0)
		{
			(void)printf("Boolean = %c\n",
				ud.asq_profile.sp_offer_list[i].args.bool_arg);
		}
		else if(ud.asq_profile.sp_offer_list[i].param_type == 1)
		{
			(void)printf("Integer type = %d   Integer value = %d\n",
				ud.asq_profile.sp_offer_list[i].args.int_arg.type,
				ud.asq_profile.sp_offer_list[i].args.int_arg.value);
		}
		else (void)printf("Invalid type\n");
	}


	for(i=0; i<ud.asq_profile.num_cds_objects; i++)
	{
		(void)printf("\n");
		(void)printf("name: %s   x=%d  y=%d  z=%d\n",
			ud.asq_profile.cds_offer_list[i].obj_name,
			ud.asq_profile.cds_offer_list[i].valid_x_dim,
			ud.asq_profile.cds_offer_list[i].valid_y_dim,
			ud.asq_profile.cds_offer_list[i].valid_z_dim);

		(void)printf("erase=%d   repertoire=%d   emphasis=%d\n",
			ud.asq_profile.cds_offer_list[i].erasure.bitcount,
			ud.asq_profile.cds_offer_list[i].valid_rep_list,
			ud.asq_profile.cds_offer_list[i].valid_emp_list);

		(void)printf("ForeColor=%d   BackColor=%d    Access Right=%d\n",
			ud.asq_profile.cds_offer_list[i].valid_fore_color,
			ud.asq_profile.cds_offer_list[i].valid_back_color,
			ud.asq_profile.cds_offer_list[i].access_right.bitcount);

		if(ud.asq_profile.cds_offer_list[i].valid_x_dim)
		{
			(void)printf("Bound=%d  Add=%d   Abs=%d   Window=%d\n",
				ud.asq_profile.cds_offer_list[i].x_dim.bound_type,
				ud.asq_profile.cds_offer_list[i].x_dim.addressing.bitcount,
				ud.asq_profile.cds_offer_list[i].x_dim.absolute.bitcount,
				ud.asq_profile.cds_offer_list[i].x_dim.window_type);

			if(ud.asq_profile.cds_offer_list[i].x_dim.window_type)
			{
				(void)printf("windowtype=%d   windowvalue=%d\n",
					ud.asq_profile.cds_offer_list[i].x_dim.window.type,
					ud.asq_profile.cds_offer_list[i].x_dim.window.value);
			}
		}
		if(ud.asq_profile.cds_offer_list[i].valid_rep_list)
		{
			(void)printf("\n");
			(void)printf("validcap=%d   Number of Repertoires=%d\n",
				ud.asq_profile.cds_offer_list[i].rep_offer.valid_cap,
				ud.asq_profile.cds_offer_list[i].rep_offer.num_reps);
			for(j=0;j<ud.asq_profile.cds_offer_list[i].rep_offer.num_reps;j++)
			{
			(void)printf("\n");
			(void)printf("reptype=%d  fontcap=%d   numfonts=%d\n",
				ud.asq_profile.cds_offer_list[i].rep_offer.repertoire[j].rep_type, ud.asq_profile.cds_offer_list[i].rep_offer.repertoire[j].valid_font_cap, ud.asq_profile.cds_offer_list[i].rep_offer.repertoire[j].num_fonts);
			if(ud.asq_profile.cds_offer_list[i].rep_offer.repertoire[j].rep_type == 2)
				(void)printf("Repertoire = %s\n", ud.asq_profile.cds_offer_list[i].rep_offer.repertoire[j].rep_assign);
			}
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

ASQpdu ::= CHOICE

{
	asqpdu [0] IMPLICIT ASQcontent [[p (PEPYPARM)parm]]
}

ASQcontent 
%{
	ASQ_MSG *arg =
			(ASQ_MSG *) parm;
%}
	::= SEQUENCE
%{
	arg->valid_imp = 0;
	arg->valid_prof = 0;
	arg->valid_coll = 0;
%}
{
	[0] IMPLICIT INTEGER [[i arg->class]],

	[1] IMPLICIT ImplemIdent [[p (PEPYPARM)&(arg->imp_id)]]
	%{arg->valid_imp = 1;%}
	OPTIONAL,

	[2] IMPLICIT BIT STRING
	%{ bitstr2int ($$, arg->func_units.bitstring, arg->func_units.bitcount); %},

	[3] IMPLICIT Profile [[p (PEPYPARM)&(arg->asq_profile)]]
	%{arg->valid_prof = 1;%}
	OPTIONAL,

	[4] IMPLICIT BIT STRING
	%{ bitstr2int ($$, arg->version.bitstring, arg->version.bitcount); %}
	OPTIONAL,

	[5] IMPLICIT INTEGER [[i arg->coll_winner]]
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
	%{arg->oid_true = 1;%}
	OPTIONAL,

	impName [1] IMPLICIT PrintableString
	[[s arg->name]]
	%{arg->name_true = 1;%}
	OPTIONAL,

	impVersion [2] IMPLICIT PrintableString
	[[s arg->version]]
	%{arg->version_true = 1;%}
}

Profile 
%{
	ARG_OFFER_LIST *arg = (ARG_OFFER_LIST *)parm;
%}
	::= SEQUENCE
%{
	arg->oid_true = 0;
	arg->num_sp_param = 0;
	arg->num_cds_objects = 0;
	arg->num_css_objects = 0;
	arg->num_dev_objects = 0;
	arg->del_ctrl.bitcount = 0;
%}
{
	name OBJECT IDENTIFIER [[O arg->prof_oid]]
	%{arg->oid_true = 1;%},

	ProfileArgList [[p parm]]
	OPTIONAL

}

ProfileArgList 
%{
	int k;
	ARG_OFFER_LIST	*arg = (ARG_OFFER_LIST *)parm;

%} ::= 
SEQUENCE OF %{

	q = arg->num_sp_param;
%}

	CHOICE
	{
		specialArgs [0] IMPLICIT SEQUENCE
		{
			identifier INTEGER
			[[i arg->sp_offer_list[q].param_num]],

			offeredValues CHOICE
			{
				boolean [0] IMPLICIT BIT STRING
				[[x arg->sp_offer_list[q].args.bool_arg $ k]]
				%{arg->sp_offer_list[q].param_type = 0;%},

				integer [1] IMPLICIT IntOffer [[p (PEPYPARM)&(arg->sp_offer_list[q].args.int_arg)]]
				%{arg->sp_offer_list[q].param_type = 1;%},

				string [2] IMPLICIT SET OF <<j=0; j<1; j++>>
					PrintableString
				[[s arg->sp_offer_list[q].args.string_arg]]
				%{arg->sp_offer_list[q].param_type = 2;%}
			}

			%{++arg->num_sp_param;
			  if(arg->num_sp_param >= MAXSPARGS) return(OK);
			%}
		},

		vteParams [1] IMPLICIT ParamOfferList [[p parm]]

	}

ParamOfferList 
%{
	ARG_OFFER_LIST	*arg = (ARG_OFFER_LIST *)parm;
%}
	::= SEQUENCE
{
	displayObjects 	[0] IMPLICIT CDSOffer [[p parm]]
	OPTIONAL,

--	controlObjects	[1] IMPLICIT CSSOffer [[p parm]]
--	OPTIONAL,

--	deviceObjects	[2] IMPLICIT DEVOffer [[p parm]]
--	OPTIONAL,

	deliveryControl	[3] IMPLICIT BIT STRING
	%{ bitstr2int ($$,
		       arg->del_ctrl.bitstring,
		       arg->del_ctrl.bitcount); %}
	OPTIONAL
}

--Note Problem with IMPLICIT SEQUENCE Definition below.  PEPY does not accept
--it as defined in 9041 and in fact that definition is ridiculous.  At the
--moment it is not clear if even hand coding available in ISODE 3.0 can
--produce the requirement of 9041.

CDSOffer 
%{
	ARG_OFFER_LIST	*arg = (ARG_OFFER_LIST *)parm;
%}
	::= 
SET OF 
%{
	l = arg->num_cds_objects;
%} SEQUENCE
{
	objectName PrintableString
		[[s arg->cds_offer_list[l].obj_name]],

	ObjectOffer [[p (PEPYPARM)&(arg->cds_offer_list[l])]]

	%{ ++arg->num_cds_objects;
	   if(arg->num_cds_objects >= MAXCDSOBJ) return(OK);
	%}
}

--CSSOffer ::= NULL 

--DEVOffer ::= NULL

ObjectOffer 
%{
	CDS_OFFER	*arg = (CDS_OFFER *)parm;
%}
	::= SEQUENCE
%{	
	arg->dimensions.bitcount = 0;
	arg->valid_x_dim = 0;
	arg->valid_y_dim = 0;
	arg->valid_z_dim = 0;
	arg->erasure.bitcount = 0;
	arg->valid_rep_list = 0;
	arg->valid_emp_list = 0;
	arg->valid_fore_color = 0;
	arg->valid_back_color = 0;
	arg->access_right.bitcount = 0;
%}
{
	dimensionOffer [0]	IMPLICIT BIT STRING
	%{ bitstr2int ($$,
		       arg->dimensions.bitstring,
		       arg->dimensions.bitcount); %}
	OPTIONAL,

	xParamOffer [1]		IMPLICIT DimOffer [[p (PEPYPARM)&(arg->x_dim)]]
	%{arg->valid_x_dim = 1;%}
	OPTIONAL,

	yParamOffer [2]		IMPLICIT DimOffer [[p (PEPYPARM)&(arg->y_dim)]]
	%{arg->valid_y_dim = 1;%}
	OPTIONAL,

	zParamOffer [3]		IMPLICIT DimOffer [[p (PEPYPARM)&(arg->z_dim)]]
	%{arg->valid_z_dim = 1;%}
	OPTIONAL,

--	erasuroffer [4]		IMPLICIT BIT STRING
--	%{ bitstr2int ($$,
--		       arg->erasure.bitstring,
--		       arg->erasure.bitcount); %}
--	OPTIONAL,

	repOfferList [5]	IMPLICIT CompRepOffer [[p (PEPYPARM)&(arg->rep_offer)]]
	%{arg->valid_rep_list = 1;%}
	OPTIONAL,

--	empOfferList [6]	IMPLICIT CompEmpOffer [[p (PEPYPRAM)&(arg->emp_offer)]]
--	%{arg->valid_emp_list = 1;%}
--	OPTIONAL,

--	foreColorList [7]	IMPLICIT ColorOffer [[p (PEPYPARM)&(arg->fore_color_list)]]
--	%{arg->valid_fore_color = 1;%}
--	OPTIONAL,

--	backColorList [8]	IMPLICIT ColorOffer [[p (PEPYPARM)&(arg->back_color_list)]]
--	%{arg->valid_back_color = 1;%}
--	OPTIONAL,

	objectAccRight [9]	IMPLICIT BIT STRING
	%{ bitstr2int ($$,
		       arg->access_right.bitstring,
		       arg->access_right.bitcount); %}
	OPTIONAL
}

DimOffer 
%{
	DIMEN_PARAM	*arg = (DIMEN_PARAM *)parm;
%}
	::= SEQUENCE
%{	
	arg->bound_type = 0;
	arg->addressing.bitcount = 0;
	arg->absolute.bitcount = 0;
	arg->window_type = 0;
%}

{
	bound		[0] IMPLICIT SEQUENCE
	{
		unbounded NULL
		%{arg->bound_type = 1;%}
		OPTIONAL,

		limit IntOffer [[p (PEPYPARM)&(arg->bound)]]
		%{arg->bound_type = 2;%}
	}
	OPTIONAL,

	addressing	[1] IMPLICIT BIT STRING
	%{ bitstr2int ($$,
		       arg->addressing.bitstring,
		       arg->addressing.bitcount); %}
	OPTIONAL,

	absolute	[2] IMPLICIT BIT STRING
	%{ bitstr2int ($$,
		       arg->absolute.bitstring,
		       arg->absolute.bitcount); %}
	OPTIONAL,

	window		[3] IMPLICIT SEQUENCE
	{
		unbounded NULL
		%{arg->window_type = 1;%}
		OPTIONAL,

		limit IntOffer [[p (PEPYPARM)&(arg->window)]]
		%{arg->window_type = 2;%}
		OPTIONAL
	}
	OPTIONAL
}

CompRepOffer 
%{
	REP_LIST	*arg = (REP_LIST *)parm;
%}
	::= SEQUENCE
%{	
	arg->valid_cap = 0;
	arg->num_reps = 0;
	arg->repertoire[m].valid_font_cap = 0;
	arg->repertoire[m].num_fonts = 0;
/*	arg->repertoire[m].rep_assign = 0; */
%}

{
	repCapability	[0] IMPLICIT IntOffer [[p (PEPYPARM)&(arg->capability)]]
	%{arg->valid_cap = 1;%}
	OPTIONAL,

			[1] IMPLICIT SEQUENCE OF 
			%{m = arg->num_reps;%}
				RepFontOffer [[p (PEPYPARM)&(arg->repertoire[m])]]
				%{++arg->num_reps;
				if(arg->num_reps >= MAXREPS) return(OK);
				%}
}

RepFontOffer 
%{
	REP_FONT	*arg = (REP_FONT *)parm;
%}
	::= 
CHOICE
{
	NULL
	%{arg->rep_type = 1;%},

	SEQUENCE
	{
		repertoire	[0] IMPLICIT PrintableString
		[[s arg->rep_assign]]
		OPTIONAL,

		fontCapability	[1] IMPLICIT IntOffer [[p (PEPYPARM)&(arg->capability)]]
		%{arg->valid_font_cap = 1;%}
		OPTIONAL,

		[2] IMPLICIT SEQUENCE OF
		%{n = arg->num_fonts;%}
		PrintableString
		[[s arg->font_names[n] ]]
		%{++arg->num_fonts;%}
		OPTIONAL
	}
	%{arg->rep_type = 2;%}
}

--CompEmpOffer ::= SEQUENCE
--%{	parm->asq_profile.cds_offer_list[l].emp_offer.valid_cap = 0;
--	parm->asq_profile.cds_offer_list[l].emp_offer.num_emps = 0;
--%}

--{
--	empCap		[0] IMPLICIT IntOffer [[p parm]]
--			%{parm->asq_profile.cds_offer_list[l].emp_offer.valid_cap = 1;%}
--			OPTIONAL,

--			SEQUENCE OF %{m = parm->asq_profile.cds_offer_list[l].emp_offer.num_emps;%}
--				PrintableString
--				[[s parm->asq_profile.cds_offer_list[l].emp_offer.emp_string[m] ]]
--				%{++parm->asq_profile.cds_offer_list[l].emp_offer.num_emps;
--				if(parm->asq_profile.cds_offer_list[l].emp_offer.num_emps >= MAXEMPS) return(OK);
--				%}
--				OPTIONAL
--}

--ColorOffer %{int i;%} ::= SEQUENCE
--%{	
--	COLOR_LIST	*arg = parm;
--
--	arg->valid_cap = 0;
--	arg->num_colors = 0;
--%}

--{
--	colorCap	[0] IMPLICIT FCIntOffer [[p parm]]
--			%{arg->valid_cap = 1;%}
--			OPTIONAL,

--	colorNames	SEQUENCE OF %{m = parm->asq_profile.cds_offer_list[l].fore_color_list.num_colors;%}
--				PrintableString
--				[[s parm->asq_profile.cds_offer_list[l].fore_color_list.color_string[m] ]]
--				%{++parm->asq_profile.cds_offer_list[l].fore_color_list.num_colors;
--				if(parm->asq_profile.cds_offer_list[l].fore_color_list.num_colors >= MAXCOLORS) return(OK);
--				%}
--				OPTIONAL
--}

IntOffer 
%{
	INT_OFFER	*arg = (INT_OFFER *)parm;
%}
	::= SEQUENCE OF

	CHOICE
	{
		indivValue [0] IMPLICIT INTEGER
		[[i arg->value]]
		%{arg->type = 0;%},

		range [1] IMPLICIT SEQUENCE
		%{arg->type = 1;%}
		{
			INTEGER [[i arg->min_val]],
			INTEGER [[i arg->max_val]]
		}
	}

END

%{

%}
