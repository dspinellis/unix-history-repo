-- VTPM: encode ASR PDU

-- $Header: /f/osi/vt/RCS/send_asr.py,v 7.1 91/02/22 09:48:15 mrose Interim $
--
--
-- $Log:	send_asr.py,v $
-- Revision 7.1  91/02/22  09:48:15  mrose
-- Interim 6.8
-- 
-- Revision 7.0  89/11/23  22:31:43  mrose
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

static int l,m;

#ifdef	PEPYTEST

char *myname;
ASR_MSG ud;
static char my_version = 0x01;
static char my_fu = 0x1c;

main(argc,argv)
int argc;
char **argv;
{

	PE pe;
	int i;

	myname = argv[0];

	ud.valid_reason = 0;
	ud.result = 1;
	ud.valid_imp = 0;
	ud.valid_arg_list = 1;
	ud.version.bitstring  = my_version;
	ud.version.bitcount = 1;
	ud.arg_list.num_sp_param = 1;
	ud.arg_list.num_cds_objects = 2;
	ud.arg_list.num_css_objects = 0;
	ud.arg_list.num_dev_objects = 0;
	ud.arg_list.sp_val[0].param_num = 1;
	ud.arg_list.sp_val[0].param_type = 1;
	ud.arg_list.sp_val[0].args.int_arg = 80;
	ud.arg_list.cds_val[0].obj_name = "KB";
	ud.arg_list.cds_val[1].obj_name = "DI";
	for(i=0; i<ud.arg_list.num_cds_objects; i++)
	{
		ud.arg_list.cds_val[i].dimensions = 0;
		ud.arg_list.cds_val[i].valid_x_dim = 0;
		ud.arg_list.cds_val[i].valid_y_dim = 0;
		ud.arg_list.cds_val[i].valid_z_dim = 0;
		ud.arg_list.cds_val[i].valid_erasure = 0;
		ud.arg_list.cds_val[i].valid_rep_list = 1;
		ud.arg_list.cds_val[i].valid_emp_list = 0;
		ud.arg_list.cds_val[i].valid_fore_color = 0;
		ud.arg_list.cds_val[i].valid_back_color = 0;
		ud.arg_list.cds_val[i].valid_access_right = 0;
		ud.arg_list.cds_val[i].rep_value.valid_cap = 1;
		ud.arg_list.cds_val[i].rep_value.capability = 1;
		ud.arg_list.cds_val[i].rep_value.num_reps = 1;
		ud.arg_list.cds_val[i].rep_value.repertoire[0].rep_type = 2;
		ud.arg_list.cds_val[i].rep_value.repertoire[0].rep_assign = 
					"ABC";
		ud.arg_list.cds_val[i].rep_value.repertoire[0].valid_font_cap =
					0;
	}
	ud.func_units.bitstring = my_fu;
	ud.func_units.bitcount = 5;

	build_ASRPDU_ASRpdu(&pe,1,NULL,NULLCP,&ud);

	print_ASRPDU_ASRpdu(pe,1,NULLIP,NULLVP,&ud);

	rcv_asr(pe);

	exit(0);
}

#endif

%}

BEGIN

SECTIONS build none none

ASRpdu ::= CHOICE <<1>>

{
	asrpdu [1] IMPLICIT ASRcontent [[p (PEPYPARM)parm]]
}

ASRcontent 
%{
	ASR_MSG	*arg = (ASR_MSG *)parm;
%}
	::= SEQUENCE
{
	userReason [0] IMPLICIT PrintableString [[s arg->reason.usr_reason]]
	OPTIONAL <<(arg->valid_reason && (arg->reason.type == 0))>>,

	provReason [1] IMPLICIT INTEGER [[i arg->reason.provider_reason]]
	OPTIONAL <<(arg->valid_reason && (arg->reason.type == 1))>>,

	[2] IMPLICIT INTEGER [[i arg->result]],

	[3] IMPLICIT ImplemIdent [[p (PEPYPARM)&(arg->imp_id)]]
	OPTIONAL <<arg->valid_imp>>,

	[4] IMPLICIT BITSTRING
	[[x int2strb (arg->version.bitstring, arg->version.bitcount)
	  $ arg->version.bitcount]],

	[5] IMPLICIT ArgumValueList [[p (PEPYPARM)&(arg->arg_list)]]
	OPTIONAL <<arg->valid_arg_list>>,

	[6] IMPLICIT BITSTRING
	[[x int2strb (arg->func_units.bitstring, arg->func_units.bitcount)
	  $ arg->func_units.bitcount]],

	[7] IMPLICIT INTEGER [[i arg->coll_winner]]
	OPTIONAL <<arg->valid_coll>>
}

ImplemIdent 
%{
	IMPLEM_ID	*arg = (IMPLEM_ID *)parm;
%}
	::= SEQUENCE
{
	impIdent [0] IMPLICIT OBJECT IDENTIFIER
	[[O arg->imp_oid]]
	OPTIONAL <<arg->oid_true>>,

	impName [1] IMPLICIT PrintableString
	[[s arg->name]]
	OPTIONAL <<arg->name_true>>,

	impVersion [2] IMPLICIT PrintableString
	[[s arg->version]]
	OPTIONAL <<arg->version_true>>
}

ArgumValueList 
%{
	ARG_VAL_LIST	*arg = (ARG_VAL_LIST *)parm;
%}
	::= SET OF <<l=0; l<(arg->num_sp_param
			+ (arg->num_cds_objects?1:0)); l++>> Goobers [[p parm]]

Goobers 
%{
	ARG_VAL_LIST	*arg = (ARG_VAL_LIST *)parm;
%}
	::= CHOICE <<(l < arg->num_sp_param) ? 1 : 2>>
{
	[0] IMPLICIT SpecialArgs [[p (PEPYPARM)&(arg->sp_val[l])]],
	vteParams [1] IMPLICIT ParamValueList [[p parm]]
}

SpecialArgs 
%{
	SPECIAL_VALUE	*arg = (SPECIAL_VALUE *)parm;
%}
	::= SEQUENCE
{
	identifier INTEGER [[i arg->param_num]],
	value CHOICE <<arg->param_type + 1>>
	{
		BOOLEAN [[b arg->args.bool_arg]],
		INTEGER [[i arg->args.int_arg]],
		PrintableString[[s arg->args.string_arg]]
	}
}

ParamValueList 
%{
	ARG_VAL_LIST	*arg = (ARG_VAL_LIST *)parm;
%}
	::= SEQUENCE
{
	displayObjects 	[0] IMPLICIT CDSValues [[p parm]]
	OPTIONAL <<arg->num_cds_objects>>,

--	controlObjects	[1] IMPLICIT CSSValues [[p parm]]
--	OPTIONAL <<arg->num_css_objects>>,

--	deviceObjects	[2] IMPLICIT DEVValues [[p parm]]
--	OPTIONAL <<arg->num_dev_objects>>,

	deliveryControl	[3] IMPLICIT INTEGER
	[[i arg->del_ctrl]]
	OPTIONAL <<arg->del_ctrl>>
}

--Note Problem with IMPLICIT SEQUENCE Definition below.  PEPY does not accept
--it as defined in 9041 and in fact that definition is ridiculous.  At the
--moment it is not clear if even hand coding available in ISODE 3.0 can
--produce the requirement of 9041.

CDSValues
%{
	ARG_VAL_LIST	*arg = (ARG_VAL_LIST *)parm;
%}
	::= SET OF <<l=0; l<arg->num_cds_objects; l++>> SEQUENCE
{
	objectName PrintableString
		[[s arg->cds_val[l].obj_name]],

	ObjectOffer [[p (PEPYPARM)&(arg->cds_val[l])]]
}

CSSValues ::= NULL --Unused for now--

DEVValues ::= NULL --Unused for now--

ObjectOffer 
%{
	CDS_VALUE	*arg = (CDS_VALUE *)parm;
%}
	::= SEQUENCE
{
	dimensionValue [0]	IMPLICIT INTEGER
	[[i arg->dimensions]]
	OPTIONAL <<arg->dimensions>>,

	xParamValue [1]		IMPLICIT DimValue [[p 
	(PEPYPARM)&(arg->x_dim)]]
	OPTIONAL <<arg->valid_x_dim>>,

	yParamValue [2]		IMPLICIT DimValue [[p (PEPYPARM)&(arg->y_dim)]]
	OPTIONAL <<arg->valid_y_dim>>,

	zParamValue [3]		IMPLICIT DimValue [[p (PEPYPARM)&(arg->z_dim)]]
	OPTIONAL <<arg->valid_z_dim>>,

--	erasurevalue [4]	IMPLICIT BOOLEAN
--	[[b arg->erasure]]
--	OPTIONAL <<arg->valid_erasure>>,

	repValueList [5]	IMPLICIT CompRepValue [[p (PEPYPARM)&(arg->rep_value)]]
	OPTIONAL <<arg->valid_rep_list>>,

--	empValueList [6]	IMPLICIT CompEmpValue [[p (PEPYPARM)&(arg->emp_value)]]
--	OPTIONAL <<arg->valid_emp_list>>,

--	foreColorVal [7]	IMPLICIT ColorValue [[p (PEPYPARM)&(arg->fore_color_list)]]
--	OPTIONAL <<arg->valid_fore_color>>,

--	backColorVal [8]	IMPLICIT ColorValue [[p (PEPYPARM)&(arg->back_color_list)]]
--	OPTIONAL <<arg->valid_back_color>>,

	objectAccRight [9]	IMPLICIT INTEGER
	[[i arg->access_right]]
	OPTIONAL <<arg->valid_access_right>>
}

DimValue 
%{
	DIMEN_VALUE	*arg = (DIMEN_VALUE *)parm;
%}
	::= SEQUENCE
{
	bound		[0] CHOICE
	<<arg->bound_type>>
	{
		unbounded NULL,

		limit INTEGER
		[[i arg->bound]]
	}
	OPTIONAL <<arg->bound_type>>,

	addressing	[1] IMPLICIT INTEGER
	[[i arg->addressing]]
	OPTIONAL <<arg->valid_addr>>,

	absolute	[2] IMPLICIT INTEGER
	[[i arg->absolute]]
	OPTIONAL <<arg->valid_abs>>,

	window		[3] CHOICE
	<<arg->window_type>>
	{
		unbounded NULL,

		limit INTEGER
		[[i arg->window]]
	}
	OPTIONAL <<arg->window_type>>
}

CompRepValue 
%{
	REP_VALUE	*arg = (REP_VALUE *)parm;
%}
	::= SEQUENCE
{
	repCapability	[0] IMPLICIT INTEGER
	[[i arg->capability]]
	OPTIONAL <<arg->valid_cap>>,

	[1] IMPLICIT SEQUENCE OF <<m=0; m<arg->num_reps; m++>>
		RepFontValue [[p (PEPYPARM)&(arg->repertoire[m])]]
		OPTIONAL <<arg->num_reps>>
}

RepFontValue 
%{ 
	int i; 
	FONT_VALUE	*arg = (FONT_VALUE *)parm;
%} ::= 
CHOICE <<arg->rep_type>>
{
	NULL,

	SEQUENCE
	{
		repertoire	[0] IMPLICIT PrintableString
		[[s arg->rep_assign]]
		OPTIONAL <<arg->rep_assign>>,

		fontCapability	[1] IMPLICIT INTEGER
		[[i arg->capability]]
		OPTIONAL <<arg->valid_font_cap>>,

				[2] IMPLICIT SEQUENCE OF
				<<i=0; i<arg->num_fonts; i++>>
				PrintableString
				[[s arg->font_names[i] ]]
				OPTIONAL <<arg->num_fonts>>
	}
}

--CompEmpValue 
--%{
--	int i;
--	EMP_VALUE	*arg = (EMP_VALUE *)parm;
--%} ::= SEQUENCE
--{
--	empCap		[0] IMPLICIT INTEGER
--			[[i arg->capability]]
--			OPTIONAL <<arg->valid_cap>>,

--			SEQUENCE OF <<i=0; i<arg->num_emps; i++>>
--				PrintableString
--				[[s arg->emp_string[i] ]]
--				OPTIONAL <<arg->num_emps>>
--}

--ColorValue 
--%{
--	int i;
--	COLOR_VALUE	*arg = (COLOR_VALUE *)parm;
--%} ::= SEQUENCE
--{
--	colorCap	[0] IMPLICIT INTEGER
--			[[i arg->capability]]
--			OPTIONAL <<arg->valid_cap>>,

--	colorNames	SEQUENCE OF <<i=0; i<arg->num_colors; i++>>
--				PrintableString
--				[[s arg->color_string[i] ]]
--				OPTIONAL <<arg->num_colors>>
--}

END

%{

#ifdef PEPYTEST

void advise(what,fmt,a,b,c,d,e,f,g,h,i,j)
char *what, *fmt, *a, *b, *c, *d, *e, *f, *g, *h, *i, *j;
{

	(void) fflush(stdout);

	fprintf(stderr,"%s:  ",myname);
	fprintf(stderr,fmt,a,b,c,d,e,f,g,h,i,j);
	if(what)
		(void) fputc(' ',stderr),perror(what);
	else
		(void) fputc('\n',stderr);
	(void)fflush(stderr);
}

testdebug(pe,words)
PE pe;			/*Not Really, but pretend*/
char *words;
{

	printf("%s \n",words);
}

#endif

%}
