-- VTPM: encode ASQ PDU

-- $Header: /f/osi/vt/RCS/send_asq.py,v 7.1 91/02/22 09:48:13 mrose Interim $
--
--
-- $Log:	send_asq.py,v $
-- Revision 7.1  91/02/22  09:48:13  mrose
-- Interim 6.8
-- 
-- Revision 7.0  89/11/23  22:31:42  mrose
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

static int l,m,n;

#ifdef PEPYTEST

char *myname;
ASQ_MSG ud;
static char my_version = 0x01;
static char my_fu = 0x1c;
static char a_char = 0x01;

main(argc,argv)
int argc;
char **argv;
{

	PE pe;
	int i,j;

	myname = argv[0];

	ud.class = 1;
	ud.valid_prof = 1;
	ud.valid_imp = 0;
	ud.valid_coll = 0;
	ud.version.bitstring = my_version;
	ud.version.bitcount = 1;
	ud.func_units.bitstring = my_fu;
	ud.func_units.bitcount = 5;
	ud.asq_profile.oid_true = 1;
	ud.asq_profile.prof_oid = ode2oid("telnet");
	ud.asq_profile.num_sp_param = 1;
	ud.asq_profile.num_cds_objects = 2;
	ud.asq_profile.num_css_objects = 0;
	ud.asq_profile.num_dev_objects = 0;
	ud.asq_profile.sp_offer_list[0].param_num = 1;
	ud.asq_profile.sp_offer_list[0].param_type = 1;
	ud.asq_profile.sp_offer_list[0].args.int_arg.type = 0;
	ud.asq_profile.sp_offer_list[0].args.int_arg.value = 80;

	ud.asq_profile.cds_offer_list[0].obj_name = "KB";
	ud.asq_profile.cds_offer_list[1].obj_name = "DI";
	for(i=0; i<ud.asq_profile.num_cds_objects; i++)
	{
	    ud.asq_profile.cds_offer_list[i].valid_x_dim = 0;
	    ud.asq_profile.cds_offer_list[i].valid_y_dim = 0;
	    ud.asq_profile.cds_offer_list[i].valid_z_dim = 0;
	    ud.asq_profile.cds_offer_list[i].erasure.bitcount = 0;
	    ud.asq_profile.cds_offer_list[i].valid_rep_list = 1;
	    ud.asq_profile.cds_offer_list[i].valid_emp_list = 0;
	    ud.asq_profile.cds_offer_list[i].valid_fore_color = 0;
	    ud.asq_profile.cds_offer_list[i].valid_back_color = 0;
	    ud.asq_profile.cds_offer_list[i].access_right.bitcount = 0;
	    ud.asq_profile.cds_offer_list[i].rep_offer.valid_cap = 1;
	    ud.asq_profile.cds_offer_list[i].rep_offer.capability.type = 0;
	    ud.asq_profile.cds_offer_list[i].rep_offer.capability.value = 1;
	    ud.asq_profile.cds_offer_list[i].rep_offer.num_reps = 1;
	    ud.asq_profile.cds_offer_list[i].rep_offer.repertoire[0].rep_type = 2;
	    ud.asq_profile.cds_offer_list[i].rep_offer.repertoire[0].rep_assign = "ABC";
	    ud.asq_profile.cds_offer_list[i].rep_offer.repertoire[0].valid_font_cap = 0;
	}
	    
	build_ASQPDU_ASQpdu(&pe,1,NULL,NULLCP,&ud);

	 print_ASQPDU_ASQpdu(pe,1,NULLIP,NULLVP,&ud);

	 rcv_asq(pe);

	exit(0);
}

#endif

%}

BEGIN

SECTIONS build none none

ASQpdu ::= CHOICE <<1>>

{
	asqpdu [0] IMPLICIT ASQcontent [[p  (PEPYPARM)parm]]
}

ASQcontent 
%{
	ASQ_MSG	*arg = (ASQ_MSG *)parm;
%}
	::= SEQUENCE
{
	[0] IMPLICIT INTEGER [[i arg->class]],

	[1] IMPLICIT ImplemIdent [[p (PEPYPARM)&(arg->imp_id)]]
	OPTIONAL <<arg->valid_imp>>,

	[2] IMPLICIT BITSTRING
	[[x int2strb (arg->func_units.bitstring, arg->func_units.bitcount)
	  $ arg->func_units.bitcount]],

	[3] IMPLICIT Profile [[p (PEPYPARM)&(arg->asq_profile)]]
	OPTIONAL <<arg->valid_prof>>,

	[4] IMPLICIT BITSTRING
	[[x int2strb (arg->version.bitstring, arg->version.bitcount)
	  $ arg->version.bitcount]],

	[5] IMPLICIT INTEGER [[i arg->coll_winner]]
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

Profile 
%{
	ARG_OFFER_LIST	*arg = (ARG_OFFER_LIST *)parm;
%}
	::= SEQUENCE
{
	name OBJECT IDENTIFIER [[O arg->prof_oid]]
	OPTIONAL <<arg->oid_true>>,

	ProfileArgList [[p parm]]
	OPTIONAL <<arg->num_sp_param +
		   arg->num_cds_objects +
		   arg->num_css_objects +
		   arg->num_dev_objects>>

}

ProfileArgList 
%{
	int j,k;
	ARG_OFFER_LIST	*arg = (ARG_OFFER_LIST *)parm;
%} 
	::= 

SEQUENCE OF <<n=0; n<(arg->num_sp_param
			+ (arg->num_cds_objects?1:0)); n++>>

	CHOICE <<(n<arg->num_sp_param) ? 1 : 2>>
	{
		specialArgs [0] IMPLICIT SEQUENCE
		%{k=1;%}
		{
			identifier INTEGER
			[[i arg->sp_offer_list[n].param_num]],

			offeredValues CHOICE
			<<arg->sp_offer_list[n].param_type + 1>>
			{
				boolean [0] IMPLICIT BITSTRING
				[[x arg->sp_offer_list[n].args.bool_arg $ k]],

				integer [1] IMPLICIT IntOffer [[p (PEPYPARM)&(arg->sp_offer_list[n].args.int_arg)]],

				string [2] IMPLICIT SET OF <<j=0; j<1; j++>>
					PrintableString
				[[s arg->sp_offer_list[n].args.string_arg]]
			}
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
	OPTIONAL <<arg->num_cds_objects>>,

--	controlObjects	[1] IMPLICIT CSSOffer [[p parm]]
--	OPTIONAL <<arg->num_css_objects>>,

--	deviceObjects	[2] IMPLICIT DEVOffer [[p parm]]
--	OPTIONAL <<arg->num_dev_objects>>,

	deliveryControl	[3] IMPLICIT BITSTRING
	[[x int2strb (arg->del_ctrl.bitstring, arg->del_ctrl.bitcount)
	  $ arg->del_ctrl.bitcount]]
	OPTIONAL <<arg->del_ctrl.bitcount>>
}

--Note Problem with IMPLICIT SEQUENCE Definition below.  PEPY does not accept
--it as defined in 9041 and in fact that definition is ridiculous.  At the
--moment it is not clear if even hand coding available in ISODE 3.0 can
--produce the requirement of 9041.

CDSOffer 
%{
	ARG_OFFER_LIST	*arg = (ARG_OFFER_LIST *)parm;
%}
	::= SET OF <<l=0; l<arg->num_cds_objects; l++>> SEQUENCE
{
	objectName PrintableString
		[[s arg->cds_offer_list[l].obj_name]],

	ObjectOffer [[p (PEPYPARM)&(arg->cds_offer_list[l])]]
}

CSSOffer ::= NULL --Unused for now--

DEVOffer ::= NULL --Unused for now--

ObjectOffer 
%{
	CDS_OFFER	*arg = (CDS_OFFER *)parm;
%}
	::= SEQUENCE
{
	dimensionOffer [0]	IMPLICIT BITSTRING
	[[x int2strb (arg->dimensions.bitstring,
		      arg->dimensions.bitcount) $ arg->dimensions.bitcount]]
	OPTIONAL <<arg->dimensions.bitcount>>,

	xParamOffer [1]		IMPLICIT DimOffer [[p (PEPYPARM)&(arg->x_dim)]]
	OPTIONAL <<arg->valid_x_dim>>,

	yParamOffer [2]		IMPLICIT DimOffer [[p (PEPYPARM)&(arg->y_dim)]]
	OPTIONAL <<arg->valid_y_dim>>,

	zParamOffer [3]		IMPLICIT DimOffer [[p (PEPYPARM)&(arg->y_dim)]]
	OPTIONAL <<arg->valid_z_dim>>,

--	erasuroffer [4]		IMPLICIT BITSTRING
--	[[x int2strb (arg->erasure.bitstring,
--		      arg->erasure.bitcount)
--	  $ arg->erasure.bitcount]]
--	OPTIONAL <<arg->erasure.bitcount>>,

	repOfferList [5]	IMPLICIT CompRepOffer [[p (PEPYPARM)&(arg->rep_offer)]]
	OPTIONAL <<arg->valid_rep_list>>,

--	empOfferList [6]	IMPLICIT CompEmpOffer [[p (PEPYPARM)&(arg->emp_offer)]]
--	OPTIONAL <<arg->valid_emp_list>>,

--	foreColorList [7]	IMPLICIT ColorOffer [[p (PEPYPARM)&(arg->fore_color_list)]]
--	OPTIONAL <<arg->valid_fore_color>>,

--	backColorList [8]	IMPLICIT ColorOffer [[p (PEPYPARM)&(arg->back_color_list)]]
--	OPTIONAL <<arg->valid_back_color>>,

	objectAccRight [9]	IMPLICIT BITSTRING
	[[x int2strb (arg->access_right.bitstring,
		      arg->access_right.bitcount)
	  $ arg->access_right.bitcount]]
	OPTIONAL <<arg->access_right.bitcount>>
}

DimOffer 
%{
	DIMEN_PARAM	*arg = (DIMEN_PARAM *)parm;
%}
	::= SEQUENCE
{
	bound		[0] IMPLICIT SEQUENCE
	{
		unbounded NULL
		OPTIONAL <<arg->bound_type == 1>>,

		limit IntOffer [[p (PEPYPARM)&(arg->bound)]]
		OPTIONAL <<arg->bound_type == 2>>
	}
	OPTIONAL <<arg->bound_type>>,

	addressing	[1] IMPLICIT BITSTRING
	[[x int2strb (arg->addressing.bitstring,
		      arg->addressing.bitcount)
	  $ arg->addressing.bitcount]]
	OPTIONAL <<arg->addressing.bitcount>>,

	absolute	[2] IMPLICIT BITSTRING
	[[x int2strb (arg->absolute.bitstring,
		      arg->absolute.bitcount)
	  $ arg->absolute.bitcount]]
	OPTIONAL <<arg->absolute.bitcount>>,

	window		[3] IMPLICIT SEQUENCE
	{
		unbounded NULL
		OPTIONAL <<arg->window_type == 1>>,

		limit IntOffer [[p (PEPYPARM)&(arg->window)]]
		OPTIONAL <<arg->window_type == 2>>
	}
	OPTIONAL <<arg->window_type>>
}

CompRepOffer 
%{
	REP_LIST	*arg = (REP_LIST *)parm;
%}
	::= SEQUENCE
{
	repCapability	[0] IMPLICIT IntOffer [[p (PEPYPARM)&(arg->capability)]]
	OPTIONAL <<arg->valid_cap>>,

	[1] IMPLICIT SEQUENCE OF <<m=0; m<arg->num_reps; m++>>
		RepFontOffer [[p (PEPYPARM)&(arg->repertoire[m])]]
		OPTIONAL <<arg->num_reps>>
}

RepFontOffer 
%{ 
	int i; 
	REP_FONT	*arg = (REP_FONT *)parm;
%} ::= 
CHOICE <<arg->rep_type>>
{
	NULL,

	SEQUENCE
	{
		repertoire	[0] IMPLICIT PrintableString
		[[s arg->rep_assign]]
		OPTIONAL <<arg->rep_assign>>,

		fontCapability	[1] IMPLICIT IntOffer [[p (PEPYPARM)&(arg->capability)]]
		OPTIONAL <<arg->valid_font_cap>>,

				[2] IMPLICIT SEQUENCE OF
				<<i=0; i<arg->num_fonts; i++>>
				PrintableString
				[[s arg->font_names[i] ]]
				OPTIONAL <<arg->num_fonts>>
	}
}

--CompEmpOffer 
--%{
--	int i;
--	EMP_LIST	*arg = (EMP_LIST *)parm;
--%} ::= SEQUENCE
--{
--	empCap		[0] IMPLICIT IntOffer [[p (PEPYPARM)&(arg->capability)]]
--			OPTIONAL <<arg->valid_cap>>,

--			SEQUENCE OF <<i=0; i<arg->num_emps; i++>>
--				PrintableString
--				[[s arg->emp_string[i] ]]
--				OPTIONAL <<arg->num_emps>>
--}

--ColorOffer 
--%{
--	int i;
--	COLOR_LIST	*arg = (COLOR_LIST *)parm;
--%} ::= SEQUENCE
--{
--	colorCap	[0] IMPLICIT IntOffer [[p (PEPYPARM)&(arg->capability)]]
--			OPTIONAL <<arg->valid_cap>>,

--	colorNames	SEQUENCE OF <<i=0; i<arg->num_colors; i++>>
--				PrintableString
--				[[s arg->color_string[i] ]]
--				OPTIONAL <<arg->num_colors>>
--}

IntOffer 
%{
	int i;
	INT_OFFER	*arg = (INT_OFFER *)parm;
%} ::= SEQUENCE OF <<i=0; i<1; i++>>

	CHOICE <<arg->type + 1>>
	{
		indivValue [0] IMPLICIT INTEGER
		[[i arg->value]],

		range [1] IMPLICIT SEQUENCE
		{
			INTEGER [[i arg->min_val]],
			INTEGER [[i arg->max_val]]
		}
	}

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
