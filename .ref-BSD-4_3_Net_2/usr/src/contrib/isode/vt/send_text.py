-- VTPM: encode NDQ PDU

-- $Header: /f/osi/vt/RCS/send_text.py,v 7.1 91/02/22 09:48:16 mrose Interim $
--
--
-- $Log:	send_text.py,v $
-- Revision 7.1  91/02/22  09:48:16  mrose
-- Interim 6.8
-- 
-- Revision 7.0  89/11/23  22:31:44  mrose
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


NDQPDU DEFINITIONS ::=

%{
#include <stdio.h>
#include "sector1.h"

#undef PEPYPARM
#define PEPYPARM int *
    
#undef	PEPYTEST

#ifdef PEPYTEST

char *myname;
int l;
TEXT_UPDATE ud;

TEXT_UPDATE *ndq_queue;

main(argc,argv)
int argc;
char **argv;
{

	PE pe;

	myname = argv[0];

	ud.echo_sw = 1;
	ud.type_sw = 0;
	ud.updates.do_list.do_name = "display";
	ud.updates.do_list.do_type = 5;
	ud.updates.do_list.do_cmd.text_ud.text_ptr = "pissant";
	ud.updates.do_list.do_cmd.text_ud.text_count = 7;

	build_NDQPDU_NDQpdu(&pe,1,NULL,NULLCP,&ud);

	print_NDQPDU_NDQpdu(pe,1,NULLIP,NULLVP,&ud);

	if(unbuild_NDQPDU_NDQpdu(pe,1,NULLIP,NULLVP,NULLCP)==NOTOK)
	(void)printf("Can't Unbuild\n");;

	exit(0);
}

#endif

%}

BEGIN

SECTIONS build none none

NDQpdu ::= CHOICE <<1>>

{
	ndqpdu [6] IMPLICIT NDQcontent [[p (PEPYPARM)parm]]
}

NDQcontent 
%{ int j; %} ::=
	SEQUENCE OF <<j=0; j<1; j++>> VTsdi [[p (PEPYPARM)parm]]

VTsdi 
%{
	int j,k;
	TEXT_UPDATE	*arg = (TEXT_UPDATE *)parm;
%} 
	::= CHOICE <<arg->echo_sw + 1>> {
		echoNow [0] IMPLICIT SEQUENCE OF <<k=0; k<1; k++>> ObjectUpdate
		[[p (PEPYPARM)parm]],

		notEchoNow [1] IMPLICIT SEQUENCE OF <<j=0; j<1; j++>> ObjectUpdate
		[[p (PEPYPARM)parm]]  
}

ObjectUpdate 
%{
	int j;
	TEXT_UPDATE	*arg = (TEXT_UPDATE *)parm;
%} ::=
	CHOICE <<arg->type_sw + 1>> {
		display [0] IMPLICIT SEQUENCE {
			doName PrintableString
				[[s arg->updates.do_list.do_name]],
			SEQUENCE OF <<j=0; j<1; j++>> 
				DOupdate [[p (PEPYPARM)&(arg->updates.do_list)]]
		},
		control [1] IMPLICIT COupdate [[p (PEPYPARM)&(arg->updates.co_list)]]
}


DOupdate 
%{
	DO_UPDATE	*arg = (DO_UPDATE *)parm;
%}
	::= CHOICE <<arg->do_type + 1>> {
		nextXarray	[0]	IMPLICIT NULL,
		nextYarray	[1]	IMPLICIT NULL,
		ptr-relative [2] IMPLICIT ExplicitPointer [[p (PEPYPARM)&(arg->do_cmd.ptr_rel)]],
		ptr-absolute [3] IMPLICIT Pointer [[p (PEPYPARM)&(arg->do_cmd.ptr_abs)]],
		text	[4]	IMPLICIT OCTETSTRING
			[[o arg->do_cmd.text_ud.text_ptr $ arg->do_cmd.text_ud.text_count]],

		repeatText	[5]	IMPLICIT SEQUENCE {
			finishAddress Pointer
			[[p (PEPYPARM)&(arg->do_cmd.rpt_seq.fin_addr)]],

			OCTETSTRING
			[[o arg->do_cmd.rpt_seq.text $ arg->do_cmd.rpt_seq.text_count]]
		},

		writeAttr	[6]	IMPLICIT SEQUENCE {
			AttrId [[p (PEPYPARM)&(arg->do_cmd.wrt_attrib)]],
			AttrExtent [[p (PEPYPARM)&(arg->do_cmd.wrt_attrib)]]
		},

		erase		[7]	IMPLICIT SEQUENCE {
			startErase Pointer[[p (PEPYPARM)&(arg->do_cmd.erase.start_erase)]],
			endErase Pointer [[p (PEPYPARM)&(arg->do_cmd.erase.end_erase)]],
			eraseAttr BOOLEAN
				[[b arg->do_cmd.erase.erase_attr]]
		},

		previousXarray	[8]	IMPLICIT NULL,
		previousYarray	[9]	IMPLICIT NULL 
	}

COupdate 
%{
	CO_UPDATE	*arg = (CO_UPDATE *)parm;
%}
	::= SEQUENCE {
		coName		PrintableString
			[[s arg->co_name]],

		objectUpdate	CHOICE <<arg->co_type + 1>> {
			characterUpdate [0] IMPLICIT PrintableString
			[[s arg->co_cmd.char_update]],

			booleanUpdate [1] IMPLICIT SEQUENCE {
				values [0] IMPLICIT BITSTRING
				[[x arg->co_cmd.bool_update.value $ arg->co_cmd.bool_update.val_count]],

				mask [1] IMPLICIT BITSTRING
				[[x arg->co_cmd.bool_update.mask $ arg->co_cmd.bool_update.mask_count]]
			},

			symbolicUpdate [2] IMPLICIT INTEGER
			[[i arg->co_cmd.sym_update]],

			integerUpdate [3] IMPLICIT INTEGER
			[[i arg->co_cmd.int_update]],

			bitStringUpdate [4] IMPLICIT BITSTRING
			[[x int2strb (arg->co_cmd.bit_update.bitstring,
				      arg->co_cmd.bit_update.bitcount)
			  $ arg->co_cmd.bit_update.bitcount]]
		}
}

ExplicitPointer 
%{
	EX_POINTER	*arg = (EX_POINTER *)parm;
%}
	::= SEQUENCE {
		x [0] IMPLICIT INTEGER
			[[i arg->x_value]]
			OPTIONAL
			<<arg->x_true>>,

		y [1] IMPLICIT INTEGER
			[[i arg->y_value]]
			OPTIONAL
			<<arg->y_true>>,

		z [2] IMPLICIT INTEGER
			[[i arg->z_value]]
			OPTIONAL
			<<arg->z_true>>
}

Pointer 
%{
	POINTER  *arg = (POINTER *)parm;
%}
	::= CHOICE <<arg->ptr_type + 1>> {
		current [0] IMPLICIT NULL,
		start	[1] IMPLICIT NULL,
		startY	[2] IMPLICIT NULL,
		startX	[3] IMPLICIT NULL,
		end	[4] IMPLICIT NULL,
		endY	[5] IMPLICIT NULL,
		endX	[6] IMPLICIT NULL,
		coords	[7] IMPLICIT ExplicitPointer [[p (PEPYPARM)&(arg->e_ptr)]]
}

AttrId 
%{
	ATTRIB	*arg = (ATTRIB *)parm;
%}
	::= CHOICE <<arg->attr_id + 1>> {
		graphCharRep	[0]	IMPLICIT INTEGER
		[[i arg->attr_val]],

		foreColor	[1]	IMPLICIT INTEGER
		[[i arg->attr_val]],

		backColor	[2]	IMPLICIT INTEGER
		[[i arg->attr_val]],

		emphasis	[3]	IMPLICIT INTEGER
		[[i arg->attr_val]],

		font		[4]	IMPLICIT INTEGER
		[[i arg->attr_val]]
}

AttrExtent 
%{
	ATTRIB	*arg = (ATTRIB *)parm;
%}
	::= CHOICE <<arg->attr_ext + 1>> {
		global 		[0]	IMPLICIT NULL,
		addrExtent	[1]	IMPLICIT SEQUENCE {
			beginning Pointer [[p (PEPYPARM)&(arg->beg_p)]],
			ending Pointer [[p (PEPYPARM)&(arg->end_p)]]
		},
		modalExtent	[2]	IMPLICIT NULL
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
PE pe;
char *words;
{

	printf("%s\n",words);
}

#endif

%}
