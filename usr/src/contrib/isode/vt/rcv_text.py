-- VTPM: decode NDQ PDU

-- $Header: /f/osi/vt/RCS/rcv_text.py,v 7.1 91/02/22 09:48:06 mrose Interim $
--
--
-- $Log:	rcv_text.py,v $
-- Revision 7.1  91/02/22  09:48:06  mrose
-- Interim 6.8
-- 
-- Revision 7.0  89/11/23  22:31:37  mrose
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

void	adios ();

#undef PEPYPARM
#define PEPYPARM int *

#undef	PEPYTEST

static int echo_sw;
static char *do_name;
extern TEXT_UPDATE *ndq_queue;

#ifdef PEPYTEST

TEXT_UPDATE ud;

rcv_text(pe)
PE pe;
{

	ud.echo_sw = ud.type_sw = -1;
	ud.updates.do_list.do_name = "\0";
	ud.updates.do_list.do_type = -1;
	ud.updates.do_list.do_cmd.text_ud.text_count = 0;

	if(unbuild_NDQPDU_NDQpdu(pe,1,NULLIP,NULLVP,&ud) == NOTOK) 
		return;

	(void)printf("Echo = %d; Type = %d; D.O. Name = %s; D. O. Update Type = %d\n", ud.echo_sw,ud.type_sw,ud.updates.do_list.do_name, ud.updates.do_list.do_type);
	if(ud.updates.do_list.do_cmd.text_ud.text_count) 
		(void)printf("Text = %s\n",ud.updates.do_list.do_cmd.text_ud.text_ptr);
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

NDQpdu ::= CHOICE

{
	ndqpdu [6] IMPLICIT NDQcontent [[p (PEPYPARM)0]]
}

NDQcontent ::= SEQUENCE OF VTsdi [[p (PEPYPARM)0]]

VTsdi ::= CHOICE
{
	echoNow 	[0] IMPLICIT SEQUENCE OF ObjectUpdate [[p (PEPYPARM)0]]
			%{echo_sw = ECHO_ON;%},
	notEchoNow	[1] IMPLICIT SEQUENCE OF ObjectUpdate [[p (PEPYPARM)0]]
			%{echo_sw = ECHO_OFF;%}
}

ObjectUpdate ::= CHOICE
{
	display		[0] IMPLICIT SEQUENCE
	{
			dOname PrintableString
			[[s do_name]],

			SEQUENCE OF DOupdate [[p (PEPYPARM)0]]
	},
	control		[1] IMPLICIT COupdate [[p (PEPYPARM)0]]
}

DOupdate 
%{
	TEXT_UPDATE	*arg = (TEXT_UPDATE *)parm;
%}
	::= CHOICE
%{
	if( !(arg = (TEXT_UPDATE *)malloc(sizeof(TEXT_UPDATE)) ))
	    adios (NULLCP, "out of memory");
	arg->echo_sw = echo_sw;
	arg->type_sw = DISPLAY_OBJ;
	arg->updates.do_list.do_name = do_name;
%}
{
		nextXarray	[0]	IMPLICIT NULL
			%{arg->updates.do_list.do_type = 0;
			(void) enq(&ndq_queue,arg);%},
		nextYarray	[1]	IMPLICIT NULL
			%{arg->updates.do_list.do_type = 1;
			(void) enq(&ndq_queue,arg);%},
		ptr-relative	[2]	IMPLICIT ExplicitPointer 
			[[p (PEPYPARM)&(arg->updates.do_list.do_cmd.ptr_rel)]]
			%{arg->updates.do_list.do_type = 2;
			(void) enq(&ndq_queue,arg);%},
		ptr-absolute	[3]	IMPLICIT Pointer 
			[[p (PEPYPARM)&(arg->updates.do_list.do_cmd.ptr_abs)]]
			%{arg->updates.do_list.do_type = 3;
			(void) enq(&ndq_queue,arg);%},
		text		[4]	IMPLICIT OCTETSTRING
					[[o arg->updates.do_list.do_cmd.text_ud.text_ptr $ arg->updates.do_list.do_cmd.text_ud.text_count]]
			%{arg->updates.do_list.do_type = 4;
			  (void) enq(&ndq_queue,arg);%},

		repeatText	[5]	IMPLICIT SEQUENCE {
						finishAddress Pointer
						[[p (PEPYPARM)parm]],

						OCTETSTRING
						[[o arg->updates.do_list.do_cmd.rpt_seq.text $ arg->updates.do_list.do_cmd.rpt_seq.text_count]]
					}
			%{arg->updates.do_list.do_type = 5;
			(void) enq(&ndq_queue,arg);%},
		writeAttr	[6]	IMPLICIT SEQUENCE {
						AttrId [[p (PEPYPARM)&(arg->updates.do_list.do_cmd.wrt_attrib)]],
						AttrExtent [[p (PEPYPARM)&(arg->updates.do_list.do_cmd.wrt_attrib)]]
					}
			%{arg->updates.do_list.do_type = 6;
			(void) enq(&ndq_queue,arg);%},
		erase		[7]	IMPLICIT SEQUENCE {
						startErase Pointer [[p (PEPYPARM)&(arg->updates.do_list.do_cmd.erase.start_erase)]],
						endErase Pointer [[p (PEPYPARM)&(arg->updates.do_list.do_cmd.erase.end_erase)]],
						eraseAttr BOOLEAN
						[[b arg->updates.do_list.do_cmd.erase.erase_attr]]
					}
			%{arg->updates.do_list.do_type = 7;
			(void) enq(&ndq_queue,arg);%},
		previousXarray	[8]	IMPLICIT NULL
			%{arg->updates.do_list.do_type = 8;
			(void) enq(&ndq_queue,arg);%},
		previousYarray	[9]	IMPLICIT NULL 
			%{arg->updates.do_list.do_type = 9;
			(void) enq(&ndq_queue,arg);%}
}

COupdate 
%{
	TEXT_UPDATE	*arg = (TEXT_UPDATE *)parm;
%}
	::= SEQUENCE 
%{
		if( !(arg = (TEXT_UPDATE *)malloc(sizeof(TEXT_UPDATE)) ))
		    adios (NULLCP, "out of memory");
		arg->echo_sw = echo_sw;
		arg->type_sw = CTRL_OBJ;
%}
{
		coName		PrintableString
		[[s arg->updates.co_list.co_name]],

		objectUpdate	CHOICE {
			characterUpdate [0] IMPLICIT PrintableString
			[[s arg->updates.co_list.co_cmd.char_update]]
			%{arg->updates.co_list.co_type = 0;
			(void) enq(&ndq_queue,arg);%},

			booleanUpdate [1] IMPLICIT SEQUENCE {
				values [0] IMPLICIT BITSTRING
				[[x arg->updates.co_list.co_cmd.bool_update.value $ arg->updates.co_list.co_cmd.bool_update.val_count]],

				mask [1] IMPLICIT BITSTRING
				[[x arg->updates.co_list.co_cmd.bool_update.mask $ arg->updates.co_list.co_cmd.bool_update.mask_count]]
			}
			%{arg->updates.co_list.co_type = 1;
			(void) enq(&ndq_queue,arg);%},

			symbolicUpdate [2] IMPLICIT INTEGER
			[[i arg->updates.co_list.co_cmd.sym_update]]
			%{arg->updates.co_list.co_type = 2;
			(void) enq(&ndq_queue,arg);%},

			integerUpdate [3] IMPLICIT INTEGER
			[[i arg->updates.co_list.co_cmd.int_update]]
			%{arg->updates.co_list.co_type = 3;
			(void) enq(&ndq_queue,arg);%},

			bitStringUpdate [4] IMPLICIT BITSTRING
			%{ bitstr2int ($$,
				       arg->updates.co_list.co_cmd.bit_update.bitstring,
				       arg->updates.co_list.co_cmd.bit_update.bitcount);
			arg->updates.co_list.co_type = 4;
			(void) enq(&ndq_queue,arg);%}
		}
}

ExplicitPointer 
%{
	EX_POINTER	*arg = (EX_POINTER *)parm;
%}
	::= SEQUENCE
			%{	arg->x_true = 0;
				arg->y_true = 0;
				arg->z_true = 0;
			%}
			{
			x [0] IMPLICIT INTEGER
			[[i arg->x_value]]
			%{arg->x_true = 1;%}
			OPTIONAL,

			y [1] IMPLICIT INTEGER
			[[i arg->y_value]]
			%{arg->y_true = 1;%}
			OPTIONAL,

			z [2] IMPLICIT INTEGER
			[[i arg->z_value]]
			%{arg->z_true = 1;%}
			OPTIONAL
}

Pointer
%{
	POINTER	*arg = (POINTER *)parm;
%}
	::= CHOICE {
		current [0] IMPLICIT NULL
			%{arg->ptr_type = 0;%},
		start	[1] IMPLICIT NULL
			%{arg->ptr_type = 1;%},
		startY	[2] IMPLICIT NULL
			%{arg->ptr_type = 2;%},
		startX	[3] IMPLICIT NULL
			%{arg->ptr_type = 3;%},
		end	[4] IMPLICIT NULL
			%{arg->ptr_type = 4;%},
		endY	[5] IMPLICIT NULL
			%{arg->ptr_type = 5;%},
		endX	[6] IMPLICIT NULL
			%{arg->ptr_type = 6;%},
		coords	[7] IMPLICIT ExplicitPointer [[p (PEPYPARM)&(arg->e_ptr)]]
			%{arg->ptr_type = 7;%}
}

AttrId 
%{
	ATTRIB	*arg = (ATTRIB *)parm;
%}
	::= CHOICE {
		graphCharRep	[0]	IMPLICIT INTEGER
		[[i arg->attr_val]]
		%{arg->attr_id = 0;%},

		foreColor	[1]	IMPLICIT INTEGER
		[[i arg->attr_val]]
		%{arg->attr_id = 1;%},

		backColor	[2]	IMPLICIT INTEGER
		[[i arg->attr_val]]
		%{arg->attr_id = 2;%},

		emphasis	[3]	IMPLICIT INTEGER
		[[i arg->attr_val]]
		%{arg->attr_id = 3;%},

		font		[4]	IMPLICIT INTEGER
		[[i arg->attr_val]]
		%{arg->attr_id = 4;%}
}

AttrExtent 
%{
	ATTRIB	*arg = (ATTRIB *)parm;
%}
	::= CHOICE {
		global 		[0]	IMPLICIT NULL
			%{arg->attr_ext = 0;%},
		addrExtent	[1]	IMPLICIT SEQUENCE {
					beginning Pointer [[p (PEPYPARM)&(arg->beg_p)]],
					ending Pointer [[p (PEPYPARM)&(arg->end_p)]]
					}
			%{arg->attr_ext = 1;%},
		modalExtent	[2]	IMPLICIT NULL
			%{arg->attr_ext = 2;%}
}

END

%{

/*
 * deq		take something out of a fifo queue (a la knuth)
 *		elements are circularly linked
 *		q head points to last entry in queue
 */

TEXT_UPDATE *
deq( qhp )
register TEXT_UPDATE **qhp;
{
	register TEXT_UPDATE *elem;


	if( (elem = (*qhp)) != 0 )
	{
		elem = elem->ndq_elem;
		(*qhp)->ndq_elem = elem->ndq_elem;
		if( elem == elem->ndq_elem )
			(*qhp) = 0;
	}

	return( elem );
}

/*
 * enq			enter something in a queue
 *			queue format is same as deq above
 */

enq( qhp,elem )
register TEXT_UPDATE **qhp;
register TEXT_UPDATE *elem;
{
	register TEXT_UPDATE *liq;

	if( (liq = (*qhp)) == 0 )
		(*qhp) = elem;
	elem->ndq_elem = (*qhp)->ndq_elem;
	(*qhp)->ndq_elem = elem;
	(*qhp) = elem;

	return ( (int) liq );	/* last-in-queue zero, says queue was empty */
}

/*
 * fiq			get the first in queue - no delinking
 *			return zero if nothing in queue
 */

TEXT_UPDATE *
fiq( qhp )
register TEXT_UPDATE **qhp;
{
	register TEXT_UPDATE *e;

	if( (e = *qhp) != 0 )		/* something in queue */
		e = e->ndq_elem;		/* get first in queue */

	return( e );			/* return that value */
}
%}
