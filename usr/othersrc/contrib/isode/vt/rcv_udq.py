-- VTPM: decode UDQ PDU

-- $Header: /f/osi/vt/RCS/rcv_udq.py,v 7.1 91/02/22 09:48:07 mrose Interim $
--
--
-- $Log:	rcv_udq.py,v $
-- Revision 7.1  91/02/22  09:48:07  mrose
-- Interim 6.8
-- 
-- Revision 7.0  89/11/23  22:31:39  mrose
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


UDQPDU DEFINITIONS ::=

%{
#include <stdio.h>
#include "sector1.h"
#undef PEPYPARM
#define PEPYPARM int *

void	adios ();


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

UDQpdu ::= CHOICE

{
	udqpdu [7] IMPLICIT COupdate [[p (PEPYPARM)parm]]
}

COupdate 
%{
	TEXT_UPDATE	*arg = (TEXT_UPDATE *)parm;
%}
	::= SEQUENCE 
{
		coName		PrintableString
		[[s arg->updates.co_list.co_name]],

		objectUpdate	CHOICE {
			characterUpdate [0] IMPLICIT PrintableString
			[[s arg->updates.co_list.co_cmd.char_update]]
			%{arg->updates.co_list.co_type = 0;%},

			booleanUpdate [1] IMPLICIT SEQUENCE {
				values [0] IMPLICIT BITSTRING
				[[x arg->updates.co_list.co_cmd.bool_update.value $ arg->updates.co_list.co_cmd.bool_update.val_count]],

				mask [1] IMPLICIT BITSTRING
				[[x arg->updates.co_list.co_cmd.bool_update.mask $ arg->updates.co_list.co_cmd.bool_update.mask_count]]
			}
			%{arg->updates.co_list.co_type = 1;%},

			symbolicUpdate [2] IMPLICIT INTEGER
			[[i arg->updates.co_list.co_cmd.sym_update]]
			%{arg->updates.co_list.co_type = 2;%},

			integerUpdate [3] IMPLICIT INTEGER
			[[i arg->updates.co_list.co_cmd.int_update]]
			%{arg->updates.co_list.co_type = 3;%},

			bitStringUpdate [4] IMPLICIT BITSTRING
			%{ bitstr2int ($$,
				       arg->updates.co_list.co_cmd.bit_update.bitstring,
				       arg->updates.co_list.co_cmd.bit_update.bitcount);
			arg->updates.co_list.co_type = 4;%}
		}
}
END
