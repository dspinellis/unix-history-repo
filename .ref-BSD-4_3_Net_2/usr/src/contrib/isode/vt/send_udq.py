-- VTPM: encode UDQ PDU

-- $Header: /f/osi/vt/RCS/send_udq.py,v 7.1 91/02/22 09:48:18 mrose Interim $
--
--
-- $Log:	send_udq.py,v $
-- Revision 7.1  91/02/22  09:48:18  mrose
-- Interim 6.8
-- 
-- Revision 7.0  89/11/23  22:31:45  mrose
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
    

%}

BEGIN

SECTIONS build none none

UDQpdu ::= CHOICE <<1>>

{
	udqpdu [7] IMPLICIT COupdate [[p (PEPYPARM)parm]]
}


COupdate 
%{
	TEXT_UPDATE	*ud = (TEXT_UPDATE *) parm;
	CO_UPDATE	*arg = (CO_UPDATE *) &(ud->updates.co_list);
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
END
