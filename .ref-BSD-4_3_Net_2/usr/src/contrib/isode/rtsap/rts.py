-- rts.py - RTS definitions
--	lifted directly from ISO9066-2

-- $Header: /f/osi/rtsap/RCS/rts.py,v 7.2 91/02/22 09:42:35 mrose Interim $
--
--
-- $Log:	rts.py,v $
-- Revision 7.2  91/02/22  09:42:35  mrose
-- Interim 6.8
-- 
-- Revision 7.1  90/11/04  19:18:05  mrose
-- update
-- 
-- Revision 7.0  89/11/23  22:22:33  mrose
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


--* Reliable-Transfer-APDUs *-- RTS
--*  { joint-iso-ccitt reliable-transfer(3) apdus(0) } *--
DEFINITIONS ::=

%{
#ifndef	lint
static char *rcsid = "$Header: /f/osi/rtsap/RCS/rts.py,v 7.2 91/02/22 09:42:35 mrose Interim $";
#endif

#include <stdio.h>
#include "rtpkt.h"


int	rtsap_priority;

/*  */
%}

BEGIN

-- EXPORTS
--	rTSE, rTSE-abstract,syntax, RTORQapdu, RTOACapdu, RTORJapdu, RTABapdu;

-- IMPORTS APPLICATION-SERVICE-ELEMENT
--	    FROM RemoteOperations-Notation-extension
--	    { joint-ccitt-iso remote-operations(4) notation-extension(2) };
  
-- rTSE-abstract-syntax OBJECT IDENTIFIER ::=
--	{ joint-iso-ccit reliable-transfer(3) abstract-syntax(2) }
-- rTSE APPLICATION-SERVICE-ELEMENT ::=
--	{ joint-iso-ccit reliable-transfer(3) aseID(1) }
    
RTSE-apdus ::=
        CHOICE {
	    rtorq-apdu[16]
		IMPLICIT RTORQapdu,

	    rtoac-apdu[17]
		IMPLICIT RTOACapdu,

	    rtorj-apdu[18]
		IMPLICIT RTORJapdu,

	    rttp-apdu
		RTTPapdu,

	    rttr-apdu
		RTTRapdu,

	    rtab-apdu[22]
		IMPLICIT RTABapdu
	}

-- Tags [19], [20], [21] are used by the values of the UNBIND macro of
-- the RO-notation of ISO 9072-1.  Tags [0] to [15] inclusive are
-- reserved for the use by the APDUs of ROSE (ISO 9072-2).  Any
-- occurrence of ANY in this module shall be replaced by a single ASN.1
-- type (if any) in an RTSE-user protocol specification.  In addition,
-- any RTSE-user protocol sharing a single named abstract syntax with
-- the RTSE protocol shall use distinct tags for the single presetnation
-- data values in the user data paramters of the RT-CLOSE 9f any) and
-- RT-TRANSFER services.  These tags shall be distinct from the tag
-- values [16], [17], [18] and [22] and from the ASN.1 types INTEGER and
-- OCTET STRING.  Note: the above conditions are ensured if the
-- RTSE-user protocol specification uses the RO-ntation of ISO9072-1

-- In X.410-1984 mode only the components of the RTORQapdu, RTOACapdu,
-- RTORJapdu and RTABapdu are used by the presentation layer.  This has
-- the effect that the following APDU types appear in the protocol in
-- X.410-1984 mode instead of the alternate types of the RTSE-apdus type:
--	RTORQapdu
--	RTOACapdu
--	RTORJapdu
--	RTTPapdu
--	RTTRapdu
--	RTABapdu

RTORQapdu ::=
	SET {
	    checkpointSize[0]
		IMPLICIT INTEGER
		DEFAULT 0,

	    windowSize[1]
		IMPLICIT INTEGER
		DEFAULT 3,

	    dialogueMode[2]
		IMPLICIT INTEGER { monologue(0), twa(1)}
		DEFAULT monologue,

	    connectionDataRQ[3]
		ConnectionData,

	    applicationProtocol[4]
		IMPLICIT INTEGER
		OPTIONAL		-- solely in X.410-1984 mode
	}

RTOACapdu ::=
	SET {
	    checkpointSize[0]
		IMPLICIT INTEGER
		DEFAULT 0,

	    windowSize[1]
		IMPLICIT INTEGER
		DEFAULT 3,

	    connectionDataAC[2]
		ConnectionData
	}

RTORJapdu ::=
	SET {
	    refuseReason[0]
		IMPLICIT OACS.RefuseReason
		OPTIONAL,		-- only in X.410-1984 mode

	    userDataRJ[1]
		ANY
		OPTIONAL		-- RTSE user data, only in normal mode
	}

RTTPapdu ::=
	-- priority
	INTEGER

RTTRapdu ::=
	OCTET STRING

RTABapdu ::=
	SET {
	    abortReason[0]
		IMPLICIT AbortReason
		OPTIONAL,

	    reflectedParameter[1]
		IMPLICIT BIT STRING
		OPTIONAL,
		-- 8 bits maximum, only if abortReason is invalidParameter

	    userdataAB[2]
		ANY
		OPTIONAL
		-- only in normal mode and if if abortReason is userError
	}

ConnectionData ::=
	CHOICE {
	    open[0]		-- RTSE user data
				-- this is encoded as [0]IMPLICIT NULL
				-- in the case of absence of RTSE user data
		ANY,

	    recover[1] IMPLICIT SessionConnectionIdentifier
	}

SessionConnectionIdentifier ::=
	SEQUENCE {
		CallingSSuserReference,

		CommonReference,

	    [0] IMPLICIT AdditionalReferenceInformation OPTIONAL
	}

-- RefuseReason in module OACS for hysterical (sic) reasons

CallingSSuserReference ::=
	CHOICE {
		T61String,	-- solely in X.410-1984 mode

		OCTET STRING	-- solely in normal mode
	}

CommonReference ::=
	UTCTime

AdditionalReferenceInformation ::=
	T61String

AbortReason ::=
	INTEGER {
	    localSystemProblem(0),

	    invalidParameter(1),	-- reflectedParameter supplied

	    unrecognizedActivity(2),

	    temporaryProblem(3),	-- the RTSE cannot accept a session
					-- for a period of time

	    protocolError(4),		-- RTSE level protocol error

	    permanentProblem(5),	-- provider-abort solely in normal mode

	    userError(6),		-- user-abort solely in normal mode

	    transferCompleted(7)	-- activity can't be discarded
	}
END
