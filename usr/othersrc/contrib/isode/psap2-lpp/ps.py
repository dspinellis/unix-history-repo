-- ps.py - pseudo-presentation service definitions
--	lifted directly from RFC1085

-- $Header: /f/osi/psap2-lpp/RCS/ps.py,v 7.1 91/02/22 09:38:01 mrose Interim $
--
-- Contributed by The Wollongong Group, Inc.
--
--
-- $Log:	ps.py,v $
-- Revision 7.1  91/02/22  09:38:01  mrose
-- Interim 6.8
-- 
-- Revision 7.0  89/11/23  22:15:49  mrose
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


--* RFC1085-PS *-- PS DEFINITIONS ::=

%{
#ifndef	lint
static char *rcsid = "$Header: /f/osi/psap2-lpp/RCS/ps.py,v 7.1 91/02/22 09:38:01 mrose Interim $";
#endif
%}

BEGIN

PDUs ::=
	CHOICE {
	    connectRequest
		ConnectRequest-PDU,

	    connectResponse
		ConnectResponse-PDU,

	    releaseRequest
		ReleaseRequest-PDU,

	    releaseResponse
		ReleaseResponse-PDU,

	    abort
		Abort-PDU,

	    userData
		UserData-PDU,

	    cL-userData
		CL-UserData-PDU
	}
	    

-- connect request PDU

ConnectRequest-PDU ::=
    [0]
        IMPLICIT SEQUENCE {
            version[0]		-- version-1 corresponds to to this memo
                IMPLICIT INTEGER { version-1(0) },

            reference
                SessionConnectionIdentifier,

	    calling
		PresentationSelector
		OPTIONAL,

	    called[2]
		IMPLICIT PresentationSelector
		OPTIONAL,

	    asn[3]		-- the ASN for PCI #1
		IMPLICIT OBJECT IDENTIFIER,

            user-data
                UserData-PDU
        }

SessionConnectionIdentifier ::=
    [0]
	SEQUENCE {
	    callingSSUserReference
		T61String,

	    commonReference
		UTCTime,

	    additionalReferenceInformation[0]
		IMPLICIT T61String
		OPTIONAL
	}

PresentationSelector ::=
    [1]
	IMPLICIT OCTET STRING


-- connect response PDU

ConnectResponse-PDU ::=
    [1]
        IMPLICIT SEQUENCE {
            reference		-- present only in the udp-based service
                SessionConnectionIdentifier
		OPTIONAL,

	    responding
		PresentationSelector
		OPTIONAL,

            reason[2]		-- present only if the connection was rejected
                IMPLICIT Rejection-reason
                OPTIONAL,

	    user-data		-- present only if reason is absent OR has the
				-- value rejected-by-responder
                UserData-PDU
		OPTIONAL
        }

Rejection-reason ::=
        INTEGER {
            rejected-by-responder(0),
            called-presentation-address-unknown(1),
            local-limit-exceeded(3),
            protocol-version-not-supported(4)
        }


-- release request PDU

ReleaseRequest-PDU ::=
    [2]
        IMPLICIT SEQUENCE {
            reference		-- present only in the udp-based service
                SessionConnectionIdentifier
		OPTIONAL,

	    user-data
                UserData-PDU
        }


-- release response PDU

ReleaseResponse-PDU ::=
    [3]
        IMPLICIT SEQUENCE {
            reference		-- present only in the udp-based service
                SessionConnectionIdentifier
		OPTIONAL,

	    user-data
                UserData-PDU
        }

-- abort PDU

Abort-PDU ::=
    [4]
        SEQUENCE {
            reference		-- present only in the udp-based service
                SessionConnectionIdentifier
                OPTIONAL,

	    user-data	-- MAY BE present on user-initiated abort
		UserData-PDU
		OPTIONAL,

	    reason[1]	-- ALWAYS present on provider-initiated abort
		IMPLICIT Abort-reason
		OPTIONAL
        }

Abort-reason ::=
        INTEGER {
            reason-not-specified(0),
            unrecognized-ppdu(1),
            unexpected-ppdu(2),
            unrecognized-ppdu-parameter(4),
            invalid-ppdu-parameter(5),
	    reference-mismatch(9)
        }


-- data PDU

UserData-PDU ::=
    [5]					-- this is the ASN.1 object
	ANY				-- if it is a top-level PDU, it
					-- is in PCI #1, otherwise PCI #3


-- data PDU for the udp-based service

CL-UserData-PDU ::=
    [6]
	IMPLICIT SEQUENCE {
            reference
                SessionConnectionIdentifier,

	    user-data[0]		-- this is the ASN.1 object
		ANY			-- it is always in PCI #1
	}

END
