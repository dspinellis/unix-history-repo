-- acsold.py - AcSAP definitions for old-style applications

-- $Header: /f/osi/acsap/RCS/acsold.py,v 7.3 91/02/22 09:14:27 mrose Interim $
--
--
-- $Log:	acsold.py,v $
-- Revision 7.3  91/02/22  09:14:27  mrose
-- Interim 6.8
-- 
-- Revision 7.2  90/07/01  21:02:14  mrose
-- pepsy
-- 
-- Revision 7.1  90/01/11  18:34:59  mrose
-- real-sync
-- 
-- Revision 7.0  89/11/23  21:22:04  mrose
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


OACS DEFINITIONS ::=

BEGIN

-- these are used by pre-ACSE versions of ROS and RTS
-- they are not part of the 8650 standard, but are included here for use
-- by old-style applications

PConnect ::= 
	SET {
	    [0]
		IMPLICIT DataTransferSyntax,

	    pUserData[1]
		IMPLICIT SET {
		    checkpointSize[0]
			IMPLICIT INTEGER	
			DEFAULT 0,

		    windowSize[1]
			IMPLICIT INTEGER	
			DEFAULT 3,

		    dialogueMode[2]
			IMPLICIT INTEGER {
			    monologue(0), twa(1)
			}			
			DEFAULT monologue,

		    [3]
			ConnectionData,

		    applicationProtocol[4]
			--* IMPLICIT INTEGER { *--
			--*	p1(1), p3(3)   *--
			--*  }                 *--
			IMPLICIT INTEGER	
			DEFAULT --* p1 *-- 1,

		    protocolVersion[5]
			IMPLICIT INTEGER
			OPTIONAL
	        }
	}

PAccept ::=
	SET {
	    [0]
		IMPLICIT DataTransferSyntax,

	    pUserData[1]
		IMPLICIT SET {
		    checkpointSize[0]
			IMPLICIT INTEGER	
			DEFAULT 0,

		    windowsize[1]
			IMPLICIT INTEGER	
			DEFAULT 3,

		    [2]
			ConnectionData
		}
	}

PRefuse ::=
	SET {
	    [0]
		IMPLICIT RefuseReason
	}

DataTransferSyntax ::=
	SET {
	    [0]
		IMPLICIT INTEGER { x409(0) }
	}

ConnectionData ::=
	CHOICE {
	    open[0]			-- ACS user data
		ANY,
	    recover[1]
		IMPLICIT SessionConnectionIdentifier
					
	}

SessionConnectionIdentifier ::=
	SEQUENCE {
	    CallingSSUserReference,

	    CommonReference,

	    [0]
		IMPLICIT AdditionalReferenceInformation
		OPTIONAL
	}

CallingSSUserReference ::=
	SSAPAddress				-- of the initiator

CommonReference ::=
	UTCTime

AdditionalReferenceInformation ::=
	T61String

SSAPAddress ::=
	T61String


RefuseReason ::=
	INTEGER {
	    busy(0),
	    cannotRecover(1),
	    validationFailure(2),
	    unacceptableDialogueMode(3)
	}				


-- similarly, this is used during a provider-initiated abort

AbortInformation ::=
	SET {
	    [0]
		IMPLICIT AbortReason
		OPTIONAL,

	    reflectedParameter[1]
		IMPLICIT BITSTRING
		OPTIONAL
	}

AbortReason ::=
	INTEGER {
	    localSystemProblem(0),

	    invalidParameter(1),	-- reflectedParameter supplied

	    unrecognizedActivity(2),

	    temporaryProblem(3),	-- the ACS cannot accept a session
					-- for a period of time

	    protocolError(4)		-- ACS level protocol error
	}				


-- these are definitions from CCITT X.410

Priority ::=
	INTEGER					

END
