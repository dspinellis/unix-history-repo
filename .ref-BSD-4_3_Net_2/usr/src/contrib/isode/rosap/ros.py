-- ros.py - ROS definitions
--	lifted directly from ISO9072-2

-- $Header: /f/osi/rosap/RCS/ros.py,v 7.2 91/02/22 09:41:25 mrose Interim $
--
-- Based on an TCP-based implementation by George Michaelson of University
-- College London.
--
--
-- $Log:	ros.py,v $
-- Revision 7.2  91/02/22  09:41:25  mrose
-- Interim 6.8
-- 
-- Revision 7.1  90/11/04  19:18:02  mrose
-- update
-- 
-- Revision 7.0  89/11/23  22:21:24  mrose
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


--* Remote-Operations-APDUs *-- ROS
--*   { joint-iso-ccitt remote-operations(4) apdus(1) } *--
DEFINITIONS ::=

%{
#ifndef	lint
static char *rcsid = "$Header: /f/osi/rosap/RCS/ros.py,v 7.2 91/02/22 09:41:25 mrose Interim $";
#endif

#include <stdio.h>
#include "ropkt.h"


int	rosap_operation;
int	rosap_error;
int	rosap_type;
int	rosap_id;
int	rosap_null;
int	rosap_linked;
int	rosap_lnull;
PE	rosap_data;
int	rosap_reason;

/*  */
%}

BEGIN

-- EXPORTS
--    	rOSE, InvokeIDType;

-- IMPORTS
--	OPERATION, ERROR
--	    FROM Remote-Operation-Notation
--	    { joint-ccitt-iso remote-operations(4) notation(0) }
--	APPLICATION-SERVICE-ELEMENT
--	    FROM RemoteOperations-Notation-extension
--	    { joint-ccitt-iso remote-operations(4) notation-extension(2) };
  
-- rOSE APPLICATION-SERVICE-ELEMENT ::=
--	{ joint-iso-ccitt remote-opreations(4) aseID(3) }

Operation ::=
	INTEGER

Error ::=
	INTEGER


-- APDUs
-- Types and values of operations and errors are defined in an ROSE-user
-- protocol specification using the RO-notation.  Operation values are either
-- of object identifier type or integer type.  If integer types are used they
-- shall be distinct within an abstract syntax.  Error values are either of
-- object identifier type or integer type.  If integer types are used they
-- shall be distinct within an abstract syntax.  There is no object identifier
-- specified for the abstract syntax name for ROSE.  However all ASN.1 data
-- types defnied in this module shall be included in the name abstract syntax
-- defined in the ROSE-user protocol specification.

ROSEapdus ::=
	CHOICE {
	    roiv-apdu[1] 
		IMPLICIT ROIVapdu,

	    rors-apdu[2] 
		IMPLICIT RORSapdu,

	    roer-apdu[3] 
		IMPLICIT ROERapdu,

	    rorj-apdu [4] 
		IMPLICIT RORJapdu
	}


-- APDU types

ROIVapdu ::=
	SEQUENCE {
	    invokeID
		InvokeIDType,

	    linked-ID[0]
		IMPLICIT --* InvokeIDType *-- INTEGER
		OPTIONAL,

	    operation-value
		Operation,

	    argument
		ANY DEFINED BY operation-value
		OPTIONAL
		-- ANY is filled by the single ASN.1 data type following the
		-- key word ARGUMENT in the type definition of a particular
		-- operation.
	}
	
InvokeIDType ::=
	INTEGER			

RORSapdu ::=
	SEQUENCE {
	    invokeID
		InvokeIDType,

	    SEQUENCE {
		operation-value
		    Operation,

		result
		    ANY DEFINED BY operation-value
		    -- ANY is filled by the single ASN.1 data type following
		    -- the key word RESULT in the type definition of a
		    -- particular operation.
	    }
	    OPTIONAL
	}

ROERapdu ::=
	SEQUENCE {
	    invokeID
		InvokeIDType,

	    error-value
		Error,

	    parameter
		ANY DEFINED BY error-value
		-- ANY is filled by the single ASN.1 data type following
		-- the key word PARAMETER in the type definition of a
		-- particular error.
		OPTIONAL
	}
	
RORJapdu ::=
	SEQUENCE {
	    invokeID
		CHOICE {
		        InvokeIDType,

		        NULL
		},

	    problem
		CHOICE {
		    [0]
			IMPLICIT GeneralProblem,

		    [1]
			IMPLICIT InvokeProblem,

		    [2]
			IMPLICIT ReturnResultProblem,

		    [3]
			IMPLICIT ReturnErrorProblem
	    }
	}

GeneralProblem ::=
	INTEGER {			-- ROSE-provider detected
	    unrecognizedAPDU(0),
	    mistypedAPDU(1),
	    badlyStructuredAPDU(2)
	}
	

InvokeProblem ::=
	INTEGER {			-- ROSE-user detected
	    duplicateInvocation(0),
	    unrecognizedOperation(1),
	    mistypedArgument(2),
	    resourceLimitation(3),
	    initiatorReleasing(4),
	    unrecognizedLinkedID(5),
	    linkedResponseUnexpected(6),
	    unexpectedChildOperation(7)
	}

ReturnResultProblem ::=
	INTEGER {			-- ROSE-user detected
	    unrecognizedInvocation(0),
	    resultResponseUnexpected(1),
	    mistypedresult(2)
	}

ReturnErrorProblem ::=
	INTEGER {			-- ROSE-user detected
	    unrecognizedInvocation(0),
	    errorResponseUnexpected(1),
	    unrecognizedError(2),
	    unexpectedError(3),
	    mistypedParameter(4)
	}


-- Note that although ISO 9072-2 uses different names for types, the syntax
-- and semantics are nearly identical to the specifications in ECMA TR/31
-- and CCITT recommendation X.410 which are used here

-- OPDU

OPDU ::=
	CHOICE {
	    [1] Invoke,

	    [2] ReturnResult,

	    [3]	ReturnError,

	    [4] Reject
	}


-- OPDU types

Invoke ::=
	SEQUENCE {
	    invokeID
		INTEGER,

	        Operation,

	    argument
		ANY
		OPTIONAL
	}
	
ReturnResult ::=
	SEQUENCE {
	    invokeID
		InvokeIDType,

	    result
		ANY
		OPTIONAL
	}
	
ReturnError ::=
	SEQUENCE {
	    invokeID
		INTEGER,

	        Error,

	    parameter
		ANY
		OPTIONAL
	}
	
Reject ::=
	RORJapdu

END
