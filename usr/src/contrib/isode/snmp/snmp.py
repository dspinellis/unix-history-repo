-- snmp.py - SNMP definitions
    
-- $Header: /f/osi/snmp/RCS/snmp.py,v 7.7 91/02/22 09:44:07 mrose Interim $
--
-- Contributed by NYSERNet Inc.  This work was partially supported by the
-- U.S. Defense Advanced Research Projects Agency and the Rome Air Development
-- Center of the U.S. Air Force Systems Command under contract number
-- F30602-88-C-0016.
--
--
-- $Log:	snmp.py,v $
-- Revision 7.7  91/02/22  09:44:07  mrose
-- Interim 6.8
-- 
-- Revision 7.6  90/10/17  11:57:18  mrose
-- sync
-- 
-- Revision 7.5  90/08/29  12:23:41  mrose
-- touch-up
-- 
-- Revision 7.4  90/06/23  17:01:21  mrose
-- update
-- 
-- Revision 7.3  90/06/21  21:27:34  mrose
-- snmpt
-- 
-- Revision 7.2  90/05/13  15:55:16  mrose
-- update
-- 
-- Revision 7.1  90/01/11  18:34:28  mrose
-- real-sync
-- 
-- Revision 7.0  89/11/23  22:23:24  mrose
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


--* RFC1098-SNMP *-- SNMP DEFINITIONS ::=

BEGIN

-- these are defined below for brevity
-- IMPORTS
--     ObjectName, ObjectSyntax, NetworkAddress, IpAddress, TimeTicks
--	    From RFC1065-SMI;


-- top-level message
    
Message ::=
        SEQUENCE {
            version			-- version-1 for this RFC
                INTEGER {
                    version-1(0)
                },

            community			-- community name
                OCTET STRING,

            data			-- e.g., PDUs if trivial
                --* ANY *-- PDUs	-- authentication is being used
        }


-- protocol data units

PDUs ::=
        CHOICE {
            get-request
                GetRequest-PDU,

            get-next-request
                GetNextRequest-PDU,

            get-response
                GetResponse-PDU,

            set-request
                SetRequest-PDU,

            trap
                Trap-PDU
        }

GetRequest-PDU ::=
    [0]
        IMPLICIT PDU

GetNextRequest-PDU ::=
    [1]
        IMPLICIT PDU

GetResponse-PDU ::=
    [2]
        IMPLICIT PDU

SetRequest-PDU ::=
    [3]
        IMPLICIT PDU

PDU ::=
        SEQUENCE {
            request-id
                INTEGER,

            error-status		-- sometimes ignored
                INTEGER {
                    noError(0),
                    tooBig(1),
                    noSuchName(2),
                    badValue(3),
                    readOnly(4),
                    genErr(5)
                },

            error-index			-- sometimes ignored
                INTEGER,

            variable-bindings		-- values are sometimes ignored
                VarBindList
        }

Trap-PDU ::=
    [4]
        IMPLICIT SEQUENCE {
            enterprise			-- type of object generating
                OBJECT IDENTIFIER,	-- trap, see sysObjectID

            agent-addr			-- address of object generating trap
                NetworkAddress,

            generic-trap	        -- generic trap type
                INTEGER {
                    coldStart(0),
                    warmStart(1),
                    linkDown(2),
                    linkUp(3),
                    authenticationFailure(4),
                    egpNeighborLoss(5),
                    enterpriseSpecific(6)
                },

            specific-trap		-- specific code, present even
                INTEGER,		-- if generic-trap is not
					-- enterpriseSpecific

            time-stamp			-- time elapsed between the last
                TimeTicks,		-- (re)initalization of the network
					-- entity and the generation of the
					-- trap

            variable-bindings		-- "interesting" information
                VarBindList
        }

VarBind ::=
        SEQUENCE {
            name
                ObjectName,

            value
                ObjectSyntax
        }

VarBindList ::=
        SEQUENCE OF
            VarBind



-- types from RFC1065-SMI

ObjectName ::=
	OBJECT IDENTIFIER

ObjectSyntax ::=
    	ANY

NetworkAddress ::=
    CHOICE {
	internet
	    IpAddress
    }

IpAddress ::=
    [APPLICATION 0]            -- in network-byte order
        IMPLICIT OCTET STRING (SIZE (4))
    
TimeTicks ::=
    [APPLICATION 3]
	IMPLICIT INTEGER

ClnpAddress ::=
	OCTET STRING (SIZE (1..21))


-- trap logging (snmpt)

Audit ::=
	SEQUENCE {
	    source
		DisplayString,

	    dateAndTime
		GeneralizedTime,

	    sizeOfEncodingWhichFollows
		INTEGER
	}

-- SMUX (experimental)

-- IMPORTS
--    DisplayString, ObjectName
--	FROM RFC1155-SMI

--    PDUs
--	FROM RFC1157-SNMP;

DisplayString ::=
    OCTET STRING


-- tags for SMUX-specific PDUs are application-wide to avoid conflict with
-- tags for current (and future) SNMP-generic PDUs

SMUX-PDUs ::=
    CHOICE {
	open			-- SMUX initiator uses
	    OpenPDU,		-- immediately after TCP open

	close			-- either uses immediately before TCP close
	    ClosePDU,

	registerRequest		-- SMUX initiator uses
	    RReqPDU,

	registerResponse	-- SNMP agent uses
	    RRspPDU,

	    PDUs,		-- note that roles are reversed:
				--   SNMP agent does get/get-next/set
				--   SMUX initiator does get-response/trap

	commitOrRollback	-- SNMP agent uses
	    SOutPDU
    }


-- open PDU
-- currently only simple authentication

OpenPDU ::=
    CHOICE {
	simple
	    SimpleOpen
    }

SimpleOpen ::=
    [APPLICATION 0] IMPLICIT
	SEQUENCE {
	    version		-- of SMUX protocol
		INTEGER {
		    version-1(0)
		},

	    identity		-- of SMUX initiator, authoritative
		OBJECT IDENTIFIER,

	    description		-- of SMUX initiator, implementation-specific
		DisplayString,

	    password		-- zero length indicates no authentication
		OCTET STRING
	}


-- close PDU

ClosePDU ::=
    [APPLICATION 1] IMPLICIT
	INTEGER {
	    goingDown(0), 
	    unsupportedVersion(1),
	    packetFormat(2),
	    protocolError(3),
	    internalError(4),
	    authenticationFailure(5)
	}


-- insert PDU

RReqPDU ::=
    [APPLICATION 2] IMPLICIT
	SEQUENCE {
	    subtree
		ObjectName,

	    priority		-- the lower the better, "-1" means default
		INTEGER (-1..2147483647),

	    operation
		INTEGER {
		    delete(0),
		    readOnly(1),
		    readWrite(2)
		}
	}

RRspPDU ::=
    [APPLICATION 3] IMPLICIT
	INTEGER {
	    failure(-1)

	    -- on success the non-negative priority is returned
	}

SOutPDU ::=
    [APPLICATION 4] IMPLICIT
	INTEGER {
	    commit(0),
	    rollback(1)
	}

END
