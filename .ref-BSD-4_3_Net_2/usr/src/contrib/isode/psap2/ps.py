-- ps.py - presentation service definitions
--	lifted directly from ISO8823
--
--      Two kinds of changes to the ASN.1
--	    - more commentary-tags for POSY
--	    - shortening some names to make loader symbols unique to 24 bytes

-- $Header: /f/osi/psap2/RCS/ps.py,v 7.1 91/02/22 09:37:25 mrose Interim $
--
--
-- $Log:	ps.py,v $
-- Revision 7.1  91/02/22  09:37:25  mrose
-- Interim 6.8
-- 
-- Revision 7.0  89/11/23  22:14:14  mrose
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


--* ISO8823-PRESENTATION *-- PS DEFINITIONS ::=

%{
#ifndef	lint
static char *rcsid = "$Header: /f/osi/psap2/RCS/ps.py,v 7.1 91/02/22 09:37:25 mrose Interim $";
#endif
%}

BEGIN

-- In X.410-1984 mode, the value of the SS-user data parameter of the S-CONNECT
-- request and indication service-service primitives shall be a CP-type value.
--
-- In normal mode, the value of the SS-user data parameter of the S-CONNECT
-- request and indication session-service primtives shall be a CP-type value,
-- followed as a requestor's option by zero or more CPC-type values.

CP-type ::=
        SET {
            mode --* *--[0]
                IMPLICIT Mode-selector,

            x410-mode --* *--[1]
                IMPLICIT SET
		--* { COMPONENTS OF Reliable-Transfer.RTORQapdu } *--
                OPTIONAL
		-- Shall be used for X.410 mode only.  Shall be bitwise
		-- compatible with CCITT Recommendation X.410-1984.
		-- This shall be the User data parameter of the CP PPDU --,

            normal-mode --* *--[2]
                IMPLICIT SEQUENCE {
                    version --* *--[0]
                        IMPLICIT Protocol-version
                        DEFAULT { version-1 },

                    calling --* *--[1]
                        IMPLICIT Calling-presentation-selector
                        OPTIONAL,

                    called --* *--[2]
                        IMPLICIT Called-presentation-selector
                        OPTIONAL,

                    context-list --* *--[4]
                        IMPLICIT --* Presentation-context- *-- Definition-list
                        OPTIONAL,

                    default-context --* *--[6]
                        IMPLICIT --* Default- *-- Context-name
                        OPTIONAL,

                    presentation-fu --* *--[8]
                        IMPLICIT Presentation-requirements
                        OPTIONAL,

                    session-fu --* *--[9]
                        IMPLICIT User-session-requirements
                        OPTIONAL
			-- shall not be present if equal to the Revised session
			-- requirements parameter--,

		    user-data --* *--
                        User-data
                        OPTIONAL
                }
                OPTIONAL
		-- Shall be used for normal mode only.
		-- Shall be the parameters of the CP PPDU.
        }
-- As an initiator's option, the presentation data values contained in a CP
-- PPDU may be encoded more than once, using CPC-type values, to allow the
-- transfer of the same presentation data values using a number of different
-- transfer syntaxes.
    

CPC-type ::=
        User-data
-- Shall be used for normal mode only.
-- Shall not be present if the Presentation-context-definition-list parameter
-- is not present in the CP PPDU.
-- Each instance of this data type shall contain all of the presentation data
-- values which were contained in the User-data parameter of the CP PPDU.
-- This shall be the same set of presentation data values which were contained
-- in the CP-type.


-- The SS-user data parameter value of the S-CONNECT response and confirm
-- session-service primitives shall be a CPA-PPDU value when the Result
-- parameter value is "accept".

CPA-type ::=
        SET {
            mode --* *--[0]
                IMPLICIT Mode-selector,

            x410-mode --* *--[1]
                IMPLICIT SET
		--* { COMPONENTS OF Reliable-Transfer.RTOACapdu } *--
                OPTIONAL
		-- Shall be used for X.410 mode only.  Shall be bitwise
		-- compatible with CCITT Recommendation X.410-1984.
		-- This shall be the User data parameter of the CPA PPDU --,

            normal-mode --* *--[2]
                IMPLICIT SEQUENCE {
                    version --* *--[0]
                        IMPLICIT Protocol-version
                        DEFAULT { version-1 },

                    responding --* *--[3]
                        IMPLICIT Responding-presentation-selector
                        OPTIONAL,

                    context-list --* *--[5]
                        IMPLICIT --* Presentation-context- *-- Definition-result-list
                        OPTIONAL,

                    presentation-fu --* *--[8]
                        IMPLICIT Presentation-requirements
                        OPTIONAL,

                    session-fu --* *--[9]
                        IMPLICIT User-session-requirements
                        OPTIONAL
			-- shall not be present if equal to the Revised session
			-- requirements parameter--,

		    user-data --* *--
                        User-data
                        OPTIONAL
                }
                OPTIONAL
		-- Shall be used for normal mode only.
        }


-- The SS-user data parameter value of the S-CONNECT response and confirm
-- session-service primitives shall be a CPR-PPDU value when the Result
-- parameter value is "reject by SS-provider" or "reject by called SS-user".

CPR-type ::=
        CHOICE {
	    x410-mode --* *--
                SET
		--* { COMPONENTS OF Reliable-Transfer.RTORJapdu } *--
		-- Shall be used for X.410 mode only.  Shall be bitwise
		-- compatible with CCITT Recommendation X.410-1984.
		-- This shall be the User data parameter of the CPR PPDU --,

	    normal-mode --* *--
                SEQUENCE {
                    version --* *--[0]
                        IMPLICIT Protocol-version
                        DEFAULT { version-1 },

                    responding --* *--[3]
                        IMPLICIT Responding-presentation-selector
                        OPTIONAL,

                    context-list --* *--[5]
                        IMPLICIT --* Presentation-context- *-- Definition-result-list
                        OPTIONAL,

                    default-context --* *--[7]
                        IMPLICIT --* Default- *-- Context-result
                        OPTIONAL,

                    reason --* *--[10]
                        IMPLICIT --* Provider-reason *--
		        INTEGER {
		            reason-not-specified(0),
		            temporary-congestion(1),
		            local-limit-exceeded(2),
		            called-presentation-address-unknown(3),
		            protocol-version-not-supported(4),
		            default-context-not-supported(5),
		            user-data-not-readable(6),
		            no-PSAP-available(7)
		        }
                        OPTIONAL,

		    user-data --* *--
                        User-data
                        OPTIONAL
                }
		-- Shall be used for normal mode only.
        }


-- The SS-user data parameter of the S-U-ABORT request and indication sevice
-- primitives shall be an Abort-type value.

Abort-type ::=
        CHOICE {
	    user-abort --* *--
                ARU-PPDU	-- for a P-U-ABORT--,

	    provider-abort --* *--
                ARP-PPDU	-- for a P-P-ABORT--
        }

ARU-PPDU ::=
        CHOICE {
	    x410-mode --* *--
                SET
		--* { COMPONENTS OF Reliable-Transfer.RTABapdu } *--
		-- Shall be used for X.410 mode only.  Shall be bitwise
		-- compatible with CCITT Recommendation X.410-1984.
		-- This shall be the User data parameter of the ARU PPDU --,

            normal-mode --* *--[0]
                IMPLICIT SEQUENCE {
                    context-list --* *--[0]
                        IMPLICIT --* Presentation-context- *-- Identifier-list
                        OPTIONAL,

		    user-data --* *--
                        User-data
                        OPTIONAL
                }
		-- Shall be used for normal mode only.
        }

ARP-PPDU ::=
        SEQUENCE {
            provider-reason[0]
                IMPLICIT Abort-reason
                OPTIONAL,

            event --* *--[1]
                IMPLICIT Event-identifier
                OPTIONAL
        }


-- The SS-user data parameter of the S-TYPED-DATA request and indication sevice
-- primitives shall be an Typed-data-type value.

Typed-data-type ::=
        CHOICE {
            acPPDU[0]
                IMPLICIT AC-PPDU
				-- P-ALTER-CONTEXT request and indication --,

            acaPPDU[1]
                IMPLICIT ACA-PPDU
				-- P-ALTER-CONTEXT response and confirm --,

            ttdPPDU
                User-data	-- P-TYPED-DATA request and indication
        }

AC-PPDU ::=
        SEQUENCE {
            additions --* *--[0]
                IMPLICIT --* Presentation-context- *-- Addition-list
                OPTIONAL,

            deletions --* *--[1]
                IMPLICIT --* Presentation-context- *-- Deletion-list
                OPTIONAL,

	    user-data --* *--
                User-data
                OPTIONAL
        }

ACA-PPDU ::=
        SEQUENCE {
            additions --* *--[0]
                IMPLICIT --* Presentation-context- *-- Addition-list
                OPTIONAL,

            deletions --* *--[1]
                IMPLICIT --* Presentation-context- *-- Deletion-list
                OPTIONAL,

	    user-data --* *--
                User-data
                OPTIONAL
        }


-- The SS-user data parameter of the S-RESYNCHRONIZE request and indication
-- sevice primitives shall be an RS-PPDU value.

RS-PPDU ::=
        SEQUENCE {
            context-list --* *--[0]
                IMPLICIT --* Presentation-context- *-- Identifier-list
                OPTIONAL,

	    user-data --* *--
                User-data
                OPTIONAL
        }

-- The SS-user data parameter of the S-RESYNCHRONIZE response and confirm
-- sevice primitives shall be an RSA-PPDU value.

RSA-PPDU ::=
        SEQUENCE {
            context-list --* *--[0]
                IMPLICIT --* Presentation-context- *-- Identifier-list
                OPTIONAL,

	    user-data --* *--
                User-data
                OPTIONAL
        }


-- The SS-user data parameter of the S-DATA, S-CAPABILITY-DATA,
-- S-EXPEDITED-DATA request and indication session-sevice primitives and
-- S-CAPABILITY-DATA response and confirm session-service primitives
-- shall be of type User-data.

-- The SS-user data parameter values of all other session-service primitives
-- not described above shall be of type User-data.


Abort-reason ::=
        INTEGER {
            reason-not-specified(0),
            unrecognized-ppdu(1),
            unexpected-ppdu(2),
            unexpected-session-service-primitive(3),
            unrecognized-ppdu-parameter(4),
            unexpected-ppdu-parameter(5),
            invalid-ppdu-parameter(6)
        }

Abstract-syntax-name ::=
        OBJECT IDENTIFIER

Called-presentation-selector ::=
        --* Presentation- *-- Selector

Calling-presentation-selector ::=
        --* Presentation- *-- Selector

Context-list ::=
        SEQUENCE OF
            SEQUENCE {
		identifier --* *--
                    --* Presentation-context-identifier *-- INTEGER,

		abstract-syntax --* *--
                    Abstract-syntax-name,


		transfer-syntax-list --* *--
                    SEQUENCE OF
                        Transfer-syntax-name
            }

--* Default- *-- Context-name ::=
        SEQUENCE {
            abstract-syntax --* *--[0]
                IMPLICIT Abstract-syntax-name,

            transfer-syntax --* *--[1]
                IMPLICIT Transfer-syntax-name
        }

--* Default- *-- Context-result ::=
        Result

Event-identifier ::=
        INTEGER {
            cp-PPDU(0),
            cpa-PPDU(1),
            cpr-PPDU(2),
            aru-PPDU(3),
            arp-PPDU(4),
            ac-PPDU(5),
            aca-PPDU(6),
            td-PPDU(7),
            ttd-PPDU(8),
            te-PPDU(9),
            tc-PPDU(10),
            tcc-PPDU(11),
            rs-PPDU(12),
            rsa-PPDU(13),
            s-release-indication(14),
            s-release-confirm(15),
            s-token-give-indication(16),
            s-token-please-indication(17),
            s-control-give-indication(18),
            s-sync-minor-indication(19),
            s-sync-minor-confirm(20),
            s-sync-major-indication(21),
            s-sync-major-confirm(22),
            s-p-exception-report-indication(23),
            s-u-exception-report-indication(24),
            s-activity-start-indication(25),
            s-activity-resume-indication(26),
            s-activity-interrupt-indication(27),
            s-activity-start-confirm(28),
            s-activity-discard-indication(29),
            s-activity-discard-confirm(30),
            s-activity-end-indication(31),
            s-activity-end-confirm(32)
        }

Mode-selector ::=
        SET {
            [0]
                IMPLICIT INTEGER {
                    x410-1984-mode(0),
                    normal-mode(1)
                }
        }

--* Presentation-context- *-- Addition-list ::=
        Context-list

--* Presentation-context- *-- Addition-result-list ::=
        Result-list

--* Presentation-context- *-- Definition-list ::=
        Context-list

--* Presentation-context- *-- Definition-result-list ::=
        Result-list

--* Presentation-context- *-- Deletion-list ::=
        SEQUENCE OF
            --* Presentation-context-identifier *-- INTEGER

--* Presentation-context- *-- Deletion-result-list ::=
        SEQUENCE OF
            INTEGER {
                acceptance(0),
                user-rejection(1)
            }

--* Presentation-context- *-- Identifier ::=
        INTEGER

--* Presentation-context- *-- Identifier-list ::=
        SEQUENCE OF
            SEQUENCE {
		identifier --* *--
                    --* Presentation-context-identifier *-- INTEGER,

		transfer-syntax --* *--
                    Transfer-syntax-name
            }

Presentation-requirements ::=
        BIT STRING {
            context-management(0),
            restoration(1)
        }

--* Presentation- *-- Selector ::=
        OCTET STRING

Protocol-version ::=
        BIT STRING {
            version-1(0)
        }

Provider-reason ::=
        INTEGER {
            reason-not-specified(0),
            temporary-congestion(1),
            local-limit-exceeded(2),
            called-presentation-address-unknown(3),
            protocol-version-not-supported(4),
            default-context-not-supported(5),
            user-data-not-readable(6),
            no-PSAP-available(7)
        }

Responding-presentation-selector ::=
        --* Presentation- *-- Selector

Result ::=
        INTEGER {
            acceptance(0),
            user-rejection(1),
            provider-rejection(2)
        }

Result-list ::=
        SEQUENCE OF
            SEQUENCE {
                result --* *--[0]
                    IMPLICIT --* Result *--
	            INTEGER {
	                acceptance(0),
	                user-rejection(1),
	                provider-rejection(2)
		    },

                transfer-syntax --* *--[1]
                    IMPLICIT Transfer-syntax-name
                    OPTIONAL,

                provider-reason[2]
                    IMPLICIT INTEGER {
                        reason-not-specified(0),
                        abstract-syntax-not-supported(1),
                        proposed-transfer-syntaxes-not-supported(2),
                        local-limit-on-DCS-exceeded(3)
                    }
                    OPTIONAL
            }

Transfer-syntax-name ::=
        OBJECT IDENTIFIER

User-data ::=
        CHOICE {
            simple --* *--[APPLICATION 0]
                IMPLICIT Simply-encoded-data,

            complex --* *--[APPLICATION 1]
                IMPLICIT Fully-encoded-data
        }
-- Clause 8.4 defines when each of the two alternatives shall be used.

Simply-encoded-data ::=
        OCTET STRING
-- See clause 8.4.1.

Fully-encoded-data ::=
        SEQUENCE OF
            PDV-list
-- contains one or more PDV-list values.
-- See clause 8.4.2.

PDV-list ::=
        SEQUENCE {
	    transfer-syntax --* *--
                Transfer-syntax-name
                OPTIONAL,

	    identifier --* *--
                --* Presentation-context-identifier *-- INTEGER,

            presentation-data-values
                CHOICE {
                    single-ASN1-type[0]
                        ANY,

                    octet-aligned[1]
                        IMPLICIT OCTET STRING,

                    arbitrary[2]
                        IMPLICIT BIT STRING
                }
		-- Contains one or more presentation data values from the same
		-- presentation context.
		-- See clause 8.4.2.
        }

User-session-requirements ::=
        BIT STRING {
            half-duplex(0),
            duplex(1),
            expedited-data(2),
            minor-synchronize(3),
            major-synchronize(4),
            resynchronize(5),
            activity-management(6),
            negotiated-release(7),
            capability-data(8),
            exceptions(9),
            typed-data(10)
        }

END
