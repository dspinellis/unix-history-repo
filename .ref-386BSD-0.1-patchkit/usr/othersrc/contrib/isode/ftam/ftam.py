-- ftam.py - FTAM PDU definitions
--	lifted directly from ISO8571-4
--
--      Two kinds of changes to the ASN.1
--	    - more commentary-tags for POSY
--	    - minor patches for POSY

-- $Header: /f/osi/ftam/RCS/ftam.py,v 7.2 91/02/22 09:22:28 mrose Interim $
--
--
-- $Log:	ftam.py,v $
-- Revision 7.2  91/02/22  09:22:28  mrose
-- Interim 6.8
-- 
-- Revision 7.1  90/03/23  10:53:44  mrose
-- update
-- 
-- Revision 7.0  89/11/23  21:53:16  mrose
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


--* ISO8571-FTAM *-- FTAM DEFINITIONS ::=

%{
#ifndef	lint
static char *rcsid = "$Header: /f/osi/ftam/RCS/ftam.py,v 7.2 91/02/22 09:22:28 mrose Interim $";
#endif
%}

BEGIN

PDU ::=
        CHOICE {
	    ftam-regime-pdu --* *--
                FTAM-Regime-PDU,

	    file-pdu --* *--
                File-PDU,

	    bulk-data-pdu --* *--
                Bulk-Data-PDU
        }


-- FTAM regime PDUs

FTAM-Regime-PDU ::=
        CHOICE {
            f-initialize-request[0]
                IMPLICIT F-INITIALIZE-request,

            f-initialize-response[1]
                IMPLICIT F-INITIALIZE-response,

            f-terminate-request[2]
                IMPLICIT F-TERMINATE-request,

            f-terminate-response[3]
                IMPLICIT F-TERMINATE-response,

            f-u-abort-request[4]
                IMPLICIT F-U-ABORT-request,

            f-p-abort-request[5]
                IMPLICIT F-P-ABORT-request
        }

F-INITIALIZE-request ::=
        SEQUENCE {
            protocol-version
                Protocol-Version
                DEFAULT { version-1 },

            implementation-information
                Implementation-Information
                OPTIONAL,

            presentation-context-management[2]
                IMPLICIT BOOLEAN
                DEFAULT FALSE,

            service-class
                Service-Class
                DEFAULT { transfer-class },
		-- only the valid combinations as specified in ISO 8571-3
		-- are allowed

            functional-units
                Functional-Units,

            attribute-groups
                Attribute-Groups
                DEFAULT {},

            shared-ASE-information
                Shared-ASE-Information
                OPTIONAL,

            ftam-quality-of-service
                FTAM-Quality-Of-Service,

            contents-type-list
                Contents-Type-List
                OPTIONAL,

            initiator-identity
                User-Identity
                OPTIONAL,

            account
                Account
                OPTIONAL,

            filestore-password
                Password
                OPTIONAL,

            checkpoint-window[8]
                IMPLICIT INTEGER
                DEFAULT 1
        }

F-INITIALIZE-response ::=
        SEQUENCE {
            state-result
                State-Result
                DEFAULT success,

            action-result
                Action-Result
                DEFAULT success,

            protocol-version
                Protocol-Version
                DEFAULT { version-1 },

            implementation-information
                Implementation-Information
                OPTIONAL,

            presentation-context-management[2]
                IMPLICIT BOOLEAN
                DEFAULT FALSE,

            service-class
                Service-Class
                DEFAULT { transfer-class },
		-- only the valid combinations as specified in ISO 8571-3
		-- are allowed

            functional-units
                Functional-Units,

            attribute-groups
                Attribute-Groups
                DEFAULT {},

            shared-ASE-information
                Shared-ASE-Information
                OPTIONAL,

            ftam-quality-of-service
                FTAM-Quality-Of-Service,

            contents-type-list
                Contents-Type-List
                OPTIONAL,

            diagnostic
                Diagnostic
                OPTIONAL,

            checkpoint-window[8]
                IMPLICIT INTEGER
                DEFAULT 1
        }

Protocol-Version ::=
    [0]
        IMPLICIT BIT STRING {
            version-1(0)
        }

Implementation-Information ::=
    [1]
        IMPLICIT GraphicString
	-- this parameter is provided solely for the convenience of
        -- implementors needing to distinguish between implementations of a
	-- specific version number on different equipment, it shall not be
	-- the subject of conformance test

Service-Class ::=
    [3]
        IMPLICIT BIT STRING {
            unconstrained-class(0),
            management-class(1),
            transfer-class(2),
            transfer-and-management-class(3),
            access-class(4)
        }

Functional-Units ::=
    [4]
        IMPLICIT BIT STRING {
            read(2),
            write(3),
            file-access(4),
            limited-file-management(5),
            enhanced-file-management(6),
            grouping(7),
            fadu-locking(8),
            recovery(9),
            restart-data-transfer(10)
        }
	-- values 2 to 10 are chosen to align with numbering scheme used in
	-- ISO8571-2

Attribute-Groups ::=
    [5]
        IMPLICIT BIT STRING {
            storage(0),
            security(1),
            private(2)
        }

FTAM-Quality-Of-Service ::=
    [6]
        IMPLICIT INTEGER {
            no-recovery(0),
            class-1-recovery(1),
            class-2-recovery(2),
            class-3-recovery(3)
        }

Contents-Type-List ::=
    [7]
        IMPLICIT SEQUENCE OF
--*         CHOICE { *--
--*             document-type-name *--
                    Document-Type-Name --* , *--

--*             abstract-syntax-name
--*                  Abstract-Syntax-Name
--*         } *--

F-TERMINATE-request ::=
        SEQUENCE {
            shared-ASE-information
                Shared-ASE-Information
                OPTIONAL
        }

F-TERMINATE-response ::=
        SEQUENCE {
            shared-ASE-information
                Shared-ASE-Information
                OPTIONAL,

            charging
                Charging
                OPTIONAL
        }

F-U-ABORT-request ::=
        SEQUENCE {
            action-result
                Action-Result
                DEFAULT success,

            diagnostic
                Diagnostic
                OPTIONAL
        }

F-P-ABORT-request ::=
        SEQUENCE {
            action-result
                Action-Result
                DEFAULT success,

            diagnostic
                Diagnostic
                OPTIONAL
        }


-- File selection and file open regime PDUs

File-PDU ::=
        CHOICE {
            f-select-request[6]
                IMPLICIT F-SELECT-request,

            f-select-response[7]
                IMPLICIT F-SELECT-response,

            f-deselect-request[8]
                IMPLICIT F-DESELECT-request,

            f-deselect-response[9]
                IMPLICIT F-DESELECT-response,

            f-create-request[10]
                IMPLICIT F-CREATE-request,

            f-create-response[11]
                IMPLICIT F-CREATE-response,

            f-delete-request[12]
                IMPLICIT F-DELETE-request,

            f-delete-response[13]
                IMPLICIT F-DELETE-response,

            f-read-attrib-request[14]
                IMPLICIT F-READ-ATTRIB-request,

            f-read-attrib-response[15]
                IMPLICIT F-READ-ATTRIB-response,

            f-change-attrib-request[16]
                IMPLICIT F-CHANGE-ATTRIB-request,

            f-change-attrib-response[17]
                IMPLICIT F-CHANGE-ATTRIB-response,

            f-open-request[18]
                IMPLICIT F-OPEN-request,

            f-open-response[19]
                IMPLICIT F-OPEN-response,

            f-close-request[20]
                IMPLICIT F-CLOSE-request,

            f-close-response[21]
                IMPLICIT F-CLOSE-response,

            f-begin-group-request[22]
                IMPLICIT F-BEGIN-GROUP-request,

            f-begin-group-response[23]
                IMPLICIT F-BEGIN-GROUP-response,

            f-end-group-request[24]
                IMPLICIT F-END-GROUP-request,

            f-end-group-response[25]
                IMPLICIT F-END-GROUP-response,

            f-recover-request[26]
                IMPLICIT F-RECOVER-request,

            f-recover-response[27]
                IMPLICIT F-RECOVER-response,

            f-locate-request[28]
                IMPLICIT F-LOCATE-request,

            f-locate-response[29]
                IMPLICIT F-LOCATE-response,

            f-erase-request[30]
                IMPLICIT F-ERASE-request,

            f-erase-response[31]
                IMPLICIT F-ERASE-response
        }

F-SELECT-request ::=
        SEQUENCE {
            attributes
                Select-Attributes,

            requested-access
                Access-Request,

            access-passwords
                Access-Passwords
                OPTIONAL,

            concurrency-control
                Concurrency-Control
                OPTIONAL,

            shared-ASE-information
                Shared-ASE-Information
                OPTIONAL,

            account
                Account
                OPTIONAL
        }

F-SELECT-response ::=
        SEQUENCE {
            state-result
                State-Result
                DEFAULT success,

            action-result
                Action-Result
                DEFAULT success,

            attributes
                Select-Attributes,

            shared-ASE-information
                Shared-ASE-Information
                OPTIONAL,

            diagnostic
                Diagnostic
                OPTIONAL
        }

F-DESELECT-request ::=
        SEQUENCE {
            shared-ASE-information
                Shared-ASE-Information
                OPTIONAL
        }

F-DESELECT-response ::=
        SEQUENCE {
            action-result
                Action-Result
                DEFAULT success,

            charging
                Charging
                OPTIONAL,
		-- present if and only if the account field was present on the
		-- PDU which established the selection regime

            shared-ASE-information
                Shared-ASE-Information
                OPTIONAL,

            diagnostic
                Diagnostic
                OPTIONAL
        }

F-CREATE-request ::=
        SEQUENCE {
            override[0]
                IMPLICIT INTEGER {
                    create-failure(0),
                    select-old-file(1),
                    delete-and-create-with-old-attributes(2),
                    delete-and-create-with-new-attributes(3)
                }
                DEFAULT create-failure,

            initial-attributes
                Create-Attributes,

            create-password
                Password
                OPTIONAL,
		-- present is an additional password to the filestore
		-- password is required to allow the file creation

            requested-access
                Access-Request,

            access-passwords
                Access-Passwords
                OPTIONAL,
		-- present only if required to satisfy access control
		-- requirement on existing file

            concurrency-control
                Concurrency-Control
                OPTIONAL,

            shared-ASE-information
                Shared-ASE-Information
                OPTIONAL,

            account
                Account
                OPTIONAL
        }

F-CREATE-response ::=
        SEQUENCE {
            state-result
                State-Result
                DEFAULT success,

            action-result
                Action-Result
                DEFAULT success,

            initial-attributes
                Create-Attributes,

            shared-ASE-information
                Shared-ASE-Information
                OPTIONAL,

            diagnostic
                Diagnostic
                OPTIONAL
        }

F-DELETE-request ::=
        SEQUENCE {
            shared-ASE-information
                Shared-ASE-Information
                OPTIONAL
        }

F-DELETE-response ::=
        SEQUENCE {
            action-result
                Action-Result
                DEFAULT success,

            shared-ASE-information
                Shared-ASE-Information
                OPTIONAL,

            charging
                Charging
                OPTIONAL,

            diagnostic
                Diagnostic
                OPTIONAL
        }

F-READ-ATTRIB-request ::=
        SEQUENCE {
            attribute-names[0]
                IMPLICIT Attribute-Names
	}

Attribute-Names ::=
	BIT STRING {
	    -- Kernel group
		read-filename(0),
		read-permitted-actions(1),
		read-content-types(2),
	    -- Storage group
		read-storage-account(3),
		read-date-and-time-of-creation(4),
		read-date-and-time-of-last-modification(5),
		read-date-and-time-of-last-read-access(6),
		read-date-and-time-of-last-attribute-modification(7),
		read-identity-of-creator(8),
		read-identity-of-last-modifier(9),
		read-identity-of-last-reader(10),
		read-identity-of-last-attribute-modifier(11),
		read-file-availability(12),
		read-filesize(13),
		read-future-filesize(14),
	    -- Security group
		read-access-control(15),
		read-legal-qualifications(16),
	    -- Private group
		read-private-use(17)
	}

F-READ-ATTRIB-response ::=
        SEQUENCE {
            action-result
                Action-Result
                DEFAULT success,

            attributes
                Read-Attributes
		OPTIONAL,
		-- Password values within access control can not be read by
		-- means of the read attribute action.  Whether other parts of
		-- the access control file attribute can be read by means of
		-- the read attribute is defined locally by the responding
		-- entity, and it shall not be the subject of conformance test

            diagnostic
                Diagnostic
                OPTIONAL
        }

F-CHANGE-ATTRIB-request ::=
        SEQUENCE {
            attributes
                Change-Attributes
        }

F-CHANGE-ATTRIB-response ::=
        SEQUENCE {
            action-result
                Action-Result
                DEFAULT success,

            attributes
                Change-Attributes
		OPTIONAL,
		-- Password values within access control attribute are never
		-- returned.  Other attributes are returned as an
		-- implementation choice

            diagnostic
                Diagnostic
                OPTIONAL
        }

F-OPEN-request ::=
        SEQUENCE {
            processing-mode[0]
                IMPLICIT BIT STRING {
                    f-read(0),
                    f-insert(1),
                    f-replace(2),
                    f-extend(3),
                    f-erase(4)
                }
                DEFAULT { f-read },

            contents-type[1]
                CHOICE {
                    unknown[0]
                        IMPLICIT NULL,

                    proposed[1]
                        Contents-Type-Attribute
                },

            concurrency-control
                Concurrency-Control
                OPTIONAL,

            shared-ASE-information
                Shared-ASE-Information
                OPTIONAL,

            enable-fadu-locking[2]
                IMPLICIT BOOLEAN
                DEFAULT FALSE,

            activity-identifier
                Activity-Identifier
                   OPTIONAL,
		-- only used in the recovery functional unit

            recovery-mode[3]
                IMPLICIT INTEGER {
                    none(0),
                    at-start-of-file(1),
                    at-any-active-checkpoint(2)
                }
                DEFAULT none,

            remove-contexts[4]
                IMPLICIT SET OF
                    Abstract-Syntax-Name
                OPTIONAL,

            define-contexts[5]
                IMPLICIT SET OF
                    Abstract-Syntax-Name
                OPTIONAL
        }

F-OPEN-response ::=
        SEQUENCE {
            state-result
                State-Result
                DEFAULT success,

            action-result
                Action-Result
                DEFAULT success,

            contents-type[1]
                Contents-Type-Attribute,

            concurrency-control
                Concurrency-Control
                OPTIONAL,

            shared-ASE-information
                Shared-ASE-Information
                OPTIONAL,

            diagnostic
                Diagnostic
                OPTIONAL,

            recovery-mode[3]
                IMPLICIT INTEGER {
                    none(0),
                    at-start-of-file(1),
                    at-any-active-checkpoint(2)
                }
                DEFAULT none,

            presentation-action[6]
                IMPLICIT BOOLEAN
                DEFAULT FALSE
		-- this flag is set if the responder is going to follow this
		-- response by a P-ALTER-CONTEXT exchange
        }

F-CLOSE-request ::=
        SEQUENCE {
            action-result
                Action-Result
                DEFAULT success,

            shared-ASE-information
                Shared-ASE-Information
                OPTIONAL,

            diagnostic
                Diagnostic
                OPTIONAL
        }

F-CLOSE-response ::=
        SEQUENCE {
            action-result
                Action-Result
                DEFAULT success,

            shared-ASE-information
                Shared-ASE-Information
                OPTIONAL,

            diagnostic
                Diagnostic
                OPTIONAL
        }

F-BEGIN-GROUP-request ::=
        SEQUENCE {
            threshold[0]
                IMPLICIT INTEGER
        }

F-BEGIN-GROUP-response ::=
        SEQUENCE {}
	 -- No elements defined, shall be empty

F-END-GROUP-request ::=
        SEQUENCE {}
	 -- No elements defined, shall be empty

F-END-GROUP-response ::=
        SEQUENCE {}
	 -- No elements defined, shall be empty

F-RECOVER-request ::=
        SEQUENCE {
            activity-identifier
                Activity-Identifier,

            bulk-transfer-number[0]
                IMPLICIT INTEGER,

            requested-access
                Access-Request,

            access-passwords
                Access-Passwords
                OPTIONAL,

            recovery-point[2]
                IMPLICIT INTEGER
                DEFAULT 0,
		-- zero indicates beginning of file
		-- point after last checkpoint indicates end of file

            remove-contexts[3]
                IMPLICIT SET OF
                    Abstract-Syntax-Name
                OPTIONAL,

            define-contexts[4]
                IMPLICIT SET OF
                    Abstract-Syntax-Name
                OPTIONAL
        }

F-RECOVER-response ::=
        SEQUENCE {
            state-result
                State-Result
                DEFAULT success,

            action-result
                Action-Result
                DEFAULT success,

            contents-type[1]
                Contents-Type-Attribute,

            recovery-point[2]
                IMPLICIT INTEGER
                DEFAULT 0,
		-- zero indicates beginning of file
		-- point after last checkpoint indicates end of file

            diagnostic
                Diagnostic
                OPTIONAL,

            presentation-action[6]
                IMPLICIT BOOLEAN
                DEFAULT FALSE
		-- this flag is set if the responder is going to follow this
		-- response by a P-ALTER-CONTEXT exchange
        }

F-LOCATE-request ::=
        SEQUENCE {
            file-access-data-unit-identity
                FADU-Identity,

            fadu-lock
                FADU-Lock
                OPTIONAL
        }

F-LOCATE-response ::=
        SEQUENCE {
            action-result
                Action-Result
                DEFAULT success,

            file-access-data-unit-identity
                FADU-Identity
                OPTIONAL,

            diagnostic
                Diagnostic
                OPTIONAL
        }

F-ERASE-request ::=
        SEQUENCE {
            file-access-data-unit-identity
                FADU-Identity
        }

F-ERASE-response ::=
        SEQUENCE {
            action-result
                Action-Result
                DEFAULT success,

            diagnostic
                Diagnostic
                OPTIONAL
        }


-- Bulk data transfer PDUs
    
Bulk-Data-PDU ::=
        CHOICE {
            f-read-request[32]
                IMPLICIT F-READ-request,

            f-write-request[33]
                IMPLICIT F-WRITE-request,

		-- there is no F-DATA FPDU, the contents of a file are
		-- transferred in a different presentation context and there is
		-- therefore no need to define the types of file contents in
		-- the FTAM PCI abstract syntax.  File contents data are
		-- carried in the values of the data type Data-Element defined
		-- in ISO 8571-2

            f-data-end-request[34]
                IMPLICIT F-DATA-END-request,

            f-transfer-end-request[35]
                IMPLICIT F-TRANSFER-END-request,

            f-transfer-end-response[36]
                IMPLICIT F-TRANSFER-END-response,

            f-cancel-request[37]
                IMPLICIT F-CANCEL-request,

            f-cancel-response[38]
                IMPLICIT F-CANCEL-response,

		-- there is no F-CHECK PDU

            f-restart-end-request[39]
                IMPLICIT F-RESTART-END-request,

            f-restart-end-response[40]
                IMPLICIT F-RESTART-END-response
        }

F-READ-request ::=
        SEQUENCE {
            file-access-data-unit-identity
                FADU-Identity,

            access-context
                Access-Context,

            fadu-lock
                FADU-Lock
                OPTIONAL
        }

F-WRITE-request ::=
        SEQUENCE {
            file-access-data-unit-operation[0]
                IMPLICIT INTEGER {
                    insert(0),
                    replace(1),
                    extend(2)
                },

            file-access-data-unit-identity
                FADU-Identity,

            fadu-lock
                FADU-Lock
                OPTIONAL
        }

F-DATA-END-request ::=
        SEQUENCE {
            action-result
                Action-Result
                DEFAULT success,

            diagnostic
                Diagnostic
                OPTIONAL
        }

F-TRANSFER-END-request ::=
        SEQUENCE {
            shared-ASE-information
                Shared-ASE-Information
                OPTIONAL
        }

F-TRANSFER-END-response ::=
        SEQUENCE {
            action-result
                Action-Result
                DEFAULT success,

            shared-ASE-information
                Shared-ASE-Information
                OPTIONAL,

            diagnostic
                Diagnostic
                OPTIONAL
        }

F-CANCEL-request ::=
        SEQUENCE {
            action-result
                Action-Result
                DEFAULT success,

            shared-ASE-information
                Shared-ASE-Information
                OPTIONAL,

            diagnostic
                Diagnostic
                OPTIONAL
        }

F-CANCEL-response ::=
        SEQUENCE {
            action-result
                Action-Result
                DEFAULT success,

            shared-ASE-information
                Shared-ASE-Information
                OPTIONAL,

            diagnostic
                Diagnostic
                OPTIONAL
        }

F-RESTART-END-request ::=
        SEQUENCE {
            checkpoint-identifier[0]
                IMPLICIT INTEGER
        }

F-RESTART-END-response ::=
        SEQUENCE {
            checkpoint-identifier[0]
                IMPLICIT INTEGER
        }


-- application-wide types

Abstract-Syntax-Name ::=
    [APPLICATION 0]
        IMPLICIT OBJECT IDENTIFIER

Access-Context ::=
    [APPLICATION 1]
        IMPLICIT SEQUENCE {
            access-context[0]
                IMPLICIT INTEGER {
                    hierarchical-all-data-units(0),	-- HA
                    hierarchical-no-data-units(1),	-- HN
                    flat-all-data-units(2),		-- FA
                    flat-one-level-units(3),		-- FL
                    flat-single-data-units(4),		-- FS
                    unstructured-all-data-units(5),	-- UA
                    unstructured-single-data-unit(6)	-- US
                },

            level-number[1]
                IMPLICIT INTEGER
                OPTIONAL
		-- present if and only if flat-one-level-data-units
		-- (access context FL) is selected
        }
	-- as defined in ISO 8571-2

Access-Passwords ::=
    [APPLICATION 2]
        IMPLICIT SEQUENCE {
            read-password[0]
                IMPLICIT Password,

            insert-password[1]
                IMPLICIT Password,

            replace-password[2]
                IMPLICIT Password,

            extend-password[3]
                IMPLICIT Password,

            erase-password[4]
                IMPLICIT Password,

            read-attribute-password[5]
                IMPLICIT Password,

            change-attribute-password[6]
                IMPLICIT Password,

            delete-password[7]
                IMPLICIT Password
        }

Access-Request ::=
    [APPLICATION 3]
        IMPLICIT BIT STRING {
            read(0),
            insert(1),
            replace(2),
            extend(3),
            erase(4),
            read-attribute(5),
            change-attribute(6),
            delete(7)
        }

Account ::=
    [APPLICATION 4]
        IMPLICIT GraphicString

Action-Result ::=
    [APPLICATION 5]
        IMPLICIT INTEGER {
            success(0),
            transient-error(1),
            permanent-error(2)
        }

Activity-Identifier ::=
    [APPLICATION 6]
        IMPLICIT INTEGER

Application-Entity-Title ::=
    [APPLICATION 7]
        --* ACSE-1 *-- ACS.AE-title

Change-Attributes ::=
    [APPLICATION 8]
        IMPLICIT --* SEQUENCE { *-- Read-Attributes
	-- Kernel group
--*         filename[0] *--
--*             IMPLICIT Filename-Attribute *--
--*             OPTIONAL, *--

	-- Storage group
--*         storage-account[3] *--
--*             Account-Attribute *--
--*             OPTIONAL, *--

--*         file-availability[12] *--
--*             File-Availability-Attribute *--
--*             OPTIONAL, *--

--*         future-filesize[14] *--
--*             Filesize-Attribute *--
--*             OPTIONAL, *--

	-- Security group
--*         access-control[15] *--
--*             Access-Control-Change-Attribute *--
--*             OPTIONAL, *--

--*         legal-qualification[16] *--
--*             Legal-Qualification-Attribute *--
--*             OPTIONAL, *--

	-- Private group
--*         private-use[17] *--
--*             Private-Use-Attribute *--
--*             OPTIONAL *--
--*     } *--

Charging ::=
    [APPLICATION 9]
        IMPLICIT SEQUENCE OF
            SEQUENCE %[ charge_element $ charge %] {
                resource-identifier[0]
                    IMPLICIT GraphicString,

                charging-unit[1]
                    IMPLICIT GraphicString,

                charging-value[2]
                    IMPLICIT INTEGER
            }

Concurrency-Control ::=
    [APPLICATION 10]
        IMPLICIT SEQUENCE {
            read[0]
                IMPLICIT Lock,

            insert[1]
                IMPLICIT Lock,

            replace[2]
                IMPLICIT Lock,

            extend[3]
                IMPLICIT Lock,

            erase[4]
                IMPLICIT Lock,

            read-attribute[5]
                IMPLICIT Lock,

            change-attribute[6]
                IMPLICIT Lock,

            delete[7]
                IMPLICIT Lock
        }

Lock ::=
        INTEGER {
            not-required(0),
            shared(1),
            exclusive(2),
            no-access(3)
        }

Constraint-Set-Name ::=
    [APPLICATION 11]
        IMPLICIT OBJECT IDENTIFIER

Create-Attributes ::=
    [APPLICATION 12]
        IMPLICIT --* SEQUENCE { *-- Read-Attributes
	-- Kernel group
--*         filename[0] *--
--*             IMPLICIT Filename-Attribute, *--

--*         permitted-actions[1] *--
--*             IMPLICIT Permitted-Actions-Attribute, *--

--*         contents-type[2] *--
--*             Contents-Type-Attribute, *--

	-- Storage group
--*         storage-account[3] *--
--*             Account-Attribute *--
--*             OPTIONAL, *--

--*         file-availability[12] *--
--*             File-Availability-Attribute *--
--*             OPTIONAL, *--

--*         future-filesize[14] *--
--*             Filesize-Attribute *--
--*             OPTIONAL, *--

	-- Security group
--*         access-control[15] *--
--*             Access-Control-Change-Attribute *--
--*             OPTIONAL, *--

--*         legal-qualification[16] *--
--*             Legal-Qualification-Attribute *--
--*             OPTIONAL, *--

	-- Private group
--*         private-use[17] *--
--*             Private-Use-Attribute *--
--*             OPTIONAL *--
--*     } *--

Diagnostic ::=
    [APPLICATION 13]
        IMPLICIT SEQUENCE OF
            SEQUENCE %[ diag_element $ diagnostic %] {
                diagnostic-type[0]
                    IMPLICIT INTEGER {
                        informative(0),
                        transient(1),
                        permanent(2)
                    },

                error-identifier[1]
                    IMPLICIT INTEGER,
		    -- as defined in ISO 8571-3

                error-observer[2]
                    IMPLICIT Entity-Reference,

                error-source[3]
                    IMPLICIT Entity-Reference,

                suggested-delay[4]
                    IMPLICIT INTEGER
                    OPTIONAL,

                further-details[5]
                    IMPLICIT GraphicString
                    OPTIONAL
            }

Entity-Reference ::=
        INTEGER {
            no-categorization-possible(0),
            initiating-file-service-user(1),
            initiating-file-protocol-machine(2),
            service-supporting-the-file-protocol-machine(3),
            responding-file-protocol-machine(4),
            responding-file-service-user(5)
        }
	-- Note
	--	1. The values 0 and 3 are only valid as values in error-source
	--
	--	2. The vaule 5 corresponds to the virtual filestore

Document-Type-Name ::=
    [APPLICATION 14]
        IMPLICIT OBJECT IDENTIFIER

FADU-Identity ::=
    [APPLICATION 15]
        CHOICE {
            first-last[0]
                IMPLICIT INTEGER {
                    first(0),
                    last(1)
                },

            relative[1]
                IMPLICIT INTEGER {
                    previous(0),
                    current(1),
                    next(2)
                },

            begin-end[2]
                IMPLICIT INTEGER {
                    begin(0),
                    end(1)
                },

            single-name[3]
                IMPLICIT Node-Name,

            name-list[4]
                IMPLICIT SEQUENCE OF %[ name_element %] 
                    Node-Name,

            fadu-number[5]
                IMPLICIT INTEGER
        }

Node-Name ::=
        --* EXTERNAL *-- FADU.Node-Name
	-- the type to be used for Node-Name is defined in ISO8571-FADU

FADU-Lock ::=
    [APPLICATION 16]
        IMPLICIT INTEGER {
            off(0),
            on(1)
        }

Password ::=
    [APPLICATION 17]
        CHOICE {
	    graphic --* *--
                GraphicString,

	    binary --* *--
                OCTET STRING
        }

Read-Attributes ::=
    [APPLICATION 18]
        IMPLICIT SEQUENCE {
	-- Kernel group
            filename[0]
                IMPLICIT Filename-Attribute
                OPTIONAL,

            permitted-actions[1]
                IMPLICIT Permitted-Actions-Attribute
                OPTIONAL,

            contents-type[2]
                Contents-Type-Attribute
                OPTIONAL,

	-- Storage group
            storage-account[3]
                Account-Attribute
                OPTIONAL,

            date-and-time-of-creation[4]
                Date-and-Time-Attribute
                OPTIONAL,

            date-and-time-of-last-modification[5]
                Date-and-Time-Attribute
                OPTIONAL,

            date-and-time-of-last-read-access[6]
                Date-and-Time-Attribute
                OPTIONAL,

            date-and-time-of-last-attribute-modification[7]
                Date-and-Time-Attribute
                OPTIONAL,

            identity-of-creator[8]
                User-Identity-Attribute
                OPTIONAL,

            identity-of-last-modifier[9]
                User-Identity-Attribute
                OPTIONAL,

            identity-of-last-reader[10]
                User-Identity-Attribute
                OPTIONAL,

            identity-of-last-attribute-modifier[11]
                User-Identity-Attribute
                OPTIONAL,

            file-availability[12]
                File-Availability-Attribute
                OPTIONAL,

            filesize[13]
                Filesize-Attribute
                OPTIONAL,

            future-filesize[14]
                Filesize-Attribute
                OPTIONAL,

	-- Security group
            access-control[15]
                Access-Control-Attribute
                OPTIONAL,

            legal-qualification[16]
                Legal-Qualification-Attribute
                OPTIONAL,

	-- Private group
	    private-use[17]
		Private-Use-Attribute
		OPTIONAL
        }

Select-Attributes ::=
    [APPLICATION 19]
        IMPLICIT --* SEQUENCE { *-- Read-Attributes
	-- Kernel group
--*         filename[0] *--
--*             IMPLICIT Filename-Attribute *--
--*     } *--

Shared-ASE-Information ::=
    [APPLICATION 20]
        IMPLICIT EXTERNAL

State-Result ::=
    [APPLICATION 21]
        IMPLICIT INTEGER {
            success(0),
            failure(1)
        }

User-Identity ::=
    [APPLICATION 22]
        IMPLICIT GraphicString


-- file attribute types

Access-Control-Attribute ::=
        CHOICE {
            no-value-available[0]
                IMPLICIT NULL,
		-- indicates partial support of this attribute.
		-- this value shall only appear in response PDUs

            actual-values[1]
                IMPLICIT --* SET OF *-- Access-Control-List
                    --* Access-Control-Element
        }
	-- the semantics of this attribute is described in ISO 8571-2

Access-Control-Change-Attribute ::=
        CHOICE {
            no-value-available[0]
                IMPLICIT NULL,
		-- indicates partial support of this attribute.
		-- this value shall only appear in response PDUs

            actual-values[1]
                IMPLICIT SEQUENCE {
                    insert-values[0]
                        IMPLICIT --* SET OF *-- Access-Control-List
                            --* Access-Control-Element
                        OPTIONAL,
			-- this field is used by the change attribute action to
			-- indicate new values to be inserted in the access
			-- control file attribute

                    delete-values[1]
                        IMPLICIT --* SET OF *-- Access-Control-List
                            --* Access-Control-Element
                        OPTIONAL
			-- this field is used by the change attribute action to
			-- indicate old values to be removed in the access
			-- control file attribute
                }
        }

Access-Control-List ::= --* *--
        SET OF --* *--
	    Access-Control-Element --* *--

Access-Control-Element ::=
        SEQUENCE {
            action-list[0]
                IMPLICIT Access-Request,

            concurrency-access[1]
                IMPLICIT Concurrency-Access
                OPTIONAL,

            identity[2]
                IMPLICIT User-Identity
                OPTIONAL,

            passwords[3]
                IMPLICIT Access-Passwords
                OPTIONAL,

            location[4]
                IMPLICIT Application-Entity-Title
                OPTIONAL
        }

Concurrency-Access ::=
        SEQUENCE {
            read[0]
                IMPLICIT Concurrency-Key,

            insert[1]
                IMPLICIT Concurrency-Key,

            replace[2]
                IMPLICIT Concurrency-Key,

            extend[3]
                IMPLICIT Concurrency-Key,

            erase[4]
                IMPLICIT Concurrency-Key,

            read-attribute[5]
                IMPLICIT Concurrency-Key,

            change-attribute[6]
                IMPLICIT Concurrency-Key,

            delete[7]
                IMPLICIT Concurrency-Key
        }

Concurrency-Key ::=
        BIT STRING {
            not-required(0),
            shared(1),
            exclusive(2),
            no-access(3)
        }

Account-Attribute ::=
        CHOICE {
            no-value-available[0]
                IMPLICIT NULL,
		-- indicates partial support of this attribute.
		-- this value shall only appear in response PDUs

            actual-values
                Account
        }

Contents-Type-Attribute ::=
--*     CHOICE { *--
            --* document-type *--[0]
                IMPLICIT SEQUENCE {
                    document-type-name
                        Document-Type-Name,

                    parameter[0]
                        ANY
                        OPTIONAL
                } --* , *--
		-- the actual types to be used for values of the parameter
		-- field are defined in the named document type

--*         constraint-set-and-abstract-syntax[1] *--
--*             IMPLICIT SEQUENCE { *--
--*                 constraint-set-name *--
--*                     Constraint-Set-Name, *--

--*                 abstract-syntax-name *--
--*                     Abstract-Syntax-Name *--
--*             } *--
--*     } *--

Date-and-Time-Attribute ::=
        CHOICE {
            no-value-available[0]
                IMPLICIT NULL,
		-- indicates partial support of this attribute.
		-- this value shall only appear in response PDUs

            actual-values[1]
                IMPLICIT GeneralizedTime
        }

File-Availability-Attribute ::=
        CHOICE {
            no-value-available[0]
                IMPLICIT NULL,
		-- indicates partial support of this attribute.
		-- this value shall only appear in response PDUs

            actual-values[1]
                IMPLICIT INTEGER {
                    immediate-availability(0),
                    deferred-availability(1)
                }
        }

Filename-Attribute ::=
        SEQUENCE OF
            GraphicString

Filesize-Attribute ::=
        CHOICE {
            no-value-available[0]
                IMPLICIT NULL,
		-- indicates partial support of this attribute.
		-- this value shall only appear in response PDUs

            actual-values[1]
                IMPLICIT INTEGER
        }

Legal-Qualification-Attribute ::=
        CHOICE {
            no-value-available[0]
                IMPLICIT NULL,
		-- indicates partial support of this attribute.
		-- this value shall only appear in response PDUs

            actual-values[1]
                IMPLICIT GraphicString
        }

Permitted-Actions-Attribute ::=
        BIT STRING {
            read(0),
            insert(1),
            replace(2),
            extend(3),
            erase(4),
            read-attribute(5),
            change-attribute(6),
            delete-file(7),
	-- FADU-Identity groups available
            traversal(8),
            reverse-traversal(9),
            random-order(10)
        }

Private-Use-Attribute ::=
        CHOICE {
            no-value-available[0]
                IMPLICIT NULL,
		-- indicates partial support of this attribute.
		-- this value shall only appear in response PDUs

            abstract-syntax-not-supported[1]
                IMPLICIT NULL,
		-- indicates that abstract syntax is not available

            actual-values[2]
                IMPLICIT EXTERNAL
        }

User-Identity-Attribute ::=
        CHOICE {
            no-value-available[0]
                IMPLICIT NULL,
		-- indicates partial support of this attribute.
		-- this value shall only appear in response PDUs

            actual-values
                User-Identity
        }

END
