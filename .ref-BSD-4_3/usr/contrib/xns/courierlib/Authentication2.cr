Authentication: PROGRAM 14 VERSION 2 =
-- a subset of Authentication, hopefully big enough for some testing --
-- $Header: Authentication2.cr,v 2.1 85/11/21 07:32:36 jqj Exp $

-- $Log:	Authentication2.cr,v $
-- Revision 2.1  85/11/21  07:32:36  jqj
-- fixed comment leaders
-- 
-- Revision 2.0  85/11/21  07:24:28  jqj
-- 4.3BSD standard release
-- 
-- Revision 1.2  85/11/20  13:08:26  jqj
-- 4.3beta version


BEGIN
    DEPENDS UPON Time(15) VERSION 2;

-- faked dependency --

Organization: TYPE = STRING;
Domain: TYPE = STRING;
Object: TYPE = STRING;

ThreePartName: TYPE = RECORD [
    organization: Organization,
    domain: Domain,
    object: Object
    ];

Clearinghouse_Name:  TYPE = ThreePartName;

-- Types --

CredentialsType: TYPE = {simple(0), strong(1)};

SimpleCredentials: TYPE = Clearinghouse_Name;

Credentials: TYPE = RECORD [type: CredentialsType,
			    value: SimpleCredentials];

HashedPassword: TYPE = CARDINAL;

SimpleVerifier: TYPE = HashedPassword;

Verifier: TYPE = SEQUENCE 12 OF UNSPECIFIED;

-- errors --

Problem: TYPE = {
    credentialsInvalid(0),
    verifierInvalid(1),
    verifierExpiered(2),
    verifierReused(3),
    credentialsExpired(4),
    inappropriateCredeitnals(5) };
AuthenticationError: ERROR[problem: Problem] = 2;

CallProblem: TYPE = {
    tooBusy(0),
    accessRightsInsufficient(1),
    keysUnavailable(2),
    strongKeyDoesNotExist(3),
    simpleKeyDoesNotExist(4),
    strongKeyAlreadyRegistered(5),
    simpleKeyAlreadyRegistered(6),
    domainForNewKeyUnavailable(7),
    domainForNewKeyUnknown(8),
    badKey(9),
    badName(10),
    databaseFull(11),
    other(12) };
Which: TYPE = {notApplicable(0), initiator(1), recipient(2), client(3) };
CallError: ERROR [problem: CallProblem, whichArg: Which] = 1;

-- PROCEDURES --

CheckSimpleCredentials: PROCEDURE [credentials: Credentials, verifier: Verifier]
	RETURNS[ok: BOOLEAN]
	REPORTS[AuthenticationError, CallError] = 2;

END.
