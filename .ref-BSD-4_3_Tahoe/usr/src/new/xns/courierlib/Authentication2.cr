-- $Header: Authentication2.cr,v 2.2 86/06/05 08:37:06 jqj Exp $ --

-- $Log:	Authentication2.cr,v $
-- Revision 2.2  86/06/05  08:37:06  jqj
-- updated it to actual Authentication V 2 instead of subset
-- (compiler has been fixed to support everything)
-- 
-- Revision 2.0  85/11/21  07:24:00  jqj
-- 4.3BSD standard release, still a small subset
-- 
-- initial version was:
-- a subset of Authentication, hopefully big enough for some testing
--

Authentication: PROGRAM 14 VERSION 2 =

BEGIN
    DEPENDS UPON Time(15) VERSION 2;

-- faked dependency: should be DEPENDS UPON Clearinghouse(2) VERSION 2; --

Organization: TYPE = STRING;
Domain: TYPE = STRING;
Object: TYPE = STRING;

ThreePartName: TYPE = RECORD [
    organization: Organization,
    domain: Domain,
    object: Object
    ];

Clearinghouse_Name:  TYPE = ThreePartName;


-- TYPES --

-- Types supporting encoding --

Key: TYPE = ARRAY 4 OF UNSPECIFIED;  -- lsb of each octet is odd parity bit --

Block: TYPE = ARRAY 4 OF UNSPECIFIED;  -- cipher text or plain text block --

HashedPassword: TYPE = CARDINAL;

-- Types describing credentials and verifiers --

CredentialsType: TYPE = {simple(0), strong(1)};

simpleCredentials: CredentialsType = simple;

Credentials: TYPE = RECORD [type: CredentialsType,
			    value: SEQUENCE OF UNSPECIFIED];

CredentialsPackage: TYPE = RECORD [
	credentials: Credentials,
	nonce: LONG CARDINAL,
	recipient: Clearinghouse_Name,
	conversationKey: Key ];

-- instances of the following type must be a multiple of 64 bits, padded --
-- with zeros, before encryption --

StrongCredentials: TYPE = RECORD [
	conversationKey: Key,
	expirationTime: Time.Time,
	initiator: Clearinghouse_Name ];

SimpleCredentials: TYPE = Clearinghouse_Name;

Verifier: TYPE = SEQUENCE 12 OF UNSPECIFIED;

StrongVerifier: TYPE = RECORD [
	timeStamp: Time.Time,
	ticks: LONG CARDINAL ];

SimpleVerifier: TYPE = HashedPassword;


-- ERRORS --

Problem: TYPE = {
    credentialsInvalid(0),
    verifierInvalid(1),
    verifierExpired(2),
    verifierReused(3),
    credentialsExpired(4),
    inappropriateCredentials(5) };
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

-- Strong Authentication --

GetStrongCredentials: PROCEDURE [
		initiator, recipient: Clearinghouse_Name,
		nonce: LONG CARDINAL ]
	RETURNS [ credentialsPackage: SEQUENCE OF UNSPECIFIED ]
	REPORTS [ CallError ] = 1;

CreateStrongKey: PROCEDURE [
		credentials: Credentials, verifier: Verifier,
		name: Clearinghouse_Name, key: Key ]
	REPORTS [ AuthenticationError, CallError ] = 3;

ChangeStrongKey: PROCEDURE [
		credentials: Credentials, verifier: Verifier,
		newKey: Block ]
	REPORTS [ AuthenticationError, CallError ] = 4;

DeleteStrongKey: PROCEDURE [
		credentials: Credentials, verifier: Verifier,
		name: Clearinghouse_Name ]
	REPORTS [ AuthenticationError, CallError ] = 5;


-- Simple Authentication -- 

CheckSimpleCredentials: PROCEDURE [
		credentials: Credentials, verifier: Verifier ]
	RETURNS[ok: BOOLEAN]
	REPORTS[AuthenticationError, CallError] = 2;

CreateSimpleKey: PROCEDURE [
		credentials: Credentials, verifier: Verifier,
		name: Clearinghouse_Name, key: HashedPassword ]
	REPORTS[AuthenticationError, CallError] = 6;

ChangeSimpleKey: PROCEDURE [
		credentials: Credentials, verifier: Verifier,
		newKey: HashedPassword ]
	REPORTS[AuthenticationError, CallError] = 7;

DeleteSimpleKey: PROCEDURE [
		credentials: Credentials, verifier: Verifier,
		name: Clearinghouse_Name ]
	REPORTS[AuthenticationError, CallError] = 8;


END.
