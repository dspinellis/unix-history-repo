-- $Header: Authentication1.cr,v 2.0 85/11/21 07:24:26 jqj Exp $ --

Authentication: PROGRAM 14 VERSION 1 =

-- $Log:	Authentication1.cr,v $
-- Revision 2.0  85/11/21  07:24:26  jqj
-- 4.3BSD standard release
-- 
-- Revision 1.3  85/03/11  16:43:49  jqj
-- *** empty log message ***
-- 
-- Revision 1.3  85/03/11  16:43:49  jqj
-- Public alpha-test version, released 11 March 1985
-- 
-- Revision 1.2  85/03/01  06:12:50  jqj
-- modifications for use with Unix Courier compiler:  eliminated dependency
--   on Clearinghouse.  Added HashedPassword declaration which had been
--   mysteriously forgotten.
-- 
-- Revision 1.1  85/03/01  06:06:51  jqj
-- Initial revision - from Rochester
-- 

BEGIN

-- faked dependency for Clearinghouse (2) VERSION 2 --

	-- DEPENDS UPON
	-- 	Clearinghouse (2) VERSION 2;

ClearinghouseOrganization: TYPE = STRING;
ClearinghouseDomain: TYPE = STRING;
ClearinghouseObject: TYPE = STRING;

ClearinghouseThreePartName: TYPE = RECORD [
    organization: ClearinghouseOrganization,
    domain: ClearinghouseDomain,
    object: ClearinghouseObject
    ];

ClearinghouseName:  TYPE = ClearinghouseThreePartName;

-- Types --

CredentialsType: TYPE = CARDINAL;

Credentials: TYPE = RECORD[
	type: CredentialsType, 
	value: SEQUENCE OF UNSPECIFIED];

simpleCredentials: CredentialsType = 0;

SimpleCredentials: TYPE = ClearinghouseName;

Verifier: TYPE = SEQUENCE 12 OF UNSPECIFIED;

HashedPassword: TYPE = CARDINAL;

SimpleVerifier: TYPE = HashedPassword;

Problem: TYPE = {credentialsInvalid(0), verifierInvalid(1)};
AuthenticationError: ERROR [problem: Problem] = 2;

END.

