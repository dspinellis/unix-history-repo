-- $Header: Authentication1.cr,v 2.2 86/06/05 08:46:56 jqj Exp $ --

Authentication: PROGRAM 14 VERSION 1 =

-- $Log:	Authentication1.cr,v $
-- Revision 2.2  86/06/05  08:46:56  jqj
-- Added more values for AuthenticationError, since Clearinghouse (which
-- DEPENDS UPON this version) might need to report inappropriateCredentials.
-- 
-- Revision 2.1  85/12/17  07:52:44  jqj
-- cleaned up some comments
-- 
-- Revision 2.0  85/11/21  07:24:26  jqj
-- 4.3BSD standard release
-- 
-- Revision 1.4  85/03/11  16:43:49  jqj
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
-- note that the dependency has been deleted to eliminate circularity --

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

-- remote errors --

Which: TYPE = {notApplicable(0), initiator(1), recipient(2), client(3)};
CallProblem: TYPE = CARDINAL;
CallError: ERROR [problem: CallProblem, whichArg: Which] = 1;

Problem: TYPE = {credentialsInvalid(0), verifierInvalid(1),
	verifierExpired(2), verifierReused(3), credentialsExpired(4),
	inappropriateCredentials(5)
	};
AuthenticationError: ERROR [problem: Problem] = 2;

END.

