Clearinghouse: PROGRAM 2 VERSION 2 =

-- fake ch for testing --

BEGIN
	DEPENDS UPON
		BulkData (0) VERSION 1,
		Authentication (14) VERSION 1;

Organization: TYPE = STRING;
Domain: TYPE = STRING;
Object: TYPE = STRING;

NetworkAddress: TYPE = RECORD [
	network: ARRAY 2 OF UNSPECIFIED,
	host: ARRAY 3 OF UNSPECIFIED,
	socket: UNSPECIFIED ];

NetworkAddressList: TYPE = SEQUENCE 40 OF NetworkAddress;

ThreePartName: TYPE = RECORD[
	Organization: Organization,
	domain: Domain,
	object: Object ];

ObjectName: TYPE = ThreePartName;

ObjectNamePattern: TYPE = ThreePartName;

Authenticator: TYPE = RECORD [
	credentials: Authentication.Credentials,
	verifier: Authentication.Verifier ];

-- error stuff --

WhichArgument: TYPE = {first(1), second(2) };
ArgumentProblem: TYPE = CARDINAL;
CallProblem: TYPE = {
	accessRightsInsufficient(1),
	tooBusy(2),
	serverDown(3),
	useCourier(4),
	other(5) };

CallError: ERROR [problem: CallProblem] = 1;
ArgumentError: ERROR[problem: ArgumentProblem, which: WhichArgument] = 2;
AuthenticationError: ERROR[problem:Authentication.Problem] = 6;
WrongServer: ERROR [hint: ObjectName] = 5;

RetrieveAddresses: PROCEDURE
	RETURNS [addresses: NetworkAddressList]
	REPORTS [CallError] = 0;

LookupObject: PROCEDURE [name: ObjectNamePattern, agent: Authenticator]
	RETURNS [distinguishedObject: ObjectName]
	REPORTS	[ArgumentError, AuthenticationError, CallError, WrongServer]
	 = 4;

ListDomainsServed: PROCEDURE [list: BulkData.Sink, agent: Authenticator]
	REPORTS	[ArgumentError, AuthenticationError, CallError ] = 7;
END.
