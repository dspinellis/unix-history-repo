Clearinghouse: PROGRAM 2 VERSION 2 =
BEGIN
-- fake ch for tessting --

NetworkAddress: TYPE = RECORD [
	network: ARRAY 2 OF UNSPECIFIED,
	host: ARRAY 3 OF UNSPECIFIED,
	socket: UNSPECIFIED ];

NetworkAddressList: TYPE = SEQUENCE 40 OF NetworkAddress;

CallProblem: TYPE = {
	accessRightsInsufficient(1),
	tooBusy(2),
	serverDown(3),
	useCourier(4),
	other(5) };

CallError: ERROR [problem: CallProblem] = 1;

RetrieveAddresses: PROCEDURE
	RETURNS [addresses: NetworkAddressList]
	REPORTS [CallError] = 0;

END.
