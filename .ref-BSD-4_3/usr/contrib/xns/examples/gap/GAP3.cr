-- $Header: GAP3.cr,v 2.0 85/11/21 07:22:56 jqj Exp $
-- $Log:	GAP3.cr,v $
-- Revision 2.0  85/11/21  07:22:56  jqj
-- 4.3BSD standard release
-- 
-- Revision 1.1  85/11/20  14:09:06  jqj
-- Initial revision
-- 

GAP: PROGRAM 3 VERSION 3 =
BEGIN

	DEPENDS UPON Authentication (14) VERSION 1;

-- types --

WaitTime: TYPE = CARDINAL;	-- in seconds --

SessionHandle: TYPE = ARRAY 2 OF UNSPECIFIED;

CharLength: TYPE = {five(0), six(1), seven(2), eight(3)};

Parity: TYPE = {none(0), odd(1), even(2), one(3), zero(4)};

StopBits: TYPE = {oneStopBit(0), twoStopBits(1)};

FlowControl: TYPE = RECORD [
	type: {flowControlNone(0), xOnXOff(1)},
	xOn: UNSPECIFIED,
	xOFF: UNSPECIFIED ];

BidReply: TYPE = {wack(0), nack(1), defaultBidReply(2)};

ExtendedBoolean: TYPE = {true(0), false(1), defaultExtendedBoolean(2)};

DeviceType: TYPE = {undefined(0), terminal(1), printer(2)};

-- the following is sometimes called a SessionParamObject --
SessionParameterObject: TYPE = CHOICE OF {
	xerox800(0) => RECORD [],
	xerox850(1), xerox860(2) => RECORD [pollProc: UNSPECIFIED],
	system6(3), cmcll(4), imb2770(5), ibm2770Host(6),
	ibm6670(7), ibm6670Host(8) => RECORD [
		sendBlocksize, receiveBlocksize: CARDINAL ],
	ibm3270(9), ibm3270Host(10) => RECORD [],
	oldTtyHost(11), oldTty(12) => RECORD [
		charLength: CharLength,
		parity: Parity,
		stopBits: StopBits,
		frameTimeout: CARDINAL ],	-- in millisec --
	otherSessionType(13) => RECORD [],
	unknown(14) => RECORD [],
	ibm2780(15), ibm2780Host(16), 
	ibm3780(17), ibm3780Host(18) => RECORD [
		sendBlocksize, receiveBlocksize: CARDINAL ],
	siemens9750(19), siemens9750Host(20) => RECORD [],
	ttyHost(21), tty(22) => RECORD [
		charLength: CharLength,
		parity: Parity,
		stopBits: StopBits,
		frameTimeout: CARDINAL,		-- in millisec --
		flowControl: FlowControl ] };

LineType: TYPE = {
	bitSynchronous(0), byteSynchronous(1), asynchronous(2),
	autoRecognition(3) };

LineSpeed: TYPE = {
	bps50(0), bps75(1), bps110(2), bps135(3), bps150(4),
	bps300(5), bps600(6), bps1200(7), bps2400(8), bps3600(9),
	bps4800(10), bps7200(11), bps9600(12),
	bps19200(13), bps28800(14), bps38400(15), bps48000(16),
	bps56000(17), bps57600(18)
	};

Duplexity: TYPE = {fullduplex(0), halfduplex(1)};

CommParamObject: TYPE = RECORD [
	accessDetail: CHOICE OF {
		directConn(0) => RECORD [
			duplex: Duplexity,
			lineType: LineType,
			lineSpeed: LineSpeed ],
		dialConn(1) => RECORD [
			duplex: Duplexity,
			lineType: LineType,
			lineSpeed: LineSpeed,
			dialMode: {manualDial(0), autoDial(1)},
			dialerNumber: CARDINAL,
			retryCount: CARDINAL ] }
	];

ReserveType: TYPE = { preemptNever(0), preemptAlways(1),
	preemptInactive(2) };

Resource: TYPE = ARRAY 2 OF UNSPECIFIED;

LineControl: TYPE = { primary(0), secondary(1) };

ControllerAddress: TYPE = CARDINAL;

TerminalAddress: TYPE = CARDINAL;

TransportObject: TYPE = CHOICE OF {
	rs232c(0) => RECORD [			-- spec doesn't say (0) --
		commParams: CommParamObject,
		preemptOthers, preemptMe: ReserveType,
		phoneNumber: STRING,
		line: CHOICE OF {		-- spec doesn't say (0) --
			alreadyReserved(0) => RECORD [resource: Resource],
			reserveNeeded(1) => RECORD [lineNumber: CARDINAL]
			}
		],
	bsc(1) => RECORD [
		localTerminalID: STRING,
		localSecurityID: STRING,
		lineControl: LineControl,
		authenticateProc: UNSPECIFIED,
		bidReply: BidReply,
		sendLineHoldingEOTs: ExtendedBoolean,
		expectLineHoldingEOTs: ExtendedBoolean ],
	teletype(2) => RECORD [],
	polledBSCController(3), sdlcController(5) => RECORD [
		hostControllerName: STRING,
		controllerAddress: ControllerAddress,
		portsOnController: CARDINAL ],
	polledBSCTerminal(4), sdlcTerminal(6) => RECORD [
		hostControllerName: STRING,
		terminalAddress: TerminalAddress ],
	service(7) => RECORD [
		id: LONG CARDINAL ],
	unused(8) => RECORD [],
	polledBSCPrinter(9), sdlcPrinter(10) => RECORD [
		hostControllerName: STRING,
		printerAddress: TerminalAddress]
	};

-- Constants --

infiniteTime: WaitTime = 177777B;	-- LAST[CARDINAL] --

NopPollProc: UNSPECIFIED = 0B;

unspecifiedTerminalAddr: TerminalAddress = 177777B;

-- Remote Errors --

unimplemented: ERROR = 0;
noCommunicationHardware: ERROR = 1;
illegalTransport: ERROR = 2;
mediumConnectFailed: ERROR = 3;
badAddressFormat: ERROR = 4;
noDialingHardware: ERROR = 5;
dialingHardwareProblem: ERROR = 6;
transmissionMediumUnavailable: ERROR = 7;
inconsistentParams: ERROR = 8;
tooManyGateStreams: ERROR = 9;
bugInGAPCode: ERROR = 10;
gapNotExported: ERROR = 11;
gapCommunicationError: ERROR = 12;
controllerAlreadyExists: ERROR = 13;
controllerDoesNotExist: ERROR = 14;
terminalAddressInUse: ERROR = 15;
terminalAddressInvalid: ERROR = 16;
-- the following are guesses as to the ERROR numbers --
serviceTooBusy: ERROR = 17;
userNotAuthenticated: ERROR = 18;
userNotAuthorized: ERROR = 19;
serviceNotFound: ERROR = 20;


-- Remote procedures --

Reset: PROCEDURE = 0;

Create: PROCEDURE [
		sessionParameterHandle: SessionParameterObject,
		transportList: SEQUENCE OF TransportObject,
		createTimeout: WaitTime,
		credentials: Authentication.Credentials,
		verifier: Authentication.Verifier ]
	RETURNS [ session: SessionHandle ]
	REPORTS [ badAddressFormat,
		controllerAlreadyExists, controllerDoesNotExist,
		dialingHardwareProblem,
		illegalTransport, inconsistentParams,
		mediumConnectFailed,
		noCommunicationHardware, noDialingHardware,
		terminalAddressInUse, terminalAddressInvalid,
		tooManyGateStreams, transmissionMediumUnavailable,
		serviceTooBusy, userNotAuthenticated, userNotAuthorized,
		serviceNotFound	]
	= 2;

Delete: PROCEDURE [ session: SessionHandle ] = 3;
	
END.

