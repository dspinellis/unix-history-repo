-- $Header: GAP2.cr,v 2.0 85/11/21 07:22:54 jqj Exp $
-- $Log:	GAP2.cr,v $
-- Revision 2.0  85/11/21  07:22:54  jqj
-- 4.3BSD standard release
-- 
-- Revision 1.1  85/11/20  14:09:05  jqj
-- Initial revision
-- 

GAP: PROGRAM 3 VERSION 2 =
BEGIN

-- types --

WaitTime: TYPE = CARDINAL;	-- in seconds --

SessionHandle: TYPE = ARRAY 2 OF UNSPECIFIED;

CharLength: TYPE = {five(0), six(1), seven(2), eight(3)};

Parity: TYPE = {none(0), odd(1), even(2), one(3), zero(4)};

StopBits: TYPE = {oneStopBit(0), twoStopBits(1)};

-- the following is sometimes called a SessionParamObject --
SessionParameterObject: TYPE = CHOICE OF {
	xerox800(0) => RECORD [],		-- spec doesn't say (0) --
	xerox850(1), xerox860(2) => RECORD [pollProc: UNSPECIFIED],
	system6(3), cmcll(4), imb2770(5), ibm2770Host(6),
	ibm6670(7), ibm6670Host(8) => RECORD [
		sendBlocksize, receiveBlocksize: CARDINAL ],
	ibm3270(9), ibm3270Host(10) => RECORD [],
	ttyHost(11), tty(12) => RECORD [
		charLength: CharLength,
		parity: Parity,
		stopBits: StopBits,
		frameTimeout: CARDINAL ],	-- in millisec --
	other(13) => RECORD [],
	unknown(14) => RECORD [] };


LineType: TYPE = {
	bitSynchronous(0), byteSynchronous(1), asynchronous(2),
	autoRecognition(3) };

LineSpeed: TYPE = {
	bps50(0), bps75(1), bps110(2), bps135(3), bps150(4),
	bps300(5), bps600(6), bps1200(7), bps2400(8), bps3600(9),
	bps4800(10), bps7200(11), bps9600(12) };

Duplexity: TYPE = {fullduplex(0), halfduplex(1)};
CommParamObject: TYPE = RECORD [
	duplex: Duplexity,
	lineType: LineType,
	lineSpeed: LineSpeed,
	accessDetail: CHOICE OF {
		directConn(0) => RECORD [],	-- spec doesn't say (0) --
		dialConn(1) => RECORD [		-- spec doesn't say (1) --
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
		authenticateProc: UNSPECIFIED ],
	teletype(2) => RECORD [],
	-- I am very uncertain about (4) and (5) --
	polledBSCController(3), polledSDLCController(5) => RECORD [
		hostControllerName: STRING,
		controllerAddress: ControllerAddress,
		portsOnController: CARDINAL ],
	polledBSCTerminal(4), polledSDLCTerminal(6) => RECORD [
		hostControllerName: STRING,
		terminalAddress: TerminalAddress ]
	};

CallBackType: TYPE = { callOnAutoRecognition(0), callOnActive(1),
		dontCall(2) };

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


-- Remote procedures --

Reset: PROCEDURE = 0;

IAmStillHere: PROCEDURE [ resource: Resource ] = 1;

Create: PROCEDURE [
		sessionParameterHandle: SessionParameterObject,
		transportList: SEQUENCE OF TransportObject,
		createTimeout: WaitTime ]	
	RETURNS [ session: SessionHandle ]
	REPORTS [ badAddressFormat, controllerAlreadyExists,
		controllerDoesNotExist, dialingHardwareProblem,
		illegalTransport, inconsistentParams,
mediumConnectFailed,
noCommunicationHardware, noDialingHardware,
terminalAddressInUse, terminalAddressInvalid,
tooManyGateStreams, transmissionMediumUnavailable ]
	= 2;

Delete: PROCEDURE [ session: SessionHandle ] = 3;
	
Reserve: PROCEDURE [
		transport: TransportObject,
		completionProcedure: UNSPECIFIED,
		callBack: CallBackType ]
	RETURNS [resource: Resource]
	REPORTS [
		bugInGAPCode, gapCommunicationError, gapNotExported,
		illegalTransport, inconsistentParams, noCommunicationHardware,
		tooManyGateStreams, transmissionMediumUnavailable ]
	= 4;
	
AbortReserve: PROCEDURE [ resource: Resource ] = 5;

UseMediumForOISCP: PROCEDURE [ transport: TransportObject ] = 8;
	   
END.
