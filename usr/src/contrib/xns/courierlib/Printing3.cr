-- $Header: Printing3.cr,v 2.3 87/05/08 16:10:25 ed Exp $ --
Printing: PROGRAM 4 VERSION 3 =

-- This specification has been modifed from the "official" one to
-- correspond to the restrictions of the Unix Courier compiler.  In
-- particular, it has been reordered to eliminate forward references
-- and some duplicate options have been renamed

-- $Log:	Printing3.cr,v $
-- Revision 2.3  87/05/08  16:10:25  ed
-- Fixed comments on MediumUnavailable and ServiceUnavailable.
-- 
-- Revision 2.2  87/01/27  08:13:52  jqj
-- from examples/print
-- 
-- Revision 2.2  86/12/27  12:09:44  jqj
-- fixed comments
-- 
-- Revision 2.1  86/09/07  07:23:14  jqj
-- Fixed values of MediumUnavailable(4) and ServiceUnavailable(5).  They
-- had been given as 5 and 4 respectively.
-- 
-- Revision 2.0  85/11/21  07:23:08  jqj
-- 4.3BSD standard release
-- 
-- Revision 1.3  85/03/11  16:44:12  jqj
-- *** empty log message ***
-- 
-- Revision 1.3  85/03/11  16:44:12  jqj
-- Public alpha-test version, released 11 March 1985
-- 
-- Revision 1.2  85/03/01  05:53:13  jqj
-- revised to work with current Unix courier compiler:
--   reordered, typos fixed, some enum tags renamed to avoid duplication
-- Revision 1.1
-- Initial revision (from Rochester)

BEGIN

	DEPENDS UPON
		BulkData (0) VERSION 1,
		Authentication (14) VERSION 1,
		Time (15) VERSION 2;



-- Types --

Time: TYPE = Time.Time; -- the standard time and date format --

RequestID: TYPE = ARRAY 5 OF UNSPECIFIED; -- the standard time and date format --

PrintAttributes: TYPE = SEQUENCE 3 OF CHOICE OF {
	printObjectName(0) => STRING, -- default is implementation-dependent --
	printObjectCreateDate(1) => Time, -- default is implementation-dependent --
	senderName(2) => STRING }; -- default is implementation-dependent --

Paper: TYPE = CHOICE OF {
	unknown(0) => RECORD [],
    	knownSize(1) => {
		usLetter(1),	-- defined as 8.5" x 11.0" or 216mm x 297mm --
		usLegal(2),	-- defined as 8.5" x 14.0" or 216mm x 356mm --
		a0(3), a1(4), a2(5), a3(6), a4(7), a5(8), a6(9), a7(10), 
			a8(11), a9(12), a10(35),
		isoB0(13), isoB1(14), isoB2(15), isoB3(16), isoB4(17),
			isoB5(18), isoB6(19), isoB7(20), isoB8(21),
			isoB9(22), isoB10(23),
		jisB0(24), jisB1(25), jisB2(26), jisB3(27), jisB4(28),
			jisB5(29), jisB6(30), jisB7(31), jisB8(32), jisB9(33),
			jisB10(34)},
	otherSize(2) => RECORD [width, length: CARDINAL]}; -- both in millimeters --

Medium: TYPE = CHOICE OF {paper(0) => Paper};

Media: TYPE = SEQUENCE 100 OF Medium;

PrintOptions: TYPE = SEQUENCE 10 OF CHOICE OF {
	printObjectSize(0) => LONG CARDINAL, -- default is size of master --
	recipientName(1) => STRING, -- default is senderName --
	message(2) => STRING, -- default is "" --
	copyCount(3) => CARDINAL, -- default is 1 --
	pagesToPrint(4) => RECORD [
		beginningPageNumber, -- default is 1, the first page of the master --
		endingPageNumber: CARDINAL], -- default is the last page of the master --
	mediumHint(5) => Medium, -- default is implementation-dependent --
	priorityHint(6) => {low(0), normal(1), high(2)}, -- default is implementation-dependent --
	releaseKey(7) => Authentication.HashedPassword, -- default is 177777B --
	staple(8) => BOOLEAN, -- default is FALSE --
	twoSided(9) => BOOLEAN }; -- default is FALSE --

PrinterProperties: TYPE = SEQUENCE 3 OF CHOICE OF {
	ppmedia(0) => Media,
	ppstaple(1) => BOOLEAN, -- default is FALSE --
	pptwoSided(2) => BOOLEAN}; -- default is FALSE --

PrinterStatus: TYPE = SEQUENCE 4 OF CHOICE OF {
	spooler(0) => {available(0), busy(1), disabled(2), full(3)},
	formatter(1) => {available(0), busy(1), disabled(2)},
	printer(2) => {available(0), busy(1), disabled(2), needsAttention(3),
		needsKeyOperator(4) },
	media(3) => Media};


RequestStatus: TYPE = SEQUENCE 2 OF CHOICE OF {
	status(0) => {pending(0), inProgress(1), completed(2),
		completedWithWarning(3), unknown(4), rejected(5), aborted(6),
		canceled(7), held(8) },
	statusMessage(1) => STRING}; -- default is "" --

-- Remote Errors --

UndefinedProblem: TYPE = CARDINAL;

ConnectionProblem: TYPE = {

	-- communications problems --
	noRoute(0), -- no route to the other party could be found. --
	noResponse(1), -- the other party never answered. --
	transmissionHardware(2), -- some local transmission hardware was inoperable. --
	transportTimeout(3), -- the other party responded but later failed to respond. --

	-- resource problems --
	tooManyLocalConnections(4), -- no additional connection is possible. --
	tooManyRemoteConnections(5), -- the other party rejected the connection attempt. --

	-- remote program implementation problems --
	missingCourier(6), -- the other party has no Courier implementation. --
	missingProgram(7), -- the other party does not implement the Bulk Data program. --
	missingProcedure(8), -- the other party does not implement the procedure. --
	protocolMismatch(9), -- the two parties have no Courier version in commmon. --
	parameterInconsistency(10), -- a protocol violation occurred in parameters. --
	invalidMessage(11), -- a protocol vilation occurred in message format. --
	returnTimedOut(12), -- the procedure call never returned. --

	-- miscellaneous --
	otherCallProblem(177777B)}; -- some other protocol violation occurred during a call. --

TransferProblem: TYPE = {
	aborted(0), -- The bulk data transfer was aborted by the party at the other end of the connection. --
	formatIncorrect(2), -- The bulk data received from the souce did not have the expected format. --
	noRendezvous(3), -- The identifier from the other party never appeared. --
	wrongDirection(4)}; -- The other party wanted to transfer the data in the wrong direction. --

Busy: ERROR = 0; -- print service cannot accept a new request at this time --

InsufficientSpoolSpace: ERROR = 1; -- print service does not have enough space to spool a new request --

InvalidPrintParameters: ERROR = 2; -- call to Print specified inconsistent arguments --

MasterTooLarge: ERROR = 3; -- master is too large for the printer service to ever accept --

MediumUnavailable: ERROR = 4; -- the specified medium was not available --

ServiceUnavailable: ERROR = 5; -- the service is not accepting any remote procedure calls --

SpoolingDisabled: ERROR = 6; -- print service is not accepting print requests --

SpoolingQueueFull: ERROR = 7; -- print service does not have enough space to accept a new request --

SystemError: ERROR = 8; -- print service is in an internally inconsistent state --

TooManyClients: ERROR = 9; -- print service does not have enough resources to open a new connection --

Undefined: ERROR [problem: UndefinedProblem] = 10; -- some procedure in Printing is not implemented --

ConnectionError: ERROR [problem: ConnectionProblem] = 11;

TransferError: ERROR [problem: TransferProblem] = 12;

-- Remote Procedures --

Print: PROCEDURE [master: BulkData.Source, printAttributes: PrintAttributes,
	printOptions: PrintOptions]
RETURNS [printRequestID: RequestID]
REPORTS [Busy, ConnectionError, InsufficientSpoolSpace,
	InvalidPrintParameters, MasterTooLarge, MediumUnavailable,
	ServiceUnavailable, SpoolingDisabled, SpoolingQueueFull, SystemError,
	TooManyClients, TransferError, Undefined] = 0;

GetPrinterProperties: PROCEDURE
RETURNS [properties: PrinterProperties]
REPORTS [ServiceUnavailable, SystemError, Undefined] = 1;

GetPrintRequestStatus: PROCEDURE [printRequestID: RequestID]
RETURNS [status: RequestStatus]
REPORTS [ServiceUnavailable, SystemError, Undefined] = 2;

GetPrinterStatus: PROCEDURE
RETURNS [status: PrinterStatus]
REPORTS [ServiceUnavailable, SystemError, Undefined] = 3;

END.
