-- $Header: BulkData1.cr,v 2.2 87/01/05 11:09:03 ed Exp $

-- $Log:	BulkData1.cr,v $
-- Revision 2.2  87/01/05  11:09:03  ed
-- Added StreamOfUnspecified for use in higher protocols
--
-- Revision 2.1  85/11/21  07:32:48  jqj
-- fixed comment leaders
-- 
-- Revision 2.0  85/11/21  07:24:29  jqj
-- 4.3BSD standard release
-- 
-- Revision 1.2  85/11/20  13:08:46  jqj
-- 4.3 beta version


BulkData: PROGRAM 0 VERSION 1 =

BEGIN

-- streams (for external use) --

StreamOfUnspecified: TYPE = CHOICE OF {
	nextSegment(0) => RECORD [
		segment: SEQUENCE OF UNSPECIFIED,
		restOfStream: StreamOfUnspecified],
	lastSegment(1) => SEQUENCE OF UNSPECIFIED};

-- types --

Identifier: TYPE = RECORD [
	host: ARRAY 3 OF UNSPECIFIED,  
	hostRelativeIdentifier: ARRAY 2 OF UNSPECIFIED ];

Descriptor: TYPE = CHOICE OF {
	null(0), immediate(1) => RECORD [ ],
	passive(2), active(3) => RECORD [ 
		network: ARRAY 2 OF UNSPECIFIED,
		host: ARRAY 3 OF UNSPECIFIED,
		identifier: Identifier ]
	};

-- sinks (for external use) --

Sink: TYPE = Descriptor;
immediateSink: Sink = immediate[];
nullSink: Sink = null[];

-- sources (for external use) --

Source: TYPE = Descriptor;
immediateSource: Source = immediate[];
nullSource: Source = null[];

-- errors --

InvalidDescriptor: ERROR = 0; -- source or sink is passive, active, or null
NoSuchIdentifier: ERROR = 1; -- identifier is unrecognized
IdentifierBusy: ERROR = 2; -- BD object is being sent, received, or cancelled
WrongHost: ERROR = 3; -- caller's host is unauthorized to transfer the data
WrongDirection: ERROR = 4; -- BDT in the other direction was expected
TransferAborted: ERROR = 5; -- BDT was aborted by sender or receiver


-- procedures

Send: PROCEDURE [identifier: Identifier, sink: Sink, timeout: CARDINAL]
REPORTS [ InvalidDescriptor, NoSuchIdentifier, IdentifierBusy, WrongHost,
	WrongDirection, TransferAborted ] = 0;
	-- transfers bulk data from callee to caller

Receive: PROCEDURE [identifier: Identifier, source: Source, timeout: CARDINAL]
REPORTS [ InvalidDescriptor, NoSuchIdentifier, IdentifierBusy, WrongHost,
	WrongDirection, TransferAborted ] = 1;
	-- transfers bulk data from caller to callee

Cancel: PROCEDURE [ identifier: Identifier, timeout: CARDINAL ]
REPORTS [ NoSuchIdentifier, IdentifierBusy, WrongHost ] = 2;
	-- cancels a bulk data transfer before it begins

END.

