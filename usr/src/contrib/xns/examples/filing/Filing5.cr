-- $Header: Filing5.cr,v 1.2 86/04/14 11:18:25 jqj Exp $

-- Note:  this is a TEST version of Filing, and is not guaranteed to
-- match the official Xerox version at all.  It does seem to be adequate
-- for FTP, however.

-- $Log:	Filing5.cr,v $
-- Revision 1.2  86/04/14  11:18:25  jqj
-- fixed some nits
-- 
-- Revision 1.1  86/03/12  10:33:01  jqj
-- Initial revision
--
-- 

Filing: PROGRAM 10 VERSION 5 =
BEGIN
	DEPENDS UPON
		BulkData(0) VERSION 1,
		Clearinghouse(2) VERSION 2,
		Authentication(14) VERSION 1,	-- should be V 2 --
		Time(15) VERSION 2;




-- TYPES AND CONSTANTS --

-- Attributes (individual attributes defined later) --

AttributeType: TYPE = LONG CARDINAL;
AttributeTypeSequence: TYPE = SEQUENCE OF AttributeType;
allAttributeTypes: AttributeTypeSequence = [37777777777B];
Attribute: TYPE = RECORD [type: AttributeType, value: SEQUENCE OF UNSPECIFIED];
AttributeSequence: TYPE = SEQUENCE OF Attribute;

-- Controls --

ControlType: TYPE = {lockControl(0), timeoutControl(1), accessControl(2)};
ControlTypeSequence: TYPE = SEQUENCE 3 OF ControlType;

Lock: TYPE = {lockNone(0), share(1), exclusive(2)};

Timeout: TYPE = CARDINAL;	-- in seconds --
defaultTimeout: Timeout = 177777B;	-- actual value impl.-dependent --

AccessType: TYPE = {
	readAccess(0), writeAccess(1), ownerAccess(2),	-- all files --
	addAccess(3), removeAccess(4) };		-- directories only --
AccessSequence: TYPE = SEQUENCE 5 OF AccessType;
-- fullAccess: AccessSequence = [177777B]; --

Control: TYPE = CHOICE ControlType OF {
	lockControl => Lock,
	timeoutControl => Timeout,
	accessControl => AccessSequence};
ControlSequence: TYPE = SEQUENCE 3 OF Control;

-- Scopes --

Count: TYPE = CARDINAL;
unlimitedCount: Count = 177777B;

Depth: TYPE = CARDINAL;
allDescendants: Depth = 177777B;

Direction: TYPE = {forward(0), backward(1)};

Interpretation: TYPE = { interpretationNone(0), boolean(1), cardinal(2),
	longCardinal(3), time(4), integer(5), longInteger(6), string(7) };
FilterType: TYPE = {
	-- relations --
	less(0), lessOrEqual(1), equal(2), notEqual(3), greaterOrEqual(4),
	greater(5), 
	-- logical --
	and(6), or(7), not(8),
	-- constants --
	filterNone(9), all(10),
	-- patterns --
	matches(11) };
RestrictedFilter: TYPE = CHOICE FilterType OF {
	less, lessOrEqual, equal, notEqual, greaterOrEqual, greater =>
		RECORD [attribute: Attribute, interpretation: Interpretation],
		-- interpretation ignored if attribute interpreted by
		-- implementor
	-- NOT IMPLEMENTED: and, or, not --
	filterNone, all => RECORD [],
	matches => RECORD [attribute: Attribute] };
Filter: TYPE = CHOICE FilterType OF {
	less, lessOrEqual, equal, notEqual, greaterOrEqual, greater =>
		RECORD [attribute: Attribute, interpretation: Interpretation],
		-- interpretation ignored if attribute interpreted by
		-- implementor
	-- NOT YET IMPLEMENTED: (at least, not generally) and, or, not --
	and, or => SEQUENCE OF RestrictedFilter,
	not => RestrictedFilter,
	filterNone, all => RECORD [],
	matches => RECORD [attribute: Attribute] };
nullFilter: Filter = all[];
	
ScopeType: TYPE = { count(0), direction(1), filter(2), depth(3) };
Scope: TYPE = CHOICE ScopeType OF {
	count => Count,
	depth => Depth,
	direction => Direction,
	filter => Filter };
ScopeSequence: TYPE = SEQUENCE 4 OF Scope;

-- Handles and Authentication --

Credentials: TYPE = Authentication.Credentials;
Verifier: TYPE = Authentication.Verifier;
SimpleVerifier: TYPE = Authentication.SimpleVerifier;

Handle: TYPE = ARRAY 2 OF UNSPECIFIED;
nullHandle: Handle = [0,0];	-- meaning depends on operation --

Session: TYPE = RECORD [token: ARRAY 2 OF UNSPECIFIED, verifier: Verifier ];

-- RANDOM ACCESS --
ByteAddress: TYPE = LONG CARDINAL;
ByteCount: TYPE = LONG CARDINAL;
endOfFile: LONG CARDINAL = 37777777777B; -- logical end of file --
ByteRange: TYPE = RECORD [firstByte: ByteAddress, count: ByteCount];

-- REMOTE ERRORS --

ArgumentProblem: TYPE = {
	illegal(0),
	disallowed(1),
	unreasonable(2),
	unimplemented(3),
	duplicated(4),
	argMissing(5) };

-- problem with an attribute type or value --
AttributeTypeError: ERROR [ problem: ArgumentProblem, type: AttributeType]
	= 0;
AttributeValueError: ERROR [ problem: ArgumentProblem, type: AttributeType]
	= 1;

-- problem with an control type or value --
ControlTypeError: ERROR [ problem: ArgumentProblem, type: ControlType]
	= 2;
ControlValueError: ERROR [ problem: ArgumentProblem, type: ControlType]
	= 3;

-- problem with an scope type or value --
ScopeTypeError: ERROR [ problem: ArgumentProblem, type: ScopeType]
	= 4;
ScopeValueError: ERROR [ problem: ArgumentProblem, type: ScopeType]
	= 5;

-- problem in obtaining access to a file --
AccessProblem: TYPE = {
	accessRightsInsufficient(0),
	accessRightsIndeterminate(1),
	fileChanged(2),
	fileDamaged(3),
	fileInUse(4),
	fileNotFound(5),
	fileOpen(6) };
AccessError: ERROR [problem: AccessProblem] = 6;

-- problem with a credentials or verifier --
AuthenticationError: ERROR [problem: Authentication.Problem] = 7;

-- problem with a BDT --
ConnectionProblem: TYPE = {
		-- communication problems --
	noRoute(0),
	noResponse(1),
	transmissionHardware(2),
	transportTimeout(3),
		-- resource problems --
	tooManyLocalConnections(4),
	tooManyRemoteConnections(5),
		-- remote program implementation problems --
	missingCourier(6),
	missingProgram(7),
	missingProcedure(8),
	protocolMismatch(9),
	parameterInconsistency(10),
	invalidMessage(11),
	returnTimedOut(12),
		-- miscellaneous --
	otherCallProblem(177777B) };
ConnectionError: ERROR [problem: ConnectionProblem] = 8;

-- problem with file handle --
HandleProblem: TYPE = {
	invalid(0),
	nullDisallowed(1),
	directoryRequired(2) };
HandleError: ERROR [problem: HandleProblem] = 9;

-- problem during insertion in directory or changing attributes --
InsertionProblem: TYPE = {
	positionUnavailable(0),
	fileNotUnique(1),
	loopInHierarchy(2) };
InsertionError: ERROR [problem: InsertionProblem] = 10;

-- problem during random access operation --
RangeError: ERROR [problem: ArgumentProblem] = 16;

-- problem during logon or logoff --
ServiceProblem: TYPE = {
	cannotAuthenticate(0),
	serviceFull(1),
	serviceUnavailable(2),
	sessionInUse(3) };
ServiceError: ERROR [problem: ServiceProblem] = 11;

-- problem with a session --
SessionProblem: TYPE = {
	tokenInvalid(0),
	serviceAlreadySet(1) };
SessionError: ERROR [problem: SessionProblem ] = 12;

-- problem obtaining space for file contents or attributes --
SpaceProblem: TYPE = {
	allocationExceeded(0),
	attributeAreadFull(1),
	mediumFull(2) };
SpaceError: ERROR [problem: SpaceProblem ] = 13;

-- problem during BDT --
TransferProblem: TYPE = {
	aborted(0),
	checksumIncorrect(1),
	formatIncorrect(2),
	noRendevous(3),
	wrongDirection(4) };
TransferError: ERROR [problem: TransferProblem ] = 14;

-- some undefined (and implementation-dependent) problem occurred --
UndefinedProblem: TYPE = CARDINAL;
UndefinedError: ERROR [problem: UndefinedProblem ] = 15;




-- REMOTE PROCEDURES --

-- Logging On and Off --

Logon: PROCEDURE [
	service: Clearinghouse.Name, credentials: Credentials,
	verifier: Verifier ] 
	RETURNS [ session: Session ]
	REPORTS [ AuthenticationError, ServiceError, SessionError,
		UndefinedError ]
	= 0;

Logoff: PROCEDURE [ session: Session ]
	REPORTS [ AuthenticationError, ServiceError, SessionError,
		UndefinedError ]
	= 1;

Continue: PROCEDURE [ session: Session ]
	RETURNS [ continuance: CARDINAL ]
	REPORTS [ AuthenticationError, SessionError, UndefinedError ]
	= 19;

-- Opening and Closing Files --

Open: PROCEDURE [ attributes: AttributeSequence, directory: Handle,
		controls: ControlSequence, session: Session ]
	RETURNS [ file: Handle ]
	REPORTS [ AccessError, AttributeTypeError, AttributeValueError,
		AuthenticationError, ControlTypeError, ControlValueError,
		HandleError, SessionError, UndefinedError ]
	= 2;

Close: PROCEDURE [ file: Handle, session: Session ]
	REPORTS [ AuthenticationError, HandleError, SessionError,
		UndefinedError ] 
	= 3;

-- Creating and Deleting Files --

Create: PROCEDURE [ directory: Handle, attributes: AttributeSequence,
		controls: ControlSequence, session: Session ]
	RETURNS [ file: Handle ]
	REPORTS [ AccessError, AttributeTypeError, AttributeValueError,
		AuthenticationError, ControlTypeError, ControlValueError,
		HandleError, InsertionError, SessionError, SpaceError,
		UndefinedError ]
	= 4;

Delete: PROCEDURE [ file: Handle, session: Session ]
	REPORTS [AccessError, AuthenticationError, HandleError, SessionError,
		UndefinedError ]
	= 5;

-- Getting and Changing Controls (transient) --

GetControls: PROCEDURE [ file: Handle, types: ControlTypeSequence,
		session: Session ]
	RETURNS [ controls: ControlSequence ]
	REPORTS [AccessError, AttributeTypeError, AuthenticationError,
		ControlTypeError,
		HandleError, SessionError, UndefinedError ]
	= 6;

ChangeControls: PROCEDURE [ file: Handle, controls: ControlSequence,
		session: Session ]
	REPORTS [AccessError, AttributeTypeError, AuthenticationError,
		ControlTypeError, ControlValueError,
		HandleError, SessionError, UndefinedError ]
	= 7;


-- Getting and Changing Attributes (permanent) --

GetAttributes: PROCEDURE [ file: Handle, types: AttributeTypeSequence,
		session: Session ]
	RETURNS [ attributes: AttributeSequence ]
	REPORTS [AccessError, AttributeTypeError, AuthenticationError,
		HandleError, SessionError, UndefinedError ]
	= 8;

ChangeAttributes: PROCEDURE [file: Handle, attributes: AttributeSequence,
		session: Session ]
	REPORTS [AccessError, AttributeTypeError, AuthenticationError,
		HandleError, SessionError, SpaceError, UndefinedError ]
	= 9;

UnifyAccessLists: PROCEDURE [directory: Handle, session: Session ]
	REPORTS [AccessError, AuthenticationError, HandleError, SessionError,
		UndefinedError ] 
	= 20;

-- Copying and Moving Files --

Copy: PROCEDURE [ file, destinationDirectory: Handle ,
		attributes: AttributeSequence, controls: ControlSequence,
		awaaion: Session ]
	RETURNS [ newFile: Handle ]
	REPORTS [AccessError, AttributeTypeError, AttributeValueError,
		AuthenticationError, ControlTypeError, ControlValueError,
		HandleError, InsertionError, SessionError, SpaceError,
		UndefinedError ] 
	= 10;

Move: PROCEDURE [ file, destinationDirectory: Handle ,
		attributes: AttributeSequence, controls: ControlSequence,
		awaaion: Session ]
	RETURNS [ newFile: Handle ]
	REPORTS [AccessError, AttributeTypeError, AttributeValueError,
		AuthenticationError, HandleError, InsertionError,
		SessionError, SpaceError, UndefinedError ] 
	= 11;

-- Transfering Bulk Data (File Content) --

Store: PROCEDURE [ directory: Handle, attributes: AttributeSequence,
		controls: ControlSequence, content: BulkData.Source,
		session: Session ]
	RETURNS [ file: Handle ]
	REPORTS [AccessError, AttributeTypeError, AttributeValueError,
		AuthenticationError, ConnectionError, ControlTypeError,
		ControlValueError, HandleError, InsertionError,	SessionError,
		SpaceError, TransferError, UndefinedError ]
	= 12;

Retrieve: PROCEDURE [ file: Handle, content: BulkData.Sink, session: Session ]
	REPORTS [AccessError, AuthenticationError, ConnectionError,
		HandleError, SessionError, SpaceError, TransferError,
		UndefinedError ]
	= 13;

Replace: PROCEDURE [ file: Handle,  attributes: AttributeSequence,
		content: BulkData.Source, session: Session ]
	REPORTS [AccessError, AttributeTypeError, AttributeValueError,
		AuthenticationError, ConnectionError, HandleError,
		SessionError, SpaceError, TransferError, UndefinedError ]
	= 14;

-- Transferring Bulk Data (Serialized Files) --

	-- NOT YET IMPLEMENTED --

-- Random Access to File Data --

RetrieveBytes: PROCEDURE [file: Handle, range: ByteRange, sink: BulkData.Sink,
		session: Session ]
	REPORTS [AccessError, HandleError, RangeError, SessionError,
		UndefinedError ]
	= 22;

ReplaceBytes: PROCEDURE [file: Handle, range: ByteRange,
		source: BulkData.Source, session: Session ]
	REPORTS [AccessError, HandleError, RangeError, SessionError,
		SpaceError, UndefinedError ]
	= 23;


-- Locating and Listing Files in a Directory --

Find: PROCEDURE [ directory: Handle, scope: ScopeSequence,
		controls: ControlSequence, session: Session ]
	RETURNS [ file: Handle ]
	REPORTS [ AccessError, AuthenticationError, ConnectionError,
		ControlTypeError, ControlValueError, HandleError,
		ScopeTypeError, ScopeValueError,
		SessionError, UndefinedError ]
	= 17;

List: PROCEDURE [ directory: Handle, types: AttributeTypeSequence,
		scope: ScopeSequence, listing: BulkData.Sink,
		session: Session ]
	REPORTS [ AccessError, AttributeTypeError,
		AuthenticationError, ConnectionError,
		HandleError,
		ScopeTypeError, ScopeValueError,
		SessionError, TransferError, UndefinedError ]
	= 18;





-- INTERPRETED ATTRIBUTE DEFINITIONS --

-- common definitions --

Time: TYPE = Time.Time;		-- seconds --
nullTime: Time = Time.earliestTime;

User: TYPE = Clearinghouse.Name;

-- attributes --

accessList: AttributeType = 19;
AccessEntry: TYPE = RECORD [key: Clearinghouse.Name, access: AccessSequence];
AccessList: TYPE = RECORD [entries: SEQUENCE OF AccessEntry, defaulted: BOOLEAN];

checksum: AttributeType = 0;
Checksum: TYPE = CARDINAL;
unknownChecksum: Checksum = 177777B;

childrenUniquelyNamed: AttributeType = 1;
ChildrenUniquelyNamed: TYPE = BOOLEAN;

createdBy: AttributeType = 2;
CreatedBy: TYPE = User;

createdOn: AttributeType = 3;
CreatedOn: TYPE = Time;

dataSize: AttributeType = 16;
DataSize: TYPE = LONG CARDINAL;

defaultAccessList: AttributeType = 20;
DefaultAccessList: TYPE = AccessList;

fileID: AttributeType = 4;
FileID: TYPE = ARRAY 5 OF UNSPECIFIED;
nullFileID: FileID = [0,0,0,0,0];

isDirectory: AttributeType = 5;
IsDirectory: TYPE = BOOLEAN;

isTemporary: AttributeType = 6;
IsTemporary: TYPE = BOOLEAN;

modifiedBy: AttributeType = 7;
ModifiedBy: TYPE = User;

modifiedOn: AttributeType = 8;
ModifiedOn: TYPE = Time;

name: AttributeType = 9;	-- name relative to parent --
Name: TYPE = STRING;	-- must not exceed 100 bytes --

numberOfChildren: AttributeType = 10;
NumberOfChildren: TYPE = CARDINAL;

ordering: AttributeType = 11;
Ordering: TYPE = RECORD [key: AttributeType, ascending: BOOLEAN,
		interpretation: Interpretation];
-- see below for defaultOrdering, byAscendingPosition, byDescendingPosition --

parentID: AttributeType = 12;
ParentID: TYPE = FileID;

pathname: AttributeType = 21;
Pathname: TYPE = STRING;

position: AttributeType = 11;
Position: TYPE = SEQUENCE 100 OF UNSPECIFIED;
firstPosition: Position = [0];
lastPosition: Position = [177777B];

readBy: AttributeType = 14;
ReadBy: TYPE = User;

readOn: AttributeType = 15;
ReadOn: TYPE = Time;

storedSize: AttributeType = 26;
StoredSize: TYPE = LONG CARDINAL;

subtreeSize: AttributeType = 23;
SubtreeSize: TYPE = LONG CARDINAL;

subtreeSizeLimit: AttributeType = 24;
SubtreeSizeLimit: TYPE = LONG CARDINAL;
nullSubtreeSizeLimit: SubtreeSizeLimit = 37777777777B;

type: AttributeType = 17;
Type: TYPE = LONG CARDINAL;

version: AttributeType = 18;
Version: TYPE = CARDINAL;
lowestVersion: Version = 0;
highestVersion: Version = 177777B;

defaultOrdering: Ordering = [key: name, ascending: TRUE, interpretation:
		string];
byAscendingPosition: Ordering = [key: position, ascending: TRUE,
		interpretation: interpretationNone];
byDescendingPosition: Ordering = [key: position, ascending: FALSE,
		interpretation: interpretationNone];




-- BULK DATA FORMATS --

	-- NOT YET IMPLEMENTED --


-- Attribute Series Format, used in List --

StreamOfAttributeSequence: TYPE = CHOICE OF {
	nextSegment(0) => RECORD [
		segment: SEQUENCE OF AttributeSequence,
		restOfStream: StreamOfAttributeSequence],
	lastSegment(1) => SEQUENCE OF AttributeSequence};





-- FILE TYPES --

tUnspecified: Type = 0;
tDirectory: Type = 1;
tText: Type = 2;
tSerialized: Type = 3;
tEmpty: Type = 4;
tAscii: Type = 6;

END. -- of Filing --
