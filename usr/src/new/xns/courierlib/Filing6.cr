-- $Header: Filing6.cr,v 2.6 87/03/23 13:06:48 ed Exp $

-- Note:  this is a TEST version of Filing, and is not guaranteed to
-- match the official Xerox version at all.  It does seem to be adequate
-- for FTP, however.

-- $Log:	Filing6.cr,v $
-- Revision 2.6  87/03/23  13:06:48  ed
-- Minor Typo in SerializedTree.
-- 
-- Revision 2.5  87/03/23  11:49:18  ed
-- Slight mod to SerializedTree to allow current implementation.
-- 
-- Revision 2.4  87/01/12  16:45:24  ed
-- Created Filing version 6 from Filing version 4
--
-- Revision 2.2  86/06/30  11:31:13  jqj
-- convert to Authentication v 2 for compatibility with spec., now that
-- compiler allows it.
-- 
-- Revision 2.1  86/06/02  07:08:22  jqj
-- typos
-- 
-- Revision 2.0  85/11/21  07:22:38  jqj
-- 4.3BSD standard release
-- 
-- Revision 1.1  85/05/27  06:30:46  jqj
-- Initial revision
-- 
-- Revision 1.1  85/05/27  06:30:46  jqj
-- Initial revision
-- 

Filing: PROGRAM 10 VERSION 6 =
BEGIN
	DEPENDS UPON
		BulkData(0) VERSION 1,
		Clearinghouse(2) VERSION 3,
		Authentication(14) VERSION 3,
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

PrimaryCredentials: TYPE = Authentication.Credentials;
-- nullPrimaryCredentials: PrimaryCredentials = Authentication.nullCredentials;

-- Secondary credentials --

SecondaryItemType: TYPE = LONG CARDINAL;
SecondaryType: TYPE = SEQUENCE 10 OF SecondaryItemType;

SecondaryItem: TYPE = RECORD [
	type: SecondaryItemType,
	value: SEQUENCE OF UNSPECIFIED ];

Secondary: TYPE = SEQUENCE 10 OF SecondaryItem;

systemPassword: SecondaryItemType = 1;
SystemPassword : TYPE = STRING;			-- value is private --

userName: SecondaryItemType = 2;
UserName: TYPE = STRING;			-- value is not private --

userPassword: SecondaryItemType = 3;
UserPassword: TYPE = STRING;			-- value is private --

userPassword2: SecondaryItemType = 4;
UserPassword2: TYPE = STRING;			-- value is private --

userServiceName: SecondaryItemType = 5;
UserServiceName: TYPE = STRING;			-- value is not private --

userServicePassword: SecondaryItemType = 6;
UserServicePassword: TYPE = STRING;		-- value is private --

userServicePassword2: SecondaryItemType = 7;
UserServicePassword2: TYPE = STRING;		-- value is private --

accountName: SecondaryItemType = 8;
AccountName: TYPE = STRING;			-- value is not private --

accountPassword: SecondaryItemType = 9;
AccountPassword: TYPE = STRING;			-- value is private --

accountPassword2: SecondaryItemType = 10;
AccountPassword2: TYPE = STRING;		-- value is private --

secondaryString: SecondaryItemType = 1000;
SecondaryString: TYPE = STRING;			-- value is not private --

privateSecondaryString: SecondaryItemType = 1001;
PrivateSecondaryString: TYPE = STRING;		-- value is not private --

EncryptedSecondary: TYPE = SEQUENCE OF Authentication.Block;

Strength: TYPE = { strengthNone(0), simple(1), strong(2) };
SecondaryCredentials: TYPE = CHOICE Strength OF {
	strengthNone => RECORD [],
	simple => Secondary,
	strong => EncryptedSecondary };

Credentials: TYPE = RECORD [
	primary: PrimaryCredentials,
	secondary: SecondaryCredentials ];

Verifier: TYPE = Authentication.Verifier;

Handle: TYPE = ARRAY 2 OF UNSPECIFIED;
nullHandle: Handle = [0,0];

Session: TYPE = RECORD [token: ARRAY 2 OF UNSPECIFIED, verifier: Verifier ];

-- Random Access --

ByteAddress: TYPE = LONG CARDINAL;
ByteCount: TYPE = LONG CARDINAL;
endOfFile: LONG CARDINAL = 3777777777B; -- logical end of file --

ByteRange: TYPE = RECORD [ firstByte: ByteAddress, count: ByteCount ];





-- REMOTE ERRORS --

ArgumentProblem: TYPE = {
	illegal(0),
	disallowed(1),
	unreasonable(2),
	unimplemented(3),
	duplicated(4),
	missing(5) };

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
AuthenticationProblem: TYPE = {
	primaryCredentialsInvalid(0),
	verifierInvalid(1),
	verifierExpired(2),
	verifierReused(3),
	primaryCredentialsExpired(4),
	inappropriatePrimaryCredentials(5),
	secondaryCredentialsRequired(6),
	secondaryCredentialsTypeInvalid(7),
	secondaryCredentialsValueInvalid(8) };
AuthenticationError: ERROR [problem: AuthenticationProblem,
			    type: SecondaryType] = 7;

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
	tokenInvalid(0) };
SessionError: ERROR [problem: SessionProblem ] = 12;

-- problem obtaining space for file contents or attributes --
SpaceProblem: TYPE = {
	allocationExceeded(0),
	attributeAreaFull(1),
	mediumFull(2) };
SpaceError: ERROR [problem: SpaceProblem ] = 13;

-- problem during BDT --
TransferProblem: TYPE = {
	aborted(0),
	checksumIncorrect(1),
	formatIncorrect(2),
	noRendezvous(3),
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
	REPORTS [ AccessError, AuthenticationError, HandleError, SessionError,
		UndefinedError ]
	= 5;

-- Getting and Changing Controls (transient) --

GetControls: PROCEDURE [ file: Handle, types: ControlTypeSequence,
		session: Session ]
	RETURNS [ controls: ControlSequence ]
	REPORTS [ AccessError, AuthenticationError, ControlTypeError,
		HandleError, SessionError, UndefinedError ]
	= 6;

ChangeControls: PROCEDURE [ file: Handle, controls: ControlSequence,
		session: Session ]
	REPORTS [ AccessError, AuthenticationError,
		ControlTypeError, ControlValueError,
		HandleError, SessionError, UndefinedError ]
	= 7;


-- Getting and Changing Attributes (permanent) --

GetAttributes: PROCEDURE [ file: Handle, types: AttributeTypeSequence,
		session: Session ]
	RETURNS [ attributes: AttributeSequence ]
	REPORTS [ AccessError, AttributeTypeError, AuthenticationError,
		HandleError, SessionError, UndefinedError ]
	= 8;

ChangeAttributes: PROCEDURE [ file: Handle, attributes: AttributeSequence,
		session: Session ]
	REPORTS [ AccessError, AttributeTypeError, AttributeValueError,
		AuthenticationError, HandleError, InsertionError,
		SessionError, SpaceError, UndefinedError ]
	= 9;

UnifyAccessLists: PROCEDURE [ directory: Handle, session: Session ]
	REPORTS [ AccessError, AuthenticationError, HandleError, SessionError,
		UndefinedError ] 
	= 20;

-- Copying and Moving Files --

Copy: PROCEDURE [ file, destinationDirectory: Handle ,
		attributes: AttributeSequence, controls: ControlSequence,
		session: Session ]
	RETURNS [ newFile: Handle ]
	REPORTS [ AccessError, AttributeTypeError, AttributeValueError,
		AuthenticationError, ControlTypeError, ControlValueError,
		HandleError, InsertionError, SessionError, SpaceError,
		UndefinedError ] 
	= 10;

Move: PROCEDURE [ file, destinationDirectory: Handle ,
		attributes: AttributeSequence, session: Session ]
	REPORTS [ AccessError, AttributeTypeError, AttributeValueError,
		AuthenticationError, HandleError, InsertionError,
		SessionError, SpaceError, UndefinedError ] 
	= 11;

-- Transfering Bulk Data (File Content) --

Store: PROCEDURE [ directory: Handle, attributes: AttributeSequence,
		controls: ControlSequence, content: BulkData.Source,
		session: Session ]
	RETURNS [ file: Handle ]
	REPORTS [ AccessError, AttributeTypeError, AttributeValueError,
		AuthenticationError, ConnectionError, ControlTypeError,
		ControlValueError, HandleError, InsertionError,	SessionError,
		SpaceError, TransferError, UndefinedError ]
	= 12;

Retrieve: PROCEDURE [ file: Handle, content: BulkData.Sink, session: Session ]
	REPORTS [ AccessError, AuthenticationError, ConnectionError,
		HandleError, SessionError, TransferError,
		UndefinedError ]
	= 13;

Replace: PROCEDURE [ file: Handle,  attributes: AttributeSequence,
		content: BulkData.Source, session: Session ]
	REPORTS [ AccessError, AttributeTypeError, AttributeValueError,
		AuthenticationError, ConnectionError, HandleError,
		SessionError, SpaceError, TransferError, UndefinedError ]
	= 14;

-- Transferring Bulk Data (Serialized Files) --

Serialize: PROCEDURE [ file: Handle, serializedFile: BulkData.Sink,
		session: Session ]
	REPORTS [ AccessError, AuthenticationError, ConnectionError,
		HandleError, SessionError, TransferError, UndefinedError ]
	= 15;

Deserialize: PROCEDURE [ directory: Handle, attributes: AttributeSequence,
		controls: ControlSequence, serializedFile: BulkData.Source,
		session: Session ]
	RETURNS [ file: Handle ]
	REPORTS [ AccessError, AttributeTypeError, AttributeValueError,
		AuthenticationError, ConnectionError, ControlTypeError,
		ControlValueError, HandleError, InsertionError,
		SessionError, SpaceError, TransferError, UndefinedError ]
	= 16;

-- Random Access to File Data --

RetrieveBytes: PROCEDURE [ file: Handle, range: ByteRange,
		sink: BulkData.Sink, session: Session ]
	REPORTS [ AccessError, HandleError, RangeError, SessionError,
		UndefinedError ]
	= 22;

ReplaceBytes: PROCEDURE [ file: Handle, range: ByteRange,
		source: BulkData.Source, session: Session ]
	REPORTS [ AccessError, HandleError, RangeError, SessionError,
		SpaceError, UndefinedError ]
	= 23;

-- Locating and Listing Files in a Directory --

Find: PROCEDURE [ directory: Handle, scope: ScopeSequence,
		controls: ControlSequence, session: Session ]
	RETURNS [ file: Handle ]
	REPORTS [ AccessError, AuthenticationError,
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

subtreeSize: AttributeType = 27;
SubtreeSize: TYPE = LONG CARDINAL;

subtreeSizeLimit: AttributeType = 28;
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

-- Serialized File Format, used in Serialize and Deserialize --

-- SerializedTree should contain the following but compiler won't allow it
-- Use Sequence of Unspecified  to get around it for now.
--
-- SerializedTree: TYPE = RECORD [
--	attributes: AttributeSequence,
--	content: RECORD [ data: BulkData.StreamOfUnspecified ,
--			lastByteSignificant: BOOLEAN ],
--	children: SEQUENCE OF SerializedTree ];

SerializedTree: TYPE = RECORD [
	attributes: AttributeSequence,
	content: RECORD [ data: BulkData.StreamOfUnspecified ,
			lastByteSignificant: BOOLEAN ],
	children: SEQUENCE OF UNSPECIFIED ];

Serializedfile: TYPE = RECORD [ version: LONG CARDINAL, file: SerializedTree ];
currentVersion: LONG CARDINAL = 3;


-- Attribute Series Format, used in List --

StreamOfAttributeSequence: TYPE = CHOICE OF {
	nextSegment(0) => RECORD [
		segment: SEQUENCE OF AttributeSequence,
		restOfStream: StreamOfAttributeSequence ],
	lastSegment(1) => SEQUENCE OF AttributeSequence };

-- Line-oriented ASCII text file format, used in file interchange --

AsciiString: TYPE = RECORD [
		lastByteSignificant: BOOLEAN,
		bytes: SEQUENCE OF UNSPECIFIED ];

-- a liberty until the compiler can accept the alternate syntax --
--	see StreamOfAttributeSequence --

LineType: TYPE = { nextLine(0), lastLine(1) };

StreamOfAsciiText: TYPE = CHOICE LineType OF {
	nextLine => RECORD [
			line: AsciiString,
			restOfText: StreamOfAsciiText ],
	lastLine => AsciiString };




-- FILE TYPES --

tUnspecified: Type = 0;
tDirectory: Type = 1;
tText: Type = 2;
tSerialized: Type = 3;
tEmpty: Type = 4;
tAscii: Type = 6;
tAsciiText: Type = 7;

END. -- of Filing --

