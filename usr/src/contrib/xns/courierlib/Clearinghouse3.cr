-- $Header: Clearinghouse3.cr,v 1.1 87/01/05 11:17:27 ed Exp $ --

-- $Log:	Clearinghouse3.cr,v $
-- Revision 1.1  87/01/05  11:17:27  ed
-- Initial revision
--
--

-- Version 3 is identical to version 2

Clearinghouse: PROGRAM 2 VERSION 3 =
BEGIN
	DEPENDS UPON
		BulkData(0) VERSION 1,
		Authentication (14) VERSION 3;

-- TYPES AND CONSTANTS DESCRIBING NAMES --

Organization: TYPE = STRING;
Domain: TYPE = STRING;
Object: TYPE = STRING;

maxOrganizationsLength: CARDINAL = 20; -- in bytes --
maxDomainLength: CARDINAL = 20; -- in bytes --
maxObjectLength: CARDINAL = 40; -- in bytes --

-- There can be no wildcard characters in any of the following types. --
OrganizationName: TYPE = Organization;

TwoPartName: TYPE = RECORD [
	organization: Organization,
	domain: Domain];

DomainName: TYPE = TwoPartName;

ThreePartName: TYPE = RECORD [
	organization: Organization,
	domain: Domain,
	object: Object];

ObjectName: TYPE = ThreePartName;

Name: TYPE = ThreePartName;

-- Wildcard characters are permittedin OrganizationNamePatterns. --
OrganizationNamePattern: TYPE = Organization;

-- Wildcard characters are permitted in the domain component of this type,
-- but not in the organization component.
DomainNamePattern: TYPE = TwoPartName;

-- Wildcard characters are permitted in the object component of this type,
-- but not in the organization and domain components.
ObjectNamePattern: TYPE = ThreePartName;

-- TYPES AND CONSTANTS DESCRIBING BULK PARAMETERS --

StreamOfDomain: TYPE = CHOICE OF {
	nextSegment (0) => RECORD [
		segment: SEQUENCE OF Domain,
		restOfStream: StreamOfDomain],
	lastSegment (1) => SEQUENCE OF Domain};

StreamOfDomainName: TYPE = CHOICE OF {
	nextSegment (0) => RECORD [
		segment: SEQUENCE OF DomainName,
		restOfStream: StreamOfDomainName],
	lastSegment (1) => SEQUENCE OF DomainName};

StreamOfObject: TYPE = CHOICE OF {
	nextSegment (0) => RECORD [
		segment: SEQUENCE OF Object,
		restOfStream: StreamOfObject],
	lastSegment (1) => SEQUENCE OF Object};

StreamOfObjectName: TYPE = CHOICE OF {
	nextSegment (0) => RECORD [
		segment: SEQUENCE OF ObjectName,
		restOfStream: StreamOfObjectName],
	lastSegment (1) => SEQUENCE OF ObjectName};

StreamOfOrganization: TYPE = CHOICE OF {
	nextSegment (0) => RECORD [
		segment: SEQUENCE OF Organization,
		restOfStream: StreamOfOrganization],
	lastSegment (1) => SEQUENCE OF Organization};

StreamOfThreePartName: TYPE = CHOICE OF {
	nextSegment (0) => RECORD [
		segment: SEQUENCE OF ThreePartName,
		restOfStream: StreamOfThreePartName],
	lastSegment (1) => SEQUENCE OF ThreePartName};

-- TYPES AND CONSTANTS DESCRIBING PROPERTIES --

Property: TYPE = LONG CARDINAL;

-- A Name can have up to 250 Properties associated with it. --
Properties: TYPE = SEQUENCE 250 OF Property;

all: Property = 0;
nullProperty: Property = 37777777777B;

-- The value associated with an item property. --
Item: TYPE = SEQUENCE 500 OF UNSPECIFIED;

-- TYPES AND CONSTANTS DESCRIBING NETWORK ADDRESSES --

-- Clearinghouse addresses aer stored in this form. --

NetworkAddress: TYPE = RECORD [
	network: ARRAY 2 OF UNSPECIFIED,
	host: ARRAY 3 OF UNSPECIFIED,
	socket: UNSPECIFIED ];

NetworkAddressList: TYPE = SEQUENCE 40 OF NetworkAddress;

-- OTHER TYPES AND CONSTANTS --

-- How the client identifies itself to the service --
Authenticator: TYPE = RECORD [
	credentials: Authentication.Credentials,
	verifier: Authentication.Verifier];

wildcard: STRING = "*"; -- the wildcard character (asterisk) --

-- ERRORS --

WhichArgument: TYPE = {
	first(1), -- concerns the first name or property argument --
	second(2) }; -- concerns the second name or property argument --

ArgumentProblem: TYPE = {
	illegalProperty(10), -- property is not usable by a client --
	illegalOrganizationName(11), -- the organization component of the name
		-- is incorrect, e.g., too long or short, or has wild card
		-- characters when not allowed --
	illegalDomainName(12), -- the domain component of the name
		-- is incorrect, e.g., too long or short, or has wild card
		-- characters when not allowed --
	illegalObjectName(13), -- the object component of the name
		-- is incorrect, e.g., too long or short, or has wild card
		-- characters when not allowed --
	noSuchOrganization(14), -- the name's organization component does not exist --
	noSuchDomain(15), -- the name's domain component does not exist --
	noSuchObject(16) }; -- the name's object component does not exist --
ArgumentError: ERROR [problem: ArgumentProblem, which: WhichArgument] = 2;

AuthenticationError: ERROR [problem: Authentication.Problem] = 6;

CallProblem: TYPE = {
	accessRightsInsufficient(1), -- operation prevented by access controls --
	tooBusy(2), -- server is too busy to service this request --
	serverDown(3), -- a remote Clearinghouse server was down and was needed for this request --
	useCourier(4), -- server insists that Courier be used for this particular request --
	other(5) };
CallError: ERROR [problem: CallProblem] = 1;

PropertyProblem: TYPE = {
	missing(20), -- the object exists, but the property doesn't --
	wrongType(21)}; -- client wanted a Group but it was an Item, or vice versa --
PropertyError: ERROR [problem: PropertyProblem,
	distinguishedObject: ObjectName] = 3;
UpdateProblem: TYPE = {
	noChange(30), -- operation wouldn't change the database --
	outOfDate(31), -- more recent information was in database --
	objectOverflow(32), -- the particular object will have too much data
		-- associated with it --
	databaseOverflow(33)}; -- the server has run out of room --
UpdateError: ERROR [problem: UpdateProblem, found: BOOLEAN,
	which: WhichArgument, distinguishedObject: ObjectName] = 4;

WrongServer: ERROR [hint: ObjectName] = 5;

-- PROCEDURES --

-- DEALING WITH OBJECTS --

CreateObject: PROCEDURE [name: ObjectName, agent: Authenticator]
REPORTS [ArgumentError, AuthenticationError, CallError, UpdateError,
	WrongServer] = 2;

DeleteObject: PROCEDURE [name: ObjectName, agent: Authenticator]
REPORTS [ArgumentError, AuthenticationError, CallError, UpdateError,
	WrongServer] = 3;

LookupObject: PROCEDURE [name: ObjectNamePattern, agent: Authenticator]
RETURNS [distinguishedObject: ObjectName]
REPORTS [ArgumentError, AuthenticationError, CallError, WrongServer] = 4;

ListOrganizations: PROCEDURE [pattern: OrganizationNamePattern,
	list: BulkData.Sink, agent: Authenticator]
REPORTS [ArgumentError, AuthenticationError, CallError, WrongServer] = 5;

ListDomain: PROCEDURE [pattern: DomainNamePattern, list: BulkData.Sink,
	agent: Authenticator]
REPORTS [ArgumentError, AuthenticationError, CallError, WrongServer] = 6;

ListObjects: PROCEDURE [pattern: ObjectNamePattern, property: Property,
	list: BulkData.Sink, agent: Authenticator]
REPORTS [ArgumentError, AuthenticationError, CallError, WrongServer] = 7;

ListAliasesOf: PROCEDURE [pattern: ObjectNamePattern, list: BulkData.Sink,
	agent: Authenticator]
RETURNS [distinguishedObject: ObjectName]
REPORTS [ArgumentError, AuthenticationError, CallError, WrongServer] = 9;

-- PROCEDURES DEALING WITH ALIASES --

CreateAlias: PROCEDURE [alias, sameAs: ObjectName, agent: Authenticator]
RETURNS [distinguishedObject: ObjectName]
REPORTS [ArgumentError, AuthenticationError, CallError, UpdateError,
	WrongServer] = 10;

DeleteAlias: PROCEDURE [alias: ObjectName, agent: Authenticator]
RETURNS [distinguishedObject: ObjectName]
REPORTS [ArgumentError, AuthenticationError, CallError, UpdateError,
	WrongServer] = 11;

ListAliases: PROCEDURE [pattern: ObjectNamePattern, list: BulkData.Sink,
	agent: Authenticator]
REPORTS [ArgumentError, AuthenticationError, CallError, WrongServer] = 8;

-- PROCEDURES DEALING WITH PROPERTIES --

DeleteProperty: PROCEDURE [name: ObjectName, property: Property,
	agent: Authenticator]
RETURNS [distinguishedObject: ObjectName]
REPORTS [ArgumentError, AuthenticationError, CallError, PropertyError,
	UpdateError, WrongServer] = 14;

ListProperties: PROCEDURE [pattern: ObjectNamePattern, agent: Authenticator]
RETURNS [distinguishedObject: ObjectName, properties: Properties]
REPORTS [ArgumentError, AuthenticationError, CallError, WrongServer] = 15;

-- PROCEDURES DEALING WITH THE ITEM PROPERTY --

AddItemProperty: PROCEDURE [name: ObjectName, newProperty: Property,
	value: Item, agent: Authenticator]
RETURNS [distinguishedObject: ObjectName]
REPORTS [ArgumentError, AuthenticationError, CallError, PropertyError,
	UpdateError, WrongServer] = 13;

RetrieveItem: PROCEDURE [pattern: ObjectNamePattern, property: Property,
	agent: Authenticator]
RETURNS [distinguishedObject: ObjectName, value: Item]
REPORTS [ArgumentError, AuthenticationError, CallError, PropertyError,
	WrongServer] = 16;

ChangeItem: PROCEDURE [name: ObjectName, property: Property, newValue: Item,
	agent: Authenticator]
RETURNS [distinguishedObject: ObjectName]
REPORTS [ArgumentError, AuthenticationError, CallError, PropertyError,
	UpdateError, WrongServer] = 17;

-- PROCEDURES DEALING WITH THE GROUP PROPERTY -- 

AddGroupProperty: PROCEDURE [name: ObjectName, newProperty: Property,
	membership: BulkData.Source, agent: Authenticator]
RETURNS [distinguishedObject: ObjectName]
REPORTS [ArgumentError, AuthenticationError, CallError, PropertyError,
	UpdateError, WrongServer] = 12;

RetrieveMembers: PROCEDURE [pattern: ObjectNamePattern, property: Property,
	membership: BulkData.Sink, agent: Authenticator]
RETURNS [distinguishedObject: ObjectName]
REPORTS [ArgumentError, AuthenticationError, CallError, PropertyError,
	WrongServer] = 18;

AddMember: PROCEDURE [name: ObjectName, property: Property,
	newMember: ThreePartName, agent: Authenticator]
RETURNS [distinguishedObject: ObjectName]
REPORTS [ArgumentError, AuthenticationError, CallError, PropertyError,
	UpdateError, WrongServer] = 19;

AddSelf: PROCEDURE [name: ObjectName, property: Property, agent: Authenticator]
RETURNS [distinguishedObject: ObjectName]
REPORTS [ArgumentError, AuthenticationError, CallError, PropertyError,
	UpdateError, WrongServer] = 20;

DeleteMember: PROCEDURE [name: ObjectName, property: Property,
	member: ThreePartName, agent: Authenticator]
RETURNS [distinguishedObject: ObjectName]
REPORTS [ArgumentError, AuthenticationError, CallError, PropertyError,
	UpdateError, WrongServer] = 21;

DeleteSelf: PROCEDURE [name: ObjectName, property: Property, agent: Authenticator]
RETURNS [distinguishedObject: ObjectName]
REPORTS [ArgumentError, AuthenticationError, CallError, PropertyError,
	UpdateError, WrongServer] = 22;

IsMember: PROCEDURE [memberOf: ObjectNamePattern,
	property, secondaryProperty: Property, name: ThreePartName,
	agent: Authenticator]
RETURNS [isMember: BOOLEAN, distinguishedObject: ObjectName]
REPORTS [ArgumentError, AuthenticationError, CallError, PropertyError,
	WrongServer] = 23;

-- PROCEDURES DEALING WITH SERVERS --

RetrieveAddresses: PROCEDURE
RETURNS [address: NetworkAddressList]
REPORTS [CallError] = 0;

ListDomainServed: PROCEDURE [domains: BulkData.Sink, agent: Authenticator]
REPORTS [AuthenticationError, CallError] = 1;

END. -- of Clearinghouse --

