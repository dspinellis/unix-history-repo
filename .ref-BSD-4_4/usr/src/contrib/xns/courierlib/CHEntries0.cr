-- $Header: CHEntries0.cr,v 2.2 87/03/23 11:15:12 ed Exp $

-- although no official Courier program corresponds to this file, the file
-- contains useful data for Courier programs.  In particular, it contains
-- data garnered from CHpids.mesa and CHentries.mesa, as well as from the
-- document "Clearinghouse Entry Formats".

-- $Log:	CHEntries0.cr,v $
-- Revision 2.2  87/03/23  11:15:12  ed
-- Modified MailBoxesValue to be SEQUENCE instead of ARRAY.
-- 
-- Revision 2.1  85/11/21  07:32:53  jqj
-- fixed comment leaders
-- 
-- Revision 2.0  85/11/21  07:24:30  jqj
-- 4.3BSD standard release
-- 
-- Revision 1.2  85/11/20  13:08:55  jqj
-- 4.3beta  version


CHEntries: PROGRAM 0 VERSION 0 =
BEGIN
	DEPENDS UPON Clearinghouse (2) VERSION 2,
		Time (15) VERSION 2;

-- OBJECTS DEFINED IN CH ENTRY FORMATS --

members: Clearinghouse.Property = 3;		-- Group property => no Item --
addressList: Clearinghouse.Property = 4;	-- Item is AddressListValue --
mailboxes: Clearinghouse.Property = 31;		-- Item is MailboxesValue --
authenticationLevel: Clearinghouse.Property = 8;
					-- Item is AuthenticationLevelValue --
fileService: Clearinghouse.Property = 10000;	-- Item is Description --
printService: Clearinghouse.Property = 10001;	-- Item is Description --
user: Clearinghouse.Property = 10003;		-- Item is Description --
mailService: Clearinghouse.Property = 10004;	-- Item is Description --
clearinghouseService: Clearinghouse.Property = 10021;
						-- Item is Description --
userGroup: Clearinghouse.Property = 10022;	-- Group property => no Item --
userData: Clearinghouse.Property = 20000;	-- Item is UserDataValue --

-- Item type for lots of things --
Description: TYPE = STRING;
-- Item type for addressList --
AddressListValue: TYPE = Clearinghouse.NetworkAddressList;
-- Item type for mailboxes.  This is private to Mailing implementation --
-- Specified as array, but in reality appears to be a sequence --
MailboxesValue: TYPE = RECORD [
	time: Time.Time,
	mailService:  SEQUENCE OF Clearinghouse.Name ];
-- Item type for authenticationLevel --
AuthenticationLevelValue: TYPE = RECORD [
	simpleSupported, strongSupported: BOOLEAN ];
-- Item type for userData --
UserDataValue: TYPE = RECORD [
	lastNameIndex: CARDINAL,
	fileService: Clearinghouse.Name ];

-- OBJECTS DEFINED IN CHPIDS.MESA --

-- generic properties --

authKeys: Clearinghouse.Property = 6;
-- the list of all services.  Presumably, Star uses this property to --
-- determine who on the net exports a services Exec.
services: Clearinghouse.Property = 51;

-- primary properties:  all have associated Item == Description --

internetworkRoutingService: Clearinghouse.Property = 10002;
workstation: Clearinghouse.Property = 10005;
externalCommunicationsService: Clearinghouse.Property = 10006;
rs232CPort: Clearinghouse.Property = 10007;
interactiveTerminalService: Clearinghouse.Property = 10008;
gatewayService: Clearinghouse.Property = 10009;
ibm3270Host: Clearinghouse.Property = 10010;
mailGateway: Clearinghouse.Property = 10011;
siemens9750Host: Clearinghouse.Property = 10012;
adobeService: Clearinghouse.Property = 10013;
librarianService: Clearinghouse.Property = 10014;
ttxGateway: Clearinghouse.Property = 10015;
authenticationService: Clearinghouse.Property = 10016;
remoteBatchService: Clearinghouse.Property = 10017;
network: Clearinghouse.Property = 10018;
networkServers: Clearinghouse.Property = 10019;
communicationsInterfaceUnit: Clearinghouse.Property = 10020;
fetchService: Clearinghouse.Property = 10023;

-- secondary properties --

rs232CData: Clearinghouse.Property = 20001;
					-- Item is RS232CData --
ibm3270HostData: Clearinghouse.Property = 20002;
					-- Item is IBM3270HostData --
siemens9750HostData: Clearinghouse.Property = 20003;
					-- Item is Siemens9750HostData --
canMailTo: Clearinghouse.Property = 20005;	-- use with user groups
mailGatewayRouteData: Clearinghouse.Property = 20006;
foreignMailSystemName: Clearinghouse.Property = 20007;

-- secondary properties for compatibility with old stuff --
userPassword: Clearinghouse.Property = 20101;
rs232CBack: Clearinghouse.Property = 20102;	-- Item is RS232CBack --
ibm3270HostBack: Clearinghouse.Property = 20103;
					-- Item is Clearinghouse.Name --

-- associated properties --

associatedWorkstation: Clearinghouse.Property = 30005;

-- Item types --

-- faked dependency on RS232CEnvironment --
Duplexity: TYPE = CARDINAL;		-- for now
CharLength: TYPE = CARDINAL;		-- for now
FlowControl: TYPE = CARDINAL;		-- for now
LineSpeed: TYPE = CARDINAL;		-- for now
Parity: TYPE = CARDINAL;		-- for now
StopBits: TYPE = CARDINAL;		-- for now

PortClientType: TYPE = {unassigned(0), outOfService(1), its(2), irs(3), 
	gws(4), ibm3270Host(5), ttyEmulation(6), rbs(7), fax(9), 
	mailGateway(10), phototypesetter(11) };
PortDialerType: TYPE = {dialerNone(0), vadic(1), hayes(2), ventel(3), rs366(4)};
PortSyncType: TYPE = {asynchronous(0), synchronous(1), bitSynchronous(2),
	byteSynchronous(3), syncAny(4)};
PortEchoingLocation: TYPE = {echoLocal(0), echoRemote(1)};
-- the Item type for rs232CData --
RS232CData: TYPE = RECORD [
	cIUPort: BOOLEAN,
	owningClientType: PortClientType,
	preemptionAllowed: BOOLEAN,
	lineNumber: CARDINAL,		-- logical line number
	dialerNumber: CARDINAL,		-- logical dialer number; must
					-- be unique within domain
	duplexity: Duplexity,
	dialingHardware: PortDialerType,
	charLength: CharLength,
	echoing: PortEchoingLocation,
	xxxxpaddingxxx: LONG CARDINAL,
	flowControl: FlowControl,
	lineSpeed: LineSpeed,
	parity: Parity,
	stopBits: StopBits,
	portActsAsDCE: BOOLEAN,
	accessControl: Clearinghouse.Name,
	validLineSpeeds: SEQUENCE OF LineSpeed ];

END. -- of CHEntries --

