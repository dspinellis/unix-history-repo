/*
 * Definitions for Clearinghouse VERSION 2 NUMBER 2.
 */
#ifndef __Clearinghouse2
#define __Clearinghouse2
#include <xnscourier/courier.h>
#include <xnscourier/courierconnection.h>


/*
 * Definitions from DEPENDS UPON BulkData inclusion
 * (must be linked with BulkData1_support.c also)
 */
#include <xnscourier/BulkData1.h>

/*
 * Definitions from DEPENDS UPON Authentication inclusion
 * (must be linked with Authentication1_support.c also)
 */
#include <xnscourier/Authentication1.h>
typedef String Clearinghouse2_Organization;
#define sizeof_Clearinghouse2_Organization sizeof_String
#define clear_Clearinghouse2_Organization clear_String
#define externalize_Clearinghouse2_Organization externalize_String
#define internalize_Clearinghouse2_Organization internalize_String

typedef String Clearinghouse2_Domain;
#define sizeof_Clearinghouse2_Domain sizeof_String
#define clear_Clearinghouse2_Domain clear_String
#define externalize_Clearinghouse2_Domain externalize_String
#define internalize_Clearinghouse2_Domain internalize_String

typedef String Clearinghouse2_Object;
#define sizeof_Clearinghouse2_Object sizeof_String
#define clear_Clearinghouse2_Object clear_String
#define externalize_Clearinghouse2_Object externalize_String
#define internalize_Clearinghouse2_Object internalize_String


static Cardinal Clearinghouse2_maxOrganizationsLength = {20};

static Cardinal Clearinghouse2_maxDomainLength = {20};

static Cardinal Clearinghouse2_maxObjectLength = {40};
typedef String Clearinghouse2_OrganizationName;
#define sizeof_Clearinghouse2_OrganizationName sizeof_String
#define clear_Clearinghouse2_OrganizationName clear_String
#define externalize_Clearinghouse2_OrganizationName externalize_String
#define internalize_Clearinghouse2_OrganizationName internalize_String


typedef struct {
	String organization;
	String domain;
} Clearinghouse2_TwoPartName;
typedef Clearinghouse2_TwoPartName Clearinghouse2_DomainName;
#define sizeof_Clearinghouse2_DomainName sizeof_Clearinghouse2_TwoPartName
#define clear_Clearinghouse2_DomainName clear_Clearinghouse2_TwoPartName
#define externalize_Clearinghouse2_DomainName externalize_Clearinghouse2_TwoPartName
#define internalize_Clearinghouse2_DomainName internalize_Clearinghouse2_TwoPartName


typedef struct {
	String organization;
	String domain;
	String object;
} Clearinghouse2_ThreePartName;
typedef Clearinghouse2_ThreePartName Clearinghouse2_ObjectName;
#define sizeof_Clearinghouse2_ObjectName sizeof_Clearinghouse2_ThreePartName
#define clear_Clearinghouse2_ObjectName clear_Clearinghouse2_ThreePartName
#define externalize_Clearinghouse2_ObjectName externalize_Clearinghouse2_ThreePartName
#define internalize_Clearinghouse2_ObjectName internalize_Clearinghouse2_ThreePartName

typedef Clearinghouse2_ThreePartName Clearinghouse2_Name;
#define sizeof_Clearinghouse2_Name sizeof_Clearinghouse2_ThreePartName
#define clear_Clearinghouse2_Name clear_Clearinghouse2_ThreePartName
#define externalize_Clearinghouse2_Name externalize_Clearinghouse2_ThreePartName
#define internalize_Clearinghouse2_Name internalize_Clearinghouse2_ThreePartName

typedef String Clearinghouse2_OrganizationNamePattern;
#define sizeof_Clearinghouse2_OrganizationNamePattern sizeof_String
#define clear_Clearinghouse2_OrganizationNamePattern clear_String
#define externalize_Clearinghouse2_OrganizationNamePattern externalize_String
#define internalize_Clearinghouse2_OrganizationNamePattern internalize_String

typedef Clearinghouse2_TwoPartName Clearinghouse2_DomainNamePattern;
#define sizeof_Clearinghouse2_DomainNamePattern sizeof_Clearinghouse2_TwoPartName
#define clear_Clearinghouse2_DomainNamePattern clear_Clearinghouse2_TwoPartName
#define externalize_Clearinghouse2_DomainNamePattern externalize_Clearinghouse2_TwoPartName
#define internalize_Clearinghouse2_DomainNamePattern internalize_Clearinghouse2_TwoPartName

typedef Clearinghouse2_ThreePartName Clearinghouse2_ObjectNamePattern;
#define sizeof_Clearinghouse2_ObjectNamePattern sizeof_Clearinghouse2_ThreePartName
#define clear_Clearinghouse2_ObjectNamePattern clear_Clearinghouse2_ThreePartName
#define externalize_Clearinghouse2_ObjectNamePattern externalize_Clearinghouse2_ThreePartName
#define internalize_Clearinghouse2_ObjectNamePattern internalize_Clearinghouse2_ThreePartName


extern struct Clearinghouse2_StreamOfDomain;
typedef struct Clearinghouse2_StreamOfDomain Clearinghouse2_StreamOfDomain;

typedef struct {
	Cardinal length;
	String *sequence;
} Clearinghouse2_T_r2_9;

typedef struct {
	Clearinghouse2_T_r2_9 segment;
	NilRecord restOfStream;
} Clearinghouse2_T_c2_8;

typedef struct {
	Cardinal length;
	String *sequence;
} Clearinghouse2_T_c2_10;

struct Clearinghouse2_StreamOfDomain {
	StreamEnumerator designator;
	union {
		Clearinghouse2_T_c2_8 u_nextSegment;
#define nextSegment_case u.u_nextSegment
		Clearinghouse2_T_c2_10 u_lastSegment;
#define lastSegment_case u.u_lastSegment
	} u;
};

extern struct Clearinghouse2_StreamOfDomainName;
typedef struct Clearinghouse2_StreamOfDomainName Clearinghouse2_StreamOfDomainName;

typedef struct {
	Cardinal length;
	Clearinghouse2_TwoPartName *sequence;
} Clearinghouse2_T_r2_12;

typedef struct {
	Clearinghouse2_T_r2_12 segment;
	NilRecord restOfStream;
} Clearinghouse2_T_c2_11;

typedef struct {
	Cardinal length;
	Clearinghouse2_TwoPartName *sequence;
} Clearinghouse2_T_c2_13;

struct Clearinghouse2_StreamOfDomainName {
	StreamEnumerator designator;
	union {
		Clearinghouse2_T_c2_11 u_nextSegment;
#define nextSegment_case u.u_nextSegment
		Clearinghouse2_T_c2_13 u_lastSegment;
#define lastSegment_case u.u_lastSegment
	} u;
};

extern struct Clearinghouse2_StreamOfObject;
typedef struct Clearinghouse2_StreamOfObject Clearinghouse2_StreamOfObject;

typedef struct {
	Cardinal length;
	String *sequence;
} Clearinghouse2_T_r2_15;

typedef struct {
	Clearinghouse2_T_r2_15 segment;
	NilRecord restOfStream;
} Clearinghouse2_T_c2_14;

typedef struct {
	Cardinal length;
	String *sequence;
} Clearinghouse2_T_c2_16;

struct Clearinghouse2_StreamOfObject {
	StreamEnumerator designator;
	union {
		Clearinghouse2_T_c2_14 u_nextSegment;
#define nextSegment_case u.u_nextSegment
		Clearinghouse2_T_c2_16 u_lastSegment;
#define lastSegment_case u.u_lastSegment
	} u;
};

extern struct Clearinghouse2_StreamOfObjectName;
typedef struct Clearinghouse2_StreamOfObjectName Clearinghouse2_StreamOfObjectName;

typedef struct {
	Cardinal length;
	Clearinghouse2_ThreePartName *sequence;
} Clearinghouse2_T_r2_18;

typedef struct {
	Clearinghouse2_T_r2_18 segment;
	NilRecord restOfStream;
} Clearinghouse2_T_c2_17;

typedef struct {
	Cardinal length;
	Clearinghouse2_ThreePartName *sequence;
} Clearinghouse2_T_c2_19;

struct Clearinghouse2_StreamOfObjectName {
	StreamEnumerator designator;
	union {
		Clearinghouse2_T_c2_17 u_nextSegment;
#define nextSegment_case u.u_nextSegment
		Clearinghouse2_T_c2_19 u_lastSegment;
#define lastSegment_case u.u_lastSegment
	} u;
};

extern struct Clearinghouse2_StreamOfOrganization;
typedef struct Clearinghouse2_StreamOfOrganization Clearinghouse2_StreamOfOrganization;

typedef struct {
	Cardinal length;
	String *sequence;
} Clearinghouse2_T_r2_21;

typedef struct {
	Clearinghouse2_T_r2_21 segment;
	NilRecord restOfStream;
} Clearinghouse2_T_c2_20;

typedef struct {
	Cardinal length;
	String *sequence;
} Clearinghouse2_T_c2_22;

struct Clearinghouse2_StreamOfOrganization {
	StreamEnumerator designator;
	union {
		Clearinghouse2_T_c2_20 u_nextSegment;
#define nextSegment_case u.u_nextSegment
		Clearinghouse2_T_c2_22 u_lastSegment;
#define lastSegment_case u.u_lastSegment
	} u;
};

extern struct Clearinghouse2_StreamOfThreePartName;
typedef struct Clearinghouse2_StreamOfThreePartName Clearinghouse2_StreamOfThreePartName;

typedef struct {
	Cardinal length;
	Clearinghouse2_ThreePartName *sequence;
} Clearinghouse2_T_r2_24;

typedef struct {
	Clearinghouse2_T_r2_24 segment;
	NilRecord restOfStream;
} Clearinghouse2_T_c2_23;

typedef struct {
	Cardinal length;
	Clearinghouse2_ThreePartName *sequence;
} Clearinghouse2_T_c2_25;

struct Clearinghouse2_StreamOfThreePartName {
	StreamEnumerator designator;
	union {
		Clearinghouse2_T_c2_23 u_nextSegment;
#define nextSegment_case u.u_nextSegment
		Clearinghouse2_T_c2_25 u_lastSegment;
#define lastSegment_case u.u_lastSegment
	} u;
};
typedef LongCardinal Clearinghouse2_Property;
#define sizeof_Clearinghouse2_Property sizeof_LongCardinal
#define clear_Clearinghouse2_Property clear_LongCardinal
#define externalize_Clearinghouse2_Property externalize_LongCardinal
#define internalize_Clearinghouse2_Property internalize_LongCardinal


typedef struct {
	Cardinal length;
	LongCardinal *sequence;
} Clearinghouse2_Properties;

#define sizeof_Clearinghouse2_Properties(p) (1 + (p)->length * 2)

static LongCardinal Clearinghouse2_all = {0};

static LongCardinal Clearinghouse2_nullProperty = {037777777777};

typedef struct {
	Cardinal length;
	Unspecified *sequence;
} Clearinghouse2_Item;

#define sizeof_Clearinghouse2_Item(p) (1 + (p)->length * 1)

typedef Unspecified Clearinghouse2_T_r2_26[2];

#define sizeof_Clearinghouse2_T_r2_26(p) 2

#define clear_Clearinghouse2_T_r2_26(p)

typedef Unspecified Clearinghouse2_T_r2_27[3];

#define sizeof_Clearinghouse2_T_r2_27(p) 3

#define clear_Clearinghouse2_T_r2_27(p)

typedef struct {
	Clearinghouse2_T_r2_26 network;
	Clearinghouse2_T_r2_27 host;
	Unspecified socket;
} Clearinghouse2_NetworkAddress;

#define sizeof_Clearinghouse2_NetworkAddress(p) 6

#define clear_Clearinghouse2_NetworkAddress(p)

typedef struct {
	Cardinal length;
	Clearinghouse2_NetworkAddress *sequence;
} Clearinghouse2_NetworkAddressList;

#define sizeof_Clearinghouse2_NetworkAddressList(p) (1 + (p)->length * 6)

typedef struct {
	Authentication1_Credentials credentials;
	Authentication1_Verifier verifier;
} Clearinghouse2_Authenticator;

static String Clearinghouse2_wildcard = {"*"};

typedef enum {
	first = 1,
	second = 2
} Clearinghouse2_WhichArgument;
#define sizeof_Clearinghouse2_WhichArgument sizeof_enumeration
#define clear_Clearinghouse2_WhichArgument clear_enumeration
#define externalize_Clearinghouse2_WhichArgument externalize_enumeration
#define internalize_Clearinghouse2_WhichArgument internalize_enumeration


typedef enum {
	illegalProperty = 10,
	illegalOrganizationName = 11,
	illegalDomainName = 12,
	illegalObjectName = 13,
	noSuchOrganization = 14,
	noSuchDomain = 15,
	noSuchObject = 16
} Clearinghouse2_ArgumentProblem;
#define sizeof_Clearinghouse2_ArgumentProblem sizeof_enumeration
#define clear_Clearinghouse2_ArgumentProblem clear_enumeration
#define externalize_Clearinghouse2_ArgumentProblem externalize_enumeration
#define internalize_Clearinghouse2_ArgumentProblem internalize_enumeration


typedef struct {
	Clearinghouse2_ArgumentProblem problem;
	Clearinghouse2_WhichArgument which;
} T_cn2_28;

#define sizeof_T_cn2_28(p) 2

#define clear_T_cn2_28(p)

#define Clearinghouse2_ArgumentError (ERROR_OFFSET+2)
#define Clearinghouse2_ArgumentErrorArgs T_cn2_28

typedef struct {
	Authentication1_Problem problem;
} T_cn2_29;

#define sizeof_T_cn2_29(p) 1

#define clear_T_cn2_29(p)

#define Clearinghouse2_AuthenticationError (ERROR_OFFSET+6)
#define Clearinghouse2_AuthenticationErrorArgs T_cn2_29

typedef enum {
	accessRightsInsufficient = 1,
	tooBusy = 2,
	serverDown = 3,
	useCourier = 4,
	other = 5
} Clearinghouse2_CallProblem;
#define sizeof_Clearinghouse2_CallProblem sizeof_enumeration
#define clear_Clearinghouse2_CallProblem clear_enumeration
#define externalize_Clearinghouse2_CallProblem externalize_enumeration
#define internalize_Clearinghouse2_CallProblem internalize_enumeration


typedef struct {
	Clearinghouse2_CallProblem problem;
} T_cn2_30;

#define sizeof_T_cn2_30(p) 1

#define clear_T_cn2_30(p)

#define Clearinghouse2_CallError (ERROR_OFFSET+1)
#define Clearinghouse2_CallErrorArgs T_cn2_30

typedef enum {
	missing = 20,
	wrongType = 21
} Clearinghouse2_PropertyProblem;
#define sizeof_Clearinghouse2_PropertyProblem sizeof_enumeration
#define clear_Clearinghouse2_PropertyProblem clear_enumeration
#define externalize_Clearinghouse2_PropertyProblem externalize_enumeration
#define internalize_Clearinghouse2_PropertyProblem internalize_enumeration


typedef struct {
	Clearinghouse2_PropertyProblem problem;
	Clearinghouse2_ThreePartName distinguishedObject;
} T_cn2_31;

#define Clearinghouse2_PropertyError (ERROR_OFFSET+3)
#define Clearinghouse2_PropertyErrorArgs T_cn2_31

typedef enum {
	noChange = 30,
	outOfDate = 31,
	objectOverflow = 32,
	databaseOverflow = 33
} Clearinghouse2_UpdateProblem;
#define sizeof_Clearinghouse2_UpdateProblem sizeof_enumeration
#define clear_Clearinghouse2_UpdateProblem clear_enumeration
#define externalize_Clearinghouse2_UpdateProblem externalize_enumeration
#define internalize_Clearinghouse2_UpdateProblem internalize_enumeration


typedef struct {
	Clearinghouse2_UpdateProblem problem;
	Boolean found;
	Clearinghouse2_WhichArgument which;
	Clearinghouse2_ThreePartName distinguishedObject;
} T_cn2_32;

#define Clearinghouse2_UpdateError (ERROR_OFFSET+4)
#define Clearinghouse2_UpdateErrorArgs T_cn2_32

typedef struct {
	Clearinghouse2_ThreePartName hint;
} T_cn2_33;

#define Clearinghouse2_WrongServer (ERROR_OFFSET+5)
#define Clearinghouse2_WrongServerArgs T_cn2_33

extern void Clearinghouse2_CreateObject();

extern void Clearinghouse2_DeleteObject();

typedef struct {
	Clearinghouse2_ThreePartName distinguishedObject;
} Clearinghouse2_LookupObjectResults;

extern Clearinghouse2_LookupObjectResults Clearinghouse2_LookupObject();

extern void Clearinghouse2_ListOrganizations();

extern void Clearinghouse2_ListDomain();

extern void Clearinghouse2_ListObjects();

typedef struct {
	Clearinghouse2_ThreePartName distinguishedObject;
} Clearinghouse2_ListAliasesOfResults;

extern Clearinghouse2_ListAliasesOfResults Clearinghouse2_ListAliasesOf();

typedef struct {
	Clearinghouse2_ThreePartName distinguishedObject;
} Clearinghouse2_CreateAliasResults;

extern Clearinghouse2_CreateAliasResults Clearinghouse2_CreateAlias();

typedef struct {
	Clearinghouse2_ThreePartName distinguishedObject;
} Clearinghouse2_DeleteAliasResults;

extern Clearinghouse2_DeleteAliasResults Clearinghouse2_DeleteAlias();

extern void Clearinghouse2_ListAliases();

typedef struct {
	Clearinghouse2_ThreePartName distinguishedObject;
} Clearinghouse2_DeletePropertyResults;

extern Clearinghouse2_DeletePropertyResults Clearinghouse2_DeleteProperty();

typedef struct {
	Clearinghouse2_ThreePartName distinguishedObject;
	Clearinghouse2_Properties properties;
} Clearinghouse2_ListPropertiesResults;

extern Clearinghouse2_ListPropertiesResults Clearinghouse2_ListProperties();

typedef struct {
	Clearinghouse2_ThreePartName distinguishedObject;
} Clearinghouse2_AddItemPropertyResults;

extern Clearinghouse2_AddItemPropertyResults Clearinghouse2_AddItemProperty();

typedef struct {
	Clearinghouse2_ThreePartName distinguishedObject;
	Clearinghouse2_Item value;
} Clearinghouse2_RetrieveItemResults;

extern Clearinghouse2_RetrieveItemResults Clearinghouse2_RetrieveItem();

typedef struct {
	Clearinghouse2_ThreePartName distinguishedObject;
} Clearinghouse2_ChangeItemResults;

extern Clearinghouse2_ChangeItemResults Clearinghouse2_ChangeItem();

typedef struct {
	Clearinghouse2_ThreePartName distinguishedObject;
} Clearinghouse2_AddGroupPropertyResults;

extern Clearinghouse2_AddGroupPropertyResults Clearinghouse2_AddGroupProperty();

typedef struct {
	Clearinghouse2_ThreePartName distinguishedObject;
} Clearinghouse2_RetrieveMembersResults;

extern Clearinghouse2_RetrieveMembersResults Clearinghouse2_RetrieveMembers();

typedef struct {
	Clearinghouse2_ThreePartName distinguishedObject;
} Clearinghouse2_AddMemberResults;

extern Clearinghouse2_AddMemberResults Clearinghouse2_AddMember();

typedef struct {
	Clearinghouse2_ThreePartName distinguishedObject;
} Clearinghouse2_AddSelfResults;

extern Clearinghouse2_AddSelfResults Clearinghouse2_AddSelf();

typedef struct {
	Clearinghouse2_ThreePartName distinguishedObject;
} Clearinghouse2_DeleteMemberResults;

extern Clearinghouse2_DeleteMemberResults Clearinghouse2_DeleteMember();

typedef struct {
	Clearinghouse2_ThreePartName distinguishedObject;
} Clearinghouse2_DeleteSelfResults;

extern Clearinghouse2_DeleteSelfResults Clearinghouse2_DeleteSelf();

typedef struct {
	Boolean isMember;
	Clearinghouse2_ThreePartName distinguishedObject;
} Clearinghouse2_IsMemberResults;

extern Clearinghouse2_IsMemberResults Clearinghouse2_IsMember();

typedef struct {
	Clearinghouse2_NetworkAddressList address;
} Clearinghouse2_RetrieveAddressesResults;

extern Clearinghouse2_RetrieveAddressesResults Clearinghouse2_RetrieveAddresses();

extern void Clearinghouse2_ListDomainServed();

#endif __Clearinghouse

