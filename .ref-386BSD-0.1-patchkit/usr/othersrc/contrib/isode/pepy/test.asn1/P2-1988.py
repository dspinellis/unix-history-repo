-- P2 - as was ...
IPMSInformationObject { joint-iso-ccitt
	mhs(6) ipms(1) modules(0) information-objects(2) }

DEFINITIONS IMPLICIT TAGS ::=
BEGIN

-- Prologue

EXPORTS
	-- Information objects
	InformationObject, IPM, IPN, RN, NRN,
	-- Information object miscellany
	Heading, Body, ORDescriptor,
	-- Heading-fields or their sub-fields
	ThisIPMField, OriginatorField, AuthorizingUsersSubfield,
	PrimaryRecipientsSubfield, CopyRecipientSubfield,
	BlindCopyRecipientSubfield, RepliedToIPMField,
	ObsoletedIPMsSubfield, RelatedIPMsSubfield, SubjectField,
	ExpiryTimeField, ReplyTimeField, ReplyRecipientsSubfield,
	ImportanceField, SensitivityField, AutoForwardedField,
	LanguageField, TimedObsoletedIPMsSubfield,
	-- Body-part types
	IA5TextBodyPart, TelexBodyPart, VoiceBodyPart, G3FacsimileBodyPart,
	TIF0BodyPart, TeletexBodyPart, VideotexBodyPart, NationalBodyPart,
	EncryptedBodyPart, MessageBodyPart, SFDBodyPart, TIF1BodyPart,
	BilateralBodyPart,
	-- Notification-fields
	SubjectIPMField, IPNOriginator, IPMPrimaryRecipientField,
	ConversionEITsField, NonRecipientReasonField, DiscardReasonField,
	AutoForwardedCommentField, ReturnedIPMField, RecipientTimeField,
	AcknowledgementModeField, SupplRecipientInfoField,
	-- IPN miscellany
	AutoForwardComment;

IMPORTS
	-- SFD aspects
	Document
		FROM SFD { joint-iso-ccitt -- to be supplied -- }
	-- TIF1 and TIF0 aspects
	ProtocolElement
		FROM T73 { joint-iso-ccitt -- to be supplied -- }
	-- MT AS aspects
	DeliverEnvelope, EncodedInformationTypes, G3NonBasicParams,
	ORAddress, ORName, SupplementaryInformation,
	TeletexNonBasicParams
		FROM MTSAbstractService { joint-iso-ccitt
			mhs(6) -- to be supplied -- };

Time ::= UTCTime

-- Information objects

InformationObject ::= CHOICE {
	ipm [0] IPM,
	ipn [1] IPN}

-- IPM

IPM ::= SEQUENCE {
	heading Heading,
	body	Body}

-- Heading

Heading ::= SET {
	this-IPM		    ThisIPMField,
	originator		[0] OriginatorField OPTIONAL,
	authorizing-users	[1] AuthorizingUsersField OPTIONAL,
	primary-recipients	[2] PrimaryRecipientsField OPTIONAL,
	copy-recipients		[3] CopyRecipientsField DEFAULT {},
	blind-copy-recipients	[4] BlindCopyRecipientsField OPTIONAL,
	replied-to-IPM		[5] RepliedToIPMField OPTIONAL,
	obsoleted-IPMs		[6] ObsoletedIPMsField OPTIONAL,
	related-IPMs		[7] RelatedIPMsField OPTIONAL,
	subject			[8] SubjectField OPTIONAL,
	expiry-time		[9] ExpiryTimeField OPTIONAL,
	reply-time	       [10] ReplyTimeField OPTIONAL,
	reply-recipients       [11] ReplyRecipientsField OPTIONAL,
	importance	       [12] ImportanceField DEFAULT normal,
	sensitivity	       [13] SensitivityField OPTIONAL,
	auto-forwarded	       [14] AutoForwardedField DEFAULT FALSE,
	language	       [15] LanguageField OPTIONAL,
	timed-obsoleted-IPMs   [16] TimedObsoletedIPMsField OPTIONAL}

-- Heading Components

IPMIdentifier ::= [APPLICATION 11] SET {
	user			    ORAddress OPTIONAL,
	user-relative-identifier    LocalIPMIdentifier}

LocalIPMIdentifier ::= PrintableString ( SIZE (0..64))

RecipientSpecification ::= SET {
	recipient	        [0] ORDescriptor,
	notification-requests	[1] NotificationRequests DEFAULT {},
	reply-requested		[2] BOOLEAN DEFAULT FALSE}
	
NotificationRequests ::= BIT STRING {
	rn(0),
	nrn(1),
	ipm-return(2)}

ORDescriptor ::= SET {
	formal-name   		ORName OPTIONAL,
	free-form-name	    [0] FreeFromName OPTIONAL,
	telephone-number    [1] TelephoneNumber OPTIONAL}

FreeFromName ::= TeletexString (SIZE (0..64))

TelephoneNumber ::= PrintableString (SIZE (0..32))

-- This IPM heading-field

ThisIPMField ::= IPMIdentifier

-- Originator heading-field

OriginatorField ::= ORDescriptor

-- Authorizing Users heading-field

AuthorizingUsersField ::= SEQUENCE OF AuthorizingUsersSubfield

AuthorizingUsersSubfield ::= ORDescriptor

-- Primary Recipients heading-field

PrimaryRecipientsField ::= SEQUENCE OF PrimaryRecipientsSubField

PrimaryRecipientsSubField ::= RecipientSpecification

-- Copy Recipients heading-field

CopyRecipientsField ::= SEQUENCE OF CopyRecipientsSubField

CopyRecipientsSubField ::= RecipientSpecification

-- Blind Copy Recipients heading-field

BlindCopyRecipientsField ::= SEQUENCE OF BlindCopyRecipientsSubField

BlindCopyRecipientsSubField ::= RecipientSpecification

-- Replied-to IPM heading-field

RepliedToIPMField ::= IPMIdentifier

-- Obsoleted IPMs heading-field

ObsoletedIPMsField ::= SEQUENCE OF ObsoletedIPMsSubField

ObsoletedIPMsSubField ::= IPMIdentifier

-- Related IPMs heading-field

RelatedIPMsField ::= SEQUENCE OF RelatedIPMsSubField

RelatedIPMsSubField ::= IPMIdentifier

-- Subject heading-field

SubjectField ::= CHOICE {
	text TeletexString (SIZE (0..128))}

-- Expiry Time heading-field

ExpiryTimeField ::= Time

-- Reply Time heading-field

ReplyTimeField ::= Time

-- Reply Recipients heading-field

ReplyRecipientsField ::= SEQUENCE OF ReplyRecipientsSubField

ReplyRecipientsSubField ::= ORDescriptor

-- Importance heading-field

ImportanceField ::= INTEGER {
	low(0),
	normal(1),
	high(2)}

-- Sensitivity heading-field

SensitivityField ::= INTEGER {
	personal(1),
	private(2),
	company-confidential(3)}

-- Auto-forwarded heading-field

AutoForwardedField ::= BOOLEAN

-- Language heading-field

LanguageField ::= INTEGER

-- Timed Obsoleted IPMs heading-field

TimedObsoletedIPMsField ::= SEQUENCE OF TimedObsoletedIPMsSubField

TimedObsoletedIPMsSubField ::= SET {
	ipm  [1] IPMIdentifier,
	time [0] Time}

-- Body

Body ::= SEQUENCE OF BodyPart

BodyPart ::= CHOICE {
	ia5-text	[0] IA5TextBodyPart,
	telex		[1] TeletexBodyPart,
	voice		[2] VoiceBodyPart,
	g3-facsimile	[3] G3FacsimileBodyPart,
	tif0		[4] TIF0BodyPart,
	teletex		[5] TeletexBodyPart,
	videotex	[6] VideotexBodyPart,
	national	[7] NationalBodyPart,
	encrypted	[8] EncryptedBodyPart,
	message		[9] MessageBodyPart,
	sfd	       [10] SFDBodyPart,
	tif1	       [11] TIF1BodyPart,
	bilateral      [12] BilateralBodyPart,
	external       [13] ExternalBodyPart}

-- IA5 Text body-part

IA5TextBodyPart ::= SEQUENCE {
	attributes IA5TextAttributes,
	data	   IA5String}

IA5TextAttributes ::= SET {
	repertoire [0] Repertoire DEFAULT ia5}

Repertoire ::= INTEGER {
	ita2(2),
	ia5(5)}

-- Telex body-part

VoiceBodyPart ::= SEQUENCE {
	attributes VoiceAttributes,
	data	   BIT STRING}

VoiceAttributes ::= SET

-- G3 Facsimile body-part

G3FacsimileBodyPart ::= SEQUENCE {
	attributes G3FacsimileAttributes,
	data	   SEQUENCE OF BIT STRING}

G3FacsimileAttributes ::= SET {
	number-of-pages		[0] INTEGER OPTIONAL,
	non-basic-parameters	[1] G3NonBasicParams OPTIONAL}

-- TIF0 and TIF1 body-parts

TIF0BodyPart ::= T73Document

TIF1BodyPart ::= T73Document

T73Document ::= SEQUENCE OF ProtocolElement

-- Teletex body-part

TeletexBodyPart ::= SEQUENCE {
	attributes TeletexAttributes,
	data	   SEQUENCE OF TeletexString}

TeletexAttributes ::= SET {
	number-of-pages		[0] INTEGER OPTIONAL,
	telex-compatible	[1] BOOLEAN DEFAULT FALSE,
	non-basic-parameters	[2] TeletexNonBasicParams OPTIONAL}

-- Videotex body-part

VideotexBodyPart ::= SEQUENCE {
	attributes VideotexAttributes,
	data	   VideotexString}

VideotexAttributes ::= SET

-- National body-part

NationalBodyPart ::= ANY

-- Encrypted body-part

EncryptedBodyPart ::= SEQUENCE {
	attributes EncryptedAttributes,
	data	   BIT STRING}

EncryptedAttributes ::= SET

-- Message body-part

MessageBodyPart ::= SEQUENCE {
	attributes MessageAttributes,
	data	   IPM}

MessageAttributes ::= SET {
	delivery-time	  [0] Time OPTIONAL,
	delivery-envelope [1] DeliverEnvelope OPTIONAL}

-- SFD body-part

SFDBodyPart ::= Document

-- Bilateral body-part

BilateralBodyPart ::= OCTET STRING

-- External body-part

ExternalBodyPart ::= EXTERNAL

-- IPN

IPN ::= SET {
	subject-ipm		    SubjectIPMField,
	ipn-originator		[1] IPNOriginatorField OPTIONAL,
	ipm-primary-recipient	[2] IPMPrimaryRecipientField OPTIONAL,
	conversion-eits		    ConversionEITsField OPTIONAL,
-- really common-fields COMPONENTS OF CommonFields,
	choice [0] CHOICE {
		non-receipt-fields  [0] NonRecipientFields,
		receipt-fields	    [1] ReceiptFields}}

RN ::= IPN -- with receipt-fields chosen

NRN ::= IPN -- with non-receipt-fields hosen

CommonFields ::= SET {
	subject-ipm		    SubjectIPMField,
	ipn-originator		[1] IPNOriginatorField OPTIONAL,
	ipm-primary-recipient	[2] IPMPrimaryRecipientField OPTIONAL,
	conversion-eits		    ConversionEITsField OPTIONAL}

NonRecipientFields ::= SET {
	non-receipt-reason 	[0] NonRecipientReasonField,
	discard-reason		[1] DiscardReasonField OPTIONAL,
	auto-forward-comment	[2] AutoForwardCommentField OPTIONAL,
	returned-ipm		[3] ReturnedIPMField OPTIONAL}

ReceiptFields ::= SET {
	receipt-time  		[0] ReceiptTimeField,
	acknowledge-mode	[1] AcknowledgementModeField DEFAULT manual,
	suppl-receipt-info	[2] SupplRecipientInfoField OPTIONAL}

-- Common-fields

SubjectIPMField ::= IPMIdentifier

IPNOriginatorField ::= ORDescriptor

IPMPrimaryRecipientField ::= ORDescriptor

ConversionEITsField ::= EncodedInformationTypes

-- Non-receipt-fields

NonRecipientReasonField ::= INTEGER {
	ipm-discarded(0),
	ipm-auto-forwarded(1)}

DiscardReasonField ::= INTEGER {
	ipm-expired(0),
	ipm-obsoleted(1),
	user-subscription-terminated(2)}

AutoForwardCommentField ::= AutoForwardComment

AutoForwardComment ::= PrintableString (SIZE (0..256))

ReturnedIPMField ::= IPM

-- Receipt-fields

ReceiptTimeField ::= Time

AcknowledgementModeField ::= INTEGER {
	manual(0),
	automatic(1)}

SupplRecipientInfoField ::= SupplementaryInformation

--
-- Hacky - shold be got from P1 defs
--
-- 
-- ORAddress ::= ORIdentifier
-- 
-- ORName ::= ORIdentifier
-- 
-- ORIdentifier ::= [APPLICATION 0] SEQUENCE
-- 
-- EncodedInformationTypes ::=  [APPLICATION 5] SET
-- 
END -- of IPMSInformationObjects
