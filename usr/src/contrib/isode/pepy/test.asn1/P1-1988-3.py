MTSAbstractService { joint-iso-ccitt mhs-motis (6) mts (3)
			modules (0) mts-abstract-service (3) }

DEFINITIONS IMPLICIT TAGS ::=

BEGIN

-- Prologue

EXPORTS -- EVERYTHING
	-- MTS Abstract Service Parameters
	MTS, SubmissionPort, DeliveryPort, AdministrationPort,
	MessageSubmission, ProbeSubmission, CancelDefferedDelivery,
	SubmissionControl, MessageDelivery, ReportDelivery,
	DeliveryControl, Register, ChangeCredentials,
	InitiatorCredentials, Context, SecurityContext,
	ResponderCredentials, OriginalEncodedInformationTypes,
	BuiltinContentType, ContentIdentifier, Priority,
	PerMessageIndicators, DeferredDeliveryTime, CountryName,
	AdministrationDomainName, PrivateDomainName,
	ExplicitConversion, ContentLength, ReturnedContent,
	ConvertedEncodedInformationTypes, Report,
	SupplementaryInformation, EXTERNAL-FIELD,
	RecipientRedirectionProhibited, DLExpansionProhibited,
	ConversionWithLossProhibited, LatestDeliveryTime,
	PhysicalDeliveryRequired, PhysicalForwardingIndication,
	DeliveryModes, RegistrationIndication,
	RecipientNumberForAdvice, PhysicalRenditionAttributes,
	OriginatorReturnAddress, PhysicalDeliveryReportRequest,
	OriginatorCertificate, MessageToken,
	ContentConfidentialityAlgorithmIdentifier,
	ContentIntegrityCheck, MessageOriginAuthenticationCheck,
	MessageSecurityLabel, ProofOfDeliveryRequest,
	ExternalContentType, ContentCorrelator,
	ProbeOriginAuthenticationCheck, RedirectionReason,
	RecipientCertificate, ProofOfDelivery,
	ReportingMTACertificate, ReportOriginAuthenticationCheck,
	Content, MTSIdentifier, GlobalDomainIdentifier, MTAName, Time,
	ORAddressAndOptionalName;

IMPORTS
	-- Abstract Service Macros
-- OBJECT causes keyword collision.
--	OBJECT, PORT, ABSTRACT-BIND, ABSTRACT-UNBIND,
--	ABSTRACT-OPERATION, ABSTRACT-ERROR
--		FROM AbstractServiceNotation { joint-iso-ccitt mhs-motis (6)
--			asdc (2) modules (0) notation (1) }

	-- Object Identifiers
	id-ot-mts, id-ot-mtsuser,
	id-pt-submission, id-pt-delivery,
	id-pt-administration,
	id-att-physicalRendition-basic,
	id-tok-asymmetricToken
		FROM MTSObjectIdentifiers { joint-iso-ccitt mhs-motis (6)
		mts (3) modules (0) object-identifiers (0) }
	-- Directory Definitions
	Name
		FROM InformationFramework { joint-iso-ccitt ds (5) modules (1)
			information-framework (1) }
	PresentationAddress
		FROM SelectedAttributeTypes { joint-iso-ccitt ds (5)
			modules (1) selectedAttributeTypes (5) }
	Certificates, AlgorithmIdentifier, ALGORITHM, SIGNED,
	SIGNATURE
		FROM AuthenticationFramework { joint-iso-ccitt ds (5)
			modules (1) authentication-framework (7) }

	EVERYTHING
		FROM MTSUpperBounds { joint-iso-ccitt mhs-motis (6)
			mts (3) modules (0) upper-bounds (3) };

-- Port definitions & Services left out...

-- Association Control Parameters

ObjectName ::= CHOICE {
	mTS-user ORAddressAndOptionalDirectoryName,
	mTA [0] MTAName }

MessagesWaiting ::= SET {
	urgent [0] DeliveryQueue,
	normal [1] DeliveryQueue,
	nonUrgent [2] DeliveryQueue }

DeliveryQueue ::= SET {
	messages [0] INTEGER (0..ub-queue-size),
	octetst [1] INTEGER (0..ub-content-length) OPTIONAL }

InitiatorCredentials ::= CHOICE {
	simple Password,
	strong [0] StrongCredentials (WITH COMPONENTS {
		...,
		bind-token PRESENT }) }

ResponderCredentials ::= CHOICE {
	simple Password,
	strong [0] StrongCredentials (WITH COMPONENTS {
		bind-token }) }

Password ::= CHOICE {
	IA5String (SIZE (0..ub-password-length)),
	OCTET STRING (SIZE (0..ub-password-length)) }

StrongCredentials ::= SET {
	bind-token [0] Token OPTIONAL,
	certificate [1] Certificates OPTIONAL }
	
Context ::= OBJECT IDENTIFIER

SecurityContext ::= SET SIZE (1..ub-security-labels) OF SecurityLabel

-- Submission Port Services

-- Missed out as we can't do Macro's!


-- Submission Port Parameters

MessageSubmissionIdentifier ::= MTSIdentifier

MessageSubmissionTime ::= Time

ProbeSubmissionIdentifier ::= MTSIdentifier

ProbeSubmissionTime ::= Time

SubmissionControls ::= Controls (WITH COMPONENTS {
	...,
	permissible-content-types ABSENT,
	permissible-encoded-information-types ABSENT } )

Waiting ::= SET {
	waiting-operations [0] Operations DEFAULT {},
	waiting-messages [1] WaitingMessages DEFAULT {},
	waiting-content-types [2] SET SIZE (0..ub-content-types)
		OF ContentType DEFAULT {},
	waiting-encoded-information-types EncodedInformationTypes
		OPTIONAL }

Operations ::= BIT STRING {
	probe-submission-or-report-delivery (0),
	message-submission-or-message-delivery (1) }
		(SIZE (1..ub-bit-options))
	-- holding 'one', not holding 'zero'

WaitingMessages ::= BIT STRING {
	long-content (0),
	low-priority (1),
	other-security-labels (2) } (SIZE (0..ub-bit-options))

-- Delivery Port Services

-- This bit also missing

RecipientCertificate ::= Certificates

ProofOfDelivery ::= -- SIGNATURE -- SEQUENCE {
	algorithm-identifier ProofOfDeliveryAlgorithmIdentifier,
	delivery-time DeliveryTime,
	this-recipient-identifier ThisRecipientName,
	originally-intended-recipient-identifier
		OriginallyIntendedRecipientName OPTIONAL,
	content Content,
	content-identifier ContentIdentifier OPTIONAL,
	message-security-label MessageSecurityLabel OPTIONAL }

ProofOfDeliveryAlgorithmIdentifier ::= AlgorithmIdentifier

DeliveryControls ::= Controls

Controls ::= SET {
	restrict [0] BOOLEAN DEFAULT TRUE,
	-- update 'TRUE", remove 'FALSE'
	permissible-operations [1] Operations OPTIONAL,
	permissible-maximum-content-length [2] ContentLength OPTIONAL,
	permissible-lowest-priority Priority OPTIONAL,
	permissible-content-types [4] SET SIZE (1..ub-content-types)
		OF ContentTypes OPTIONAL,
	permissible-encoded-information-types EncodedInformationTypes OPTIONAL,
	permissible-security-context [5] SecurityContext OPTIONAL }

-- Temporary Note: The tags [0], [1] and [2] are altered for the
-- Register operation only.

-- Administration Port Services

-- Also missing

-- Administration Port Parameters

UserName ::= ORAddressAndOptionalDirectoryName

UserAddress ::= CHOICE {
	x121 [0] SEQUENCE {
		x121-address NumericString (SIZE (1..ub-x121-address-length))
			 OPTIONAL,
		tsap-id PrintableString (SIZE (1..ub-tsap-id-length)) 
			OPTIONAL },
	presentation [1] PSAPAddress }

PSAPAddress ::= PresentationAddress

DefaultDeliveryControls ::= Controls (WITH COMPONENTS {
	...,
	permissible-security-context ABSENT } )

Credentials ::= CHOICE {
	simple Password,
	strong [0] StrongCredentials (WITH COMPONENTS {
		certificate }) }

LabelAndRedirection ::= SET {
	user-security-label [0] UserSecurityLabel OPTIONAL,
	recipient-requested-alternate-recipient [1]
		RecipientRequestedAlternateRecipient OPTIONAL }

UserSecurityLabel ::= SecurityLabel

RecipientRequestedAlternateRecipient ::= ORAddressAndOptionalDirectoryName

-- Message Submission Envelope

MessageSubmissionEnvelope ::= SET {
	per-message-fields COMPONENTS OF PerMessageSubmissionFields,
	per-recipient-fields [1] SEQUENCE SIZE (1..ub-recipients) OF
		PerRecipientSubmissionFields }

PerMessageSubmissionFields ::= SET {
	originator-identifier OriginatorName,
	original-encoded-information-types
		OriginalEncodedInformationTypes OPTIONAL,
	content-type ContentType,
	content-identifier ContentIdentifier OPTIONAL,
	priority Priority DEFAULT normal,
	per-message-indicators PerMessageIndicators DEFAULT {},
	deferred-delivery-time [0] DeferredDeliveryTime OPTIONAL,
	external-fields [2] ExternalFields DEFAULT {} }

PerRecipientSubmissionFields ::= SET {
	recipient-identifier RecipientName,
	originator-report-request [0] OriginatorReportRequest,
	explicit-conversion [1] ExplicitConversion OPTIONAL,
	external-fields [2] ExternalFields DEFAULT {} }

-- Probe Submission Envelope

ProbeSubmissionEnvelope ::= SET {
	per-probe-fields COMPONENTS OF PerProbeSubmissionFields,
	per-recipient-fields [3] SEQUENCE SIZE (1..ub-recipients) OF
		PerRecipientProbeSubmissionFields }

PerProbeSubmissionFields ::= SET {
	originator-identifier OriginatorName,
	original-encoded-information-types
		OriginalEncodedInformationTypes OPTIONAL,
	content-type ContentType,
	content-identifier ContentIdentifier OPTIONAL,
	content-length [0] ContentLength OPTIONAL,
	per-message-indicators PerMessageIndicators DEFAULT {},
	external-fields [2] ExternalFields DEFAULT {} }

PerRecipientProbeSubmissionFields ::= SET {
	recipient-name RecipientName,
	originator-report-request [0] OriginatorReportRequest,
	explicit-conversion [1] ExplicitConversion OPTIONAL,
	extensions [2] ExternalFields DEFAULT {} }

-- Message Delivery Envelope

MessageDeliveryEnvelope ::= SEQUENCE {
	message-delivery-identifier MessageDeliveryIdentifier,
	message-delivery-time MessageDeliveryTime,
	other-fields OtherMessageDeliveryFields }

OtherMessageDeliveryFields ::= SET {
	content-type [0] DeliveredContentType,
	originator-identifier OriginatorName,
	original-encoded-information-types [1]
		OriginalEncodedInformationTypes OPTIONAL,
	priority Priority DEFAULT normal,
	delivery-flags [2] DeliveryFlags OPTIONAL,
	other-recipient-names [3] OtherRecipientNames OPTIONAL,
	this-recipient-name [4] ThisRecipientName,
	originally-intended-recipient-name [5]
		OriginallyIntendedRecipientName OPTIONAL,
	converted-encoded-information-types [6]
		ConvertedEncodedInformationTypes OPTIONAL,
	message-submission-time [7] MessageSubmissionTime,
	content-identifier [8] ContentIdentifier OPTIONAL,
	external-fields [9] ExternalFields DEFAULT {} }

-- Report Delivery Envelope

ReportDeliveryEnvelope ::= SET {
	per-report-fields COMPONENTS OF PerReportDeliveryFields,
	per-recipient-fields SEQUENCE SIZE (1..ub-recipients) OF
		PerRecipientReportDeliveryFields }

PerReportDeliveryFields ::= SET {
	subject-submission-identifier SubjectSubmissionIdentifier,
	content-identifier ContentIdentifier OPTIONAL,
	content-type ContentType OPTIONAL,
	original-encoded-information-types
		OriginalEncodedInformationTypes OPTIONAL,
	external-fields [1] ExternalFields DEFAULT {} }

PerRecipientReportDeliveryFields ::= SET {
	actual-recipient-name [0] ActualRecipientName,
	report [1] Report,
	converted-encoded-information-types
		ConvertedEncodedInformationTypes OPTIONAL,
	originally-intended-recipient-name [2]
		OriginallyIntendedRecipientName OPTIONAL,
	supplementary-information [3] SupplementaryInformation OPTIONAL,
	external-fields [4] ExternalFields DEFAULT {} }

Report ::= CHOICE {
	delivery [0] DeliveryReport,
	non-delivery [1] NonDeliveryReport }

DeliveryReport ::= SET {
	message-delivery-time [0] MessageDeliveryTime,
	type-of-MTS-user [1] TypeOfMTSUser DEFAULT public }

NonDeliveryReport ::= SET {
	non-delivery-reason-code [0] NonDeliveryReasonCode,
	non-delivery-diagnostic-code [1] NonDeliveryDiagnosticCode
		OPTIONAL }

-- Envelope Fields

OriginatorName ::= ORAddressAndOrDirectoryName

OriginalEncodedInformationTypes ::= EncodedInformationTypes

ContentType ::= CHOICE {
	built-in BuiltinContentType,
	external ExternalContentType }

BuiltinContentType ::= [APPLICATION 6] INTEGER {
	unidentified (0),
	external (1),	-- identified by the ExternalContentType external-field
	interpersonal-messaging-1984 (2),
	interpersonal-messaging-1988 (22) } (0..ub-built-in-content-type)

ExternalContentType ::= OBJECT IDENTIFIER

DeliveredContentType ::= CHOICE {
	built-in [0] BuiltinContentType,
	external ExternalContentType }

ContentIdentifier ::= [APPLICATION 10] PrintableString
				(SIZE (1..ub-content-id-length))

PerMessageIndicators ::= [APPLICATION 8] BIT STRING {
	disclosur-ofe-recipients (0),
				  -- disclosure-of-recipients-allowed 'one'
				  -- disclosure-of-recipients-prohibited 'zero'
				  -- ignored for Probe-Submission
	implicit-conversion-prohibited (1),
				  -- conversion-prohibited 'one',
				  -- conversion-allowed 'zero'
	alternate-recipient-allowed (2),
				  -- alternate-recipient-allowed 'one'
				  -- alternate-recipient-prohibited 'zero'
	content-return-request (3)-- content-return-request 'one',
				  -- content-return-not-requested 'zero';
				  -- ignored for Probe-Submission -- }
				  (SIZE (0..ub-bit-options))

RecipientName ::= ORAddressAndOrDirectoryName

OriginatorReportRequest ::= BIT STRING {
	report (3),
	non-delivery-report (4)
	-- at most one bit shall be 'one':
	-- report bit 'one' requests a 'report';
	-- non-delivery-report bit 'one' requests a 'non-delivery-report';
	-- both bits 'zero' requests 'no-report' --  } 
			(SIZE (1..ub-bit-options))

ExplicitConversion ::= INTEGER {
	ia5-text-to-telex (0),
	teletex-to-telex (1),
	telex-to-ia5-text (2),
	telex-to-teletex (3),
	telex-to-g4-class-1 (4),
	telex-to-videotex (5),
	ia5-text-to-telex (6),
	ia5-text-to-teletex (7),
	ia5-text-to-g3-facsimile (8),
	ia5-text-to-g4-class-1 (9),
	ia5-text-to-videotex (10),
	teletex-to-ia5-text (11),
	teletex-to-g3-facsimile (12),
	teletex-to-g4-class-1 (13),
	teletex-to-videotex (14),
	videotex-to-telex (15),
	videotex-to-ia5-text (16),
	videotex-to-teletex (17) } (0..ub-integer-options)
	
DeferredDeliveryTime ::= Time

Priority ::= [APPLICATION 7] ENUMERATED {
	normal (0),
	non-urgent (1),
	urgent (2) }

ContentLength ::= INTEGER (0..ub-content-length)

MessageDeliveryIdentifier ::= MTSIdentifier

MessageDeliveryTime ::= Time

DeliveryFlags ::= BIT STRING {
	conversion-prohibited (1) -- conversion-prohibited 'one',
				  -- conversion-allowed 'zeo' -- }
		(SIZE (0..ub-bit-options))

OtherRecipientNames ::= SEQUENCE SIZE (1..ub-recipient) OF
			OtherRecipientName

OtherRecipientName ::= ORAddressAndOrDirectoryName

ThisRecipientName ::= ORAddressAndOrDirectoryName

OriginallyIntendedRecipientName ::= ORAddressAndOrDirectoryName

ConvertedEncodedInformationTypes ::= EncodedInformationTypes

SubjectSubmissionIdentifier ::= MTSIdentifier

ActualRecipientName ::= ORAddressAndOrDirectoryName

TypeOfMTSUser ::= INTEGER {
	public (0),
	private (1),
	ms (2),
	dl (3),
	pdau (4),
	physical-recipient (5),
	other (6) } (0..ub-mts-user-types)

NonDeliveryReasonCode ::= INTEGER {
	transfer-failure (0),
	unable-to-transfer (1),
	conversion-not-performed (2),
	physical-rendition-not-performed (3),
	physical-delivery-not-performed (4),
	restricted-delivery (5),
	directory-operation-unsucessful (6) } (0..ub-reason-codes)

NonDeliveryDiagnosticCode ::= INTEGER {
	unrecognised-OR-identifier (0),
	ambiguous-OR-identifier (1),
	mts-congestion (2),
	loop-detected (3),
	recipient-unavailable (4),
	maximum-time-expired (5),
	encoded-information-types-unsupported (6),
	content-too-long (7),
	conversion-impractical (8),
	implicit-conversion-prohibited (9),
	implicit-conversion-not-subscribed (10),
	invalid-arguments (11),
	content-syntax-error (12),
	size-constraint-violation (13),
	protocol-violation (14),
	content-type-not-supported (15),
	too-many-recipients (16),
	no-bilateral-agreement (17),
	unsupported-critical-function (18),
	conversion-with-loss-prohibited (19),
	line-too-long (20),
	page-split (21),
	pictorial-symbol-loss (22),
	punctuation-symbol-loss (23),
	alphabetic-character-loss (24),
	multiple-information-loss (25),
	recipient-reassignment-prohibited (26),
	redirection-loop-detected (27),
	dl-expansion-prohibited (28),
	no-DL-submit-permission (29),
	dl-expansion-failure (30),
	physical-rendition-attributes-not-supported (31),
	undeliverable-mail-physical-delivery-address-incorrect (32),
	undeliverable-mail-physical-delivery-office-incorrect-or-invalid (33),
	undeliverable-mail-physical-delivery-address-incomplete (34),
	undeliverable-mail-recipient-unknown (35),
	undeliverable-mail-recipient-deceased (36),
	undeliverable-mail-organisation-expired (37),
	undeliverable-mail-recipient-refused-to-accept (38),
	undeliverable-mail-recipient-did-not-claim (39),
	undeliverable-mail-recipient-changed-address-permanently (40),
	undeliverable-mail-recipient-changed-address-temporarily (41),
	undeliverable-mail-recipient-changed-temporary-address (42),
	undeliverable-mail-recipient-new-address-unknown (43),
	undeliverable-mail-recipient-did-not-want-forwarding (44),
	undeliverable-mail-recipient-prohibited-forwarding (45),
	secure-messaging-error (46) } (0..ub-diagnostic-codes)

SupplementaryInformation ::= PrintableString
			(SIZE (1..ub-supplementary-info-length))

-- Extension Fields

ExtensionField ::= SEQUENCE {
	type [0] ExtensionType,
	criticality [1] Criticality DEFAULT { },
	value [2] ANY DEFINED BY type OPTIONAL }
	
ExtensionType ::= INTEGER (0..ub-extension-types)

Criticality ::= BIT STRING {
	for-submission (0),
	for-transfer (1),
	for-delivery (2) } (SIZE (0..ub-bit-options))   -- critical 'one';
							-- non-critical 'zero'

-- EXTENSION MACRO - missing...

RecipientReassignmentProhibited ::= ENUMERATED {
	recipient-reassignment-allowed (0),
	recipient-reassignment-prohibited (1) }

OriginatorRequestedAlternateRecipient ::= ORAddressAndOrDirectoryName

DLExpansionProhibited ::= ENUMERATED {
	dl-expansion-allowed (0),
	dl-expansion-prohibited (1) }

ConversionWithLossProhibited ::= ENUMERATED {
	conversion-with-loss-allowed (0),
	conversion-with-loss-prohibited (1) }

LatestDeliveryTime ::= Time

RequestedDeliveryMethod ::= SEQUENCE OF INTEGER { -- each different
	any-delivery-method (0),
	mhs-delivery (1),
	physical-delivery (2),
	telex-delivery (3),
	teletex-delivery (4),
	g3-facsimile-delivery (5),
	g4-facsimile-delivery (6),
	ia5-terminal-delivery (7),
	viodeotex-delivery (8) } (0..ub-integer-options)

PhysicalForwardingProhibited ::= ENUMERATED {
	physical-forwarding-allowed (0),
	physical-forwarding-prohibited (1) }

PhysicalForwardingAddressRequest ::= ENUMERATED {
	physical-forwarding-address-not-requested (0),
	physical-forwarding-address-requested (1) }


DeliveryModes ::= BIT STRING {
	ordinary-mail (0),
	special-delivery (1),
	express-mail (2),
	counter-collection (3),
	counter-collection-with-telephone-advice (4),
	counter-collection-with-telex-advice (5),
	counter-collection-with-teletex-advice (6),
	bureau-fax-delivery (7)
-- bits 0 to 6 are mutually exclusive
-- bit 7 can be set with any of bits 0 to 6 -- } (SIZE (0..ub-bit-options))

RegistrationMailType ::= INTEGER {
	non-registered-mail (0),
	registered-mail (1),
	registered-mail-to-addressee-in-person (2) } (0..ub-integer-options)

RecipientNumberForAdvice ::= TeletexString 
	(SIZE (1..ub-recipient-number-for-advice-length))

PhysicalRenditionAttributes ::= OBJECT IDENTIFIER

OriginatorReturnAddress ::= ORAddress

PhysicalDeliveryReportRequest ::= INTEGER {
	return-of-undeliverable-mail-by-PDS (0),
	return-of-notification-by-PDS (1),
	return-of-notification-by-MHS (2),
	return-of-notification-by-MHS-and-PDS (3) } (0..ub-integer-options)

OriginatorCertificate ::= Certificates

MessageToken ::= Token

ContentConfidentialityAlgorithmIdentifier ::= AlgorithmIdentifier


ContentIntegrityCheck ::= -- SIGNATURE -- SEQUENCE {
	algorithm-identifier ContentIntegrityAlgorithmIdentifier,
	conent Content }

ContentIntegrityAlgorithmIdentifier ::= AlgorithmIdentifier

MessageOriginAuthenticationCheck ::= -- SIGNATURE -- SEQUENCE {
	algorithm-identifier MessageOriginAuthenticationAlgorithmIdentifier,
	content Content,
	content-identifier ContentIdentifier OPTIONAL,
	message-security-label MessageSecurityLabel OPTIONAL }

MessageOriginAuthenticationAlgorithmIdentifier ::= AlgorithmIdentifier

MessageSecurityLabel ::= SecurityLabel

ProofOfSubmissionRequest ::= ENUMERATED {
	proof-of-delivery-not-requested (0),
	proof-of-delivery-requested (1) }

ProofOfDeliveryRequest ::= ENUMERATED {
	proof-of-delivery-not-requested (0),
	proof-of-delivery-requested (1) }

ContentCorrelator ::= ANY

ProbeOriginAuthenticationCheck ::= -- SIGNATURE -- SEQUENCE {
	algorithm-identifier ProbeOriginAuthenticationAlgorithmIdentifier,
	content-identifier ContentIdentifier OPTIONAL,
	message-security-label MessageSecurityLabel OPTIONAL }

ProbeOriginAuthenticationAlgorithmIdentifier ::= AlgorithmIdentifier

RedirectionHistory ::= SEQUENCE SIZE (1..ub-redirections) OF Redirection

Redirection ::= SEQUENCE {
	intended-recipient-identifier IntendedRecipientName,
	redirection-reason RedirectionReason }

RedirectionReason ::= ENUMERATED {
	recipient-requested-alternate-recipient (0),
	originator-requested-alternate-recipient (1),
	recipient-MD-assigned-alternate-recipient (2) }

DLExpansionHistory ::= SEQUENCE SIZE (1..ub-dl-expansions) OF DLExpansion

DLExpansion ::= ORAddressAndOrDirectoryName

PhysicalForwardingAddress ::= ORAddressAndOptionalDirectoryName

OriginatorAndDLExpansionHistory ::= SEQUENCE SIZE (0..ub-dl-expansions)
				OF ORAddressAndOrDirectoryName

ReportingDLIdentifier ::= ORAddressAndOrDirectoryName

ReportingMTACertificate ::= Certificates

ReportOriginAuthenticationCheck ::= -- SIGNATURE -- SEQUENCE {
	algorithm-identifier ReportOriginAuthenticationAlgorithmIdentifier,
	content-identifier ContentIdentifier OPTIONAL,
	message-security-label MessageSecurityLabel OPTIONAL,
	per-recipient SEQUENCE SIZE (1..ub-recipients) OF 
			PerRecipientReportFields }

ReportOriginAuthenticationAlgorithmIdentifier ::= AlgorithmIdentifier

PerRecipientReportFields ::= SEQUENCE {
	actual-recipient-name ActualRecipientName,
	originally-intended-recipient-name
		OriginallyIntendedRecipientName OPTIONAL,
	CHOICE {
		delivery [0] PerRecipientDeliveryReportFields,
		non-delivery [1] PerRecipientNonDeliveryReportFields } }

PerRecipientDeliveryReportFields ::= SEQUENCE {
	message-delivery-time MessageDeliveryTime,
	type-of-MTS-user TypeOfMTSUser,
	recipient-certificate [0] RecipientCertificate OPTIONAL,
	proof-of-delivery [1] ProofOfDelivery OPTIONAL }

PerRecipientNonDeliveryReportFields ::= SEQUENCE {
	non-delivery-reason-code NonDeliveryReasonCode,
	non-delivery-diagnostic-code NonDeliveryDiagnosticCode OPTIONAL }

OriginatingMTACertificate ::= Certificates

ProofOfSubmission ::= -- SIGNATURE -- SEQUENCE {
	algorithm-identifier ProofOfSubmissionAlgorithmIdentifier,
	message-submission-envelope MessageSubmissionEnvelope,
	message-submission-identifier MessageSubmissionIdentifier,
	message-submission-time MessageSubmissionTime }

ProofOfSubmissionAlgorithmIdentifier ::= AlgorithmIdentifier

SecurityProblem ::= INTEGER

-- Common Parameter Types

Content ::= OCTET STRING

ExternalContent ::= EXTERNAL

MTSIdentifier ::= [APPLICATION 4] SEQUENCE {
	global-domain-identifier GlobalDomainIdentifier,
	local-identifier LocalIdentifier }

LocalIdentifier ::= IA5String (SIZE (1..ub-local-id-length))

GlobalDomainIdentifier ::= [APPLICATION 3] SEQUENCE {
	country-name CountryName,
	administration-domain-name AdministrationDomainName,
	private-domain-identifier PrivateDomainIdentifier OPTIONAL }

PrivateDomainIdentifier ::= CHOICE {
	numeric NumericString (SIZE (1..ub-domain-name-length)),
	printable PrintableString (SIZE (1..ub-domain-name-length)) }

MTAName ::= IA5String (SIZE (1..ub-mta-name-length))

Time ::= UTCTime

-- OR Names

ORAddressAndOrDirectoryName ::= ORName

ORAddressAndOptionalDirectoryName ::= ORName

ORName ::= [APPLICATION 0] SEQUENCE {
	address COMPONENTS OF ORAddress,
	directory-name [0] Name OPTIONAL }

ORAddress ::= SEQUENCE {
	standard-attributes StandardAttributes,
	domain-defined-attributes DomainDefinedAttributes OPTIONAL,
	extension-attributes ExtensionAttributes OPTIONAL }
-- Note: the OR-address is semmantically absent from the OR-name if the
-- standard-attribute sequence is empty and the domain-defined-attributes
-- and extension-attributes are both omitted.

-- Standard Attributes

StandardAttributes ::= SEQUENCE {
	country-name CountryName OPTIONAL,
	administration-domain-name AdministrationDomainName OPTIONAL,
	network-address [0] NetworkAddress OPTIONAL,
	terminal-identifier [1] TerminalIdentifier OPTIONAL,
	private-domain-name [2] PrivateDomainIdentifier OPTIONAL,
	organisation-name [3] OrganisationName OPTIONAL,
	numeric-user-identifier [4] NumericUserIdentifier OPTIONAL,
	personal-name [5] PersonalName,
	organisational-unit-names [6] OrganisationalUnitNames OPTIONAL }

CountryName ::= [APPLICATION 1]  CHOICE {
	x121-dcc-code NumericString (SIZE (ub-country-name-numeric-length)),
	ios-3166-aplha2-code PrintableString 
		(SIZE (ub-country-name-alpha-length)) }

AdministrationDomainName ::= [APPLICATION 2] CHOICE {
	numeric NumericString (SIZE (0..ub-domain-name-length)),
	printable PrintableString (SIZE (0..ub-domain-name-length)) }

NetworkAddress ::= X121Address

X121Address ::= NumericString (SIZE (1..ub-x121-address-length))

TerminalIdentifier ::= PrintableString (SIZE (1..ub-terminal-id-length))

PrivateDomainName ::= CHOICE {
	numeric NumericString (SIZE (1..ub-domain-name-length)),
	printable PrintableString (SIZE (1..ub-domain-name-length)) }

OrganisationName ::= PrintableString (SIZE (1..ub-organisation-name-length))

NumericUserIdentifier ::= NumericString (SIZE (1..ub-numeric-user-id-length))

PersonalName ::= SET {
	surname [0] PrintableString (SIZE (1..ub-surname-length)),
	given-name [1] PrintableString 
		(SIZE (1..ub-given-name-length)) OPTIONAL,
	initials [2] PrintableString (SIZE (1..ub-initials-length)) OPTIONAL,
	generation-qualifier [3] PrintableString 
				(SIZE (1..ub-generation-qualifier))
				OPTIONAL }

OrganizationalUnitNames ::= SEQUENCE SIZE (1..ub-organization-unit-length)
				OF OrganizationUnitName

OrganizationUnitName ::= PrintableString 
		(SIZE (1..ub-organizational-unit-name))

-- Domain Defined Attributes

DomainDefinedAttributes ::= SEQUENCE SIZE (1..ub-domain-defined-attributes)
		OF DomainDefinedAttribute

DomainDefinedAttribute ::= SEQUENCE {
	type PrintableString
		(SIZE (1..ub-domain-defined-attributes-type-length)),
	value PrintableString
		(SIZE (1..ub-domain-defined-attributes-value-length)) }

-- Extension Attributes

ExtensionAttributes ::= SET SIZE (1..ub-extension-attributes)
			OF ExtensionAttribute

ExtensionAttribute ::= SEQUENCE {
	extension-attribute-type [0] EXTENSION-ATTRIBUTE,
	extension-attribute-value [1] ANY DEFINED BY extension-attribute-type }

-- extension-attribute macro missing

CommonName ::= PrintableString (SIZE (1..ub-common-name-length))

TeletexCommonName ::= TeletexString (SIZE (1..ub-common-name-length))

TeletexOrganizationalName ::= TeletexString 
		(SIZE (1..ub-organisation-name-length))

TeletexPersonalName ::= SET {
	surname [0] TeletexString (SIZE (1..ub-surname-length)),
	given-name [1] TeletexString 
		(SIZE (1..ub-given-name-length)) OPTIONAL,
	initials [2] TeletexString (SIZE (1..ub-initials-length)) OPTIONAL,
	generation-qualifier [3] TeletexString 
				(SIZE (1..ub-generation-qualifier)) }


TeletexOrganizationalUnitNames ::= SEQUENCE SIZE (1..ub-organizational-units)
		OF TeletexOrganizationalName

TeletexOrganizationalName ::= TeletexString 
		(SIZE (1..ub-organizational-unit-name-length))

TeletexDomainDefinedAttributes ::= SEQUENCE
		 SIZE (1..ub-domain-defined-attributes)
		OF TeletexDomainDefinedAttribute

TeletexDomainDefinedAttribute ::= SEQUENCE {
	type TeletexString
		(SIZE (1..ub-domain-defined-attributes-type-length)),
	value TeletexString
		(SIZE (1..ub-domain-defined-attributes-value-length)) }

PDSName ::= PrintableString (SIZE (1..ub-pds-name-length))

PhysicalDeliveryCountryName ::= CHOICE {
	x121-dcc-code NumericString (SIZE (1..ub-country-name-numeric-length)),
	iso-3166-alpha2-code PrintableString
		(SIZE (1..ub-country-name-alpha-length)) }

PostalCode ::= CHOICE {
	numeric-code NumericString (SIZE (1..ub-postal-code-length)),
	printable PrintableString (SIZE (1..ub-postal-code-length)) }

PhysicalDeliveryOfficeName ::= SET {
	printable-string PrintableString
		(SIZE (1..ub-pds-parmameter-length)) OPTIONAL,
	teletex-string TeletexString (SIZE (1..ub-pds-parmameter-length))
		OPTIONAL }

PhysicalDeliveryOfficeNumber ::= SET {
	printable-string PrintableString (SIZE (1..ub-pds-parmameter-length)),
	teletex-string TeletexString (SIZE (1..ub-pds-parmameter-length)) }

ExtensionORAddressComponents ::= SET {
	printable-string PrintableString
		(SIZE (1..ub-pds-parmameter-length)) OPTIONAL,
	teletex-string TeletexString (SIZE (1..ub-pds-parmameter-length)) }

PhysicalDeliveryPersonalName ::= SET {
	printable-string PrintableString 
		(SIZE (1..ub-pds-parmameter-length)) OPTIONAL,
	teletex-string TeletexString
		(SIZE (1..ub-pds-parmameter-length)) OPTIONAL }

PhysicalDeliveryOrganisationName ::= SET {
        printable-string PrintableString
                (SIZE (1..ub-pds-parmameter-length)) OPTIONAL,
        teletex-string TeletexString
                (SIZE (1..ub-pds-parmameter-length)) OPTIONAL }

ExtensionPhysicalDeliveryAddressComponents ::= SET {
        printable-string PrintableString
                (SIZE (1..ub-pds-parmameter-length)) OPTIONAL,
        teletex-string TeletexString
                (SIZE (1..ub-pds-parmameter-length)) OPTIONAL }

UnformattedPostalAddress ::= SET {
	printable-string SEQUENCE SIZE (1..ub-physical-address-lines) OF
		PrintableString (SIZE (1..ub-pds-parameter-length)) OPTIONAL,
	teletex-string TeletexString 
		(SIZE (1..ub-unformatted-address-length)) OPTIONAL }

StreetAddress ::= SEQUENCE {
	printable-string [0] PrintableString
		(SIZE (1..ub-pds-parameter-length)) OPTIONAL,
	teletex-string [1] TeletexString
		(SIZE (1..ub-pds-parameter-length)) OPTIONAL }

PostOfficeAddress ::= SET {
	printable-string PrintableString
		(SIZE (1..ub-pds-parameter-length)) OPTIONAL,
	teletex-string TeletexString
		(SIZE (1..ub-pds-parameter-length)) OPTIONAL }

PosteRestanteAddress ::= SET {
	printable-string PrintableString
		(SIZE (1..ub-pds-parameter-length)) OPTIONAL,
	teletex-string TeletexString
                (SIZE (1..ub-pds-parameter-length)) OPTIONAL }

UniquePostalAddress ::= SET {
        printable-string PrintableString
                (SIZE (1..ub-pds-parameter-length)) OPTIONAL,
        teletex-string TeletexString
                (SIZE (1..ub-pds-parameter-length)) OPTIONAL }

LocalPostalAttributes ::= SET {
        printable-string PrintableString
                (SIZE (1..ub-pds-parameter-length)) OPTIONAL,
        teletex-string TeletexString
                (SIZE (1..ub-pds-parameter-length)) OPTIONAL }

ExtendedNetworkAddress ::= CHOICE {
	e163-4-address SEQUENCE {
		number [0] NumericString (SIZE (1..ub-e163-4-number-length)),
		sub-address [1] NumericString 
			(SIZE (1..ub-e163-4-sub-address-length)) OPTIONAL },
	psap-address PresentationAddress }

TerminalType ::= INTEGER {
	telex (3),
	teletex (4),
	g3-facsimile (5),
	g4-facsimile (6),
	ia5-terminal (7),
	videotex (8) } (0..ub-integer-options)

-- Encoded Information Types

EncodedInformationTypes ::= [APPLICATION 5] SET {
	built-in-encoded-information-types [0] BuiltInEncodedInformationTypes,
	non-basic-parameters COMPONENTS OF NonBasicParameters,
	external-encoded-information-types [4]
		ExternalEncodedInformationTypes OPTIONAL }

-- Built-in Encoded Information Types

BuiltInEncodedInformationTypes ::= BIT STRING {
	undefined (0),
	telex (1),
	ia5-text (2),
	g3-facsimile (3),
	g4-class-1 (4),
	teletex (5),
	videotex (6),
	voice (7),
	sfd (8),
	mixed-mode (9) } (SIZE (1..ub-built-in-encoded-information-types))

-- Non-basic Parameters

NonBasicParameters ::= SET {
	g3-facsimile [1] G3FacsimileNonBasicParameters DEFAULT {},
	teletex [2] TeletexNonBasicParamters DEFAULT {},
	g4-class-1-and-mixed-mode [3] G4Class1AndMixedModeNonBasicParameters
						OPTIONAL }

G3FacsimileNonBasicParameters ::= BIT STRING {
	two-dimensional (8),
	fine-resolution (9),
	unlimited-length (20),
	b4-length (21),
	a3-width (22),
	b4-width (23),
	uncompressed (30) }

TeletexNonBasicParamters ::= SET {
	graphic-character-sets [0] TeletexString OPTIONAL,
	control-character-sets [1] TeletexString OPTIONAL,
	page-formats [2] OCTET STRING OPTIONAL,
	miscellaneous-terminal-capabilites [3] TeletexString OPTIONAL,
	private-use [4] OCTET STRING OPTIONAL }

G4Class1AndMixedModeNonBasicParameters ::= PresentationCapabilities

PresentationCapabilities ::= ANY

-- External Encoded Information Types

ExternalEncodedInformationTypes ::= SET
	SIZE (1..ub-encoded-information-types) OF
	ExternalEncodedInformationType

ExternalEncodedInformationType ::= OBJECT IDENTIFIER

-- Token

Token ::= SEQUENCE {
	token-type-identifier [0] OBJECT IDENTIFIER,
	token [1] ANY DEFINED BY token-type-identifier }


AssymetricToken ::= -- SIGNED -- SEQUENCE {
	signature-algorithm-identifier AlgorithmIdentifier,
	recipient-identifier RecipientIdentifier,
	time Time,
	signed-data [0] TokenData OPTIONAL,
	encryption-algorithm-identifier [1] AlgorithmIdentifier OPTIONAL,
	encryption-data [2] ENCRYPTED TokenData OPTIONAL }

TokenData ::= SEQUENCE {
	type [0] INTEGER,
	value [1] ANY DEFINED BY type }

BindTokenSignedData ::= RandomNumber

RandomNumber ::= BIT STRING

MessageTokenSignedData ::= SEQUENCE {
	content-confidentiallity-algorithm-identifier [0]
		ContentConfidentialityAlgorithmIdentifier OPTIONAL,
	content-integrity-check [1] ContentIntegrityCheck OPTIONAL,
	message-security-label [2] MessageSecurityLabel OPTIONAL,
	proof-of-delivery-request [3] ProofOfDeliveryRequest OPTIONAL,
	message-sequence-number [4] INTEGER OPTIONAL}

MessageTokenEncryptionData ::= SEQUENCE {
	content-confidentiallity-key [0] EncryptionKey OPTIONAL,
	content-integrity-check [1] ContentIntegrityCheck OPTIONAL,
	message-security-label [2] MessageSecurityLabel OPTIONAL,
	content-integrity-key [3] EncryptionKey OPTIONAL,
	message-sequence-number [4] INTEGER OPTIONAL }

EncryptionKey ::= BIT STRING

-- Security Label

SecurityLabel ::= SET {
	security-policy-identifier SecurityPolicyIdentifier OPTIONAL,
	security-classification SecurityClassification OPTIONAL,
	privacy-mark PrivacyMark OPTIONAL,
	security-categories SecurityCategories OPTIONAL }

SecurityPolicyIdentifier ::= OBJECT IDENTIFIER

SecurityClassification ::= INTEGER {
	unmarked (0),
	unclassified (1),
	restricted (2),
	confidential (3),
	secret (4),
	top-secret (5) }

PrivacyMark ::= PrintableString (SIZE (1..ub-privacy-mark-length))

SecurityCategories ::= SET SIZE (1..ub-security-categories)
		OF SecurityCategory

SecurityCategory ::= SEQUENCE {
	type [0] OBJECT IDENTIFIER,
	value [1] ANY DEFINED BY type }

END -- of MTSAbstractService
