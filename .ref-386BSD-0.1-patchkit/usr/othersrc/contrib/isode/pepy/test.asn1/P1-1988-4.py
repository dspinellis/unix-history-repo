MTAAbstractService { joint-iso-ccitt mhs (6) mts (3)
			modules (0) mTa-abstract-service (4) }

DEFINITIONS IMPLICIT TAGS ::=

BEGIN

-- Prologue

EXPORTS
	-- MTA Transfer Port Abstract Service Parameters
	MessageTransferEnvelope, ProbeTransferEnvelope,
	ReportTransferEnvelope, ReportTransferContent;

IMPORTS
	-- Abstract Service Macros
-- OBJECT causes keyword collision.
--	REFINE, OBJECT, PORT, ABSTRACT-BIND, ABSTRACT-UNBIND,
--	ABSTRACT-OPERATION, ABSTRACT-ERROR
--		FROM AbstractServiceNotation { joint-iso-ccitt mhs (6)
--			asdc (2) modules (0) notation (1) }

	-- MTS Abstract Service Parameters
	MTS, SubmissionPort, DeliverPort, AdministrationPort,
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
	ORAddressAndOptionalName
		FROM MTSAbstractService { joint-iso-ccitt mhs (6)
			mts (3) modules (0) mTs-abstract-service (3) }
	-- Object Identifiers
	objIds-objects-mta, objIds-ports-transfer
		FROM MTSObjectIdentifiers { joint-iso-ccitt mhs (6)
			mts (3) modules (0) object-identifiers (0) };


-- Port definitions & Services left out...

-- Message Transfer Envelope

MessageTransferEnvelope ::= SET {
	per-message-fields COMPONENTS OF PerMessageTransferFields,
	per-recipient-fields [2] SEQUENCE OF PerRecipientFields
					(SIZE (1..32767)) }

PerMessageTransferFields ::= SET {
	message-identifier MessageIdentifier,
	originator-identifier OriginatorIdentifier,
	original-encoded-information-types
		OriginalEncodedInformationTypes OPTIONAL,
	content-type BuiltinContentType,
	content-identifier ContentIdentifier OPTIONAL,
	priority Priority DEFAULT normal,
	per-message-indicators PerMessageIndicators DEFAULT {},
	deferred-delivery-time [0] DeferredDeliveryTime OPTIONAL,
	per-domain-bilateral-information [1] SEQUENCE OF
		PerDomainBilateralInformation OPTIONAL,
	trace-information TraceInformation,
	external-fields [3] ExternalFields DEFAULT {} }

PerRecipientTransferFields ::= SET {
	recipient-identifier RecipientIdentifier,
	originally-specified-Recipient-number 
		[0] OriginallySpecifiedRecipientNumber,
	per-recipient-indicators [1] PerRecipientIndicators,
	explicit-fields [3] ExternalFields DEFAULT {} }

-- Probe Transfer Envelope

ProbeTransferEnvelope ::= SET {
	pre-probe-fields COMPONENTS OF PerProbeTransferFields,
	per-recipient-fields [2] SEQUENCE OF PerRecipientTransferFields
			(SIZE (1..32767)) }

PerProbeTransferFields ::= SET {
	probe-identifier ProbeIdentifier,
	originator-identifier OriginatorIdentifier,
	original-encoded-information-types
		OriginalEncodedInformationTypes OPTIONAL,
	content-type BuiltinContentType,
	content-identifier ContentIdentifier OPTIONAL,
	content-length [0] ContentLength OPTIONAL,
	per-message-indicators PerMessageIndicators DEFAULT {},
	per-domain-bilateral-information [1] SEQUENCE OF
		PerDomainBilateralInformation OPTIONAL,
	trace-information TraceInformation,
	external-fields [3] ExternalFields DEFAULT {} }

-- Report Transfer Envelope

ReportTransferEnvelope ::= SET {
	report-identifier ReportIdentifier,
	report-destination-identifier ReportDestinationIdentifier,
	trace-information TraceInformation,
	external-fields [1] ExternalFields DEFAULT {} }

-- Report Transfer Content

ReportTransferContent ::= SET {
	per-report-fields COMPONENTS OF PerReportTransferFields,
	per-recipient-fields [0] SEQUENCE OF 
		PerRecipientReportTransferFields (SIZE (1..32767)) }

PerReportTransferFields ::= SET {
	subject-identifier SubjectIdentifier,
	subject-intermediate-trace-information 
		SubjectIntermediateTraceInformation OPTIONAL,
	content-type BuiltinContentType OPTIONAL,
	content-identifier ContentIdentifier OPTIONAL,
	returned-content [1] ReturnedContent OPTIONAL,
	additional-information [2] AdditionalInformation OPTIONAL,
	external-fields ExternalFields DEFAULT {} }

PerRecipientReportTransferFields ::= SET {
	actual-recipient-identifier [0] ActualRecipientIdentifier,
	originally-specified-recipient-number [1]
		OriginallySpecifiedRecipientNumber,
	per-recipient-indicators [2] PerRecipientIndicators,
	last-trace-information [3] LastTraceInformation,
	originally-intended-recipient-identifier [4]
		OriginallyIntendedRecipientIdentifier OPTIONAL,
	supplementary-information [5] SupplementaryInformation OPTIONAL,
	external-fields [6] ExternalFields DEFAULT {} }

-- Envelope & Report Content Fields

MessageIdentifier ::= MTSIdentifier

OriginatorIdentifier ::= ORAddressAndOptionalName

PerDomainBilateralInformation ::= SEQUENCE {
	country-name CountryName,
	administration-domain-name AdministrationDomainName OPTIONAL,
	private-domain-identifier [0] PrivateDomainIdentifier OPTIONAL,
	bilateral-information BilateralInformation }

BilateralInformation ::= ANY (SIZE (1..1024))

RecipientIdentifier ::= ORAddressAndOptionalName

OriginallySpecifiedRecipientNumber ::= INTEGER (SIZE (1..32767))

PerRecipientIndicators ::= BIT STRING {
	reponsibility (0),
	-- responsible 'one' not-responsible 'zero'
	originating-MTA-report (1),
	originating-MTA-non-delivery-report (2),
	-- either originating-MTA-report, or
	-- originating-MTA-non-delivery-report, or both, shall be 'one':
	-- originating-MTA-report bit 'one' requests a 'report'
	-- originating-MTA-non-delivery-report bit 'one' requests a
	-- 'non-delivery-report';
	-- both bits 'one' requests an 'audited-report'
	originator-report (3),
	originator-non-delivery-report (4),
	-- at most one bit shall be 'one':
	-- originator-report bit 'one' requests a 'report';
	-- originator-non-delivery-report bit 'one' requests a
	-- 'non-delivery-report';
	-- both bits 'zero' requests 'no-report'
	reserved-5 (5),
	reserved-6 (6),
	reserved-7 (7)
	-- reserved- bits 5 - 7 shall be 'zero' -- } (SIZE (1..16))

ProbeIdentifier ::= MTSIdentifier

ReportIdentifier ::= MTSIdentifier

ReportDestinationIdentifier ::= ORAddressAndOptionalName


SubjectIdentifier ::= CHOICE {
	MessageIdentifier,
	ProbeIdentifier }

SubjectIntermediateTraceInformation ::= TraceInformation

AdditionalInformation ::= ANY (SIZE (1..1024))

ActualRecipientIdentifier ::= ORAddressAndOptionalName

LastTraceInformation ::= SET {
	arrival-time [0] ArrivalTime,
	converted-encoded-information-types
		ConvertedEncodedInformationTypes OPTIONAL,
	report [1] Report }

OriginallyIntendedRecipientIdentifier ::= ORAddressAndOptionalName

-- EXternal fields

-- In MACRO format - hence missing

RedirectionHistory ::= SEQUENCE OF Redirection

Redirection ::= SEQUENCE {
	intended-recipient-identifier IntendedRecipientIdentifier,
	redirection-reason RedirectionReason }

IntendedRecipientIdentifier ::= ORAddressAndOptionalName

DLExpansionHistory ::= SEQUENCE OF DLExpansion

DLExpansion ::= ORAddressAndOptionalName

OriginatorAndDLExpansion ::= SEQUENCE OF ORAddressAndOptionalName

ReportingDLIdentifier ::= ORAddressAndOptionalName

InternalTraceInformation ::= SEQUENCE OF InternalTraceInformationElement

InternalTraceInformationElement ::= SEQUENCE {
	global-domain-identifier GlobalDomainIdentifier,
	mta-name MTAName,
	mta-supplied-information MTASuppliedInformation }

MTASuppliedInformation ::= SET {
	arrival-time ArrivalTime,
	routing-action [2] RoutingAction,
	attempted CHOICE {
		mta MTAName,
		domain GlobalDomainIdentifier } OPTIONAL,
	additional-actions COMPONENTS OF InternalAdditionalActions }

InternalAdditionalActions ::= AdditionalActions (WITH COMPONENTS {
	...,
	converted-encoded-information-types ABSENT })

TraceInformation ::= [APPLICATION 9] SEQUENCE OF TraceInformationElement

TraceInformationElement ::= SEQUENCE {
	global-domain-identifier GlobalDomainIdentifier,
	domain-supplied-information DomainSuppliedInformation }

DomainSuppliedInformation ::= SET {
	arrival-time ArrivalTime,
	routing-action [2] RoutingAction,
	attempted-domain GlobalDomainIdentifier OPTIONAL,
	additional-actions COMPONENTS OF AdditionalActions }
	
AdditionalActions ::= SET {
	deferred-time [1] DeferredTime OPTIONAL,
	converted-encoded-information-types
		ConvertedEncodedInformationTypes OPTIONAL,
	other-actions [3] OtherActions DEFAULT { } }

RoutingAction ::= INTEGER {
	relayed (0),
	rerouted (1) }

DeferredTime ::= Time

OtherActions ::= BITSTRING {
	redirected (0),
	expanded (1) }

END
