-- IPMS message content definition

IPMSMessageContent { joint-iso-ccitt
	mhs(6) ipms(1) modules(0) message-context(5) }

DEFINITIONS IMPLICIT TAGS ::=
BEGIN

-- Prologue

EXPORTS
	-- Content-type
	content-type, p2,
	-- Content
	Content;

IMPORTS
	-- Information object
	InformationObject
		FROM IMPSInformationObjects { joint-iso-ccitt
			mhs(6) ipms(1) modules(0) information-objects(2)}
	-- MT AS ascpects
	ContentType
		FROM MTSAbstrctService {joint-iso-ccitt
			mhs(6) ipms(1) modules(0) };

-- Mesage contnet-type

ContentType ::= INTEGER

content-type ContentType ::= 2

p2 ContentType ::= content-type

-- Message content

Content ::= InformationObject

END -- of IPMSMessageContent
