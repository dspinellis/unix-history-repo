-- af.py

-- $Header: /f/osi/dsap/x500as/RCS/af.py,v 7.1 91/02/22 09:21:43 mrose Interim $
--
--
-- $Log:	af.py,v $
-- Revision 7.1  91/02/22  09:21:43  mrose
-- Interim 6.8
-- 
-- Revision 7.0  90/12/06  07:34:39  mrose
-- *** empty log message ***
-- 
-- 

--
--				  NOTICE
--
--    Acquisition, use, and distribution of this module and related
--    materials are subject to the restrictions of a license agreement.
--    Consult the Preface in the User's Manual for the full terms of
--    this agreement.
--
--


AF
	{
	joint-iso-ccitt
	ds(5)
	modules(1)
	authenticationFramework(7)
	}

DEFINITIONS ::=

PREFIXES encode decode print

BEGIN

-- EXPORTS
-- 	AlgorithmIdentifier ,
-- 	Certificate ,
-- 	Certificates ,
-- 	CertificationPath;

IMPORTS
	Name
		FROM IF
			{
			joint-iso-ccitt
			ds(5)
			modules(1)
			informationFramework(1)
			};

Version
	::=
	INTEGER {
		v1988(0)
		}

CertificateSerialNumber [[P struct certificate *]]
	::=
	INTEGER [[i serial]]

Validity [[P struct validity *]]
	::=
	SEQUENCE 
	{
	notBefore
		UTCTime [[s not_before]],
	notAfter
		UTCTime [[s not_after]]
	}

AlgorithmIdentifier [[P struct alg_id *]]
	::=
        %E{
	if ((parm->p_type == ALG_PARM_NUMERIC) &&
		(parm->asn == NULLPE))
		parm->asn = int2prim(parm->un.numeric);
        %}
	SEQUENCE
	{
	algorithm
		OBJECT IDENTIFIER [[O algorithm]],
	parameters
		ANY [[a asn]]
		    OPTIONAL
	}

SubjectPublicKeyInfo [[P struct key_info *]]
	::=
	SEQUENCE
	{
	algorithm
		AlgorithmIdentifier [[p &parm->alg]],
	subjectPublicKey
		BIT STRING [[x value $ n_bits]]
	}

CertificateToSign [[P struct certificate *]]
	::=
	SEQUENCE
	{
	version
		[0] Version [[i parm->version]]
		    DEFAULT v1988 ,
	serialNumber
		CertificateSerialNumber [[p *]] ,
	signature
		AlgorithmIdentifier [[p &parm->alg]],
	issuer
		Name [[p issuer]] ,
	validity
		Validity [[p &parm->valid]],
	subject
		Name [[p subject]] ,
	subjectPublicKeyInfo
		SubjectPublicKeyInfo [[p &parm->key]]
	}

Certificate [[P struct certificate *]]
	::=
	SEQUENCE
	{
		CertificateToSign [[p *]],
		AlgorithmIdentifier [[p &parm->sig.alg]],
		BIT STRING [[x sig.encrypted $ sig.n_bits]]
	}

CrossCertificates [[P struct certificate_list *]]
	::=
	SET OF [[T struct certificate_list * $ *]] <<next>>
		Certificate [[p cert]]

ForwardCertificationPath [[P struct certificate_list *]]
	::=
	SEQUENCE OF [[T struct certificate_list * $ *]] <<superior>>
		CrossCertificates [[p *]]

Certificates [[P struct certificate_list *]]
	::=
	SEQUENCE
	{
	certificate 
		Certificate [[p cert]],
	certificationPath
		ForwardCertificationPath [[p superior]]
		    OPTIONAL
	}

CertificatePair [[P struct certificate_list *]]
	::=
	SEQUENCE
	{
	forward
		[0] Certificate [[p cert]]
		    OPTIONAL,
	reverse
		[1] Certificate [[p reverse]]
		    OPTIONAL
	}

CertificationPath [[P struct certificate_list *]]
	::=
	SEQUENCE
	{
	userCertificate
		Certificate [[p parm->cert]],
	theCACertificates
		SEQUENCE OF [[ T struct certificate_list * $ superior]] << next >>
			CertificatePair [[p *]] OPTIONAL
	}

RevokedCertificatesToSign [[P struct revoked_certificate *]]
	::=
	SEQUENCE OF [[ T struct revoked_certificate * $ * ]] <<next>>
	SEQUENCE [[T struct revoked_certificate * $ *]]
	{
	signature
		AlgorithmIdentifier [[p &parm->alg]],
	issuer
		Name [[p subject]] ,
	subject
		CertificateSerialNumber [[p *]],
	revokationDate
		UTCTime [[s revocation_date]]
	} 

RevokedCertificates [[P struct revocation_list *]]
	::=
	SEQUENCE 
	{
	RevokedCertificatesToSign [[p revoked]],
	AlgorithmIdentifier [[p &parm->sig2.alg]],
	BIT STRING [[x sig2.encrypted $ sig2.n_bits]]
	} 
	
CertificateListToSign [[P struct revocation_list *]]
	::=
	SEQUENCE
	{
	%E{
	   if (parm->revoked)
	      BITSET (parm->test,1);
	   else
	      BITCLR (parm->test,1);
	%}
	signature
		AlgorithmIdentifier [[p &parm->alg]],
	issuer
		Name [[p issuer]] ,
	lastUpdate
		UTCTime [[s last_update]],
	revokedCertificates
		RevokedCertificates [[p *]]
		    OPTIONAL <<parm->test $ 1>>
	}

CertificateList [[P struct revocation_list *]]
	::=
	SEQUENCE
	{
		CertificateListToSign [[p *]],
		AlgorithmIdentifier [[p &parm->sig.alg]],
		BIT STRING [[x sig.encrypted $ sig.n_bits]]
	}

END
