-- bigpepytest.py - test out most of the pepy constructs

-- $Header: /f/osi/pepy/RCS/bigpepytest.py,v 7.1 91/02/22 09:34:49 mrose Interim $
--
--
-- $Log:	bigpepytest.py,v $
-- Revision 7.1  91/02/22  09:34:49  mrose
-- Interim 6.8
-- 
-- Revision 7.0  89/11/23  22:11:37  mrose
-- Release 6.0
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


BigTest DEFINITIONS ::=

%{
#ifndef lint
static char *rcsid = "$Header: /f/osi/pepy/RCS/bigpepytest.py,v 7.1 91/02/22 09:34:49 mrose Interim $";
#endif

#include <stdio.h>


static int  count;

static char *myname = "bigpepytest";


void	adios ();

/*  */

/* ARGSUSED */

main (argc, argv, envp)
int	argc;
char  **argv,
      **envp;
{
    PE	    pe;

    myname = argv[0];

    if (encoder_BigTest_Test1 (&pe, 1, NULL, NULLCP, NULLCP) == NOTOK)
	adios (NULLCP, "encoder fails");

    if (printer_BigTest_Test1 (pe, 1, NULLIP, NULLVP, NULLCP) == NOTOK)
	adios (NULLCP, "printer fails");

    if (decoder_BigTest_Test1 (pe, 1, NULLIP, NULLVP, NULLCP) == NOTOK)
	adios (NULLCP, "decoder fails");

    exit (0);			/* NOTREACHED */
}
%}

BEGIN


ENCODER	encoder

Test1 %{ int fd = rnd (2); %} ::=
	SEQUENCE {
	    boolean
		BOOLEAN
		[[b rnd(2)]],

	    taggedBoolean
		[0] BOOLEAN
		[[b rnd (2)]],

	    implicitTaggedBoolean
		[PRIVATE 0] IMPLICIT BOOLEAN
		[[b rnd(2)]],

	    integer
		INTEGER
		[[i rnd(10000)]],

	    taggedInteger
		[APPLICATION 1] INTEGER
		[[i rnd(10000)]],

	    implicitTaggedInteger
		[UNIVERSAL 1] IMPLICIT INTEGER
		[[i rnd(10000)]],

	    integerOneValue
		INTEGER
		[[i 1]] { nn(1) },

	    taggedIntegerOneValue
		[PRIVATE 2] INTEGER
		[[i 1]] { nn(1) },

	    integerManyValues
		INTEGER
		[[i rnd(3) + 1]]
		{ nn1(1), nn2(2), nn3(3) },

	    implicitTaggedIntegerManyValues
		[2] IMPLICIT INTEGER
		[[i rnd(3) + 1]]
		{ nn1(1), nn2(2), nn3(3) },

	    bitstring
		%{
		   int i;
		   $$ = pe_alloc(PE_CLASS_UNIV, PE_FORM_PRIM, PE_PRIM_BITS);
		   $$ = prim2bit($$);
		   for(i = 0; i < 32; i++)
			   if( rnd(2) )
				   (void) bit_on($$, i);
		%}
		BITSTRING,

	    taggedBitstring [UNIVERSAL 3]
		%{
		   int i;
		   $$ = pe_alloc(PE_CLASS_UNIV, PE_FORM_PRIM, PE_PRIM_BITS);
		   $$ = prim2bit($$);
		   for(i = 0; i < 32; i++)
			   if( rnd(2) && rnd(3) )
				   (void) bit_on($$, i);
		%}
		BITSTRING,

	    bitstring2
		BIT STRING
		[[x int2strb(0177, 8) $ 8]],

	    taggedBitstring2
		[4] BIT STRING
		[[x int2strb(0177, 8) $ 8]],

	    implicitTaggedBitstring2
		[PRIVATE 4] IMPLICIT BIT STRING
		[[x int2strb(0xffff, 16) $ 16]],

	    bitstringValue
		BIT STRING
		[[x int2strb(1, 1) $ 1]]
		{ nn1(1) },

	    taggedBitstringValue
		[APPLICATION 5] BIT STRING
		[[x int2strb(1, 1) $ 1]]
		{ nn1(1) },

	    bitstringValues
		BIT STRING
		[[x int2strb(03, 3) $ 3]]
		{ nn1(1), nn2(2), nn3(3) },

	    taggedBitstringValues
		[PRIVATE 6] BIT STRING
		[[x int2strb(03, 3) $ 3]]
		{ nn1(1), nn2(2), nn3(3) },

	    implicitTaggedBitstringValues
		[APPLICATION 6] IMPLICIT BIT STRING
		[[x int2strb(03, 3) $ 3]]
		{ nn1(1), nn2(2), nn3(3) },

	    octetstring
		OCTET STRING
		[[s "hello world"]],

	    taggedOctetstring
		[UNIVERSAL 7] OCTET STRING
		[[s "hello world"]],

	    octetstring2
		OCTETSTRING
		[[s "goodbye world"]],

	    implicitTaggedOctetstring2
		[PRIVATE 7] IMPLICIT OCTETSTRING
		[[o "goodbye world" $ 8]],

	    null
		NULL,

	    taggedNull
		[8] NULL,

	    implicitTaggedNull
		[PRIVATE 8] IMPLICIT NULL,

	    sequence
		SEQUENCE,

	    taggedSequence
		[APPLICATION 9] SEQUENCE,

	    implicitTaggedSequence
		[PRIVATE 9] SEQUENCE,

	    sequenceOf
		SEQUENCE OF
		    <<count = rnd(10); count >= 0; count-->>
		    SequenceA,

	    taggedSequenceOf
		[PRIVATE 10] SEQUENCE OF
		    <<count = rnd(10); count >= 0; count-->>
		    SequenceA,

	    sequenceOf2
		SEQUENCEOF
		    <<count = rnd(3); count >= 0; count-->>
		    SequenceB,

	    implicitTaggedSequenceOf2
		[10] IMPLICIT SEQUENCEOF
		    <<count = rnd(3); count >= 0; count-->>
		    SequenceA,

	    set
		SET,

	    taggedSet
		[UNIVERSAL 11] SET,

	    implicitTaggedSet
		[PRIVATE 11] IMPLICIT SET,

	    setOf
		SET OF
		    <<count = rnd(10); count >= 0; count -->>
		    SetA,

	    taggedSetOf
		[12] SET OF
		    <<count = rnd(10); count >= 0; count -->>
		    SetA,

	    setOf2
		SETOF
		    <<count = rnd(4); count >= 0; count -->>
		    SetB,

	    implicitTaggedSetOf2
		[PRIVATE 12] IMPLICIT SETOF
		    <<count = rnd(4); count >= 0; count -->>
		    SetA,

	    setValues
		SET { SequenceA, SequenceB, SetA, SetB },

	    taggedSetValues
		[APPLICATION 13] SET { SequenceA, SequenceB, SetA, SetB },

	    implicitTaggedSetValues
		[13] IMPLICIT SET { SequenceA, SequenceB, SetA, SetB },

	    choice
		CHOICE << rnd(2) + 1 >> {
		    [1]
			SequenceA,

		    [2]
			SequenceB,

		    [3]
			SetA,

		    [4]
			SetB
		},

	    taggedChoice
		[PRIVATE 14] CHOICE << rnd(2) + 1 >> {
		    [1]
			SequenceA,

		    [2]
			SequenceB,

		    [3]
			SetA,

		    [4]
			SetB
		},

	    bounded
		objectname < ObjectA,

	    taggedBounded
		[UNIVERSAL 15] objectname < ObjectA,

	    any
		ANY
		[[a pe_alloc (PE_CLASS_UNIV, PE_FORM_PRIM, PE_PRIM_NULL)]],

	    taggedAny [PRIVATE 15]
		ANY
		[[a pe_alloc (PE_CLASS_UNIV, PE_FORM_PRIM, PE_PRIM_NULL)]],

	    oid
		%{
		    OID oid;
		    register int i;

		    oid = (OID) calloc (1, sizeof(OIDentifier));
		    oid->oid_nelem = rnd(6)+2;
		    oid->oid_elements = (unsigned int *)
				calloc((unsigned) oid->oid_nelem,
					    sizeof(unsigned int));
		    for(i = 0; i < oid->oid_nelem; i++)
			oid->oid_elements[i] = rnd(10);
		    $$ = oid;
		%}
		OBJECT IDENTIFIER,

	    taggedOid [16]
		%{
		    OID oid;
		    register int i;

		    oid = (OID) calloc (1, sizeof(OIDentifier));
		    oid->oid_nelem = rnd(6)+2;
		    oid->oid_elements = (unsigned int *)
				calloc((unsigned) oid->oid_nelem,
					    sizeof(unsigned int));
		    for(i = 0; i < oid->oid_nelem; i++)
			oid->oid_elements[i] = rnd(10);
		    $$ = oid;
		%}
		OBJECT IDENTIFIER,

	    oid2
		OBJECT IDENTIFIER
		[[O str2oid ("1.2.3.4.5.6.7.8.9")]],

	    implicitTaggedOid2
		[APPLICATION 17] IMPLICIT OBJECT IDENTIFIER
		[[O str2oid ("1.2.3.4.5.6.7.8.9")]],

	    funnyDefault
		[18] IMPLICIT INTEGER
		[[i fd]]
		DEFAULT 0 << fd != 0 >>
	}

SequenceA ::=
	SEQUENCE {
	    seqAinteger
		INTEGER
		[[i rnd(50)]],

	    seqAbool
		BOOLEAN
		[[b rnd(2)]]
	}

SequenceB ::=
	[PRIVATE 100] SEQUENCE {
	    seqBinteger
		INTEGER
		[[i rnd(75)]]
	}

SetA ::=
	SET {
	    setAinteger
		INTEGER
		[[i rnd(20000)]],

	    setAbool
	    	BOOLEAN
		[[b rnd(2)]]
	}

SetB ::=
	[PRIVATE 101] SET {
	    setBinteger
		INTEGER
		[[i rnd(3000)]]
	}

ObjectA ::=
	BOOLEAN
	[[b rnd(2)]]


SECTIONS none decoder printer

Test1 ::=
	SEQUENCE {
	    boolean
		BOOLEAN,

	    taggedBoolean
		[0] BOOLEAN,

	    implicitTaggedBoolean
		[PRIVATE 0] IMPLICIT BOOLEAN,

	    integer
		INTEGER,

	    taggedInteger
		[APPLICATION 1] INTEGER,

	    implicitTaggedInteger
		[UNIVERSAL 1] IMPLICIT INTEGER,

	    integerOneValue
		INTEGER { nn(1) },

	    taggedIntegerOneValue
		[PRIVATE 2] INTEGER { nn(1) },

	    integerManyValues
		INTEGER { nn1(1), nn2(2), nn3(3) },

	    implicitTaggedIntegerManyValues
		[2] IMPLICIT INTEGER { nn1(1), nn2(2), nn3(3) },

	    bitstring
		BITSTRING,

	    taggedBitString
		[UNIVERSAL 3] BITSTRING,

	    bitstring2
		BIT STRING,

	    taggedBitstring2
		[4] BIT STRING,

	    implictTaggedBitstring2
		[PRIVATE 4] IMPLICIT BIT STRING,

	    bitstringValue
		BIT STRING { nn1(1) },

	    taggedBitstringValue
		[APPLICATION 5] BIT STRING { nn1(1) },

	    bitstringValues
		BIT STRING { nn1(1), nn2(2), nn3(3) },

	    taggedBitstringValues
		[PRIVATE 6] BIT STRING { nn1(1), nn2(2), nn3(3) },

	    implicitTaggedBitstringValues
		[APPLICATION 6] IMPLICIT BIT STRING
		{ nn1(1), nn2(2), nn3(3) },

	    octetstring
		OCTET STRING,

	    taggedOctetstring
		[UNIVERSAL 7] OCTET STRING,

	    octetstring2
		OCTETSTRING,

	    implicitTaggedOctetstring2
		[PRIVATE 7] IMPLICIT OCTETSTRING,

	    null
		NULL,

	    taggedNull
		[8] NULL,

	    implicitTaggedNull
		[PRIVATE 8] IMPLICIT NULL,

	    sequence
		SEQUENCE,

	    taggedSequence
		[APPLICATION 9] SEQUENCE,

	    implicitTaggedSequence
		[PRIVATE 9] IMPLICIT SEQUENCE,

	    sequenceOf
		SEQUENCE OF
		    SequenceA,

	    taggedSequenceOf
		[PRIVATE 10] SEQUENCE OF
		    SequenceA,

	    sequenceOf2
		SEQUENCEOF
		    SequenceB,

	    implicitTaggedSequenceOf2
		[10] IMPLICIT SEQUENCEOF
		    SequenceA,

	    set
		SET,

	    taggedSet
		[UNIVERSAL 11] SET,

	    implicitTaggedSet
		[PRIVATE 11] IMPLICIT SET,

	    setOf
		SET OF
		    SetA,

	    taggedSetOf
		[12] SET OF
		    SetA,

	    setOf2
		SETOF
		    SetB,

	    implicitTaggedSetOf2
		[PRIVATE 12] IMPLICIT SETOF
		    SetA,

	    setValues
		SET { SequenceA, SequenceB, SetA, SetB },

	    taggedSetValues
		[APPLICATION 13] SET { SequenceA, SequenceB, SetA, SetB },

	    implicitTaggedSetValues
		[13] IMPLICIT SET { SequenceA, SequenceB, SetA, SetB },

	    choice
		CHOICE {
		    [1]
			SequenceA,

		    [2]
			SequenceB,

		    [3]
			SetA,

		    [4]
			SetB
		},

	    taggedChoice
		[PRIVATE 14] CHOICE {
		    [1]
			SequenceA,

		    [2]
			SequenceB,

		    [3]
			SetA,

		    [4]
			SetB
		},

	    bounded
		objectname < ObjectA,

	    taggedBounded
		[UNIVERSAL 15] objectname < ObjectA,

	    any
		ANY,

	    taggedAny
		[PRIVATE 15] ANY,

	    oid
		OBJECT IDENTIFIER,

	    taggedOid
		[16] OBJECT IDENTIFIER,

	    oid2
		OBJECT IDENTIFIER,

	    implicitTaggedOid2
		[APPLICATION 17] IMPLICIT OBJECT IDENTIFIER,

	    funnyDefault
		[18] IMPLICIT INTEGER
		%{ if ($$ == 0) advise (NULLCP, "funny default!"); %}
		DEFAULT 0
	}

SequenceA ::=
	SEQUENCE {
	    seqAinteger
		INTEGER,

	    seqAbool
		BOOLEAN
	}

SequenceB ::=
	[PRIVATE 100] SEQUENCE {
	    seqBinteger
		INTEGER
	}

SetA ::=
	SET {
	    setAinteger
		INTEGER,

	    setAbool
		BOOLEAN
	}

SetB ::=
	[PRIVATE 101] SET {
	    setBinteger
		INTEGER
	}

ObjectA ::=
	BOOLEAN


END

%{

/* 	MISC */

#ifdef BSD42
#define SRAND(s)	srandom ((int) (s))
#define RAND		random

long	random ();
#else
#define SRAND		srand
#define	RAND		rand

int	rand ();
#endif

long	time ();


static int rnd(n)
int	n;
{
    static int	init = 0;

    if (!init) {
	SRAND (time ((long *) 0));
	init = 1;
    }
    return (RAND () % n);
}

/* 	ERRORS */

#include <varargs.h>


#ifndef	lint
void	_advise ();


static void  adios (va_alist)
va_dcl
{
    va_list ap;

    va_start (ap);

    _advise (ap);

    va_end (ap);

    _exit (1);
}
#else
/* VARARGS */

static void  adios (what, fmt)
char   *what,
       *fmt;
{
    adios (what, fmt);
}
#endif


#ifndef	lint
static void  advise (va_alist)
va_dcl
{
    va_list ap;

    va_start (ap);

    _advise (ap);

    va_end (ap);
}


static void  _advise (ap)
va_list	ap;
{
    char    buffer[BUFSIZ];

    asprintf (buffer, ap);

    (void) fflush (stdout);

    fprintf (stderr, "%s: ", myname);
    (void) fputs (buffer, stderr);
    (void) fputc ('\n', stderr);

    (void) fflush (stderr);
}
#else
/* VARARGS */

static void  advise (what, fmt)
char   *what,
       *fmt;
{
    advise (what, fmt);
}
#endif

%}
