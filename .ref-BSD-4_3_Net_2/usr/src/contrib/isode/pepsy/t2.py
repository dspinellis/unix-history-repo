
T2 DEFINITIONS ::=


BEGIN

-- Embedded Sequences test


SECTIONS enc dec prnt

Info ::= SEQUENCE {
	a1 [0] IMPLICIT INTEGER,
	a2 [1] IMPLICIT INTEGER,
	a3 [2] IMPLICIT INTEGER,
	a4 MPDU 
	}

MPDU ::=
	SEQUENCE {
	    a-seq SEQUENCE {
		fred [0] IMPLICIT INTEGER

	    }
	}

-- Simple type optimisations of POSY/PEPY

 Bstring ::= [APPLICATION 8] BIT STRING

 Ostring ::= [31] IMPLICIT OCTET STRING

 Obj ::= [101] IMPLICIT MPDU

 Sset ::= SET

 Sseq ::= SEQUENCE

 Sany ::= ANY

 Soid ::= OBJECT IDENTIFIER

END
