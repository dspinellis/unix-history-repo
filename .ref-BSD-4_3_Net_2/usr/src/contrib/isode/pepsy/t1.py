T1 { joint-iso-ccitt rubbish(999) modid(1) } DEFINITIONS ::=


BEGIN

IMPORTS
	+ Info,
	+ Sset,
	+ Sseq,
	- Sany,
	+ Soid
		FROM T2
	Pepy-refs,
	Single-type,
	Repeated,
	Defined,
	CodeTest,
	AllSimples,
	UAction
		FROM T3;

-- Embedded Sequences test


    -- Test of Embedded Strings
Emb-Strings ::= SEQUENCE {
	atest INTEGER,
	btest OCTET STRING,
	ctest BIT STRING,
	big-test Strings,
	emb-test SEQUENCE {
	    em-int INTEGER,
	    em-oct OCTET STRING,
	    em-bit BIT STRING
	}
    }


Strings ::= SEQUENCE { -- Test Octet strings, bit strings and character
		       -- Strings in general
	ostring OCTET STRING,
	bstring BIT STRING,
	nstring NumericString,
	pstring PrintableString,
	tstring TeletexString,
	t61string T61String,
	vstring	VideotexString,
	vis-string VisibleString,
	i646string ISO646String,
	ia5string IA5String,
	graphstring GraphicString,
	genstring GeneralString
	}

Embedded ::= SEQUENCE {
	anMPDU MPDU,
	ei INTEGER
	}

MPDU ::=
	SEQUENCE {
	    a-seq SEQUENCE {
		fred [0] IMPLICIT INTEGER,

		    george BOOLEAN
	    }
	}

ServiceMPDU ::=
	SET {
	    a-set SET {
		    a-data [1] IMPLICIT BIT STRING,

		a-more-data [2] IMPLICIT INTEGER
	    },
	    b-set SET {
		    b-data [1] IMPLICIT BIT STRING,

		b-more-data [2] IMPLICIT INTEGER
	    }
	}
-- Implicit tags

Impl-Tags ::= SEQUENCE {
	i-impl [1] IMPLICIT INTEGER,
	o-impl [2] IMPLICIT OCTET STRING,
	b-impl [3] IMPLICIT BIT STRING,
	f-impl [4] IMPLICIT BOOLEAN,
	obj-impl [5] IMPLICIT Emb-Strings,

    -- Test of Embedded Strings
	i-emb-test [APPLICATION 88] IMPLICIT SEQUENCE {
	    i-em-int INTEGER,
	    i-em-oct OCTET STRING,
	    i-em-bit BIT STRING
	}
    }

-- Explicit tags

Expl-Tags ::= SEQUENCE {
	i-expl [1] INTEGER,
	o-expl [2] OCTET STRING,
	b-expl [3] BIT STRING,
	f-expl [4] BOOLEAN,
	obj-expl [5] Emb-Strings,

    -- Test of Embedded Strings
	i-exp-test [APPLICATION 88] SEQUENCE {
	    i-ex-int INTEGER,
	    i-ex-oct OCTET STRING,
	    i-ex-bit BIT STRING
	}
    }

  -- Test of Sequence of
Seqof-Test ::= SEQUENCE {
	sqof-test1 SEQUENCE OF Expl-Tags,
	stof-test1 SET OF Expl-Tags,
	i-test1 INTEGER,
	sqof-test2 SEQUENCE OF SEQUENCE {
	    sqof-in Expl-Tags,
	    sqof-i INTEGER,
	    sqof-o OCTET STRING
	},
	stof-test2 SET OF SEQUENCE {
	    stof-in Expl-Tags,
	    stof-i INTEGER,
	    stof-o OCTET STRING
	},
	i-test2 INTEGER
    }


   -- Test data for the Choice type
Choice-Test ::= SET {
	c1 CHOICE {
	i-c1 [1] IMPLICIT INTEGER,
	o-c1 [2] IMPLICIT OCTET STRING,
	b-c1 [3] IMPLICIT BIT STRING,
	f-c1 [4] IMPLICIT BOOLEAN,
	obj-c1 [5] IMPLICIT Emb-Strings
	},
	c2 CHOICE {
	i-c2 [6] INTEGER,
	o-c2 [7] OCTET STRING,
	b-c2 [8] BIT STRING,
	f-c2 [9] BOOLEAN,
	obj-c2 [10] Emb-Strings
	},
	c3 CHOICE {
	    seq-c3 SEQUENCE {
		seq-c3-in Expl-Tags,
		seq-c3-i INTEGER,
		seq-c3-o OCTET STRING
	    },
	    set-c3 SET {
		set-c3-in Expl-Tags,
		set-c3-i INTEGER,
		set-c3-o OCTET STRING
	    },
	    i-c3 INTEGER
	},
	c4 [12] IMPLICIT SEQUENCE {
	    c4-choice CHOICE {
		c4-i INTEGER,
		c4-obj [2] IMPLICIT Expl-Tags,
		c4-subch CHOICE {
		    sc-a-i [0] IMPLICIT INTEGER,
		    sc-b-i [1] IMPLICIT INTEGER
		}
	    }
	}


    }

  -- Test of Optional fields

Opt-Strings ::= SEQUENCE {
	a-opt INTEGER OPTIONAL,
	b-opt OCTET STRING,
	c-opt BIT STRING OPTIONAL,
	d-opt BOOLEAN OPTIONAL,
	e-opt NULL OPTIONAL,
	big-opt Strings OPTIONAL,
	emb-opt SEQUENCE {
	    oem-int INTEGER OPTIONAL,
	    oem-oct OCTET STRING OPTIONAL,
	    oem-bit BIT STRING OPTIONAL
	}	OPTIONAL,
	st-opt SET {
	    st-int0 [0] IMPLICIT INTEGER OPTIONAL,
	    st-int1 [1] IMPLICIT INTEGER OPTIONAL,
	    st-int2 [2] IMPLICIT INTEGER OPTIONAL
	}	OPTIONAL,
	obj-opt [APPLICATION 1] IMPLICIT  MPDU OPTIONAL,
	etag-opt [APPLICATION 2] INTEGER OPTIONAL,
	ch-opt CHOICE {
	    ch-1 [10] IMPLICIT INTEGER,
	    ch-2 [20] IMPLICIT INTEGER
	} OPTIONAL
    }

  -- Test of Default fields

Def-Strings ::= SEQUENCE {
	a-def INTEGER  { a-def-0 (0), a-def-1(1) } DEFAULT a-def-0,
	-- posy bug does not allow handling of 'xxx'b or 'xx'h notation
	-- so we don't use proper posy notation
	-- b-def OCTET STRING DEFAULT 'ff01ab20'h,
	b-def PrintableString   DEFAULT "hello, world",
	c-def BIT STRING  { c-def-one(3), c-def-two (1)}
		DEFAULT { c-def-one },
	okay BOOLEAN DEFAULT TRUE,
	e-def NULL DEFAULT NULL,
	big-def Strings,
	emb-def SEQUENCE {
	    colour INTEGER { red(1), green(2), yellow(3) } DEFAULT green,
	    oem-oct OCTET STRING OPTIONAL,
	    version BIT STRING { basic(0), patch1(1), patch2(2) }
		DEFAULT { basic }
	}    DEFAULT { red, { basic } },
	st-def SET {
	    wine [0] IMPLICIT INTEGER { claret(1), burgundy(2), moselle(3) }
		DEFAULT claret,
	    beer [1] IMPLICIT INTEGER { vb(0), fosters(1), hieneken(2) }
		DEFAULT vb,
	    spirit [2] IMPLICIT INTEGER { brandy(0), vodka(1), wisky(2) }
		DEFAULT vodka
	}	DEFAULT { vb, vodka }
    }

-- External References
--
E-ref	::= SEQUENCE {
    a-ref T2.Info,
    b-ref [APPLICATION 33] IMPLICIT T2.Info,
    c-ref [1] T1.Choice-Test,
    d-ref [2] T2.Info OPTIONAL,
    e-ref [APPLICATION 33] IMPLICIT T2.Info OPTIONAL
    }


-- Simple type optimisations of POSY/PEPY

 Bstring ::= [APPLICATION 8] BIT STRING

 Ostring ::= [31] IMPLICIT OCTET STRING

 Obj ::= [101] IMPLICIT MPDU

 Sset ::= SET

 Sseq ::= SEQUENCE

 Sany ::= ANY

 Soid ::= OBJECT IDENTIFIER


Optimised ::= SEQUENCE {
    o1 Bstring,
    o2 Ostring,
    o3 Obj,
    o4 [APPLICATION 3] IMPLICIT Sset OPTIONAL,
    [APPLICATION 21] SET {
	o5 [PRIVATE 9] IMPLICIT Sseq,
	o6 [PRIVATE 33] IMPLICIT Sany,
	o7 [PRIVATE 8] IMPLICIT Soid
    }
}

-- Use of External Simple types
Ext-Opt ::= SEQUENCE {
    o1 T2.Bstring,
    o2 T2.Ostring,
    o3 T2.Obj,
    o4 [PRIVATE 38] IMPLICIT T2.Sset OPTIONAL,
    [APPLICATION 21] SET {
	o5 [PRIVATE 1] IMPLICIT T2.Sseq,
	o6 [PRIVATE 2] IMPLICIT T2.Sany,
	o7 [PRIVATE 3] IMPLICIT T2.Soid
    }
}

-- External type and ANY usage

Ext-typ ::= SEQUENCE {
     ext EXTERNAL,
     a-ny	[APPLICATION 32] ANY,
     ext-impl	[PRIVATE 6] IMPLICIT EXTERNAL,
     any-impl	[APPLICATION 7] ANY,
     ext-expl	[PRIVATE 9] EXTERNAL,
     any-expl	[APPLICATION 10] ANY
   }

SExt ::= [PRIVATE 99] IMPLICIT EXTERNAL

-- check of posy's %[ name $ head %] stuff
Names ::=
	SEQUENCE {
	    SEQUENCE %[ seq_str $ ptr %] {
		fred [0] IMPLICIT INTEGER
	    }
	}

-- Test explicit tags as the first field of an object
Etags ::=
	[APPLICATION 3]
	   CHOICE {
		aE [0] IMPLICIT INTEGER,
		bE [1] IMPLICIT INTEGER
	    }

Stest ::=
    [APPLICATION 4] SET {
	    st1 Sint DEFAULT 33,
	    st2 Soctet DEFAULT "goodbye, world"
	}

Sint ::= INTEGER

Soctet ::= OCTET STRING

Simpl-test ::= SEQUENCE { si-t [23] IMPLICIT Sbstring }

Sbstring ::= BIT STRING { bit0(0), bit1(1), bit2(2) }

SStest ::= [101] IMPLICIT Simpl-test

Enum-type ::= ENUMERATED { pork(0), beef(1), chicken(3), lamb(-1) }

T-enum ::= SEQUENCE {
		ae1 Enum-type,
		ae2 [12] Enum-type,
		ae3 [13] IMPLICIT Enum-type,
		ae4 [15] IMPLICIT Enum-type DEFAULT chicken,
		ae5 Enum-type OPTIONAL
	    }
Real ::= REAL

T-real ::= SEQUENCE {
		r1 Real,
		r2 [99] Real,
		r3 [101] IMPLICIT Real,
		r4 [501] IMPLICIT Real DEFAULT { 31415962, 10, -7 },
		r5 Real OPTIONAL
	    }

T-pepy ::= T3.Pepy-refs

T3-Single ::= T3.Single-type

T3-Repeat ::= T3.Repeated

T3-Defined ::= T3.Defined

T3-CodeTest ::= T3.CodeTest

T3-AllSimples ::= T3.AllSimples

T3-UAction ::= T3.UAction

END
