
T3 DEFINITIONS ::=

%{
/*
 * this stuff is to test the verbatim actions at the start of the 
 * type definitions
 */
%}

BEGIN



Pepy-refs [[ P struct pepy_refs * ]] ::= SEQUENCE {
    [1] IMPLICIT INTEGER [[ i parm->t_int ]],
    [2] IMPLICIT ENUMERATED [[ i parm->t_enum ]] {
	orange(1), apple(2), pear(3), bannana(4) },

    [3] IMPLICIT OCTET STRING [[ q parm->t_qbuf ]],
    [4] IMPLICIT OCTET STRING [[ s parm->t_string ]],
    [5] IMPLICIT OCTET STRING [[ o parm->t_ostring $ parm->t_olen ]],
    [6] IMPLICIT BIT STRING [[ t parm->t_pe ]],
    [7] IMPLICIT BIT STRING [[ x parm->t_bstring $ t_blen ]],
    [8] IMPLICIT BOOLEAN [[ b parm->t_bool ]],
    [9] IMPLICIT OBJECT IDENTIFIER [[ O parm->t_oid ]],
    [10] IMPLICIT REAL [[ r parm->t_real ]],
    [11] ANY [[ a parm->t_any ]],
    [12] Def-types [[ p parm->t_def ]],
    [13] Opt-types [[ p parm->t_opt ]] OPTIONAL

    }

-- Optional and Default test cases

Def-types [[ P struct pepy_refs1 * ]] ::= SEQUENCE {
    [1] INTEGER [[ i parm->t_int ]] DEFAULT 2,
    [2] IMPLICIT INTEGER [[ i parm->t_int1 ]] DEFAULT 2,
    [3] IMPLICIT ENUMERATED [[ i parm->t_enum ]]
	{ orange(1), apple(2), pear(3), bannana(4) } DEFAULT pear,

    [4] ENUMERATED [[ i parm->t_enum1 ]] {
	orange(1), apple(2), pear(3), bannana(4) } DEFAULT pear,
    [5] IMPLICIT OCTET STRING [[ q parm->t_qbuf ]]
			DEFAULT '536f6d65206973206f757420746865726521'H,
    [6] OCTET STRING [[ q parm->t_qbuf1 ]]
			DEFAULT '536f6d65206973206f757420746865726521'H,
    [7] IMPLICIT OCTET STRING [[ s parm->t_string ]] DEFAULT '003132'h,
    [8] OCTET STRING [[ s parm->t_string1 ]] DEFAULT '003132'h,
    [9] IMPLICIT OCTET STRING [[ o parm->t_ostring $ parm->t_olen ]]
			DEFAULT 'ff01ab20'h,
    [10] OCTET STRING [[ o parm->t_ostring1 $ parm->t_olen1 ]]
			DEFAULT 'ff01ab20'h,
    [11] IMPLICIT BIT STRING [[ t parm->t_pe ]]
	DEFAULT '00010010001100010001101000110111001010101'B,
    [12] BIT STRING [[ t parm->t_pe1 ]]
	DEFAULT '00010010001100010001101000110111001010101'B,
    [13] IMPLICIT BIT STRING [[ x parm->t_bstring $ t_blen ]]
	DEFAULT '00000000001100010011001000110011'B,
    [14] BIT STRING [[ x parm->t_bstring1 $ t_blen1 ]]
	DEFAULT '00000000001100010011001000110011'B,
    [15] IMPLICIT BOOLEAN [[ b parm->t_bool ]] DEFAULT TRUE,
    [16] BOOLEAN [[ b parm->t_bool ]] DEFAULT TRUE,
    [17] IMPLICIT OBJECT IDENTIFIER [[ O parm->t_oid ]], -- one day  DEFAULT 1.17.42,
    [18] OBJECT IDENTIFIER [[ O parm->t_oid1 ]] OPTIONAL, -- one day  DEFAULT 1.17.42,
    [19] IMPLICIT REAL [[ r parm->t_real ]] DEFAULT { 306998, 10, -5 },
    [20] REAL [[ r parm->t_real1 ]] DEFAULT { 306998, 10, -5 },
    [21] ANY [[ a parm->t_any ]] OPTIONAL

    }

Opt-types [[ P struct pepy_refs1 * ]] ::= SEQUENCE {
    [1] IMPLICIT INTEGER [[ i parm->t_int ]] OPTIONAL << opt_set $ OPT_INT1 >>,
    [2] INTEGER [[ i parm->t_int1 ]] OPTIONAL << opt_set $ OPT_INT2 >>,
    [3] IMPLICIT ENUMERATED [[ i parm->t_enum ]] {
	orange(1), apple(2), pear(3), bannana(4) } OPTIONAL << opt_set $ OPT_ENUM1 >>,
    [4] ENUMERATED [[ i parm->t_enum1 ]] {
	orange(1), apple(2), pear(3), bannana(4) } OPTIONAL << opt_set $ OPT_ENUM2 >>,

    [5] IMPLICIT OCTET STRING [[ q parm->t_qbuf ]] OPTIONAL,
    [6] OCTET STRING [[ q parm->t_qbuf1 ]] OPTIONAL,
    [7] IMPLICIT OCTET STRING [[ s parm->t_string ]] OPTIONAL,
    [8] OCTET STRING [[ s parm->t_string1 ]] OPTIONAL,
    [9] IMPLICIT OCTET STRING [[ o parm->t_ostring $ parm->t_olen ]]
			OPTIONAL,
    [10] OCTET STRING [[ o parm->t_ostring1 $ parm->t_olen1 ]]
			OPTIONAL,
    [11] IMPLICIT BIT STRING [[ t parm->t_pe ]] OPTIONAL,
    [12] BIT STRING [[ t parm->t_pe1 ]] OPTIONAL,
    [13] IMPLICIT BIT STRING [[ x parm->t_bstring $ t_blen ]] OPTIONAL,
    [14] BIT STRING [[ x parm->t_bstring1 $ t_blen1 ]] OPTIONAL,
    [15] IMPLICIT BOOLEAN [[ b parm->t_bool ]] OPTIONAL << opt_set $ OPT_BOOL1>>,
    [16] BOOLEAN [[ b parm->t_bool1 ]] OPTIONAL << opt_set $ OPT_BOOL2>>,
    [17] IMPLICIT OBJECT IDENTIFIER [[ O parm->t_oid ]], -- one day  OPTIONAL,
    [18] IMPLICIT REAL [[ r parm->t_real ]] OPTIONAL << opt_set $ OPT_REAL1 >>,
    [19] REAL [[ r parm->t_real1 ]] OPTIONAL << opt_set $ OPT_REAL2 >>,
    [20] ANY [[ a parm->t_any ]]

    }

    Single-type [[ P struct pepy_refs1 * ]] ::= SEQUENCE {
	a SEQUENCE [[ T struct pepy_refs1 * $ * ]] {
		[1] IMPLICIT INTEGER [[ i parm->t_int ]] OPTIONAL << opt_set $ OPT_INT1 >>
	    },
	b SEQUENCE [[ T struct pepy_refs1 * $ * ]] {
		[2] INTEGER [[ i parm->t_int1 ]] OPTIONAL << opt_set $ OPT_INT2 >>
	    },

	c SET [[ T struct pepy_refs1 * $ *parm ]] {
	    [3] IMPLICIT ENUMERATED [[ i parm->t_enum ]] {
orange(1), apple(2), pear(3), bannana(4) } OPTIONAL << opt_set $ OPT_ENUM1 >>
	},
	d SET [[ T struct pepy_refs1 *  $ *parm ]] {
	    [4] ENUMERATED [[ i parm->t_enum1 ]] {
orange(1), apple(2), pear(3), bannana(4) } OPTIONAL << opt_set $ OPT_ENUM2 >>
	}
    }

    Repeated [[ P struct repeats * ]]::= SEQUENCE {
	a SEQUENCE OF [[ T struct rep_int * $ rp_sq1 ]]
	    <<r>> [1] INTEGER [[ i i ]],

	b SEQUENCE OF [[ T struct rep_elem * $ rp_sq2 ]] <<r_next>>
		Rep-elem [[ p * ]],

	c SET OF [[ T struct rep_int * $ rp_st1 ]] <<r>> INTEGER [[ i i ]],

	d SET OF [[ T struct rep_elem * $ rp_st2 ]] <<r_next>>
		[3] IMPLICIT Rep-elem [[ p * ]],

	e CHOICE [[ T struct repeats  * $ * ]] <<rp_choice>> {
		[1] INTEGER [[ i rp_int ]],
		[2] BOOLEAN [[ b rp_bool ]],
		[3] OCTET STRING [[ o rp_ostring $ rp_int ]]
	    }
	}

    Rep-elem [[ P struct rep_elem * ]]
	::= SEQUENCE {
	    a INTEGER [[ i r_int ]],
	    b OCTET STRING [[ s r_ostring ]],
	    c BIT STRING [[ x r_bstring $ r_int ]]
	}

    Defined [[ P struct pepy_refs * ]] ::= 
	SEQUENCE {
	a PrintableString [[ s t_string ]],
	b [ APPLICATION 3 ] AString [[ q t_qbuf ]],
	c [ APPLICATION 3 ] IMPLICIT AnOctetString [[ o t_ostring $ t_olen ]],
	d BitsAndPieces [[ x t_bstring $ t_blen ]],
	e AnAny [[ a t_any ]],
	ABitString [[ t t_pe ]],
	AnotherType [[ i t_int ]],
	[12] AReal [[ r t_real ]],
	[99] IMPLICIT AnOID [[ O t_oid ]],
	[13] IMPLICIT Boodles [[ b t_bool ]],
	AnEnumerated [[ i t_enum ]]
	}

    AString ::= GraphicString

    AnOctetString ::= OCTET STRING

    BitsAndPieces ::= [31] SomeBits

    SomeBits ::= [APPLICATION 10] IMPLICIT ABitString

    ABitString ::= BIT STRING

    AnAny ::= ANY

    AnotherType ::= [21] AnInteger

    AnInteger ::= INTEGER

    AReal ::= REAL

    AnOID ::= OBJECT IDENTIFIER

    Boodles ::= [PRIVATE 9] ABoolean

    ABoolean ::= [APPLICATION 12] BOOLEAN

    AnEnumerated ::= [4] ENUMERATED { orange(1), apple(2), pear(3), bannana(4) } 
    -- Coding functions tests

    CodeTest [[ P struct codedata * ]] ::= SEQUENCE {
	a BasicANY [[ p cd_a ]],
	b [42] E-ANY [[ p cd_b ]],
	c [36] D-ANY [[ p cd_c ]],
	d DE-ANY [[ p cd_d ]],
	e [APPLICATION 44] IMPLICIT INTEGER [[ i cd_int ]]
	    [[ E get_int ]] [[ D put_int ]] DEFAULT 1,
	f [17] GraphicString [[ s cd_string ]]
	    [[ E get_string ]][[ D put_string ]] [[ F fr_string ]] OPTIONAL,
	left CodeOpt [[ p cd_left ]] OPTIONAL,
	right CodeDflt [[ p cd_right ]] OPTIONAL
    }

   BasicANY ::= ANY [[ a * ]] 

   E-ANY ::= ANY [[ a * ]] [[ E get_pe ]]

   D-ANY ::= ANY [[ a * ]]  [[ D put_pe ]] [[ F fr_pe ]]

   DE-ANY ::= ANY [[ a * ]]  [[ D put_pe ]] [[ E get_pe ]]

    CodeOpt [[ P struct codedata * ]] ::= SEQUENCE {
	a [PRIVATE 5] BasicANY [[ p cd_a ]] OPTIONAL,
	b [35] E-ANY [[ p cd_b ]] OPTIONAL,
	c [45] EXPLICIT D-ANY [[ p cd_c ]] OPTIONAL <<cd_opt_set $ CD_C>>,
	d [1] DE-ANY [[ p cd_d ]] OPTIONAL <<cd_opt_set $ CD_D>>,
	e [15] IMPLICIT INTEGER [[ i cd_int ]] [[ E get_int ]] [[ D put_int ]]
		OPTIONAL << cd_opt_set $ CD_INT >>,
	f GraphicString [[ s cd_string ]] [[ E get_string ]][[ D put_string ]]
		OPTIONAL
    }

    -- It would be nice to do DEFAULTs for ANY but how do we specify
    -- DEFAULT ANY in the ASN.1 ?? Let alone parse it

    CodeDflt [[ P struct codedata * ]] ::= SEQUENCE {
	a [1] BasicANY [[ p cd_a ]] OPTIONAL,
	b [2] E-ANY [[ p cd_b ]] OPTIONAL,
	c [3] D-ANY [[ p cd_c ]] OPTIONAL <<cd_opt_set $ CD_C>>,
	d [4] DE-ANY [[ p cd_d ]] OPTIONAL <<cd_opt_set $ CD_D>>,
	e [APPLICATION 4] INTEGER [[ i cd_int ]] [[ E get_int ]] [[ D put_int ]]
		DEFAULT 3,
	f [90] IMPLICIT GraphicString [[ s cd_string ]] [[ E get_string2 ]]
	    [[ D put_string2 ]] DEFAULT '536f6d65206973206f7574'H
    }

    Simple1 [[ P struct codedata * ]] ::= CodeDflt [[ p cd_right ]]

    Simple2 [[ P struct codedata * ]] ::= INTEGER [[ i cd_int ]]

    Simple3 [[ P struct codedata * ]] ::= [1] INTEGER [[ i cd_int1 ]]

    Simple4 [[ P struct codedata * ]] ::= [1] IMPLICIT INTEGER [[ i cd_int2 ]]

    Simple5 [[ P struct codedata * ]] ::= OCTET STRING [[ s cd_string ]]

    Simple6 [[ P struct codedata * ]] ::= [1] OCTET STRING [[ s cd_string1 ]]

    Simple7 [[ P struct codedata * ]] ::= [1] IMPLICIT OCTET STRING
						[[ s cd_string2 ]]

    Simple8 [[ P struct codedata * ]] ::= OBJECT IDENTIFIER [[ O cd_oid ]]

    Simple9 [[ P struct codedata * ]] ::= [1] OBJECT IDENTIFIER [[ O cd_oid1 ]]

    Simple10 [[ P struct codedata * ]] ::= [1] IMPLICIT OBJECT IDENTIFIER
						[[ O cd_oid2 ]]

    Simple11 [[ P struct codedata * ]] ::= ANY [[ a cd_a ]]

    Simple12 [[ P struct codedata * ]] ::= BIT STRING [[ t cd_bit ]]

    Simple13 [[ P struct codedata * ]] ::= REAL [[ r cd_real ]]

    Simple14 [[ P struct codedata * ]] ::= BOOLEAN [[ b cd_bool ]]

    AllSimples [[ P struct codedata * ]] ::= SEQUENCE {

	a Simple1 [[ p * ]],
	b Simple2 [[ p * ]],
	c Simple3 [[ p * ]],
	d Simple4 [[ p * ]],
	e Simple5 [[ p * ]],
	f Simple6 [[ p * ]],
	g Simple7 [[ p * ]],
	h Simple8 [[ p * ]],
	i Simple9 [[ p * ]],
	j Simple10 [[ p * ]],
	k Simple11 [[ p * ]],
	l Simple12 [[ p * ]],
	m Simple13 [[ p * ]],
	n Simple14 [[ p * ]]

    }

    UAction [[ P struct repeats * ]]::= SEQUENCE {
	a SEQUENCE OF [[ T struct rep_int * $ rp_sq1 ]]
	    <<r>> [1] INTEGER [[ i i ]],

	b SEQUENCE OF [[ T struct rep_elem * $ rp_sq2 ]] <<r_next>>
		Rep-action [[ p * ]],

	c SET OF [[ T struct rep_int * $ rp_st1 ]] <<r>> INTEGER [[ i i ]],

	d SET OF [[ T struct rep_elem * $ rp_st2 ]] <<r_next>>
		%E{ (void) printf("encoding SET OF Rep-action\n");%}
		[3] IMPLICIT Rep-action [[ p * ]],

	e CHOICE [[ T struct repeats  * $ * ]] <<rp_choice>> {
	    %D{ (void) printf("Before Encoding CHOICE\n"); %}
		[1] INTEGER [[ i rp_int ]],
		[2]
		%E{ (void) printf("encoding Boolean %d\n", parm->rp_bool);%}
		BOOLEAN [[ b rp_bool ]]
		%D{ (void) printf("Decoding Boolean\n"); %},
		[3] OCTET STRING [[ o rp_ostring $ rp_int ]]
	    }
	%D{ (void) printf("After Decoding CHOICE: chosen %d\n", (*parm)->rp_choice); %}
	}  %D{ (void) printf("After Decoding UAction\n"); %}

    Rep-action [[ P struct rep_elem * ]]
	::=
		%D{ (void) printf("Before Decoding Rep-action\n"); %}
	SEQUENCE {
		%E{ (void) printf("before encoding INTEGER %d\n", parm->r_int);%}
	    a INTEGER [[ i r_int ]],
		%E{ (void) printf("before encoding OCTET STRING \"%s\"\n",
			    parm->r_ostring);
	    %}
	    b OCTET STRING [[ s r_ostring ]],
		%E{
		    int i;

		    (void) printf("before encoding BIT STRING: ");
		    for (i = 0; i < parm->r_int; i++) {
			if (BITTEST(parm->r_bstring, i))
			    (void) putchar('1');
			else
			    (void) putchar('0');
		    }
		    (void) putchar('\n');

		%}
	    c BIT STRING [[ x r_bstring $ r_int ]]
		%E{
		    (void) printf("After Bitstring encoding:");
		    vunknown(*ppe);
		%}
	}
		%E{
		    (void) printf("After Sequence encoding:\n");
		    vunknown(*ppe);
		%}
		%D{
		    int i;

		    (void) printf("After Decoding Rep-action:\n");
		    (void) printf("rp_int %d\n", (*parm)->r_int);
		    (void) printf("r_ostring \"%s\"\n", (*parm)->r_ostring);
		    (void) printf("r_bstring: ");

		    for (i = 0; i < (*parm)->r_int; i++) {
			if (BITTEST((*parm)->r_bstring, i))
			    (void) putchar('1');
			else
			    (void) putchar('0');
		    }
		    (void) putchar('\n');

		%}

END
%{

/*
 * Encoding function for test examples of the Coding functions in t3.py
 */

get_pe(parm, ppe)
PE	parm;
PE	*ppe;
{
    if (ppe == (PE *)0) {
	if (parm == NULLPE)	/* Optional */
	    return (OK + 1);
	return (OK);
    }

    *ppe = pe_cpy(parm);
    return (OK);
}

put_pe(parm, pe)
PE	*parm;
PE	pe;
{
    *parm = pe_cpy(pe);
    return (OK);
}

get_int(parm, ppe)
struct codedata	*parm;
PE	*ppe;
{
    if (ppe == (PE *)0) {
	if (parm->cd_int == 1)	/* Default case */
	    return (OK + 1);
	return (OK);
    }

    *ppe = int2prim(parm->cd_int);
    return (OK);
}

put_int(parm, pe)
struct codedata	**parm;
PE	pe;
{
    if (pe)
	(*parm)->cd_int = prim2num(pe);
    else
	(*parm)->cd_int = 1;	/* Default */
    return (OK);
}

get_string(parm, ppe)
struct codedata	*parm;
PE	*ppe;
{
    if (ppe == (PE *)0) {
	if (parm->cd_string == NULLCP) /* Not present */
	    return (OK + 1);
	return (OK);
    }

    *ppe = str2prim(parm->cd_string, strlen(parm->cd_string), 0, 0);
    return (OK);
}

put_string(parm, pe)
struct codedata	**parm;
PE	pe;
{
    int	len;

    if (pe)
	(*parm)->cd_string = prim2str(pe, &len);
    else
	(*parm)->cd_string = NULLCP;	/* Default */

    return (OK);
}
get_string2(parm, ppe)
struct codedata	*parm;
PE	*ppe;
{
    if (ppe == (PE *)0) {
	if (parm->cd_string == NULLCP
	 || strcmp(parm->cd_string, "Some is out") == 0)/* Not present */
	    return (OK + 1);
	return (OK);
    }

    *ppe = str2prim(parm->cd_string, strlen(parm->cd_string), 0, 0);
    return (OK);
}

put_string2(parm, pe)
struct codedata	**parm;
PE	pe;
{
    int	len;

    if (pe)
	(*parm)->cd_string = prim2str(pe, &len);
    else
	(*parm)->cd_string = strdup("Some is out");	/* Default */

    return (OK);
}

fr_string(parm)
struct codedata	*parm;
{
     if (parm->cd_string)
	free(parm->cd_string);

     return (OK);
}

fr_pe(parm)
PE	parm;
{
    if (parm)
	pe_free(parm);

    return (OK);
}

%}

