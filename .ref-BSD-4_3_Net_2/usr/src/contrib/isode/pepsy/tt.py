TT DEFINITIONS ::=

%{
int foo;
%}

BEGIN

EntryInformationSelection [[P struct entryinfoselection *]]
	::=
	SET
	{
	attributeTypes
		CHOICE [[ T struct entryinfoselection * $ *]]
		<D< (*parm)->eis_allattributes>>
		<E< parm->eis_allattributes ? 1 : 2>>
		{
		allAttributes
			[0] NULL,
		select
			[1] SET OF [[ T struct attrcomp * $ eis_select ]] <<attr_link>>
				AttributeType [[p attr_type]]
		}
		%D{
			if ((*parm)->eis_allattributes == 1)
			   (*parm)->eis_allattributes = TRUE;
			else
			   (*parm)->eis_allattributes = FALSE;
		%}
		    -- DEFAULT allAttributes NULL,
		    OPTIONAL <E<parm->eis_allattributes != FALSE>><D<0>>,
	infoTypes
		[2] INTEGER [[i eis_infotypes]]
		{
		attributeTypesOnly(0) ,
		attributeTypesAndValues(1)
		}
		    DEFAULT attributeTypesAndValues
	}


TestChoice ::= CHOICE <E< 1 >> <D< foo >> {
	one OCTET STRING,
	two SEQUENCE {
		one2 IA5String OPTIONAL <E< 1 >> <D< 0 >>,
		two2 CHOICE {
			one3 NULL,
			two3 INTEGER
		} OPTIONAL <E<1>> <D<foo = 1>>
	},
	three INTEGER {eine(1), zwei(2), drie(3) },
	four BIT STRING { un(1), deux(2), trois(3), quatre(4) }
}
END

