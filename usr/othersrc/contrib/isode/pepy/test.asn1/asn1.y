/* yacc.y - yacc ASN.1 parser */

%token		DEFINITIONS CCE BGIN END ID MODULEREFERENCE DOT
		TYPEREFERENCE VALUEREFERENCE IDENTIFIER BOOLEAN TRUE
		FALSE INTEGER LBRACE RBRACE COMMA LBRACKET RBRACKET
		NUMBER MINUS BIT STRING BSTRING HSTRING OCTET NIL
		SEQUENCE OPTIONAL DEFAULT COMPONENTS OF SET CHOICE
		LANGLE IMPLICIT UNIVERSAL APPLICATION PRIVATE ANY
		OBJECT LPAREN RPAREN CSTRING MACROREFERENCE MACRO TYPE
		NOTATION VALUE PRODUCTIONREFERENCE BAR ASTRING QSTRING
		QIDENTIFIER QNUMBER QEMPTY LOCALTYPEREFERENCE
		LOCALVALUEREFERENCE VVALUE RANGLE

%%

ModuleDefinition:	MODULEREFERENCE
			DEFINITIONS CCE
			BGIN
			ModuleBody
			END 
	;

ModuleBody:		AssignmentList
	|		empty
	;

AssignmentList:		Assignment AssignmentList
	|		Assignment
	;

Assignment:		Typeassignment
	|		Valueassignment
	;

Externaltypereference:	MODULEREFERENCE DOT TYPEREFERENCE
	;

Externalvaluereference:	MODULEREFERENCE DOT VALUEREFERENCE
	;

DefinedType:		Externaltypereference
	|		TYPEREFERENCE
	;

DefinedValue:		Externalvaluereference
	|		VALUEREFERENCE
	;

Typeassignment:		TYPEREFERENCE CCE Type
	;

Valueassignment:	VALUEREFERENCE Type CCE Value
	;

Type:			BuiltinType
	|		DefinedType
	;

BuiltinType:		BooleanType
	|		IntegerType
	|		BitStringType
	|		OctetStringType
	|		NullType
	|		SequenceType
	|		SequenceOfType
	|		SetType
	|		SetOfType
	|		ChoiceType
	|		SelectionType
	|		TaggedType
	|		AnyType
	|		ObjectIdentifierType
/*	|		CharacterStringType
	|		UsefulType */
	;

NamedType:		IDENTIFIER Type
	|		Type
	|		SelectionType
	;

Value:			BuiltinValue
	|		DefinedValue
	;

BuiltinValue:		BooleanValue
	|		IntegerValue
	|		BitStringValue
	|		OctetStringValue
	|		NullValue
	|		SequenceValue
	|		SequenceOfValue
	|		SetValue
	|		SetOfValue
	|		ChoiceValue
	|		SelectionValue
	|		TaggedValue
	|		AnyValue
	|		ObjectIdentifierValue
/*	|		CharacterStringValue */
	;

NamedValue:		IDENTIFIER Value
	|		Value
	;

BooleanType:		BOOLEAN
	;

BooleanValue:		TRUE
	|		FALSE
	;

IntegerType:		INTEGER
	|		INTEGER LBRACE NamedNumberList RBRACE
	;

NamedNumberList:	NamedNumber
	|		NamedNumberList COMMA NamedNumber
	;

NamedNumber:		IDENTIFIER LPAREN SignedNumber RPAREN
	|		IDENTIFIER LPAREN DefinedValue RPAREN
	;

SignedNumber:		NUMBER
	|		MINUS NUMBER
	;

IntegerValue:		SignedNumber
	|		IDENTIFIER
	;

BitStringType:		BIT STRING
	|		BIT STRING LBRACE NamedBitList RBRACE
	;

NamedBitList:		NamedBit
	|		NamedBitList COMMA NamedBit
	;

NamedBit:		IDENTIFIER LPAREN NUMBER RPAREN
	|		IDENTIFIER LPAREN DefinedValue RPAREN
	;

BitStringValue:		BSTRING
	|		HSTRING
	|		LBRACE IdentifierList RBRACE
	|		LBRACE RBRACE
	;

IdentifierList:		IDENTIFIER
	|		IdentifierList COMMA IDENTIFIER
	;

OctetStringType:	OCTET STRING
	;

OctetStringValue:	BSTRING
	|		HSTRING
	;

NullType:		NIL
	;

NullValue:		NIL
	;

SequenceType:		SEQUENCE LBRACE ElementTypeList RBRACE
	|		SEQUENCE LBRACE RBRACE
	;

ElementTypeList:	ElementType
	|		ElementTypeList COMMA ElementType
	;

ElementType:		NamedType
	|		NamedType OPTIONAL
	|		NamedType DEFAULT Value
	|		COMPONENTS OF Type
	;

SequenceValue:		LBRACE ElementValueList RBRACE
	|		LBRACE RBRACE
	;

ElementValueList:	NamedValue
	|		ElementValueList COMMA NamedValue
	;

SequenceOfType:		SEQUENCE OF Type
	|		SEQUENCE
	;

SequenceOfValue:	LBRACE ValueList RBRACE
	|		LBRACE RBRACE
	;

ValueList:		Value
	|		ValueList COMMA Value
	;

SetType:		SET LBRACE ElementTypeList RBRACE
	|		SET LBRACE RBRACE
	;

SetValue:		LBRACE ElementValueList RBRACE
	|		LBRACE RBRACE
	;

SetOfType:		SET OF Type
	|		SET
	;

SetOfValue:		LBRACE ValueList RBRACE
	|		LBRACE RBRACE
	;

ChoiceType:		CHOICE LBRACE AlternativeTypeList RBRACE
	;

AlternativeTypeList:	NamedType
	|		AlternativeTypeList COMMA NamedType
	;

ChoiceValue:		NamedValue
	;

SelectionType:		IDENTIFIER LANGLE Type
	;

SelectionValue:		NamedValue
	;

TaggedType:		Tag Type
	|		Tag IMPLICIT Type
	;

Tag:			LBRACKET Class ClassNumber RBRACKET
	;

ClassNumber:		NUMBER
	|		DefinedValue
	;

Class:			UNIVERSAL
	|		APPLICATION
	|		PRIVATE
	|		empty
	;

TaggedValue:		Value
	;

AnyType:		ANY
	;

AnyValue:		Type Value
	;

ObjectIdentifierType:	OBJECT IDENTIFIER
	;

ObjectIdentifierValue:	LBRACE ObjIdComponentList RBRACE
	|		LBRACE DefinedValue ObjIdComponentList RBRACE
	;

ObjIdComponentList:	ObjIdComponent
	|		ObjIdComponent ObjIdComponentList
	;

ObjIdComponent:		NameForm
	|		NumberForm
	|		NameAndNumberForm
	;

NameForm:		IDENTIFIER
	;

NumberForm:		NUMBER
	|		DefinedValue
	;

NameAndNumberForm:	IDENTIFIER LPAREN NumberForm RPAREN
	;

/* CharacterStringType:	TYPEREFERENCE
	;

CharacterStringValue:	CSTRING
	;

UsefulType:		TYPEREFERENCE
	;
*/
empty:			/* empty */
	;


/* Macro stuff */

MacroDefinition:	MACROREFERENCE 
			MACRO CCE
			BGIN
			MacroBody
			END
	;

MacroBody:		TypeProduction
	|		ValueProduction
	|		SupportingProductions
	;

TypeProduction:		TYPE NOTATION
			CCE
			MacroAlternativeList
	;

ValueProduction:	VALUE NOTATION
			CCE
			MacroAlternativeList
	;

SupportingProductions:	ProductionList
	|		empty
	;

ProductionList:		Production
	|		ProductionList Production
	;

Production:		PRODUCTIONREFERENCE
			CCE
			MacroAlternativeList
	;

MacroAlternativeList:	MacroAlternative
	|		MacroAlternativeList BAR MacroAlternative
	;

MacroAlternative:	SymbolList
	;

SymbolList:		SymbolElement
	|		SymbolList SymbolElement
	;

SymbolElement:		SymbolDefn
	|		EmbeddedDefinitions
	;

SymbolDefn:		ASTRING
	|		PRODUCTIONREFERENCE
	|		QSTRING
	|		QIDENTIFIER
	|		QNUMBER
	|		QEMPTY
	|		TYPE
	|		TYPE LPAREN LOCALTYPEREFERENCE RPAREN
	|		VALUE LPAREN MacroType RPAREN
	|		VALUE LPAREN LOCALVALUEREFERENCE RPAREN
	|		VALUE LPAREN VVALUE MacroType RPAREN
	;

MacroType:		LOCALVALUEREFERENCE
	|		Type
	;

EmbeddedDefinitions:	RANGLE EmbeddedDefinitionList LANGLE
	;

EmbeddedDefinitionList:	EmbeddedDefinition
	|		EmbeddedDefinitionList EmbeddedDefinition
	;

EmbeddedDefinition:	LocalTypeAssignment
	|		LocalValueAssignment
	;

LocalTypeAssignment:	LOCALTYPEREFERENCE CCE MacroType
	;

LocalValueAssignment:	LOCALVALUEREFERENCE MacroType CCE MacroValue
	;

MacroValue:		Value
	|		LOCALVALUEREFERENCE
	;
