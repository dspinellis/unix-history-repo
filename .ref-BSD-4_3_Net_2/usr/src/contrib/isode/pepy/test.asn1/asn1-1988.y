%token	typereference identifier valuereference modulereference number
	bstring hstring cstring CCE LBRACE RBRACE LANGLE DOT COMMA
	LPAREN RPAREN LBRACKET RBRACKET MINUS BOOLEAN INTEGER BIT
	STRING OCTET NULL SEQUENCE OF SET IMPLICIT CHOICE ANY EXTERNAL
	OBJECT IDENTIFIER OPTIONAL DEFAULT COMPONENTS UNIVERSAL
	APPLICATION PRIVATE TRUE FALSE BGIN END DEFINITIONS EXPLICIT
	ENUMERATED EXPORTS IMPORTS ENCRYPTED REAL INCLUDES MIN MAX
	SIZE FROM WITH COMPONENT PRESENT ABSENT DEFINED BY
	PLUS_INFINITY MINUS_INFINITY macroreference
	productionreference localtypereference localvaluereference BAR
	RANGLE astring qstring qidentifier qnumber qempty MACRO TYPE
	NOTATION VALUE value type SEMICOLON TAGS ZERO TWO TEN DOTDOT
	DOTDOTDOT

%%

ModuleDefinition:	ModuleIdentifier DEFINITIONS TagDefault
			CCE BGIN ModuleBody END
	;

TagDefault:		EXPLICIT TAGS
	|		IMPLICIT TAGS
	|		empty
	;

ModuleIdentifier:	modulereference AssignedIdentifier
	;

AssignedIdentifier:	ObjectIdentifierValue
	|		empty
	;

ModuleBody:		Exports Imports AssignmentList
	;

Exports:		EXPORTS SymbolsExported SEMICOLON
	;

SymbolsExported:	SymbolList
	|		empty
	;

Imports:		IMPORTS SymbolsImported SEMICOLON
	|		empty
	;

SymbolsImported:	SymbolsFromModuleList
	|		empty
	;

SymbolsFromModuleList:	SymbolsFromModule SymbolsFromModuleList
	|		SymbolsFromModule
	;

SymbolsFromModule:	SymbolList FROM ModuleIdentifier
	;

SymbolList:		Symbol COMMA SymbolList
	|		Symbol
	;

Symbol:			typereference
	|		valuereference
	;

AssignmentList:		Assignment AssignmentList
	|		Assignment
	;

Assignment:		Typeassignment
	|		Valueassignment
	;

Externaltypereference:	modulereference DOT typereference
	;

Externalvaluereference:	modulereference DOT valuereference
	;

DefinedType:		Externaltypereference
	|		typereference
	;

DefinedValue:		Externalvaluereference
	|		valuereference
	;

Typeassignment:		typereference CCE Type
	;

Valueassignment:	valuereference Type CCE Value
	;

Type:			BuiltinType
	|		DefinedType
	|		SubType
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
	|		CharacterStringType
	|		UsefulType
	|		EnumeratedType
	|		RealType
	|		EncryptedType
	;

NamedType:		identifier Type
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
	|		CharacterStringValue
	|		EnumeratedValue
	|		RealValue
	|		EncryptedValue
	;

NamedValue:		identifier Value
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

NamedNumber:		identifier LPAREN SignedNumber RPAREN
	|		identifier LPAREN DefinedValue RPAREN
	;
	
SignedNumber:		number
	|		MINUS number
	;

IntegerValue:		SignedNumber
	|		identifier
	;

EnumeratedType:		ENUMERATED LBRACE Enumeration RBRACE
	;

Enumeration:		NamedNumber
	|		NamedNumber COMMA Enumeration
	;

EnumeratedValue:	identifier
	;

RealType:		REAL
	;

RealValue:		NumericRealValue
	|		SpecialRealValue
	;

NumericRealValue:	LBRACE Mantissa COMMA Base COMMA Exponent RBRACE
	|		ZERO
	;

Mantissa:		SignedNumber
	;

Base:			TWO
	|		TEN
	;

Exponent:		SignedNumber
	;

SpecialRealValue:	PLUS_INFINITY
	|		MINUS_INFINITY
	;
	
BitStringType:		BIT STRING
	|		BIT STRING LBRACE NamedBitList RBRACE
	;

NamedBitList:		NamedBit
	|		NamedBitList COMMA NamedBit
	;

NamedBit:		identifier LPAREN number RPAREN
	|		identifier LPAREN number RPAREN
	;

BitStringValue:		bstring
	|		hstring
	|		LBRACE IdentifierList RBRACE
	;

IdentifierList:		identifier
	|		IdentifierList COMMA identifier
	;
	
OctetStringType:	OCTET STRING
	;

OctetStringValue:	bstring
	|		hstring
	;

NullType:		NULL
	;

NullValue:		NULL
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

SelectionType:		identifier LANGLE Type
	;

SelectionValue:		NamedValue
	;

TaggedType:		Tag Type
	|		Tag IMPLICIT Type
	|		Tag EXPLICIT Type
	;
	
Tag:			LBRACKET Class ClassNumber RBRACKET
	;

ClassNumber:		number
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
	|		ANY DEFINED BY identifier
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

NameForm:		identifier
	;

NumberForm:		number
	|		DefinedValue
	;

NameAndNumberForm:	identifier LPAREN NumberForm RPAREN
	;

EncryptedType:		ENCRYPTED Type
	;

EncryptedValue:		Value
	;

CharacterStringType:	typereference
	;

CharacterStringValue:	cstring
	;

UsefulType:		typereference
	;

SubType:		ParentType SubtypeSpec
	|		SET SizeConstraint OF Type
	|		SEQUENCE SizeConstraint OF Type
	;

ParentType:		Type
	;

SubtypeSpec:		LPAREN SubtypeAlternative SubtypeAlternativeList RPAREN
	;

SubtypeAlternativeList:	BAR SubtypeAlternative SubtypeAlternativeList
	|		empty
	;

SubtypeAlternative:	SubtypeValueSet
	|		SubtypeConstraint
	;

SubtypeValueSet:	SingleValue
	|		ContaindSubtype
	|		ValueRange
	|		PermittedAlphabet
	;

SubtypeConstraint:	SizeConstraint
	|		InnerTypeConstraint
	;

SingleValue:		Value
	;

ContaindSubtype:	INCLUDES Type
	;

ValueRange:		LowerEndpoint DOTDOT UpperEndpoint
	;

LowerEndpoint:		LowerEndValue
	|		LowerEndValue LANGLE
	;
	
UpperEndpoint:		UpperEndValue
	|		LANGLE UpperEndValue
	;

LowerEndValue:		Value
	|		MIN
	;

UpperEndValue:		Value
	|		MAX
	;

SizeConstraint:		SIZE SubtypeSpec
	;

PermittedAlphabet:	FROM SubtypeSpec
	;

InnerTypeConstraint:	WITH COMPONENT SingleTypeConstraint
	|		WITH COMPONENTS MultipleTypeConstraints
	;

SingleTypeConstraint:	SubtypeSpec
	;

MultipleTypeConstraints:FullSpecification
	|		PartialSpecification
	;

FullSpecification:	LBRACE TypeConstraints RBRACE
	;

PartialSpecification:	LBRACE DOTDOTDOT COMMA TypeConstraints RBRACE
	;

TypeConstraints:	NamedConstraint
	|		NamedConstraint COMMA TypeConstraints
	;

NamedConstraint:	identifier Constraint
	|		Constraint
	;

Constraint:		ValueConstraint PresenceConstraint
	;

ValueConstraint:	SubtypeSpec
	|		empty
	;

PresenceConstraint:	PRESENT
	|		ABSENT
	|		empty
	;

/* Macro defintions */

MacroDefinition:    	macroreference MACRO CCE MacroSubstance
	;

MacroSubstance:		BGIN MacroBody END
	|		macroreference
	|		Externalmacroreference
	;
	
MacroBody:		TypeProduction
	|		ValueProduction
	|		SupportingProductions
	;

TypeProduction:		TYPE NOTATION CCE MacroAlternativeList
	;

ValueProduction:	VALUE NOTATION CCE MacroAlternativeList
	;

SupportingProductions:	ProductionList
	|		empty
	;

ProductionList:		Production
	|		ProductionList Production
	;

Production:		productionreference CCE MacroAlternativeList
	;

Externalmacroreference:	modulereference DOT macroreference
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

SymbolDefn:		astring
	|		productionreference
	|		qstring
	|		qidentifier
	|		qnumber
	|		qempty
	|		type
	|		type LPAREN localtypereference RPAREN
	|		value LPAREN MacroType RPAREN
	|		value LPAREN localvaluereference RPAREN
	|		value LPAREN VALUE MacroType RPAREN
	;

MacroType:	localtypereference
	|	Type
	;

EmbeddedDefinitions:	LANGLE EmbeddedDefinitionList RANGLE
	;

EmbeddedDefinitionList:	EmbeddedDefinition
	|		EmbeddedDefinitionList EmbeddedDefinition
	;

EmbeddedDefinition:	LocalTypeassignment
	|		LocalValueassignment
	;

LocalTypeassignment:	localtypereference CCE MacroType
	;

LocalValueassignment:	localvaluereference MacroType CCE MacroValue
	;

MacroValue:		Value
	|		localvaluereference
	;

empty:			/* empty */
	;
