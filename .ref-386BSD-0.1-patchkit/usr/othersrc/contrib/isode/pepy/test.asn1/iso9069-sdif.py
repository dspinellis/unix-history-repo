ISO9069-SDIF DEFINITIONS ::=
BEGIN

Data-stream ::= SEQUENCE
  { data-stream-character-set Character-set-designation ,
    data-stream-name SDIF-name-and-description ,
    related-documents-A [APPLICATION 8] IMPLICIT SEQUENCE OF Document-descriptor OPTIONAL ,
    main-document [APPLICATION 9] IMPLICIT Document-descriptor ,
    related-documents-B [APPLICATION 10] IMPLICIT SEQUENCE OF Document-descriptor OPTIONAL }

Document-descriptor ::= [APPLICATION 11] IMPLICIT SEQUENCE
  { document-name SDIF-name-and-description ,
    first-identifier SDIF-identifier ,
    document-entity SGML-document-entity ,
    external-entities SET OF Entity-descriptor OPTIONAL }

Entity-descriptor ::= [APPLICATION 12] IMPLICIT SEQUENCE
  { entity-identifier SDIF-identifier ,
    entity-name SGML-name ,
    CHOICE
      { subdocument-structure Subdocument-structure ,
        text-entity SGML-text-entity ,
        data-entity Non-SGML-data-entity ,
        cross-reference SDIF-identifier ,
        omitted-public-text NULL } }

Subdocument-structure ::= [APPLICATION 13] IMPLICIT SEQUENCE
  { first-identifier SDIF-identifier ,
    subdocument-entity SGML-subdocument-entity }

Character-set-designation ::= [APPLICATION 0] IMPLICIT OCTET STRING

Non-SGML-data-entity ::= [APPLICATION 1] IMPLICIT OCTET STRING

SDIF-identifier ::= [APPLICATION 2] IMPLICIT OCTET STRING

SDIF-name-and-description ::= [APPLICATION 3] IMPLICIT OCTET STRING

SGML-document-entity ::= [APPLICATION 4] IMPLICIT OCTET STRING

SGML-name ::= [APPLICATION 5] IMPLICIT OCTET STRING

SGML-subdocument-entity ::= [APPLICATION 6] IMPLICIT OCTET STRING

SGML-text-entity ::= [APPLICATION 7] IMPLICIT OCTET STRING

END
