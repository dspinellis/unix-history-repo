-- VTPM: print VT PDUs

-- $Header: /f/osi/vt/RCS/print_vt.py,v 7.1 91/02/22 09:48:00 mrose Interim $
--
--
-- $Log:	print_vt.py,v $
-- Revision 7.1  91/02/22  09:48:00  mrose
-- Interim 6.8
-- 
-- Revision 7.0  89/11/23  22:31:33  mrose
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


VT DEFINITIONS ::=

BEGIN

SECTIONS none none print

PDUs ::=
        CHOICE {
            asqpdu[0]
                IMPLICIT ASQcontent,

            asrpdu[1]
                IMPLICIT ASRcontent,

            ndqpdu[6]
                IMPLICIT NDQcontent,

            udqpdu[7]
                IMPLICIT COupdate,

            bkqpdu[8]
                IMPLICIT BKQcontent,

            bkrpdu[9]
                IMPLICIT BKRcontent
        }

ASQcontent ::=
        SEQUENCE {
            [0]
                IMPLICIT INTEGER,

            [1]
                IMPLICIT ImplemIdent
                OPTIONAL,

            [2]
                IMPLICIT BIT STRING,

            [3]
                IMPLICIT Profile
                OPTIONAL,

            [4]
                IMPLICIT BIT STRING,

            [5]
                IMPLICIT INTEGER
                OPTIONAL
        }

ImplemIdent ::=
        SEQUENCE {
            impIdent[0]
                IMPLICIT OBJECT IDENTIFIER
                OPTIONAL,

            impName[1]
                IMPLICIT PrintableString
                OPTIONAL,

            impVersion[2]
                IMPLICIT PrintableString
                OPTIONAL
        }

Profile ::=
        SEQUENCE {
            name
                OBJECT IDENTIFIER
                OPTIONAL,

                ProfileArgList
                OPTIONAL
        }

ProfileArgList ::=
        SEQUENCE OF
            CHOICE {
                specialArgs[0]
                    IMPLICIT SEQUENCE {
                        identifier
                            INTEGER,

                        offeredValues
                            CHOICE {
                                boolean[0]
                                    IMPLICIT BIT STRING,

                                integer[1]
                                    IMPLICIT IntOffer,

                                string[2]
                                    IMPLICIT SET OF
                                        PrintableString
                            }
                    },

                vteParams[1]
                    IMPLICIT ParamOfferList
            }

ParamOfferList ::=
        SEQUENCE {
            displayObjects[0]
                IMPLICIT CDSOffer
                OPTIONAL,

            controlObjects[1]
                IMPLICIT CSSOffer
                OPTIONAL,

            deviceObjects[2]
                IMPLICIT DEVOffer
                OPTIONAL,

            deliveryControl[3]
                IMPLICIT BIT STRING
                OPTIONAL
        }

CDSOffer ::=
        SET OF
            SEQUENCE {
                objectName
                    PrintableString,

                    ObjectOffer
            }

CSSOffer ::=
        NULL

DEVOffer ::=
        NULL

ObjectOffer ::=
        SEQUENCE {
            dimensionOffer[0]
                IMPLICIT BIT STRING
                OPTIONAL,

            xParamOffer[1]
                IMPLICIT DimOffer
                OPTIONAL,

            yParamOffer[2]
                IMPLICIT DimOffer
                OPTIONAL,

            zParamOffer[3]
                IMPLICIT DimOffer
                OPTIONAL,

            erasuroffer[4]
                IMPLICIT BIT STRING
                OPTIONAL,

            repOfferList[5]
                IMPLICIT CompRepOffer
                OPTIONAL,

            empOfferList[6]
                IMPLICIT CompEmpOffer
                OPTIONAL,

            foreColorList[7]
                IMPLICIT ColorOffer
                OPTIONAL,

            backColorList[8]
                IMPLICIT ColorOffer
                OPTIONAL,

            objectAccRight[9]
                IMPLICIT BIT STRING
                OPTIONAL
        }

DimOffer ::=
        SEQUENCE {
            bound[0]
                IMPLICIT SEQUENCE {
                    unbounded
                        NULL
                        OPTIONAL,

                    limit
                        IntOffer
                        OPTIONAL
                }
                OPTIONAL,

            addressing[1]
                IMPLICIT BIT STRING
                OPTIONAL,

            absolute[2]
                IMPLICIT BIT STRING
                OPTIONAL,

            window[3]
                IMPLICIT SEQUENCE {
                    unbounded
                        NULL
                        OPTIONAL,

                    limit
                        IntOffer
                        OPTIONAL
                }
                OPTIONAL
        }

CompRepOffer ::=
        SEQUENCE {
            repCapability[0]
                IMPLICIT IntOffer
                OPTIONAL,

            [1]
                IMPLICIT SEQUENCE OF
                    RepFontOffer
                OPTIONAL
        }

RepFontOffer ::=
        CHOICE {
                NULL,

                SEQUENCE {
                    repertoire[0]
                        IMPLICIT PrintableString
                        OPTIONAL,

                    fontCapability[1]
                        IMPLICIT IntOffer
                        OPTIONAL,

                    [2]
                        IMPLICIT SEQUENCE OF
                            PrintableString
                        OPTIONAL
                }
        }

CompEmpOffer ::=
        SEQUENCE {
            empCap[0]
                IMPLICIT IntOffer
                OPTIONAL,

                SEQUENCE OF
                    PrintableString
                OPTIONAL
        }

ColorOffer ::=
        SEQUENCE {
            colorCap[0]
                IMPLICIT IntOffer
                OPTIONAL,

            colorNames
                SEQUENCE OF
                    PrintableString
                OPTIONAL
        }

IntOffer ::=
        SEQUENCE OF
            CHOICE {
                indivValue[0]
                    IMPLICIT INTEGER,

                range[1]
                    IMPLICIT SEQUENCE {
                            INTEGER,

                            INTEGER
                    }
            }

ASRcontent ::=
        SEQUENCE {
            userReason[0]
                IMPLICIT PrintableString
                OPTIONAL,

            provReason[1]
                IMPLICIT INTEGER
                OPTIONAL,

            [2]
                IMPLICIT INTEGER,

            [3]
                IMPLICIT ImplemIdent
                OPTIONAL,

            [4]
                IMPLICIT BIT STRING,

            [5]
                IMPLICIT ArgumValueList,

            [6]
                IMPLICIT BIT STRING,

            [7]
                IMPLICIT INTEGER
                OPTIONAL
        }

ArgumValueList ::=
        SET OF
            Squat

Squat ::=
        CHOICE {
            specArgs[0]
                IMPLICIT SpecialArgs,

            vteParams[1]
                IMPLICIT ParamValueList
        }

SpecialArgs ::=
        SEQUENCE {
            identifier
                INTEGER,

            value
                CHOICE {
                        BOOLEAN,

                        INTEGER,

                        PrintableString
                }
        }

ParamValueList ::=
        SEQUENCE {
            displayObjects[0]
                IMPLICIT CDSValues
                OPTIONAL,

            controlObjects[1]
                IMPLICIT CSSValues
                OPTIONAL,

            deviceObjects[2]
                IMPLICIT DEVValues
                OPTIONAL,

            deliveryControl[3]
                IMPLICIT INTEGER
                OPTIONAL
        }

CDSValues ::=
        SET OF
            SEQUENCE {
                objectName
                    PrintableString,

                    ObjectOffer
            }

CSSValues ::=
        NULL

DEVValues ::=
        NULL

DimValue ::=
        SEQUENCE {
            bound[0]
                CHOICE {
                    unbounded
                        NULL,

                    limit
                        INTEGER
                }
                OPTIONAL,

            addressing[1]
                IMPLICIT INTEGER
                OPTIONAL,

            absolute[2]
                IMPLICIT INTEGER
                OPTIONAL,

            window[3]
                CHOICE {
                    unbounded
                        NULL,

                    limit
                        INTEGER
                }
                OPTIONAL
        }

CompRepValue ::=
        SEQUENCE {
            repCapability[0]
                IMPLICIT INTEGER
                OPTIONAL,

            [1]
                IMPLICIT SEQUENCE OF
                    RepFontValue
                OPTIONAL
        }

RepFontValue ::=
        CHOICE {
                NULL,

                SEQUENCE {
                    repertoire[0]
                        IMPLICIT PrintableString
                        OPTIONAL,

                    fontCapability[1]
                        IMPLICIT INTEGER
                        OPTIONAL,

                    [2]
                        IMPLICIT SEQUENCE OF
                            PrintableString
                        OPTIONAL
                }
        }

CompEmpValue ::=
        SEQUENCE {
            empCap[0]
                IMPLICIT INTEGER
                OPTIONAL,

                SEQUENCE OF
                    PrintableString
                OPTIONAL
        }

ColorValue ::=
        SEQUENCE {
            colorCap[0]
                IMPLICIT INTEGER
                OPTIONAL,

            colorNames
                SEQUENCE OF
                    PrintableString
        }

NDQcontent ::=
        SEQUENCE OF
            VTsdi

VTsdi ::=
        CHOICE {
            echoNow[0]
                IMPLICIT SEQUENCE OF
                    ObjectUpdate,

            notEchoNow[1]
                IMPLICIT SEQUENCE OF
                    ObjectUpdate
        }

ObjectUpdate ::=
        CHOICE {
            display[0]
                IMPLICIT SEQUENCE {
                    doName
                        PrintableString,

                        SEQUENCE OF
                            DOupdate
                },

            control[1]
                IMPLICIT COupdate
        }

DOupdate ::=
        CHOICE {
            nextXarray[0]
                IMPLICIT NULL,

            nextYarray[1]
                IMPLICIT NULL,

            ptr-relative[2]
                IMPLICIT ExplicitPointer,

            ptr-absolute[3]
                IMPLICIT Pointer,

            text[4]
                IMPLICIT OCTET STRING,

            repeatText[5]
                IMPLICIT SEQUENCE {
                    finishAddress
                        Pointer,

                        OCTET STRING
                },

            writeAttr[6]
                IMPLICIT SEQUENCE {
                        AttrId,

                        AttrExtent
                },

            erase[7]
                IMPLICIT SEQUENCE {
                    startErase
                        Pointer,

                    endErase
                        Pointer,

                    eraseAttr
                        BOOLEAN
                },

            previousXarray[8]
                IMPLICIT NULL,

            previousYarray[9]
                IMPLICIT NULL
        }

COupdate ::=
        SEQUENCE {
            coName
                PrintableString,

            objectUpdate
                CHOICE {
                    characterUpdate[0]
                        IMPLICIT PrintableString,

                    booleanUpdate[1]
                        IMPLICIT SEQUENCE {
                            values[0]
                                IMPLICIT BIT STRING,

                            mask[1]
                                IMPLICIT BIT STRING
                        },

                    symbolicUpdate[2]
                        IMPLICIT INTEGER,

                    integerUpdate[3]
                        IMPLICIT INTEGER,

                    bitStringUpdate[4]
                        IMPLICIT BIT STRING
                }
        }

ExplicitPointer ::=
        SEQUENCE {
            x[0]
                IMPLICIT INTEGER
                OPTIONAL,

            y[1]
                IMPLICIT INTEGER
                OPTIONAL,

            z[2]
                IMPLICIT INTEGER
                OPTIONAL
        }

Pointer ::=
        CHOICE {
            current[0]
                IMPLICIT NULL,

            start[1]
                IMPLICIT NULL,

            startY[2]
                IMPLICIT NULL,

            startX[3]
                IMPLICIT NULL,

            end[4]
                IMPLICIT NULL,

            endY[5]
                IMPLICIT NULL,

            endX[6]
                IMPLICIT NULL,

            coords[7]
                IMPLICIT ExplicitPointer
        }

AttrId ::=
        CHOICE {
            graphCharRep[0]
                IMPLICIT INTEGER,

            foreColor[1]
                IMPLICIT INTEGER,

            backColor[2]
                IMPLICIT INTEGER,

            emphasis[3]
                IMPLICIT INTEGER,

            font[4]
                IMPLICIT INTEGER
        }

AttrExtent ::=
        CHOICE {
            global[0]
                IMPLICIT NULL,

            addrExtent[1]
                IMPLICIT SEQUENCE {
                    beginning
                        Pointer,

                    ending
                        Pointer
                },

            modalExtent[2]
                IMPLICIT NULL
        }

BKQcontent ::=
        SEQUENCE {
            token
                INTEGER {
                    initiator(0),
                    acceptor(1),
                    accChoice(2)
                }
                OPTIONAL,

                ExplicitPointer
        }

BKRcontent ::=
        SEQUENCE {
            token
                INTEGER {
                    initiator(0),
                    acceptor(1)
                }
                OPTIONAL,

                ExplicitPointer
        }

END
