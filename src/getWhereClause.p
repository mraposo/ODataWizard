/*------------------------------------------------------------------------
    File        : getWhereClause.p
    Purpose     : 

    Syntax      :

    Description : 

    Author(s)   : Mario & Levi
    Created     : Mon Dec 24 14:46:13 CET 2024
    Notes       :
  ----------------------------------------------------------------------*/

/* ************************  Function Prototypes ********************** */

FUNCTION getDataType RETURNS CHARACTER PRIVATE
    (INPUT piField AS CHARACTER) FORWARD.

FUNCTION replaceHas RETURNS CHARACTER PRIVATE
    (INPUT piQueryString AS CHARACTER) FORWARD.

FUNCTION removeTableNameFromQueryStringIfNotQuoted RETURNS CHARACTER PRIVATE
    (INPUT piQueryString AS CHARACTER) FORWARD.

FUNCTION replaceEndsWith RETURNS CHARACTER PRIVATE
    (INPUT piQueryString AS CHARACTER) FORWARD.

/* ***************************  Main Block  *************************** */
DEFINE INPUT  PARAMETER piQueryString  AS CHARACTER NO-UNDO.
DEFINE INPUT  PARAMETER piTabel        AS CHARACTER NO-UNDO.
DEFINE OUTPUT PARAMETER poWhereClause  AS CHARACTER NO-UNDO.

DEFINE VARIABLE iCounter         AS INTEGER   NO-UNDO.
DEFINE VARIABLE gcTabel          AS CHARACTER NO-UNDO.
DEFINE VARIABLE gcFieldName      AS CHARACTER NO-UNDO.
DEFINE VARIABLE gcFieldList      AS CHARACTER NO-UNDO.
DEFINE VARIABLE dbBuffer         AS HANDLE    NO-UNDO.
DEFINE VARIABLE gcFieldType      AS CHARACTER NO-UNDO.
DEFINE VARIABLE gcFieldListTypes AS CHARACTER NO-UNDO.
DEFINE VARIABLE gcFieldListNames AS CHARACTER NO-UNDO.
DEFINE VARIABLE gcQueryString    AS CHARACTER NO-UNDO.

gcQueryString = piQueryString.
gcTabel       = piTabel.

gcQueryString = removeTableNameFromQueryStringIfNotQuoted(gcQueryString).

CREATE BUFFER dbBuffer FOR TABLE gcTabel.

DO iCounter = 1 TO dbBuffer:NUM-FIELDS:
    
    gcFieldName = dbBuffer:BUFFER-FIELD(iCounter):NAME.
    gcFieldType = dbBuffer:BUFFER-FIELD(iCounter):DATA-TYPE.
    
    gcFieldListNames = IF iCounter > 1 
                        THEN gcFieldListNames + "," + gcFieldName 
                        ELSE gcFieldName.
                        
    gcFieldListTypes = IF iCounter > 1 
                        THEN gcFieldListTypes + "," + gcFieldType 
                        ELSE gcFieldType.                      
END.

DO iCounter = 1 TO NUM-ENTRIES(gcFieldListNames):
    gcQueryString = REPLACE(gcQueryString, ENTRY(iCounter,gcFieldListNames), gcTabel + "." + ENTRY(iCounter,gcFieldListNames)).
END.

IF INDEX(gcQueryString,"endswith(") <> 0 THEN
    gcQueryString = replaceEndsWith(gcQueryString).

IF INDEX(gcQueryString,"has ") <> 0 THEN
    gcQueryString = replaceHas(gcQueryString).
    
    
poWhereClause = "WHERE " + gcQueryString.



/* ************************  Function Implementations ***************** */
FUNCTION getDataType RETURNS CHARACTER PRIVATE
    (INPUT piField AS CHARACTER):
/*------------------------------------------------------------------------------
 Purpose:
 Notes:  Wordt nu niks mee gedaan!!  Zet nou altijd STRING() bij has & endswith..
------------------------------------------------------------------------------*/    
    DEFINE VARIABLE iCounter  AS INTEGER NO-UNDO.
    DEFINE VARIABLE cDataType AS CHARACTER NO-UNDO.
    
    DO iCounter = 1 TO NUM-ENTRIES(gcFieldListNames):
        IF ENTRY(iCounter,gcFieldListNames) = piField THEN
            cDataType = ENTRY(iCounter,gcFieldListTypes).
    END.
    
    RETURN cDataType.
        
END FUNCTION.


FUNCTION replaceHas RETURNS CHARACTER PRIVATE
    (INPUT piQueryString AS CHARACTER):
/*------------------------------------------------------------------------------
 Purpose:
 Notes:
------------------------------------------------------------------------------*/    
    DEFINE VARIABLE iPosStart      AS INTEGER   NO-UNDO.
    DEFINE VARIABLE iPosEnd        AS INTEGER   NO-UNDO.
    DEFINE VARIABLE iPosBeginQuote AS INTEGER   NO-UNDO.
    DEFINE VARIABLE cHasPart       AS CHARACTER NO-UNDO.
    DEFINE VARIABLE cHasField      AS CHARACTER NO-UNDO.
    DEFINE VARIABLE cHasValue      AS CHARACTER NO-UNDO.
    DEFINE VARIABLE iPosDot        AS INTEGER   NO-UNDO.
    DEFINE VARIABLE replaceString  AS CHARACTER NO-UNDO.

    DO WHILE INDEX(piQueryString, "has ") > 0:
        iPosStart = INDEX(piQueryString,"has ").
        iPosBeginQuote = INDEX(piQueryString, "'", iPosStart).
        iPosEnd   = INDEX(piQueryString, "'", iPosBeginQuote + 1).  
        
        cHasPart = SUBSTRING(piQueryString,iPosStart + 4, iPosEnd - (iPosStart + 4)).
        
        cHasPart = REPLACE(cHasPart,gcTabel + "." , "").
        
        iPosDot  = INDEX(cHasPart, ".").
        
        cHasField = ENTRY(1,cHasPart,"'").
        cHasValue = ENTRY(2,cHasPart,"'").

        replaceString = SUBSTITUTE("STRING(&1.&2) MATCHES '*&3*'",
                                    gcTabel,
                                    cHasField, 
                                    cHasValue).
                                    
        piQueryString = REPLACE(piQueryString, 
                                SUBSTRING(piQueryString, iPosStart, (iPosEnd - iPosStart) + 1), 
                                replaceString).
    END.
    
    RETURN piQueryString.
        
END FUNCTION.

FUNCTION removeTableNameFromQueryStringIfNotQuoted RETURNS CHARACTER PRIVATE
    (INPUT piQueryString AS CHARACTER):
/*------------------------------------------------------------------------------
 Purpose:
 Notes:
------------------------------------------------------------------------------*/    
    DEFINE VARIABLE iPos         AS INTEGER   NO-UNDO.
    DEFINE VARIABLE cResult      AS CHARACTER NO-UNDO.
    DEFINE VARIABLE lInsideQuote AS LOGICAL   NO-UNDO.
    DEFINE VARIABLE cCurrentValue AS CHARACTER   NO-UNDO.
    
    // Loop door elk karakter in de string 
    DO iPos = 1 TO LENGTH(piQueryString):
        
        cCurrentValue = SUBSTRING(piQueryString, iPos, 1).
        
        IF cCurrentValue = "'" THEN
            lInsideQuote = NOT lInsideQuote.
    
        // Als we buiten quotes zijn, vervang de tabelnaam
        IF NOT lInsideQuote THEN DO:
            // Controleer of de tekst begint met cTabel + "."
            IF SUBSTRING(piQueryString, iPos, LENGTH(gcTabel) + 1) = gcTabel + "." THEN
                // Sla de tabelnaam over en vervang het door niets
                iPos = iPos + LENGTH(gcTabel).
            ELSE
                cResult = cResult + SUBSTRING(piQueryString, iPos, 1).
        END.
        ELSE
            // Voeg gewoon toe als we binnen quotes zijn
            cResult = cResult + SUBSTRING(piQueryString, iPos, 1).
    END.
    
    RETURN cResult.
        
END FUNCTION.

FUNCTION replaceEndsWith RETURNS CHARACTER PRIVATE
    (INPUT piQueryString AS CHARACTER):
/*------------------------------------------------------------------------------
 Purpose:
 Notes:
------------------------------------------------------------------------------*/        
    DEFINE VARIABLE iPosStart      AS INTEGER   NO-UNDO.
    DEFINE VARIABLE iPosEnd        AS INTEGER   NO-UNDO.
    DEFINE VARIABLE cEndsWithPart  AS CHARACTER NO-UNDO.
    DEFINE VARIABLE cEndsWithField AS CHARACTER NO-UNDO.
    DEFINE VARIABLE cEndsWithValue AS CHARACTER NO-UNDO.
    DEFINE VARIABLE replaceString  AS CHARACTER NO-UNDO.
    DEFINE VARIABLE i              AS INTEGER   NO-UNDO.            
    
    DO WHILE INDEX(piQueryString, "endswith(") > 0:
        
        iPosStart = INDEX(piQueryString,"endswith(").
        iPosEnd   = INDEX(piQueryString,")", iPosStart). //vind t sluit haakje beginnen bij startposititie: iPosStart.  
        
        /* +9: Omdat je de string endswith( wilt overslaan, tel je 9 op bij de startpositie. De lengte van het woord endswith( is 9 tekens!
           iPosEnd - (iPosStart + 9): Dit bepaalt de lengte van de substring. De lengte wordt berekend door het verschil te nemen tussen de positie van de sluitende haak en de startpositie van de parameters van endswith(. */ 

        cEndsWithPart = SUBSTRING(piQueryString,iPosStart + 9, iPosEnd - (iPosStart + 9)).  
        
        cEndsWithField = ENTRY(1,cEndsWithPart,","). //CustNum                        
        cEndsWithValue = ENTRY(2,cEndsWithPart,","). //3000 
        
        cEndsWithValue = TRIM(cEndsWithValue,"'").
        
        replaceString  = SUBSTITUTE("&1 MATCHES '*&2'", 
                                    "STRING(" + cEndsWithField + ")", 
                                    cEndsWithValue).
                                   
        
        piQueryString = REPLACE(piQueryString,
                                SUBSTRING(piQueryString, iPosStart, (iPosEnd - iPosStart) + 1),
                                replaceString).
    END.        

    RETURN piQueryString.
        
END FUNCTION.