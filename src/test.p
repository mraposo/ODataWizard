

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

DEFINE VARIABLE  piQueryString  AS CHARACTER NO-UNDO.
DEFINE VARIABLE  piTabel        AS CHARACTER NO-UNDO.

DEFINE VARIABLE iCounter        AS INTEGER   NO-UNDO.
DEFINE VARIABLE cTabel          AS CHARACTER NO-UNDO.
DEFINE VARIABLE cFieldName      AS CHARACTER NO-UNDO.
DEFINE VARIABLE cFieldList      AS CHARACTER NO-UNDO.
DEFINE VARIABLE dbBuffer        AS HANDLE    NO-UNDO.
DEFINE VARIABLE cFieldType      AS CHARACTER NO-UNDO.
DEFINE VARIABLE cFieldListTypes AS CHARACTER NO-UNDO.
DEFINE VARIABLE cFieldListNames AS CHARACTER NO-UNDO.
DEFINE VARIABLE cQueryString    AS CHARACTER NO-UNDO.

piQueryString = "(CustNum gt 4000 and endswith(CustNum,5) and has Customer.CreditLimit'335') or Customer.CustNum gt 5000 and endswith(Name,'st') or has Customer.Name'Mario'".
piTabel = "Customer".

cQueryString = piQueryString.
cTabel       = piTabel.

cQueryString = removeTableNameFromQueryStringIfNotQuoted(cQueryString).

CREATE BUFFER dbBuffer FOR TABLE cTabel.

DO iCounter = 1 TO dbBuffer:NUM-FIELDS:
    
    cFieldName = dbBuffer:BUFFER-FIELD(iCounter):NAME.
    cFieldType = dbBuffer:BUFFER-FIELD(iCounter):DATA-TYPE.
    
    cFieldListNames = IF iCounter > 1 
                        THEN cFieldListNames + "," + cFieldName 
                        ELSE cFieldName.
                        
    cFieldListTypes = IF iCounter > 1 
                        THEN cFieldListTypes + "," + cFieldType 
                        ELSE cFieldType.                      
END.

DO iCounter = 1 TO NUM-ENTRIES(cFieldListNames):
    cQueryString = REPLACE(cQueryString, ENTRY(iCounter,cFieldListNames), cTabel + "." + ENTRY(iCounter,cFieldListNames)).
END.

IF INDEX(cQueryString,"endswith(") <> 0 THEN
    cQueryString = replaceEndsWith(cQueryString).


IF INDEX(cQueryString,"has ") <> 0 THEN
    cQueryString = replaceHas(cQueryString).

MESSAGE "cQueryString = " cQueryString
    VIEW-AS ALERT-BOX.






/* ************************  Function Implementations ***************** */
FUNCTION getDataType RETURNS CHARACTER PRIVATE
    (INPUT piField AS CHARACTER):
/*------------------------------------------------------------------------------
 Purpose:
 Notes:
------------------------------------------------------------------------------*/    
    DEFINE VARIABLE iCounter  AS INTEGER NO-UNDO.
    DEFINE VARIABLE cDataType AS CHARACTER NO-UNDO.
    
    DO iCounter = 1 TO NUM-ENTRIES(cFieldListNames):

    IF ENTRY(iCounter,cFieldListNames) = piField THEN
        cDataType = ENTRY(iCounter,cFieldListTypes).
    END.
    
    RETURN cDataType.
        
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
    
    /* Loop door elk karakter in de string */
    DO iPos = 1 TO LENGTH(piQueryString):
        /* Kijk of we een quote tegenkomen */
        
        cCurrentValue = SUBSTRING(piQueryString, iPos, 1).
        
        IF cCurrentValue = "'" THEN 
            lInsideQuote = NOT lInsideQuote.
    
        /* Als we buiten quotes zijn, vervang de tabelnaam */
        IF NOT lInsideQuote THEN DO:
            /* Controleer of de tekst begint met cTabel + "." */
            IF SUBSTRING(piQueryString, iPos, LENGTH(cTabel) + 1) = cTabel + "." THEN DO:
                /* Sla de tabelnaam over en vervang het door niets */
                iPos = iPos + LENGTH(cTabel).
            END.
            ELSE
                cResult = cResult + SUBSTRING(piQueryString, iPos, 1).
        END.
        ELSE
            /* Voeg gewoon toe als we binnen quotes zijn */
            cResult = cResult + SUBSTRING(piQueryString, iPos, 1).
    END.
    
    /* Resultaat tonen */
    RETURN cResult.
        
END FUNCTION.

FUNCTION replaceEndsWith RETURNS CHARACTER PRIVATE
    (INPUT piQueryString AS CHARACTER):
/*----------------------------------------------------------*/    
    DEFINE VARIABLE iPosStart      AS INTEGER   NO-UNDO.
    DEFINE VARIABLE iPosEnd        AS INTEGER   NO-UNDO.
    DEFINE VARIABLE cEndsWithPart  AS CHARACTER NO-UNDO.
    DEFINE VARIABLE cEndsWithField AS CHARACTER NO-UNDO.
    DEFINE VARIABLE cEndsWithValue AS CHARACTER NO-UNDO.
    DEFINE VARIABLE replaceString  AS CHARACTER NO-UNDO.
    DEFINE VARIABLE i AS INTEGER NO-UNDO.            
    DEFINE VARIABLE cDataType AS CHARACTER NO-UNDO.
/*DEBUGGER:INITIATE(). */
/*DEBUGGER:SET-BREAK().*/

    DO WHILE INDEX(piQueryString, "endswith(") > 0:
        
        iPosStart = INDEX(piQueryString,"endswith(").
        iPosEnd   = INDEX(piQueryString,")", iPosStart). //vind t sluit haakje beginnen bij startposititie: iPosStart.  
        
        // +9: Omdat je de string endswith( wilt overslaan, tel je 9 op bij de startpositie. De lengte van het woord endswith( is 9 tekens!
        // iPosEnd - (iPosStart + 9): Dit bepaalt de lengte van de substring. De lengte wordt berekend door het verschil te nemen tussen de positie van de sluitende haak en de startpositie van de parameters van endswith(.        
        cEndsWithPart = SUBSTRING(piQueryString,iPosStart + 9, iPosEnd - (iPosStart + 9)). //shiittt
        
        //cEndsWithPart = TRIM(cEndsWithPart,"endswith()").        
        
        cEndsWithField = ENTRY(1,cEndsWithPart,","). //CustNum
        
        DO i = 1 TO NUM-ENTRIES(cFieldListNames):

            IF ENTRY(i,cFieldListNames) = ENTRY(2,cEndsWithField,".") THEN
                cDataType = ENTRY(i,cFieldListTypes).
        END.
        
/*        MESSAGE  "cEndsWithField = " cEndsWithField SKIP*/
/*                 "cDataType = " cDataType               */
/*            VIEW-AS ALERT-BOX.                          */

        
        cEndsWithValue = ENTRY(2,cEndsWithPart,","). 
        cEndsWithValue = TRIM(cEndsWithValue,"'").
        
        replaceString  = SUBSTITUTE("&1 MATCHES '*&2'",
                                   IF cDataType <> "character" 
                                        THEN "STRING(" + cEndsWithField + ")" 
                                        ELSE cEndsWithField,
                                   cEndsWithValue).
                                   
        
        piQueryString = REPLACE(piQueryString,
                                SUBSTRING(piQueryString, iPosStart, (iPosEnd - iPosStart) + 1),
                                replaceString).
    END.        

    RETURN piQueryString.
        
END FUNCTION.

FUNCTION replaceHas RETURNS CHARACTER PRIVATE
    (INPUT piQueryString AS CHARACTER):
/*------------------------------------------------------------------------------
 Purpose:
 Notes:
------------------------------------------------------------------------------*/    
    DEFINE VARIABLE iPosStart      AS INTEGER   NO-UNDO.
    DEFINE VARIABLE iPosEnd        AS INTEGER   NO-UNDO.
    DEFINE VARIABLE cEndsWithPart  AS CHARACTER NO-UNDO.
    DEFINE VARIABLE iPosBeginQuote AS INTEGER   NO-UNDO.
    DEFINE VARIABLE cHasPart       AS CHARACTER NO-UNDO.
    DEFINE VARIABLE cHasField      AS CHARACTER NO-UNDO.
    DEFINE VARIABLE cHasValue      AS CHARACTER NO-UNDO.
    DEFINE VARIABLE iPosDot        AS INTEGER   NO-UNDO.
    DEFINE VARIABLE replaceString  AS CHARACTER NO-UNDO.
    DEFINE VARIABLE cDataType      AS CHARACTER NO-UNDO.
//QueryString = has Customer.testfield'Albertoo'

/*DEBUGGER:INITIATE(). */
/*DEBUGGER:SET-BREAK().*/

    MESSAGE "1 picQuertString = " piQueryString 
    VIEW-AS ALERT-BOX.
    DO WHILE INDEX(piQueryString, "has ") > 0:
        iPosStart = INDEX(piQueryString,"has ").
        iPosBeginQuote = INDEX(piQueryString, "'", iPosStart).
        iPosEnd   = INDEX(piQueryString, "'", iPosBeginQuote + 1).  
        
        cHasPart = SUBSTRING(piQueryString,iPosStart + 4, iPosEnd - (iPosStart + 4)).
        
        cHasPart = REPLACE(cHasPart,cTabel + "." , "").
        
        iPosDot  = INDEX(cHasPart, ".").
        
        //cHasField = TRIM(SUBSTRING(cHasPart, iPosDot + 1, iPosBeginQuote - iPosDot - 4),"'").
        cHasField = ENTRY(1,cHasPart,"'").
        
        //cHasValue = SUBSTRING(cQueryString,iPosBeginQuote, R-INDEX(cQueryString, ".", iPosBeginQuote ) + 1 ) .
        cHasValue = ENTRY(2,cHasPart,"'").
        
        cDataType = getDataType(cHasField).
        
        
        MESSAGE "cHasPart  = " cHasPart  SKIP
                "cHasField = " cHasField SKIP
                "cHasValue = " cHasValue SKIP 
                "cDataType = " cDataType
            VIEW-AS ALERT-BOX.
            
         replaceString = SUBSTITUTE("&1 MATCHES '*&2*'",
                                    IF cDataType <> "character"
                                        THEN "STRING(" + cHasField + ")"
                                        ELSE cHasField,
                                    cHasValue).
                                    
         piQueryString = REPLACE(piQueryString, SUBSTRING(piQueryString, iPosStart, (iPosEnd - iPosStart) + 1), replaceString).
         
         MESSAGE "2 piQueryString = " piQueryString
         VIEW-AS ALERT-BOX.
    END.
    
    
    RETURN piQueryString.

END FUNCTION.
