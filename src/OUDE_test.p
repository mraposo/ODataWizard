
/*------------------------------------------------------------------------
    File        : test.p
    Purpose     : 

    Syntax      :

    Description : 

    Author(s)   : mario
    Created     : Wed Dec 18 22:21:44 CET 2024
    Notes       :
  ----------------------------------------------------------------------*/

/* ***************************  Definitions  ************************** */


DEFINE TEMP-TABLE ttStringBuilder
    FIELD cFilter   AS CHARACTER
    FIELD cOperator AS CHARACTER  
    FIELD iVolgNr   AS INTEGER  
    INDEX VolgNr IS PRIMARY UNIQUE iVolgNr.
    
DEFINE TEMP-TABLE ttQueryString
    FIELD cString  AS CHARACTER
    FIELD iVolgNr  AS INTEGER  
    INDEX VolgNr IS PRIMARY UNIQUE iVolgNr.
    
/* ********************  Preprocessor Definitions  ******************** */

/* ************************  Function Prototypes ********************** */


FUNCTION addHooksRechts RETURNS CHARACTER PRIVATE
    (INPUT piSaveString AS CHARACTER,
     INPUT piOpenHaken AS INTEGER) FORWARD.

FUNCTION addHooksLinks RETURNS CHARACTER PRIVATE
    (INPUT piSaveString AS CHARACTER ,
     INPUT piOpenHaken AS INTEGER) FORWARD.

FUNCTION countBeginHaakjes RETURNS INTEGER PRIVATE
    (INPUT picQueryString AS CHARACTER,
     INPUT piOpenHaken AS INTEGER) FORWARD.

FUNCTION createQueryString RETURNS CHARACTER PRIVATE
    (INPUT cQueryString AS CHARACTER) FORWARD.

FUNCTION ExtractNestedString RETURNS CHARACTER PRIVATE
    (INPUT cQueryString AS CHARACTER) FORWARD.

FUNCTION checkNestLevelAndPosition RETURNS CHARACTER PRIVATE
    (INPUT cQueryString AS CHARACTER) FORWARD.

FUNCTION createTTStringBuilder RETURNS CHARACTER PRIVATE
    (INPUT piQueryString AS CHARACTER) FORWARD.


/* ***************************  Main Block  *************************** */

/* ************************  Function Implementations ***************** */



DEFINE VARIABLE cQueryString          AS CHARACTER NO-UNDO.
DEFINE VARIABLE i                     AS INTEGER   NO-UNDO.
DEFINE VARIABLE cValue                AS CHARACTER NO-UNDO.
DEFINE VARIABLE iPositionOpen         AS INTEGER   NO-UNDO.
DEFINE VARIABLE iPositionClose        AS INTEGER   NO-UNDO.
DEFINE VARIABLE cNestedCondition      AS CHARACTER NO-UNDO.
DEFINE VARIABLE VolgNr                AS INTEGER   NO-UNDO.
DEFINE VARIABLE iLevelNest            AS INTEGER   NO-UNDO INIT 1.
DEFINE VARIABLE cNestLevelAndPosition AS CHARACTER NO-UNDO.
DEFINE VARIABLE cSaveString           AS CHARACTER NO-UNDO.
DEFINE VARIABLE iIndex                AS INTEGER   NO-UNDO.
DEFINE VARIABLE iOpenHaken            AS INTEGER NO-UNDO.
DEFINE VARIABLE giNestLevel           AS INTEGER NO-UNDO.
DEFINE VARIABLE iPosLastHook          AS INTEGER NO-UNDO.

cQueryString = "((CustNum gt 4000 and CustNum lt 4100 or CustNum gt 5000 and CustNum lt 5100)) or CustNum gt 6000 and CustNum lt 6100".

// "CustNum gt 4000 and Name lt 'test()' or (CustNum gt 5000 and CustNum lt 5100)".
// "CustNum gt 4000 and CustNum lt 4100 or (CustNum gt 5000 and CustNum lt 5100)".
// "(CustNum gt 4000 and CustNum lt 4100) or CustNum gt 5000 and CustNum lt 5100".
// "CustNum gt 4000 and CustNum lt 4100 or CustNum gt 5000 and CustNum lt 5100".
// "((CustNum gt 4000 and CustNum lt 4100) or (CustNum gt 5000 and CustNum lt 5100))".
// "((CustNum gt 4000 and CustNum lt 4100) or (CustNum gt 5000 and endswith(Name,'ilk')))".
// "((CustNum gt 4000 and CustNum lt 4100) or (CustNum gt 5000 and has CustNum'3000'))".
//Products?$filter=not endswith(Name,'ilk')
//Products?$filter=style has Sales.Pattern'Yellow'


DEFINE VARIABLE ffDeWorkString AS CHARACTER NO-UNDO.
DEFINE VARIABLE iPosFirstHook  AS INTEGER NO-UNDO.

/*MESSAGE 1 countBeginHaakjes(cQueryString, 2)*/
/*VIEW-AS ALERT-BOX.                          */

/*DEBUGGER:INITIATE(). */
/*DEBUGGER:SET-BREAK().*/

DEFINE VARIABLE lHooksEverReachedTwo AS LOGICAL NO-UNDO INITIAL FALSE.


LOOP:
DO WHILE cQueryString <> "":    
    
    iPosFirstHook = INDEX(cQueryString,"(").
    iPosLastHook  = R-INDEX(cQueryString,")").
    
    ffDeWorkString = SUBSTRING(cQueryString,1,iPosFirstHook - 1).
    
    
    IF ffDeWorkString = "or "  THEN ffDeWorkString = " or (".
    IF ffDeWorkString = "and " THEN ffDeWorkString = " and (".
    
    IF ffDeWorkString <> "" THEN DO:
        IF INDEX(cQueryString, "or (")  = 1 OR         
           INDEX(cQueryString, "and (") = 1 THEN DO:
 
              IF INDEX(cQueryString, "or (") > 0 THEN DO:
                 cSaveString = " or ".
                 ffDeWorkString =  " or ".
              END.
              IF INDEX(cQueryString, "and (") > 0 THEN DO:
                 cSaveString = " and ".
                 ffDeWorkString =  " and ". 
              END.
        END.
        ELSE DO:                
        
          cSaveString = createTTStringBuilder(ffDeWorkString).
         //TODO: cSaveString opslaan in temp-table..
        
        END.
                 
         iIndex = iIndex + 1.

         CREATE ttQueryString.
         ASSIGN
            ttQueryString.cString = cSaveString
            ttQueryString.iVolgNr = iIndex.            
    END.

    
    ELSE DO:
        
        iOpenHaken = countBeginHaakjes(cQueryString, iOpenHaken).
        
        IF iOpenHaken >= 2 THEN
            lHooksEverReachedTwo = TRUE.
            
        ffDeWorkString = TRIM(ExtractNestedString(cQueryString),"()").
        
        
        //CustNum gt 5000 and CustNum lt 5100) or (CustNum gt 6000 and CustNum lt 6100
        
        IF INDEX(ffDeWorkString,"(") = 0 THEN DO:    
             //cSaveString = createTTStringBuilder(ffDeWorkString).
             //TODO: cSaveString opslaan in temp-table..
             
            cSaveString = "(" + TRIM(createTTStringBuilder(ffDeWorkString)) + ")".
             
/*            MESSAGE 2 addHooks(INPUT cSaveString, INPUT iOpenHaken, giNestLevel)*/
/*                VIEW-AS ALERT-BOX.                                              */
                
            cSaveString = addHooksLinks(INPUT cSaveString, INPUT iOpenHaken).
            
            IF iOpenHaken < 2 AND lHooksEverReachedTwo THEN   
                cSaveString = addHooksRechts(INPUT cSaveString, INPUT iOpenHaken).
            
             iIndex = iIndex + 1.
             
             CREATE ttQueryString.
             ASSIGN
                ttQueryString.cString = cSaveString
                ttQueryString.iVolgNr = iIndex.
                
             iOpenHaken = 0.   
        END.
        ELSE DO:
                      
            cQueryString = "(" + ffDeWorkString + ")".
            NEXT LOOP.
        END.
        
        ffDeWorkString = "(" + ffDeWorkString + ")".
        
    END.
         
    cQueryString = LEFT-TRIM(cQueryString,ffDeWorkString).
    
    //iIndex = iIndex + 1.
    
END.    

FOR EACH ttQueryString:
    MESSAGE ttQueryString.cString SKIP iVolgNr VIEW-AS ALERT-BOX.
END.

/*MESSAGE "1 ffDeWorkString =" ffDeWorkString SKIP                       */
/*        "1 cQueryString = " cQueryString                               */
/*    VIEW-AS ALERT-BOX.                                                 */
/*                                                                       */
/*ffDeWorkString = TRIM(ffDeWorkString,"()").                            */
/*                                                                       */
/*IF INDEX(ffDeWorkString,"(") = 0 THEN DO:                              */
/*    createQueryString(ffDeWorkString).                                 */
/*                                                                       */
/*    MESSAGE "2 ffDeWorkString =" ffDeWorkString SKIP                   */
/*            "2 cQueryString = " cQueryString                           */
/*        VIEW-AS ALERT-BOX.                                             */
/*END.                                                                   */
/*ELSE DO:                                                               */
/*    cQueryString = REPLACE(cQueryString,ffDeWorkString,"").            */
/*                                                                       */
/*    ffDeWorkString = ExtractNestedString(cQueryString).                */
/*                                                                       */
/*    MESSAGE "3 ffDeWorkString =" ffDeWorkString SKIP                   */
/*            "3 cQueryString = " cQueryString                           */
/*    VIEW-AS ALERT-BOX.                                                 */
/*END.                                                                   */
/*                                                                       */
/*MESSAGE "1" ExtractNestedString(cQueryString)                          */
/*    VIEW-AS ALERT-BOX.                                                 */
/*                                                                       */
/*                                                                       */
/*cNestedCondition = "(" + createTTStringBuilder(cNestedCondition) + ")".*/
/*                                                                       */
/*MESSAGE "cNestedCondition = " cNestedConditioN                         */
/*VIEW-AS ALERT-BOX.                                                     */
/*                                                                       */
/*CREATE ttQueryString.                                                  */
/*ASSIGN                                                                 */
/*    ttQueryString.cString = cNestedCondition.                          */
/*    ttQueryString.iVolgNr = VolgNr + 1.                                */
/*                                                                       */
/*MESSAGE "cQueryString = " cQueryString                                 */
/*VIEW-AS ALERT-BOX.                                                     */



FUNCTION addHooksRechts RETURNS CHARACTER PRIVATE
    (INPUT piSaveString AS CHARACTER, INPUT piOpenHaken AS INTEGER):
/*------------------------------------------------------------------------------
 Purpose:
 Notes:
------------------------------------------------------------------------------*/    
    
    IF piSaveString = "" THEN 
        RETURN piSaveString.
            
    piSaveString = SUBSTITUTE("&1&2",
                              piSaveString,
                              FILL(")", piOpenHaken)).
                              
                              
    RETURN piSaveString.
        


        
END FUNCTION.

FUNCTION addHooksLinks RETURNS CHARACTER PRIVATE
    (INPUT piSaveString AS CHARACTER, INPUT piOpenHaken AS INTEGER):
/*------------------------------------------------------------------------------
 Purpose:
 Notes:
------------------------------------------------------------------------------*/    
    
    IF piOpenHaken <= 1 OR piSaveString = "" THEN 
        RETURN piSaveString.
            
    piSaveString = SUBSTITUTE("&1&2",
                              FILL("(", piOpenHaken - 1),
                              piSaveString).
                              
                              
    RETURN piSaveString.
        
END FUNCTION.

FUNCTION countBeginHaakjes RETURNS INTEGER PRIVATE
    (INPUT picQueryString AS CHARACTER, INPUT piOpenHaken AS INTEGER):
/*------------------------------------------------------------------------------
 Purpose:
 Notes:
------------------------------------------------------------------------------*/    
    DEFINE VARIABLE iPosition AS INTEGER NO-UNDO.
    DEFINE VARIABLE iCount    AS INTEGER NO-UNDO.
    DEFINE VARIABLE iNumHooks AS INTEGER NO-UNDO.
    
    DO iCount = 1 TO LENGTH(cQueryString):
            
        cValue = SUBSTRING(cQueryString,iCount,1).
                 
        IF cValue = "(" THEN 
            iNumHooks = iNumHooks + 1.
        ELSE
            LEAVE.
        
    END.
    
/*    DO WHILE TRUE:                                         */
/*        iPosition = INDEX(cQueryString,"(", iPosition + 1).*/
/*        IF iPosition = 0 THEN                              */
/*            LEAVE.                                         */
/*        ELSE                                               */
/*            iCount = iCount + 1.                           */
/*    END.                                                   */

    IF iNumHooks >= piOpenHaken THEN
        RETURN iNumHooks.
    ELSE
        RETURN piOpenHaken.
        
END FUNCTION.

FUNCTION createQueryString RETURNS CHARACTER PRIVATE
    (INPUT cQueryString AS CHARACTER):
/*------------------------------------------------------------------------------
 Purpose:
 Notes:
------------------------------------------------------------------------------*/    
/*    DEFINE VARIABLE cTabel            AS CHARACTER NO-UNDO INIT "Customer".                                                    */
/*    DEFINE VARIABLE cBuildQueryString AS CHARACTER NO-UNDO.                                                                    */
/*                                                                                                                               */
/*    FOR EACH ttStringBuilder BY iVolgNr:                                                                                       */
/*                                                                                                                               */
/*        cBuildQueryString = cBuildQueryString + cTabel + "." + ttStringBuilder.cFilter + " " + ttStringBuilder.cOperator + " ".*/
/*                                                                                                                               */
/*    /*    DISPLAY ttStringBuilder.cFilter   FORMAT "x(30)"*/                                                                   */
/*    /*            ttStringBuilder.cOperator FORMAT "x(10)"*/                                                                   */
/*    /*            ttStringBuilder.iVolgNr.                */                                                                   */
/*    END.                                                                                                                       */
        
END FUNCTION.

FUNCTION ExtractNestedString RETURNS CHARACTER PRIVATE
    (INPUT cQueryString AS CHARACTER):
/*------------------------------------------------------------------------------
 Purpose:
 Notes:
------------------------------------------------------------------------------*/    
    DEFINE VARIABLE iCountNestLevel AS INTEGER NO-UNDO.
    DEFINE VARIABLE cCurrentFilter  AS CHARACTER NO-UNDO.
    
    DO i = 1 TO LENGTH(cQueryString):
            
        cValue = SUBSTRING(cQueryString, i, 1).
        
        CASE cValue:
            WHEN "(" THEN DO:
                iCountNestLevel = iCountNestLevel + 1.
            END.
            WHEN ")" THEN DO:
                iCountNestLevel = iCountNestLevel - 1.
    
            END.
       END CASE.
       
        
       IF iCountNestLevel <> 0 THEN DO:
            giNestLevel = iCountNestLevel.
            cCurrentFilter = cCurrentFilter + SUBSTRING(cQueryString,i,1).
       END.
       ELSE IF cCurrentFilter <> "" THEN DO:  
                    
            //cCurrentFilter = "".                    
            RETURN cCurrentFilter + ")".
       END.
    END.

        
END FUNCTION.

FUNCTION checkNestLevelAndPosition RETURNS CHARACTER PRIVATE
    (INPUT cQueryString AS CHARACTER   ):
/*------------------------------------------------------------------------------
 Purpose:
 Notes:
------------------------------------------------------------------------------*/    

        DEFINE VARIABLE result AS CHARACTER NO-UNDO.

        RETURN result.


        
END FUNCTION.

FUNCTION createTTStringBuilder RETURNS CHARACTER PRIVATE
    (INPUT piQueryString AS CHARACTER):
/*------------------------------------------------------------------------------
 Purpose:
 Notes:
------------------------------------------------------------------------------*/    
    DEFINE VARIABLE cCondition        AS CHARACTER NO-UNDO.
    DEFINE VARIABLE cOperator         AS CHARACTER NO-UNDO.
    DEFINE VARIABLE iVolgNr           AS INTEGER   NO-UNDO.
    DEFINE VARIABLE cRemainingString  AS CHARACTER NO-UNDO.
    DEFINE VARIABLE cTabel            AS CHARACTER NO-UNDO INIT "Customer".
    DEFINE VARIABLE cBuildQueryString AS CHARACTER NO-UNDO.

    EMPTY TEMP-TABLE ttStringBuilder.

    DO WHILE piQueryString <> "":
        
/*        DEBUGGER:INITIATE(). */
/*        DEBUGGER:SET-BREAK().*/
      
        iVolgNr = iVolgNr + 1.
        
        // Controleer of "or" aanwezig is in de string. 
        IF INDEX(piQueryString, " or ") > 0 AND 
        // Zolang "or" eerder voorkomt dan "and", of als "and" niet aanwezig is, dan hebben we een "or" te pakken.  
          (INDEX(piQueryString, " or ") < INDEX(piQueryString, " and ") OR INDEX(piQueryString, " and ") = 0) THEN DO:
            
            cCondition = TRIM(SUBSTRING(piQueryString, 1, INDEX(piQueryString, " or "))).
            cOperator = "or".
            piQueryString = SUBSTRING(piQueryString, INDEX(piQueryString, " or ") + 4). // 4 =  skip " or " 
        END.
        ELSE IF INDEX(piQueryString, " and ") > 0 THEN DO:
            cCondition = TRIM(SUBSTRING(piQueryString, 1, INDEX(piQueryString, " and "))).
            cOperator = "and".
            piQueryString = SUBSTRING(piQueryString, INDEX(piQueryString, " and ") + 5). // 5 = skip " and " 
        END.
        ELSE DO: 
            // Laatste conditie krijgt geen operator
            cCondition = TRIM(piQueryString).
            cOperator = "".  //leeg
            piQueryString = "". //maak string leeg, en leave loop
        END.
    
        CREATE ttStringBuilder.
        ASSIGN
            ttStringBuilder.cFilter   = cCondition
            ttStringBuilder.cOperator = cOperator
            ttStringBuilder.iVolgNr   = iVolgNr.
            
            
/*        MESSAGE "piQueryString = " piQueryString*/
/*            VIEW-AS ALERT-BOX.                  */
    END.
    

    FOR EACH ttStringBuilder BY iVolgNr:
        
        cBuildQueryString = cBuildQueryString + cTabel + "." + ttStringBuilder.cFilter + " " + ttStringBuilder.cOperator + " ".        
        
    /*    DISPLAY ttStringBuilder.cFilter   FORMAT "x(30)"*/
    /*            ttStringBuilder.cOperator FORMAT "x(10)"*/
    /*            ttStringBuilder.iVolgNr.                */
    END.
    
    RETURN cBuildQueryString.
        
END FUNCTION.