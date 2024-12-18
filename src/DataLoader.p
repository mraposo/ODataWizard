
/*------------------------------------------------------------------------
    File        : DataLoader.p
    Purpose     : 

    Syntax      :

    Description : 

    Author(s)   : mario
    Created     : Mon Dec 16 14:46:13 CET 2024
    Notes       :
  ----------------------------------------------------------------------*/

/* ************************  Function Prototypes ********************** */

FUNCTION makeQuery RETURNS HANDLE PRIVATE
    (INPUT cTabel AS CHARACTER,
     INPUT cField AS CHARACTER) FORWARD.

FUNCTION getOperatorAndValue RETURNS CHARACTER PRIVATE 
    (INPUT cQueryString AS CHARACTER,
     INPUT cOperator AS CHARACTER) FORWARD.

FUNCTION getField2 RETURNS CHARACTER  PRIVATE
    (INPUT cQueryString AS CHARACTER) FORWARD.

FUNCTION URIhasFilter RETURNS LOGICAL PRIVATE
    (  ) FORWARD.

FUNCTION getWhereClause RETURNS CHARACTER PRIVATE
    (INPUT cFilter AS CHARACTER,
    INPUT cTabel AS CHARACTER) FORWARD.


/* ***************************  Main Block  *************************** */
DEFINE INPUT  PARAMETER poRequest AS OpenEdge.Web.IWebRequest NO-UNDO.
DEFINE OUTPUT PARAMETER p-output  AS LONGCHAR NO-UNDO.

DEFINE VARIABLE gcURI     AS CHARACTER NO-UNDO.
DEFINE VARIABLE gcPath    AS CHARACTER NO-UNDO.
DEFINE VARIABLE gcTabel   AS CHARACTER NO-UNDO.
DEFINE VARIABLE gcFilter AS CHARACTER NO-UNDO.
DEFINE VARIABLE gcWhereClause AS CHARACTER NO-UNDO.

   
ASSIGN
    gcPath  = poRequest:PathInfo
    gcURI   = STRING(poRequest:URI)    
    gcTabel = ENTRY(3,gcPath,"/").

IF URIhasFilter() THEN
    ASSIGN 
        gcFilter      = ENTRY(2,gcURI,"?")
        gcWhereClause = IF URIhasFilter() 
                            THEN getWhereClause(gcFilter, gcTabel) 
                            ELSE "".    

/*MESSAGE "gcTabel ========" gcTabel SKIP */
/*        "gcPath =========" gcPath SKIP  */
/*        "gcUri ==========" gcURI    SKIP*/
/*        "gcFilter =======" gcFilter     */
/*    VIEW-AS ALERT-BOX.                  */

RUN createOutputDataset.




/* **********************  Internal Procedures  *********************** */
PROCEDURE createOutputDataset:
    /*------------------------------------------------------------------------------
     Purpose:
     Notes:
    ------------------------------------------------------------------------------*/

    DEFINE VARIABLE hQuery     AS HANDLE NO-UNDO.
    DEFINE VARIABLE hBufferDB  AS HANDLE NO-UNDO.
    DEFINE VARIABLE hTempTable AS HANDLE NO-UNDO.
    DEFINE VARIABLE hBufferTT  AS HANDLE NO-UNDO.
    DEFINE VARIABLE hOutputDataset AS HANDLE NO-UNDO.
    DEFINE VARIABLE hDatasource    AS HANDLE NO-UNDO.
    
    
    CREATE DATASET hOutputDataset.
    
    hOutputDataset:SERIALIZE-NAME = "oData".
    
    CREATE DATA-SOURCE hDatasource.
    
    CREATE QUERY hQuery.
    
    CREATE BUFFER hBufferDB FOR TABLE gcTabel.
    
    CREATE TEMP-TABLE hTempTable.
    
    hTempTable:CREATE-LIKE (hBufferDB).
    
    hTempTable:TEMP-TABLE-PREPARE (gcTabel).
    
    hBufferTT = hTempTable:DEFAULT-BUFFER-HANDLE.
    
    hOutputDataset:ADD-BUFFER (hBufferTT).
    
    hQuery:SET-BUFFERS(hBufferDB).
    hQuery:QUERY-PREPARE (SUBSTITUTE("FOR EACH &1 NO-LOCK &2",gcTabel,gcWhereClause)).
    
    hDatasource:QUERY = hQuery.
        
    hBufferTT:ATTACH-DATA-SOURCE (hDatasource).   
    
    hOutputDataset:FILL(). 
        
    hOutputDataset:WRITE-JSON("longchar",p-output,TRUE,?).
        
    hQuery:QUERY-CLOSE(). 
    
        
    FINALLY:
        IF VALID-OBJECT(hOutputDataset) THEN
            DELETE OBJECT hOutputDataset.
        IF VALID-OBJECT(hQuery) THEN
            DELETE OBJECT hQuery.
        IF VALID-OBJECT(hDatasource) THEN
            DELETE OBJECT hDatasource.
        IF VALID-OBJECT(hTempTable) THEN
            DELETE OBJECT hTempTable.
        IF VALID-OBJECT(hBufferDB) THEN
            DELETE OBJECT hBufferDB.
        IF VALID-OBJECT(hBufferTT) THEN
            DELETE OBJECT hBufferTT.                
    END FINALLY.
    
END PROCEDURE.


/* ************************  Function Implementations ***************** */

FUNCTION makeQuery RETURNS HANDLE PRIVATE
    (INPUT cTabel AS CHARACTER, INPUT cField AS CHARACTER):
/*------------------------------------------------------------------------------
 Purpose:
 Notes:
------------------------------------------------------------------------------*/    

        DEFINE VARIABLE hQuery         AS HANDLE NO-UNDO.
        DEFINE VARIABLE cPrepareString AS CHARACTER NO-UNDO.
        
        cPrepareString = SUBSTITUTE("FOR EACH &1", cTabel).
        CREATE QUERY hQuery.
        
        hQuery:SET-BUFFERS (cTabel).
        hQuery:QUERY-PREPARE (cPrepareString).
        
        RETURN hQuery.

END FUNCTION.


FUNCTION getOperatorAndValue RETURNS CHARACTER PRIVATE
    (INPUT cQueryString AS CHARACTER, INPUT cOperator AS CHARACTER):
    /*------------------------------------------------------------------------------
     Purpose:
     Notes:
    ------------------------------------------------------------------------------*/    

    DEFINE VARIABLE cOperAndValue AS CHARACTER NO-UNDO.
    
    ASSIGN
        cOperAndValue = SUBSTRING(cQueryString, INDEX(cQueryString,cOperator))
        cOperAndValue = REPLACE(cOperAndValue,cOperator, "")
        cOperAndValue = REPLACE(cOperAndValue,getField2(cQueryString), "").
        
    RETURN cOperAndValue.
    
END FUNCTION.


FUNCTION getField2 RETURNS CHARACTER PRIVATE
    (INPUT cQueryString AS CHARACTER):
    /*------------------------------------------------------------------------------
     Purpose:
     Notes:  
    ------------------------------------------------------------------------------*/    

    DEFINE VARIABLE cField1          AS CHARACTER NO-UNDO.
    DEFINE VARIABLE cField2          AS CHARACTER NO-UNDO.
    DEFINE VARIABLE cOperator        AS CHARACTER NO-UNDO.
    DEFINE VARIABLE cOperatorsList   AS CHARACTER NO-UNDO INIT "eq,ne,gt,ge,lt,le".
    DEFINE VARIABLE iCounter         AS INTEGER   NO-UNDO.
    DEFINE VARIABLE cCurrentOperator AS CHARACTER NO-UNDO.
    
    cOperator = IF INDEX(cQueryString," and ") <> 0 THEN " and " ELSE " or ". //hier moeten spacties voor en achter!

    ASSIGN 
        cField1 = ENTRY(1,cQueryString," ")
        cField2 = SUBSTRING(cQueryString, INDEX(cQueryString,cOperator))                                     
        cField2 = TRIM(REPLACE(cField2,cOperator, ""))
        .
        
    DO iCounter = 1 TO NUM-ENTRIES(cOperatorsList,","):
         
        cCurrentOperator = ENTRY(1,cField2, " ").   
         
        IF cCurrentOperator = ENTRY(iCounter,cOperatorsList) THEN
            RETURN cField1.         
    END.
    
    cField2 = ENTRY(1,cField2," "). 
    
    RETURN cField2.
         
END FUNCTION.


FUNCTION URIhasFilter RETURNS LOGICAL PRIVATE
    (  ):
    /*------------------------------------------------------------------------------
     Purpose:
     Notes:
    ------------------------------------------------------------------------------*/    
  
    RETURN INDEX(gcURI,"?$filter") <> 0.  //?filter
        
END FUNCTION.


FUNCTION getWhereClause RETURNS CHARACTER PRIVATE
    (INPUT cFilter AS CHARACTER, INPUT cTabel AS CHARACTER):
    /*------------------------------------------------------------------------------
     Purpose:
     Notes:
    ------------------------------------------------------------------------------*/    
    DEFINE VARIABLE cWhereClause  AS CHARACTER NO-UNDO.
    DEFINE VARIABLE cQueryString  AS CHARACTER NO-UNDO.
    DEFINE VARIABLE cFilter2      AS CHARACTER NO-UNDO.
    DEFINE VARIABLE cOperator     AS CHARACTER NO-UNDO.
    DEFINE VARIABLE cOperAndValue AS CHARACTER NO-UNDO.
    DEFINE VARIABLE cField        AS CHARACTER NO-UNDO.
    DEFINE VARIABLE cParams       AS CHARACTER NO-UNDO.
    DEFINE VARIABLE iPos          AS INTEGER   NO-UNDO.
    DEFINE VARIABLE iPosEind      AS INTEGER   NO-UNDO.
    DEFINE VARIABLE cValue        AS CHARACTER NO-UNDO.
    DEFINE VARIABLE cResult       AS CHARACTER NO-UNDO.  
      
    cQueryString = REPLACE(cFilter,"$filter=","").
    
   
    IF INDEX(cQueryString," and ") <> 0 OR 
       INDEX(cQueryString," or ")  <> 0 THEN DO:  
        
        cOperator = IF INDEX(cQueryString," and ") <> 0 THEN "and " ELSE "or ". //hier moeten GEEN spacties voor!!

        ASSIGN
            cFilter2     = SUBSTITUTE("&1.&2", 
                                      cTabel,    
                                      getField2(cQueryString)).
            cQueryString = SUBSTITUTE("&1 &2 &3 &4",
                                      ENTRY(1,cQueryString,cOperator),          
                                      cOperator,
                                      cFilter2,
                                      getOperatorAndValue(cQueryString,cOperator)).
    END.
    ELSE IF INDEX(cQueryString,"endswith(") <> 0 THEN DO:  // NOT?!?!
        
        iPos     = INDEX(cQueryString,"(").
        iPosEind = INDEX(cQueryString,")").
        cParams  = TRIM(SUBSTRING(cQueryString,iPos,iPosEind),"()").
        
        cField   = ENTRY(1,cParams).
        cValue   = TRIM(ENTRY(2,cParams),"'").

        cQueryString = SUBSTITUTE("&1 MATCHES('*&2')",
                                 cField,
                                 cValue).
        
    END.
    ELSE IF INDEX(cQueryString,"has ") <> 0 THEN DO:
        //has Customer.Name'Mario'
        
        cField = ENTRY(2,cQueryString,".").
        cField = ENTRY(1,cField,"'").
        
        cValue = TRIM(ENTRY(2,cQueryString,"'"), "'").
        
        cQueryString = SUBSTITUTE("&1 MATCHES('*&2*')",
                                 cField,
                                 cValue).
                                  
                                          
        MESSAGE "cField = " cField SKIP 
                "cValue = " cValue SKIP 
                "cQueryString" cQueryString 
            VIEW-AS ALERT-BOX.                                  
    END. 

    cWhereClause = SUBSTITUTE("WHERE &1.&2",
                              cTabel,
                              cQueryString).

    RETURN cWhereClause.
        
END FUNCTION.

         