
/*------------------------------------------------------------------------
    File        : DataLoader.p
    Purpose     : 

    Syntax      :

    Description : 

    Author(s)   : mario
    Created     : Mon Dec 16 14:46:13 CET 2024
    Notes       :
  ----------------------------------------------------------------------*/

/* ***************************  Definitions  ************************** */


/* ********************  Preprocessor Definitions  ******************** */

/* ************************  Function Prototypes ********************** */


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

MESSAGE "gatewayData.p >>>>>>>>>>>>" SKIP 
        "PathParameterNames =======" poRequest:PathParameterNames SKIP
        "poRequest:RemoteAddress ==" poRequest:RemoteAddress SKIP
        "ResolvedWebAppPath =======" poRequest:ResolvedWebAppPath SKIP
        "URI  =====================" poRequest:URI SKIP
        "poRequest:PathInfo =======" poRequest:PathInfo SKIP 
        "WebAppPath ===============" poRequest:WebAppPath SKIP
    VIEW-AS ALERT-BOX.

   
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
    hQuery:QUERY-OPEN.
    hQuery:GET-FIRST ().
    
    hDatasource:QUERY = hQuery.
        
    hBufferTT:ATTACH-DATA-SOURCE (hDatasource).   
    
    hOutputDataset:FILL(). 
        
    hOutputDataset:WRITE-JSON("longchar",p-output,TRUE,?).
        
    hQuery:QUERY-CLOSE(). 
    
        
    FINALLY:
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
    DEFINE VARIABLE cWhereClause AS CHARACTER NO-UNDO.
    
    cWhereClause = SUBSTITUTE("WHERE &1.&2 = &3",cTabel,
                                            ENTRY(2,cFilter,"="),
                                            ENTRY(3,cFilter,"=")).
                                                
    MESSAGE "cWhereClause = " cWhereClause
        VIEW-AS ALERT-BOX.

    RETURN cWhereClause.
        
END FUNCTION.

         