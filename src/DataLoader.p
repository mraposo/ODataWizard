
/*------------------------------------------------------------------------
    File        : DataLoader.p
    Purpose     : 

    Syntax      :

    Description : 

    Author(s)   : Mario & Levi
    Created     : Mon Dec 16 14:46:13 CET 2024
    Notes       :
  ----------------------------------------------------------------------*/
DEFINE TEMP-TABLE ttETag NO-UNDO
  FIELD rawhack AS RAW. 

/* ************************  Function Prototypes ********************** */
FUNCTION HashRecord RETURNS CHARACTER PRIVATE
    (INPUT hBuffer AS HANDLE) FORWARD.

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
        gcWhereClause = getWhereClause(gcFilter, gcTabel). 


RUN createOutputDataset.


/* **********************  Internal Procedures  *********************** */
PROCEDURE generateETag:
/*------------------------------------------------------------------------------
 Notes: Hoe verzin je een record versie als OpenEdge die zelf niet bijhoudt ?
        Het hele database record hashen is een quick & ditry oplossing - als het record verandert, krijg je een andere hash waarde 
------------------------------------------------------------------------------*/
    DEFINE INPUT PARAMETER DATASET-HANDLE hDataSet. 
    
    SELF::Etag = HashRecord(SELF:DATA-SOURCE:GET-SOURCE-BUFFER()).

END PROCEDURE.


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
    hTemptable:ADD-NEW-FIELD("Etag","CHARACTER").
    hTempTable:TEMP-TABLE-PREPARE (gcTabel).
    
    hBufferTT = hTempTable:DEFAULT-BUFFER-HANDLE.
 
    hBufferTT:BUFFER-FIELD("Etag"):SERIALIZE-NAME = "@odata.etag".
    
    hOutputDataset:ADD-BUFFER (hBufferTT).
    
    hQuery:SET-BUFFERS(hBufferDB).
    hQuery:QUERY-PREPARE (SUBSTITUTE("FOR EACH &1 NO-LOCK &2",gcTabel,gcWhereClause)).
    
    hDatasource:QUERY = hQuery.
        
    hBufferTT:ATTACH-DATA-SOURCE (hDatasource).   
    
    hBufferTT:SET-CALLBACK("AFTER-ROW-FILL","generateETag").
    
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

FUNCTION HashRecord RETURNS CHARACTER PRIVATE
    (INPUT hBuffer AS HANDLE):
/*------------------------------------------------------------------------------
 Purpose:
 Notes:
------------------------------------------------------------------------------*/    
  
  CREATE ttETag.
  
  hBuffer:RAW-TRANSFER(TRUE,BUFFER ttETag:BUFFER-FIELD("rawhack"):HANDLE).
  
  RETURN STRING(BASE64-ENCODE(MESSAGE-DIGEST("SHA-1",ttETag.rawhack))).
        
END FUNCTION.

FUNCTION URIhasFilter RETURNS LOGICAL PRIVATE
    (  ):
/*------------------------------------------------------------------------------
 Purpose:
 Notes:
------------------------------------------------------------------------------*/    
  
    RETURN INDEX(gcURI,"?$filter") <> 0.
        
END FUNCTION.


FUNCTION getWhereClause RETURNS CHARACTER PRIVATE
    (INPUT cFilter AS CHARACTER, INPUT cTabel AS CHARACTER):
    /*------------------------------------------------------------------------------
     Purpose:
     Notes:
    ------------------------------------------------------------------------------*/    
    DEFINE VARIABLE cWhereClause  AS CHARACTER NO-UNDO.
    DEFINE VARIABLE cQueryString  AS CHARACTER NO-UNDO.
    
    cQueryString = REPLACE(cFilter,"$filter=","").
    
    RUN getWhereClause.p (INPUT cQueryString, INPUT cTabel, OUTPUT cWhereClause).
    
    MESSAGE "cWhereClause = " cWhereClause
        VIEW-AS ALERT-BOX.

    RETURN cWhereClause.
        
END FUNCTION.

         