
/*------------------------------------------------------------------------
    File        : Filltest.p
    Purpose     : 

    Syntax      :

    Description : 

    Author(s)   : meulbfr
    Created     : Tue Dec 17 16:38:02 CET 2024
    Notes       :
  ----------------------------------------------------------------------*/

/* ***************************  Definitions  ************************** */

BLOCK-LEVEL ON ERROR UNDO, THROW.

/* ********************  Preprocessor Definitions  ******************** */
DEFINE VARIABLE hTemptable  AS HANDLE NO-UNDO.
DEFINE VARIABLE hDataset    AS HANDLE NO-UNDO.
DEFINE VARIABLE hDataSource AS HANDLE NO-UNDO.

DEFINE TEMP-TABLE ttHack NO-UNDO
  FIELD rawhack AS RAW.
CREATE tthack. 

CREATE WIDGET-POOL "dataobjects".

FUNCTION HashRecord RETURNS CHARACTER(INPUT hBuffer AS HANDLE):
/* We gebruiken ttHack.rawhack temp-table field omdat dynamische raw-transfer niet direct naar een RAW of MEMPTR variabele kan schrijven.
   Statische raw-transfer kan dat wel, maar dan zit je vast aan statische buffers en kun je dit niet generiek doen. 
*/
    
  hBuffer:RAW-TRANSFER(TRUE,BUFFER tthack:BUFFER-FIELD("rawhack"):HANDLE).
  RETURN STRING(BASE64-ENCODE(MESSAGE-DIGEST("SHA-1",ttHack.rawhack))).
END.

/* ***************************  Main Block  *************************** */
PROCEDURE AfterCustomerRowFill:
    DEFINE INPUT PARAMETER DATASET-HANDLE hDataSet. 
    /* Hoe verzin je een record versie als OpenEdge die zelf niet bijhoudt ?
       Het hele database record hashen is een quick & ditry oplossing - als het record verandert, krijg je een andere hash waarde 
    */ 
    SELF::Etag = HashRecord(SELF:DATA-SOURCE:GET-SOURCE-BUFFER()).
END.

CREATE TEMP-TABLE hTemptable IN WIDGET-POOL "dataobjects".

hTemptable:CREATE-LIKE("Customer","CustNum").
hTemptable:ADD-NEW-FIELD("Etag","CHARACTER").
hTemptable:TEMP-TABLE-PREPARE("ttCustomer").
hTemptable:DEFAULT-BUFFER-HANDLE:BUFFER-FIELD("Etag"):SERIALIZE-NAME = "@odata.etag".

CREATE DATASET hDataset IN WIDGET-POOL "dataobjects".
hDataset:ADD-BUFFER(hTemptable:DEFAULT-BUFFER-HANDLE).

CREATE DATA-SOURCE hDataSource IN WIDGET-POOL "dataobjects".
hDataSource:ADD-SOURCE-BUFFER(BUFFER customer:HANDLE, "").

hTemptable:DEFAULT-BUFFER-HANDLE:SET-CALLBACK("AFTER-ROW-FILL","AfterCustomerRowFill").
hTemptable:DEFAULT-BUFFER-HANDLE:ATTACH-DATA-SOURCE(hDataSource).

hDataset:FILL().

hDataset:WRITE-JSON("FILE","Etagtest.json",YES).

DELETE WIDGET-POOL "dataobjects".
