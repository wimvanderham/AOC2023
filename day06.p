
/*------------------------------------------------------------------------
    File        : day06.p
    Purpose     : Solve Day 06 of Advent of Code 2023

    Syntax      :

    Description : Solution of Advent of Code 2023 Day 06

    Author(s)   : Wim van der Ham (WITS)
    Created     : Wed Dec 06 14:19:55 CET 2023
    Notes       :
  ----------------------------------------------------------------------*/

/* ***************************  Definitions  ************************** */

BLOCK-LEVEL ON ERROR UNDO, THROW.

/* Variables for Problem */
DEFINE VARIABLE cURL         AS CHARACTER NO-UNDO INITIAL "https://adventofcode.com/&1/day/&2".
DEFINE VARIABLE cCommand     AS CHARACTER NO-UNDO.
/* Variables for input handling */
DEFINE VARIABLE lDownload    AS LOGICAL   NO-UNDO.
DEFINE VARIABLE cSession     AS CHARACTER NO-UNDO INITIAL "53616c7465645f5f0aa2b48889c4ecb0a71e7086a3ce378be60c9c62fff2ce2f0a803b3cf401a90e48d12df95cfd2383f2923a50c7378e392a1b5d4ce4438c7e".
DEFINE VARIABLE iYear        AS INTEGER   NO-UNDO INITIAL 2023.
DEFINE VARIABLE iDay         AS INTEGER   NO-UNDO INITIAL 6.
DEFINE VARIABLE hPLIP        AS HANDLE    NO-UNDO.
DEFINE VARIABLE cInputFile   AS CHARACTER NO-UNDO.
DEFINE VARIABLE lcInput      AS LONGCHAR  NO-UNDO.
DEFINE VARIABLE iLine        AS INTEGER   NO-UNDO.
DEFINE VARIABLE cLine        AS CHARACTER NO-UNDO.
DEFINE VARIABLE iChar        AS INTEGER   NO-UNDO.
DEFINE VARIABLE cChar        AS CHARACTER NO-UNDO.
DEFINE VARIABLE lOpenURL     AS LOGICAL   NO-UNDO INITIAL YES.
DEFINE VARIABLE lPart        AS LOGICAL   NO-UNDO EXTENT 2.
/* Variables for solving */
/* Generic */
DEFINE VARIABLE iSolution    AS INT64     NO-UNDO.
DEFINE VARIABLE lOk          AS LOGICAL   NO-UNDO.
DEFINE VARIABLE cMessage     AS CHARACTER NO-UNDO.
DEFINE VARIABLE lvlDebug     AS LOGICAL   NO-UNDO INITIAL FALSE.
DEFINE VARIABLE lvlShow      AS LOGICAL   NO-UNDO INITIAL FALSE.
DEFINE VARIABLE iPart        AS INTEGER   NO-UNDO.

/* Specific */
DEFINE TEMP-TABLE ttLine
   FIELD IDLine      AS INTEGER 
   FIELD cInputLine  AS CHARACTER FORMAT "X(80)"
INDEX indLine IS UNIQUE IDLine.

DEFINE TEMP-TABLE ttRace
   FIELD IDRace    AS INTEGER 
   FIELD iTime     AS INTEGER  
   FIELD iDistance AS INTEGER 
INDEX indID IS UNIQUE IDRace.

DEFINE VARIABLE iNewIDRace AS INTEGER   NO-UNDO.
DEFINE VARIABLE iIndex     AS INTEGER   NO-UNDO.
DEFINE VARIABLE cTime      AS CHARACTER NO-UNDO.
DEFINE VARIABLE cDistance  AS CHARACTER NO-UNDO.
DEFINE VARIABLE iBeatIT    AS INTEGER   NO-UNDO.
DEFINE VARIABLE iStart     AS INTEGER   NO-UNDO.
    
/* ********************  Preprocessor Definitions  ******************** */

{AOC_session.i}

/* ************************  Function Prototypes ********************** */

FUNCTION myDistance RETURNS INTEGER 
   ( INPUT ipiRaceTime AS INTEGER,
     INPUT ipiStart    AS INTEGER   ) FORWARD.

/* ***************************  Main Block  *************************** */

DISPLAY
   SUBSTITUTE ("Year &1 Day &2", iYear, iDay) FORMAT "X(16)" NO-LABELS SKIP
   lOpenURL  LABEL "Open URL?"       VIEW-AS TOGGLE-BOX SKIP
   lDownload LABEL "Download Input?" VIEW-AS TOGGLE-BOX SKIP   
   lPart[1]  LABEL "Solve Part 1?"   VIEW-AS TOGGLE-BOX SKIP
   lPart[2]  LABEL "Solve Part 2?"   VIEW-AS TOGGLE-BOX SKIP 
   lvlDebug  LABEL "Debug?"          VIEW-AS TOGGLE-BOX SKIP 
   lvlShow   LABEL "Show?"           VIEW-AS TOGGLE-BOX SKIP
WITH FRAME fr-Parameters SIDE-LABELS ROW 3 CENTERED TITLE " Parameters ".
ASSIGN 
   lDownload  = FALSE
   cInputfile = SUBSTITUTE ("C:\OpenEdge\WRK\AOC&1\input\&2.txt", STRING (iYear, "9999"), STRING (iDay, "99"))
   cURL       = SUBSTITUTE (cURL, iYear, iDay)
.
FILE-INFO:FILE-NAME = cInputFile.
IF FILE-INFO:FILE-TYPE EQ ? THEN DO:
   lDownload = TRUE.
END.

UPDATE
   lOpenURL
   lDownload
   lPart
   lvlDebug
   lvlShow
WITH FRAME fr-Parameters.

RUN plip_aoc.p PERSISTENT SET hPLIP.

IF lOpenURL THEN DO:
   RUN chkURL IN hPLIP
      (INPUT  iYear,
       INPUT  iDay,
       OUTPUT lOk,
       OUTPUT cMessage).
   IF lOk EQ FALSE THEN DO:
      MESSAGE cMessage
      VIEW-AS ALERT-BOX WARNING.
      RETURN.
   END.
   cCommand = SUBSTITUTE ("start &1", cURL).
   OS-COMMAND SILENT VALUE (cCommand).
END.

IF lDownload THEN DO:
   RUN getInput IN hPLIP
      (INPUT  cSession,
       INPUT  iYear,
       INPUT  iDay,
       INPUT  cInputFile,
       OUTPUT lOk,
       OUTPUT cMessage).
   IF lOk EQ FALSE THEN DO:
      MESSAGE cMessage
      VIEW-AS ALERT-BOX WARNING.
      RETURN.
   END.
END.

/* Start Processing */
iSolution = 0.

ETIME (YES).
COPY-LOB FROM FILE cInputfile TO OBJECT lcInput.

IF lvlDebug THEN DO:
   cInputFile = REPLACE (cInputFile, STRING (iDay, "99"), SUBSTITUTE ("&1_sample", STRING (iDay, "99"))).
   FILE-INFO:FILE-NAME = cInputFile.
   IF FILE-INFO:FILE-TYPE NE ? THEN 
      COPY-LOB FROM FILE cInputFile TO OBJECT lcInput.
   ELSE
      MESSAGE 
      "No input file found with sample data:" SKIP(1) 
      cInputFile SKIP(1)
      VIEW-AS ALERT-BOX.
END.

/* Read Input into Temp-table */
ReadBlock:
DO iLine = 1 TO NUM-ENTRIES (lcInput, "~n"):
   cLine = TRIM (ENTRY (iLine, lcInput, "~n")).

   IF cLine EQ "" THEN 
      NEXT.
      
   CREATE ttLine.
   ASSIGN 
      ttLine.IDLine     = iLine
      ttLine.cInputLine = cLine
   .

   /* Parsing */
   IF iLine EQ 1 THEN DO:
      DO iIndex = 2 TO NUM-ENTRIES (ttLine.cInputLine, " "):
         cTime = ENTRY (iIndex, ttLine.cInputLine, " ").
         IF TRIM (cTime) EQ "" THEN 
            NEXT.
         iNewIDRace = iNewIDRace + 1.
         CREATE ttRace.
         ASSIGN 
            ttRace.IDRace = iNewIDRace
            ttRace.iTime  = INTEGER (cTime)
         .
      END.
   END.
   IF iLine EQ 2 THEN DO:
      iNewIDRace = 0.
      DO iIndex = 2 TO NUM-ENTRIES (ttLine.cInputLine, " ").
         cDistance = ENTRY (iIndex, ttLine.cInputLine, " ").
         IF TRIM (cDistance) EQ "" THEN
            NEXT.
         iNewIDRace = iNewIDRace + 1.
         FIND ttRace WHERE ttRace.IDRace EQ iNewIDRace.
         ASSIGN 
            ttRace.iDistance = INTEGER (cDistance)
         .
      END.
   END.
          
END. /* ReadBlock: */

IF lvlShow THEN DO:
   RUN sy\win\wbrowsett.w
      (INPUT TEMP-TABLE ttRace:HANDLE).
END.

IF lPart[1] THEN DO:
   /* Process Part One */
   iSolution = 1.

   /* Calcolate Solution for Part 1 */
   FOR EACH ttRace:
      iBeatIT = 0.
      DO iStart = 1 TO ttRace.iTime - 1:
         IF myDistance(ttRace.iTime, iStart) GT ttRace.iDistance THEN
            iBeatIT = iBeatIT + 1.
      END.
      IF iBeatIT GT 0 THEN 
         iSolution = iSolution * iBeatIT.  
   END.
            
   OUTPUT TO "clipboard".
   PUT UNFORMATTED iSolution SKIP.
   OUTPUT CLOSE.
   MESSAGE 
      SUBSTITUTE ("Solution: &1.", 
         iSolution) SKIP (1)
      SUBSTITUTE ("Found solution in &1 msecs.", ETIME)
   VIEW-AS ALERT-BOX TITLE " 2023 - Day 06 - Part One".
   
END. /* Process Part One */

IF lPart[2] THEN DO:
   /* Process Part Two */
   iSolution = 0.

   IF lvlShow THEN DO:

   END.

   /* Calcolate Solution for Part 2 */
   
   OUTPUT TO "clipboard".
   PUT UNFORMATTED iSolution SKIP.
   OUTPUT CLOSE.

   MESSAGE 
      SUBSTITUTE ("Solution: &1.", 
         iSolution) SKIP (1)
      SUBSTITUTE ("Found solution in &1 msecs.", ETIME)
   VIEW-AS ALERT-BOX TITLE " 2023 - Day 06 - Part Two".
END. /* Process Part Two */

CATCH oError AS Progress.Lang.Error :
   DEFINE VARIABLE iMessage      AS INTEGER   NO-UNDO.
   DEFINE VARIABLE cErrorMessage AS CHARACTER NO-UNDO.
   cErrorMessage = oError:GetMessage(1).
   iMessage = 2.
   DO WHILE iMessage LT oError:NumMessages:
      cErrorMessage = SUBSTITUTE ("&1~n&2", cErrorMessage, oError:GetMessage(iMessage)).
      iMessage = iMessage + 1.
   END.
   IF oError:CallStack NE ? THEN DO:
      cErrorMessage = SUBSTITUTE ("&1~n~nCall Stack:~n&2", cErrorMessage, oError:CallStack).
   END.
   MESSAGE "Error!" SKIP (1)
   SUBSTITUTE ("At line #: &1: &2", iLine, cLine) SKIP
   cErrorMessage SKIP(1) 
   VIEW-AS ALERT-BOX ERROR.
   IF lvlShow THEN DO:
   END.
   RETURN.      
END CATCH.


/* **********************  Internal Procedures  *********************** */

/* ************************  Function Implementations ***************** */


FUNCTION myDistance RETURNS INTEGER 
   ( INPUT ipiRaceTime  AS INTEGER,
     INPUT ipiStartTime AS INTEGER):
        
/*------------------------------------------------------------------------------
 Purpose: Calculate the distance considering start time and race time
 Notes:
------------------------------------------------------------------------------*/   
DEFINE VARIABLE iDistance AS INTEGER NO-UNDO.

   iDistance = (ipiRaceTime - ipiStartTime) * ipiStartTime.
   
   RETURN iDistance.
   
END FUNCTION.
