
/*------------------------------------------------------------------------
    File        : day14.p
    Purpose     : Solve Day 14 of Advent of Code 2023

    Syntax      :

    Description : Solution for Advent of Code 2023 - Day 14

    Author(s)   : Wim van der Ham (WITS)
    Created     : Sat Dec 30 18:36:01 CET 2023
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
DEFINE VARIABLE iDay         AS INTEGER   NO-UNDO INITIAL 14.
DEFINE VARIABLE hPLIP        AS HANDLE    NO-UNDO.
DEFINE VARIABLE cInputFile   AS CHARACTER NO-UNDO.
DEFINE VARIABLE hShowFile    AS HANDLE    NO-UNDO.
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
DEFINE VARIABLE edProgress   AS LONGCHAR  NO-UNDO.
DEFINE VARIABLE lvlShow      AS LOGICAL   NO-UNDO INITIAL FALSE.
DEFINE VARIABLE lvlOutput    AS LOGICAL   NO-UNDO INITIAL FALSE.
DEFINE VARIABLE cOutputFile  AS CHARACTER NO-UNDO.
DEFINE VARIABLE iPart        AS INTEGER   NO-UNDO.
DEFINE VARIABLE iCycle       AS INTEGER   NO-UNDO.
DEFINE VARIABLE iDirection   AS INTEGER   NO-UNDO.
DEFINE VARIABLE iStartY      AS INTEGER   NO-UNDO.
DEFINE VARIABLE iEndY        AS INTEGER   NO-UNDO.
DEFINE VARIABLE iBy          AS INTEGER   NO-UNDO.
DEFINE VARIABLE iStartX      AS INTEGER   NO-UNDO.
DEFINE VARIABLE iEndX        AS INTEGER   NO-UNDO.

/* Specific */
DEFINE TEMP-TABLE ttLine
   FIELD IDLine      AS INTEGER 
   FIELD cInputLine  AS CHARACTER FORMAT "X(80)"
INDEX indLine IS UNIQUE IDLine.
    
DEFINE TEMP-TABLE ttGrid
   FIELD IDGrid     AS INTEGER 
   FIELD iX         AS INTEGER 
   FIELD iY         AS INTEGER 
   FIELD Symbol     AS CHARACTER
   FIELD RockLoad   AS INTEGER  
INDEX indID IS UNIQUE IDGrid
INDEX indXY IS PRIMARY iX iY
INDEX indYX iY iX.
DEFINE VARIABLE iNewIDGrid AS INTEGER NO-UNDO.
DEFINE BUFFER ttNextGrid FOR ttGrid.

DEFINE VARIABLE iCurrentRound AS INTEGER NO-UNDO.
// Variables for Part Two
DEFINE VARIABLE iX            AS INTEGER   NO-UNDO.
DEFINE VARIABLE iY            AS INTEGER   NO-UNDO.
DEFINE VARIABLE iMaxX         AS INTEGER   NO-UNDO.
DEFINE VARIABLE iMaxY         AS INTEGER   NO-UNDO.
DEFINE VARIABLE iNextY        AS INTEGER   NO-UNDO.
DEFINE VARIABLE iRound        AS INTEGER   NO-UNDO.
DEFINE VARIABLE cRocks        AS CHARACTER NO-UNDO.
DEFINE VARIABLE rHASH         AS RAW       NO-UNDO.
DEFINE VARIABLE cRocksHASH    AS CHARACTER NO-UNDO.
DEFINE VARIABLE iCyclesLeft   AS INT64     NO-UNDO.
DEFINE VARIABLE iMaxCycles    AS INTEGER   NO-UNDO.
    
DEFINE TEMP-TABLE ttHistory
   FIELD IDHistory AS INTEGER 
   FIELD RocksHASH AS CHARACTER 
   FIELD Cycle     AS INTEGER 
INDEX indID IS UNIQUE IDHistory
INDEX indRocks RocksHASH.
DEFINE VARIABLE iNewIDHistory AS INTEGER NO-UNDO.

/* ********************  Preprocessor Definitions  ******************** */

/* ************************  Function Prototypes ********************** */

{AOC_session.i}

/* ***************************  Main Block  *************************** */

DISPLAY
   SUBSTITUTE ("Year &1 Day &2", iYear, iDay) FORMAT "X(16)" NO-LABELS SKIP
   lOpenURL  LABEL "Open URL?"       VIEW-AS TOGGLE-BOX SKIP
   lDownload LABEL "Download Input?" VIEW-AS TOGGLE-BOX SKIP   
   lPart[1]  LABEL "Solve Part 1?"   VIEW-AS TOGGLE-BOX SKIP
   lPart[2]  LABEL "Solve Part 2?"   VIEW-AS TOGGLE-BOX SKIP 
   lvlDebug  LABEL "Debug?"          VIEW-AS TOGGLE-BOX SKIP 
   lvlShow   LABEL "Show?"           VIEW-AS TOGGLE-BOX SKIP
   lvlOutput LABEL "Output?"         VIEW-AS TOGGLE-BOX SKIP 
   SKIP (2)
   "Progress:" SKIP
   edProgress VIEW-AS EDITOR SIZE 76 BY 10 LARGE NO-LABELS
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
ELSE DO:
   lOpenURL = FALSE.
END.

UPDATE
   lOpenURL
   lDownload
   lPart
   lvlDebug
   lvlShow
   lvlOutput
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
   RUN sy\win\show-file.w
      (cInputFile).
END.

/* Start Processing */
iSolution = 0.

ETIME (YES).
COPY-LOB FROM FILE cInputfile TO OBJECT lcInput.

IF lvlDebug THEN DO:
   cInputFile = REPLACE (cInputFile, SUBSTITUTE ("&1.txt", STRING (iDay, "99")), SUBSTITUTE ("&1_sample.txt", STRING (iDay, "99"))).
   FILE-INFO:FILE-NAME = cInputFile.
   IF FILE-INFO:FILE-TYPE NE ? THEN DO: 
      COPY-LOB FROM FILE cInputFile TO OBJECT lcInput.
   END.
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

   IF cLine EQ "" THEN DO:
      /* Empty Line */
      NEXT.
   END.
      
   CREATE ttLine.
   ASSIGN 
      ttLine.IDLine     = iLine
      ttLine.cInputLine = cLine
   .
   
   iMaxX = MAXIMUM (iMaxX, LENGTH (ttLine.cInputLine)).
   
   DO iChar = 1 TO LENGTH (ttLine.cInputLine):
      cChar = SUBSTRING (ttLine.cInputLine, iChar, 1).
      iNewIDGrid = iNewIDGrid + 1.
      CREATE ttGrid.
      ASSIGN 
         ttGrid.IDGrid = iNewIDGrid
         ttGrid.iX     = iChar
         ttGrid.iY     = iLine
         ttGrid.Symbol = cChar
      .
   END.
   iMaxY = iLine.
   
END. /* ReadBlock: */

IF lvlShow THEN DO:
   RUN sy\win\wbrowsett.w
      (INPUT TEMP-TABLE ttGrid:HANDLE). 
END.

IF lPart[1] THEN DO:
   /* Process Part One */
   iSolution = 0.
   iRound = 0.          
   IF lvlOutput THEN 
      RUN outputGrid
         (INPUT iDay,
          INPUT iRound,
          INPUT "Start Grid",
          INPUT "").
   
   /* Run tilt one time in direction 1 = North */
   RUN tilt
      (INPUT 1,
       INPUT iMaxX,
       INPUT iMaxY).
       
   iSolution = 0.
   FOR EACH ttGrid
   WHERE ttGrid.Symbol EQ "O":
      ttGrid.RockLoad = iMaxY - ttGrid.iY + 1.
      iSolution = iSolution + ttGrid.RockLoad.
   END.
         
   OUTPUT TO "clipboard".
   PUT UNFORMATTED iSolution SKIP.
   OUTPUT CLOSE.
   MESSAGE 
      SUBSTITUTE ("Solution: &1.", iSolution) SKIP (1)
      SUBSTITUTE ("Found solution in &1 msecs.", ETIME)
   VIEW-AS ALERT-BOX TITLE " 2023 - Day 14 - Part One".
   
   IF lvlShow THEN DO:
      RUN sy\win\wbrowsett.w
         (INPUT TEMP-TABLE ttGrid:HANDLE).
      ENABLE edProgress WITH FRAME fr-Parameters.
      WAIT-FOR CLOSE OF THIS-PROCEDURE. 
   END.    
        
END. /* Process Part One */

IF lPart[2] THEN DO:
   /* Process Part Two */
   IF lPart[1] THEN DO:
      /* Reload Grid */
      EMPTY TEMP-TABLE ttGrid.
      FOR EACH ttLine:
         DO iChar = 1 TO LENGTH (ttLine.cInputLine):
            cChar = SUBSTRING (ttLine.cInputLine, iChar, 1).
            iNewIDGrid = iNewIDGrid + 1.
            CREATE ttGrid.
            ASSIGN 
               ttGrid.IDGrid = iNewIDGrid
               ttGrid.iX     = iChar
               ttGrid.iY     = ttLine.IDLine
               ttGrid.Symbol = cChar
            .
         END.
      END.
   END.
   iRound = 0.          
   IF lvlOutput THEN 
      RUN outputGrid
         (INPUT iDay,
          INPUT iRound,
          INPUT "Start Grid",
          INPUT "").
   
   iMaxCycles = 1000000000.
   CycleBlock:
   DO iCycle = 1 TO iMaxCycles:
      DO iDirection = 1 TO 4:
         /* 4 Directions: N, W, S, E */
         RUN tilt
            (INPUT iDirection,
             INPUT iMaxX,
             INPUT iMaxY).
      END. /* Direction */
      RUN getRocks
         (OUTPUT cRocks).
      rHASH = SHA1-DIGEST(cRocks).
      cRocksHASH = STRING (rHASH).
             
      FIND  ttHistory
      WHERE ttHistory.RocksHASH EQ cRocksHASH NO-ERROR.
      IF NOT AVAILABLE ttHistory THEN DO:
         iNewIDHistory = iNewIDHistory + 1.
         CREATE ttHistory.
         ASSIGN 
            ttHistory.IDHistory = iNewIDHistory
            ttHistory.RocksHASH = cRocksHASH
            ttHistory.Cycle     = iCycle
         .
      END.
      ELSE DO:
         IF iCyclesLeft EQ 0 THEN DO:
            iCyclesLeft = iMaxCycles - ttHistory.Cycle. // Skip the first cycles without a repeat
            iCyclesLeft = iMaxCycles - (ttHistory.Cycle + INT64 (TRUNCATE (iCyclesLeft / (iCycle - ttHistory.Cycle), 0)) * (iCycle - ttHistory.Cycle)).
            edProgress:INSERT-STRING (SUBSTITUTE ("Found a pattern after &1 cycles.~n", iCycle)).
            edProgress:INSERT-STRING (SUBSTITUTE ("This situation was also present after &1 cycles.~n", ttHistory.Cycle)).
            edProgress:INSERT-STRING (SUBSTITUTE ("Cycles left: &1.~n", iCyclesLeft)). 
            PROCESS EVENTS.            
         END.
         ELSE DO:
            iCyclesLeft = iCyclesLeft - 1.
            IF iCyclesLeft EQ 0 THEN
               LEAVE CycleBlock.
         END.
      END.
   END. /* Cycle */

   iSolution = 0.
   FOR EACH ttGrid
   WHERE ttGrid.Symbol EQ "O":
      ttGrid.RockLoad = iMaxY - ttGrid.iY + 1.
      iSolution = iSolution + ttGrid.RockLoad.
   END.
       
   OUTPUT TO "clipboard".
   PUT UNFORMATTED iSolution SKIP.
   OUTPUT CLOSE.

   MESSAGE 
      SUBSTITUTE ("Solution: &1.", iSolution) SKIP (1)
      SUBSTITUTE ("Found solution in &1 msecs.", ETIME)
   VIEW-AS ALERT-BOX TITLE " 2023 - Day 14 - Part Two".
   
   IF lvlShow THEN DO:
      RUN sy\win\wbrowsett.w
         (INPUT TEMP-TABLE ttHistory:HANDLE).
   END.      
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
      RUN sy\win\wbrowsett.w
         (INPUT TEMP-TABLE ttLine:HANDLE).
   END.
   RETURN.      
END CATCH.


/* **********************  Internal Procedures  *********************** */


PROCEDURE getRocks:
/*------------------------------------------------------------------------------
 Purpose:
 Notes:
------------------------------------------------------------------------------*/
DEFINE OUTPUT PARAMETER opcRocks AS CHARACTER NO-UNDO.

DEFINE BUFFER ttGrid FOR ttGrid.
  
   opcRocks = "". 
   FOR EACH ttGrid:
      IF ttGrid.Symbol EQ "O" THEN 
         opcRocks = opcRocks + "O".
      ELSE 
         opcRocks = opcRocks + " ".
   END.

END PROCEDURE.

PROCEDURE outputGrid:
/*------------------------------------------------------------------------------
 Purpose: Output a grid to a file
 Notes:
------------------------------------------------------------------------------*/
DEFINE INPUT  PARAMETER ipiDay         AS INTEGER NO-UNDO.
DEFINE INPUT  PARAMETER ipcInput       AS CHARACTER NO-UNDO.
DEFINE INPUT  PARAMETER ipcTitle       AS CHARACTER NO-UNDO.
DEFINE INPUT  PARAMETER ipcDescription AS CHARACTER NO-UNDO.

DEFINE VARIABLE cOutputFile  AS CHARACTER NO-UNDO.
DEFINE VARIABLE cCoordinates AS CHARACTER NO-UNDO.

DEFINE BUFFER ttGrid     FOR ttGrid.
DEFINE BUFFER ttGridLoad FOR ttGrid.

   cOutputFile = SUBSTITUTE ("output\&1_&2.txt", ipiDay, ipcInput).
   
   OUTPUT TO VALUE (cOutputFile).
   PUT UNFORMATTED 
      ipcTitle SKIP.
      
   FOR EACH ttGrid
   BREAK 
   BY ttGrid.iY
   BY ttGrid.iX:
      PUT UNFORMATTED 
         ttGrid.Symbol.

      IF LAST-OF (ttGrid.iY) THEN DO:
         PUT UNFORMATTED 
            SKIP.
      END.
   END.

   PUT UNFORMATTED 
      ipcDescription SKIP.
   OUTPUT CLOSE.            

END PROCEDURE.

PROCEDURE tilt:
/*------------------------------------------------------------------------------
 Purpose: Tilt in a direction: 1 = North, 2 = West, 3 = South and 4 = East
 Notes:
------------------------------------------------------------------------------*/
DEFINE INPUT  PARAMETER ipiDirection AS INTEGER NO-UNDO.
DEFINE INPUT  PARAMETER ipiMaxX      AS INTEGER NO-UNDO.
DEFINE INPUT  PARAMETER ipiMaxY      AS INTEGER NO-UNDO.

DEFINE BUFFER ttGrid     FOR ttGrid.
DEFINE BUFFER ttNextGrid FOR ttGrid.

DEFINE VARIABLE iX      AS INTEGER   NO-UNDO.
DEFINE VARIABLE iY      AS INTEGER   NO-UNDO.
DEFINE VARIABLE iStartX AS INTEGER   NO-UNDO.
DEFINE VARIABLE iEndX   AS INTEGER   NO-UNDO.
DEFINE VARIABLE ideltaX AS INTEGER   NO-UNDO.
DEFINE VARIABLE iStartY AS INTEGER   NO-UNDO.
DEFINE VARIABLE iEndY   AS INTEGER   NO-UNDO.
DEFINE VARIABLE ideltaY AS INTEGER   NO-UNDO.
DEFINE VARIABLE iRound  AS INTEGER   NO-UNDO.
DEFINE VARIABLE cOrder  AS CHARACTER NO-UNDO.
DEFINE VARIABLE iNextX  AS INTEGER   NO-UNDO.
DEFINE VARIABLE iNextY  AS INTEGER   NO-UNDO.
DEFINE VARIABLE lMove   AS LOGICAL   NO-UNDO.

   CASE ipiDirection:
      WHEN 1 THEN // North
         ASSIGN 
            iStartX = 1
            iStartY = 1
            iEndX   = ipiMaxX
            iEndY   = ipiMaxY
            ideltaX = 1
            ideltaY = 1
            cOrder  = "X,Y"
         .
      WHEN 2 THEN // West
         ASSIGN 
            iStartX = 1
            iStartY = 1
            iEndX   = ipiMaxX
            iEndY   = ipiMaxY
            ideltaX = 1
            ideltaY = 1
            cOrder  = "Y,X"
         .
      WHEN 3 THEN // South
         ASSIGN
            iStartX = 1
            iStartY = ipiMaxY
            iEndX   = ipiMaxX
            iEndY   = 1
            ideltaX = 1
            ideltaY = -1
            cOrder  = "X,Y"
         .            
      WHEN 4 THEN // East
         ASSIGN 
            iStartX = ipiMaxX
            iStartY = 1
            iEndX   = 1
            iEndY   = 1
            ideltaX = -1
            ideltaY = 1
            cOrder  = "Y,X"
         .
   END CASE.

   BlockOne:
   REPEAT:
      ASSIGN 
         iX = iStartX
         iY = iStartY
      .
      BlockTwo:
      REPEAT:
         iRound = iRound + 1.
         lMove  = TRUE.
         FIND  ttGrid
         WHERE ttGrid.iX EQ iX
         AND   ttGrid.iY EQ iY.
         IF ttGrid.Symbol NE "O" THEN DO: 
            /* No moves */
            lMove = FALSE.
         END.
         IF (ipiDirection EQ 1 AND ttGrid.iY EQ 1)
         OR (ipiDirection EQ 2 AND ttGrid.iX EQ 1)
         OR (ipiDirection EQ 3 AND ttGrid.iY EQ ipiMaxY)
         OR (ipiDirection EQ 4 AND ttGrid.iX EQ ipiMaxX) THEN 
            /* Don't move */
            lMove = FALSE.               
            
         IF lMove THEN DO:
            /* Search for first blocking object */
            CASE ipiDirection:
               WHEN 1 THEN DO:
                  FIND  LAST ttNextGrid
                  WHERE ttNextGrid.iX EQ ttGrid.iX
                  AND   ttNextGrid.iY LT ttGrid.iY
                  AND  (ttNextGrid.Symbol EQ "#" OR ttNextGrid.Symbol EQ "O") NO-ERROR.
                  IF AVAILABLE ttNextGrid THEN
                     /* Found a block, stop 1 row "higher" */
                     iNextY = ttNextGrid.iY + 1.
                  ELSE 
                     /* Nothing found that blocks, stop on first row */
                     iNextY = 1.
      
                  FIND  ttNextGrid 
                  WHERE ttNextGrid.iX EQ ttGrid.iX
                  AND   ttNextGrid.iY EQ iNextY.
                  IF ttNextGrid.iY NE ttGrid.iY THEN DO:
                     ASSIGN 
                        /* Do the "switch" */
                        ttNextGrid.Symbol   = ttGrid.Symbol
                        ttGrid.Symbol       = " "
                     .
                  END.
               END.
               WHEN 3 THEN DO:
                  FIND  FIRST ttNextGrid
                  WHERE ttNextGrid.iX EQ ttGrid.iX
                  AND   ttNextGrid.iY GT ttGrid.iY
                  AND  (ttNextGrid.Symbol EQ "#" OR ttNextGrid.Symbol EQ "O") NO-ERROR.
                  IF AVAILABLE ttNextGrid THEN
                     /* Found a block, stop 1 row "lower" */
                     iNextY = ttNextGrid.iY - 1.
                  ELSE 
                     /* Nothing found that blocks, stop on first row */
                     iNextY = ipiMaxY.
      
                  FIND  ttNextGrid 
                  WHERE ttNextGrid.iX EQ ttGrid.iX
                  AND   ttNextGrid.iY EQ iNextY.
                  IF ttNextGrid.iY NE ttGrid.iY THEN DO:
                     ASSIGN 
                        /* Do the "switch" */
                        ttNextGrid.Symbol   = ttGrid.Symbol
                        ttGrid.Symbol       = " "
                     .
                  END.
               END.
               WHEN 2 THEN DO:
                  FIND LAST ttNextGrid
                  WHERE ttNextGrid.iY EQ ttGrid.iY
                  AND   ttNextGrid.iX LT ttGrid.iX
                  AND  (ttNextGrid.Symbol EQ "#" OR ttNextGrid.Symbol EQ "O") NO-ERROR.
                  IF AVAILABLE ttNextGrid THEN 
                     iNextX = ttNextGrid.iX + 1.
                  ELSE 
                     iNextX = 1.
                  FIND  ttNextGrid
                  WHERE ttNextGrid.iY EQ ttGrid.iy
                  AND   ttNextGrid.iX EQ iNextX.
                  IF ttNextGrid.iX NE ttGrid.iX THEN
                     ASSIGN 
                        ttNextGrid.Symbol = ttGrid.Symbol
                        ttGrid.Symbol     = " "
                     .
               END.
               WHEN 4 THEN DO:
                  FIND FIRST ttNextGrid
                  WHERE ttNextGrid.iY EQ ttGrid.iY
                  AND   ttNextGrid.iX GT ttGrid.iX
                  AND  (ttNextGrid.Symbol EQ "#" OR ttNextGrid.Symbol EQ "O") NO-ERROR.
                  IF AVAILABLE ttNextGrid THEN
                     iNextX = ttNextGrid.iX - 1.
                  ELSE 
                     iNextX = ipiMaxX.
                  FIND  ttNextGrid
                  WHERE ttNextGrid.iY EQ ttGrid.iY
                  AND   ttNextGrid.iX EQ iNextX.
                  IF ttNextGrid.iX NE ttGrid.iX THEN 
                     ASSIGN
                        ttNextGrid.Symbol = ttGrid.Symbol
                        ttGrid.Symbol     = " "
                     .   
               END.
            END.
         END.
                  
         CASE ENTRY (1, cOrder):
            WHEN "X" THEN 
               iX = iX + ideltaX.
            WHEN "Y" THEN 
               iY = iY + ideltaY.
         END.
         
         IF iY GT ipiMaxY
         OR iY LT 1
         OR iX GT ipiMaxX
         OR iX LT 1
         THEN
            LEAVE.         
         
      END. /* BlockTwo */
      
      CASE ENTRY (2, cOrder):
         WHEN "X" THEN
            iStartX = iStartX + ideltaX.
         WHEN "Y" THEN
            iStartY = iStartY + ideltaY.
      END.

      IF iStartY GT ipiMaxY
      OR iStartY LT 1
      OR iStartX GT ipiMaxX
      OR iStartX LT 1
      THEN
         LEAVE.         
   END. /* BlockOne */
         
END PROCEDURE.

