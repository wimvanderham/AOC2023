
/*------------------------------------------------------------------------
    File        : day10.p
    Purpose     : Solve Day 10 of Advent of Code 2023

    Syntax      :

    Description : Solution for Advent of Code 2023 - Day 10

    Author(s)   : Wim van der Ham (WITS)
    Created     : Sun Dec 10 09:11:17 CET 2023
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
DEFINE VARIABLE iDay         AS INTEGER   NO-UNDO INITIAL 10.
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
DEFINE VARIABLE iPart        AS INTEGER   NO-UNDO.

/* Specific */
DEFINE TEMP-TABLE ttLine
   FIELD IDLine      AS INTEGER 
   FIELD cInputLine  AS CHARACTER FORMAT "X(80)"
INDEX indLine IS UNIQUE IDLine.

DEFINE TEMP-TABLE ttGrid
   FIELD IDGrid AS INTEGER
   FIELD iX     AS INTEGER  
   FIELD iY     AS INTEGER 
   FIELD Symbol AS CHARACTER
   FIELD Steps  AS INTEGER   INITIAL ?
INDEX indID    IS UNIQUE IDGrid
INDEX indGrid  iY iX
INDEX indSteps IS PRIMARY Steps iX iY.
DEFINE VARIABLE iNewIDGrid AS INTEGER   NO-UNDO.
DEFINE BUFFER ttGridS    FOR ttGrid.
DEFINE BUFFER ttNextGrid FOR ttGrid.

DEFINE TEMP-TABLE ttDirection
   FIELD IDDirection AS INTEGER 
   FIELD Symbol      AS CHARACTER 
   FIELD iDeltaX1    AS INTEGER 
   FIELD iDeltaY1    AS INTEGER
   FIELD iDeltaX2    AS INTEGER 
   FIELD iDeltaY2    AS INTEGER 
   FIELD Alternate   AS CHARACTER
   FIELD Directions  AS CHARACTER  
INDEX indID     IS UNIQUE IDDirection
INDEX indSymbol IS UNIQUE PRIMARY Symbol.
DEFINE VARIABLE iNewIDDirection AS INTEGER NO-UNDO.

DEFINE TEMP-TABLE ttGridDirection
   FIELD IDGridDirection AS INTEGER 
   FIELD IDGrid          AS INTEGER 
   FIELD iFromX          AS INTEGER 
   FIELD iFromY          AS INTEGER 
   FIELD iToX            AS INTEGER 
   FIELD iToY            AS INTEGER 
INDEX indID     IS UNIQUE IDGridDirection
INDEX indIDGrid IS UNIQUE PRIMARY IDGrid iFromX iFromY iToX iToY.
DEFINE VARIABLE iNewIDGridDirection AS INTEGER NO-UNDO.
    
DEFINE VARIABLE iX             AS INTEGER   NO-UNDO.
DEFINE VARIABLE iY             AS INTEGER   NO-UNDO.
DEFINE VARIABLE cLocation      AS CHARACTER NO-UNDO.
DEFINE VARIABLE cTest          AS CHARACTER NO-UNDO.
DEFINE VARIABLE cComplementary AS CHARACTER NO-UNDO INITIAL "WE,NS,EW,SN".
DEFINE VARIABLE iCurrentSteps  AS INTEGER   NO-UNDO.
DEFINE VARIABLE iFound         AS INTEGER   NO-UNDO.

/* ********************  Preprocessor Definitions  ******************** */

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
/*   SKIP (2)                                               */
/*   "Progress:" SKIP                                       */
/*   edProgress VIEW-AS EDITOR SIZE 76 BY 10 LARGE NO-LABELS*/
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


/* Directions */
// | is a vertical pipe connecting north and south.
iNewIDDirection = iNewIDDirection + 1.
CREATE ttDirection.
ASSIGN
   ttDirection.IDDirection = iNewIDDirection
   ttDirection.Symbol      = "|" 
   ttDirection.iDeltaX1    = 0
   ttDirection.iDeltaY1    = 1
   ttDirection.iDeltaX2    = 0
   ttDirection.iDeltaY2    = -1
   ttDirection.Alternate   = "│"
   ttDirection.Directions  = "SN"
.

// - is a horizontal pipe connecting east and west.
iNewIDDirection = iNewIDDirection + 1.
CREATE ttDirection.
ASSIGN
   ttDirection.IDDirection = iNewIDDirection
   ttDirection.Symbol      = "-"
   ttDirection.iDeltaX1    = -1
   ttDirection.iDeltaY1    = 0
   ttDirection.iDeltaX2    = 1
   ttDirection.iDeltaY2    = 0
   ttDirection.Alternate   = "─"
   ttDirection.Directions  = "EW"
.

// L is a 90-degree bend connecting north and east.
iNewIDDirection = iNewIDDirection + 1.
CREATE ttDirection.
ASSIGN
   ttDirection.IDDirection = iNewIDDirection
   ttDirection.Symbol      = "L"
   ttDirection.iDeltaX1    = 1
   ttDirection.iDeltaY1    = 0
   ttDirection.iDeltaX2    = 0
   ttDirection.iDeltaY2    = -1   
   ttDirection.Alternate   = "└"   
   ttDirection.Directions  = "NE"
.

// J is a 90-degree bend connecting north and west.
iNewIDDirection = iNewIDDirection + 1.
CREATE ttDirection.
ASSIGN
   ttDirection.IDDirection = iNewIDDirection
   ttDirection.Symbol      = "J"
   ttDirection.iDeltaX1    = -1
   ttDirection.iDeltaY1    = 0
   ttDirection.iDeltaX2    = 0
   ttDirection.iDeltaY2    = -1
   ttDirection.Alternate   = "┘"
   ttDirection.Directions  = "NW"
.

//7 is a 90-degree bend connecting south and west.
iNewIDDirection = iNewIDDirection + 1.
CREATE ttDirection.
ASSIGN
   ttDirection.IDDirection = iNewIDDirection
   ttDirection.Symbol      = "7"
   ttDirection.iDeltaX1    = 0
   ttDirection.iDeltaY1    = 1
   ttDirection.iDeltaX2    = -1
   ttDirection.iDeltaY2    = 0
   ttDirection.Alternate   = "┐"
   ttDirection.Directions  = "SW"
.

// F is a 90-degree bend connecting south and east.
iNewIDDirection = iNewIDDirection + 1.
CREATE ttDirection.
ASSIGN
   ttDirection.IDDirection = iNewIDDirection
   ttDirection.Symbol      = "F"
   ttDirection.iDeltaX1    = 0
   ttDirection.iDeltaY1    = 1
   ttDirection.iDeltaX2    = 1
   ttDirection.iDeltaY2    = 0
   ttDirection.Alternate   = "┌"
   ttDirection.Directions  = "SE"
.

//. is ground; there is no pipe in this tile.
// S is the starting position of the animal; there is a pipe on this tile, but your sketch doesn't show what shape the pipe has.

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

   DO iX = 1 TO LENGTH (ttLine.cInputLine):
      iNewIDGrid = iNewIDGrid + 1.
      CREATE ttGrid.
      ASSIGN 
         ttGrid.IDGrid = iNewIDGrid
         ttGrid.iX     = iX
         ttGrid.iY     = iLine
         ttGrid.Symbol = SUBSTRING (ttLine.cInputLine, iX, 1)
      .
      FIND ttDirection WHERE ttDirection.Symbol EQ ttGrid.Symbol NO-ERROR.
      IF AVAILABLE ttDirection THEN DO:
         iNewIDGridDirection = iNewIDGridDirection + 1.
         CREATE ttGridDirection.
         ASSIGN 
            ttGridDirection.IDGridDirection = iNewIDGridDirection
            ttGridDirection.IDGrid          = ttGrid.IDGrid
            ttGridDirection.iFromX          = ttGrid.iX
            ttGridDirection.iFromY          = ttGrid.iY
            ttGridDirection.iToX            = ttGrid.iX + ttDirection.iDeltaX1
            ttGridDirection.iToY            = ttGrid.iY + ttDirection.iDeltaY1
         .

         iNewIDGridDirection = iNewIDGridDirection + 1.
         CREATE ttGridDirection.
         ASSIGN 
            ttGridDirection.IDGridDirection = iNewIDGridDirection
            ttGridDirection.IDGrid          = ttGrid.IDGrid
            ttGridDirection.iFromX          = ttGrid.iX
            ttGridDirection.iFromY          = ttGrid.iY
            ttGridDirection.iToX            = ttGrid.iX + ttDirection.iDeltaX2
            ttGridDirection.iToY            = ttGrid.iY + ttDirection.iDeltaY2
         . 
      END.          
   END.      
END. /* ReadBlock: */

IF lvlShow THEN DO:
   RUN sy\win\wbrowsett.w
      (INPUT TEMP-TABLE ttGrid:HANDLE). 
   RUN sy\win\wbrowsett.w
      (INPUT TEMP-TABLE ttDirection:HANDLE).
   RUN sy\win\wbrowsett.w
      (INPUT TEMP-TABLE ttGridDirection:HANDLE).
         
   IF lvlOutput THEN DO:  
      /* Output Starting Grid with "graphical" characters */             
      OUTPUT TO "output\10.txt".
      FOR EACH ttGrid
      BREAK 
      BY ttGrid.iY
      BY ttGrid.iX:
         FIND ttDirection WHERE ttDirection.Symbol EQ ttGrid.Symbol NO-ERROR.
         IF AVAILABLE ttDirection THEN 
            PUT UNFORMATTED 
               ttDirection.Alternate.
         ELSE 
            PUT UNFORMATTED 
               ttGrid.Symbol.
         IF LAST-OF (ttGrid.iY) THEN 
            PUT UNFORMATTED SKIP.
      END.
      OUTPUT CLOSE.
   END. /* Output Starting Grid with "graphical" characters */
END.

IF lPart[1] THEN DO:
   /* Process Part One */
   iSolution = 0.

   /* Calcolate Solution for Part 1 */
   FIND ttGridS WHERE ttGridS.Symbol EQ "S".
   ASSIGN
      ttGridS.Steps = 0
      iCurrentSteps = 0
   .
   
   REPEAT:
      iFound = 0.
      FOR EACH ttGridS
      WHERE ttGridS.Steps EQ iCurrentSteps:
         IF ttGridS.Symbol EQ "S" THEN DO:
            /* Determine the shape of S and reachable neighbors */
            FOR EACH ttNextGrid
            WHERE ttNextGrid.iX     GE ttGridS.iX - 1
            AND   ttNextGrid.iX     LE ttGridS.iX + 1
            AND   ttNextGrid.iY     GE ttGridS.iY - 1
            AND   ttNextGrid.iY     LE ttGridS.iY + 1
            AND   ttNextGrid.IDGrid NE ttGridS.IDGrid
            AND  (ABSOLUTE (ttNextGrid.iX - ttGridS.iX) + ABSOLUTE (ttNextGrid.iY - ttGridS.iY)) LT 2
            AND   ttNextGrid.Steps  EQ ?,
            FIRST ttDirection 
            WHERE ttDirection.Symbol EQ ttNextGrid.Symbol:
               /* All neighbors of starting point S */
               IF ttNextGrid.iY LT ttGridS.iY THEN 
                  cLocation = "N".
               IF ttNextGrid.iY GT ttGridS.iY THEN 
                  cLocation = "S".
               IF ttNextGrid.iX LT ttGridS.iX THEN 
                  cLocation = "W".
               IF ttNextGrid.iX GT ttGridS.iX THEN 
                  cLocation = "E".
               cTest = SUBSTRING (ttDirection.Directions, 2, 1).

               IF lvlShow THEN 
                  MESSAGE SUBSTITUTE ("Check (&1,&2): &3 + &4", ttNextGrid.iX, ttNextGrid.iY, cLocation, cTest)  
                  VIEW-AS ALERT-BOX.
               
                  
               IF LOOKUP (cLocation + cTest, cComplementary) NE 0 THEN DO:
                  /* Found a match */
                  iFound = iFound + 1.
                  ttNextGrid.Steps = iCurrentSteps + 1.
                  IF lvlShow THEN DO:
                     MESSAGE SUBSTITUTE ("Found a match: &1 + &2 in &3",
                                         cLocation,
                                         cTest,
                                         cComplementary) SKIP (1)
                      SUBSTITUTE ("S (&1, &2) vs Next (&3, &4) &5 Delta (&6, &7) &8",
                                         ttGridS.iX,
                                         ttGridS.iY,
                                         ttNextGrid.iX,
                                         ttNextGrid.iY,
                                         ttDirection.Directions,
                                         ttDirection.iDeltaX1,
                                         ttDirection.iDeltaY1,
                                         ttNextGrid.Symbol)
                     VIEW-AS ALERT-BOX.
                  END.
               END. /* Found a match */
            END. /* All neighbors of starting point S */
         END. /* Determine the shape of S and reachable neighbors */
         ELSE DO:
            /* Use the Grid Direction temp-table to reach next grid locations */
            FOR EACH ttGridDirection
            WHERE ttGridDirection.IDGrid EQ ttGridS.IDGrid,
            FIRST ttNextGrid 
            WHERE ttNextGrid.iX    EQ ttGridDirection.iToX
            AND   ttNextGrid.iY    EQ ttGridDirection.iToY
            AND   ttNextGrid.Steps EQ ?:
               iFound = iFound + 1.
               ttNextGrid.Steps = iCurrentSteps + 1.
            END.
         END. /* Use the Grid Direction temp-table to reach next grid locations */
      END.
      IF iFound EQ 0 THEN 
         LEAVE.
      
      IF lvlOutput THEN DO:
         IF iCurrentSteps MOD 1000 EQ 0 
         OR iCurrentSteps LE  100 THEN DO:
            OUTPUT TO VALUE (SUBSTITUTE ("output\10_&1.txt", iCurrentSteps)).
            FOR EACH ttGrid
            BREAK 
            BY ttGrid.iY
            BY ttGrid.iX:
               IF ttGrid.Steps NE ? THEN
                  PUT UNFORMATTED 
                     ttGrid.Steps MOD 10.
               ELSE DO:
                  FIND ttDirection WHERE ttDirection.Symbol EQ ttGrid.Symbol NO-ERROR.
                  IF AVAILABLE ttDirection THEN 
                     PUT UNFORMATTED 
                        ttDirection.Alternate.
                  ELSE 
                     PUT UNFORMATTED 
                        ttGrid.Symbol.
               END.
               IF LAST-OF (ttGrid.iY) THEN 
                  PUT UNFORMATTED SKIP.
            END.
            OUTPUT CLOSE.
         END.
      END.
      IF lvlShow THEN DO:
         IF iCurrentSteps MOD 1000 EQ 0 THEN DO:
            MESSAGE "Steps:" iCurrentSteps
            VIEW-AS ALERT-BOX.            
            RUN sy\win\wbrowsett.w
               (INPUT TEMP-TABLE ttGrid:HANDLE).
         END.
      END.
                               
      iCurrentSteps = iCurrentSteps + 1.
   END.

   IF lvlOutput THEN DO:
      OUTPUT TO VALUE (SUBSTITUTE ("output\10_&1.txt", iCurrentSteps)).
      FOR EACH ttGrid
      BREAK 
      BY ttGrid.iY
      BY ttGrid.iX:
         IF ttGrid.Steps NE ? THEN
            PUT UNFORMATTED 
               ttGrid.Steps MOD 10.
         ELSE DO:
            FIND ttDirection WHERE ttDirection.Symbol EQ ttGrid.Symbol NO-ERROR.
            IF AVAILABLE ttDirection THEN 
               PUT UNFORMATTED 
                  ttDirection.Alternate.
            ELSE 
               PUT UNFORMATTED 
                  ttGrid.Symbol.
         END.
         IF LAST-OF (ttGrid.iY) THEN 
            PUT UNFORMATTED SKIP.
      END.
      OUTPUT CLOSE.
   END.
   
   iSolution = iCurrentSteps.
   
   OUTPUT TO "clipboard".
   PUT UNFORMATTED iSolution SKIP.
   OUTPUT CLOSE.
   MESSAGE 
      SUBSTITUTE ("Solution: &1.", iSolution) SKIP (1)
      SUBSTITUTE ("Found solution in &1 msecs.", ETIME)
   VIEW-AS ALERT-BOX TITLE " 2023 - Day 10 - Part One".
   
   IF lvlShow THEN DO:
      RUN sy\win\wbrowsett.w
         (INPUT TEMP-TABLE ttGrid:HANDLE).
   END.
            
END. /* Process Part One */

IF lPart[2] THEN DO:
   /* Process Part Two */
   iSolution = 0.
   
   /* Calcolate Solution for Part 2 */
   
   OUTPUT TO "clipboard".
   PUT UNFORMATTED iSolution SKIP.
   OUTPUT CLOSE.

   MESSAGE 
      SUBSTITUTE ("Solution: &1.", iSolution) SKIP (1)
      SUBSTITUTE ("Found solution in &1 msecs.", ETIME)
   VIEW-AS ALERT-BOX TITLE " 2023 - Day 10 - Part Two".
   
   IF lvlShow THEN DO:
      RUN sy\win\wbrowsett.w
         (INPUT TEMP-TABLE ttGrid:HANDLE).
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
         (INPUT TEMP-TABLE ttGrid:HANDLE).
   END.
   RETURN.      
END CATCH.


/* **********************  Internal Procedures  *********************** */




