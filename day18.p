
/*------------------------------------------------------------------------
    File        : day18.p
    Purpose     : Solve Day 18 of Advent of Code 2023

    Syntax      :

    Description : Solution Advent of Code 2023 - Day 18

    Author(s)   : Wim van der Ham (WITS)
    Created     : Sat Jan 06 15:21:44 CET 2024
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
DEFINE VARIABLE iDay         AS INTEGER   NO-UNDO INITIAL 18.
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
DEFINE VARIABLE lviMethod    AS INTEGER   NO-UNDO.

/* Specific */
DEFINE TEMP-TABLE ttLine
   FIELD IDLine      AS INTEGER 
   FIELD cInputLine  AS CHARACTER FORMAT "X(80)"
INDEX indLine IS UNIQUE IDLine.

DEFINE TEMP-TABLE ttInstruction
   FIELD IDInstruction AS INTEGER 
   FIELD Direction     AS CHARACTER 
   FIELD Steps         AS INTEGER 
   FIELD HexColor      AS CHARACTER 
   FIELD NewDirection  AS CHARACTER 
   FIELD NewSteps      AS INTEGER 
INDEX indID IS UNIQUE PRIMARY IDInstruction.
DEFINE VARIABLE iNewIDInstruction AS INTEGER NO-UNDO.

DEFINE TEMP-TABLE ttDirection
   FIELD IDDirection AS INTEGER 
   FIELD Direction   AS CHARACTER 
   FIELD deltaX      AS INTEGER 
   FIELD deltaY      AS INTEGER 
   FIELD Opposite    AS CHARACTER 
   FIELD Arrow       AS CHARACTER 
INDEX indID IS UNIQUE IDDirection
INDEX indDirection IS UNIQUE PRIMARY Direction.
DEFINE VARIABLE iNewIDDirection AS INTEGER NO-UNDO.

DEFINE TEMP-TABLE ttGrid
   FIELD IDGrid     AS INTEGER
   FIELD iX         AS INTEGER  
   FIELD iY         AS INTEGER 
   FIELD Symbol     AS CHARACTER
   FIELD Touched    AS INTEGER 
   FIELD Directions AS CHARACTER 
INDEX indID     IS UNIQUE IDGrid
INDEX indGridYX IS UNIQUE PRIMARY iY iX
INDEX indGridXY iX iY
INDEX indSymbol Symbol.
DEFINE VARIABLE iNewIDGrid AS INTEGER   NO-UNDO.
DEFINE BUFFER ttNextGrid FOR ttGrid.

DEFINE TEMP-TABLE ttPoint
   FIELD IDPoint AS INTEGER 
   FIELD iX      AS INTEGER 
   FIELD iY      AS INTEGER 
INDEX indID IS UNIQUE IDPoint.
DEFINE VARIABLE iNewIDPoint AS INTEGER NO-UNDO.
DEFINE BUFFER ttNextPoint FOR ttPoint.
DEFINE BUFFER ttPrevPoint FOR ttPoint.


DEFINE VARIABLE iX             AS INTEGER   NO-UNDO.
DEFINE VARIABLE iY             AS INTEGER   NO-UNDO.
DEFINE VARIABLE iMinX          AS INTEGER   NO-UNDO.
DEFINE VARIABLE iMaxX          AS INTEGER   NO-UNDO.
DEFINE VARIABLE iMinY          AS INTEGER   NO-UNDO.
DEFINE VARIABLE iMaxY          AS INTEGER   NO-UNDO.
DEFINE VARIABLE iStep          AS INTEGER   NO-UNDO.
DEFINE VARIABLE lInside        AS LOGICAL   NO-UNDO.
DEFINE VARIABLE lUp            AS LOGICAL   NO-UNDO.
/* Alternative formula Shoelace formula */
DEFINE VARIABLE iSum           AS INTEGER NO-UNDO.
DEFINE VARIABLE iArea          AS INTEGER NO-UNDO.
DEFINE VARIABLE iInterior      AS INTEGER NO-UNDO.
DEFINE VARIABLE iBoundary      AS INTEGER NO-UNDO.


/* ********************  Preprocessor Definitions  ******************** */

/* ************************  Function Prototypes ********************** */


FUNCTION getInteger RETURNS INTEGER 
   ( INPUT ipcHexString AS CHARACTER ) FORWARD.

{AOC_session.i}

/* ***************************  Main Block  *************************** */

DISPLAY
   SUBSTITUTE ("Year &1 Day &2", iYear, iDay) FORMAT "X(16)" NO-LABELS SKIP
   lOpenURL  LABEL "Open URL?"       VIEW-AS TOGGLE-BOX SKIP
   lDownload LABEL "Download Input?" VIEW-AS TOGGLE-BOX SKIP   
   lPart[1]  LABEL "Solve Part 1?"   VIEW-AS TOGGLE-BOX SKIP
   lviMethod LABEL "Choose method"   VIEW-AS FILL-IN FORMAT "9" SKIP 
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
   lPart[1]
   lviMethod
   lPart[2]
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

   /* Parse input line into instruction with format:
      R 6 (#70c710)
      | |  Hexcolor
      | Steps
      Direction
   */
   iNewIDInstruction = iNewIDInstruction + 1.
   CREATE ttInstruction.
   ASSIGN 
      ttInstruction.IDInstruction = iNewIDInstruction
      ttInstruction.Direction     = ENTRY (1, ttLine.cInputLine, " ")
      ttInstruction.Steps         = INTEGER (ENTRY (2, ttLine.cInputLine, " "))
      ttInstruction.HexColor      = TRIM (ENTRY (3, ttLine.cInputLine, " "), "()")
   .
         
END. /* ReadBlock: */

RUN fillDirections.

IF lvlShow THEN DO:
   RUN sy\win\wbrowsett.w
      (INPUT TEMP-TABLE ttInstruction:HANDLE). 
END.

IF lPart[1] THEN DO:
   /* Process Part One */
   IF lviMethod EQ 1 THEN DO:
      /* Calcolate Solution for Part 1, using Method 1 */
      ASSIGN 
         iX = 1
         iY = 1
      .
      ASSIGN 
         iMinX = iX
         iMaxX = iX
         iMinY = iY
         iMaxY = iY
      .
      
      /* Start Position */
      iNewIDGrid = iNewIDGrid + 1.
      CREATE ttGrid.
      ASSIGN 
         ttGrid.IDGrid = iNewIDGrid
         ttGrid.iX     = iX
         ttGrid.iY     = iY
         ttGrid.Symbol = "#"
      .
      /* Create "border" trench */  
      FOR EACH ttInstruction,
      FIRST ttDirection OF ttInstruction:
         DO iStep = 1 TO ttInstruction.Steps:
            ASSIGN 
               iX = iX + ttDirection.deltaX
               iY = iY + ttDirection.deltaY
            .
            FIND  ttGrid
            WHERE ttGrid.iX EQ iX
            AND   ttGrid.iY EQ iY NO-ERROR.
            IF NOT AVAILABLE ttGrid THEN DO:
               iNewIDGrid = iNewIDGrid + 1.
               CREATE ttGrid.
               ASSIGN 
                  ttGrid.IDGrid = iNewIDGrid
                  ttGrid.iX     = iX
                  ttGrid.iY     = iY
                  ttGrid.Symbol = "#"
               .
            END.
            ASSIGN 
               ttGrid.Touched = ttGrid.Touched + 1
            .
         END.
         IF iX GT iMaxX THEN iMaxX = iX.
         IF iX LT iMinX THEN iMinX = iX.
         IF iY GT iMaxY THEN iMaxY = iY.
         IF iY LT iMinY THEN iMinY = iY.
      END.
   
      IF lvlOutput THEN DO:
         RUN outputGrid
            (INPUT iDay,
             INPUT "Start",
             INPUT iMinX,
             INPUT iMaxX,
             INPUT iMinY,
             INPUT iMaxY).
      END.
      
   
      /* Now fill the interior */
      ASSIGN 
         lUp = ?
      .
      
      DO iY = iMinY TO iMaxY:
         ASSIGN 
            lInside = FALSE
         .
         DO iX = iMinX TO iMaxX:
            FIND  ttGrid
            WHERE ttGrid.iX EQ iX
            AND   ttGrid.iY EQ iY NO-ERROR.
            IF AVAILABLE ttGrid THEN DO:
               FOR EACH ttDirection,
               FIRST ttNextGrid
               WHERE ttNextGrid.iX     EQ ttGrid.iX + ttDirection.deltaX
               AND   ttNextGrid.iY     EQ ttGrid.iY + ttDirection.deltaY
               AND   ttNextGrid.Symbol EQ "#":
                  ttGrid.Directions = SUBSTITUTE ("&1&2&3",
                                                  ttGrid.Directions,
                                                  (IF ttGrid.Directions NE "" THEN "," ELSE ""),
                                                  ttDirection.Direction).
               END.
               CASE ttGrid.Directions:
                  WHEN "D,R" OR 
                  WHEN "R,D" THEN DO:
                     lUp = TRUE.
                  END.
                  WHEN "U,L" OR 
                  WHEN "L,U" THEN DO:
                     IF lUp EQ TRUE THEN 
                        lInside = NOT lInside.
                  END.
                  WHEN "L,D" OR 
                  WHEN "D,L" THEN DO:
                     IF lUp EQ FALSE THEN 
                        lInside = NOT lInside.
                  END.
                  WHEN "U,R" OR 
                  WHEN "R,U" THEN 
                     lUp = FALSE.
                  WHEN "U,D" OR 
                  WHEN "D,U" THEN DO:
                     lInside = NOT lInside.
                  END.
                  WHEN "R"   OR 
                  WHEN "L,R" OR 
                  WHEN "R,L" THEN DO:
                     /* Postpone decision */
                  END.
                  OTHERWISE DO:
                     MESSAGE SUBSTITUTE ("Unexpected directions from (&1,&2): &3.", 
                                         ttGrid.iX,
                                         ttGrid.iY,
                                         ttGrid.Directions)
                     VIEW-AS ALERT-BOX.
                  END.
               END CASE.
            END.
            ELSE DO:
               IF lInside EQ TRUE THEN DO:
                  /* Create ttGrid for interior */
                  iNewIDGrid = iNewIDGrid + 1.
                  CREATE ttGrid.
                  ASSIGN 
                     ttGrid.IDGrid  = iNewIDGrid
                     ttGrid.iX      = iX
                     ttGrid.iY      = iY
                     ttGrid.Symbol  = "*"
                     ttGrid.Touched = 1
                  .
               END.
            END.
         END.
      END.
         
      IF lvlOutput THEN DO:
         RUN outputGrid
            (INPUT iDay,
             INPUT "Fill",
             INPUT iMinX,
             INPUT iMaxX,
             INPUT iMinY,
             INPUT iMaxY).
      END.
      
      iSolution = 0.
      FOR EACH ttGrid:
         iSolution = iSolution + 1.
      END.
               
      OUTPUT TO "clipboard".
      PUT UNFORMATTED iSolution SKIP.
      OUTPUT CLOSE.
      MESSAGE 
         SUBSTITUTE ("Solution: &1.", iSolution) SKIP (1)
         SUBSTITUTE ("Found solution in &1 msecs.", ETIME)
      VIEW-AS ALERT-BOX TITLE " 2023 - Day 18 - Part One".
      
      IF lvlShow THEN DO:
         RUN sy\win\wbrowsett.w
            (INPUT TEMP-TABLE ttGrid:HANDLE).
      END.
   END.   

   IF lviMethod EQ 2 THEN DO:   
      /* Alternative Method 
      ** Shoelace formula: https://en.wikipedia.org/wiki/Shoelace_formula
      */
      ASSIGN 
         iX = 0
         iY = 0
      .
      iNewIDPoint = iNewIDPoint + 1.
      CREATE ttPoint.
      ASSIGN 
         ttPoint.IDPoint = iNewIDPoint
         ttPoint.iX      = iX
         ttPoint.iY      = iY
      .
      
      FOR EACH ttInstruction,
      FIRST ttDirection OF ttInstruction:
         ASSIGN 
            iX = iX + ttDirection.deltaX * ttInstruction.Steps
            iY = iY + ttDirection.deltaY * ttInstruction.Steps
         .
         iNewIDPoint = iNewIDPoint + 1.
         CREATE ttPoint.
         ASSIGN 
            ttPoint.IDPoint = iNewIDPoint
            ttPoint.iX      = iX
            ttPoint.iY      = iY
         .
         iBoundary = iBoundary + ttInstruction.Steps. 
      END.
      
      edProgress:INSERT-STRING (SUBSTITUTE ("Boundaries: &1~n", iBoundary)).
      
      FOR EACH ttPoint:
         FIND  ttPrevPoint
         WHERE ttPrevPoint.IDPoint EQ ttPoint.IDPoint - 1 NO-ERROR.
         IF NOT AVAILABLE ttPrevPoint THEN 
            FIND LAST ttPrevPoint.
         FIND  ttNextPoint 
         WHERE ttNextPoint.IDPoint = ttPoint.IDPoint + 1 NO-ERROR.
         IF NOT AVAILABLE ttNextPoint THEN 
            FIND FIRST ttNextPoint.
         iSum = iSum + ttPoint.iX * (ttPrevPoint.iY - ttNextPoint.iY).
         edProgress:INSERT-STRING (SUBSTITUTE ("Point (&1,&2) Next(&5,&6) Previous (&3,&4)  Sum &7~n", 
                                               ttPoint.iX,
                                               ttPoint.iY,
                                               ttPrevPoint.iX,
                                               ttPrevPoint.iY,
                                               ttNextPoint.iX,
                                               ttNextPoint.iY,
                                               iSum)).
      END.
      iSum = ABSOLUTE (iSum).
      iArea = iSum / 2.
      edProgress:INSERT-STRING (SUBSTITUTE ("Area: &1 (&2 / 2)~n", iArea, iSum)).
      /* Use Pick's Theorem for interior points */
      iInterior = iArea - (iBoundary / 2) + 1.
      edProgress:INSERT-STRING (SUBSTITUTE ("Interior: &1~n", iInterior)).
      edProgress:INSERT-STRING (SUBSTITUTE ("Boundary: &1~n", iBoundary)).
      iArea = iInterior + iBoundary.
      iSolution = iArea.
      edProgress:INSERT-STRING (SUBSTITUTE ("Solution: &1~n", iSolution)).
      
      OUTPUT TO "clipboard".
      PUT UNFORMATTED iSolution SKIP.
      OUTPUT CLOSE.
      MESSAGE 
         SUBSTITUTE ("Solution: &1.", iSolution) SKIP (1)
         SUBSTITUTE ("Found solution in &1 msecs.", ETIME)
      VIEW-AS ALERT-BOX TITLE " 2023 - Day 18 - Part One".
      
      IF lvlShow THEN DO:
         RUN sy\win\wbrowsett.w
            (INPUT TEMP-TABLE ttPoint:HANDLE).
      END.
      ENABLE edProgress WITH FRAME fr-Parameters.
      WAIT-FOR CLOSE OF THIS-PROCEDURE.
   END.   
END. /* Process Part One */

IF lPart[2] THEN DO:
   /* Process Part Two */
   iSolution = 0.
   
   /* Calcolate Solution for Part 2 */
   FOR EACH ttInstruction:
      /* Determine new values for Direction and Steps */
      ASSIGN
         ttInstruction.NewDirection = SUBSTRING ("RDLU", INTEGER (SUBSTRING (ttInstruction.HexColor, 7, 1)) + 1, 1)
         ttInstruction.NewSteps     = getInteger(SUBSTRING (ttInstruction.HexColor, 2, 5))
      .
   END.
   IF lvlShow THEN DO:
      RUN sy\win\wbrowsett.w
         (INPUT TEMP-TABLE ttInstruction:HANDLE).
   END.
   
   /* Calcolate Solution for Part 2 */
   EMPTY TEMP-TABLE ttGrid.
   ASSIGN 
      iX = 1
      iY = 1
   .
   ASSIGN 
      iMinX = iX
      iMaxX = iX
      iMinY = iY
      iMaxY = iY
   .
   
   /* Start Position */
   iNewIDGrid = iNewIDGrid + 1.
   CREATE ttGrid.
   ASSIGN 
      ttGrid.IDGrid = iNewIDGrid
      ttGrid.iX     = iX
      ttGrid.iY     = iY
      ttGrid.Symbol = "#"
   .
   /* Create "border" trench */  
   FOR EACH ttInstruction,
   FIRST ttDirection 
   WHERE ttDirection.Direction EQ ttInstruction.NewDirection:
      ACCUM "" (COUNT).
      IF (ACCUM COUNT "") MOD 10 EQ 0 THEN DO:
         IF lvlShow THEN DO:
            edProgress:INSERT-STRING (SUBSTITUTE ("Instruction #&1: Move &2 steps in direction: &3.~n",
                                                  ttInstruction.IDInstruction,
                                                  ttInstruction.NewSteps,
                                                  ttInstruction.NewDirection)).
            PROCESS EVENTS.                                                  
         END.                                                  
      END.                                                  
      DO iStep = 1 TO ttInstruction.NewSteps:
         ASSIGN 
            iX = iX + ttDirection.deltaX
            iY = iY + ttDirection.deltaY
         .
         FIND  ttGrid
         WHERE ttGrid.iX EQ iX
         AND   ttGrid.iY EQ iY NO-ERROR.
         IF NOT AVAILABLE ttGrid THEN DO:
            iNewIDGrid = iNewIDGrid + 1.
            CREATE ttGrid.
            ASSIGN 
               ttGrid.IDGrid = iNewIDGrid
               ttGrid.iX     = iX
               ttGrid.iY     = iY
               ttGrid.Symbol = "#"
            .
         END.
         ASSIGN 
            ttGrid.Touched = ttGrid.Touched + 1
         .
      END.
      IF iX GT iMaxX THEN iMaxX = iX.
      IF iX LT iMinX THEN iMinX = iX.
      IF iY GT iMaxY THEN iMaxY = iY.
      IF iY LT iMinY THEN iMinY = iY.
   END.

   IF lvlOutput THEN DO:
      RUN outputGrid
         (INPUT iDay,
          INPUT "2_Start",
          INPUT iMinX,
          INPUT iMaxX,
          INPUT iMinY,
          INPUT iMaxY).
   END.
   

   /* Now fill the interior */
   ASSIGN 
      lUp = ?
   .
   
   DO iY = iMinY TO iMaxY:
      ASSIGN 
         lInside = FALSE
      .
      DO iX = iMinX TO iMaxX:
         FIND  ttGrid
         WHERE ttGrid.iX EQ iX
         AND   ttGrid.iY EQ iY NO-ERROR.
         IF AVAILABLE ttGrid THEN DO:
            FOR EACH ttDirection,
            FIRST ttNextGrid
            WHERE ttNextGrid.iX     EQ ttGrid.iX + ttDirection.deltaX
            AND   ttNextGrid.iY     EQ ttGrid.iY + ttDirection.deltaY
            AND   ttNextGrid.Symbol EQ "#":
               ttGrid.Directions = SUBSTITUTE ("&1&2&3",
                                               ttGrid.Directions,
                                               (IF ttGrid.Directions NE "" THEN "," ELSE ""),
                                               ttDirection.Direction).
            END.
            CASE ttGrid.Directions:
               WHEN "D,R" OR 
               WHEN "R,D" THEN DO:
                  lUp = TRUE.
               END.
               WHEN "U,L" OR 
               WHEN "L,U" THEN DO:
                  IF lUp EQ TRUE THEN 
                     lInside = NOT lInside.
               END.
               WHEN "L,D" OR 
               WHEN "D,L" THEN DO:
                  IF lUp EQ FALSE THEN 
                     lInside = NOT lInside.
               END.
               WHEN "U,R" OR 
               WHEN "R,U" THEN 
                  lUp = FALSE.
               WHEN "U,D" OR 
               WHEN "D,U" THEN DO:
                  lInside = NOT lInside.
               END.
               WHEN "R"   OR 
               WHEN "L,R" OR 
               WHEN "R,L" THEN DO:
                  /* Postpone decision */
               END.
               OTHERWISE DO:
                  MESSAGE SUBSTITUTE ("Unexpected directions from (&1,&2): &3.", 
                                      ttGrid.iX,
                                      ttGrid.iY,
                                      ttGrid.Directions)
                  VIEW-AS ALERT-BOX.
               END.
            END CASE.
         END.
         ELSE DO:
            IF lInside EQ TRUE THEN DO:
               /* Create ttGrid for interior */
               iNewIDGrid = iNewIDGrid + 1.
               CREATE ttGrid.
               ASSIGN 
                  ttGrid.IDGrid  = iNewIDGrid
                  ttGrid.iX      = iX
                  ttGrid.iY      = iY
                  ttGrid.Symbol  = "*"
                  ttGrid.Touched = 1
               .
            END.
         END.
      END.
   END.
      
   IF lvlOutput THEN DO:
      RUN outputGrid
         (INPUT iDay,
          INPUT "2_Fill",
          INPUT iMinX,
          INPUT iMaxX,
          INPUT iMinY,
          INPUT iMaxY).
   END.
   
   iSolution = 0.
   FOR EACH ttGrid:
      iSolution = iSolution + 1.
   END.
    
   OUTPUT TO "clipboard".
   PUT UNFORMATTED iSolution SKIP.
   OUTPUT CLOSE.

   MESSAGE 
      SUBSTITUTE ("Solution: &1.", iSolution) SKIP (1)
      SUBSTITUTE ("Found solution in &1 msecs.", ETIME)
   VIEW-AS ALERT-BOX TITLE " 2023 - Day 18 - Part Two".
   
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

PROCEDURE fillDirections:
/*------------------------------------------------------------------------------
 Purpose: Create the deltaX and deltaY records for the 4 directions
 Notes:
------------------------------------------------------------------------------*/
DEFINE VARIABLE cDirections AS CHARACTER NO-UNDO INITIAL "U,L,D,R".
DEFINE VARIABLE cArrows     AS CHARACTER NO-UNDO INITIAL "^,<,V,>".
DEFINE VARIABLE cOpposites  AS CHARACTER NO-UNDO INITIAL "D,R,U,L".
DEFINE VARIABLE cListaX     AS CHARACTER NO-UNDO INITIAL "0,-1,0,+1".
DEFINE VARIABLE cListaY     AS CHARACTER NO-UNDO INITIAL "-1,0,+1,0".

DEFINE VARIABLE iDirection AS INTEGER NO-UNDO.

   DO iDirection = 1 TO 4:
      iNewIDDirection = iNewIDDirection + 1.
      CREATE ttDirection.
      ASSIGN 
         ttDirection.IDDirection = iNewIDDirection
         ttDirection.Direction   = ENTRY (iDirection, cDirections)
         ttDirection.deltaX      = INTEGER (ENTRY (iDirection, cListaX))
         ttDirection.deltaY      = INTEGER (ENTRY (iDirection, cListaY))
         ttDirection.Opposite    = ENTRY (iDirection, cOpposites)
         ttDirection.Arrow       = ENTRY (iDirection, cArrows)
      .
   END.

END PROCEDURE.



PROCEDURE outputGrid:
/*------------------------------------------------------------------------------
 Purpose: Output a grid to a file
 Notes:
------------------------------------------------------------------------------*/
DEFINE INPUT  PARAMETER ipiDay     AS INTEGER   NO-UNDO.
DEFINE INPUT  PARAMETER ipcNr      AS CHARACTER NO-UNDO.
DEFINE INPUT  PARAMETER ipiMinX    AS INTEGER   NO-UNDO.
DEFINE INPUT  PARAMETER ipiMaxX    AS INTEGER   NO-UNDO.
DEFINE INPUT  PARAMETER ipiMinY    AS INTEGER   NO-UNDO.
DEFINE INPUT  PARAMETER ipiMaxY    AS INTEGER   NO-UNDO.

DEFINE VARIABLE cOutputFile AS CHARACTER NO-UNDO.

DEFINE BUFFER ttGrid      FOR ttGrid.

DEFINE VARIABLE iX    AS INTEGER   NO-UNDO.
DEFINE VARIABLE iY    AS INTEGER   NO-UNDO.

   cOutputFile = SUBSTITUTE ("output\&1_&2.txt", 
                             ipiDay,
                             ipcNr).

   OUTPUT TO VALUE (cOutputFile).
   /* Show Grid with path */
   DO iY = ipiMinY TO ipiMaxY:
      DO iX = ipiMinX TO ipiMaxX:
         FIND  ttGrid
         WHERE ttGrid.iX EQ iX
         AND   ttGrid.iY EQ iY NO-ERROR.
         IF AVAILABLE ttGrid THEN 
            PUT UNFORMATTED 
               ttGrid.Symbol.
         ELSE 
            PUT UNFORMATTED 
               ".".
      END.
      PUT UNFORMATTED SKIP. 
   END.
   
   OUTPUT CLOSE.            

END PROCEDURE.


/* ************************  Function Implementations ***************** */

FUNCTION getInteger RETURNS INTEGER 
   ( INPUT ipcHexString AS CHARACTER ):
/*------------------------------------------------------------------------------
 Purpose: Get integer value from hex string
 Notes:
------------------------------------------------------------------------------*/   
DEFINE VARIABLE iChar      AS INTEGER   NO-UNDO.
DEFINE VARIABLE cChar      AS CHARACTER NO-UNDO.
DEFINE VARIABLE cValues    AS CHARACTER NO-UNDO INITIAL "0123456789abcdef".
DEFINE VARIABLE iBase      AS INTEGER   NO-UNDO.
DEFINE VARIABLE iBaseValue AS INTEGER   NO-UNDO.
DEFINE VARIABLE iValue     AS INTEGER   NO-UNDO.

   iBase = 1.
   DO iChar = LENGTH (ipcHexString) TO 1 BY -1:
      cChar = SUBSTRING (ipcHexString, iChar, 1).
      iBaseValue = INTEGER (INDEX (cValues, cChar) - 1).
      iValue = iValue + (iBase * iBaseValue).
      iBase = iBase * 16.
   END.
   
   RETURN iValue.
      
END FUNCTION.
