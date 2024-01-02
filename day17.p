
/*------------------------------------------------------------------------
    File        : day17.p
    Purpose     : Solve Advent of Code 2023 - Day 17

    Syntax      :

    Description : Solution for Advent of Code 2023 - Day 17

    Author(s)   : Wim van der Ham (WITS)
    Created     : Mon Jan 01 21:53:29 CET 2024
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
DEFINE VARIABLE iDay         AS INTEGER   NO-UNDO INITIAL 17.
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

/* Specific */
DEFINE TEMP-TABLE ttLine
   FIELD IDLine      AS INTEGER 
   FIELD cInputLine  AS CHARACTER FORMAT "X(80)"
INDEX indLine IS UNIQUE IDLine.
    
DEFINE TEMP-TABLE ttGrid
   FIELD IDGrid     AS INTEGER 
   FIELD iX         AS INTEGER 
   FIELD iY         AS INTEGER 
   FIELD Number     AS INTEGER  
   FIELD Steps      AS INTEGER   INITIAL ?
   FIELD Previous   AS CHARACTER  
   FIELD HeatLoss   AS INTEGER 
   FIELD Touched    AS LOGICAL 
   FIELD Symbol     AS CHARACTER 
INDEX indID IS UNIQUE IDGrid
INDEX indXY IS PRIMARY iX iY.
DEFINE VARIABLE iNewIDGrid AS INTEGER NO-UNDO.

DEFINE BUFFER ttNextGrid FOR ttGrid.

DEFINE TEMP-TABLE ttDirection
   FIELD IDDirection AS INTEGER 
   FIELD Direction   AS CHARACTER 
   FIELD deltaX      AS INTEGER 
   FIELD deltaY      AS INTEGER 
   FIELD Opposite    AS CHARACTER 
   FIELD Arrow       AS CHARACTER 
INDEX indID IS UNIQUE IDDirection.
DEFINE VARIABLE iNewIDDirection AS INTEGER NO-UNDO.

DEFINE TEMP-TABLE ttPath
   FIELD IDPath   AS INTEGER 
   FIELD IDStart  AS INTEGER 
   FIELD IDEnd    AS INTEGER
   FIELD HeatLoss AS INTEGER
INDEX indID IS UNIQUE IDPath
INDEX indIDs IDStart IDEnd.
DEFINE VARIABLE iNewIDPath AS INTEGER NO-UNDO.  

DEFINE TEMP-TABLE ttStack
   FIELD IDStack   AS INTEGER 
   FIELD HeatLoss  AS INTEGER 
   FIELD iX        AS INTEGER 
   FIELD iY        AS INTEGER 
   FIELD Direction AS CHARACTER
   FIELD Nr        AS INTEGER 
   FIELD Previous  AS CHARACTER 
INDEX indID IS UNIQUE IDStack
INDEX popleft IS PRIMARY HeatLoss iX DESCENDING iY DESCENDING. 
DEFINE VARIABLE iNewIDStack AS INTEGER NO-UNDO.
DEFINE BUFFER ttNewStack FOR ttStack.

DEFINE TEMP-TABLE ttSeen
   FIELD IDSeen    AS INTEGER 
   FIELD iX        AS INTEGER 
   FIELD iY        AS INTEGER 
   FIELD Direction AS CHARACTER
   FIELD Nr        AS INTEGER 
INDEX indID IS UNIQUE IDSeen
INDEX indXY IS PRIMARY iX iY Direction Nr.
DEFINE VARIABLE iNewIDSeen AS INTEGER NO-UNDO.
      
DEFINE VARIABLE iCurrentRound AS INTEGER NO-UNDO.
DEFINE VARIABLE lFound        AS LOGICAL NO-UNDO.

DEFINE VARIABLE iX            AS INTEGER NO-UNDO.
DEFINE VARIABLE iY            AS INTEGER NO-UNDO.
DEFINE VARIABLE iMaxX         AS INTEGER NO-UNDO.
DEFINE VARIABLE iMaxY         AS INTEGER NO-UNDO.
    
/* ********************  Preprocessor Definitions  ******************** */

/* ************************  Function Prototypes ********************** */

FUNCTION getMinHeatLoss RETURNS INTEGER 
   (INPUT ipiIDStart  AS INTEGER,
    INPUT ipcPrevious AS CHARACTER, 
    INPUT ipiIDEnd    AS INTEGER  ) FORWARD.

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
   cInputFile = REPLACE (cInputFile, STRING (iDay, "99"), SUBSTITUTE ("&1_sample", STRING (iDay, "99"))).
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
   
   IF cLine EQ "." THEN 
      LEAVE.
      
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
         ttGrid.Number = INTEGER (cChar)
      .
   END.
   iMaxY = iLine.
   
END. /* ReadBlock: */

IF lvlShow THEN DO:
   RUN sy\win\wbrowsett.w
      (INPUT TEMP-TABLE ttGrid:HANDLE). 
END.

RUN fillDirections.

IF lPart[1] THEN DO:
   /* Process Part One */
   FIND  ttGrid
   WHERE ttGrid.iX EQ 1
   AND   ttGrid.iY EQ 1.
   FIND  ttNextGrid
   WHERE ttNextGrid.iX EQ iMaxX
   AND   ttNextGrid.iY EQ iMaxY.
   
   iNewIDStack = iNewIDStack + 1. 
   CREATE ttStack.
   ASSIGN 
      ttStack.IDStack   = iNewIDStack
      ttStack.iX        = ttGrid.iX
      ttStack.iY        = ttGrid.iY
      ttStack.Direction = ""
      ttStack.Previous  = ""
      ttStack.Nr        = 0
   .
   
   PopBlock:
   REPEAT:
      FIND FIRST ttStack USE-INDEX popleft.

      IF lvlShow THEN DO:
         edProgress:INSERT-STRING (SUBSTITUTE ("Pop #&1 (&2,&3) &4 x &5 &6.",
                                               ttStack.IDStack,
                                               ttStack.iX,
                                               ttStack.iY,
                                               ttStack.Nr,
                                               ttStack.Direction,
                                               ttStack.Previous)).
         PROCESS EVENTS.
      END.                                               

      IF  ttStack.iX EQ iMaxX
      AND ttStack.iY EQ iMaxY THEN DO:
         iSolution = ttStack.HeatLoss.
         IF lvlOutput THEN DO:
            RUN outputGrid
               (INPUT iDay,
                INPUT "End",
                INPUT ttStack.IDStack).
         END.
         LEAVE PopBlock.
      END.
      
      FIND  ttSeen 
      WHERE ttSeen.iX        EQ ttStack.iX
      AND   ttSeen.iY        EQ ttStack.iY
      AND   ttSeen.Direction EQ ttStack.Direction 
      AND   ttSeen.Nr        EQ ttStack.Nr NO-ERROR.
      IF AVAILABLE ttSeen THEN DO:
         DELETE ttStack. 
         NEXT PopBlock.
      END.

      iNewIDSeen = iNewIDSeen + 1. 
      CREATE ttSeen.
      ASSIGN 
         ttSeen.IDSeen    = iNewIDSeen
         ttSeen.iX        = ttStack.iX
         ttSeen.iY        = ttStack.iY
         ttSeen.Direction = ttStack.Direction
         ttSeen.Nr        = ttStack.Nr
      .
      
      FIND  ttGrid
      WHERE ttGrid.iX EQ ttStack.iX
      AND   ttGrid.iY EQ ttStack.iY.
            
      IF (ttStack.Nr LT 3 OR lvlDebug)
      AND ttStack.Direction NE "" THEN DO:
         /* Continue in this direction */
         FIND  ttDirection 
         WHERE ttDirection.Direction EQ ttStack.Direction.
         FIND  ttNextGrid
         WHERE ttNextGrid.iX EQ ttGrid.iX + ttDirection.deltaX
         AND   ttNextGrid.iY EQ ttGrid.iY + ttDirection.deltaY NO-ERROR.
         IF AVAILABLE ttNextGrid THEN DO:
            iNewIDStack = iNewIDStack + 1.
            CREATE ttNewStack.
            ASSIGN 
               ttNewStack.IDStack   = iNewIDStack
               ttNewStack.HeatLoss  = ttStack.HeatLoss + ttNextGrid.Number
               ttNewStack.iX        = ttNextGrid.iX
               ttNewStack.iY        = ttNextGrid.iY
               ttNewStack.Direction = ttDirection.Direction
               ttNewStack.Previous  = SUBSTITUTE ("&1&2", ttDirection.Direction, ttStack.Previous)
               ttNewStack.Nr        = ttStack.Nr + 1
            .
         END.
      END. /* Continue in this direction */
      
      FOR EACH ttDirection
      WHERE ttDirection.Direction NE ttStack.Direction
      AND   ttDirection.Opposite  NE ttStack.Direction,
      FIRST ttNextGrid
      WHERE ttNextGrid.iX = ttGrid.iX + ttDirection.deltaX
      AND   ttNextGrid.iY = ttGrid.iY + ttDirection.deltaY:
         /* Explore alternative directions */
         iNewIDStack = iNewIDStack + 1.
         CREATE ttNewStack.
         ASSIGN 
            ttNewStack.IDStack   = iNewIDStack
            ttNewStack.HeatLoss  = ttStack.HeatLoss + ttNextGrid.Number
            ttNewStack.iX        = ttNextGrid.iX
            ttNewStack.iY        = ttNextGrid.iY
            ttNewStack.Direction = ttDirection.Direction
            ttNewStack.Previous  = SUBSTITUTE ("&1&2", ttDirection.Direction, ttStack.Previous)
            ttNewStack.Nr        = 1
         .
      END. /* Explore alternative directions */
            
      DELETE ttStack.
      
   END. /* PopBlock */
      
   OUTPUT TO "clipboard".
   PUT UNFORMATTED iSolution SKIP.
   OUTPUT CLOSE.
   MESSAGE 
      SUBSTITUTE ("Solution: &1.", iSolution) SKIP (1)
      SUBSTITUTE ("Found solution in &1 msecs.", ETIME)
   VIEW-AS ALERT-BOX TITLE " 2023 - Day 17 - Part One".
   
   IF lvlShow THEN DO:
      RUN sy\win\wbrowsett.w
         (INPUT TEMP-TABLE ttGrid:HANDLE).
   END.            
END. /* Process Part One */

IF lPart[2] THEN DO:
   /* Process Part Two */
   iSolution = 0.

   EMPTY TEMP-TABLE ttStack.
   EMPTY TEMP-TABLE ttSeen.
   
   FIND  ttGrid
   WHERE ttGrid.iX EQ 1
   AND   ttGrid.iY EQ 1.
   FIND  ttNextGrid
   WHERE ttNextGrid.iX EQ iMaxX
   AND   ttNextGrid.iY EQ iMaxY.
   
   iNewIDStack = iNewIDStack + 1. 
   CREATE ttStack.
   ASSIGN 
      ttStack.IDStack   = iNewIDStack
      ttStack.iX        = ttGrid.iX
      ttStack.iY        = ttGrid.iY
      ttStack.Direction = ""
      ttStack.Previous  = ""
      ttStack.Nr        = 0
   .
   
   PopBlock:
   REPEAT:
      FIND FIRST ttStack USE-INDEX popleft.

      IF lvlShow THEN DO:
         edProgress:INSERT-STRING (SUBSTITUTE ("Pop #&1 (&2,&3) &4 x &5 &6.",
                                               ttStack.IDStack,
                                               ttStack.iX,
                                               ttStack.iY,
                                               ttStack.Nr,
                                               ttStack.Direction,
                                               ttStack.Previous)).
         PROCESS EVENTS.
      END.                                               

      IF  ttStack.iX EQ iMaxX
      AND ttStack.iY EQ iMaxY 
      AND ttStack.Nr GE 4 THEN DO:
         iSolution = ttStack.HeatLoss.
         LEAVE PopBlock.
      END.
      
      FIND  ttSeen 
      WHERE ttSeen.iX        EQ ttStack.iX
      AND   ttSeen.iY        EQ ttStack.iY
      AND   ttSeen.Direction EQ ttStack.Direction 
      AND   ttSeen.Nr        EQ ttStack.Nr NO-ERROR.
      IF AVAILABLE ttSeen THEN DO:
         DELETE ttStack. 
         NEXT PopBlock.
      END.

      iNewIDSeen = iNewIDSeen + 1. 
      CREATE ttSeen.
      ASSIGN 
         ttSeen.IDSeen    = iNewIDSeen
         ttSeen.iX        = ttStack.iX
         ttSeen.iY        = ttStack.iY
         ttSeen.Direction = ttStack.Direction
         ttSeen.Nr        = ttStack.Nr
      .
      
      FIND  ttGrid
      WHERE ttGrid.iX EQ ttStack.iX
      AND   ttGrid.iY EQ ttStack.iY.
            
      IF  ttStack.Nr LT 10
      AND ttStack.Direction NE "" THEN DO:
         /* Continue in this direction */
         FIND  ttDirection 
         WHERE ttDirection.Direction EQ ttStack.Direction.
         FIND  ttNextGrid
         WHERE ttNextGrid.iX EQ ttGrid.iX + ttDirection.deltaX
         AND   ttNextGrid.iY EQ ttGrid.iY + ttDirection.deltaY NO-ERROR.
         IF AVAILABLE ttNextGrid THEN DO:
            iNewIDStack = iNewIDStack + 1.
            CREATE ttNewStack.
            ASSIGN 
               ttNewStack.IDStack   = iNewIDStack
               ttNewStack.HeatLoss  = ttStack.HeatLoss + ttNextGrid.Number
               ttNewStack.iX        = ttNextGrid.iX
               ttNewStack.iY        = ttNextGrid.iY
               ttNewStack.Direction = ttDirection.Direction
               ttNewStack.Previous  = SUBSTITUTE ("&1&2", ttDirection.Direction, ttStack.Previous)
               ttNewStack.Nr        = ttStack.Nr + 1
            .
         END.
      END. /* Continue in this direction */
      
      IF ttStack.Nr GE 4
      OR ttStack.Direction EQ "" THEN DO:
         FOR EACH ttDirection
         WHERE ttDirection.Direction NE ttStack.Direction
         AND   ttDirection.Opposite  NE ttStack.Direction,
         FIRST ttNextGrid
         WHERE ttNextGrid.iX = ttGrid.iX + ttDirection.deltaX
         AND   ttNextGrid.iY = ttGrid.iY + ttDirection.deltaY:
            /* Explore alternative directions */
            iNewIDStack = iNewIDStack + 1.
            CREATE ttNewStack.
            ASSIGN 
               ttNewStack.IDStack   = iNewIDStack
               ttNewStack.HeatLoss  = ttStack.HeatLoss + ttNextGrid.Number
               ttNewStack.iX        = ttNextGrid.iX
               ttNewStack.iY        = ttNextGrid.iY
               ttNewStack.Direction = ttDirection.Direction
               ttNewStack.Previous  = SUBSTITUTE ("&1&2", ttDirection.Direction, ttStack.Previous)
               ttNewStack.Nr        = 1
            .
         END. /* Explore alternative directions */
      END.
                  
      DELETE ttStack.
      
   END. /* PopBlock */
   
   OUTPUT TO "clipboard".
   PUT UNFORMATTED iSolution SKIP.
   OUTPUT CLOSE.

   MESSAGE 
      SUBSTITUTE ("Solution: &1.", iSolution) SKIP (1)
      SUBSTITUTE ("Found solution in &1 msecs.", ETIME)
   VIEW-AS ALERT-BOX TITLE " 2023 - Day 17 - Part Two".
   
   IF lvlShow THEN DO:
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

PROCEDURE fillDirections:
/*------------------------------------------------------------------------------
 Purpose: Create the deltaX and deltaY records for the 4 directions
 Notes:
------------------------------------------------------------------------------*/
DEFINE VARIABLE cDirections AS CHARACTER NO-UNDO INITIAL "N,W,S,E".
DEFINE VARIABLE cArrows     AS CHARACTER NO-UNDO INITIAL "^,<,V,>".
DEFINE VARIABLE cOpposites  AS CHARACTER NO-UNDO INITIAL "S,E,N,W".
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
DEFINE INPUT  PARAMETER ipiIDStack AS INTEGER   NO-UNDO.

DEFINE VARIABLE cOutputFile AS CHARACTER NO-UNDO.

DEFINE BUFFER ttStack     FOR ttStack.
DEFINE BUFFER ttDirection FOR ttDirection.
DEFINE BUFFER ttGrid      FOR ttGrid.
DEFINE BUFFER ttNextGrid  FOR ttGrid.

DEFINE VARIABLE iGrid AS INTEGER   NO-UNDO.
DEFINE VARIABLE iX    AS INTEGER   NO-UNDO.
DEFINE VARIABLE iY    AS INTEGER   NO-UNDO.
DEFINE VARIABLE cPath AS CHARACTER NO-UNDO.

   cOutputFile = SUBSTITUTE ("output\&1_&2.txt", 
                             ipiDay,
                             ipcNr).

   
   FIND ttStack WHERE ttStack.IDStack EQ ipiIDStack.
   
   ASSIGN 
      iX = 1
      iY = 1
   .
   FIND  ttGrid 
   WHERE ttGrid.iX EQ iX
   AND   ttGrid.iY EQ iY.
   
   /* Fill in arrows in grid */
   DO iChar = LENGTH (ttStack.Previous) TO 1 BY -1:
      cChar = SUBSTRING (ttStack.Previous, iChar, 1).
      FIND  ttDirection
      WHERE ttDirection.Direction EQ cChar.
      cPath = SUBSTITUTE ("&1&2&3", cPath, (IF cPath NE "" THEN ", " ELSE ""), ttDirection.Arrow).
      FIND  ttNextGrid
      WHERE ttNextGrid.iX EQ (ttGrid.iX + ttDirection.deltaX)
      AND   ttNextGrid.iY EQ (ttGrid.iY + ttDirection.deltaY).
      ASSIGN 
         ttNextGrid.Touched = TRUE  
         ttNextGrid.Symbol  = ttDirection.Arrow
      .
      FIND ttGrid WHERE ttGrid.IDGrid EQ ttNextGrid.IDGrid.
   END.
   
   OUTPUT TO VALUE (cOutputFile).
   DO iGrid = 1 TO 2:
      /* Show starting Grid with numbers and
      ** Grid with path
      */
      FOR EACH ttGrid
      BREAK 
      BY ttGrid.iY
      BY ttGrid.iX:
         IF ttGrid.Touched AND iGrid EQ 2 THEN
            PUT UNFORMATTED 
               ttGrid.Symbol.
         ELSE 
            PUT UNFORMATTED 
               ttGrid.Number.
   
         IF LAST-OF (ttGrid.iY) THEN DO:
            PUT UNFORMATTED 
               SKIP.
         END.
      END.
      
      PUT UNFORMATTED SKIP (1).
   END.
   
   PUT UNFORMATTED SKIP (1)
      "Heat Loss: " ttStack.HeatLoss SKIP 
      "Path.....: " cPath            SKIP.
      
   OUTPUT CLOSE.            

END PROCEDURE.


/* ************************  Function Implementations ***************** */

