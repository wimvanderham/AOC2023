
/*------------------------------------------------------------------------
    File        : day16.p
    Purpose     : Solve Day 16 of Advent of Code 2023

    Syntax      :

    Description : Solution for Advent of Code 2023 - Day 16

    Author(s)   : Wim van der Ham (WITS)
    Created     : Sat Dec 16 14:25:30 CET 2023
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
DEFINE VARIABLE iDay         AS INTEGER   NO-UNDO INITIAL 16.
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
   FIELD Symbol     AS CHARACTER 
   FIELD Touched    AS LOGICAL 
INDEX indID IS UNIQUE IDGrid.
DEFINE VARIABLE iNewIDGrid AS INTEGER NO-UNDO.

DEFINE TEMP-TABLE ttBeam
   FIELD IDBeam    AS INTEGER 
   FIELD iX        AS INTEGER 
   FIELD iY        AS INTEGER 
   FIELD iRound    AS INTEGER
   FIELD Direction AS CHARACTER  
INDEX indID IS UNIQUE IDBeam
INDEX indRound IS PRIMARY iRound iX iY.
DEFINE BUFFER ttSplitBeam FOR ttBeam.
DEFINE VARIABLE iNewIDBeam AS INTEGER NO-UNDO.

DEFINE TEMP-TABLE ttGridBeam
   FIELD IDGrid    AS INTEGER 
   FIELD Direction AS CHARACTER 
INDEX indID IS UNIQUE IDGrid Direction.
 
DEFINE VARIABLE iCurrentRound AS INTEGER NO-UNDO.
DEFINE VARIABLE iBeamsOnGrid  AS INTEGER NO-UNDO.
    
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

   IF cLine EQ "" THEN DO:
      /* Empty Line */
      NEXT.
   END.
      
   CREATE ttLine.
   ASSIGN 
      ttLine.IDLine     = iLine
      ttLine.cInputLine = cLine
   .
   
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
END. /* ReadBlock: */

IF lvlShow THEN DO:
   RUN sy\win\wbrowsett.w
      (INPUT TEMP-TABLE ttGrid:HANDLE). 
END.

IF lPart[1] THEN DO:
   /* Process Part One */
   RUN getSolution
      (INPUT  1,
       INPUT  1,
       INPUT  ">",
       OUTPUT iSolution).
   
   OUTPUT TO "clipboard".
   PUT UNFORMATTED iSolution SKIP.
   OUTPUT CLOSE.
   MESSAGE 
      SUBSTITUTE ("Solution: &1.", iSolution) SKIP (1)
      SUBSTITUTE ("Found solution in &1 msecs.", ETIME)
   VIEW-AS ALERT-BOX TITLE " 2023 - Day 15 - Part One".
   
   IF lvlShow THEN DO:
      RUN sy\win\wbrowsett.w
         (INPUT TEMP-TABLE ttGrid:HANDLE).
   END.            
END. /* Process Part One */

IF lPart[2] THEN DO:
   /* Process Part Two */
   iSolution = 0.
   
   OUTPUT TO "clipboard".
   PUT UNFORMATTED iSolution SKIP.
   OUTPUT CLOSE.

   MESSAGE 
      SUBSTITUTE ("Solution: &1.", iSolution) SKIP (1)
      SUBSTITUTE ("Found solution in &1 msecs.", ETIME)
   VIEW-AS ALERT-BOX TITLE " 2023 - Day 15 - Part Two".
   
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

PROCEDURE getSolution:
/*------------------------------------------------------------------------------
 Purpose: Calcolate Solution from Starting Point and Direction
 Notes:
------------------------------------------------------------------------------*/
DEFINE INPUT  PARAMETER ipiX         AS INTEGER   NO-UNDO.
DEFINE INPUT  PARAMETER ipiY         AS INTEGER   NO-UNDO.
DEFINE INPUT  PARAMETER ipcDirection AS CHARACTER NO-UNDO.
DEFINE OUTPUT PARAMETER opiSolution  AS INTEGER   NO-UNDO.

DEFINE BUFFER ttSplitBeam FOR ttBeam.

   /* Prepare the first Beam */
   iNewIDBeam = iNewIDBeam + 1.
   CREATE ttBeam.
   ASSIGN 
      ttBeam.IDBeam    = iNewIDBeam
      ttBeam.iX        = ipiX
      ttBeam.iY        = ipiY
      ttBeam.Direction = ipcDirection
      ttBeam.iRound    = 0
   .
   
   iCurrentRound = 0.
   MainRepeat:
   REPEAT:
      /* Main Repeat */
      IF lvlDebug THEN DO:
         MESSAGE "Start" iCurrentRound
         VIEW-AS ALERT-BOX.
      END.
      IF lvlOutput THEN 
         RUN outputGrid
            (INPUT iCurrentRound).
      iBeamsOnGrid = 0.
      FOR EACH ttBeam
      WHERE ttBeam.iRound EQ iCurrentRound:
         /* For Each Beam in this Round */
         FIND  ttGrid
         WHERE ttGrid.iX EQ ttBeam.iX
         AND   ttGrid.iY EQ ttBeam.iY NO-ERROR.
         IF AVAILABLE ttGrid THEN DO:
            /* Beam still on the Grid */
            iBeamsOnGrid = iBeamsOnGrid + 1.
            FIND  ttGridBeam
            WHERE ttGridBeam.IDGrid    EQ ttGrid.IDGrid
            AND   ttGridBeam.Direction EQ ttBeam.Direction NO-ERROR.
            IF NOT AVAILABLE ttGridBeam THEN DO:
               /* First time this beam direction on this grid */
               CREATE ttGridBeam.
               ASSIGN 
                  ttGridBeam.IDGrid    = ttGrid.IDGrid
                  ttGridBeam.Direction = ttBeam.Direction
               . 
               ttGrid.Touched = TRUE.
               CASE ttGrid.Symbol:
                  WHEN "/" THEN DO:
                     CASE ttBeam.Direction:
                        WHEN ">" THEN 
                           ttBeam.Direction = "^".
                        WHEN "<" THEN 
                           ttBeam.Direction = "v".
                        WHEN "^" THEN 
                           ttBeam.Direction = ">".
                        WHEN "v" THEN 
                           ttBeam.Direction = "<".
                     END CASE. /* ttBeam.Direction */
                  END.
                  WHEN "\" THEN DO:
                     CASE ttBeam.Direction:
                        WHEN ">" THEN 
                           ttBeam.Direction = "v".
                        WHEN "<" THEN 
                           ttBeam.Direction = "^".
                        WHEN "^" THEN 
                           ttBeam.Direction = "<".
                        WHEN "v" THEN 
                           ttBeam.Direction = ">".
                     END CASE. /* ttBeam.Direction */
                  END.
                  WHEN "|" THEN DO:
                     CASE ttBeam.Direction:
                        WHEN ">" OR 
                        WHEN "<" THEN DO:
                           /* Split Beam in Two */
                           ttBeam.Direction = "^".
                           iNewIDBeam = iNewIDBeam + 1.
                           CREATE ttSplitBeam.
                           ASSIGN
                              ttSplitBeam.IDBeam    = iNewIDBeam
                              ttSplitBeam.iX        = ttBeam.iX
                              ttSplitBeam.iY        = ttBeam.iY
                              ttSplitBeam.Direction = "v"
                              ttSplitBeam.iRound    = ttBeam.iRound
                           .
                           iBeamsOnGrid = iBeamsOnGrid + 1.
                        END.
                     END CASE. /* ttBeam.Direction */
                  END.
                  WHEN "-" THEN DO:
                     CASE ttBeam.Direction:
                        WHEN "^" OR 
                        WHEN "v" THEN DO:
                           /* Split Beam in Two */
                           ttBeam.Direction = "<".
                           iNewIDBeam = iNewIDBeam + 1.
                           CREATE ttSplitBeam.
                           ASSIGN
                              ttSplitBeam.IDBeam    = iNewIDBeam
                              ttSplitBeam.iX        = ttBeam.iX
                              ttSplitBeam.iY        = ttBeam.iY
                              ttSplitBeam.Direction = ">"
                              ttSplitBeam.iRound    = ttBeam.iRound
                           .
                           iBeamsOnGrid = iBeamsOnGrid + 1.
                        END.
                     END CASE. /* ttBeam.Direction */
                  END.                            
               END CASE.
               CASE ttBeam.Direction:
                  WHEN ">" THEN
                     ttBeam.iX = ttBeam.iX + 1.
                  WHEN "<" THEN
                     ttBeam.iX = ttBeam.iX - 1.
                  WHEN "^" THEN 
                     ttBeam.iY = ttBeam.iY - 1.
                  WHEN "v" THEN
                     ttBeam.iY = ttBeam.iY + 1.
               END.
               ttBeam.iRound = ttBeam.iRound + 1.
               IF AVAILABLE ttSplitBeam THEN DO:
                  /* Available Split Beam */
                  CASE ttSplitBeam.Direction:
                     WHEN ">" THEN
                        ttSplitBeam.iX = ttSplitBeam.iX + 1.
                     WHEN "<" THEN
                        ttSplitBeam.iX = ttSplitBeam.iX - 1.
                     WHEN "^" THEN 
                        ttSplitBeam.iY = ttSplitBeam.iY - 1.
                     WHEN "v" THEN
                        ttSplitBeam.iY = ttSplitBeam.iY + 1.
                  END.
                  ttSplitBeam.iRound = ttSplitBeam.iRound + 1.
               END. /* Available Split Beam */
            END. /* First time this beam direction on this grid */
            ELSE DO:
               ASSIGN 
                  ttBeam.iX = 0
                  ttBeam.iY = 0
               .
            END.
         END. /* Beam still on the Grid */
      END. /* For Each Beam in this Round */
      
      IF lvlShow THEN DO:
         FOR EACH ttGrid
         WHERE ttGrid.Touched:
            ACCUM "" (COUNT).
         END.
         DO WITH FRAME fr-Parameters: 
            edProgress:INSERT-STRING (SUBSTITUTE ("Round: &1 Touched: &2 Beams: &3~n",
                                                  iCurrentRound,
                                                  (ACCUM COUNT ""),
                                                  iBeamsOnGrid)).
            PROCESS EVENTS.
         END.                                               
      END.                                               
      IF iBeamsOnGrid EQ 0 THEN
         LEAVE MainRepeat.
      iCurrentRound = iCurrentRound + 1.
   END. /* Main Repeat */ 

   FOR EACH ttGrid
   WHERE ttGrid.Touched:
      opiSolution = opiSolution + 1.
   END.
   
END PROCEDURE.

PROCEDURE outputGrid:
/*------------------------------------------------------------------------------
 Purpose: Output a grid to a file
 Notes:
------------------------------------------------------------------------------*/
DEFINE INPUT  PARAMETER ipiRound AS INTEGER NO-UNDO.

DEFINE VARIABLE cOutputFile AS CHARACTER NO-UNDO.
   cOutputFile = SUBSTITUTE ("output\16_&1.txt", ipiRound).
   
   OUTPUT TO VALUE (cOutputFile).
   FOR EACH ttGrid
   BREAK 
   BY ttGrid.iY
   BY ttGrid.iX:
      IF ttGrid.Touched THEN
         PUT UNFORMATTED 
            "#".
      ELSE 
         PUT UNFORMATTED 
            ttGrid.Symbol.

      /*         
      IF ttGrid.Symbol EQ "." THEN DO:
         FOR EACH ttBeam
         WHERE    ttBeam.iX EQ ttGrid.iX
         AND      ttBeam.iY EQ ttGrid.iY:
            ACCUM "" (COUNT).
         END.
         IF (ACCUM COUNT "") GT 0 THEN DO:
            PUT UNFORMATTED
            (ACCUM COUNT "") MOD 10.
         END.
         ELSE DO:
            PUT UNFORMATTED 
               ttGrid.Symbol.
         END.
      END.
      ELSE DO:
         PUT UNFORMATTED 
            ttGrid.Symbol.
      END.
      */
      IF LAST-OF (ttGrid.iY) THEN DO:
         PUT UNFORMATTED 
            SKIP.
      END.
   END.

   FOR EACH ttBeam 
   WHERE ttBeam.iRound EQ ipiRound
   BREAK 
   BY ttBeam.iY
   BY ttBeam.iX:
      DISPLAY 
         ttBeam.iY        COLUMN-LABEL "Y"
         ttBeam.iX        COLUMN-LABEL "X"
         ttBeam.Direction COLUMN-LABEL "Direction" (COUNT)
      WITH STREAM-IO TITLE "Beam List".
   END.
   OUTPUT CLOSE.            

END PROCEDURE.

/* ************************  Function Implementations ***************** */

