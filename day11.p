
/*------------------------------------------------------------------------
    File        : day11.p
    Purpose     : Solve Advent of Code 2023 Day 11

    Syntax      :

    Description : Solution Advent of Code 2023 - Day 11

    Author(s)   : Wim van der Ham (WITS)
    Created     : Tue Dec 12 01:22:02 CET 2023
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
DEFINE VARIABLE iDay         AS INTEGER   NO-UNDO INITIAL 11.
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
   FIELD IDGrid    AS INTEGER
   FIELD iX        AS INTEGER  
   FIELD iY        AS INTEGER 
   FIELD Symbol    AS CHARACTER
   FIELD OriginalX AS INTEGER 
   FIELD OriginalY AS INTEGER 
INDEX indID     IS UNIQUE IDGrid
INDEX indGridYX IS UNIQUE PRIMARY iY iX
INDEX indGridXY iX iY
INDEX indSymbol Symbol.
DEFINE VARIABLE iNewIDGrid AS INTEGER   NO-UNDO.

DEFINE TEMP-TABLE ttGalaxy
   FIELD IDGalaxy AS INTEGER 
   FIELD iX       AS INTEGER 
   FIELD iY       AS INTEGER 
INDEX indID IS UNIQUE IDGalaxy.
DEFINE VARIABLE iNewIDGalaxy AS INTEGER NO-UNDO.
DEFINE BUFFER ttOtherGalaxy FOR ttGalaxy.
       
DEFINE VARIABLE iX             AS INTEGER   NO-UNDO.
DEFINE VARIABLE iY             AS INTEGER   NO-UNDO.
DEFINE VARIABLE cEmptyRows     AS CHARACTER NO-UNDO.
DEFINE VARIABLE cEmptyCols     AS CHARACTER NO-UNDO.
DEFINE VARIABLE lEmpty         AS LOGICAL   NO-UNDO.
DEFINE VARIABLE iEmpty         AS INTEGER   NO-UNDO.
DEFINE VARIABLE iRow           AS INTEGER   NO-UNDO.
DEFINE VARIABLE iCol           AS INTEGER   NO-UNDO.
DEFINE VARIABLE iMaxX          AS INTEGER   NO-UNDO.
DEFINE VARIABLE iMaxY          AS INTEGER   NO-UNDO.
DEFINE VARIABLE iDistance      AS INT64     NO-UNDO.

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
   iMaxX = LENGTH (ttLine.cInputLine).
   
   DO iX = 1 TO LENGTH (ttLine.cInputLine):
      iNewIDGrid = iNewIDGrid + 1.
      CREATE ttGrid.
      ASSIGN 
         ttGrid.IDGrid     = iNewIDGrid
         ttGrid.iX         = iX
         ttGrid.iY         = iLine
         ttGrid.Symbol     = SUBSTRING (ttLine.cInputLine, iX, 1)
      .
      ASSIGN 
         ttGrid.OriginalX = ttGrid.iX
         ttGrid.OriginalY = ttGrid.iY
      .
   END.
   iMaxY = iLine.
         
END. /* ReadBlock: */

IF lvlShow THEN DO:
   RUN sy\win\wbrowsett.w
      (INPUT TEMP-TABLE ttGrid:HANDLE). 
END.

IF lvlOutput THEN DO:
   cOutputFile = SUBSTITUTE ("output\11_start.txt").
   OUTPUT TO VALUE (cOutputFile).
   DO iY = 1 TO iMaxY:
      DO iX = 1 TO iMaxX:
         FIND  ttGrid
         WHERE ttGrid.iY = iY
         AND   ttGrid.iX = iX NO-ERROR.
         IF AVAILABLE ttGrid THEN
            PUT UNFORMATTED 
               ttGrid.Symbol.
         ELSE 
            PUT UNFORMATTED 
               "-".
      END.
      PUT UNFORMATTED SKIP. 
   END.         
   OUTPUT CLOSE.
END.

IF lPart[1] THEN DO:
   /* Process Part One */
   iSolution = 0.

   /* Calcolate Solution for Part 1 */
   ASSIGN 
      cEmptyRows = ""
      cEmptyCols = ""
   .
   FOR EACH ttGrid
   BREAK
   BY ttGrid.iY
   BY ttGrid.iX:
      IF FIRST-OF (ttGrid.iY) THEN DO:
         lEmpty = TRUE.
      END.
      IF ttGrid.Symbol NE "." THEN 
         lEmpty = FALSE.
      IF LAST-OF (ttGrid.iY) THEN DO:
         IF lEmpty THEN DO:
            cEmptyRows = SUBSTITUTE ("&1&2&3",
                                     ttGrid.iY,
                                     (IF cEmptyRows NE "" THEN "," ELSE ""),
                                     cEmptyRows).
         END.
      END.
   END.

   FOR EACH ttGrid
   BREAK
   BY ttGrid.iX
   BY ttGrid.iY:
      IF FIRST-OF (ttGrid.iX) THEN DO:
         lEmpty = TRUE.
      END.
      IF ttGrid.Symbol NE "." THEN 
         lEmpty = FALSE.
      IF LAST-OF (ttGrid.iX) THEN DO:
         IF lEmpty THEN DO:
            cEmptyCols = SUBSTITUTE ("&1&2&3",
                                     ttGrid.iX,
                                     (IF cEmptyCols NE "" THEN "," ELSE ""),
                                     cEmptyCols).
         END.
      END.
   END.
      
   DO iEmpty = 1 TO NUM-ENTRIES (cEmptyRows):
      iRow = INTEGER (ENTRY (iEmpty, cEmptyRows)).
      FOR EACH ttGrid
      WHERE ttGrid.iY GT iRow
      BY ttGrid.iY DESCENDING 
      BY ttGrid.iX:
         ASSIGN 
            ttGrid.iY = ttGrid.iY + 1
         .
      END.
   END.
   
   DO iEmpty = 1 TO NUM-ENTRIES (cEmptyCols):
      iCol = INTEGER (ENTRY (iEmpty, cEmptyCols)).
      FOR EACH ttGrid
      WHERE ttGrid.iX GT iCol
      BY ttGrid.iX DESCENDING 
      BY ttGrid.iY:
         ASSIGN 
            ttGrid.iX = ttGrid.iX + 1
         .
      END.   
   END.
   
   IF lvlShow THEN DO:
      RUN sy\win\wbrowsett.w
         (INPUT TEMP-TABLE ttGrid:HANDLE).
   END.
   
   IF lvlOutput THEN DO:
      cOutputFile = SUBSTITUTE ("output\11_expanded.txt").
      OUTPUT TO VALUE (cOutputFile).
      DO iY = 1 TO iMaxY:
         DO iX = 1 TO iMaxX:
            FIND  ttGrid
            WHERE ttGrid.iY = iY
            AND   ttGrid.iX = iX NO-ERROR.
            IF AVAILABLE ttGrid THEN
               PUT UNFORMATTED 
                  ttGrid.Symbol.
            ELSE 
               PUT UNFORMATTED 
                  "-".
         END.
         PUT UNFORMATTED SKIP. 
      END.         
      OUTPUT CLOSE.
   END.
   
   FOR EACH ttGrid
   WHERE ttGrid.Symbol EQ "#":
      /* Determine Galaxies */
      iNewIDGalaxy = iNewIDGalaxy + 1.
      CREATE ttGalaxy.
      ASSIGN 
         ttGalaxy.IDGalaxy = iNewIDGalaxy
         ttGalaxy.iX       = ttGrid.iX
         ttGalaxy.iY       = ttGrid.iY
      .
   END.
   
   FOR EACH ttGalaxy:
      FOR EACH ttOtherGalaxy
      WHERE ttOtherGalaxy.IDGalaxy NE ttGalaxy.IDGalaxy:
         iDistance = ABSOLUTE (ttGalaxy.iX - ttOtherGalaxy.iX) + ABSOLUTE (ttGalaxy.iY - ttOtherGalaxy.iY).
         IF iSolution EQ 0 THEN 
            iSolution = iDistance.
         ELSE 
            iSolution = iSolution + iDistance.
      END.
   END.
   
   iSolution = iSolution / 2.
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
   IF lPart[1] EQ TRUE THEN DO:
      /* Reset X - Y to Original values */
      FOR EACH ttGrid
      BY ttGrid.IDGrid:
         ASSIGN 
            ttGrid.iX = ttGrid.OriginalX
            ttGrid.iY = ttGrid.OriginalY
         .
      END.
   END. /* Reset X - Y to Original values */
   
   ASSIGN 
      cEmptyRows = ""
      cEmptyCols = ""
   .
   FOR EACH ttGrid
   BREAK
   BY ttGrid.iY
   BY ttGrid.iX:
      IF FIRST-OF (ttGrid.iY) THEN DO:
         lEmpty = TRUE.
      END.
      IF ttGrid.Symbol NE "." THEN 
         lEmpty = FALSE.
      IF LAST-OF (ttGrid.iY) THEN DO:
         IF lEmpty THEN DO:
            cEmptyRows = SUBSTITUTE ("&1&2&3",
                                     ttGrid.iY,
                                     (IF cEmptyRows NE "" THEN "," ELSE ""),
                                     cEmptyRows).
         END.
      END.
   END.

   FOR EACH ttGrid
   BREAK
   BY ttGrid.iX
   BY ttGrid.iY:
      IF FIRST-OF (ttGrid.iX) THEN DO:
         lEmpty = TRUE.
      END.
      IF ttGrid.Symbol NE "." THEN 
         lEmpty = FALSE.
      IF LAST-OF (ttGrid.iX) THEN DO:
         IF lEmpty THEN DO:
            cEmptyCols = SUBSTITUTE ("&1&2&3",
                                     ttGrid.iX,
                                     (IF cEmptyCols NE "" THEN "," ELSE ""),
                                     cEmptyCols).
         END.
      END.
   END.
      
   DO iEmpty = 1 TO NUM-ENTRIES (cEmptyRows):
      iRow = INTEGER (ENTRY (iEmpty, cEmptyRows)).
      FOR EACH ttGrid
      WHERE ttGrid.iY GT iRow
      BY ttGrid.iY DESCENDING 
      BY ttGrid.iX:
         ASSIGN 
            ttGrid.iY = ttGrid.iY + 1000000 - 1
         .
      END.
   END.
   
   DO iEmpty = 1 TO NUM-ENTRIES (cEmptyCols):
      iCol = INTEGER (ENTRY (iEmpty, cEmptyCols)).
      FOR EACH ttGrid
      WHERE ttGrid.iX GT iCol
      BY ttGrid.iX DESCENDING 
      BY ttGrid.iY:
         ASSIGN 
            ttGrid.iX = ttGrid.iX + 1000000 - 1
         .
      END.   
   END.

   FOR EACH ttGrid
   WHERE ttGrid.Symbol EQ "#":
      /* Determine Galaxies */
      iNewIDGalaxy = iNewIDGalaxy + 1.
      CREATE ttGalaxy.
      ASSIGN 
         ttGalaxy.IDGalaxy = iNewIDGalaxy
         ttGalaxy.iX       = ttGrid.iX
         ttGalaxy.iY       = ttGrid.iY
      .
   END. /* Determine Galaxies */
   
   FOR EACH ttGalaxy:
      FOR EACH ttOtherGalaxy
      WHERE ttOtherGalaxy.IDGalaxy NE ttGalaxy.IDGalaxy:
         iDistance = ABSOLUTE (ttGalaxy.iX - ttOtherGalaxy.iX) + ABSOLUTE (ttGalaxy.iY - ttOtherGalaxy.iY).
         IF iSolution EQ 0 THEN 
            iSolution = iDistance.
         ELSE 
            iSolution = iSolution + iDistance.
      END.
   END.
   
   iSolution = iSolution / 2.
   
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




