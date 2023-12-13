
/*------------------------------------------------------------------------
    File        : day13.p
    Purpose     : Solve Day 13 for Advent of Code 2023

    Syntax      :

    Description : Solution for Advent of Code 2023 - Day 13

    Author(s)   : Wim van der Ham (WITS)
    Created     : Wed Dec 13 09:18:05 CET 2023
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
DEFINE VARIABLE iDay         AS INTEGER   NO-UNDO INITIAL 13.
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

DEFINE TEMP-TABLE ttPattern
   FIELD IDPattern   AS INTEGER 
   FIELD StartLine   AS INTEGER 
   FIELD MaxColumnNr AS INTEGER 
   FIELD MaxRowNr    AS INTEGER 
   FIELD LeftColumns AS INTEGER 
   FIELD TopRows     AS INTEGER 
   /* Extra fields for Part Two */
   FIELD UseMirrorAt AS CHARACTER 
INDEX indID IS UNIQUE IDPattern.
DEFINE VARIABLE iNewIDPattern AS INTEGER NO-UNDO.

DEFINE TEMP-TABLE ttColumn
   FIELD IDColumn             AS INTEGER 
   FIELD IDPattern            AS INTEGER 
   FIELD ColumnNr             AS INTEGER 
   FIELD ColumnString         AS CHARACTER FORMAT "X(20)"
   FIELD ColumnNumber         AS INTEGER 
   FIELD OriginalColumnString AS CHARACTER 
INDEX indID  IS UNIQUE IDColumn
INDEX indIDs IS UNIQUE PRIMARY IDPattern ColumnNr.
DEFINE VARIABLE iNewIDColumn AS INTEGER NO-UNDO.
DEFINE BUFFER ttNextColumn  FOR ttColumn.
DEFINE BUFFER ttLeftColumn  FOR ttColumn.
DEFINE BUFFER ttRightColumn FOR ttColumn.

DEFINE TEMP-TABLE ttRow
   FIELD IDRow             AS INTEGER 
   FIELD IDPattern         AS INTEGER 
   FIELD RowNr             AS INTEGER 
   FIELD RowString         AS CHARACTER FORMAT "X(20)"
   FIELD RowNumber         AS INTEGER 
   FIELD OriginalRowString AS CHARACTER 
INDEX indID  IS UNIQUE IDRow
INDEX indIDs IS UNIQUE PRIMARY IDPattern RowNr.
DEFINE VARIABLE iNewIDRow AS INTEGER NO-UNDO.
DEFINE BUFFER ttNextRow   FOR ttRow.  
DEFINE BUFFER ttTopRow    FOR ttRow.
DEFINE BUFFER ttBottomRow FOR ttRow.
   
DEFINE VARIABLE iLeftColumnNr  AS INTEGER NO-UNDO.
DEFINE VARIABLE iRightColumnNr AS INTEGER NO-UNDO.
DEFINE VARIABLE lMirrorOK      AS LOGICAL NO-UNDO.
DEFINE VARIABLE iTopRowNr      AS INTEGER NO-UNDO.
DEFINE VARIABLE iBottomRowNr   AS INTEGER NO-UNDO.
DEFINE VARIABLE iCenter        AS INTEGER NO-UNDO.
   
/* Extra temp-tables for Part Two */
DEFINE TEMP-TABLE ttDifferences
   FIELD IDPattern   AS INTEGER 
   FIELD lColumn     AS LOGICAL FORMAT "Column/Row"
   FIELD Nr1         AS INTEGER 
   FIELD Nr2         AS INTEGER
   FIELD MirrorAt    AS CHARACTER
   FIELD Differences AS INTEGER 
INDEX indID IS UNIQUE IDPattern lColumn Nr1 Nr2.
    
/* ********************  Preprocessor Definitions  ******************** */

/* ************************  Function Prototypes ********************** */

FUNCTION getBinary RETURNS CHARACTER 
   ( INPUT ipiValue  AS INT64,
     INPUT ipiLength AS INTEGER ) FORWARD.

FUNCTION getDifferences RETURNS INTEGER 
   ( INPUT ipcString1 AS CHARACTER,
     INPUT ipcString2 AS CHARACTER ) FORWARD.

FUNCTION getINT64 RETURNS INT64 
   ( INPUT ipcBinary AS CHARACTER ) FORWARD.

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


iNewIDPattern = iNewIDPattern + 1.
CREATE ttPattern.
ASSIGN 
   ttPattern.IDPattern = iNewIDPattern
   ttPattern.StartLine = 1
.

/* Read Input into Temp-table */
ReadBlock:
DO iLine = 1 TO NUM-ENTRIES (lcInput, "~n"):
   cLine = TRIM (ENTRY (iLine, lcInput, "~n")).

   IF cLine EQ "" THEN DO:
      /* End of Pattern */
      IF iLine NE NUM-ENTRIES (lcInput, "~n") THEN DO:
         iNewIDPattern = iNewIDPattern + 1.
         CREATE ttPattern.
         ASSIGN 
            ttPattern.IDPattern   = iNewIDPattern
            ttPattern.StartLine   = iLine + 1
         .
      END.
      NEXT.
   END.
      
   CREATE ttLine.
   ASSIGN 
      ttLine.IDLine     = iLine
      ttLine.cInputLine = cLine
   .

   IF ttPattern.MaxColumnNr EQ 0 THEN 
      ttPattern.MaxColumnNr = LENGTH (ttLine.cInputLine).
      
   ttPattern.MaxRowNr = iLine - ttPattern.StartLine + 1.
      
   iNewIDRow = iNewIDRow + 1.
   CREATE ttRow.
   ASSIGN
      ttRow.IDRow             = iNewIDRow 
      ttRow.IDPattern         = ttPattern.IDPattern
      ttRow.RowNr             = iLine - ttPattern.StartLine + 1
      ttRow.RowString         = ttLine.cInputLine
      ttRow.OriginalRowString = ttRow.RowString
   .
   
   DO iChar = 1 TO LENGTH (ttLine.cInputLine):
      cChar = SUBSTRING (ttLine.cInputLine, iChar, 1).
      IF iLine EQ ttPattern.StartLine THEN DO:
         iNewIDColumn = iNewIDColumn + 1.
         CREATE ttColumn.
         ASSIGN 
            ttColumn.IDColumn     = iNewIDColumn
            ttColumn.IDPattern    = ttPattern.IDPattern
            ttColumn.ColumnNr     = iChar
         .
      END.
      ELSE DO:
         FIND  ttColumn
         WHERE ttColumn.IDPattern = ttPattern.IDPattern
         AND   ttColumn.ColumnNr  = iChar.
      END.
      ASSIGN 
         ttColumn.ColumnString         = ttColumn.ColumnString + cChar
         ttColumn.OriginalColumnString = ttColumn.ColumnString
      .
   END.
   IF iLine EQ NUM-ENTRIES (lcInput, "~n") THEN 
      ttPattern.MaxRowNr = iLine - ttPattern.StartLine + 1.
      
   FIND FIRST ttRow OF ttPattern NO-ERROR.
   IF NOT AVAILABLE ttRow THEN DO:
      DELETE ttPattern.
   END.   
END. /* ReadBlock: */

IF lvlShow THEN DO:
   RUN sy\win\wbrowsett.w
      (INPUT TEMP-TABLE ttLine:HANDLE). 
   RUN sy\win\wbrowsett.w
      (INPUT TEMP-TABLE ttPattern:HANDLE).
   RUN sy\win\wbrowsett.w
      (INPUT TEMP-TABLE ttRow:HANDLE).
   RUN sy\win\wbrowsett.w
      (INPUT TEMP-TABLE ttColumn:HANDLE).
END.

IF lPart[1] THEN DO:
   /* Process Part One */
   iSolution = 0.

   /* Calcolate Solution for Part 1 */
   FOR EACH ttPattern:
      /* For All Patterns */
      IF lvlDebug THEN DO:
         MESSAGE "Pattern:" ttPattern.IDPattern
         SUBSTITUTE ("Pattern: &1 &2 (&3-&4)", 
                     ttPattern.IDPattern, 
                     ttPattern.StartLine, 
                     ttPattern.MaxColumnNr, 
                     ttPattern.MaxRowNr)
         VIEW-AS ALERT-BOX.
      END.
      FOR EACH ttColumn
      WHERE ttColumn.IDPattern EQ ttPattern.IDPattern
      AND   ttColumn.ColumnNr  LT ttPattern.MaxColumnNr,
      FIRST ttNextColumn
      WHERE ttNextColumn.IDPattern    EQ ttColumn.IDPattern
      AND   ttNextColumn.ColumnNr     EQ ttColumn.ColumnNr + 1
      AND   ttNextColumn.ColumnString EQ ttColumn.ColumnString:
         /* Found two consecutive identical Columns, check their neighbours */
         ASSIGN 
            iLeftColumnNr  = ttColumn.ColumnNr - 1
            iRightColumnNr = ttNextColumn.ColumnNr + 1
            lMirrorOK      = TRUE 
         .
         CheckColumns:
         DO WHILE iLeftColumnNr GE 1
         AND iRightColumnNr LE ttPattern.MaxColumnNr:
            /* Check all neighbour columns */
            FIND  ttLeftColumn
            WHERE ttLeftColumn.IDPattern EQ ttColumn.IDPattern
            AND   ttLeftColumn.ColumnNr  EQ iLeftColumnNr.
            FIND  ttRightColumn
            WHERE ttRightColumn.IDPattern EQ ttNextColumn.IDPattern
            AND   ttRightColumn.ColumnNr  EQ iRightColumnNr.
            IF ttLeftColumn.ColumnString NE ttRightColumn.ColumnString THEN DO:
               lMirrorOK = FALSE.
               LEAVE CheckColumns.
            END.
            ASSIGN 
               iLeftColumnNr  = iLeftColumnNr  - 1
               iRightColumnNr = iRightColumnNr + 1
            .
         END. /* Check all neighbour columns */
         IF lMirrorOK = TRUE THEN DO: 
            ttPattern.LeftColumns = ttColumn.ColumnNr.
            iSolution = iSolution + ttPattern.LeftColumns.
         END.
      END. /* Found two consecutive identical Columns, check their neighbours */
      
      /* Check Mirrored Rows */
      FOR EACH ttRow
      WHERE ttRow.IDPattern EQ ttPattern.IDPattern
      AND   ttRow.RowNr     LT ttPattern.MaxRowNr,
      FIRST ttNextRow
      WHERE ttNextRow.IDPattern EQ ttRow.IDPattern
      AND   ttNextRow.RowNr     EQ ttRow.RowNr + 1
      AND   ttNextRow.RowString EQ ttRow.RowString:
         /* Found two consecutive identical Rows, check their neighbours */
         ASSIGN 
            iTopRowNr    = ttRow.RowNr     - 1
            iBottomRowNr = ttNextRow.RowNr + 1
            lMirrorOK    = TRUE 
         .
         CheckRows:
         DO WHILE iTopRowNr GE 1
         AND iBottomRowNr   LE ttPattern.MaxRowNr:
            /* Check all neighbour Rows */
            FIND  ttTopRow
            WHERE ttTopRow.IDPattern EQ ttRow.IDPattern
            AND   ttTopRow.RowNr     EQ iTopRowNr.
            FIND  ttBottomRow
            WHERE ttBottomRow.IDPattern EQ ttNextRow.IDPattern
            AND   ttBottomRow.RowNr     EQ iBottomRowNr.
            IF ttTopRow.RowString NE ttBottomRow.RowString THEN DO:
               lMirrorOK = FALSE.
               LEAVE CheckRows.
            END.
            ASSIGN 
               iTopRowNr    = iTopRowNr  - 1
               iBottomRowNr = iBottomRowNr + 1
            .
         END. /* Check all neighbour Rows */
         IF lMirrorOK = TRUE THEN DO:
            ASSIGN 
               ttPattern.TopRows = ttRow.RowNr
               iSolution = iSolution + 100 * ttPattern.TopRows
            .
         END.            
      END. /* Check Mirrored Rows */
      
   END. /* For All Patterns */
         
   OUTPUT TO "clipboard".
   PUT UNFORMATTED iSolution SKIP.
   OUTPUT CLOSE.
   MESSAGE 
      SUBSTITUTE ("Solution: &1.", iSolution) SKIP (1)
      SUBSTITUTE ("Found solution in &1 msecs.", ETIME)
   VIEW-AS ALERT-BOX TITLE " 2023 - Day 13 - Part One".
   
   IF lvlShow THEN DO:
      RUN sy\win\wbrowsett.w
         (INPUT TEMP-TABLE ttPattern:HANDLE).
   END.
            
END. /* Process Part One */

IF lPart[2] THEN DO:
   /* Process Part Two */
   iSolution = 0.
   
   /* Calcolate Solution for Part 2 */
   FOR EACH ttPattern:
      FOR EACH ttColumn OF ttPattern,
      EACH  ttNextColumn OF ttPattern
      WHERE ttNextColumn.ColumnNr GT ttColumn.ColumnNr:
         IF getDifferences(ttColumn.ColumnString, ttNextColumn.ColumnString) LE 1 THEN DO:
            iCenter = INTEGER (TRUNCATE ((ttNextColumn.ColumnNr - ttColumn.ColumnNr) / 2, 0)) + ttColumn.ColumnNr.
            CREATE ttDifferences.
            ASSIGN 
               ttDifferences.IDPattern   = ttPattern.IDPattern
               ttDifferences.lColumn     = TRUE 
               ttDifferences.Nr1         = ttColumn.ColumnNr
               ttDifferences.Nr2         = ttNextColumn.ColumnNr
               ttDifferences.MirrorAt    = SUBSTITUTE ("&1&2", iCenter, iCenter + 1)
               ttDifferences.Differences = getDifferences(ttColumn.ColumnString, ttNextColumn.ColumnString)
            .
         END.
      END.
      FOR EACH ttRow OF ttPattern,
      EACH  ttNextRow OF ttPattern
      WHERE ttNextRow.RowNr GT ttRow.RowNr:
         IF getDifferences(ttRow.RowString, ttNextRow.RowString) LE 1 THEN DO:
            iCenter = INTEGER (TRUNCATE ((ttNextRow.RowNr - ttRow.RowNr) / 2, 0)) + ttRow.RowNr.
            CREATE ttDifferences.
            ASSIGN 
               ttDifferences.IDPattern   = ttPattern.IDPattern
               ttDifferences.lColumn     = FALSE  
               ttDifferences.Nr1         = ttRow.RowNr
               ttDifferences.Nr2         = ttNextRow.RowNr
               ttDifferences.MirrorAt    = SUBSTITUTE ("&1&2", iCenter, iCenter + 1)
               ttDifferences.Differences = getDifferences(ttRow.RowString, ttNextRow.RowString)
            .
         END.
      END.
      FOR EACH ttDifferences OF ttPattern
      BREAK 
      BY ttDifferences.IDPattern
      BY ttDifferences.lColumn
      BY ttDifferences.MirrorAt:
         IF ttDifferences.Differences EQ 1 THEN DO:
            ttPattern.UseMirrorAt = ttDifferences.MirrorAt.
            IF ttDifferences.lColumn THEN DO:
               FIND  ttColumn OF ttPattern
               WHERE ttColumn.ColumnNr EQ ttDifferences.Nr1.
               FIND  ttNextColumn OF ttPattern
               WHERE ttNextColumn.ColumnNr EQ ttDifferences.Nr2.
               ASSIGN 
                  ttColumn.ColumnString = ttNextColumn.ColumnString
               .
            END.
            ELSE DO:
               FIND  ttRow OF ttPattern
               WHERE ttRow.RowNr EQ ttDifferences.Nr1.
               FIND  ttNextRow OF ttPattern
               WHERE ttNextRow.RowNr     EQ ttDifferences.Nr2 NO-ERROR.
               IF NOT AVAILABLE ttNextRow THEN DO:
                  MESSAGE ttDifferences.IDPattern ttPattern.IDPattern ttDifferences.Nr2
                  VIEW-AS ALERT-BOX.
                  RUN sy\win\wbrowsett.w
                     (INPUT TEMP-TABLE ttNextRow:HANDLE).
               END.
               ELSE DO:
                  ASSIGN 
                     ttRow.RowString = ttNextRow.RowString
                  .
               END.
            END.
         END.               
      END.            
   END.       

   IF lvlShow THEN DO:
      RUN sy\win\wbrowsett.w
         (INPUT TEMP-TABLE ttDifferences:HANDLE).
      RUN sy\win\wbrowsett.w
         (INPUT TEMP-TABLE ttPattern:HANDLE).
   END.
   
   FOR EACH ttPattern:
      /* For All Patterns */
      IF lvlDebug THEN DO:
         MESSAGE "Pattern:" ttPattern.IDPattern
         SUBSTITUTE ("Pattern: &1 &2 (&3-&4)", 
                     ttPattern.IDPattern, 
                     ttPattern.StartLine, 
                     ttPattern.MaxColumnNr, 
                     ttPattern.MaxRowNr)
         VIEW-AS ALERT-BOX.
      END.
      FOR EACH ttColumn
      WHERE ttColumn.IDPattern EQ ttPattern.IDPattern
      AND   ttColumn.ColumnNr  LT ttPattern.MaxColumnNr,
      FIRST ttNextColumn
      WHERE ttNextColumn.IDPattern    EQ ttColumn.IDPattern
      AND   ttNextColumn.ColumnNr     EQ ttColumn.ColumnNr + 1
      AND   ttNextColumn.ColumnString EQ ttColumn.ColumnString:
         /* Found two consecutive almost identical Columns, check their neighbours */
         ASSIGN 
            iLeftColumnNr  = ttColumn.ColumnNr - 1
            iRightColumnNr = ttNextColumn.ColumnNr + 1
            lMirrorOK      = TRUE 
         .
         CheckColumns:
         DO WHILE iLeftColumnNr GE 1
         AND iRightColumnNr     LE ttPattern.MaxColumnNr:
            /* Check all neighbour columns */
            FIND  ttLeftColumn
            WHERE ttLeftColumn.IDPattern EQ ttColumn.IDPattern
            AND   ttLeftColumn.ColumnNr  EQ iLeftColumnNr.
            FIND  ttRightColumn
            WHERE ttRightColumn.IDPattern EQ ttNextColumn.IDPattern
            AND   ttRightColumn.ColumnNr  EQ iRightColumnNr.
            
            IF ttLeftColumn.ColumnString NE ttRightColumn.ColumnString THEN DO:
               lMirrorOK = FALSE.
               LEAVE CheckColumns.
            END.
            ASSIGN 
               iLeftColumnNr  = iLeftColumnNr  - 1
               iRightColumnNr = iRightColumnNr + 1
            .
         END. /* Check all neighbour columns */
         IF lMirrorOK = TRUE THEN DO:
            MESSAGE SUBSTITUTE ("Pattern: &1, add column &2", ttColumn.IDPattern, ttColumn.ColumnNr)
            VIEW-AS ALERT-BOX. 
            ttPattern.LeftColumns = ttColumn.ColumnNr.
            iSolution = iSolution + ttPattern.LeftColumns.
         END.
      END. /* Found two consecutive identical Columns, check their neighbours */
      
      /* Check Mirrored Rows */
      FOR EACH ttRow
      WHERE ttRow.IDPattern EQ ttPattern.IDPattern
      AND   ttRow.RowNr     LT ttPattern.MaxRowNr,
      FIRST ttNextRow
      WHERE ttNextRow.IDPattern EQ ttRow.IDPattern
      AND   ttNextRow.RowNr     EQ ttRow.RowNr + 1
      AND   ttNextRow.RowString EQ ttRow.RowString:
         /* Found two consecutive almost identical Rows, check their neighbours */
         ASSIGN 
            iTopRowNr    = ttRow.RowNr     - 1
            iBottomRowNr = ttNextRow.RowNr + 1
            lMirrorOK    = TRUE 
         .
         CheckRows:
         DO WHILE iTopRowNr GE 1
         AND iBottomRowNr   LE ttPattern.MaxRowNr:
            /* Check all neighbour Rows */
            FIND  ttTopRow
            WHERE ttTopRow.IDPattern EQ ttRow.IDPattern
            AND   ttTopRow.RowNr     EQ iTopRowNr.
            FIND  ttBottomRow
            WHERE ttBottomRow.IDPattern EQ ttNextRow.IDPattern
            AND   ttBottomRow.RowNr     EQ iBottomRowNr.
            IF ttBottomRow.RowString NE ttTopRow.RowString THEN DO:
               lMirrorOK = FALSE.
               LEAVE CheckRows.
            END.
            ASSIGN 
               iTopRowNr    = iTopRowNr  - 1
               iBottomRowNr = iBottomRowNr + 1
            .
         END. /* Check all neighbour Rows */
         IF lMirrorOK = TRUE THEN DO:
            ASSIGN 
               ttPattern.TopRows = ttRow.RowNr
               iSolution = iSolution + 100 * ttPattern.TopRows
            .
            MESSAGE SUBSTITUTE ("Pattern: &1, add row &2", ttRow.IDPattern, 100 * ttRow.RowNr)
            VIEW-AS ALERT-BOX. 
         END.            
      END. /* Check Mirrored Rows */
      
   END. /* For All Patterns */
   
   OUTPUT TO "clipboard".
   PUT UNFORMATTED iSolution SKIP.
   OUTPUT CLOSE.

   MESSAGE 
      SUBSTITUTE ("Solution: &1.", iSolution) SKIP (1)
      SUBSTITUTE ("Found solution in &1 msecs.", ETIME)
   VIEW-AS ALERT-BOX TITLE " 2023 - Day 13 - Part Two".
   
   IF lvlShow THEN DO:
      RUN sy\win\wbrowsett.w
         (INPUT TEMP-TABLE ttPattern:HANDLE).
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

/* ************************  Function Implementations ***************** */

/* Helper functions for conversion binary string to integer and back */

FUNCTION getDifferences RETURNS INTEGER 
   ( INPUT ipcString1 AS CHARACTER,
     INPUT ipcString2 AS CHARACTER ):
/*------------------------------------------------------------------------------
 Purpose: Return the number of different characters between two strings
 Notes:
------------------------------------------------------------------------------*/   
DEFINE VARIABLE iChar AS INTEGER NO-UNDO.
DEFINE VARIABLE cChar AS CHARACTER NO-UNDO.
DEFINE VARIABLE iDiff AS INTEGER NO-UNDO.
   
   DO iChar = 1 TO LENGTH (ipcString1):
      cChar = SUBSTRING (ipcString1, iChar, 1).
      IF SUBSTRING (ipcString2, iChar, 1) NE cChar THEN 
         iDiff = iDiff + 1.
   END.
   
   RETURN iDiff.                
      
END FUNCTION.

FUNCTION getINT64 RETURNS INT64 
   ( INPUT ipcBinary AS CHARACTER   ):
/*------------------------------------------------------------------------------
 Purpose: Returns the INT64 value of a binary string
 Notes:   0000 = 0
          1111 = 15
------------------------------------------------------------------------------*/   
DEFINE VARIABLE iBase   AS INTEGER NO-UNDO.
DEFINE VARIABLE iBit    AS INTEGER NO-UNDO.
DEFINE VARIABLE cBit    AS CHARACTER NO-UNDO.
DEFINE VARIABLE iResult AS INT64   NO-UNDO.

   iBase = 1.
   
   DO iBit = LENGTH (ipcBinary) TO 1 BY -1:
      cBit = SUBSTRING (ipcBinary, iBit, 1).
      IF cBit EQ "1" THEN 
         iResult = iResult + iBase.
      iBase = iBase * 2.
   END.
   
   RETURN iResult.
      
END FUNCTION.

FUNCTION getBinary RETURNS CHARACTER 
   ( INPUT ipiValue  AS INT64,
     INPUT ipiLength AS INTEGER ):
/*------------------------------------------------------------------------------
 Purpose: Returns Bit string for an integer value per una certa lunghezza
 Notes:
------------------------------------------------------------------------------*/
DEFINE VARIABLE iBaseValue AS INT64     NO-UNDO.
DEFINE VARIABLE cBinary    AS CHARACTER NO-UNDO.

   iBaseValue = 1.
   DO WHILE iBaseValue LT ipiValue:
      iBaseValue = iBaseValue * 2.
   END.
   IF iBaseValue GT ipiValue THEN
      iBaseValue = INTEGER (TRUNCATE(iBaseValue / 2, 0)).
   
   DO WHILE iBaseValue GE 1:
      
      IF INTEGER (TRUNCATE (ipiValue / iBaseValue, 0)) EQ 0 THEN 
         cBinary = SUBSTITUTE ("&10", cBinary).
      ELSE DO:
         cBinary  = SUBSTITUTE ("&11", cBinary).
         ipiValue = ipiValue - iBaseValue.  
      END.
      
      iBaseValue = INTEGER (TRUNCATE(iBaseValue / 2, 0)).
   END.

   IF LENGTH (cBinary) LT ipiLength THEN 
      cBinary = SUBSTITUTE ("&1&2",
                            FILL ("0", ipiLength - LENGTH (cBinary)),
                            cBinary).
                               
   RETURN cBinary.
   
END FUNCTION.

