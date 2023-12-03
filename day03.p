
/*------------------------------------------------------------------------
    File        : day03.p
    Purpose     : Solve day 3 of Advent of Code 2023

    Syntax      :

    Description : Solution to Advent of Code 2023 - Day 03

    Author(s)   : Wim van der Ham (WITS)
    Created     : Sun Dec 03 10:06:11 CET 2023
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
DEFINE VARIABLE iDay         AS INTEGER   NO-UNDO INITIAL 3.
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

DEFINE TEMP-TABLE ttNumber
   FIELD IDNumber AS INTEGER 
   FIELD iFromX   AS INTEGER 
   FIELD iFromY   AS INTEGER
   FIELD iToX     AS INTEGER 
   FIELD iToY     AS INTEGER 
   FIELD iNumber  AS INTEGER 
   FIELD lPart    AS LOGICAL 
INDEX indID IS UNIQUE IDNumber
INDEX indXY IS PRIMARY iFromX iFromY iToX iToY.

DEFINE TEMP-TABLE ttSymbol
   FIELD IDSymbol  AS INTEGER
   FIELD iX        AS INTEGER 
   FIELD iY        AS INTEGER  
   FIELD cSymbol   AS CHARACTER
   FIELD Numbers   AS INTEGER  
   FIELD GearRatio AS INTEGER INITIAL 1 
INDEX indID IS UNIQUE IDSymbol.

DEFINE VARIABLE iX           AS INTEGER   NO-UNDO.
DEFINE VARIABLE iY           AS INTEGER   NO-UNDO.
DEFINE VARIABLE lNumber      AS LOGICAL   NO-UNDO.
DEFINE VARIABLE cNumber      AS CHARACTER NO-UNDO.
DEFINE VARIABLE iNewIDNumber AS INTEGER   NO-UNDO.
DEFINE VARIABLE iNewIDSymbol AS INTEGER   NO-UNDO.
DEFINE VARIABLE lIsDigit     AS LOGICAL   NO-UNDO.

/* ********************  Preprocessor Definitions  ******************** */

{AOC_session.i}

/* ************************  Function Prototypes ********************** */

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
   cInputfile = SUBSTITUTE ("C:\OpenEdge\WRK\AOC2023\input\&1.txt", STRING (iDay, "99"))
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
   lcInput = "467..114..~n...*......~n..35..633.~n......#...~n617*......~n.....+.58.~n..592.....~n......755.~n...$.*....~n.664.598..".
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

   lNumber = FALSE.
   
   DO iChar = 1 TO LENGTH (ttLine.cInputLine):
      cChar = SUBSTRING (ttLine.cInputLine, iChar, 1).
      
      ASSIGN 
         iX = iChar
         iY = ttLine.IDLine
      .
      lIsDigit = INDEX ("0123456789", cChar) GT 0.
      IF lIsDigit THEN DO:
         /* Found a number */
         IF lNumber = FALSE THEN DO:
            /* Found a new number */
            ASSIGN 
               iNewIDNumber = iNewIDNumber + 1
            .
            CREATE ttNumber.
            ASSIGN 
               ttNumber.IDNumber = iNewIDNumber
               ttNumber.iFromX   = iX
               ttNumber.iFromY   = iY
            .
            lNumber = TRUE.
         END.
         /* Adjust To position */
         ASSIGN 
            ttNumber.iToX = iX
            ttNumber.iToY = iY
         .
         cNumber = cNumber + cChar.
      END.
      IF lIsDigit EQ FALSE
      OR iChar EQ LENGTH (ttLine.cInputLine)
      THEN DO: 
         IF lNumber EQ TRUE THEN DO:
            ASSIGN 
               ttNumber.iToX    = iX - 1 WHEN lIsDigit EQ FALSE 
               ttNumber.iToX    = iX     WHEN lIsDigit EQ TRUE 
               ttNumber.iToY    = iY
               ttNumber.iNumber = INTEGER (cNumber)
               ttNumber.lPart   = FALSE 
            .
            lNumber = FALSE.
            cNumber = "".
         END.
         IF  lIsDigit EQ FALSE 
         AND cChar NE "." THEN DO:
            /* Found a Symbol */
            iNewIDSymbol = iNewIDSymbol + 1.
            CREATE ttSymbol.
            ASSIGN 
               ttSymbol.IDSymbol = iNewIDSymbol
               ttSymbol.iX       = iX
               ttSymbol.iY       = iY
               ttSymbol.cSymbol  = cChar
            .
         END.
      END.
   END.
END. /* ReadBlock: */

IF lPart[1] THEN DO:
   /* Process Part One */
   iSolution = 0.
   
   FOR EACH ttNumber:
      FIND FIRST ttSymbol
      WHERE ttSymbol.iX GE ttNumber.iFromX - 1
      AND   ttSymbol.iX LE ttNumber.iToX   + 1
      AND   ttSymbol.iY GE ttNumber.iFromY - 1
      AND   ttSymbol.iY LE ttNumber.iToY   + 1 NO-ERROR.
      IF AVAILABLE ttSymbol THEN DO:
         ASSIGN 
            ttNumber.lPart = TRUE 
            iSolution = iSolution + ttNumber.iNumber
         .
      END.
   END.
         
   OUTPUT TO "clipboard".
   PUT UNFORMATTED iSolution SKIP.
   OUTPUT CLOSE.
   MESSAGE 
      SUBSTITUTE ("Solution: &1.", 
         iSolution) SKIP (1)
      SUBSTITUTE ("Found solution in &1 msecs.", ETIME)
   VIEW-AS ALERT-BOX TITLE " 2023 - Day 03 - Part One".
   
END. /* Process Part One */

IF lPart[2] THEN DO:
   /* Process Part Two */
   iSolution = 0.

   FOR EACH ttNumber:
      FIND FIRST ttSymbol
      WHERE ttSymbol.iX GE ttNumber.iFromX - 1
      AND   ttSymbol.iX LE ttNumber.iToX   + 1
      AND   ttSymbol.iY GE ttNumber.iFromY - 1
      AND   ttSymbol.iY LE ttNumber.iToY   + 1 NO-ERROR.
      IF AVAILABLE ttSymbol THEN DO:
         IF ttSymbol.cSymbol EQ "*" THEN DO:
            /* Found a Gear */
            ASSIGN 
               ttSymbol.Numbers   = ttSymbol.Numbers + 1
               ttSymbol.GearRatio = ttSymbol.GearRatio * ttNumber.iNumber
            .
         END.
      END.
   END.

   FOR EACH ttSymbol
   WHERE ttSymbol.cSymbol EQ "*"
   AND   ttSymbol.Numbers EQ 2:
      iSolution = iSolution + ttSymbol.GearRatio.
   END. 
     
   OUTPUT TO "clipboard".
   PUT UNFORMATTED iSolution SKIP.
   OUTPUT CLOSE.

   MESSAGE 
      SUBSTITUTE ("Solution: &1.", 
         iSolution) SKIP (1)
      SUBSTITUTE ("Found solution in &1 msecs.", ETIME)
   VIEW-AS ALERT-BOX TITLE " 2023 - Day 03 - Part Two".
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
