
/*------------------------------------------------------------------------
    File        : day01.p
    Purpose     : 

    Syntax      :

    Description : Solution for Day01 of 2023

    Author(s)   : Wim van der Ham (WITS)
    Created     : Thu Nov 30 23:55:44 CET 2023
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
DEFINE VARIABLE iDay         AS INTEGER   NO-UNDO INITIAL 1.
DEFINE VARIABLE hPLIP        AS HANDLE    NO-UNDO.
DEFINE VARIABLE cInputFile   AS CHARACTER NO-UNDO.
DEFINE VARIABLE lcInput      AS LONGCHAR  NO-UNDO.
DEFINE VARIABLE iLine        AS INTEGER   NO-UNDO.
DEFINE VARIABLE cLine        AS CHARACTER NO-UNDO.
DEFINE VARIABLE iChar        AS INTEGER   NO-UNDO.
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
   FIELD IDLine AS INTEGER 
   FIELD cInputLine AS CHARACTER 
   FIELD iValue     AS INTEGER
   FIELD iValue2    AS INTEGER  
INDEX indLine IS UNIQUE IDLine.

/* ********************  Preprocessor Definitions  ******************** */

{AOC_session.i}

/* ************************  Function Prototypes ********************** */

FUNCTION getValue RETURNS INTEGER 
   ( INPUT ipcString AS CHARACTER  ) FORWARD.

FUNCTION getWordValue RETURNS INTEGER 
   ( INPUT ipcString AS CHARACTER ) FORWARD.

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
   lcInput = "two1nine~neightwothree~nabcone2threexyz~nxtwone3four~n4nineeightseven2~nzoneight234~n7pqrstsixteen".
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
      ttLine.iValue     = getValue(ttLine.cInputLine)
      ttLine.iValue2    = getWordValue(ttLine.cInputLine)
   .

   iSolution = iSolution + ttLine.iValue.
      
END. /* ReadBlock: */

IF lvlShow THEN DO:
   RUN sy\win\wbrowsett.w
      (INPUT TEMP-TABLE ttLine:HANDLE).
END.

IF lPart[1] THEN DO:
   /* Process Part One */
   OUTPUT TO "clipboard".
   PUT UNFORMATTED iSolution SKIP.
   OUTPUT CLOSE.
   MESSAGE 
      SUBSTITUTE ("Solution: &1.", 
         iSolution) SKIP (1)
      SUBSTITUTE ("Found solution in &1 msecs.", ETIME)
   VIEW-AS ALERT-BOX TITLE " 2023 - Day 01 - Part One".
END. /* Process Part One */

IF lPart[2] THEN DO:

   iSolution = 0.
   FOR EACH ttLine:
      iSolution = iSolution + ttLine.iValue2.
   END.
   
   OUTPUT TO "clipboard".
   PUT UNFORMATTED iSolution SKIP.
   OUTPUT CLOSE.

   MESSAGE 
      SUBSTITUTE ("Solution: &1.", 
         iSolution) SKIP (1)
      SUBSTITUTE ("Found solution in &1 msecs.", ETIME)
   VIEW-AS ALERT-BOX TITLE " 2023 - Day 01 - Part Two".
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

/* ************************  Function Implementations ***************** */


FUNCTION getValue RETURNS INTEGER 
( INPUT ipcString AS CHARACTER ):
/*------------------------------------------------------------------------------
 Purpose: Extract first and last number from string
 Notes:
------------------------------------------------------------------------------*/   
DEFINE VARIABLE iChar AS INTEGER NO-UNDO.
DEFINE VARIABLE cChar AS CHARACTER NO-UNDO.

DEFINE VARIABLE iFirst AS INTEGER NO-UNDO INIT ?.
DEFINE VARIABLE iLast  AS INTEGER NO-UNDO INIT ?.

   DO iChar = 1 TO LENGTH (ipcString):
      cChar = SUBSTRING (ipcString, iChar, 1).
      IF INDEX ("0123456789", cChar) NE 0 THEN DO:
         /* Found a number */
         IF iFirst EQ ? THEN 
            iFirst  = INTEGER (cChar).
         iLast = INTEGER (cChar).
      END.
   END.
   
   RETURN iFirst * 10 + iLast.
   
END FUNCTION.

FUNCTION getWordValue RETURNS INTEGER 
( INPUT ipcString AS CHARACTER   ):
/*------------------------------------------------------------------------------
 Purpose: Get first and last value from string, but the numbers can be words
 Notes:
------------------------------------------------------------------------------*/   
DEFINE VARIABLE cWordList  AS CHARACTER NO-UNDO.
DEFINE VARIABLE iWord      AS INTEGER NO-UNDO.
DEFINE VARIABLE cWord      AS CHARACTER NO-UNDO.
DEFINE VARIABLE iChar      AS INTEGER NO-UNDO.
DEFINE VARIABLE cChar      AS CHARACTER NO-UNDO.
DEFINE VARIABLE cNewString AS CHARACTER NO-UNDO.
DEFINE VARIABLE lFoundWord AS LOGICAL NO-UNDO.

   ASSIGN 
      cWordList = "one,two,three,four,five,six,seven,eight,nine"
   .
   
   IF lvlDebug THEN
      MESSAGE ipcString
      VIEW-AS ALERT-BOX.
   
   iChar = 1.
   DO iChar = 1 TO LENGTH (ipcString):
      lFoundWord = FALSE.
      cChar = SUBSTRING (ipcString, iChar, 1).
      DO iWord = 1 TO NUM-ENTRIES (cWordList):
         cWord = ENTRY (iWord, cWordList).
         IF SUBSTRING (ipcString, iChar) BEGINS cWord THEN DO:
            /* Found a number as a word, replace */
            lFoundWord = TRUE.
            cNewString = cNewString + STRING (iWord).
         END.
      END.
      IF lFoundWord EQ FALSE THEN DO:
         cNewString = cNewString + cChar.
      END.
   END.

   IF lvlDebug THEN    
      MESSAGE cNewString
      VIEW-AS ALERT-BOX.
   
   RETURN getValue(cNewString).

END FUNCTION.
