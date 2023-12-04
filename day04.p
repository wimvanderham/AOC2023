
/*------------------------------------------------------------------------
    File        : day04.p
    Purpose     : Solve Day 04 of Advent of Code 2023

    Syntax      :

    Description : Solution to Day 04 of Advent of Code 2023

    Author(s)   : Wim van der Ham (WITS)
    Created     : Sun Dec 03 23:55:16 CET 2023
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
DEFINE VARIABLE iDay         AS INTEGER   NO-UNDO INITIAL 4.
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


DEFINE TEMP-TABLE ttGame
   FIELD IDGame AS INTEGER 
   FIELD GameNr AS INTEGER 
   FIELD Score  AS INTEGER 
INDEX indID IS UNIQUE IDGame.
DEFINE VARIABLE iNewIDGame AS INTEGER NO-UNDO.

DEFINE TEMP-TABLE ttWinNumber
   FIELD IDWinNumber AS INTEGER 
   FIELD IDGame      AS INTEGER 
   FIELD Order       AS INTEGER 
   FIELD Number      AS INTEGER
INDEX indID IS UNIQUE IDWinNumber 
INDEX indGame IS UNIQUE PRIMARY IDGame Order.
DEFINE VARIABLE iNewIDWinNumber AS INTEGER NO-UNDO.

DEFINE TEMP-TABLE ttMyNumber
   FIELD IDMyNumber  AS INTEGER 
   FIELD IDGame      AS INTEGER 
   FIELD Order       AS INTEGER 
   FIELD Number      AS INTEGER
INDEX indID IS UNIQUE IDMyNumber 
INDEX indGame IS UNIQUE PRIMARY IDGame Order.
DEFINE VARIABLE iNewIDMyNumber AS INTEGER NO-UNDO.

DEFINE VARIABLE cGame       AS CHARACTER NO-UNDO.
DEFINE VARIABLE cWinNumbers AS CHARACTER NO-UNDO.
DEFINE VARIABLE cMyNumbers  AS CHARACTER NO-UNDO.
DEFINE VARIABLE iNumber     AS INTEGER   NO-UNDO.
DEFINE VARIABLE cNumber     AS CHARACTER NO-UNDO.  
DEFINE VARIABLE iOrder      AS INTEGER   NO-UNDO.  
   
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
   lcInput = "Card 1: 41 48 83 86 17 | 83 86  6 31 17  9 48 53~nCard 2: 13 32 20 16 61 | 61 30 68 82 17 32 24 19~nCard 3:  1 21 53 59 44 | 69 82 63 72 16 21 14  1~nCard 4: 41 92 73 84 69 | 59 84 76 51 58  5 54 83~nCard 5: 87 83 26 28 32 | 88 30 70 12 93 22 82 36~nCard 6: 31 18 13 56 72 | 74 77 10 23 35 67 36 11".
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
   ASSIGN 
      cGame = ENTRY (1, ttLine.cInputLine, ":")
      cWinNumbers = TRIM (ENTRY (1, TRIM (ENTRY (2, ttLine.cInputLine, ":")), "|"))
      cMyNumbers  = TRIM (ENTRY (2, TRIM (ENTRY (2, ttLine.cInputLine, ":")), "|"))
   .
   
   IF lvlShow THEN DO:
      MESSAGE 
      SUBSTITUTE ("Input: '&1'", ttLine.cInputLine) SKIP
      SUBSTITUTE ("Game: '&1'", cGame) SKIP
      SUBSTITUTE ("Win Numbers: '&1'", cWinNumbers) SKIP 
      SUBSTITUTE ("My Numbers: '&1'", cMyNumbers)
      VIEW-AS ALERT-BOX.
   END.
   
   iNewIDGame = iNewIDGame + 1.
   CREATE ttGame.
   ASSIGN 
      ttGame.IDGame = iNewIDGame
      ttGame.GameNr = INTEGER (TRIM (SUBSTRING (cGame, 6)))
   .
   
   iOrder = 0.
   WinBlock:
   DO iNumber = 1 TO NUM-ENTRIES (cWinNumbers, " "):
      cNumber = TRIM (ENTRY (iNumber, cWinNumbers, " ")).
      IF cNumber EQ "" THEN
         NEXT WinBlock. 
      iNewIDWinNumber = iNewIDWinNumber + 1.
      iOrder = iOrder + 1.
      CREATE ttWinNumber.
      ASSIGN 
         ttWinNumber.IDWinNumber = iNewIDWinNumber
         ttWinNumber.IDGame      = ttGame.IDGame
         ttWinNumber.Order       = iOrder
         ttWinNumber.Number      = INTEGER (cNumber)
      .
   END.
   
   iOrder = 0.
   MyBlock:
   DO iNumber = 1 TO NUM-ENTRIES (cMyNumbers, " "):
      cNumber = TRIM (ENTRY (iNumber, cMyNumbers, " ")).
      IF cNumber EQ "" THEN 
         NEXT MyBlock.
      iNewIDMyNumber = iNewIDMyNumber + 1.
      iOrder = iOrder + 1.
      CREATE ttMyNumber.
      ASSIGN 
         ttMyNumber.IDMyNumber = iNewIDMyNumber
         ttMyNumber.IDGame     = ttGame.IDGame
         ttMyNumber.Order      = iOrder
         ttMyNumber.Number     = INTEGER (cNumber)
      .
   END.
   
END. /* ReadBlock: */

IF lvlShow THEN DO:
   RUN sy\win\wbrowsett.w
      (INPUT TEMP-TABLE ttGame:HANDLE).
   RUN sy\win\wbrowsett.w
      (INPUT TEMP-TABLE ttWinNumber:HANDLE).
   RUN sy\win\wbrowsett.w
      (INPUT TEMP-TABLE ttMyNumber:HANDLE).
END.

IF lPart[1] THEN DO:
   /* Process Part One */
   iSolution = 0.

   FOR EACH ttGame:
      FOR EACH ttMyNumber OF ttGame:
         IF CAN-FIND (ttWinNumber WHERE ttWinNumber.IDGame EQ ttMyNumber.IDGame AND ttWinNumber.Number EQ ttMyNumber.Number) THEN DO:
            /* Found a winning number */
            IF ttGame.Score = 0 THEN 
               ttGame.Score = 1.
            ELSE 
               ttGame.Score = ttGame.Score * 2.
         END.
      END.
      iSolution = iSolution + ttGame.Score.
   END.
   
   OUTPUT TO "clipboard".
   PUT UNFORMATTED iSolution SKIP.
   OUTPUT CLOSE.
   MESSAGE 
      SUBSTITUTE ("Solution: &1.", 
         iSolution) SKIP (1)
      SUBSTITUTE ("Found solution in &1 msecs.", ETIME)
   VIEW-AS ALERT-BOX TITLE " 2023 - Day 04 - Part One".
   
END. /* Process Part One */

IF lPart[2] THEN DO:
   /* Process Part Two */
   iSolution = 0.

     
   OUTPUT TO "clipboard".
   PUT UNFORMATTED iSolution SKIP.
   OUTPUT CLOSE.

   MESSAGE 
      SUBSTITUTE ("Solution: &1.", 
         iSolution) SKIP (1)
      SUBSTITUTE ("Found solution in &1 msecs.", ETIME)
   VIEW-AS ALERT-BOX TITLE " 2023 - Day 04 - Part Two".
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
