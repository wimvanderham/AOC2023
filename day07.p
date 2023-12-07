
/*------------------------------------------------------------------------
    File        : day07.p
    Purpose     : Solve Advent of Code 2023 - Day 07

    Syntax      :

    Description : Solution Advent of Code 2023 - Day 07 - Play Poker

    Author(s)   : wim
    Created     : Thu Dec 07 12:20:18 CET 2023
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
DEFINE VARIABLE iDay         AS INTEGER   NO-UNDO INITIAL 7.
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

DEFINE TEMP-TABLE ttHand
   FIELD IDHand     AS INTEGER 
   FIELD Cards      AS CHARACTER 
   FIELD BidAmount  AS DECIMAL     
   FIELD Rank       AS INTEGER 
   FIELD Strength   AS DECIMAL   FORMAT "9.99999999999"
   FIELD WinAmount  AS DECIMAL 
   FIELD JCards     AS CHARACTER // Substitution of J into best alternative
   FIELD JRank      AS INTEGER 
   FIELD JStrength  AS DECIMAL   FORMAT "9.99999999999"
   FIELD JWinAmount AS DECIMAL 
INDEX indID IS UNIQUE IDHand.
DEFINE VARIABLE iNewIDHand AS INTEGER NO-UNDO.
 
/* ********************  Preprocessor Definitions  ******************** */

{AOC_session.i}

/* ************************  Function Prototypes ********************** */

FUNCTION getDecimal RETURNS DECIMAL 
   ( INPUT ipcHexString AS CHARACTER ) FORWARD.

FUNCTION getJStrength RETURNS DECIMAL 
   ( INPUT  ipcCards     AS CHARACTER,
     INPUT  ipcCardOrder AS CHARACTER,
     OUTPUT opcJCards    AS CHARACTER ) FORWARD.

FUNCTION getStrength RETURNS DECIMAL 
   ( INPUT ipcCards     AS CHARACTER,
     INPUT ipcCardOrder AS CHARACTER  ) FORWARD.

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

   /* Parsing */
   iNewIDHand = iNewIDHand + 1.
   CREATE ttHand.
   ASSIGN 
      ttHand.IDHand = iNewIDHand
      ttHand.Cards  = ENTRY (1, ttLine.cInputLine, " ")
      ttHand.BidAmount = DECIMAL (ENTRY (2, ttLine.cInputLine, " "))
   .
             
   ASSIGN 
      ttHand.Strength = getStrength(ttHand.Cards, "AKQJT98765432")
   .             
END. /* ReadBlock: */

IF lvlShow THEN DO:
   RUN sy\win\wbrowsett.w
      (INPUT TEMP-TABLE ttHand:HANDLE).   
END.

IF lPart[1] THEN DO:
   /* Process Part One */
   iSolution = 0.

   /* Calcolate Solution for Part 1 */
   FOR EACH ttHand
   BY ttHand.Strength:
      ACCUM "" (COUNT).
      ASSIGN 
         ttHand.Rank = (ACCUM COUNT "")
         ttHand.WinAmount = ttHand.Rank * ttHand.BidAmount
      .
      iSolution = iSolution + ttHand.WinAmount.
   END.
   
   OUTPUT TO "clipboard".
   PUT UNFORMATTED iSolution SKIP.
   OUTPUT CLOSE.
   MESSAGE 
      SUBSTITUTE ("Solution: &1.", iSolution) SKIP (1)
      SUBSTITUTE ("Found solution in &1 msecs.", ETIME)
   VIEW-AS ALERT-BOX TITLE " 2023 - Day 07 - Part One".
   
END. /* Process Part One */

IF lPart[2] THEN DO:
   /* Process Part Two */
   iSolution = 0.
   
   /* Calcolate Solution for Part 2 */
   FOR EACH ttHand:
      ttHand.JStrength = getStrength(ttHand.Cards, "AKQT98765432J").
      IF INDEX (ttHand.Cards, "J") NE 0 THEN
         ttHand.JStrength = getJStrength(ttHand.Cards, "AKQT98765432J", ttHand.JCards).
   END.
   
   FOR EACH ttHand
   BY ttHand.JStrength:
      ACCUM "" (COUNT).
      ASSIGN 
         ttHand.JRank = (ACCUM COUNT "")
         ttHand.JWinAmount = ttHand.JRank * ttHand.BidAmount
      .
      iSolution = iSolution + ttHand.JWinAmount.
   END.
      
   IF lvlShow THEN
      RUN sy\win\wbrowsett.w
         (INPUT TEMP-TABLE ttHand:HANDLE).
      
   OUTPUT TO "clipboard".
   PUT UNFORMATTED iSolution SKIP.
   OUTPUT CLOSE.

   MESSAGE 
      SUBSTITUTE ("Solution: &1.", iSolution) SKIP (1)
      SUBSTITUTE ("Found solution in &1 msecs.", ETIME)
   VIEW-AS ALERT-BOX TITLE " 2023 - Day 07 - Part Two".
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


/* **********************  Internal Procedures  *********************** */



/* ************************  Function Implementations ***************** */

FUNCTION getDecimal RETURNS DECIMAL 
   ( INPUT ipcHexString AS CHARACTER ):
/*------------------------------------------------------------------------------
 Purpose: Return decimal value for a hex string
 Notes:
------------------------------------------------------------------------------*/   
DEFINE VARIABLE iChar      AS INTEGER   NO-UNDO.
DEFINE VARIABLE cChar      AS CHARACTER NO-UNDO.
DEFINE VARIABLE iBase      AS INTEGER   NO-UNDO.
DEFINE VARIABLE iValue     AS INTEGER   NO-UNDO.
DEFINE VARIABLE cValueList AS CHARACTER NO-UNDO INITIAL "0123456789ABCDEF".
DEFINE VARIABLE deValue    AS DECIMAL   NO-UNDO.
   
   iBase = 1. // Start value 16^0
   DO iChar = LENGTH (ipcHexString) TO 1 BY -1:
      cChar  = SUBSTRING (ipcHexString, iChar, 1).
      iValue = (INTEGER (INDEX (cValueList, cChar)) - 1) * iBase.
      deValue = deValue + iValue.
      iBase = iBase * 16.
   END.
   
   RETURN deValue.
   
END FUNCTION.

FUNCTION getJStrength RETURNS DECIMAL 
   ( INPUT  ipcJCards    AS CHARACTER,
     INPUT  ipcCardOrder AS CHARACTER,
     OUTPUT opcJCards    AS CHARACTER ):
/*------------------------------------------------------------------------------
 Purpose: Find the best card to replace the J with
 Notes:
------------------------------------------------------------------------------*/   
DEFINE VARIABLE cCards        AS CHARACTER NO-UNDO INITIAL "AKQT98765432".
DEFINE VARIABLE iCard         AS INTEGER   NO-UNDO.
DEFINE VARIABLE cCard         AS CHARACTER NO-UNDO.
DEFINE VARIABLE cJCards       AS CHARACTER NO-UNDO.
DEFINE VARIABLE deStrength    AS DECIMAL   NO-UNDO.
DEFINE VARIABLE deJStrength   AS DECIMAL   NO-UNDO.
DEFINE VARIABLE deMaxStrength AS DECIMAL   NO-UNDO.

   deStrength = getStrength (ipcJCards, ipcCardOrder).
   deStrength = deStrength - TRUNCATE (deStrength, 0).
   
   DO iCard = 1 TO LENGTH (cCards):
      cCard = SUBSTRING (cCards, iCard, 1).
      cJCards = REPLACE (ipcJCards, "J", cCard).
      deJStrength = TRUNCATE (getStrength(cJCards, ipcCardOrder), 0) + deStrength.
      IF deJStrength GT deMaxStrength THEN DO:
         deMaxStrength = deJStrength.
         opcJCards     = cJCards.
      END.
   END.
   
   RETURN deMaxStrength.
      
END FUNCTION.

FUNCTION getStrength RETURNS DECIMAL 
   ( INPUT ipcCards     AS CHARACTER,
     INPUT ipcCardOrder AS CHARACTER ):
/*------------------------------------------------------------------------------
 Purpose: Determine strength of cards
 Notes:   There are seven strength levels based on the combination
          7 = Five of a Kind
          6 = Four of a kind
          5  = Full House
          4 = Three of a Kind
          3 = Two Pairs
          2 = One Pair
          1 = High Card
          And 13 sublevels based on the card values 
------------------------------------------------------------------------------*/   
DEFINE VARIABLE iNrCards      AS INTEGER   NO-UNDO EXTENT 13. /* Number of cards per "label" */
DEFINE VARIABLE cHexValues    AS CHARACTER NO-UNDO INITIAL "EDCBA98765432". // Hex value for single cards
DEFINE VARIABLE cHexValue     AS CHARACTER NO-UNDO.
DEFINE VARIABLE iLabel        AS INTEGER   NO-UNDO.
DEFINE VARIABLE iCard         AS INTEGER   NO-UNDO.
DEFINE VARIABLE cCard         AS CHARACTER NO-UNDO.
DEFINE VARIABLE deStrength    AS DECIMAL   NO-UNDO.
DEFINE VARIABLE deNewStrength AS DECIMAL   NO-UNDO.
DEFINE VARIABLE lFullHouse3   AS LOGICAL   NO-UNDO.
DEFINE VARIABLE lFoundPair    AS LOGICAL   NO-UNDO.
DEFINE VARIABLE deHandValue   AS DECIMAL   NO-UNDO.
DEFINE VARIABLE cMaxValue     AS CHARACTER NO-UNDO.

   DO iCard = 1 TO LENGTH (ipcCards):
      ASSIGN 
         cCard            = SUBSTRING (ipcCards, iCard, 1)
         iLabel           = INDEX (ipcCardOrder, cCard)
         iNrCards[iLabel] = iNrCards[iLabel] + 1
      .
      // Use base 16 (Hex) for the sum of values of the single cards
      cHexValue = SUBSTITUTE ("&1&2",
                              cHexValue,
                              SUBSTRING (cHexValues, iLabel, 1)).
   END.
   
   /* Check combination */
   deStrength = 0.
   CombinationBlock:
   DO iCard = 1 TO 13:
      IF iNrCards[iCard] EQ 5 THEN DO:
         deStrength = 7.
         /* Won't get any higher */
         LEAVE CombinationBlock.
      END.
      IF iNrCards[iCard] EQ 4 THEN
         deNewStrength = 6.
      IF iNrCards[iCard] EQ 3 THEN DO: 
         deNewStrength = 4.
         lFullHouse3 = TRUE.
         IF lFoundPair EQ TRUE THEN 
            deNewStrength = 5.
      END.
      IF iNrCards[iCard] EQ 2 THEN DO:
         deNewStrength = 2.
         IF lFoundPair EQ TRUE THEN DO:
            // Found another pair
            deNewStrength = 3.
         END.
         ELSE DO:
            lFoundPair = TRUE.
         END.
         IF lFullHouse3 EQ TRUE THEN 
            deNewStrength = 5.
      END.
      IF iNrCards[iCard] EQ 1 THEN DO:
         deNewStrength = 1.                
      END.
      
      IF deNewStrength GT deStrength THEN 
         deStrength = deNewStrength.    
   END.
   
   cMaxValue = "FFFFF".
   
   deHandValue = deStrength + getDecimal(cHexValue) / getDecimal(cMaxValue).
   
   RETURN deHandValue.
   
END FUNCTION.
