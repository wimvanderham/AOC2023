
/*------------------------------------------------------------------------
    File        : day12.p
    Purpose     : Solve Day 12 of Advent of Code 2023

    Syntax      :

    Description : Solution for Advent of Code 2023 - Day 12

    Author(s)   : Wim van der Ham (WITS)
    Created     : Tue Dec 12 12:11:22 CET 2023
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
DEFINE VARIABLE iDay         AS INTEGER   NO-UNDO INITIAL 12.
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

DEFINE VARIABLE cConditionRecord AS CHARACTER NO-UNDO.
DEFINE VARIABLE cSizeList        AS CHARACTER NO-UNDO.
DEFINE VARIABLE iArrangements    AS INTEGER   NO-UNDO.

/* ********************  Preprocessor Definitions  ******************** */

/* ************************  Function Prototypes ********************** */

FUNCTION getArrangements RETURNS INTEGER 
   ( INPUT ipcConditionRecord AS CHARACTER, 
     INPUT ipcSizesList       AS CHARACTER   ) FORWARD.

FUNCTION getBinary RETURNS CHARACTER 
   ( INPUT ipiValue  AS INT64,
     INPUT ipiLength AS INTEGER ) FORWARD.

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
END. /* ReadBlock: */

IF lvlShow THEN DO:
   RUN sy\win\wbrowsett.w
      (INPUT TEMP-TABLE ttLine:HANDLE). 
END.

IF lvlOutput THEN DO:
   cOutputFile = SUBSTITUTE ("output\&1_start.txt", STRING (iDay, "99")).
   OUTPUT TO VALUE (cOutputFile).
   PUT UNFORMATTED 
      SUBSTITUTE ("Start run &1", STRING (NOW, "99-99-9999 HH:MM:SS")) SKIP.
   OUTPUT CLOSE.   
END.

IF lPart[1] THEN DO:
   /* Process Part One */
   iSolution = 0.

   /* Calcolate Solution for Part 1 */
   FOR EACH ttLine:
      ASSIGN 
         cConditionRecord = ENTRY(1, ttLine.cInputLine, " ")
         cSizeList        = ENTRY(2, ttLine.cInputLine, " ")
      .
      IF lvlDebug THEN DO:
         MESSAGE "Calculate Arrangements for:" SKIP 
         "Condition Record:" cConditionRecord SKIP 
         "Size List:" cSizeList SKIP  
         VIEW-AS ALERT-BOX.
      END.
      
      IF lvlOutput THEN DO:
         OUTPUT TO VALUE (cOutputFile) APPEND.
      END.
      
      iArrangements = getArrangements(cConditionRecord, cSizeList).

      IF lvlOutput THEN DO:
         OUTPUT CLOSE.
      END.
            
      IF lvlDebug THEN DO:
         MESSAGE "Arrangements:" SKIP 
         iArrangements
         VIEW-AS ALERT-BOX.
      END.
      
      iSolution = iSolution + iArrangements. 
   END.
   
   OUTPUT TO "clipboard".
   PUT UNFORMATTED iSolution SKIP.
   OUTPUT CLOSE.
   MESSAGE 
      SUBSTITUTE ("Solution: &1.", iSolution) SKIP (1)
      SUBSTITUTE ("Found solution in &1 msecs.", ETIME)
   VIEW-AS ALERT-BOX TITLE " 2023 - Day 12 - Part One".
   
   IF lvlShow THEN DO:
      RUN sy\win\wbrowsett.w
         (INPUT TEMP-TABLE ttLine:HANDLE).
   END.
            
END. /* Process Part One */

IF lPart[2] THEN DO:
   /* Process Part Two */
   iSolution = 0.
   
   /* Calcolate Solution for Part 2 */
   FOR EACH ttLine:
      ASSIGN 
         cConditionRecord = ENTRY(1, ttLine.cInputLine, " ")
         cSizeList        = ENTRY(2, ttLine.cInputLine, " ")
      .

      /*
      To unfold the records, on each row, replace the list of spring conditions 
      with five copies of itself (separated by ?) and replace the list of 
      contiguous groups of damaged springs with five copies of itself (separated by ,).
      */
      ASSIGN 
         cConditionRecord = SUBSTITUTE ("&1?&1?&1?&1?&1", cConditionRecord)
         cSizeList        = SUBSTITUTE ("&1,&1,&1,&1,&1", cSizeList)
      .
      
      IF lvlOutput THEN DO:
         OUTPUT TO VALUE (cOutputFile) APPEND.
      END.
      
      // Don't solve Part Two in the same way as Part One (would take much too long...
      // iArrangements = getArrangements(cConditionRecord, cSizeList).

      IF lvlOutput THEN DO:
         OUTPUT CLOSE.
      END.
            
      IF lvlDebug THEN DO:
         MESSAGE "Arrangements:" SKIP 
         iArrangements
         VIEW-AS ALERT-BOX.
      END.
      
      iSolution = iSolution + iArrangements. 
   END.
   
   OUTPUT TO "clipboard".
   PUT UNFORMATTED iSolution SKIP.
   OUTPUT CLOSE.

   MESSAGE 
      SUBSTITUTE ("Solution: &1.", iSolution) SKIP (1)
      SUBSTITUTE ("Found solution in &1 msecs.", ETIME)
   VIEW-AS ALERT-BOX TITLE " 2023 - Day 12 - Part Two".
   
   IF lvlShow THEN DO:
      RUN sy\win\wbrowsett.w
         (INPUT TEMP-TABLE ttLine:HANDLE).
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

FUNCTION getArrangements RETURNS INTEGER 
   ( INPUT ipcConditionRecord AS CHARACTER, 
     INPUT ipcSizesList       AS CHARACTER  ):
/*------------------------------------------------------------------------------
 Purpose: Find all possibile arrangements for a ConditionRecord
          satisfying the sizes list
 Notes:   Input:
            ?###???????? 3,2,1
 
          Has 10 possible arrangements:
            .###.##.#...
            .###.##..#..
            .###.##...#.
            .###.##....#
            .###..##.#..
            .###..##..#.
            .###..##...#
            .###...##.#.
            .###...##..#
            .###....##.# 
             
   Approach: 1. Extract (and count) the number of question marks in the
                ConditionRecord
             2. Consider them bits a a binary string 0 = NOK or #, 1 = OK or .
             3. Loop from 0 to the maximum value of the binary string and substitute
                the question marks
                  Confront the result with the sizeslist
                  If ok, add 1 to the number of arrangements
                           
------------------------------------------------------------------------------*/   
DEFINE VARIABLE iCondition AS INTEGER NO-UNDO.
DEFINE VARIABLE cCondition AS CHARACTER NO-UNDO.

DEFINE VARIABLE cQuestionMarkList AS CHARACTER NO-UNDO.
DEFINE VARIABLE iLenghtBinary     AS INTEGER   NO-UNDO.
DEFINE VARIABLE cBinary           AS CHARACTER NO-UNDO.
DEFINE VARIABLE iBinary           AS INTEGER   NO-UNDO.
DEFINE VARIABLE iBinaryTo         AS INT64     NO-UNDO.
DEFINE VARIABLE iBit              AS INTEGER   NO-UNDO.
DEFINE VARIABLE cBit              AS CHARACTER NO-UNDO.
DEFINE VARIABLE iIndex            AS INTEGER   NO-UNDO.
DEFINE VARIABLE iElement          AS INTEGER   NO-UNDO.
DEFINE VARIABLE cArrangement      AS CHARACTER NO-UNDO.
DEFINE VARIABLE iArrangementsOK   AS INTEGER   NO-UNDO.

   IF lvlOutput THEN DO:
      PUT UNFORMATTED SKIP(1)
         SUBSTITUTE ("Start Condition Record: '&1' and Size List: '&2'.", 
                     ipcConditionRecord,
                     ipcSizesList) SKIP.
   END.
                        
   DO iCondition = 1 TO LENGTH (ipcConditionRecord):
      cCondition = SUBSTRING (ipcConditionRecord, iCondition, 1).
      IF cCondition EQ "?" THEN DO:
         cQuestionMarkList = SUBSTITUTE ("&1&2&3",
                                         cQuestionMarkList,
                                         (IF cQuestionMarkList EQ "" THEN "" ELSE ","),
                                         iCondition).
      END.
   END.
   
   cBinary = FILL ("1", NUM-ENTRIES (cQuestionMarkList)).
   iBinaryTo = getINT64(cBinary).
   
   IF lvlOutput THEN DO:
      PUT UNFORMATTED 
         SUBSTITUTE ("Question Mark List: '&1', Binary to '&2'.",
                     cQuestionMarkList,
                     iBinaryTo) SKIP.
   END.
                        
   DO iBinary = 0 TO iBinaryTo:
      /* All possible arrangements */
      cBinary = getBinary(iBinary, NUM-ENTRIES (cQuestionMarkList)).
      IF lvlOutput THEN DO:
         PUT UNFORMATTED 
            SUBSTITUTE ("Length(&1) EQ NUM-ENTRIES(&2)? &3",
                        cBinary,
                        cQuestionMarkList,
                        LENGTH (cBinary) EQ NUM-ENTRIES (cQuestionMarkList)) SKIP.
      END.                        
      cArrangement = ipcConditionRecord.
      DO iBit = 1 TO LENGTH (cBinary):
         cBit = SUBSTRING (cBinary, iBit, 1).
         IF cBit EQ "0" THEN 
            SUBSTRING (cArrangement, INTEGER (ENTRY (iBit, cQuestionMarkList))) = ".".
         ELSE 
            SUBSTRING (cArrangement, INTEGER (ENTRY (iBit, cQuestionMarkList))) = "#".
      END.
      IF lvlOutput THEN DO:
         PUT UNFORMATTED 
            SUBSTITUTE ("Try option #&1: '&2'",
                        iBinary,
                        cArrangement) SKIP.
      END.                        
      
      cArrangement = REPLACE (cArrangement, ".", ",").
      DO WHILE INDEX (cArrangement, ",,") NE 0:
         cArrangement = REPLACE (cArrangement, ",,", ",").
      END.
      /* Remove start and end "," characters */
      cArrangement = TRIM (cArrangement, ",").         
         
      IF NUM-ENTRIES (cArrangement) EQ NUM-ENTRIES (ipcSizesList) THEN 
      CheckBlock:
      DO:
         IF lvlOutput THEN DO:
            PUT UNFORMATTED 
               SUBSTITUTE ("Check arrangement: '&1'.",
                           cArrangement) SKIP.
         END.
                                          
         DO iIndex = 1 TO NUM-ENTRIES (cArrangement):
            IF LENGTH (ENTRY (iIndex, cArrangement)) NE INTEGER (ENTRY (iIndex, ipcSizesList)) THEN DO:
               /* Mismatch, stop checking */
               LEAVE CheckBlock.
            END.
         END.
         iArrangementsOK = iArrangementsOK + 1. 
         IF lvlOutput THEN DO:
            PUT UNFORMATTED 
               SUBSTITUTE ("Arrangement: '&1' OK #&2",
                           cArrangement,
                           iArrangementsOK) SKIP.
         END.                        
      END. /* CheckBlock */
      ELSE DO:
         /* NOK */
         IF lvlOutput THEN DO:
            PUT UNFORMATTED 
               SUBSTITUTE ("Arrangement NOK: NUM-ENTRIES('&1') NE NUM-ENTRIES('&2').",
                           cArrangement,
                           ipcSizesList) SKIP.
         END.
      END. /* NOK */         
   END. /* All possible arrangements */
   
   RETURN iArrangementsOK.
      
END FUNCTION.

/* Helper functions for conversion binary string to integer and back */
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
