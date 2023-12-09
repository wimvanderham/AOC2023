
/*------------------------------------------------------------------------
    File        : day09.p
    Purpose     : Solve Day 09 of Advent of Code 2023

    Syntax      :

    Description : Solution for Advent of Code 2023 - Day 09

    Author(s)   : Wim van der Ham (WITS)
    Created     : Sat Dec 09 14:17:11 CET 2023
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
DEFINE VARIABLE iDay         AS INTEGER   NO-UNDO INITIAL 9.
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
DEFINE VARIABLE iPart        AS INTEGER   NO-UNDO.

/* Specific */
DEFINE TEMP-TABLE ttLine
   FIELD IDLine      AS INTEGER 
   FIELD cInputLine  AS CHARACTER FORMAT "X(80)"
   /* Extra field for Solution Part 1 */
   FIELD AddNumber   AS INT64 
INDEX indLine IS UNIQUE IDLine.

DEFINE TEMP-TABLE ttLineHist
   FIELD IDLineHist AS INTEGER
   FIELD IDLine     AS INTEGER  
   FIELD NrHist     AS INTEGER 
   FIELD DiffLine   AS CHARACTER 
   FIELD AddNumber  AS INT64 
INDEX indID   IS UNIQUE IDLineHist
INDEX indLine IS UNIQUE PRIMARY IDLine NrHist.
DEFINE VARIABLE iNewIDLineHist AS INTEGER   NO-UNDO.
DEFINE VARIABLE cDiffLine      AS CHARACTER NO-UNDO.
DEFINE VARIABLE iNrHist        AS INTEGER   NO-UNDO.
DEFINE VARIABLE cNumberLine    AS CHARACTER NO-UNDO.
DEFINE VARIABLE iNumber        AS INT64     NO-UNDO.
DEFINE VARIABLE iAddNumber     AS INT64     NO-UNDO.
DEFINE VARIABLE iNextNumber    AS INT64     NO-UNDO.
DEFINE VARIABLE iValue1        AS INT64     NO-UNDO.
DEFINE VARIABLE iValue2        AS INT64     NO-UNDO.
 
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

IF lPart[1] THEN DO:
   /* Process Part One */
   iSolution = 0.

   /* Calcolate Solution for Part 1 */
   FOR EACH ttLine:
      iNrHist = 0.
      cNumberLine = ttLine.cInputLine.
      HistBlock:
      REPEAT:
         cDiffLine = "".
         DO iNumber = 1 TO NUM-ENTRIES (cNumberLine, " ") - 1:
            ASSIGN 
               iValue1 = INTEGER (ENTRY (iNumber, cNumberLine, " "))
               iValue2 = INTEGER (ENTRY (iNumber + 1, cNumberLine, " "))
            .
            cDiffLine = SUBSTITUTE ("&1&2&3",
                                    cDiffLine,
                                    (IF cDiffLine NE "" THEN " " ELSE ""),
                                    iValue2 - iValue1).
         END.
         IF lvlShow THEN DO:
            MESSAGE 
            "Input numbers:" cNumberLine SKIP 
            "Differences:" cDiffLine
            VIEW-AS ALERT-BOX.
         END.
         
         iNewIDLineHist = iNewIDLineHist + 1.
         iNrHist        = iNrHist + 1.
         CREATE ttLineHist.
         ASSIGN 
            ttLineHist.IDLineHist = iNewIDLineHist
            ttLineHist.IDLine     = ttLine.IDLine
            ttLineHist.NrHist     = iNrHist
            ttLineHist.DiffLine   = cDiffLine
         .
         IF TRIM (REPLACE (cDiffLine, "0", "")) EQ "" THEN DO:
            /* Reached the final difference, all 0's */
            ASSIGN 
               iAddNumber          = 0
               ttLineHist.DiffLine = SUBSTITUTE ("&1 &2", ttLineHist.DiffLine, iAddNumber)
            .
            UpdateBlock:
            REPEAT:
               iNrHist = iNrHist - 1.
               FIND  ttLineHist 
               WHERE ttLineHist.IDLine EQ ttLine.IDLine
               AND   ttLineHist.NrHist EQ iNrHist.
               iNextNumber = INTEGER (ENTRY (NUM-ENTRIES (ttLineHist.DiffLine, " "), ttLineHist.DiffLine, " ")) + iAddNumber.
               iAddNumber  = iNextNumber.
               ttLineHist.DiffLine = SUBSTITUTE ("&1 &2", ttLineHist.DiffLine, iAddNumber).
               ttLineHist.AddNumber = iAddNumber.
               IF lvlShow THEN DO:
                  MESSAGE 
                  "NrHist:" iNrHist SKIP 
                  "New Differences:" ttLineHist.DiffLine
                  VIEW-AS ALERT-BOX.
               END.
               
               IF iNrHist EQ 1 THEN DO:
                  iNextNumber = INTEGER (ENTRY (NUM-ENTRIES (ttLine.cInputLine, " "), ttLine.cInputLine, " ")) + iAddNumber.
                  iAddNumber = iNextNumber.
                  ttLine.cInputLine = SUBSTITUTE ("&1 &2", ttLine.cInputLine, iAddNumber).
                  ttLine.AddNumber  = iAddNumber.
                  IF lvlShow THEN DO:
                     MESSAGE 
                     "Line:" ttLine.IDLine SKIP 
                     "New Numbers:" ttLine.cInputLine
                     VIEW-AS ALERT-BOX.
                  END.
                  iSolution = iSolution + iAddNumber.
                  LEAVE UpdateBlock.
               END.
               ELSE DO:
                  IF lvlShow THEN DO:
                     MESSAGE "Process HistLine" iNrHist SKIP 
                     "for inputline:" ttLine.IDLine SKIP 
                     ttLine.cInputLine
                     VIEW-AS ALERT-BOX.
                  END.
               END.
            END.
            LEAVE HistBlock.
         END.
         ELSE DO:
            cNumberLine = ttLineHist.DiffLine.
         END. 
      END.
   END.    
                                
   OUTPUT TO "clipboard".
   PUT UNFORMATTED iSolution SKIP.
   OUTPUT CLOSE.
   MESSAGE 
      SUBSTITUTE ("Solution: &1.", iSolution) SKIP (1)
      SUBSTITUTE ("Found solution in &1 msecs.", ETIME)
   VIEW-AS ALERT-BOX TITLE " 2023 - Day 09 - Part One".
   
   IF lvlShow THEN DO:
      RUN sy\win\wbrowsett.w
         (INPUT TEMP-TABLE ttLine:HANDLE).
      RUN sy\win\wbrowsett.w
         (INPUT TEMP-TABLE ttLineHist:HANDLE).
   END.
            
END. /* Process Part One */

IF lPart[2] THEN DO:
   /* Process Part Two */
   iSolution = 0.
   
   /* Calcolate Solution for Part 2 */
   
   OUTPUT TO "clipboard".
   PUT UNFORMATTED iSolution SKIP.
   OUTPUT CLOSE.

   MESSAGE 
      SUBSTITUTE ("Solution: &1.", iSolution) SKIP (1)
      SUBSTITUTE ("Found solution in &1 msecs.", ETIME)
   VIEW-AS ALERT-BOX TITLE " 2023 - Day 09 - Part Two".
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
      RUN sy\win\wbrowsett.w
         (INPUT TEMP-TABLE ttLineHist:HANDLE).
   END.
   RETURN.      
END CATCH.


/* **********************  Internal Procedures  *********************** */




