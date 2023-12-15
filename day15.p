
/*------------------------------------------------------------------------
    File        : day15.p
    Purpose     : Solve Day 15 of Advent of Code 2023

    Syntax      :

    Description : Solution for Advent of Code 2023 - Day 15

    Author(s)   : Wim van der Ham (WITS)
    Created     : Fri Dec 15 10:18:11 CET 2023
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
DEFINE VARIABLE iDay         AS INTEGER   NO-UNDO INITIAL 15.
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
    
DEFINE TEMP-TABLE ttStep
   FIELD IDStep     AS INTEGER 
   FIELD Step       AS CHARACTER 
   FIELD HASH       AS INTEGER 
   // Field for Part Two
   FIELD HASHLabel  AS INTEGER INITIAL ?
   FIELD FocusPower AS INTEGER  
INDEX indID IS UNIQUE IDStep.
DEFINE VARIABLE iNewIDStep AS INTEGER NO-UNDO.

/* Temp-tables for Part Two */
DEFINE TEMP-TABLE ttBox
   FIELD IDBox       AS INTEGER 
   FIELD BoxContents AS CHARACTER
   FIELD FocusPower  AS INTEGER 
INDEX indID IS UNIQUE IDBox.
DEFINE VARIABLE iNewIDBox AS INTEGER NO-UNDO.
 
DEFINE VARIABLE iStep      AS INTEGER    NO-UNDO.
DEFINE VARIABLE cLabel     AS CHARACTER  NO-UNDO.
DEFINE VARIABLE cOperation AS CHARACTER  NO-UNDO.
DEFINE VARIABLE iFocal     AS INTEGER    NO-UNDO.
DEFINE VARIABLE iEntry     AS INTEGER   NO-UNDO.
    
/* ********************  Preprocessor Definitions  ******************** */

/* ************************  Function Prototypes ********************** */

FUNCTION getHASH RETURNS INTEGER 
   ( INPUT ipcString AS CHARACTER ) FORWARD.

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
   lcInput = "rn=1,cm-,qp=3,cm=2,qp-,pc=4,ot=9,ab=5,pc-,pc=6,ot=7".
END.

/* Read Input into Temp-table */
ReadBlock:
DO iLine = 1 TO NUM-ENTRIES (lcInput, "~n"):
   cLine = TRIM (ENTRY (iLine, lcInput, "~n")).

   IF cLine EQ "" THEN DO:
      /* End of Pattern */
      NEXT.
   END.
      
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

IF lPart[1] 
OR lPart[2] THEN DO:
   /* Process Part One */
   /* and load Steps in tt for Part 2 */
   iSolution = 0.

   /* Calcolate Solution for Part 1 */
   FOR EACH ttLine:
      DO iStep = 1 TO NUM-ENTRIES (ttLine.cInputLine):
         iNewIDStep = iNewIDStep + 1.
         CREATE ttStep.
         ASSIGN 
            ttStep.IDStep = iNewIDStep
            ttStep.Step   = TRIM (ENTRY (iStep, ttLine.cInputLine))
            ttStep.HASH   = getHASH(ttStep.Step)
         .
         iSolution = iSolution + ttStep.HASH.
      END.      
   END.
   
   IF lPart[1] THEN DO:
      OUTPUT TO "clipboard".
      PUT UNFORMATTED iSolution SKIP.
      OUTPUT CLOSE.
      MESSAGE 
         SUBSTITUTE ("Solution: &1.", iSolution) SKIP (1)
         SUBSTITUTE ("Found solution in &1 msecs.", ETIME)
      VIEW-AS ALERT-BOX TITLE " 2023 - Day 15 - Part One".
      
      IF lvlShow THEN DO:
      END.
   END.            
END. /* Process Part One */

IF lPart[2] THEN DO:
   /* Process Part Two */
   iSolution = 0.

   DO iNewIDBox = 0 TO 255:
      CREATE ttBox.
      ASSIGN 
         ttBox.IDBox = iNewIDBox
      .
   END.
   
   FOR EACH ttStep:
      /* For each ttStep */
      IF NUM-ENTRIES (ttStep.Step, "=") GT 1 THEN DO:
         /* Add lens */
         ASSIGN 
            cOperation = "="
            cLabel     = ENTRY (1, ttStep.Step, "=")
            iFocal     = INTEGER (ENTRY (2, ttStep.Step, "="))
         .
      END. /* Add lens */
      IF NUM-ENTRIES (ttStep.Step, "-") GT 1 THEN DO:
         /* Remove lens */
         ASSIGN
            cOperation = "-"
            cLabel     = ENTRY (1, ttStep.Step, "-")
         .
      END. /* Remove lens */
      
      IF ttStep.HASHLabel EQ ? THEN 
         ttStep.HASHLabel = getHASH(cLabel).
      
      FIND  ttBox 
      WHERE ttBox.IDBox EQ ttStep.HASHLabel.
      iEntry = LOOKUP (cLabel, ttBox.BoxContents).
      IF cOperation EQ "=" THEN DO:
         IF iEntry EQ 0 THEN DO:
            // Box Contents contains a list of pairs label,focal
            ttBox.BoxContents = SUBSTITUTE ("&1&2&3",
                                             ttBox.BoxContents,
                                             (IF ttBox.BoxContents NE "" THEN "," ELSE ""),
                                             SUBSTITUTE ("&1,&2", cLabel, iFocal)).
         END.
         ELSE DO:
            ENTRY (iEntry + 1, ttBox.BoxContents) = SUBSTITUTE ("&1", iFocal).
         END.
      END.
      IF cOperation EQ "-" THEN DO:
         IF iEntry NE 0 THEN DO:
            ENTRY (iEntry,     ttBox.BoxContents) = "".
            ENTRY (iEntry + 1, ttBox.BoxContents) = "".
            ttBox.BoxContents = TRIM (REPLACE (ttBox.BoxContents, ",,,", ","), ",").
         END.
      END. /* Remove lens */
      IF lvlDebug AND lvlShow THEN DO:
         MESSAGE SUBSTITUTE ("Step: '&1', Box: '&2' Contents: '&3'",
                             ttStep.Step,
                             ttBox.IDBox,
                             ttBox.BoxContents)
         VIEW-AS ALERT-BOX. 
      END.
      
   END. /* For each ttStep */
   
   FOR EACH ttBox:
      DO iEntry = 1 TO NUM-ENTRIES (ttBox.BoxContents) - 1 BY 2:
         ASSIGN 
            cLabel = ENTRY (iEntry, ttBox.BoxContents)
            iFocal = INTEGER (ENTRY (iEntry + 1, ttBox.BoxContents))
         .
         ttBox.FocusPower = ttBox.FocusPower + 
            (ttBox.IDBox + 1) * INTEGER (TRUNCATE ((iEntry + 1) / 2, 0)) * iFocal.
      END.
      iSolution = iSolution + ttBox.FocusPower.
   END.
   
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

/* ************************  Function Implementations ***************** */

FUNCTION getHASH RETURNS INTEGER 
   ( INPUT ipcString AS CHARACTER  ):
/*------------------------------------------------------------------------------
 Purpose:
 Notes:   
 
 The HASH algorithm is a way to turn any string of characters into a single number in the range 0 to 255. 
 To run the HASH algorithm on a string, start with a current value of 0. Then, for each character in the string starting from the beginning:

Determine the ASCII code for the current character of the string.
Increase the current value by the ASCII code you just determined.
Set the current value to itself multiplied by 17.
Set the current value to the remainder of dividing itself by 256.
After following these steps for each character in the string in order, 
the current value is the output of the HASH algorithm.
------------------------------------------------------------------------------*/   
DEFINE VARIABLE iChar  AS INTEGER   NO-UNDO.
DEFINE VARIABLE cChar  AS CHARACTER NO-UNDO.
DEFINE VARIABLE iASCII AS INTEGER   NO-UNDO.
DEFINE VARIABLE iValue AS INTEGER   NO-UNDO.

   iValue = 0.
   DO iChar = 1 TO LENGTH (ipcString):
      cChar = SUBSTRING (ipcString, iChar, 1).
      iASCII = ASC (cChar).
      iValue = iValue + iASCII.
      iValue = iValue * 17.
      iValue = iValue MOD 256.
   END.

   RETURN iValue.
      
END FUNCTION.

