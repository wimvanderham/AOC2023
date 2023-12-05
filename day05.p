
/*------------------------------------------------------------------------
    File        : day05.p
    Purpose     : Solve Day 05 of Advent of Code 2023

    Syntax      :

    Description : Solution to Day 05 of Advent of Code 2023

    Author(s)   : Wim van der Ham (WITS)
    Created     : Tue Dec 05 09:35:09 CET 2023
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
DEFINE VARIABLE iDay         AS INTEGER   NO-UNDO INITIAL 5.
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

DEFINE TEMP-TABLE ttTransform
   FIELD IDTransform AS INTEGER 
   FIELD cFrom       AS CHARACTER 
   FIELD cTo         AS CHARACTER 
INDEX indID IS UNIQUE IDTransform
INDEX indFromTo IS UNIQUE PRIMARY cFrom cTo.
DEFINE VARIABLE iNewIDTransform AS INTEGER NO-UNDO.

DEFINE TEMP-TABLE ttRange
   FIELD IDRange     AS INTEGER 
   FIELD IDTransform AS INTEGER 
   FIELD cFrom       AS CHARACTER 
   FIELD iFromStart  AS INT64     FORMAT "zzz,zzz,zzz,zz9"
   FIELD iFromEnd    AS INT64     FORMAT "zzz,zzz,zzz,zz9"
   FIELD cTo         AS CHARACTER 
   FIELD iToStart    AS INT64     FORMAT "zzz,zzz,zzz,zz9"
   FIELD iToEnd      AS INT64     FORMAT "zzz,zzz,zzz,zz9" 
INDEX indID IS UNIQUE IDRange
INDEX indFrom IS PRIMARY UNIQUE IDTransform cFrom iFromStart iFromEnd.
DEFINE VARIABLE iNewIDRange AS INTEGER NO-UNDO.

DEFINE VARIABLE cSection  AS CHARACTER NO-UNDO. // Section of  input file (Transform, Range)
DEFINE VARIABLE cSeedList AS CHARACTER NO-UNDO.
DEFINE VARIABLE iSeed     AS INTEGER   NO-UNDO.
DEFINE VARIABLE cSeed     AS CHARACTER NO-UNDO.
DEFINE VARIABLE iStart    AS INT64     NO-UNDO.
DEFINE VARIABLE iEnd      AS INT64     NO-UNDO.
    
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
   cInputfile = SUBSTITUTE ("C:\OpenEdge\WRK\AOC&1\input\&2.txt", STRING (iYear, "9999"), STRING (iDay, "99"))
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

   IF TRIM (ttLine.cInputLine) EQ "" THEN DO: 
      cSection = "".
      NEXT.
   END.
   
   /* Parsing */
   /* Where are we ? */
   IF ttLine.cInputLine BEGINS "seeds" THEN 
      cSection = "seeds".
   ELSE IF LOOKUP ("to", ttLine.cInputLine, "-") NE 0 THEN 
      cSection = "transform".
   ELSE 
      cSection = "range".
   
   CASE cSection:
      WHEN "seeds" THEN DO:
         ASSIGN 
            cSeedList = TRIM (ENTRY (2, ttLine.cInputLine, ":"))
         .
      END.
      WHEN "transform" THEN DO:
         iNewIDTransform = iNewIDTransform + 1.
         CREATE ttTransform.
         ASSIGN 
            ttTransform.IDTransform = iNewIDTransform
            /* seed-to-soil map: */
            ttTransform.cFrom = ENTRY (1, ENTRY (1, ttLine.cInputLine, " "), "-")
            ttTransform.cTo   = ENTRY (3, ENTRY (1, ttLine.cInputLine, " "), "-")
         .
      END.
      WHEN "range" THEN DO:
         iNewIDRange = iNewIDRange + 1.
         CREATE ttRange.
         ASSIGN 
            ttRange.IDRange     = iNewIDRange 
            ttRange.IDTransform = ttTransform.IDTransform
            ttRange.cFrom       = ttTransform.cFrom
            ttRange.cTo         = ttTransform.cTo
            ttRange.iFromStart  = INT64 (ENTRY (2, ttLine.cInputLine, " "))
            ttRange.iFromEnd    = ttRange.iFromStart + INT64 (ENTRY (3, ttLine.cInputLine, " ")) - 1
            ttRange.iToStart    = INT64 (ENTRY (1, ttLine.cInputLine, " "))
            ttRange.iToEnd      = ttRange.iToStart +  INT64 (ENTRY (3, ttLine.cInputLine, " ")) - 1
         .        
      END.   
   END.
          
END. /* ReadBlock: */

IF lvlShow THEN DO:
   RUN sy\win\wbrowsett.w
      (INPUT TEMP-TABLE ttTransform:HANDLE).
   RUN sy\win\wbrowsett.w
      (INPUT TEMP-TABLE ttRange:HANDLE).
END.

IF lPart[1] THEN DO:
   /* Process Part One */
   iSolution = 0.

   /* Calcolate Solution for Part 1 */
   DO iSeed = 1 TO NUM-ENTRIES (cSeedList, " ").
      cSeed = ENTRY (iSeed, cSeedList, " ").
      iStart = INT64 (cSeed).
      
      RUN getEndValue
         (INPUT  "seed",     // Start material
          INPUT  "location", // End material 
          INPUT  iStart,
          OUTPUT iEnd).

      IF iSolution EQ 0
      OR iSolution GT iEnd THEN 
         iSolution = iEnd. 
   END.
            
   OUTPUT TO "clipboard".
   PUT UNFORMATTED iSolution SKIP.
   OUTPUT CLOSE.
   MESSAGE 
      SUBSTITUTE ("Solution: &1.", 
         iSolution) SKIP (1)
      SUBSTITUTE ("Found solution in &1 msecs.", ETIME)
   VIEW-AS ALERT-BOX TITLE " 2023 - Day 05 - Part One".
   
END. /* Process Part One */

IF lPart[2] THEN DO:
   /* Process Part Two */
   iSolution = 0.

   IF lvlShow THEN DO:

   END.

   /* Calcolate Solution for Part 2 */
     
        
   OUTPUT TO "clipboard".
   PUT UNFORMATTED iSolution SKIP.
   OUTPUT CLOSE.

   MESSAGE 
      SUBSTITUTE ("Solution: &1.", 
         iSolution) SKIP (1)
      SUBSTITUTE ("Found solution in &1 msecs.", ETIME)
   VIEW-AS ALERT-BOX TITLE " 2023 - Day 05 - Part Two".
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

PROCEDURE getEndValue:
/*------------------------------------------------------------------------------
 Purpose: Return the value of the end material based on the input value
 Notes:
------------------------------------------------------------------------------*/
DEFINE INPUT  PARAMETER ipcStart      AS CHARACTER NO-UNDO.
DEFINE INPUT  PARAMETER ipcEnd        AS CHARACTER NO-UNDO.
DEFINE INPUT  PARAMETER ipiStartValue AS INT64     NO-UNDO.
DEFINE OUTPUT PARAMETER opiEndValue   AS INT64     NO-UNDO.

DEFINE BUFFER ttTransform FOR ttTransform.
DEFINE BUFFER ttRange     FOR ttRange.

DEFINE VARIABLE iNewStartValue AS INT64 NO-UNDO.

   FIND FIRST ttTransform
   WHERE ttTransform.cFrom EQ ipcStart.
   
   FIND FIRST ttRange
   WHERE ttRange.IDTransform EQ ttTransform.IDTransform
   AND   ttRange.iFromStart  LE ipiStartValue
   AND   ttRange.iFromEnd    GE ipiStartValue NO-ERROR.
   IF AVAILABLE ttRange THEN
      iNewStartValue = ttRange.iToStart + (ipiStartValue - ttRange.iFromStart).
   ELSE
      iNewStartValue = ipiStartValue.
      
   IF ttTransform.cTo EQ ipcEnd THEN 
      opiEndValue = iNewStartValue.
   ELSE
      RUN getEndValue
         (INPUT  ttTransform.cTo,
          INPUT  ipcEnd,
          INPUT  iNewStartValue,
          OUTPUT opiEndValue).

END PROCEDURE. /* getEndValue */

