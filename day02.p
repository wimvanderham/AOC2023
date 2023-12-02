
/*------------------------------------------------------------------------
    File        : day02.p
    Purpose     : Solve Day 02 of Advent of Code 2023

    Syntax      :

    Description : Solution for Day 02 of Advent of Code 2023

    Author(s)   : Wim van der Ham (WITS)
    Created     : Fri Dec 01 23:47:24 CET 2023
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
DEFINE VARIABLE iDay         AS INTEGER   NO-UNDO INITIAL 2.
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
   FIELD IDLine      AS INTEGER 
   FIELD cInputLine  AS CHARACTER FORMAT "X(80)"
   FIELD iGameID     AS INTEGER 
   FIELD iMaxNrRed   AS INTEGER 
   FIELD iMaxNrGreen AS INTEGER  
   FIELD iMaxNrBlue  AS INTEGER 
   FIELD lPossible   AS LOGICAL 
   FIELD iPower      AS INTEGER 
INDEX indLine IS UNIQUE IDLine.

DEFINE VARIABLE iMaxRed   AS INTEGER NO-UNDO.
DEFINE VARIABLE iMaxGreen AS INTEGER NO-UNDO.
DEFINE VARIABLE iMaxBlue  AS INTEGER NO-UNDO.

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
   lcInput = "Game 1: 3 blue, 4 red; 1 red, 2 green, 6 blue; 2 green~nGame 2: 1 blue, 2 green; 3 green, 4 blue, 1 red; 1 green, 1 blue~nGame 3: 8 green, 6 blue, 20 red; 5 blue, 4 red, 13 green; 5 green, 1 red~nGame 4: 1 green, 3 red, 6 blue; 3 green, 6 red; 3 green, 15 blue, 14 red~nGame 5: 6 red, 1 blue, 3 green; 2 blue, 1 red, 2 green".
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

   RUN getMaxNumbers
      (INPUT  ttLine.cInputLine,
       OUTPUT ttLine.iGameID,
       OUTPUT ttLine.iMaxNrRed,
       OUTPUT ttLine.iMaxNrGreen,
       OUTPUT ttLine.iMaxNrBlue).
      
END. /* ReadBlock: */

IF lPart[1] THEN DO:
   /* Process Part One */
   /*
   Determine which games would have been possible if the bag had been loaded with only 
   12 red cubes, 13 green cubes, and 14 blue cubes. 
   What is the sum of the IDs of those games?
   */      
   ASSIGN 
      iMaxRed   = 12
      iMaxGreen = 13
      iMaxBlue  = 14
   .
   
   FOR EACH ttLine:
      IF  ttLine.iMaxNrRed   LE iMaxRed
      AND ttLine.iMaxNrGreen LE iMaxGreen
      AND ttLine.iMaxNrBlue  LE iMaxBlue THEN
         ttLine.lPossible = TRUE.
      ELSE 
         ttLine.lPossible = FALSE.
         
      IF ttLine.lPossible THEN 
         iSolution = iSolution + ttLine.iGameID.
   END.
         
   IF lvlShow THEN DO:
      RUN sy\win\wbrowsett.w
         (INPUT TEMP-TABLE ttLine:HANDLE).
   END.
         
   OUTPUT TO "clipboard".
   PUT UNFORMATTED iSolution SKIP.
   OUTPUT CLOSE.
   MESSAGE 
      SUBSTITUTE ("Solution: &1.", 
         iSolution) SKIP (1)
      SUBSTITUTE ("Found solution in &1 msecs.", ETIME)
   VIEW-AS ALERT-BOX TITLE " 2023 - Day 02 - Part One".
END. /* Process Part One */

IF lPart[2] THEN DO:
   /* Process Part Two */

   iSolution = 0.
   FOR EACH ttLine:
      ASSIGN
         ttLine.iPower = ttLine.iMaxNrRed * ttLine.iMaxNrGreen * ttLine.iMaxNrBlue
      .
      
      iSolution = iSolution + ttLine.iPower. 
       
   END.
   
   OUTPUT TO "clipboard".
   PUT UNFORMATTED iSolution SKIP.
   OUTPUT CLOSE.

   MESSAGE 
      SUBSTITUTE ("Solution: &1.", 
         iSolution) SKIP (1)
      SUBSTITUTE ("Found solution in &1 msecs.", ETIME)
   VIEW-AS ALERT-BOX TITLE " 2023 - Day 02 - Part Two".
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

PROCEDURE getMaxNumbers:
/*------------------------------------------------------------------------------
 Purpose: Extract maximum numbers from inputline
 Notes:   Sample input line:
          Game 1: 3 blue, 4 red; 1 red, 2 green, 6 blue; 2 green
------------------------------------------------------------------------------*/
DEFINE INPUT  PARAMETER ipcInputLine AS CHARACTER NO-UNDO.
DEFINE OUTPUT PARAMETER opiGameID    AS INTEGER   NO-UNDO.
DEFINE OUTPUT PARAMETER opiMaxRed    AS INTEGER   NO-UNDO.
DEFINE OUTPUT PARAMETER opiMaxGreen  AS INTEGER   NO-UNDO.
DEFINE OUTPUT PARAMETER opiMaxBlue   AS INTEGER   NO-UNDO.

DEFINE VARIABLE cGame     AS CHARACTER NO-UNDO.
DEFINE VARIABLE cExtracts AS CHARACTER NO-UNDO.

DEFINE VARIABLE iExtract  AS INTEGER NO-UNDO.
DEFINE VARIABLE cExtract  AS CHARACTER NO-UNDO.
DEFINE VARIABLE iNrRed    AS INTEGER NO-UNDO.
DEFINE VARIABLE iNrGreen  AS INTEGER NO-UNDO.
DEFINE VARIABLE iNrBlue   AS INTEGER NO-UNDO.
DEFINE VARIABLE cColor    AS CHARACTER NO-UNDO.
DEFINE VARIABLE iCubes    AS INTEGER NO-UNDO.
DEFINE VARIABLE cCubes    AS CHARACTER NO-UNDO.

   IF lvlDebug THEN 
      MESSAGE "Start Parsing" SKIP 
      ipcInputLine
      VIEW-AS ALERT-BOX.

   ASSIGN 
      cGame     = TRIM (ENTRY (1, ipcInputLine, ":"))
      cExtracts = TRIM (ENTRY (2, ipcInputLine, ":"))
   .
   
   ASSIGN 
      opiGameID = INTEGER (ENTRY (2, cGame, " "))
   .
   
   DO iExtract = 1 TO NUM-ENTRIES (cExtracts, ";"):
      cExtract = TRIM (ENTRY (iExtract, cExtracts, ";")).
      
      DO iCubes = 1 TO NUM-ENTRIES (cExtract):
          /*  Game 1: 3 blue, 4 red; 1 red, 2 green, 6 blue; 2 green */
          cCubes = TRIM (ENTRY (iCubes, cExtract)).
          ASSIGN 
            cColor = ENTRY (2, cCubes, " ")
         .
         CASE cColor:
            WHEN "red"   THEN DO: 
               iNrRed = INTEGER (ENTRY (1, cCubes, " ")).
               IF iNrRed GT opiMaxRed THEN 
                  opiMaxRed = iNrRed.
            END.
            WHEN "green" THEN DO:
               iNrGreen = INTEGER (ENTRY (1, cCubes, " ")).
               IF iNrGreen GT opiMaxGreen THEN 
                  opiMaxGreen = iNrGreen.
            END.
            WHEN "blue"  THEN DO:
               iNrBlue  = INTEGER (ENTRY (1, cCubes, " ")).
               IF iNrBlue GT opiMaxBlue THEN 
                  opiMaxBlue = iNrBlue.
            END.
         END CASE.
      END.
   END.
   
END PROCEDURE.


/* ************************  Function Implementations ***************** */

