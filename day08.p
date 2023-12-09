
/*------------------------------------------------------------------------
    File        : day08.p
    Purpose     : Solve Day 08 of Advent of Code 2023

    Syntax      :

    Description : Solution for Day 08 of Advent of Code 2023

    Author(s)   : Wim van der Ham (WITS)
    Created     : Fri Dec 08 07:25:31 CET 2023
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
DEFINE VARIABLE iDay         AS INTEGER   NO-UNDO INITIAL 8.
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
INDEX indLine IS UNIQUE IDLine.

DEFINE TEMP-TABLE ttNode
   FIELD IDNode     AS INTEGER 
   FIELD Node       AS CHARACTER 
   FIELD LNode      AS CHARACTER 
   FIELD RNode      AS CHARACTER
   // Extra fields for Part Two
   FIELD isA        AS LOGICAL // Is last character A?
   FIELD isZ        AS LOGICAL // Is last character B?  
INDEX indID   IS UNIQUE IDNode
INDEX indNode IS UNIQUE PRIMARY Node.
DEFINE VARIABLE iNewIDNode AS INTEGER NO-UNDO.
DEFINE BUFFER ttNextNode FOR ttNode.

DEFINE TEMP-TABLE ttGhostNode
   FIELD IDGhostNode  AS INTEGER
   FIELD StartNode    AS CHARACTER 
   FIELD CurrentNode  AS CHARACTER 
   FIELD ExitAfter    AS INTEGER
   FIELD Shown        AS LOGICAL  
INDEX indID IS UNIQUE IDGhostNode.
DEFINE VARIABLE iNewIDGhostNode AS INTEGER NO-UNDO.
 
DEFINE VARIABLE cInstructions AS CHARACTER NO-UNDO.
DEFINE VARIABLE iInstruction  AS INTEGER   NO-UNDO.
DEFINE VARIABLE cInstruction  AS CHARACTER NO-UNDO.
DEFINE VARIABLE cStartNode    AS CHARACTER NO-UNDO.
DEFINE VARIABLE cEndNode      AS CHARACTER NO-UNDO.
DEFINE VARIABLE cCurrentNode  AS CHARACTER NO-UNDO.
DEFINE VARIABLE lAllZNodes    AS LOGICAL   NO-UNDO.
 
/* ********************  Preprocessor Definitions  ******************** */

{AOC_session.i}

/* ************************  Function Prototypes ********************** */
/* ************************  Function Implementations ***************** */
FUNCTION gcd RETURNS INT64 (pX AS INT64, pY AS INT64):
  IF py = 0 THEN RETURN pX.
  RETURN gcd(pY, pX MOD pY).
END FUNCTION.

FUNCTION lcm RETURNS INT64 (pX AS INT64, pY AS INT64):
  RETURN INT64((pX * pY) / gcd(pX, pY)).
END FUNCTION.


/* ***************************  Main Block  *************************** */

DISPLAY
   SUBSTITUTE ("Year &1 Day &2", iYear, iDay) FORMAT "X(16)" NO-LABELS SKIP
   lOpenURL  LABEL "Open URL?"       VIEW-AS TOGGLE-BOX SKIP
   lDownload LABEL "Download Input?" VIEW-AS TOGGLE-BOX SKIP   
   lPart[1]  LABEL "Solve Part 1?"   VIEW-AS TOGGLE-BOX SKIP
   lPart[2]  LABEL "Solve Part 2?"   VIEW-AS TOGGLE-BOX SKIP 
   lvlDebug  LABEL "Debug?"          VIEW-AS TOGGLE-BOX SKIP 
   lvlShow   LABEL "Show?"           VIEW-AS TOGGLE-BOX SKIP
   SKIP (2)
   "Progress:" SKIP 
   edProgress VIEW-AS EDITOR SIZE 76 BY 10 LARGE NO-LABELS 
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

   /* Parsing */
   IF iLine EQ 1 THEN DO:
      /* Line with instructions */
      cInstructions = ttLine.cInputLine.
   END.
   ELSE DO:
      /* Node Line */      
      iNewIDNode = iNewIDNode + 1.
      CREATE ttNode.
      ASSIGN 
         ttNode.IDNode = iNewIDNode
         ttNode.Node   = TRIM (ENTRY (1, ttLine.cInputLine, "="))
         ttNode.LNode  = TRIM (ENTRY (1, TRIM (ENTRY (2, ttLine.cInputLine, "="))), "(")
         ttNode.RNode  = TRIM (ENTRY (2, TRIM (ENTRY (2, ttLine.cInputLine, "="))), ") ")
      .
      ASSIGN 
         ttNode.isA = SUBSTRING (ttNode.Node, LENGTH (ttNode.Node), 1) EQ "A"
         ttNode.isZ = SUBSTRING (ttNode.Node, LENGTH (ttNode.Node), 1) EQ "Z"
      .
   END. /* Node Line */             
END. /* ReadBlock: */

IF lvlShow THEN DO:
   RUN sy\win\wbrowsett.w
      (INPUT TEMP-TABLE ttNode:HANDLE).   
END.

IF lPart[1] THEN DO:
   /* Process Part One */
   iSolution = 0.

   /* Calcolate Solution for Part 1 */
   ASSIGN 
      cStartNode   = "AAA"
      cEndNode     = "ZZZ"
      cCurrentNode = cStartNode
      iInstruction = 1
   .
   
   DO WHILE cCurrentNode NE cEndNode:
      iSolution = iSolution + 1.
      FIND ttNode WHERE ttNode.Node EQ cCurrentNode NO-ERROR.
      IF NOT AVAILABLE ttNode THEN DO:
         MESSAGE SUBSTITUTE ("Couldn't find node '&1'.", cCurrentNode)
         VIEW-AS ALERT-BOX.
         RUN sy\win\wbrowsett.w
            (INPUT TEMP-TABLE ttNode:HANDLE).
      END.
      cInstruction = SUBSTRING (cInstructions, iInstruction, 1).
      CASE cInstruction:
         WHEN "L" THEN 
            cCurrentNode = ttNode.LNode.
         WHEN "R" THEN
            cCurrentNode = ttNode.RNode.
      END CASE.

/*      IF lvlDebug THEN DO:                                                                                           */
/*         MESSAGE SUBSTITUTE ("Step &1. From '&2' --(&4)--> '&3'", iSolution, ttNode.Node, cCurrentNode, cInstruction)*/
/*         VIEW-AS ALERT-BOX.                                                                                          */
/*      END.                                                                                                           */
            
      IF iInstruction LT LENGTH (cInstructions) THEN 
         iInstruction = iInstruction + 1.
      ELSE 
         iInstruction = 1.
   END.
   
   OUTPUT TO "clipboard".
   PUT UNFORMATTED iSolution SKIP.
   OUTPUT CLOSE.
   MESSAGE 
      SUBSTITUTE ("Solution: &1.", iSolution) SKIP (1)
      SUBSTITUTE ("Found solution in &1 msecs.", ETIME)
   VIEW-AS ALERT-BOX TITLE " 2023 - Day 08 - Part One".
   
END. /* Process Part One */

IF lPart[2] THEN DO:
   /* Process Part Two */
   iSolution = 0.
   
   /* Calcolate Solution for Part 2 */
   FOR EACH ttNode WHERE ttNode.isA EQ TRUE:
      iNewIDGhostNode = iNewIDGhostNode + 1.
      CREATE ttGhostNode.
      ASSIGN 
         ttGhostNode.IDGhostNode = iNewIDGhostNode
         ttGhostNode.StartNode   = ttNode.Node
         ttGhostNode.CurrentNode = ttGhostNode.StartNode
      .
   END.
   IF lvlShow THEN DO:
      RUN sy\win\wbrowsett.w
         (INPUT TEMP-TABLE ttGhostNode:HANDLE).
   END.
   
   ASSIGN
      lAllZNodes   = FALSE  
      iInstruction = 1
   .
   
   DO WHILE lAllZNodes EQ FALSE:
      iSolution = iSolution + 1.
      
      cInstruction = SUBSTRING (cInstructions, iInstruction, 1).
      FOR EACH ttGhostNode,
      FIRST ttNode WHERE ttNode.Node EQ ttGhostNode.CurrentNode:
         CASE cInstruction:
            WHEN "L" THEN 
               ttGhostNode.CurrentNode = ttNode.LNode.
            WHEN "R" THEN
               ttGhostNode.CurrentNode = ttNode.RNode.
         END CASE.
         FIND ttNextNode WHERE ttNextNode.Node EQ ttGhostNode.CurrentNode.
         IF ttNextNode.isZ THEN DO:
            ttGhostNode.ExitAfter = iSolution.
            ttGhostNode.Shown     = FALSE.
         END. 
            
         IF lvlShow THEN DO:
            IF ttGhostNode.Shown EQ FALSE THEN DO: 
               edProgress:INSERT-STRING (SUBSTITUTE ("Step &1. IDGhostNode &5. From '&2' --(&4)--> '&3' On Z Node? &6~n",
                                                     iSolution, 
                                                     ttNode.Node, 
                                                     ttGhostNode.CurrentNode, 
                                                     cInstruction,
                                                     ttGhostNode.IDGhostNode,
                                                     ttNextNode.isZ)).
               ttGhostNode.Shown = TRUE.
            END.                                                     
            PROCESS EVENTS.
         END.
      END.

      
      IF iInstruction LT LENGTH (cInstructions) THEN 
         iInstruction = iInstruction + 1.
      ELSE 
         iInstruction = 1.

      lAllZNodes = TRUE.
      FOR EACH ttGhostNode
      WHERE ttGhostNode.ExitAfter EQ 0:
         lAllZNodes = FALSE.
      END.
         
   END.
   
   IF lvlShow THEN DO:
      RUN sy\win\wbrowsett.w
         (INPUT TEMP-TABLE ttGhostNode:HANDLE).
   END.         

   iSolution = 0.
   FOR EACH ttGhostNode:
      IF iSolution = 0 THEN 
         iSolution = ttGhostNode.ExitAfter.
      ELSE 
         iSolution = lcm(iSolution, ttGhostNode.ExitAfter).
   END.
       
   OUTPUT TO "clipboard".
   PUT UNFORMATTED iSolution SKIP.
   OUTPUT CLOSE.

   MESSAGE 
      SUBSTITUTE ("Solution: &1.", iSolution) SKIP (1)
      SUBSTITUTE ("Found solution in &1 msecs.", ETIME)
   VIEW-AS ALERT-BOX TITLE " 2023 - Day 08 - Part Two".
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



