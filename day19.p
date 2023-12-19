
/*------------------------------------------------------------------------
    File        : day19.p
    Purpose     : Solve Day 19 of Advent of Code 2023

    Syntax      :

    Description : Solution of Advent of Code 2023 - Day 19

    Author(s)   : Wim van der Ham (WITS)
    Created     : Tue Dec 19 21:52:13 CET 2023
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
DEFINE VARIABLE iDay         AS INTEGER   NO-UNDO INITIAL 19.
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
    
DEFINE VARIABLE cSection     AS CHARACTER NO-UNDO.
DEFINE VARIABLE cCode        AS CHARACTER NO-UNDO.
DEFINE VARIABLE lcProgram    AS LONGCHAR  NO-UNDO.
DEFINE VARIABLE cProgramFile AS CHARACTER NO-UNDO.
DEFINE VARIABLE hProgram     AS HANDLE    NO-UNDO.
DEFINE VARIABLE intX         AS INTEGER   NO-UNDO.
DEFINE VARIABLE intM         AS INTEGER   NO-UNDO.
DEFINE VARIABLE intA         AS INTEGER   NO-UNDO.
DEFINE VARIABLE intS         AS INTEGER   NO-UNDO.
DEFINE VARIABLE fnCall       AS CHARACTER NO-UNDO.
DEFINE VARIABLE cReturnValue AS CHARACTER NO-UNDO.
    
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

ASSIGN 
   cSection     = "Functions"
   cProgramFile = "day19fns.p"
.

FILE-INFO:FILE-NAME = cProgramFile.
IF FILE-INFO:FILE-TYPE NE ? THEN DO:
   OS-DELETE VALUE (FILE-INFO:FULL-PATHNAME).
END.
ELSE DO:
   MESSAGE cProgramFile "not found"
   VIEW-AS ALERT-BOX.
END.

/* Read Input into Temp-table */
ReadBlock:
DO iLine = 1 TO NUM-ENTRIES (lcInput, "~n"):
   cLine = TRIM (ENTRY (iLine, lcInput, "~n")).

   IF cLine EQ "" THEN DO:
      /* Empty Line */
      IF cSection EQ "Functions" THEN 
         cSection = "Input".
      NEXT.
   END.
      
   CREATE ttLine.
   ASSIGN 
      ttLine.IDLine     = iLine
      ttLine.cInputLine = cLine
   .
   
   IF cSection EQ "Functions" THEN DO:
      RUN addFunction
         (INPUT  ttLine.cInputLine,
          OUTPUT cCode).
      lcProgram = lcProgram + "~n~n" + cCode.    
   END.
   ELSE DO:
      FILE-INFO:FILE-NAME = cProgramFile.
      IF FILE-INFO:FILE-TYPE EQ ? THEN DO:
         /* Create Program */
         COPY-LOB 
            FROM OBJECT lcProgram 
            TO   FILE   cProgramFile.
         RUN VALUE (cProgramFile) PERSISTENT SET hProgram.             
      END. /* Create Program */
      
      RUN getInputValues
         (INPUT  ttLine.cInputLine,
          OUTPUT intX,
          OUTPUT intM,
          OUTPUT intA,
          OUTPUT intS).

      /* Start with function 'in' */  
      fnCall = "fnin".
      fnCallBlock:
      REPEAT:
         cReturnValue = DYNAMIC-FUNCTION (fnCall IN hProgram, intX, intM, intA, intS).
         IF lvlDebug THEN DO:
            MESSAGE SUBSTITUTE ("&1 (&2, &3, &4, &5): &6", fnCall, intX, intM, intA, intS, cReturnValue)
            VIEW-AS ALERT-BOX.
         END.
         IF cReturnValue EQ "R" THEN
            /* Rejected */
            LEAVE fnCallBlock.
         IF cReturnValue EQ "A" THEN DO:
            /* Accepted */
            iSolution = iSolution + (intX + intM + intA + intS).
            LEAVE fnCallBlock.
         END.
         /* Continue the Workflow */
         fnCall = SUBSTITUTE ("fn&1", cReturnValue).
      END.
      
   END.
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
      SUBSTITUTE ("Solution: &1.", iSolution) SKIP (1)
      SUBSTITUTE ("Found solution in &1 msecs.", ETIME)
   VIEW-AS ALERT-BOX TITLE " 2023 - Day 19 - Part One".
   
   IF lvlShow THEN DO:
      RUN sy\win\wbrowsett.w
         (INPUT TEMP-TABLE ttLine:HANDLE).
   END.            
END. /* Process Part One */

IF lPart[2] THEN DO:
   /* Process Part Two */
   iSolution = 0.
   
                
   OUTPUT TO "clipboard".
   PUT UNFORMATTED iSolution SKIP.
   OUTPUT CLOSE.

   MESSAGE 
      SUBSTITUTE ("Solution: &1.", iSolution) SKIP (1)
      SUBSTITUTE ("Found solution in &1 msecs.", ETIME)
   VIEW-AS ALERT-BOX TITLE " 2023 - Day 19 - Part Two".
   
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

PROCEDURE addFunction:
/*------------------------------------------------------------------------------
 Purpose: Transform input string in a function
 Notes:   Sample input:
          brn{a>3342:A,R}
          
          
      
------------------------------------------------------------------------------*/
DEFINE INPUT  PARAMETER ipcFunction AS CHARACTER NO-UNDO.
DEFINE OUTPUT PARAMETER opcCode     AS CHARACTER NO-UNDO.

DEFINE VARIABLE cTemplateStart AS CHARACTER NO-UNDO.
DEFINE VARIABLE cTemplateEnd   AS CHARACTER NO-UNDO.
DEFINE VARIABLE cName          AS CHARACTER NO-UNDO.
DEFINE VARIABLE iRule          AS INTEGER   NO-UNDO.
DEFINE VARIABLE cRule          AS CHARACTER NO-UNDO.
DEFINE VARIABLE cFunctionStart AS CHARACTER NO-UNDO.
DEFINE VARIABLE cFunctionBody  AS CHARACTER NO-UNDO.
DEFINE VARIABLE cFunctionEnd   AS CHARACTER NO-UNDO.
DEFINE VARIABLE iOperator      AS INTEGER   NO-UNDO.
DEFINE VARIABLE cOperators     AS CHARACTER NO-UNDO INITIAL "<>".
DEFINE VARIABLE cOperator      AS CHARACTER NO-UNDO.
DEFINE VARIABLE cTest          AS CHARACTER NO-UNDO.
DEFINE VARIABLE cNext          AS CHARACTER NO-UNDO.
DEFINE VARIABLE iPart          AS INTEGER   NO-UNDO.
DEFINE VARIABLE cPart          AS CHARACTER NO-UNDO.

   ASSIGN 
      cTemplateStart = "FUNCTION fn&2 RETURNS CHARACTER (x AS INT, m AS INT, a AS INT, s AS INT):" + "~n" +
                       "/*------------------------------------------------------------------------------" + "~n" +
                       "Purpose: Workflow implemenation for '&2'" + "~n" +
                       "Notes:   &1" + "~n" +
                       "------------------------------------------------------------------------------*/"
      cTemplateEnd   = "END FUNCTION."
   .                       

   ASSIGN 
      cName = ENTRY (1, ipcFunction, "~{")
   .
   
   
   ASSIGN 
      cFunctionStart = SUBSTITUTE (cTemplateStart,
                                   ipcFunction,
                                   cName)
   .
   
   ipcFunction = ENTRY (1, SUBSTRING (ipcFunction, INDEX (ipcFunction, "~{") + 1), "}").
   
   opcCode = cFunctionStart.
   
   DO iPart = 1 TO NUM-ENTRIES (ipcFunction):
      cPart = ENTRY (iPart, ipcFunction).
      
      IF NUM-ENTRIES (cPart, ":") EQ 2 THEN DO:
         /* Add spaces around operator */
         DO iOperator = 1 TO LENGTH (cOperators):
            cOperator = SUBSTRING (cOperators, iOperator, 1).
            cPart = REPLACE (cPart, cOperator, " " + cOperator + " ").
         END.
         
         ASSIGN
            cTest = ENTRY (1, cPart, ":") 
            cNext = ENTRY (2, cPart, ":")
         .
         
         cFunctionBody = SUBSTITUTE ("   IF &1 THEN RETURN &2.", cTest, QUOTER (cNext)).
      END.
      ELSE DO:
         cNext = cPart.         
         cFunctionBody = SUBSTITUTE ("   RETURN &1.", QUOTER (cNext)).   
      END.
      
      opcCode = opcCode + "~n" + cFunctionBody.
   END.
   
   opcCode = opcCode + "~n" + cTemplateEnd. 

END PROCEDURE.

PROCEDURE getInputValues:
/*------------------------------------------------------------------------------
 Purpose: Extract the values for x, m, a, s from an input line
 Notes:   Sample input line:
          {x=1065,m=825,a=1002,s=2038}
------------------------------------------------------------------------------*/
DEFINE INPUT  PARAMETER ipcValueList AS CHARACTER NO-UNDO.
DEFINE OUTPUT PARAMETER opiX         AS INTEGER   NO-UNDO.
DEFINE OUTPUT PARAMETER opiM         AS INTEGER   NO-UNDO.
DEFINE OUTPUT PARAMETER opiA         AS INTEGER   NO-UNDO.
DEFINE OUTPUT PARAMETER opiS         AS INTEGER   NO-UNDO.

DEFINE VARIABLE iPair     AS INTEGER   NO-UNDO.
DEFINE VARIABLE cPair     AS CHARACTER NO-UNDO.
DEFINE VARIABLE cVariable AS CHARACTER NO-UNDO.
DEFINE VARIABLE iValue    AS INTEGER NO-UNDO.

   /* Remove {} characters */
   ipcValueList = TRIM (ipcValueList, "~{}").

   /* Loop through the variable=value pairs */
   DO iPair = 1 TO NUM-ENTRIES (ipcValueList):
      cPair = ENTRY (iPair, ipcValueList).
      ASSIGN 
         cVariable = ENTRY (1, cPair, "=")
         iValue    = INTEGER (ENTRY (2, cPair, "="))
      .
      CASE cVariable:
         WHEN "x" THEN opiX = iValue.
         WHEN "m" THEN opiM = iValue.
         WHEN "a" THEN opiA = iValue.
         WHEN "s" THEN opiS = iValue.
      END CASE.   
   END.
   
END PROCEDURE.

/* ************************  Function Implementations ***************** */

FUNCTION brn RETURNS CHARACTER (x AS INTEGER, m AS INTEGER, a AS INTEGER, s AS INTEGER):
/*------------------------------------------------------------------------------
 Purpose: Workflow implemenation for 'brn'
 Notes:   brn{a>3342:A,R}
------------------------------------------------------------------------------*/   
DEFINE VARIABLE cResult AS CHARACTER NO-UNDO.

   IF a > 3342 THEN RETURN "A".
   
   RETURN "R".
   
END FUNCTION.

