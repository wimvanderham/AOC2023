
/*------------------------------------------------------------------------
    File        : day25.p
    Purpose     : Solve Advent of Code 2023 - Day 25

    Syntax      :

    Description : Solution for Advent of Code 2023 - Day 25

    Author(s)   : Wim van der Ham (WITS)
    Created     : Mon Dec 25 13:20:52 CET 2023
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
DEFINE VARIABLE iDay         AS INTEGER   NO-UNDO INITIAL 25.
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
    
DEFINE TEMP-TABLE ttConnection
   FIELD IDConnection   AS INTEGER 
   FIELD FromComponent  AS CHARACTER 
   FIELD ToComponent    AS CHARACTER     
   FIELD ComponentGroup AS INTEGER 
   FIELD PathCount      AS INTEGER 
INDEX indID IS UNIQUE IDConnection
INDEX indConnection IS PRIMARY FromComponent ToComponent
INDEX indBackConnection ToComponent FromComponent
INDEX indCount PathCount DESCENDING.
DEFINE VARIABLE iNewIDConnection AS INTEGER NO-UNDO.
   
DEFINE TEMP-TABLE ttComponent
   FIELD IDComponent    AS INTEGER 
   FIELD Component      AS CHARACTER 
   FIELD NodesOut       AS INTEGER 
   FIELD ComponentGroup AS INTEGER 
INDEX indID IS UNIQUE IDComponent
INDEX indComponent IS PRIMARY Component.
DEFINE VARIABLE iNewIDComponent AS INTEGER NO-UNDO.

DEFINE BUFFER ttNextComponent FOR ttComponent.

DEFINE TEMP-TABLE ttPath
   FIELD IDPath AS INTEGER 
   FIELD FromComponent AS CHARACTER 
   FIELD ToComponent   AS CHARACTER 
   FIELD Steps         AS INTEGER 
   FIELD Path          AS CHARACTER  
INDEX indID IS UNIQUE IDPath
INDEX indComponent IS UNIQUE PRIMARY FromComponent ToComponent.
DEFINE VARIABLE iNewIDPath AS INTEGER NO-UNDO.

DEFINE BUFFER ttNextPath FOR ttPath.

DEFINE VARIABLE cFromComponent    AS CHARACTER NO-UNDO.
DEFINE VARIABLE cListToComponents AS CHARACTER NO-UNDO.
DEFINE VARIABLE iComponent        AS INTEGER   NO-UNDO.
DEFINE VARIABLE lvlFound          AS LOGICAL   NO-UNDO.
DEFINE VARIABLE cFrom             AS CHARACTER NO-UNDO.
DEFINE VARIABLE cNext             AS CHARACTER NO-UNDO.
DEFINE VARIABLE iSteps            AS INTEGER   NO-UNDO. 
DEFINE VARIABLE iPath             AS INTEGER   NO-UNDO.
DEFINE VARIABLE iNrGroups         AS INTEGER   NO-UNDO.
   
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
   IF FILE-INFO:FILE-TYPE NE ? THEN DO: 
      COPY-LOB FROM FILE cInputFile TO OBJECT lcInput.
   END.
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

   IF cLine EQ "" THEN DO:
      /* Empty Line */
      NEXT.
   END.
      
   CREATE ttLine.
   ASSIGN 
      ttLine.IDLine     = iLine
      ttLine.cInputLine = cLine
   .

   ASSIGN 
      cFromComponent    = ENTRY (1, ttLine.cInputLine, ":")
      cListToComponents = TRIM (ENTRY (2, ttLine.cInputLine, ":"))
   .

   FIND ttComponent 
   WHERE ttComponent.Component EQ cFromComponent NO-ERROR.
   IF NOT AVAILABLE ttComponent THEN DO:
      iNewIDComponent = iNewIDComponent + 1.
      CREATE ttComponent.
      ASSIGN 
         ttComponent.IDComponent = iNewIDComponent
         ttComponent.Component   = cFromComponent
      .
   END.
        
   DO iComponent = 1 TO NUM-ENTRIES (cListToComponents, " "):
      iNewIDConnection = iNewIDConnection + 1. 
      CREATE ttConnection.
      ASSIGN 
         ttConnection.IDConnection  = iNewIDConnection
         ttConnection.FromComponent = cFromComponent
         ttConnection.ToComponent   = ENTRY (iComponent, cListToComponents, " ")
      .
      FIND  ttNextComponent 
      WHERE ttNextComponent.Component EQ ttConnection.ToComponent NO-ERROR.
      IF NOT AVAILABLE ttNextComponent THEN DO:
         iNewIDComponent = iNewIDComponent + 1.
         CREATE ttNextComponent.
         ASSIGN 
            ttNextComponent.IDComponent = iNewIDComponent
            ttNextComponent.Component   = ttConnection.ToComponent
         .
         ttComponent.NodesOut = ttComponent.NodesOut + 1. 
      END.
      ttNextComponent.NodesOut = ttNextComponent.NodesOut + 1. 
   END.

END. /* ReadBlock: */

IF lvlShow THEN DO:
   FOR EACH ttComponent,
   EACH  ttNextComponent
   WHERE ttNextComponent.IDComponent GT ttComponent.IDComponent:
      ACCUM "" (COUNT).
   END.
   MESSAGE SUBSTITUTE ("Found &1 possible pairs.", STRING ((ACCUM COUNT ""), "zz,zzz,zzz,zz9")) 
   VIEW-AS ALERT-BOX.      
END.

IF lvlShow THEN DO:
   RUN sy\win\wbrowsett.w
      (INPUT TEMP-TABLE ttConnection:HANDLE).
   RUN sy\win\wbrowsett.w
      (INPUT TEMP-TABLE ttComponent:HANDLE). 
END.

IF lPart[1] THEN DO:
   /* Process Part One */
   iSolution = 0.
            
/*   FOR EACH ttComponent,                                                                       */
/*   EACH  ttNextComponent                                                                       */
/*   WHERE ttNextComponent.IDComponent GT ttComponent.IDComponent:                               */
/*      ACCUM "" (COUNT).                                                                        */
/*      IF (ACCUM COUNT "") MOD 1 EQ 0 THEN DO:                                                  */
/*         /* Every combinations, find a path */                                                 */
/*         IF lvlShow THEN DO WITH FRAME fr-Parameters:                                          */
/*            edProgress:INSERT-STRING (SUBSTITUTE ("#&1. Check for a Path from '&2' to '&3'.~n",*/
/*                                                 (ACCUM COUNT ""),                             */
/*                                                 ttComponent.Component,                        */
/*                                                 ttNextComponent.Component)).                  */
/*            PROCESS EVENTS.                                                                    */
/*         END.                                                                                  */
/*                                                                                               */
/*         IF lvlOutput THEN DO:                                                                 */
/*            OUTPUT TO "output\25.txt" APPEND.                                                  */
/*            PUT UNFORMATTED                                                                    */
/*               SUBSTITUTE ("#&1. Check for a Path from '&2' to '&3'.",                         */
/*                           (ACCUM COUNT ""),                                                   */
/*                           ttComponent.Component,                                              */
/*                           ttNextComponent.Component) SKIP.                                    */
/*         END.                                                                                  */
/*                                                                                               */
/*         lvlFound = FALSE.                                                                     */
/*         RUN getPath                                                                           */
/*            (INPUT  ttComponent.Component,                                                     */
/*             INPUT  ttNextComponent.Component).                                                */
/*                                                                                               */
/*          IF lvlOutput THEN DO:                                                                */
/*             OUTPUT CLOSE.                                                                     */
/*          END.                                                                                 */
/*      END.                                                                                     */
/*   END.                                                                                        */

   /* BFS from each component do a BFS */
   FOR EACH ttComponent:
      iSteps = 0.      
      cFrom = ttComponent.Component.
      iNewIDPath = iNewIDPath + 1.
      CREATE ttPath.
      ASSIGN 
         ttPath.IDPath        = iNewIDPath
         ttPath.FromComponent = cFrom
         ttPath.ToComponent   = cFrom
         ttPath.Steps         = 0
         ttPath.Path          = ttPath.ToComponent
      .
      
      BFSBlock:
      REPEAT:
         FOR EACH ttPath
         WHERE ttPath.FromComponent EQ cFrom
         AND   ttPath.Steps         EQ iSteps:
            ACCUM "" (COUNT).
               
            iSteps = iSteps + 1.
            FOR EACH ttConnection
            WHERE ttConnection.FromComponent EQ ttPath.ToComponent
            OR    ttConnection.ToComponent   EQ ttPath.ToComponent:
               IF ttConnection.FromComponent EQ ttPath.ToComponent THEN 
                  cNext = ttConnection.ToComponent.
               ELSE 
                  cNext = ttConnection.FromComponent.
               FIND  ttNextPath
               WHERE ttNextPath.FromComponent EQ cFrom
               AND   ttNextPath.ToComponent   EQ cNext NO-ERROR.
               IF NOT AVAILABLE ttNextPath THEN DO:
                  iNewIDPath = iNewIDPath + 1.
                  CREATE ttNextPath.
                  ASSIGN 
                     ttNextPath.IDPath        = iNewIDPath
                     ttNextPath.FromComponent = cFrom
                     ttNextPath.ToComponent   = cNext
                     ttNextPath.Steps         = iSteps
                     ttNextPath.Path          = ttPath.Path + "," + cNext
                  .
               END.
            END.
         END.
         IF (ACCUM COUNT "") EQ 0 THEN
            /* No more new connections discovered */
            LEAVE BFSBlock.
      END.
      
   END.
   
   FOR EACH ttPath:
      DO iPath = 1 TO NUM-ENTRIES (ttPath.Path) - 1:
         FIND  ttConnection
         WHERE ttConnection.FromComponent EQ ENTRY (iPath, ttPath.Path)
         AND   ttConnection.ToComponent   EQ ENTRY (iPath + 1, ttPath.Path) NO-ERROR.
         IF NOT AVAILABLE ttConnection THEN
            FIND  ttConnection
            WHERE ttConnection.FromComponent EQ ENTRY (iPath + 1, ttPath.Path)
            AND   ttConnection.ToComponent   EQ ENTRY (iPath, ttPath.Path).
         ASSIGN 
            ttConnection.PathCount = ttConnection.PathCount + 1
         .
      END.
   END.
   
   IF lvlShow THEN DO:
      RUN sy\win\wbrowsett.w
         (INPUT TEMP-TABLE ttConnection:HANDLE).
   END.
   
/*   FOR EACH ttConnection                          */
/*   BY ttConnection.PathCount DESCENDING:          */
/*      /* Do the 3 cuts of the mostly used paths */*/
/*      ACCUM "" (COUNT).                           */
/*         DELETE ttConnection.                     */
/*      IF (ACCUM COUNT "") EQ 3 THEN               */
/*         LEAVE.                                   */
/*   END.                                           */
   
   /* The automatic selection of the connections to cut isn't working (yet)
   ** By examing the connections in the graph, it becomes clear the connections to cut are the following:
   IDConnection   FromComponent  ToComponent ComponentGroup PathCount   Connection  Back Connection
   1591  ljh   tbg   0  12466 ljh-tbg  tbg-ljh
   933   mnh   qnv   0  7270  mnh-qnv  qnv-mnh
   709   mfs   ffv   0  1220  mfs-ffv  ffv-mfs
   */
   FOR EACH ttConnection
   WHERE ttConnection.IDConnection EQ 1591
   OR    ttConnection.IDConnection EQ 933
   OR    ttConnection.IDConnection EQ 709:
      DELETE ttConnection.
   END.
         
   
   RUN createGroup
      (OUTPUT iNrGroups).
   IF iNrGroups EQ 2 THEN DO:      
      
      FOR EACH ttConnection,
      FIRST ttComponent
      WHERE ttComponent.Component EQ ttConnection.FromComponent,
      FIRST ttNextComponent
      WHERE ttNextComponent.Component EQ ttConnection.ToComponent:
         ASSIGN 
            ttComponent.ComponentGroup     = ttConnection.ComponentGroup
            ttNextComponent.ComponentGroup = ttConnection.ComponentGroup
         .
      END.
      
      iSolution = 1.
      FOR EACH ttComponent   
      BREAK 
      BY ttComponent.ComponentGroup:
         ACCUM "" (COUNT BY ttComponent.ComponentGroup).
         IF LAST-OF (ttComponent.ComponentGroup) THEN DO:
            iSolution = iSolution * (ACCUM COUNT BY ttComponent.ComponentGroup "").
            IF lvlShow THEN 
               MESSAGE ttComponent.ComponentGroup iSolution
               VIEW-AS ALERT-BOX.
         END.
      END.
   END.
   ELSE DO:
      MESSAGE "Found" iNrGroups
      VIEW-AS ALERT-BOX.
   END.
   
      
   OUTPUT TO "clipboard".
   PUT UNFORMATTED iSolution SKIP.
   OUTPUT CLOSE.
   MESSAGE 
      SUBSTITUTE ("Solution: &1.", iSolution) SKIP (1)
      SUBSTITUTE ("Found solution in &1 msecs.", ETIME)
   VIEW-AS ALERT-BOX TITLE " 2023 - Day 25 - Part One".
   
   IF lvlShow THEN DO:
      RUN sy\win\wbrowsett.w
         (INPUT TEMP-TABLE ttConnection:HANDLE).
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
   VIEW-AS ALERT-BOX TITLE " 2023 - Day 25 - Part Two".
   
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

PROCEDURE createGroup:
/*------------------------------------------------------------------------------
 Purpose:
 Notes:
------------------------------------------------------------------------------*/
DEFINE OUTPUT PARAMETER opiNrGroups AS INTEGER NO-UNDO.

DEFINE VARIABLE iNewGroup AS INTEGER NO-UNDO.

DEFINE BUFFER ttConnection FOR ttConnection.

   FOR EACH ttConnection
   WHERE ttConnection.ComponentGroup EQ 0:
      iNewGroup = iNewGroup + 1.
      RUN getConnections
         (INPUT iNewGroup,
          INPUT ttConnection.FromComponent).
   END.
   
   FOR EACH ttConnection
   BREAK 
   BY ttConnection.ComponentGroup:
      IF LAST (ttConnection.ComponentGroup) THEN 
         opiNrGroups = ttConnection.ComponentGroup.
   END.
   
END PROCEDURE.

PROCEDURE getConnections:
/*------------------------------------------------------------------------------
 Purpose:
 Notes:
------------------------------------------------------------------------------*/
DEFINE INPUT  PARAMETER ipiGroup         AS INTEGER   NO-UNDO.
DEFINE INPUT  PARAMETER ipcComponentFrom AS CHARACTER NO-UNDO.

DEFINE VARIABLE cNextComponent AS CHARACTER NO-UNDO.

DEFINE BUFFER ttConnection FOR ttConnection.

   FOR EACH ttConnection
   WHERE (ttConnection.FromComponent  EQ ipcComponentFrom OR ttConnection.ToComponent EQ ipcComponentFrom)
   AND   ttConnection.ComponentGroup EQ 0:
      ttConnection.ComponentGroup = ipiGroup.
   
      IF ttConnection.FromComponent EQ ipcComponentFrom THEN
         cNextComponent = ttConnection.ToComponent.
      ELSE 
         cNextComponent = ttConnection.FromComponent.
             
      RUN getConnections
         (INPUT ipiGroup,
          INPUT cNextComponent).
   END.
                                      
END PROCEDURE.

PROCEDURE getPath:
/*------------------------------------------------------------------------------
 Purpose: Find a Path from a start path to an end point
 Notes:
------------------------------------------------------------------------------*/
DEFINE INPUT  PARAMETER ipcPath         AS LONGCHAR  NO-UNDO.
DEFINE INPUT  PARAMETER ipcEndComponent AS CHARACTER NO-UNDO.

DEFINE VARIABLE iPath    AS INTEGER   NO-UNDO.
DEFINE VARIABLE cFrom    AS CHARACTER NO-UNDO.
DEFINE VARIABLE cNext    AS CHARACTER NO-UNDO.
DEFINE VARIABLE cNewPath AS LONGCHAR  NO-UNDO.

DEFINE VARIABLE cTraverseFrom AS CHARACTER NO-UNDO.
DEFINE VARIABLE cTraverseTo   AS CHARACTER NO-UNDO.

DEFINE BUFFER ttConnection FOR ttConnection.
DEFINE BUFFER ttTraversed  FOR ttConnection.

   IF lvlOutput THEN DO:
      PUT UNFORMATTED 
         SUBSTITUTE ("&1: &2", NUM-ENTRIES (ipcPath), ipcPath) SKIP.
   END.
   
   IF lvlFound EQ TRUE THEN 
      RETURN.
      
   cFrom = ENTRY (NUM-ENTRIES (ipcPath), ipcPath).
   FOR EACH ttConnection
   WHERE (ttConnection.FromComponent  EQ cFrom OR ttConnection.ToComponent EQ cFrom):
   
      IF ttConnection.FromComponent EQ cFrom THEN
         cNext = ttConnection.ToComponent.
      ELSE 
         cNext = ttConnection.FromComponent.
             
      IF cNext EQ ipcEndComponent THEN DO:
         /* Found a Path, save the connections traversed */
         lvlFound = TRUE.   
         IF lvlOutput THEN DO:
            PUT UNFORMATTED 
               SUBSTITUTE ("Found it in '&1' steps.", NUM-ENTRIES (ipcPath) + 1) SKIP.
         END.
         ELSE DO:
            IF lvlShow THEN DO WITH FRAME fr-Parameters:
               edProgress:INSERT-STRING (SUBSTITUTE ("Found it in '&1' steps.~n", 
                                                    NUM-ENTRIES (ipcPath) + 1)).
               PROCESS EVENTS.                                                 
            END.     
         END.
         
         DO iPath = 1 TO NUM-ENTRIES (ipcPath):
            ASSIGN 
               cTraverseFrom = ENTRY (iPath, ipcPath)
            .
            IF iPath EQ NUM-ENTRIES (ipcPath) THEN 
               cTraverseTo = cNext.
            ELSE 
               cTraverseTo = ENTRY (iPath + 1, ipcPath).
            FIND  ttTraversed
            WHERE ttTraversed.FromComponent EQ cTraverseFrom
            AND   ttTraversed.ToComponent   EQ cTraverseTo NO-ERROR.
            IF NOT AVAILABLE ttTraversed THEN
               FIND  ttTraversed
               WHERE ttTraversed.FromComponent EQ cTraverseTo
               AND   ttTraversed.ToComponent   EQ cTraverseFrom.
            ASSIGN 
               ttTraversed.PathCount = ttTraversed.PathCount + 1
            .
         END.
         RETURN.                
      END.
      ELSE DO:
         IF LOOKUP (cNext, ipcPath) EQ 0 THEN DO:
            /* Explore this (new) path */
            cNewPath = ipcPath + "," + cNext.
            RUN getPath
               (INPUT  cNewPath,
                INPUT  ipcEndComponent).
            IF lvlFound THEN 
               RETURN.                
         END.
      END.
   END.

END PROCEDURE.

