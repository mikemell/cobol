       IDENTIFICATION DIVISION.

       PROGRAM-ID.    .
       AUTHOR.        MICHAEL S. MELL.
       INSTALLATION.  .
       DATE-WRITTEN.  .
       DATE-COMPILED. .
       SECURITY.      [UNCLAS|FOUO|CONFIDENTIAL|SECRET|TOP SECRET].

      ******************************************************************
      * Columns 1-6     Sequence number area (ppplll)                  *
      * Column 7        Indicator area ("*" = comment,- "=" cont.,     *
      *                   "/" = print stopper, "D" = debug)            *
      * Columns 8-11    Area a                                         *
      * Columns 12-72   Area b                                         *
      * Columns 73-80   reserved (system generated number)             *
      ******************************************************************

       ENVIRONMENT DIVISION.

       CONFIGURATION SECTION.

       SOURCE-COMPUTER. X86_64 Linux.
       OBJECT-COMPUTER. X86_64 Linux.

       INPUT-OUTPUT SECTION.

       FILE-CONTROL.

       DATA DIVISION.

       FILE SECTION.

       WORKING-STORAGE SECTION.

       PROCEDURE DIVISION.
      *    Arithmetic - COMPUTE ADD SUBTRACT MULTIPLY DIVIDE
      *    Compiler directives - COPY REPLACE ENTER USE
      *    Conditional - EVALUATE IF
      *    Data movement - INITIALIZE INSPECT MOVE STRING UNSTRING
      *    Ending - STOP
      *    Input/output - ACCEPT CLOSE DELETE DISPLAY OPEN REWRITE START
      *         READ WRITE
      *    Interprocess communication - CALL CANCEL
      *    Ordering - MERGE RELEASE RETURN SORT
      *    Procedure/branching - ALTER EXIT GOTO PERFORM
      *    Table handling - SEARCH SET

