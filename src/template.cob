       IDENTIFICATION DIVISION.

       PROGRAM-ID.    .
       AUTHOR.        MICHAEL S. MELL.
       INSTALLATION.  M10.
       DATE-WRITTEN.  .
       DATE-COMPILED. .
       SECURITY.      [UNCLAS|FOUO|CONFIDENTIAL|SECRET|TOP SECRET].

      ******************************************************************

       ENVIRONMENT DIVISION.

       CONFIGURATION SECTION.

       SOURCE-COMPUTER. X86_64/Linux.
       OBJECT-COMPUTER. X86_64/Linux.

       INPUT-OUTPUT SECTION.

       FILE-CONTROL.

           SELECT INPUT-FILE
                 ASSIGN TO UT-S-INPUT
           SELECT OUTPUT-FILE
                 ASSIGN TO UT-S-PRINT

      ******************************************************************

       DATA DIVISION.

       FILE SECTION.

       FD  INPUT-FILE
             RECORD CONTAINS 62 CHARACTERS
             LABEL RECORDS ARE STANDARD
             DATA RECORD IS INPUT-RECORD.
       01  INPUT-RECORD.
           05  CUSTOMER-NAME-INPUT     PICTURE X(20).
           05  AGENT-NAME-INPUT        PICTURE X(20).
           05  INSURANCE-TYPE-INPUT    PICTURE X(10).
           05  POLICY-NUMBER-INPUT     PICTURE X(12).

       FD  REPORT-FILE
             RECORD CONTAINS 133 CHARACTERS
             LABEL RECORDS ARE OMITTED
             DATA RECORD IS REPORT-LINE.
       01  REPORT-LINE.
           05  CARRIAGE-CONTROL        PICTURE X.
           05  POLICY-NUMBER-REPORT    PICTURE X(12).
           05  FILLER                  PICTURE X(4).
           05  CUSTOMER-NAME-REPORT    PICTURE X(20).
           05  FILLER                  PICTURE X(4).
           05  AGENT-NAME-REPORT       PICTURE X(20).
           05  FILLER                  PICTURE X(4).
           05  INSURANCE-TYPE-REPORT   PICTURE X(10).
           05  FILLER                  PICTURE X(58).

       WORKING-STORAGE SECTION.

       01  PROGRAM-INDICATORS.
           05  ARE-THERE-MORE-RECORDS  PICTURE X(3) VALUE 'YES'.

      ******************************************************************

       PROCEDURE DIVISION.
      * Arithmetic - COMPUTE ADD SUBTRACT MULTIPLY DIVIDE
      * Compiler directives - COPY REPLACE ENTER USE
      * Conditional - EVALUATE IF
      * Data movement - INITIALIZE INSPECT MOVE STRING UNSTRING
      * Ending - STOP
      * Input/output - ACCEPT CLOSE DELETE DISPLAY OPEN REWRITE START
      *      READ WRITE
      * Interprocess communication - CALL CANCEL
      * Ordering - MERGE RELEASE RETURN SORT
      * Procedure/branching - ALTER EXIT GOTO PERFORM
      * Table handling - SEARCH SET

       A000-CREATE-POLICY-REPORT.
           OPEN INPUT  INPUT-FILE
                OUTPUT REPORT-FILE.
           READ POLICY-INPUT-FILE
                 AT END
                    MOVE 'NO ' TO ARE-THERE-MORE-RECORDS.
           PERFORM A001-FORMAT-PRINT-LINE
                 UNTIL ARE-THERE-MORE-RECORDS = 'NO '.
           CLOSE INPUT-FILE
                 REPORT-FILE.
           STOP RUN.

       A001-FORMAT-PRINT-LINE.
           MOVE SPACES TO REPORT-LINE.
           MOVE POLICY-NUMBER-INPUT TO POLICY-NUMBER-REPORT.
           MOVE CUSTOMER-NAME-INPUT TO CUSTOMER-NAME-REPORT.
           MOVE AGENT-NAME-INPUT TO AGENT-NAME-REPORT.
           MOVE INSURANCE-TYPE-INPUT TO INSURANCE-TYPE-REPORT.
           WRITE POLICY-REPORT-LINE
                 AFTER ADVANCING 1 LINES.
           READ POLICY-INPUT-FILE
                 AT END
                    MOVE 'NO ' TO ARE-THERE-MORE-RECORDS.
