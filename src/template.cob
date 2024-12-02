001010 IDENTIFICATION DIVISION.                                         PROGNAME
001030 PROGRAM-ID.    PROGNAME.                                         PROGNAME
001040 AUTHOR.        MMELL.                                            PROGNAME
001050 INSTALLATION.  AUBURN.                                           PROGNAME
001060 DATE-WRITTEN.  NOV 29,2024.                                      PROGNAME
001070 DATE-COMPILED. NOV 29,2024.                                      PROGNAME
001080 SECURITY.      UNCLASSIFIED.                                     PROGNAME
001100******************************************************************PROGNAME
001110*                                                                *PROGNAME
001120* THIS PROGRAM PRODUCES A POLICY REPORT                          *PROGNAME
001130*                                                                *PROGNAME
001140******************************************************************PROGNAME
002010 ENVIRONMENT DIVISION.                                            PROGNAME
002030 CONFIGURATION SECTION.                                           PROGNAME
002050 SOURCE-COMPUTER. X86_64.                                         PROGNAME
002060 OBJECT-COMPUTER. X86_64.                                         PROGNAME
002080 INPUT-OUTPUT SECTION.                                            PROGNAME
002090 FILE-CONTROL.                                                    PROGNAME
002100     SELECT INPUT-FILE                                            PROGNAME
002110           ASSIGN TO UT-S-INPUT                                   PROGNAME
002120     SELECT OUTPUT-FILE                                           PROGNAME
002130           ASSIGN TO UT-S-PRINT                                   PROGNAME
003010 DATA DIVISION.                                                   PROGNAME
003030 FILE SECTION.                                                    PROGNAME
003040 FD  INPUT-FILE                                                   PROGNAME
003050       RECORD CONTAINS 62 CHARACTERS                              PROGNAME
003060       LABEL RECORDS ARE STANDARD                                 PROGNAME
003070       DATA RECORD IS INPUT-RECORD.                               PROGNAME
003080 01  INPUT-RECORD.                                                PROGNAME
003090     05  CUSTOMER-NAME-INPUT     PICTURE X(20).                   PROGNAME
003100     05  AGENT-NAME-INPUT        PICTURE X(20).                   PROGNAME
003110     05  INSURANCE-TYPE-INPUT    PICTURE X(10).                   PROGNAME
003120     05  POLICY-NUMBER-INPUT     PICTURE X(12).                   PROGNAME
003140 FD  REPORT-FILE                                                  PROGNAME
003150       RECORD CONTAINS 133 CHARACTERS                             PROGNAME
003160       LABEL RECORDS ARE OMITTED                                  PROGNAME
003170       DATA RECORD IS REPORT-LINE.                                PROGNAME
003180 01  REPORT-LINE.                                                 PROGNAME
003190     05  CARRIAGE-CONTROL        PICTURE X.                       PROGNAME
003190     05  POLICY-NUMBER-REPORT    PICTURE X(12).                   PROGNAME
003190     05  FILLER                  PICTURE X(4).                    PROGNAME
003190     05  CUSTOMER-NAME-REPORT    PICTURE X(20).                   PROGNAME
003190     05  FILLER                  PICTURE X(4).                    PROGNAME
003190     05  AGENT-NAME-REPORT       PICTURE X(20).                   PROGNAME
003190     05  FILLER                  PICTURE X(4).                    PROGNAME
003190     05  INSURANCE-TYPE-REPORT   PICTURE X(10).                   PROGNAME
003190     05  FILLER                  PICTURE X(58).                   PROGNAME
003210 WORKING-STORAGE SECTION.                                         PROGNAME
003230 01  PROGRAM-INDICATORS.                                          PROGNAME
003240     05  ARE-THERE-MORE-RECORDS  PICTURE X(3) VALUE 'YES'.        PROGNAME
004010 PROCEDURE DIVISION.                                              PROGNAME
004020******************************************************************PROGNAME
004030* Arithmetic - COMPUTE ADD SUBTRACT MULTIPLY DIVIDE              *PROGNAME
004040* Compiler directives - COPY REPLACE ENTER USE                   *PROGNAME
004050* Conditional - EVALUATE IF                                      *PROGNAME
004060* Data movement - INITIALIZE INSPECT MOVE STRING UNSTRING        *PROGNAME
004070* Ending - STOP                                                  *PROGNAME
004080* Input/output - ACCEPT CLOSE DELETE DISPLAY OPEN REWRITE START  *PROGNAME
004090*      WRITE                                                     *PROGNAME
004100* Interprocess communication - CALL CANCEL                       *PROGNAME
004110* Ordering - MERGE RELEASE RETURN SORT                           *PROGNAME
004120* Procedure/branching - ALTER EXIT GOTO PERFORM                  *PROGNAME
004130* Table handling - SEARCH SET                                    *PROGNAME
004140******************************************************************PROGNAME
004160******************************************************************PROGNAME
004170*                                                                *PROGNAME
004180* THIS PROGRAM READS THE POLICY INPUT RECORDS AND CREATES THE    *PROGNAME
004190* POLICY REPORT.  IT IS ENTERED FROM THE OPERATING SYSTEM AND    *PROGNAME
004200* EXITS TO THE OPERATING SYSYTEM.                                *PROGNAME
004210*                                                                *PROGNAME
004220******************************************************************PROGNAME
004310 A000-CREATE-POLICY-REPORT.                                       PROGNAME
004320                                                                  PROGNAME
004330     OPEN INPUT  INPUT-FILE                                       PROGNAME
004340          OUTPUT REPORT-FILE.                                     PROGNAME
004350     READ POLICY-INPUT-FILE                                       PROGNAME
004360           AT END                                                 PROGNAME
004370              MOVE 'NO ' TO ARE-THERE-MORE-RECORDS.               PROGNAME
004380     PERFORM A001-FORMAT-PRINT-LINE                               PROGNAME
004390           UNTIL ARE-THERE-MORE-RECORDS = 'NO '.                  PROGNAME
004400     CLOSE INPUT-FILE                                             PROGNAME
004410           REPORT-FILE.                                           PROGNAME
004420     STOP RUN.                                                    PROGNAME
004440 A001-FORMAT-PRINT-LINE.                                          PROGNAME
004450     MOVE SPACES TO REPORT-LINE.                                  PROGNAME
004460     MOVE POLICY-NUMBER-INPUT TO POLICY-NUMBER-REPORT.            PROGNAME
004470     MOVE CUSTOMER-NAME-INPUT TO CUSTOMER-NAME-REPORT.            PROGNAME
004480     MOVE AGENT-NAME-INPUT TO AGENT-NAME-REPORT.                  PROGNAME
004490     MOVE INSURANCE-TYPE-INPUT TO INSURANCE-TYPE-REPORT.          PROGNAME
004500     WRITE POLICY-REPORT-LINE                                     PROGNAME
004510           AFTER ADVANCING 1 LINES.                               PROGNAME
004520     READ POLICY-INPUT-FILE                                       PROGNAME
004530           AT END                                                 PROGNAME
004540              MOVE 'NO ' TO ARE-THERE-MORE-RECORDS.               PROGNAME