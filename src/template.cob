PPPSSSCAAAABBBBBBBBBBBBBBBBBBBBBBBBBBBBBBBBBBBBBBBBBBBBBBBBBBBBBBBBBBBBBIIIIIIII
123456789012345678901234567890123456789012345678901234567890123456789012345678901234567890123456789012345678901234567890123456789012
                      |

001010 IDENTIFICATION DIVISION.                                         PROGNAME
001020                                                                  PROGNAME
001030 PROGRAM-ID.    PROGNAME.                                         PROGNAME
001040 AUTHOR.        MMELL.                                            PROGNAME
001050 INSTALLATION.  AUBURN.                                           PROGNAME
001060 DATE-WRITTEN.  NOV 29,2024.                                      PROGNAME
001070 DATE-COMPILED. NOV 29,2024.                                      PROGNAME
001080 SECURITY.      UNCLASSIFIED.                                     PROGNAME
001090                                                                  PROGNAME
001100***************************************************************** PROGNAME
001110*                                                               * PROGNAME
001120* THIS PROGRAM PRODUCES A POLICY REPORT                         * PROGNAME
001130*                                                               * PROGNAME
001140***************************************************************** PROGNAME

002010 ENVIRONMENT DIVISION.                                            PROGNAME
002020                                                                  PROGNAME
002030 CONFIGURATION SECTION.                                           PROGNAME
002040
002050 SOURCE-COMPUTER. X86_64.                                         PROGNAME
002060 OBJECT-COMPUTER. X86_64.                                         PROGNAME
002070                                                                  PROGNAME
002080 INPUT-OUTPUT SECTION.                                            PROGNAME
002090 FILE-CONTROL.                                                    PROGNAME
002100     SELECT INPUT-FILE                                            PROGNAME
002110           ASSIGN TO UT-S-INPUT                                   PROGNAME
002120     SELECT OUTPUT-FILE                                           PROGNAME
002130           ASSIGN TO UT-S-PRINT                                   PROGNAME
	  
003010 DATA DIVISION.                                                   PROGNAME
003020                                                                  PROGNAME
003030 FILE SECTION.                                                    PROGNAME
003030                                                                  PROGNAME
003040 FD  INPUT-FILE                                                   PROGNAME
003050       RECORD CONTAINS 62 CHARACTERS                              PROGNAME
003060       LABEL RECORDS ARE STANDARD                                 PROGNAME
003070       DATA RECORD IS INPUT-RECORD.                               PROGNAME
003080 01  INPUT-RECORD.                                                PROGNAME
003090     05  CUSTOMER-NAME-INPUT     PICTURE X(20).                   PROGNAME
003100     05  AGENT-NAME-INPUT        PICTURE X(20).                   PROGNAME
003110     05  INSURANCE-TYPE-INPUT    PICTURE X(10).                   PROGNAME
003120     05  POLICY-NUMBER-INPUT     PICTURE X(12).                   PROGNAME
003130                                                                  PROGNAME
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
003200                                                                  PROGNAME
003210 WORKING-STORAGE SECTION.                                         PROGNAME
003220                                                                  PROGNAME
003230 01  PROGRAM-INDICATORS.                                          PROGNAME
003240     05  ARE-THERE-MORE-RECORDS  PICTURE X(3) VALUE 'YES'.        PROGNAME

Arithmetic - COMPUTE ADD SUBTRACT MULTIPLY DIVIDE
Compiler directives - COPY REPLACE ENTER USE
Conditional - EVALUATE IF
Data movement - INITIALIZE INSPECT MOVE STRING UNSTRING
Ending - STOP
Input/output - ACCEPT CLOSE DELETE DISPLAY OPEN REWRITE START WRITE
Interprocess communication - CALL CANCEL
Ordering - MERGE RELEASE RETURN SORT
Procedure/branching - ALTER EXIT GOTO PERFORM
Table handling - SEARCH SET



004010 PROCEDURE DIVISION.                                              PROGNAME
004020                                                                  PROGNAME
004030***************************************************************** PROGNAME
004040*                                                               * PROGNAME
004050* THIS PROGRAM READS THE POLICY INPUT RECORDS AND CREATES THE   * PROGNAME
004060* POLICY REPORT.  IT IS ENTERED FROM THE OPERATING SYSTEM AND   * PROGNAME
004070* EXITS TO THE OPERATING SYSYTEM.                               * PROGNAME
004080*                                                               * PROGNAME
004090***************************************************************** PROGNAME
004100                                                                  PROGNAME
004110 A000-CREATE-POLICY-REPORT.                                       PROGNAME
004120                                                                  PROGNAME
004130     OPEN INPUT  INPUT-FILE                                       PROGNAME
004140          OUTPUT REPORT-FILE.                                     PROGNAME
004150     READ POLICY-INPUT-FILE                                       PROGNAME
004160           AT END                                                 PROGNAME
004170              MOVE 'NO ' TO ARE-THERE-MORE-RECORDS.               PROGNAME
004180     PERFORM A001-FORMAT-PRINT-LINE                               PROGNAME
004190           UNTIL ARE-THERE-MORE-RECORDS = 'NO '.                  PROGNAME
004200     CLOSE INPUT-FILE                                             PROGNAME
004210           REPORT-FILE.                                           PROGNAME
004220     STOP RUN.                                                    PROGNAME

005010 A001-FORMAT-PRINT-LINE.                                          PROGNAME
005020     MOVE SPACES TO REPORT-LINE.                                  PROGNAME
005030     MOVE POLICY-NUMBER-INPUT TO POLICY-NUMBER-REPORT.            PROGNAME
005040     MOVE CUSTOMER-NAME-INPUT TO CUSTOMER-NAME-REPORT.            PROGNAME
005050     MOVE AGENT-NAME-INPUT TO AGENT-NAME-REPORT.                  PROGNAME
005060     MOVE INSURANCE-TYPE-INPUT TO INSURANCE-TYPE-REPORT.          PROGNAME
005070     WRITE POLICY-REPORT-LINE                                     PROGNAME
005080           AFTER ADVANCING 1 LINES.                               PROGNAME
005090     READ POLICY-INPUT-FILE                                       PROGNAME
005100           AT END                                                 PROGNAME
005110              MOVE 'NO ' TO ARE-THERE-MORE-RECORDS.               PROGNAME
