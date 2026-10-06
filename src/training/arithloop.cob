       IDENTIFICATION DIVISION.

       PROGRAM-ID.    ARITH-LOOP.
       AUTHOR.        MICHAEL S. MELL.
       INSTALLATION.  TT ODYSSEY, 41786AL.
       DATE-WRITTEN.  OCTOBER 5, 2026.
       DATE-COMPILED. OCTOBER 5, 2026.
       SECURITY.      UNCLAS.

      ***************************************************************** 
      *                                                               * 
      * ARITH-LOOP, A SAMPLE PROGRAM DEMONSTRATING ROUTINE ARITHMATIC *
      * AND LOOPING MECHANISMS                                        * 
      *                                                               * 
      ***************************************************************** 

       ENVIRONMENT DIVISION.

       CONFIGURATION SECTION.

       SOURCE-COMPUTER. X86_64 Linux.
       OBJECT-COMPUTER. X86_64 Linux.

       DATA DIVISION.

       FILE SECTION.

       WORKING-STORAGE SECTION.
           01  WS-INDEX             PIC 9(02) VALUE 1.
           01  WS-CNT               PIC 9(02) VALUE 5.
           01  WS-PRICE             PIC 9(03)V99 VALUE 12.50.
           01  WS-QTY               PIC 9(03) VALUE 1.
           01  WS-LINE              PIC 9(05)V99.
           01  WS-TOTAL             PIC 9(07)V99 VALUE 0.
           01  WS-DISCNT            PIC 9(02)V99 VALUE 0.
           01  WS-TOTAL-EDIT        PIC Z,ZZZ,ZZ9.99.

       PROCEDURE DIVISION.
           PERFORM VARYING WS-INDEX FROM 1 BY 1
               UNTIL WS-INDEX > WS-CNT
           COMPUTE WS-LINE = WS-PRICE * WS-QTY
           ADD WS-LINE TO WS-TOTAL
           ADD 1 TO WS-QTY
           END-PERFORM
           PERFORM APPLY-DISCNT
           MOVE WS-TOTAL TO WS-TOTAL-EDIT
           DISPLAY 'Subtotal: ' WS-TOTAL-EDIT
           DISPLAY 'Discount: ' WS-DISCNT
           COMPUTE WS-TOTAL = WS-TOTAL - WS-DISCNT
           MOVE WS-TOTAL TO WS-TOTAL-EDIT
           DISPLAY 'Grand total: ' WS-TOTAL-EDIT
           GOBACK.
           APPLY-DISCNT.
           EVALUATE TRUE
           WHEN WS-TOTAL >= 100.00
           MOVE 5.00 TO WS-DISCNT
           WHEN WS-TOTAL >= 50.00
           MOVE 2.00 TO WS-DISCNT
           WHEN OTHER
           MOVE 0.00 TO WS-DISCNT
           END-EVALUATE.
