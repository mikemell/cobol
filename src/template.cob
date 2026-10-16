       IDENTIFICATION DIVISION.

       PROGRAM-ID.    .
       AUTHOR.        MICHAEL S. MELL.
       INSTALLATION.  ODYSSEY .
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
      /
       ENVIRONMENT DIVISION.

       CONFIGURATION SECTION.

       SOURCE-COMPUTER. X86_64 Linux.
       OBJECT-COMPUTER. X86_64 Linux.
      /
       DATA DIVISION.

       FILE SECTION.

       WORKING-STORAGE SECTION.
      /
       PROCEDURE DIVISION.

           STOP RUN
