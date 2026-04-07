       IDENTIFICATION DIVISION.                                         00010000
                                                                        00020000
       PROGRAM-ID. RPT6000.                                             00030001
                                                                        00040000
      *   Programmers.: Violet French, Hayden Schmidt                   00050002
      *   Date........: 2026.04.02                                      00060002
      *   Github URL..: https://github.com/Pirategirl9000/RPT6000       00070001
      *   Description.: This program produces a sales report based on   00080000
      *   values acquired from the CUSTMAST dataset and produces        00090000
      *   subtotals and grandtotals for the different branches and      00100000
      *   sales representatives                                         00110000
       ENVIRONMENT DIVISION.                                            00120000
                                                                        00130000
       INPUT-OUTPUT SECTION.                                            00140000
                                                                        00150000
       FILE-CONTROL.                                                    00160000
           SELECT CUSTMAST ASSIGN TO CUSTMAST.
           SELECT INPUT-SALESREP ASSIGN TO SALESREP.                    00170000
           SELECT ORPT6000 ASSIGN TO RPT6000.                           00180001
                                                                        00190000
       DATA DIVISION.                                                   00200000
                                                                        00210000
       FILE SECTION.
      **************************************************************    00240000
      * INPUT FILEs                                                *    00250000
      **************************************************************

       FD  CUSTMAST                                                     00270000
           RECORDING MODE IS F                                          00280000
           LABEL RECORDS ARE STANDARD                                   00290000
           RECORD CONTAINS 130 CHARACTERS                               00300000
           BLOCK CONTAINS 130 CHARACTERS.                               00310000
       01  CUSTOMER-MASTER-RECORD.                                      00320000
           05  CM-BRANCH-NUMBER        PIC 9(2).                        00330000
           05  CM-SALESREP-NUMBER      PIC 9(2).                        00340000
           05  CM-CUSTOMER-NUMBER      PIC 9(5).                        00350000
           05  CM-CUSTOMER-NAME        PIC X(20).                       00360000
           05  CM-SALES-THIS-YTD       PIC S9(5)V9(2).                  00370000
           05  CM-SALES-LAST-YTD       PIC S9(5)V9(2).                  00380000
           05  FILLER                  PIC X(87).

       FD  INPUT-SALESREP
           RECORDING MODE IS F                                          00280000
           LABEL RECORDS ARE STANDARD                                   00290000
           RECORD CONTAINS 130 CHARACTERS                               00300000
           BLOCK CONTAINS 130 CHARACTERS.                               00310000
       01  SALESREP-MASTER-RECORD.                                      00320000
           05 SM-SALESREP-NUMBER       PIC 9(2).
           05 SM-SALESREP-NAME         PIC 9(10).
           05  FILLER                  PIC X(118).                      00390000
                                                                        00400000
      **************************************************************    00410000
      * OUTPUT FILE                                                *    00420000
      **************************************************************    00430000
       FD  ORPT6000                                                     00440001
           RECORDING MODE IS F                                          00450000
           LABEL RECORDS ARE STANDARD                                   00460000
           RECORD CONTAINS 130 CHARACTERS                               00470000
           BLOCK CONTAINS 130 CHARACTERS.                               00480000
       01  PRINT-AREA      PIC X(130).                                  00490000
                                                                        00500000
       WORKING-STORAGE SECTION.                                         00510000
                                                                        00520000
      *------------------------------------------------------------*    00530000
      *                        WORKING FIELDS                      *    00540000
      *============================================================*    00550000
      *     THE FOLLOWING RECORDS ARE USED FOR WORKING WITH DATA   *    00560000
      *              AND ARE NOT USED FOR PROGRAM OUTPUT           *    00570000
      *------------------------------------------------------------*    00580000
       01 SALESREP-TABLE.
           05 SALESREP-GROUP OCCURS 100 TIMES                           00610007
                             INDEXED BY SRT-INDEX.                      00620007
               10 SALESREP-NUMBER   PIC 99.                             00630007
               10 SALESREP-NAME     PIC X(10).

      **************************************************************    00650000
      * SWITCHES FOR END OF FILE AND FIRST RECORD                  *    00660000
      **************************************************************    00670000
       01  SWITCHES.
           05  SALESREP-EOF-SWITCH     PIC X    VALUE "N".              00690000
               88  SALESREP-EOF                 VALUE "Y".
           05  CUSTMAST-EOF-SWITCH     PIC X    VALUE "N".              00690000
               88  CUSTMAST-EOF                 VALUE "Y".              00700000
           05  FIRST-RECORD-SWITCH     PIC X    VALUE "Y".              00710000
               88  FIRST-RECORD                 VALUE "Y"               00720000
                   WHEN FALSE IS                      "N".              00730000
                                                                        00740000
      **************************************************************    00750000
      * SWITCH FOR END OF FILE                                     *    00760000
      **************************************************************    00770000
       01  CONTROL-FIELDS PACKED-DECIMAL.                               00780000
           05  OLD-BRANCH-NUMBER       PIC 99.                          00790000
           05  OLD-SALESREP-NUMBER     PIC 99.                          00800000
                                                                        00810000
      **************************************************************    00820000
      * STORES INFORMATION RELEVANT TO THE PAGE                    *    00830000
      **************************************************************    00840000
       01  PRINT-FIELDS PACKED-DECIMAL.                                 00850000
           05  PAGE-COUNT      PIC S9(3)   VALUE ZERO.                  00860000
           05  LINES-ON-PAGE   PIC S9(3)   VALUE +55.                   00870000
           05  LINE-COUNT      PIC S9(3)   VALUE +99.                   00880000
                                                                        00890000
      **************************************************************    00900000
      * STORES TOTAL FIELDS FOR CALCULATING                        *    00910000
      **************************************************************    00920000
       01  TOTAL-FIELDS PACKED-DECIMAL.                                 00930000
           05  BRANCH-TOTAL-THIS-YTD    PIC S9(6)V99   VALUE ZERO.      00940000
           05  BRANCH-TOTAL-LAST-YTD    PIC S9(6)V99   VALUE ZERO.      00950000
           05  SALESREP-TOTAL-THIS-YTD  PIC S9(6)V99   VALUE ZERO.      00960000
           05  SALESREP-TOTAL-LAST-YTD  PIC S9(6)V99   VALUE ZERO.      00970000
           05  GRAND-TOTAL-THIS-YTD     PIC S9(7)V99   VALUE ZERO.      00980000
           05  GRAND-TOTAL-LAST-YTD     PIC S9(7)V99   VALUE ZERO.      00990000
                                                                        01000000
      **************************************************************    01010000
      * USED TO PULL IN THE CURRENT-DATE-TIME VIA THE FUNCTION     *    01020000
      * CURRENT-DATE-AND-TIME WHICH WILL BE USED IN HEADER LINES   *    01030000
      **************************************************************    01040000
       01  CURRENT-DATE-AND-TIME.                                       01050000
           05  CD-YEAR         PIC 9999.                                01060000
           05  CD-MONTH        PIC 99.                                  01070000
           05  CD-DAY          PIC 99.                                  01080000
           05  CD-HOURS        PIC 99.                                  01090000
           05  CD-MINUTES      PIC 99.                                  01100000
           05  FILLER          PIC X(9).                                01110000
                                                                        01120000
      **************************************************************    01130000
      * STORES VALUES USED FOR CALCULATIONS                       *     01140000
      **************************************************************    01150000
       01  CALCULATED-FIELDS.                                           01160000
           05 CHANGE-AMOUNT    PIC S9(5)V99.                            01170000
                                                                        01180000
      *------------------------------------------------------------*    01190000
      *                       OUTPUT FIELDS                        *    01200000
      *============================================================*    01210000
      *     THE FOLLOWING RECORDS ARE USED FOR PRINTING DATA TO    *    01220000
      *                      THE OUTPUT FILE                       *    01230000
      *------------------------------------------------------------*    01240000
                                                                        01250000
      **************************************************************    01260000
      * STORES THE FIRST HEADER LINE INFORMATION                   *    01270000
      * HOLDS THE DATE, REPORT TITLE, AND PAGE NUMBER              *    01280000
      **************************************************************    01290000
       01  HEADING-LINE-1.                                              01300000
           05  FILLER          PIC X(7)    VALUE "DATE:  ".             01310000
           05  HL1-MONTH       PIC 9(2).                                01320000
           05  FILLER          PIC X(1)    VALUE "/".                   01330000
           05  HL1-DAY         PIC 9(2).                                01340000
           05  FILLER          PIC X(1)    VALUE "/".                   01350000
           05  HL1-YEAR        PIC 9(4).                                01360000
           05  FILLER          PIC X(26)   VALUE SPACE.                 01370000
           05  FILLER          PIC X(20)   VALUE "YEAR-TO-DATE SALES R".01380000
           05  FILLER          PIC X(31)   VALUE "EPORT".               01390000
           05  FILLER          PIC X(6)    VALUE "PAGE: ".              01400000
           05  HL1-PAGE-NUMBER PIC ZZZ9.                                01410000
           05  FILLER          PIC X(26)   VALUE SPACE.                 01420000
                                                                        01430000
      **************************************************************    01440000
      * STORES THE SECOND HEADER LINE INFORMATION                  *    01450000
      * HOLDS THE TIME AND THE PROGRAM ID                          *    01460000
      **************************************************************    01470000
       01  HEADING-LINE-2.                                              01480000
           05  FILLER          PIC X(7)    VALUE "TIME:  ".             01490000
           05  HL2-HOURS       PIC 9(2).                                01500000
           05  FILLER          PIC X(1)    VALUE ":".                   01510000
           05  HL2-MINUTES     PIC 9(2).                                01520000
           05  FILLER          PIC X(82)   VALUE SPACE.                 01530000
           05  FILLER          PIC X(7)    VALUE "RPT6000".             01540001
           05  FILLER          PIC X(29)   VALUE SPACE.                 01550000
                                                                        01560000
      **************************************************************    01570000
      * STORES THE THIRD HEADER LINE USED TO DISPLAY A LINE SPACER *    01580000
      **************************************************************    01590000
       01  HEADING-LINE-3.                                              01600000
           05 FILLER               PIC X(130)   VALUE SPACE.            01610000
                                                                        01620000
      **************************************************************    01630000
      * STORES THE FOURTH HEADER LINE INFORMATION                  *    01640000
      * HOLDS THE DIFFERENT COLUMN NAMES - SOME ARE SPLIT ACROSS   *    01650000
      * THE NEXT HEADER LINE                                       *    01660000
      **************************************************************    01670000
       01  HEADING-LINE-4.                                              01680000
           05  FILLER      PIC X(54)   VALUE SPACES.                    01690000
           05  FILLER      PIC X(19)   VALUE "SALES         SALES".     01700000
           05  FILLER      PIC X(8)    VALUE SPACES.                    01710014
           05  FILLER      PIC X(17)   VALUE "CHANGE     CHANGE".       01720014
           05  FILLER      PIC X(32)   VALUE SPACES.                    01730014
                                                                        01740000
      **************************************************************    01750000
      * STORES THE FIFTH HEADER LINE INFORMATION                   *    01760000
      * HOLDS SOME OF THE COLUMN NAMES AS WELL AS THE OTHER HALF   *    01770000
      * OF COLUMN NAMES THAT STARTED IN THE LAST HEADER LINE       *    01780000
      **************************************************************    01790000
       01  HEADING-LINE-5.                                              01800000
           05  FILLER         PIC X(17)  VALUE "BRANCH   SALESREP".     01810000
           05  FILLER         PIC X(13)  VALUE SPACES.                  01820000
           05  FILLER         PIC X(8)   VALUE "CUSTOMER".              01830000
           05  FILLER         PIC X(14)  VALUE SPACES.                  01840000
           05  FILLER         PIC X(22)  VALUE "THIS YTD      LAST YTD".01850000
           05  FILLER         PIC X(7)   VALUE SPACES.                  01860000
           05  FILLER         PIC X(18)  VALUE "AMOUNT     PERCENT".    01870000
           05  FILLER         PIC X(31)  VALUE SPACE.                   01880014
                                                                        01890000
      **************************************************************    01900000
      * STORES THE SIXTH HEADER LINE WHICH IS USED FOR SPACING     *    01910000
      **************************************************************    01920000
       01  HEADING-LINE-6.                                              01930000
           05  FILLER           PIC X(6)   VALUE ALL '-'.               01940014
           05  FILLER           PIC X(1)   VALUE SPACE.                 01950014
           05  FILLER           PIC X(13)  VALUE ALL '-'.               01960014
           05  FILLER           PIC X(1)   VALUE SPACE.                 01970014
           05  FILLER           PIC X(26)   VALUE ALL '-'.              01980014
           05  FILLER           PIC X(3)   VALUE SPACE.                 01990014
           05  FILLER           PIC X(11)  VALUE ALL '-'.               02000014
           05  FILLER           PIC X(3)   VALUE SPACE.                 02010014
           05  FILLER           PIC X(11)  VALUE ALL '-'.               02020014
           05  FILLER           PIC X(4)   VALUE SPACE.                 02030014
           05  FILLER           PIC X(11)  VALUE ALL '-'.               02040014
           05  FILLER           PIC X(2)   VALUE SPACE.                 02050014
           05  FILLER           PIC x(7)   VALUE ALL '-'.               02060014
           05  FILLER           PIC X(31)  VALUE SPACE.                 02070014
                                                                        02080000
      **************************************************************    02090000
      * STORES INFORMATION ABOUT CURRENT CUSTOMER                  *    02100000
      * HOLDS THE BRANCH NUMBER, SALES REP NUMBER, CUSTOMER NUMBER,*    02110000
      * CUSTOMER NAME, SALES THIS AND LAST YEAR-TO-DATE,           *    02120000
      * DIFFERENCE BETWEEN THIS YEARS SALES AND LAST, AND THE      *    02130000
      * DIFFERENCE IN PERCENT.                                     *    02140000
      **************************************************************    02150000
       01  CUSTOMER-LINE.                                               02160000
           05  FILLER               PIC X(2)       VALUE SPACE.
           05  CL-BRANCH-NUMBER     PIC X(2).
           05  FILLER               PIC X(3)       VALUE SPACE.
           05  CL-SALESREP-NUMBER   PIC X(2).
           05  FILLER               PIC X(1)       VALUE SPACE.
           05  CL-SALESREP-NAME     PIC X(10).
           05  FILLER               PIC X(1)       VALUE SPACE.
           05  CL-CUSTOMER-NUMBER   PIC X(5).
           05  FILLER               PIC X(1)       VALUE SPACE.
           05  CL-CUSTOMER-NAME     PIC X(20).
           05  FILLER               PIC X(6)       VALUE SPACE.
           05  CL-SALES-THIS-YTD    PIC ZZ,ZZ9.99-.
           05  FILLER               PIC X(4)       VALUE SPACE.
           05  CL-SALES-LAST-YTD    PIC ZZ,ZZ9.99-.
           05  FILLER               PIC X(4)       VALUE SPACE.
           05  CL-CHANGE-AMOUNT     PIC ZZ,ZZ9.99-.
           05  FILLER               PIC X(2)       VALUE SPACE.
           05  CL-CHANGE-PERCENT    PIC +++9.9.
           05  CL-CHANGE-PERCENT-R  REDEFINES  CL-CHANGE-PERCENT
                                    PIC X(6).
           05  FILLER               PIC X(31)      VALUE SPACE.
                                                                        02380000
      **************************************************************    02390000
      * STORES THE BRANCH TOTAL LINE                               *    02400000
      * HOLDS THE TOTALS FOR THIS AND LAST YEAR-TO-DATE IN SALES   *    02410000
      * FOR THIS BRANCH AS WELL AS THE PERCENT DIFFERENCE          *    02420000
      * USED FOR OUTPUTTING                                        *    02430000
      **************************************************************    02440000
       01  BRANCH-TOTAL-LINE.                                           02450016
           05  FILLER               PIC X(36)   VALUE SPACE.            02460016
           05  FILLER               PIC X(16)   VALUE "  BRANCH TOTAL". 02470016
           05  BTL-SALES-THIS-YTD   PIC $$$,$$9.99-.                    02480016
           05  FILLER               PIC X(3)    VALUE SPACE.            02490016
           05  BTL-SALES-LAST-YTD   PIC $$$,$$9.99-.                    02500016
           05  FILLER               PIC X(3)    VALUE SPACE.            02510016
           05  BTL-CHANGE-AMOUNT    PIC $$$,$$9.99-.                    02520016
           05  FILLER               PIC X(2)    VALUE SPACE.            02530016
           05  BTL-CHANGE-PERCENT   PIC +++9.9.                         02540016
           05  BTL-CHANGE-PERCENT-R REDEFINES BTL-CHANGE-PERCENT        02550016
                                    PIC X(6).                           02560016
           05  FILLER               PIC X(31)   VALUE "**".             02570016
                                                                        02580000
      **************************************************************    02590000
      * STORES THE SALES REP TOTAL LINE                            *    02600000
      * HOLDS THE TOTALS FOR THIS AND LAST YEAR-TO-DATE IN SALES   *    02610000
      * FOR THIS REP AS WELL AS THE PERCENT DIFFERENCE             *    02620000
      * USED FOR OUTPUTTING                                        *    02630000
      **************************************************************    02640000
       01  SALESREP-TOTAL-LINE.                                         02650016
           05  FILLER               PIC X(36)   VALUE SPACE.            02660016
           05  FILLER               PIC X(16)   VALUE "SALESREP TOTAL". 02670016
           05  STL-SALES-THIS-YTD   PIC $$$,$$9.99-.                    02680016
           05  FILLER               PIC X(3)    VALUE SPACE.            02690016
           05  STL-SALES-LAST-YTD   PIC $$$,$$9.99-.                    02700016
           05  FILLER               PIC X(3)    VALUE SPACE.            02710016
           05  STL-CHANGE-AMOUNT    PIC $$$,$$9.99-.                    02720016
           05  FILLER               PIC X(2)    VALUE SPACE.            02730016
           05  STL-CHANGE-PERCENT   PIC +++9.9.                         02740016
           05  STL-CHANGE-PERCENT-R REDEFINES STL-CHANGE-PERCENT        02750016
                                    PIC X(6).                           02760016
           05  FILLER               PIC X(31)   VALUE "*".              02770016
      **************************************************************    02780000
      * STORES THE SECOND GRAND TOTAL LINE                         *    02790000
      * HOLDS THE TOTAL SALES FOR THIS AND LAST YEAR-TO-DATE,      *    02800000
      * THE TOTAL DIFFERENCE IN SALES MADE BETWEEN THE TWO YEARS   *    02810000
      * AND THE PERCENTAGE DIFFERENCE - FOR OUTPUTTING             *    02820000
      **************************************************************    02830000
       01  GRAND-TOTAL-LINE.                                            02840016
           05  FILLER               PIC X(36)    VALUE SPACE.           02850016
           05  FILLER               PIC X(14)    VALUE "   GRAND TOTAL".02860016
           05  GTL-SALES-THIS-YTD   PIC $,$$$,$$9.99-.                  02870016
           05  FILLER               PIC X(1)     VALUE SPACE.           02880016
           05  GTL-SALES-LAST-YTD   PIC $,$$$,$$9.99-.                  02890016
           05  FILLER               PIC X(1)     VALUE SPACE.           02900016
           05  GTL-CHANGE-AMOUNT    PIC $,$$$,$$9.99-.                  02910016
           05  FILLER               PIC X(2)     VALUE SPACE.           02920016
           05  GTL-CHANGE-PERCENT   PIC +++9.9.                         02930016
           05  GTL-CHANGE-PERCENT-R REDEFINES GTL-CHANGE-PERCENT        02940016
                                    PIC X(6).                           02950016
           05  FILLER               PIC X(31)    VALUE "***".           02960016
                                                                        02970000
       PROCEDURE DIVISION.                                              02980000
                                                                        02990000
      **************************************************************    03000000
      * OPENS AND CLOSES THE FILES AND DELEGATES THE WORK FOR      *    03010000
      * READING AND WRITING TO AND FROM THEM                       *    03020000
      **************************************************************    03030000
       000-PREPARE-SALES-REPORT.                                        03040000
           INITIALIZE SALESREP-TABLE.

           OPEN INPUT  CUSTMAST
                INPUT INPUT-SALESREP
                OUTPUT ORPT6000.
                                                                        03080000
           *> GRABS THE DATE AND TIME INFORMATION FOR                   03090000
           *> THE HEADER LINES                                          03100000
           PERFORM 100-FORMAT-REPORT-HEADING.
                                                                        03120000
           *> GRAB AND PRINT CUSTOMER SALES TO THE OUPUT FILE UNTIL     03130000
           *> THE END OF THE INPUT FILE                                 03140000
           PERFORM 200-PREPARE-SALES-LINES                              03150000
               UNTIL CUSTMAST-EOF-SWITCH = "Y".                         03160000

           PERFORM 205-LOAD-SALESREP-TABLE.


           *> OUTPUT THE GRAND TOTALS TO THE OUTPUT FILE                03180000
           PERFORM 300-PRINT-GRAND-TOTALS.                              03190000
                                                                        03200000
           CLOSE CUSTMAST                                               03210000
                 ORPT6000.                                              03220001
           STOP RUN.                                                    03230000
                                                                        03240000
      **************************************************************    03250000
      * FORMATS THE REPORT HEADER BY GRABBING THE DATE TIME AND    *    03260000
      * STORING IT IN THE RELEVENT HEADER DATA ITEMS               *    03270000
      **************************************************************    03280000
       100-FORMAT-REPORT-HEADING.                                       03290000
                                                                        03300000
           MOVE FUNCTION CURRENT-DATE TO CURRENT-DATE-AND-TIME.         03310000
                                                                        03320000
           *> MOVE THE RESULT OF THE DATE-TIME FUNCTION TO THE          03330000
           *> DIFFERENT HEADER LINE FIELDS ASSOCIATED WITH THEM         03340000
           *> SO WE CAN INCLUDE THE DATE IN THE OUTPUT HEADER           03350000
           MOVE CD-MONTH   TO HL1-MONTH.                                03360000
           MOVE CD-DAY     TO HL1-DAY.                                  03370000
           MOVE CD-YEAR    TO HL1-YEAR.                                 03380000
           MOVE CD-HOURS   TO HL2-HOURS.                                03390000
           MOVE CD-MINUTES TO HL2-MINUTES.                              03400000
                                                                        03410000
      **************************************************************    03420000
      * CALLS THE PARAGRAPH TO READ A LINE OF THE CUSTOMER RECORD  *    03430000
      * THEN CALLS THE PARAGRAPH TO PRINT THE LINE IF ITS NOT THE  *    03440000
      * TERMINATING LINE OF THE FILE                               *    03450000
      **************************************************************    03460000
       200-PREPARE-SALES-LINES.                                         03470000
                                                                        03480000
           *> GRAB THE NEXT LINE FROM THE CUSTOMER RECORD               03490000
           PERFORM 210-READ-CUSTOMER-RECORD.                            03500000
                                                                        03510000
           *> PERFORMS DUTIES BASED ON THE ENTRY                        03520000
           *>  * IF WE RUN OUT OF DATA PRINT THE SALES AND BRANCH TOTALS03530000
           *>  * IF IT'S THE FIRST RECORD PRINT THE CUSTOMER LINE AND   03540000
           *>    STORE THE CURRENT SALESREP AND BRANCH NUMBER TO THE OLD03550000
           *>  * IF THE BRANCH NUMBER IS GREATER THAN THE CURRENT ONE   03560000
           *>    THEN PRINT THE SALES REP LINE, BRANCH TOTAL LINE, AND  03570000
           *>    THEN THE NEW CUSTOMER'S LINE. AFTER UPDATE THE BRANCH  03580000
           *>    AND SALESREP NUMBERS                                   03590000
           *>  * IF THE SALES REP NUMBER IS GREATER THAN THE CURRENT ONE03600000
           *>    PRINT SALES LINE THEN THE CURRENT CUSTOMER LINE AFTER  03610000
           *>    UPDATE THE SALES REP NUMBER                            03620000
           *>  * IF NOTHING ELSE JUST PRINT THE CUSTOMER RECORD         03630000
           EVALUATE TRUE                                                03640000
               WHEN CUSTMAST-EOF                                        03650000
                   PERFORM 250-PRINT-SALESREP-LINE                      03660000
                   PERFORM 240-PRINT-BRANCH-LINE                        03670000
               WHEN FIRST-RECORD                                        03680000
                   PERFORM 220-PRINT-CUSTOMER-LINE                      03690000
                   SET FIRST-RECORD TO FALSE                            03700000
                 *>MOVE "N" TO FIRST-RECORD-SWITCH                      03710000
                   MOVE CM-SALESREP-NUMBER TO OLD-SALESREP-NUMBER       03720000
                   MOVE CM-BRANCH-NUMBER TO OLD-BRANCH-NUMBER           03730000
               WHEN CM-BRANCH-NUMBER > OLD-BRANCH-NUMBER                03740000
                   PERFORM 250-PRINT-SALESREP-LINE                      03750000
                   PERFORM 240-PRINT-BRANCH-LINE                        03760000
                   PERFORM 220-PRINT-CUSTOMER-LINE                      03770000
                   MOVE CM-SALESREP-NUMBER TO OLD-SALESREP-NUMBER       03780000
                   MOVE CM-BRANCH-NUMBER TO OLD-BRANCH-NUMBER           03790000
               WHEN NOT (CM-SALESREP-NUMBER = OLD-SALESREP-NUMBER)      03800000
                   PERFORM 250-PRINT-SALESREP-LINE                      03810000
                   PERFORM 220-PRINT-CUSTOMER-LINE                      03820000
                   MOVE CM-SALESREP-NUMBER TO OLD-SALESREP-NUMBER       03830000
               WHEN OTHER                                               03840000
                   PERFORM 220-PRINT-CUSTOMER-LINE                      03850000
           END-EVALUATE.


       205-LOAD-SALESREP-TABLE.
           PERFORM WITH TEST AFTER
                VARYING SRT-INDEX FROM 1 BY 1
                UNTIL SALESREP-EOF OR SRT-INDEX = 100
                PERFORM 110-READ-SALESREP-TABLE-RECORD
                IF NOT SALESREP-EOF
                     MOVE SM-SALESREP-NUMBER
                          TO SALESREP-NUMBER (SRT-INDEX)
                     MOVE SM-SALESREP-NAME TO SALESREP-NAME (SRT-INDEX)
                END-IF
           END-PERFORM.

       110-READ-SALESREP-TABLE-RECORD.
           READ INPUT-SALESREP
                AT END
                    SET SALESREP-EOF TO TRUE.

                                                                        03870000
      **************************************************************    03880000
      * READS A LINE OF THE INPUT FILE AND IF ITS THE LAST ONE     *    03890000
      * UPDATES THE CUSTOMER-EOF-SWITCH (END-OF-FILE)              *    03900000
      **************************************************************    03910000
       210-READ-CUSTOMER-RECORD.                                        03920000
                                                                        03930000
           READ CUSTMAST                                                03940000
               AT END                                                   03950000
                   MOVE "Y" TO CUSTMAST-EOF-SWITCH.                     03960000
                                                                        03970000
      **************************************************************    03980000
      * PRINTS THE CURRENT CUSTOMER LINE TO THE OUTPUT FILE        *    03990000
      * UPDATES THE LINE COUNTER SO IT KNOWS WHEN IT HAS TO        *    04000000
      * REPRINT THE HEADER LINES FOR A NEW PAGE                    *    04010000
      **************************************************************    04020000
       220-PRINT-CUSTOMER-LINE.                                         04030000
                                                                        04040000
           *> IF INFORMATION WE HAVE PRINTED EXCEEDS THE PAGE LIMIT     04050000
           *> WE REPRINT THE HEADERS FOR THE NEW PAGE                   04060000
           IF LINE-COUNT >= LINES-ON-PAGE                               04070000
               PERFORM 230-PRINT-HEADING-LINES.                         04080000
                                                                        04090000
           *> PERFROMS DUTIES BASED ON THE ENTRY                        04100000
           *>  * IF IT'S THE FIRST RECORD PRINT THE BRANCH NUMBER       04110000
           *>    AND THE SALESREP NUMBER                                04120000
           *>  * IF IT'S A NEW BRANCH PRINT THE BRANCH NUMBER AND       04130000
           *>    SALES REP NUMBER                                       04140000
           *>  * IF IT'S A NEW SALES REP PRINT THE SALESREP NUMBER      04150000
           *>  * OTHERWISE PRINT SPACES IN THOSE LINES FOR PADDING      04160000
           EVALUATE TRUE                                                04170000
               WHEN FIRST-RECORD                                        04180000
                   MOVE CM-BRANCH-NUMBER TO CL-BRANCH-NUMBER            04190000
                   MOVE CM-SALESREP-NUMBER TO CL-SALESREP-NUMBER        04200000
                   PERFORM 223-MOVE-SALESREP-NUMBER                     04210008
               WHEN CM-BRANCH-NUMBER > OLD-BRANCH-NUMBER                04220000
                   MOVE CM-BRANCH-NUMBER TO CL-BRANCH-NUMBER            04230000
                   MOVE CM-SALESREP-NUMBER TO CL-SALESREP-NUMBER        04240000
                   PERFORM 223-MOVE-SALESREP-NUMBER                     04250008
               WHEN NOT (CM-SALESREP-NUMBER = OLD-SALESREP-NUMBER)      04260000
                   MOVE CM-SALESREP-NUMBER TO CL-SALESREP-NUMBER        04270000
                   PERFORM 223-MOVE-SALESREP-NUMBER                     04280008
                   MOVE SPACES TO CL-BRANCH-NUMBER                      04290000
               WHEN OTHER                                               04300000
                   MOVE SPACES TO CL-BRANCH-NUMBER                      04310000
                   MOVE SPACES TO CL-SALESREP-NUMBER                    04320000
           END-EVALUATE.                                                04330000
                                                                        04340000
           *> MOVE THE DATA PULLED FROM THE INPUT FILE INTO THE         04350000
           *> CUSTOMER LINE RECORD FOR LATER OUTPUT                     04360000
           MOVE CM-CUSTOMER-NUMBER  TO CL-CUSTOMER-NUMBER.              04370000
           MOVE CM-CUSTOMER-NAME    TO CL-CUSTOMER-NAME.                04380000
           MOVE CM-SALES-THIS-YTD   TO CL-SALES-THIS-YTD.               04390000
           MOVE CM-SALES-LAST-YTD   TO CL-SALES-LAST-YTD.               04400000
                                                                        04410000
           *> CALCULATE THE DIFFERENCE BETWEEN THIS YEAR'S SALES AND    04420000
           *> AND LAST THEN SAVE THESE RESULT TO CHANGE-AMOUNT AND      04430000
           COMPUTE CHANGE-AMOUNT =                                      04440000
               CM-SALES-THIS-YTD - CM-SALES-LAST-YTD.                   04450000
           MOVE CHANGE-AMOUNT TO CL-CHANGE-AMOUNT.                      04460000
                                                                        04470000
           *> CALCULATE THE PERCENT FOR THE CHANGE IN SALES BETWEEN     04480000
           *> THIS AND LAST YTD, IF THERE WAS NO LAST YEAR SALES        04490000
           *> NUMBER WE MOVE 999.9 TO THE PERECENTAGE SINCE IT'S        04500000
           *> A DIVIDE BY ZERO ERROR OTHERWISE                          04510000
           IF CM-SALES-LAST-YTD = ZERO                                  04520000
               MOVE "  N/A " TO CL-CHANGE-PERCENT-R                     04530000
           ELSE                                                         04540000
               COMPUTE CL-CHANGE-PERCENT ROUNDED =                      04550000
                   CHANGE-AMOUNT * 100 / CM-SALES-LAST-YTD              04560000
                   ON SIZE ERROR                                        04570000
                       MOVE "OVRFLW" TO CL-CHANGE-PERCENT-R.            04580000
                                                                        04590000
           *> PRINT THIS CUSTOMERS INFORMATION TO THE OUTPUT FILE       04600000
           MOVE CUSTOMER-LINE TO PRINT-AREA.                            04610000
           PERFORM 225-WRITE-REPORT-LINE.                               04620000
                                                                        04630000
           *> ADD THIS CUSTOMERS SALES TO THE SALESREP TOTALS           04640000
           ADD CM-SALES-THIS-YTD TO SALESREP-TOTAL-THIS-YTD.            04650000
           ADD CM-SALES-LAST-YTD TO SALESREP-TOTAL-LAST-YTD.            04660000
                                                                        04670008
      **************************************************************    04680008
      * TODO                                                       *    04690008
      *                                                            *    04700008
      **************************************************************    04710008
       223-MOVE-SALESREP-NUMBER.                                        04720010
           SET SRT-INDEX TO 1.                                          04730010
           SEARCH SALESREP-GROUP                                        04740010
               AT END                                                   04750010
                   MOVE "UNKNOWN" TO CL-SALESREP-NAME                   04760010
               WHEN SALESREP-NUMBER (SRT-INDEX) = CM-SALESREP-NUMBER    04770010
                   MOVE SALESREP-NAME (SRT-INDEX) TO CL-SALESREP-NAME   04780010
           END-SEARCH.                                                  04790010
                                                                        04800008
      **************************************************************    04810000
      * PRINT ALL THE HEADER LINES TO THE OUTPUT FILE, RAN ONCE    *    04820000
      * FOR EVERY PAGE                                             *    04830000
      **************************************************************    04840000
       225-WRITE-REPORT-LINE.                                           04850000
           WRITE PRINT-AREA.                                            04860000
           ADD 1 TO LINE-COUNT.                                         04870000
                                                                        04880000
      **************************************************************    04890000
      * PRINT ALL THE HEADER LINES TO THE OUTPUT FILE, RAN ONCE    *    04900000
      * FOR EVERY PAGE                                             *    04910000
      **************************************************************    04920000
       230-PRINT-HEADING-LINES.                                         04930000
                                                                        04940000
           *> HEADERS ARE PLACED AT THE START OF EVERY PAGE             04950000
           *> SO WE INCREASE THE PAGE COUNT HERE                        04960000
           ADD 1 TO PAGE-COUNT.                                         04970000
           MOVE PAGE-COUNT     TO HL1-PAGE-NUMBER.                      04980000
                                                                        04990000
           *> PRINT EACH HEADER LINE TO THE OUTPUT FILE                 05000000
           MOVE HEADING-LINE-1 TO PRINT-AREA.                           05010000
           WRITE PRINT-AREA.                                            05020000
           MOVE HEADING-LINE-2 TO PRINT-AREA.                           05030000
           WRITE PRINT-AREA.                                            05040000
           MOVE HEADING-LINE-3 TO PRINT-AREA.                           05050000
           WRITE PRINT-AREA.                                            05060000
           MOVE HEADING-LINE-4 TO PRINT-AREA.                           05070000
           WRITE PRINT-AREA.                                            05080000
           MOVE HEADING-LINE-5 TO PRINT-AREA.                           05090000
           WRITE PRINT-AREA.                                            05100000
           MOVE HEADING-LINE-6 TO PRINT-AREA.                           05110000
           WRITE PRINT-AREA.                                            05120000
                                                                        05130000
           *> RESET THE LINE COUNTER SINCE EVERY HEADER IS THE START    05140000
           *> OF A NEW PAGE                                             05150000
           MOVE ZERO TO LINE-COUNT.                                     05160000
                                                                        05170000
      **************************************************************    05180000
      * PRINTS THE CURRENT BRANCH LINE TOTALS, RAN ONCE FOR EVERY  *    05190000
      * BRANCH. ALSO CALCULATES THE CHANGE IN THE BRANCH           *    05200000
      **************************************************************    05210000
       240-PRINT-BRANCH-LINE.                                           05220000
                                                                        05230000
           *> MOVE THE BRANCH TOTALS TO THE BRANCH TOTAL LINE           05240000
           MOVE BRANCH-TOTAL-THIS-YTD TO BTL-SALES-THIS-YTD.            05250000
           MOVE BRANCH-TOTAL-LAST-YTD TO BTL-SALES-LAST-YTD.            05260000
                                                                        05270000
           *> CALCULATE THE CHANGE BETWEEN THIS-YTD AND LAST            05280000
           *> FOR THE CURRENT BRANCH AND ADD IT TO THE TOTAL LINE       05290000
           COMPUTE CHANGE-AMOUNT =                                      05300000
               BRANCH-TOTAL-THIS-YTD - BRANCH-TOTAL-LAST-YTD.           05310000
           MOVE CHANGE-AMOUNT TO BTL-CHANGE-AMOUNT.                     05320000
                                                                        05330000
           *> CALCULATE THE CHANGE PERCENT BETWEEN YTD'S                05340000
           *> THEN MOVE TO THE BRANCH TOTAL LINE                        05350000
           IF BRANCH-TOTAL-LAST-YTD = ZERO                              05360000
               MOVE "  N/A " TO BTL-CHANGE-PERCENT-R                    05370000
           ELSE                                                         05380000
               COMPUTE BTL-CHANGE-PERCENT ROUNDED =                     05390000
                   CHANGE-AMOUNT * 100 / BRANCH-TOTAL-LAST-YTD          05400000
                   ON SIZE ERROR                                        05410000
                       MOVE "OVRFLW" TO BTL-CHANGE-PERCENT-R.           05420000
                                                                        05430000
           *> PRINT BRANCH LINE                                         05440000
           MOVE BRANCH-TOTAL-LINE TO PRINT-AREA.                        05450000
           PERFORM 225-WRITE-REPORT-LINE.                               05460000
                                                                        05470000
           *> WRITE A BLANK SPACER LINE                                 05480000
           MOVE SPACES TO PRINT-AREA.                                   05490000
           PERFORM 225-WRITE-REPORT-LINE.                               05500000
                                                                        05510000
           *> ADD THE BRANCH TOTALS TO THE GRAND TOTALS                 05520000
           ADD BRANCH-TOTAL-THIS-YTD TO GRAND-TOTAL-THIS-YTD.           05530000
           ADD BRANCH-TOTAL-LAST-YTD TO GRAND-TOTAL-LAST-YTD.           05540000
                                                                        05550000
           *> ZERO OUT THE BRANCH TOTALS                                05560000
           INITIALIZE BRANCH-TOTAL-THIS-YTD                             05570009
                      BRANCH-TOTAL-LAST-YTD.                            05580009
                                                                        05590000
      **************************************************************    05600000
      * PRINTS THE CURRENT SALESREP'S TOTALS, RAN ONCE FOR EVERY   *    05610000
      * SALESREP. ALSO CALCULATES THE CHANGE BETWEEN YEARS         *    05620000
      **************************************************************    05630000
       250-PRINT-SALESREP-LINE.                                         05640000
                                                                        05650000
           *> MOVE THE SALESREP TOTALS TO THE SALESREP TOTAL LINE       05660000
           MOVE SALESREP-TOTAL-THIS-YTD TO STL-SALES-THIS-YTD.          05670000
           MOVE SALESREP-TOTAL-LAST-YTD TO STL-SALES-LAST-YTD.          05680000
                                                                        05690000
           *> CALCULATE THE CHANGE BETWEEN THIS-YTD AND LAST            05700000
           *> FOR THE CURRENT SALESREP AND ADD IT TO THE TOTAL LINE     05710000
           COMPUTE CHANGE-AMOUNT =                                      05720000
               SALESREP-TOTAL-THIS-YTD - SALESREP-TOTAL-LAST-YTD.       05730000
           MOVE CHANGE-AMOUNT TO STL-CHANGE-AMOUNT.                     05740000
                                                                        05750000
           *> CALCULATE THE CHANGE PERCENT BETWEEN YTD'S                05760000
           *> THEN MOVE TO THE SALESREP TOTAL LINE                      05770000
           IF SALESREP-TOTAL-LAST-YTD = ZERO                            05780000
               MOVE "  N/A " TO STL-CHANGE-PERCENT-R                    05790000
           ELSE                                                         05800000
               COMPUTE STL-CHANGE-PERCENT ROUNDED =                     05810000
                   CHANGE-AMOUNT * 100 / SALESREP-TOTAL-LAST-YTD        05820000
                   ON SIZE ERROR                                        05830000
                       MOVE "OVRFLW" TO STL-CHANGE-PERCENT-R.           05840000
                                                                        05850000
           *> PRINT SALESREP LINE                                       05860000
           MOVE SALESREP-TOTAL-LINE TO PRINT-AREA.                      05870000
           PERFORM 225-WRITE-REPORT-LINE.                               05880000
                                                                        05890000
           *> PRINT A SPACER LINE                                       05900000
           MOVE SPACES TO PRINT-AREA.                                   05910000
           PERFORM 225-WRITE-REPORT-LINE.                               05920000
                                                                        05930000
           *> ADD THE SALESREP TOTALS TO THE BRANCH TOTALS              05940000
           *> WHEN A BRANCH IS PRINTED THEN THOSE TOTALS ARE MOVED      05950000
           *> TO THE GRAND TOTALS                                       05960000
           *> CUSTOMER->SALESREP->BRANCH->GRAND-TOTAL                   05970000
           ADD SALESREP-TOTAL-THIS-YTD TO BRANCH-TOTAL-THIS-YTD.        05980000
           ADD SALESREP-TOTAL-LAST-YTD TO BRANCH-TOTAL-LAST-YTD.        05990000
                                                                        06000000
           *> ZERO OUT THE SALESREP TOTALS                              06010000
           MOVE ZERO TO SALESREP-TOTAL-THIS-YTD.                        06020000
           MOVE ZERO TO SALESREP-TOTAL-LAST-YTD.                        06030000
      **************************************************************    06040000
      * PRINTS THE GRAND TOTALS FOR ALL THE CUSTOMERS, RAN ONCE    *    06050000
      * AT THE VERY END OF THE PROGRAM WHEN ALL CUSTOMERS HAVE     *    06060000
      * BEEN PRINTED                                               *    06070000
      **************************************************************    06080000
       300-PRINT-GRAND-TOTALS.                                          06090000
                                                                        06100000
           *> MOVE THE GRAND TOTALS FOR THE SALES TO THE                06110000
           *> OUTPUT LINE FOR GRAND TOTALS                              06120000
           MOVE GRAND-TOTAL-THIS-YTD TO GTL-SALES-THIS-YTD.             06130000
           MOVE GRAND-TOTAL-LAST-YTD TO GTL-SALES-LAST-YTD.             06140000
                                                                        06150000
           *> COMPUTE THE GRAND TOTAL FOR THE CHANGE AMOUNT             06160000
           COMPUTE CHANGE-AMOUNT =                                      06170000
               GRAND-TOTAL-THIS-YTD - GRAND-TOTAL-LAST-YTD.             06180000
           MOVE CHANGE-AMOUNT TO GTL-CHANGE-AMOUNT.                     06190000
                                                                        06200000
           *> CALCULATE THE TOTAL CHANGE IN PERCENT BETWEEN             06210000
           *> THIS YTD AND LAST YTD FOR ALL CUSTOMERS                   06220000
           *> IF THERE WAS NO LAST YEAR FOR ANYONE DEFAULT TO           06230000
           *> A PERCENT OF 999.9 TO AVOID DIVIDE BY ZERO ERROR          06240000
           IF GRAND-TOTAL-LAST-YTD = ZERO                               06250000
               MOVE "  N/A " TO GTL-CHANGE-PERCENT-R                    06260000
           ELSE                                                         06270000
               COMPUTE GTL-CHANGE-PERCENT ROUNDED =                     06280000
                   CHANGE-AMOUNT * 100 / GRAND-TOTAL-LAST-YTD           06290000
                   ON SIZE ERROR                                        06300000
                       MOVE "OVRFLW" TO GTL-CHANGE-PERCENT-R.           06310000
                                                                        06320000
           *> PRINT THE GRAND-TOTAL TO THE OUTPUT FILE                  06330000
           MOVE GRAND-TOTAL-LINE TO PRINT-AREA.                         06340000
           PERFORM 225-WRITE-REPORT-LINE.                               06350000
