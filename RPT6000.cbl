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
           SELECT CUSTMAST ASSIGN TO CUSTMAST.                          00170000
           SELECT ORPT6000 ASSIGN TO RPT6000.                           00180001
                                                                        00190000
       DATA DIVISION.                                                   00200000
                                                                        00210000
       FILE SECTION.                                                    00220000
                                                                        00230000
      **************************************************************    00240000
      * INPUT FILE                                                 *    00250000
      **************************************************************    00260000
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
           05  FILLER                  PIC X(87).                       00390000
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
       01 SALESREP-TABLE VALUE "WHATEVER12 11JSMITH   12TTHOMAS 14 BJ   00590007
      -    "ONES   18GFRANKLIN 21RWILLIAMS ".                           00600007
           05 SALESREP-GROUP OCCURS 6 TIMES                             00610007
                             INDEXED BY SRT-INDEX.                      00620007
               10 SALESREP-NUMBER   PIC 9(2).                           00630007
               10 SALESREP-NAME     PIC X(10).                          00640011
      **************************************************************    00650000
      * SWITCHES FOR END OF FILE AND FIRST RECORD                  *    00660000
      **************************************************************    00670000
       01  SWITCHES.                                                    00680000
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
           05  FILLER          PIC X(16)   VALUE SPACE.                 01370000
           05  FILLER          PIC X(20)   VALUE "YEAR-TO-DATE SALES R".01380000
           05  FILLER          PIC X(10)   VALUE "EPORT     ".          01390000
           05  FILLER          PIC X(19)   VALUE SPACE.                 01400000
           05  FILLER          PIC X(8)    VALUE "  PAGE: ".            01410000
           05  HL1-PAGE-NUMBER PIC ZZZ9.                                01420000
           05  FILLER          PIC X(39)   VALUE SPACE.                 01430000
                                                                        01440000
      **************************************************************    01450000
      * STORES THE SECOND HEADER LINE INFORMATION                  *    01460000
      * HOLDS THE TIME AND THE PROGRAM ID                          *    01470000
      **************************************************************    01480000
       01  HEADING-LINE-2.                                              01490000
           05  FILLER          PIC X(7)    VALUE "TIME:  ".             01500000
           05  HL2-HOURS       PIC 9(2).                                01510000
           05  FILLER          PIC X(1)    VALUE ":".                   01520000
           05  HL2-MINUTES     PIC 9(2).                                01530000
           05  FILLER          PIC X(72)   VALUE SPACE.                 01540000
           05  FILLER          PIC X(10)   VALUE "RPT6000".             01550001
           05  FILLER          PIC X(39)   VALUE SPACE.                 01560000
                                                                        01570000
      **************************************************************    01580000
      * STORES THE THIRD HEADER LINE USED TO DISPLAY A LINE SPACER *    01590000
      **************************************************************    01600000
       01  HEADING-LINE-3.                                              01610000
           05 FILLER               PIC X(130)   VALUE SPACE.            01620000
                                                                        01630000
      **************************************************************    01640000
      * STORES THE FOURTH HEADER LINE INFORMATION                  *    01650000
      * HOLDS THE DIFFERENT COLUMN NAMES - SOME ARE SPLIT ACROSS   *    01660000
      * THE NEXT HEADER LINE                                       *    01670000
      **************************************************************    01680000
       01  HEADING-LINE-4.                                              01690000
           05  FILLER      PIC X(7)    VALUE "BRANCH ".                 01700000
           05  FILLER      PIC X(6)    VALUE "SALES ".                  01710000
           05  FILLER      PIC X(20)   VALUE "CUST                ".    01720000
           05  FILLER      PIC X(20)   VALUE "            SALES   ".    01730000
           05  FILLER      PIC X(20)   VALUE "      SALES         ".    01740000
           05  FILLER      PIC X(20)   VALUE "CHANGE     CHANGE   ".    01750000
           05  FILLER      PIC X(44)   VALUE SPACE.                     01760000
                                                                        01770000
      **************************************************************    01780000
      * STORES THE FIFTH HEADER LINE INFORMATION                   *    01790000
      * HOLDS SOME OF THE COLUMN NAMES AS WELL AS THE OTHER HALF   *    01800000
      * OF COLUMN NAMES THAT STARTED IN THE LAST HEADER LINE       *    01810000
      **************************************************************    01820000
       01  HEADING-LINE-5.                                              01830000
           05  FILLER      PIC X(8)    VALUE " NUM    ".                01840000
           05  FILLER      PIC X(5)    VALUE "REP  ".                   01850000
           05  FILLER      PIC X(20)   VALUE "NUM    CUSTOMER NAME".    01860000
           05  FILLER      PIC X(20)   VALUE "           THIS YTD ".    01870000
           05  FILLER      PIC X(20)   VALUE "     LAST YTD       ".    01880000
           05  FILLER      PIC X(20)   VALUE "AMOUNT    PERCENT   ".    01890000
           05  FILLER      PIC X(44)   VALUE SPACE.                     01900000
                                                                        01910000
      **************************************************************    01920000
      * STORES THE SIXTH HEADER LINE WHICH IS USED FOR SPACING     *    01930000
      **************************************************************    01940000
       01  HEADING-LINE-6.                                              01950000
           05  FILLER      PIC X(130)  VALUE SPACES.                    01960000
                                                                        01970000
      **************************************************************    01980000
      * STORES INFORMATION ABOUT CURRENT CUSTOMER                  *    01990000
      * HOLDS THE BRANCH NUMBER, SALES REP NUMBER, CUSTOMER NUMBER,*    02000000
      * CUSTOMER NAME, SALES THIS AND LAST YEAR-TO-DATE,           *    02010000
      * DIFFERENCE BETWEEN THIS YEARS SALES AND LAST, AND THE      *    02020000
      * DIFFERENCE IN PERCENT.                                     *    02030000
      **************************************************************    02040000
       01  CUSTOMER-LINE.                                               02050000
           05  FILLER              PIC X(2)     VALUE SPACE.            02060000
           05  CL-BRANCH-NUMBER    PIC X(2).                            02070000
           05  FILLER              PIC X(4)     VALUE SPACE.            02080000
           05  CL-SALESREP-NUMBER  PIC X(2).                            02090000
           05  FILLER              PIC X(3)     VALUE SPACE.            02100000
           05  CL-CUSTOMER-NUMBER  PIC 9(5).                            02110000
           05  FILLER              PIC X(2)     VALUE SPACE.            02120000
           05  CL-SALESREP-NAME    PIC X(10).                           02130008
           05  FILLER              PIC X(2)     VALUE SPACE.            02140010
           05  CL-CUSTOMER-NAME    PIC X(20).                           02150000
           05  FILLER              PIC X(3)     VALUE SPACE.            02160000
           05  CL-SALES-THIS-YTD   PIC ZZ,ZZ9.99-.                      02170000
           05  FILLER              PIC X(4)     VALUE SPACE.            02180000
           05  CL-SALES-LAST-YTD   PIC ZZ,ZZ9.99-.                      02190000
           05  FILLER              PIC X(4)     VALUE SPACE.            02200000
           05  CL-CHANGE-AMOUNT    PIC ZZ,ZZ9.99-.                      02210000
           05  FILLER              PIC X(3)     VALUE SPACE.            02220000
           05  CL-CHANGE-PERCENT   PIC ---9.9.                          02230003
           05  CL-CHANGE-PERCENT-R REDEFINES CL-CHANGE-PERCENT          02240003
                                   PIC X(6).                            02250003
           05  FILLER              PIC X(47)    VALUE SPACE.            02260000
                                                                        02270000
      **************************************************************    02280000
      * STORES THE BRANCH TOTAL LINE                               *    02290000
      * HOLDS THE TOTALS FOR THIS AND LAST YEAR-TO-DATE IN SALES   *    02300000
      * FOR THIS BRANCH AS WELL AS THE PERCENT DIFFERENCE          *    02310000
      * USED FOR OUTPUTTING                                        *    02320000
      **************************************************************    02330000
       01  BRANCH-TOTAL-LINE.                                           02340000
           05  FILLER                PIC X(28)    VALUE SPACE.          02350004
           05  FILLER                PIC X(14)    VALUE "BRANCH TOTAL". 02360004
           05  BTL-SALES-THIS-YTD    PIC $$$,$$9.99-.                   02370005
           05  FILLER                PIC X(3)     VALUE SPACE.          02380004
           05  BTL-SALES-LAST-YTD    PIC $$$,$$9.99-.                   02390005
           05  FILLER                PIC X(3)     VALUE SPACE.          02400004
           05  BTL-CHANGE-AMOUNT     PIC $$$,$$9.99-.                   02410006
           05  FILLER                PIC X(3)     VALUE SPACE.          02420004
           05  BTL-CHANGE-PERCENT    PIC +++9.9.                        02430004
           05  BTL-CHANGE-PERCENT-R  REDEFINES BTL-CHANGE-PERCENT       02440004
                                     PIC X(6).                          02450004
           05  FILLER                PIC X(48)    VALUE " **".          02460004
                                                                        02470000
      **************************************************************    02480000
      * STORES THE SALES REP TOTAL LINE                            *    02490000
      * HOLDS THE TOTALS FOR THIS AND LAST YEAR-TO-DATE IN SALES   *    02500000
      * FOR THIS REP AS WELL AS THE PERCENT DIFFERENCE             *    02510000
      * USED FOR OUTPUTTING                                        *    02520000
      **************************************************************    02530000
       01  SALESREP-TOTAL-LINE.                                         02540000
           05  FILLER               PIC X(28)    VALUE SPACE.           02550005
           05  FILLER               PIC X(14)    VALUE "SALESREP TOTAL".02560005
           05  STL-SALES-THIS-YTD   PIC $$$,$$9.99-.                    02570005
           05  FILLER               PIC X(3)     VALUE SPACE.           02580005
           05  STL-SALES-LAST-YTD   PIC $$$,$$9.99-.                    02590005
           05  FILLER               PIC X(3)     VALUE SPACE.           02600005
           05  STL-CHANGE-AMOUNT    PIC $$$,$$9.99-.                    02610006
           05  FILLER               PIC X(3)     VALUE SPACE.           02620005
           05  STL-CHANGE-PERCENT   PIC ZZ9.9-.                         02630005
           05  STL-CHANGE-PERCENT-R REDEFINES STL-CHANGE-PERCENT        02640005
                                    PIC X(6).                           02650005
           05  FILLER               PIC X(48)    VALUE " *".            02660005
      **************************************************************    02670000
      * STORES THE SECOND GRAND TOTAL LINE                         *    02680000
      * HOLDS THE TOTAL SALES FOR THIS AND LAST YEAR-TO-DATE,      *    02690000
      * THE TOTAL DIFFERENCE IN SALES MADE BETWEEN THE TWO YEARS   *    02700000
      * AND THE PERCENTAGE DIFFERENCE - FOR OUTPUTTING             *    02710000
      **************************************************************    02720000
       01  GRAND-TOTAL-LINE.                                            02730000
           05  FILLER              PIC X(28)    VALUE SPACE.            02740000
           05  FILLER              PIC X(12)    VALUE "GRAND TOTAL ".   02750000
           05  GTL-SALES-THIS-YTD  PIC $,$$$,$$9.99-.                   02760006
           05  FILLER              PIC X(1)     VALUE SPACE.            02770000
           05  GTL-SALES-LAST-YTD  PIC $,$$$,$$9.99-.                   02780006
           05  FILLER              PIC X        VALUE SPACE.            02790000
           05  GTL-CHANGE-AMOUNT   PIC $,$$$,$$9.99-.                   02800006
           05  FILLER              PIC X(3)     VALUE SPACE.            02810000
           05  GTL-CHANGE-PERCENT  PIC ZZ9.9-.                          02820000
           05  GTL-CHANGE-PERCENT-R REDEFINES GTL-CHANGE-PERCENT        02830007
                                   PIC X(6).                            02840007
           05  FILLER              PIC X(43)    VALUE " ***".           02850000
                                                                        02860000
       PROCEDURE DIVISION.                                              02870000
                                                                        02880000
      **************************************************************    02890000
      * OPENS AND CLOSES THE FILES AND DELEGATES THE WORK FOR      *    02900000
      * READING AND WRITING TO AND FROM THEM                       *    02910000
      **************************************************************    02920000
       000-PREPARE-SALES-REPORT.                                        02930000
                                                                        02940000
           OPEN INPUT  CUSTMAST                                         02950000
                OUTPUT ORPT6000.                                        02960001
                                                                        02970000
           *> GRABS THE DATE AND TIME INFORMATION FOR                   02980000
           *> THE HEADER LINES                                          02990000
           PERFORM 100-FORMAT-REPORT-HEADING.                           03000000
                                                                        03010000
           *> GRAB AND PRINT CUSTOMER SALES TO THE OUPUT FILE UNTIL     03020000
           *> THE END OF THE INPUT FILE                                 03030000
           PERFORM 200-PREPARE-SALES-LINES                              03040000
               UNTIL CUSTMAST-EOF-SWITCH = "Y".                         03050000
                                                                        03060000
           *> OUTPUT THE GRAND TOTALS TO THE OUTPUT FILE                03070000
           PERFORM 300-PRINT-GRAND-TOTALS.                              03080000
                                                                        03090000
           CLOSE CUSTMAST                                               03100000
                 ORPT6000.                                              03110001
           STOP RUN.                                                    03120000
                                                                        03130000
      **************************************************************    03140000
      * FORMATS THE REPORT HEADER BY GRABBING THE DATE TIME AND    *    03150000
      * STORING IT IN THE RELEVENT HEADER DATA ITEMS               *    03160000
      **************************************************************    03170000
       100-FORMAT-REPORT-HEADING.                                       03180000
                                                                        03190000
           MOVE FUNCTION CURRENT-DATE TO CURRENT-DATE-AND-TIME.         03200000
                                                                        03210000
           *> MOVE THE RESULT OF THE DATE-TIME FUNCTION TO THE          03220000
           *> DIFFERENT HEADER LINE FIELDS ASSOCIATED WITH THEM         03230000
           *> SO WE CAN INCLUDE THE DATE IN THE OUTPUT HEADER           03240000
           MOVE CD-MONTH   TO HL1-MONTH.                                03250000
           MOVE CD-DAY     TO HL1-DAY.                                  03260000
           MOVE CD-YEAR    TO HL1-YEAR.                                 03270000
           MOVE CD-HOURS   TO HL2-HOURS.                                03280000
           MOVE CD-MINUTES TO HL2-MINUTES.                              03290000
                                                                        03300000
      **************************************************************    03310000
      * CALLS THE PARAGRAPH TO READ A LINE OF THE CUSTOMER RECORD  *    03320000
      * THEN CALLS THE PARAGRAPH TO PRINT THE LINE IF ITS NOT THE  *    03330000
      * TERMINATING LINE OF THE FILE                               *    03340000
      **************************************************************    03350000
       200-PREPARE-SALES-LINES.                                         03360000
                                                                        03370000
           *> GRAB THE NEXT LINE FROM THE CUSTOMER RECORD               03380000
           PERFORM 210-READ-CUSTOMER-RECORD.                            03390000
                                                                        03400000
           *> PERFORMS DUTIES BASED ON THE ENTRY                        03410000
           *>  * IF WE RUN OUT OF DATA PRINT THE SALES AND BRANCH TOTALS03420000
           *>  * IF IT'S THE FIRST RECORD PRINT THE CUSTOMER LINE AND   03430000
           *>    STORE THE CURRENT SALESREP AND BRANCH NUMBER TO THE OLD03440000
           *>  * IF THE BRANCH NUMBER IS GREATER THAN THE CURRENT ONE   03450000
           *>    THEN PRINT THE SALES REP LINE, BRANCH TOTAL LINE, AND  03460000
           *>    THEN THE NEW CUSTOMER'S LINE. AFTER UPDATE THE BRANCH  03470000
           *>    AND SALESREP NUMBERS                                   03480000
           *>  * IF THE SALES REP NUMBER IS GREATER THAN THE CURRENT ONE03490000
           *>    PRINT SALES LINE THEN THE CURRENT CUSTOMER LINE AFTER  03500000
           *>    UPDATE THE SALES REP NUMBER                            03510000
           *>  * IF NOTHING ELSE JUST PRINT THE CUSTOMER RECORD         03520000
           EVALUATE TRUE                                                03530000
               WHEN CUSTMAST-EOF                                        03540000
                   PERFORM 250-PRINT-SALESREP-LINE                      03550000
                   PERFORM 240-PRINT-BRANCH-LINE                        03560000
               WHEN FIRST-RECORD                                        03570000
                   PERFORM 220-PRINT-CUSTOMER-LINE                      03580000
                   SET FIRST-RECORD TO FALSE                            03590000
                 *>MOVE "N" TO FIRST-RECORD-SWITCH                      03600000
                   MOVE CM-SALESREP-NUMBER TO OLD-SALESREP-NUMBER       03610000
                   MOVE CM-BRANCH-NUMBER TO OLD-BRANCH-NUMBER           03620000
               WHEN CM-BRANCH-NUMBER > OLD-BRANCH-NUMBER                03630000
                   PERFORM 250-PRINT-SALESREP-LINE                      03640000
                   PERFORM 240-PRINT-BRANCH-LINE                        03650000
                   PERFORM 220-PRINT-CUSTOMER-LINE                      03660000
                   MOVE CM-SALESREP-NUMBER TO OLD-SALESREP-NUMBER       03670000
                   MOVE CM-BRANCH-NUMBER TO OLD-BRANCH-NUMBER           03680000
               WHEN NOT (CM-SALESREP-NUMBER = OLD-SALESREP-NUMBER)      03690000
                   PERFORM 250-PRINT-SALESREP-LINE                      03700000
                   PERFORM 220-PRINT-CUSTOMER-LINE                      03710000
                   MOVE CM-SALESREP-NUMBER TO OLD-SALESREP-NUMBER       03720000
               WHEN OTHER                                               03730000
                   PERFORM 220-PRINT-CUSTOMER-LINE                      03740000
           END-EVALUATE.                                                03750000
                                                                        03760000
      **************************************************************    03770000
      * READS A LINE OF THE INPUT FILE AND IF ITS THE LAST ONE     *    03780000
      * UPDATES THE CUSTOMER-EOF-SWITCH (END-OF-FILE)              *    03790000
      **************************************************************    03800000
       210-READ-CUSTOMER-RECORD.                                        03810000
                                                                        03820000
           READ CUSTMAST                                                03830000
               AT END                                                   03840000
                   MOVE "Y" TO CUSTMAST-EOF-SWITCH.                     03850000
                                                                        03860000
      **************************************************************    03870000
      * PRINTS THE CURRENT CUSTOMER LINE TO THE OUTPUT FILE        *    03880000
      * UPDATES THE LINE COUNTER SO IT KNOWS WHEN IT HAS TO        *    03890000
      * REPRINT THE HEADER LINES FOR A NEW PAGE                    *    03900000
      **************************************************************    03910000
       220-PRINT-CUSTOMER-LINE.                                         03920000
                                                                        03930000
           *> IF INFORMATION WE HAVE PRINTED EXCEEDS THE PAGE LIMIT     03940000
           *> WE REPRINT THE HEADERS FOR THE NEW PAGE                   03950000
           IF LINE-COUNT >= LINES-ON-PAGE                               03960000
               PERFORM 230-PRINT-HEADING-LINES.                         03970000
                                                                        03980000
           *> PERFROMS DUTIES BASED ON THE ENTRY                        03990000
           *>  * IF IT'S THE FIRST RECORD PRINT THE BRANCH NUMBER       04000000
           *>    AND THE SALESREP NUMBER                                04010000
           *>  * IF IT'S A NEW BRANCH PRINT THE BRANCH NUMBER AND       04020000
           *>    SALES REP NUMBER                                       04030000
           *>  * IF IT'S A NEW SALES REP PRINT THE SALESREP NUMBER      04040000
           *>  * OTHERWISE PRINT SPACES IN THOSE LINES FOR PADDING      04050000
           EVALUATE TRUE                                                04060000
               WHEN FIRST-RECORD                                        04070000
                   MOVE CM-BRANCH-NUMBER TO CL-BRANCH-NUMBER            04080000
                   MOVE CM-SALESREP-NUMBER TO CL-SALESREP-NUMBER        04090000
                   PERFORM 223-MOVE-SALESREP-NUMBER                     04100008
               WHEN CM-BRANCH-NUMBER > OLD-BRANCH-NUMBER                04110000
                   MOVE CM-BRANCH-NUMBER TO CL-BRANCH-NUMBER            04120000
                   MOVE CM-SALESREP-NUMBER TO CL-SALESREP-NUMBER        04130000
                   PERFORM 223-MOVE-SALESREP-NUMBER                     04140008
               WHEN NOT (CM-SALESREP-NUMBER = OLD-SALESREP-NUMBER)      04150000
                   MOVE CM-SALESREP-NUMBER TO CL-SALESREP-NUMBER        04160000
                   PERFORM 223-MOVE-SALESREP-NUMBER                     04170008
                   MOVE SPACES TO CL-BRANCH-NUMBER                      04180000
               WHEN OTHER                                               04190000
                   MOVE SPACES TO CL-BRANCH-NUMBER                      04200000
                   MOVE SPACES TO CL-SALESREP-NUMBER                    04210000
           END-EVALUATE.                                                04220000
                                                                        04230000
           *> MOVE THE DATA PULLED FROM THE INPUT FILE INTO THE         04240000
           *> CUSTOMER LINE RECORD FOR LATER OUTPUT                     04250000
           MOVE CM-CUSTOMER-NUMBER  TO CL-CUSTOMER-NUMBER.              04260000
           MOVE CM-CUSTOMER-NAME    TO CL-CUSTOMER-NAME.                04270000
           MOVE CM-SALES-THIS-YTD   TO CL-SALES-THIS-YTD.               04280000
           MOVE CM-SALES-LAST-YTD   TO CL-SALES-LAST-YTD.               04290000
                                                                        04300000
           *> CALCULATE THE DIFFERENCE BETWEEN THIS YEAR'S SALES AND    04310000
           *> AND LAST THEN SAVE THESE RESULT TO CHANGE-AMOUNT AND      04320000
           COMPUTE CHANGE-AMOUNT =                                      04330000
               CM-SALES-THIS-YTD - CM-SALES-LAST-YTD.                   04340000
           MOVE CHANGE-AMOUNT TO CL-CHANGE-AMOUNT.                      04350000
                                                                        04360000
           *> CALCULATE THE PERCENT FOR THE CHANGE IN SALES BETWEEN     04370000
           *> THIS AND LAST YTD, IF THERE WAS NO LAST YEAR SALES        04380000
           *> NUMBER WE MOVE 999.9 TO THE PERECENTAGE SINCE IT'S        04390000
           *> A DIVIDE BY ZERO ERROR OTHERWISE                          04400000
           IF CM-SALES-LAST-YTD = ZERO                                  04410000
               MOVE "  N/A " TO CL-CHANGE-PERCENT-R                     04420000
           ELSE                                                         04430000
               COMPUTE CL-CHANGE-PERCENT ROUNDED =                      04440000
                   CHANGE-AMOUNT * 100 / CM-SALES-LAST-YTD              04450000
                   ON SIZE ERROR                                        04460000
                       MOVE "OVRFLW" TO CL-CHANGE-PERCENT-R.            04470000
                                                                        04480000
           *> PRINT THIS CUSTOMERS INFORMATION TO THE OUTPUT FILE       04490000
           MOVE CUSTOMER-LINE TO PRINT-AREA.                            04500000
           PERFORM 225-WRITE-REPORT-LINE.                               04510000
                                                                        04520000
           *> ADD THIS CUSTOMERS SALES TO THE SALESREP TOTALS           04530000
           ADD CM-SALES-THIS-YTD TO SALESREP-TOTAL-THIS-YTD.            04540000
           ADD CM-SALES-LAST-YTD TO SALESREP-TOTAL-LAST-YTD.            04550000
                                                                        04560008
      **************************************************************    04570008
      * TODO                                                       *    04580008
      *                                                            *    04590008
      **************************************************************    04600008
       223-MOVE-SALESREP-NUMBER.                                        04610010
           SET SRT-INDEX TO 1.                                          04620010
           SEARCH SALESREP-GROUP                                        04630010
               AT END                                                   04640010
                   MOVE "UNKNOWN" TO CL-SALESREP-NAME                   04650010
               WHEN SALESREP-NUMBER (SRT-INDEX) = CM-SALESREP-NUMBER    04660010
                   MOVE SALESREP-NAME (SRT-INDEX) TO CL-SALESREP-NAME   04670010
           END-SEARCH.                                                  04680010
                                                                        04690008
      **************************************************************    04700000
      * PRINT ALL THE HEADER LINES TO THE OUTPUT FILE, RAN ONCE    *    04710000
      * FOR EVERY PAGE                                             *    04720000
      **************************************************************    04730000
       225-WRITE-REPORT-LINE.                                           04740000
           WRITE PRINT-AREA.                                            04750000
           ADD 1 TO LINE-COUNT.                                         04760000
                                                                        04770000
      **************************************************************    04780000
      * PRINT ALL THE HEADER LINES TO THE OUTPUT FILE, RAN ONCE    *    04790000
      * FOR EVERY PAGE                                             *    04800000
      **************************************************************    04810000
       230-PRINT-HEADING-LINES.                                         04820000
                                                                        04830000
           *> HEADERS ARE PLACED AT THE START OF EVERY PAGE             04840000
           *> SO WE INCREASE THE PAGE COUNT HERE                        04850000
           ADD 1 TO PAGE-COUNT.                                         04860000
           MOVE PAGE-COUNT     TO HL1-PAGE-NUMBER.                      04870000
                                                                        04880000
           *> PRINT EACH HEADER LINE TO THE OUTPUT FILE                 04890000
           MOVE HEADING-LINE-1 TO PRINT-AREA.                           04900000
           WRITE PRINT-AREA.                                            04910000
           MOVE HEADING-LINE-2 TO PRINT-AREA.                           04920000
           WRITE PRINT-AREA.                                            04930000
           MOVE HEADING-LINE-3 TO PRINT-AREA.                           04940000
           WRITE PRINT-AREA.                                            04950000
           MOVE HEADING-LINE-4 TO PRINT-AREA.                           04960000
           WRITE PRINT-AREA.                                            04970000
           MOVE HEADING-LINE-5 TO PRINT-AREA.                           04980000
           WRITE PRINT-AREA.                                            04990000
           MOVE HEADING-LINE-6 TO PRINT-AREA.                           05000000
           WRITE PRINT-AREA.                                            05010000
                                                                        05020000
           *> RESET THE LINE COUNTER SINCE EVERY HEADER IS THE START    05030000
           *> OF A NEW PAGE                                             05040000
           MOVE ZERO TO LINE-COUNT.                                     05050000
                                                                        05060000
      **************************************************************    05070000
      * PRINTS THE CURRENT BRANCH LINE TOTALS, RAN ONCE FOR EVERY  *    05080000
      * BRANCH. ALSO CALCULATES THE CHANGE IN THE BRANCH           *    05090000
      **************************************************************    05100000
       240-PRINT-BRANCH-LINE.                                           05110000
                                                                        05120000
           *> MOVE THE BRANCH TOTALS TO THE BRANCH TOTAL LINE           05130000
           MOVE BRANCH-TOTAL-THIS-YTD TO BTL-SALES-THIS-YTD.            05140000
           MOVE BRANCH-TOTAL-LAST-YTD TO BTL-SALES-LAST-YTD.            05150000
                                                                        05160000
           *> CALCULATE THE CHANGE BETWEEN THIS-YTD AND LAST            05170000
           *> FOR THE CURRENT BRANCH AND ADD IT TO THE TOTAL LINE       05180000
           COMPUTE CHANGE-AMOUNT =                                      05190000
               BRANCH-TOTAL-THIS-YTD - BRANCH-TOTAL-LAST-YTD.           05200000
           MOVE CHANGE-AMOUNT TO BTL-CHANGE-AMOUNT.                     05210000
                                                                        05220000
           *> CALCULATE THE CHANGE PERCENT BETWEEN YTD'S                05230000
           *> THEN MOVE TO THE BRANCH TOTAL LINE                        05240000
           IF BRANCH-TOTAL-LAST-YTD = ZERO                              05250000
               MOVE "  N/A " TO BTL-CHANGE-PERCENT-R                    05260000
           ELSE                                                         05270000
               COMPUTE BTL-CHANGE-PERCENT ROUNDED =                     05280000
                   CHANGE-AMOUNT * 100 / BRANCH-TOTAL-LAST-YTD          05290000
                   ON SIZE ERROR                                        05300000
                       MOVE "OVRFLW" TO BTL-CHANGE-PERCENT-R.           05310000
                                                                        05320000
           *> PRINT BRANCH LINE                                         05330000
           MOVE BRANCH-TOTAL-LINE TO PRINT-AREA.                        05340000
           PERFORM 225-WRITE-REPORT-LINE.                               05350000
                                                                        05360000
           *> WRITE A BLANK SPACER LINE                                 05370000
           MOVE SPACES TO PRINT-AREA.                                   05380000
           PERFORM 225-WRITE-REPORT-LINE.                               05390000
                                                                        05400000
           *> ADD THE BRANCH TOTALS TO THE GRAND TOTALS                 05410000
           ADD BRANCH-TOTAL-THIS-YTD TO GRAND-TOTAL-THIS-YTD.           05420000
           ADD BRANCH-TOTAL-LAST-YTD TO GRAND-TOTAL-LAST-YTD.           05430000
                                                                        05440000
           *> ZERO OUT THE BRANCH TOTALS                                05450000
           INITIALIZE BRANCH-TOTAL-THIS-YTD                             05460009
                      BRANCH-TOTAL-LAST-YTD.                            05470009
                                                                        05480000
      **************************************************************    05490000
      * PRINTS THE CURRENT SALESREP'S TOTALS, RAN ONCE FOR EVERY   *    05500000
      * SALESREP. ALSO CALCULATES THE CHANGE BETWEEN YEARS         *    05510000
      **************************************************************    05520000
       250-PRINT-SALESREP-LINE.                                         05530000
                                                                        05540000
           *> MOVE THE SALESREP TOTALS TO THE SALESREP TOTAL LINE       05550000
           MOVE SALESREP-TOTAL-THIS-YTD TO STL-SALES-THIS-YTD.          05560000
           MOVE SALESREP-TOTAL-LAST-YTD TO STL-SALES-LAST-YTD.          05570000
                                                                        05580000
           *> CALCULATE THE CHANGE BETWEEN THIS-YTD AND LAST            05590000
           *> FOR THE CURRENT SALESREP AND ADD IT TO THE TOTAL LINE     05600000
           COMPUTE CHANGE-AMOUNT =                                      05610000
               SALESREP-TOTAL-THIS-YTD - SALESREP-TOTAL-LAST-YTD.       05620000
           MOVE CHANGE-AMOUNT TO STL-CHANGE-AMOUNT.                     05630000
                                                                        05640000
           *> CALCULATE THE CHANGE PERCENT BETWEEN YTD'S                05650000
           *> THEN MOVE TO THE SALESREP TOTAL LINE                      05660000
           IF SALESREP-TOTAL-LAST-YTD = ZERO                            05670000
               MOVE "  N/A " TO STL-CHANGE-PERCENT-R                    05680000
           ELSE                                                         05690000
               COMPUTE STL-CHANGE-PERCENT ROUNDED =                     05700000
                   CHANGE-AMOUNT * 100 / SALESREP-TOTAL-LAST-YTD        05710000
                   ON SIZE ERROR                                        05720000
                       MOVE "OVRFLW" TO STL-CHANGE-PERCENT-R.           05730000
                                                                        05740000
           *> PRINT SALESREP LINE                                       05750000
           MOVE SALESREP-TOTAL-LINE TO PRINT-AREA.                      05760000
           PERFORM 225-WRITE-REPORT-LINE.                               05770000
                                                                        05780000
           *> PRINT A SPACER LINE                                       05790000
           MOVE SPACES TO PRINT-AREA.                                   05800000
           PERFORM 225-WRITE-REPORT-LINE.                               05810000
                                                                        05820000
           *> ADD THE SALESREP TOTALS TO THE BRANCH TOTALS              05830000
           *> WHEN A BRANCH IS PRINTED THEN THOSE TOTALS ARE MOVED      05840000
           *> TO THE GRAND TOTALS                                       05850000
           *> CUSTOMER->SALESREP->BRANCH->GRAND-TOTAL                   05860000
           ADD SALESREP-TOTAL-THIS-YTD TO BRANCH-TOTAL-THIS-YTD.        05870000
           ADD SALESREP-TOTAL-LAST-YTD TO BRANCH-TOTAL-LAST-YTD.        05880000
                                                                        05890000
           *> ZERO OUT THE SALESREP TOTALS                              05900000
           MOVE ZERO TO SALESREP-TOTAL-THIS-YTD.                        05910000
           MOVE ZERO TO SALESREP-TOTAL-LAST-YTD.                        05920000
      **************************************************************    05930000
      * PRINTS THE GRAND TOTALS FOR ALL THE CUSTOMERS, RAN ONCE    *    05940000
      * AT THE VERY END OF THE PROGRAM WHEN ALL CUSTOMERS HAVE     *    05950000
      * BEEN PRINTED                                               *    05960000
      **************************************************************    05970000
       300-PRINT-GRAND-TOTALS.                                          05980000
                                                                        05990000
           *> MOVE THE GRAND TOTALS FOR THE SALES TO THE                06000000
           *> OUTPUT LINE FOR GRAND TOTALS                              06010000
           MOVE GRAND-TOTAL-THIS-YTD TO GTL-SALES-THIS-YTD.             06020000
           MOVE GRAND-TOTAL-LAST-YTD TO GTL-SALES-LAST-YTD.             06030000
                                                                        06040000
           *> COMPUTE THE GRAND TOTAL FOR THE CHANGE AMOUNT             06050000
           COMPUTE CHANGE-AMOUNT =                                      06060000
               GRAND-TOTAL-THIS-YTD - GRAND-TOTAL-LAST-YTD.             06070000
           MOVE CHANGE-AMOUNT TO GTL-CHANGE-AMOUNT.                     06080000
                                                                        06090000
           *> CALCULATE THE TOTAL CHANGE IN PERCENT BETWEEN             06100000
           *> THIS YTD AND LAST YTD FOR ALL CUSTOMERS                   06110000
           *> IF THERE WAS NO LAST YEAR FOR ANYONE DEFAULT TO           06120000
           *> A PERCENT OF 999.9 TO AVOID DIVIDE BY ZERO ERROR          06130000
           IF GRAND-TOTAL-LAST-YTD = ZERO                               06140000
               MOVE "  N/A " TO GTL-CHANGE-PERCENT-R                    06150000
           ELSE                                                         06160000
               COMPUTE GTL-CHANGE-PERCENT ROUNDED =                     06170000
                   CHANGE-AMOUNT * 100 / GRAND-TOTAL-LAST-YTD           06180000
                   ON SIZE ERROR                                        06190000
                       MOVE "OVRFLW" TO GTL-CHANGE-PERCENT-R.           06200000
                                                                        06210000
           *> PRINT THE GRAND-TOTAL TO THE OUTPUT FILE                  06220000
           MOVE GRAND-TOTAL-LINE TO PRINT-AREA.                         06230000
           PERFORM 225-WRITE-REPORT-LINE.                               06240000
