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
           SELECT CUSTMAST ASSIGN TO CUSTMAST.                          00170034
           SELECT INPUT-SALESREP ASSIGN TO SALESREP.                    00180000
           SELECT ORPT6000 ASSIGN TO RPT6000.                           00190001
                                                                        00200000
       DATA DIVISION.                                                   00210000
                                                                        00220000
       FILE SECTION.                                                    00230034
      **************************************************************    00240000
      * INPUT FILES                                                *    00250000
      **************************************************************    00260034
                                                                        00270034
       FD  CUSTMAST                                                     00280000
           RECORDING MODE IS F                                          00290000
           LABEL RECORDS ARE STANDARD                                   00300000
           RECORD CONTAINS 130 CHARACTERS                               00310000
           BLOCK CONTAINS 130 CHARACTERS.                               00320000
       01  CUSTOMER-MASTER-RECORD.                                      00330000
           05  CM-BRANCH-NUMBER        PIC 9(2).                        00340000
           05  CM-SALESREP-NUMBER      PIC 9(2).                        00350000
           05  CM-CUSTOMER-NUMBER      PIC 9(5).                        00360000
           05  CM-CUSTOMER-NAME        PIC X(20).                       00370000
           05  CM-SALES-THIS-YTD       PIC S9(5)V9(2).                  00380000
           05  CM-SALES-LAST-YTD       PIC S9(5)V9(2).                  00390000
           05  FILLER                  PIC X(87).                       00400034
                                                                        00410034
       FD  INPUT-SALESREP                                               00420034
           RECORDING MODE IS F                                          00430000
           LABEL RECORDS ARE STANDARD                                   00440000
           RECORD CONTAINS 130 CHARACTERS                               00450000
           BLOCK CONTAINS 130 CHARACTERS.                               00460000
       01  SALESREP-MASTER-RECORD.                                      00470000
           05 SM-SALESREP-NUMBER       PIC 9(2).                        00480034
           05 SM-SALESREP-NAME         PIC 9(10).                       00490034
           05  FILLER                  PIC X(118).                      00500000
                                                                        00510000
      **************************************************************    00520000
      * OUTPUT FILE                                                *    00530000
      **************************************************************    00540000
       FD  ORPT6000                                                     00550001
           RECORDING MODE IS F                                          00560000
           LABEL RECORDS ARE STANDARD                                   00570000
           RECORD CONTAINS 130 CHARACTERS                               00580000
           BLOCK CONTAINS 130 CHARACTERS.                               00590000
       01  PRINT-AREA      PIC X(130).                                  00600000
                                                                        00610000
       WORKING-STORAGE SECTION.                                         00620000
                                                                        00630000
      *------------------------------------------------------------*    00640000
      *                        WORKING FIELDS                      *    00650000
      *============================================================*    00660000
      *     THE FOLLOWING RECORDS ARE USED FOR WORKING WITH DATA   *    00670000
      *              AND ARE NOT USED FOR PROGRAM OUTPUT           *    00680000
      *------------------------------------------------------------*    00690000
       01 SALESREP-TABLE.                                               00700034
           05 SALESREP-GROUP OCCURS 100 TIMES                           00710007
                             INDEXED BY SRT-INDEX.                      00720007
               10 SALESREP-NUMBER   PIC 99.                             00730007
               10 SALESREP-NAME     PIC X(10).                          00740034
                                                                        00750034
      **************************************************************    00760000
      * SWITCHES FOR END OF FILE AND FIRST RECORD                  *    00770000
      **************************************************************    00780000
       01  SWITCHES.                                                    00790034
           05  SALESREP-EOF-SWITCH     PIC X    VALUE "N".              00800000
               88  SALESREP-EOF                 VALUE "Y".              00810034
           05  CUSTMAST-EOF-SWITCH     PIC X    VALUE "N".              00820000
               88  CUSTMAST-EOF                 VALUE "Y".              00830000
           05  FIRST-RECORD-SWITCH     PIC X    VALUE "Y".              00840000
               88  FIRST-RECORD                 VALUE "Y"               00850000
                   WHEN FALSE IS                      "N".              00860000
                                                                        00870000
      **************************************************************    00880000
      * SWITCH FOR END OF FILE                                     *    00890000
      **************************************************************    00900000
       01  CONTROL-FIELDS PACKED-DECIMAL.                               00910000
           05  OLD-BRANCH-NUMBER       PIC 99.                          00920000
           05  OLD-SALESREP-NUMBER     PIC 99.                          00930000
                                                                        00940000
      **************************************************************    00950000
      * STORES INFORMATION RELEVANT TO THE PAGE                    *    00960000
      **************************************************************    00970000
       01  PRINT-FIELDS PACKED-DECIMAL.                                 00980000
           05  PAGE-COUNT      PIC S9(3)   VALUE ZERO.                  00990000
           05  LINES-ON-PAGE   PIC S9(3)   VALUE +55.                   01000000
           05  LINE-COUNT      PIC S9(3)   VALUE +99.                   01010000
                                                                        01020000
      **************************************************************    01030000
      * STORES TOTAL FIELDS FOR CALCULATING                        *    01040000
      **************************************************************    01050000
       01  TOTAL-FIELDS PACKED-DECIMAL.                                 01060000
           05  BRANCH-TOTAL-THIS-YTD    PIC S9(6)V99   VALUE ZERO.      01070000
           05  BRANCH-TOTAL-LAST-YTD    PIC S9(6)V99   VALUE ZERO.      01080000
           05  SALESREP-TOTAL-THIS-YTD  PIC S9(6)V99   VALUE ZERO.      01090000
           05  SALESREP-TOTAL-LAST-YTD  PIC S9(6)V99   VALUE ZERO.      01100000
           05  GRAND-TOTAL-THIS-YTD     PIC S9(7)V99   VALUE ZERO.      01110000
           05  GRAND-TOTAL-LAST-YTD     PIC S9(7)V99   VALUE ZERO.      01120000
                                                                        01130000
      **************************************************************    01140000
      * USED TO PULL IN THE CURRENT-DATE-TIME VIA THE FUNCTION     *    01150000
      * CURRENT-DATE-AND-TIME WHICH WILL BE USED IN HEADER LINES   *    01160000
      **************************************************************    01170000
       01  CURRENT-DATE-AND-TIME.                                       01180000
           05  CD-YEAR         PIC 9999.                                01190000
           05  CD-MONTH        PIC 99.                                  01200000
           05  CD-DAY          PIC 99.                                  01210000
           05  CD-HOURS        PIC 99.                                  01220000
           05  CD-MINUTES      PIC 99.                                  01230000
           05  FILLER          PIC X(9).                                01240000
                                                                        01250000
      **************************************************************    01260000
      * STORES VALUES USED FOR CALCULATIONS                       *     01270000
      **************************************************************    01280000
       01  CALCULATED-FIELDS.                                           01290000
           05 CHANGE-AMOUNT    PIC S9(5)V99.                            01300000
                                                                        01310000
      *------------------------------------------------------------*    01320000
      *                       OUTPUT FIELDS                        *    01330000
      *============================================================*    01340000
      *     THE FOLLOWING RECORDS ARE USED FOR PRINTING DATA TO    *    01350000
      *                      THE OUTPUT FILE                       *    01360000
      *------------------------------------------------------------*    01370000
                                                                        01380000
      **************************************************************    01390000
      * STORES THE FIRST HEADER LINE INFORMATION                   *    01400000
      * HOLDS THE DATE, REPORT TITLE, AND PAGE NUMBER              *    01410000
      **************************************************************    01420000
       01  HEADING-LINE-1.                                              01430000
           05  FILLER          PIC X(7)    VALUE "DATE:  ".             01440000
           05  HL1-MONTH       PIC 9(2).                                01450000
           05  FILLER          PIC X(1)    VALUE "/".                   01460000
           05  HL1-DAY         PIC 9(2).                                01470000
           05  FILLER          PIC X(1)    VALUE "/".                   01480000
           05  HL1-YEAR        PIC 9(4).                                01490000
           05  FILLER          PIC X(26)   VALUE SPACE.                 01500000
           05  FILLER          PIC X(20)   VALUE "YEAR-TO-DATE SALES R".01510000
           05  FILLER          PIC X(31)   VALUE "EPORT".               01520000
           05  FILLER          PIC X(6)    VALUE "PAGE: ".              01530000
           05  HL1-PAGE-NUMBER PIC ZZZ9.                                01540000
           05  FILLER          PIC X(26)   VALUE SPACE.                 01550000
                                                                        01560000
      **************************************************************    01570000
      * STORES THE SECOND HEADER LINE INFORMATION                  *    01580000
      * HOLDS THE TIME AND THE PROGRAM ID                          *    01590000
      **************************************************************    01600000
       01  HEADING-LINE-2.                                              01610000
           05  FILLER          PIC X(7)    VALUE "TIME:  ".             01620000
           05  HL2-HOURS       PIC 9(2).                                01630000
           05  FILLER          PIC X(1)    VALUE ":".                   01640000
           05  HL2-MINUTES     PIC 9(2).                                01650000
           05  FILLER          PIC X(82)   VALUE SPACE.                 01660000
           05  FILLER          PIC X(7)    VALUE "RPT6000".             01670001
           05  FILLER          PIC X(29)   VALUE SPACE.                 01680000
                                                                        01690000
      **************************************************************    01700000
      * STORES THE THIRD HEADER LINE USED TO DISPLAY A LINE SPACER *    01710000
      **************************************************************    01720000
       01  HEADING-LINE-3.                                              01730000
           05 FILLER               PIC X(130)   VALUE SPACE.            01740000
                                                                        01750000
      **************************************************************    01760000
      * STORES THE FOURTH HEADER LINE INFORMATION                  *    01770000
      * HOLDS THE DIFFERENT COLUMN NAMES - SOME ARE SPLIT ACROSS   *    01780000
      * THE NEXT HEADER LINE                                       *    01790000
      **************************************************************    01800000
       01  HEADING-LINE-4.                                              01810000
           05  FILLER      PIC X(54)   VALUE SPACES.                    01820000
           05  FILLER      PIC X(19)   VALUE "SALES         SALES".     01830000
           05  FILLER      PIC X(8)    VALUE SPACES.                    01840014
           05  FILLER      PIC X(17)   VALUE "CHANGE     CHANGE".       01850014
           05  FILLER      PIC X(32)   VALUE SPACES.                    01860014
                                                                        01870000
      **************************************************************    01880000
      * STORES THE FIFTH HEADER LINE INFORMATION                   *    01890000
      * HOLDS SOME OF THE COLUMN NAMES AS WELL AS THE OTHER HALF   *    01900000
      * OF COLUMN NAMES THAT STARTED IN THE LAST HEADER LINE       *    01910000
      **************************************************************    01920000
       01  HEADING-LINE-5.                                              01930000
           05  FILLER         PIC X(17)  VALUE "BRANCH   SALESREP".     01940000
           05  FILLER         PIC X(13)  VALUE SPACES.                  01950000
           05  FILLER         PIC X(8)   VALUE "CUSTOMER".              01960000
           05  FILLER         PIC X(14)  VALUE SPACES.                  01970000
           05  FILLER         PIC X(22)  VALUE "THIS YTD      LAST YTD".01980000
           05  FILLER         PIC X(7)   VALUE SPACES.                  01990000
           05  FILLER         PIC X(18)  VALUE "AMOUNT     PERCENT".    02000000
           05  FILLER         PIC X(31)  VALUE SPACE.                   02010014
                                                                        02020000
      **************************************************************    02030000
      * STORES THE SIXTH HEADER LINE WHICH IS USED FOR SPACING     *    02040000
      **************************************************************    02050000
       01  HEADING-LINE-6.                                              02060000
           05  FILLER           PIC X(6)   VALUE ALL '-'.               02070014
           05  FILLER           PIC X(1)   VALUE SPACE.                 02080014
           05  FILLER           PIC X(13)  VALUE ALL '-'.               02090014
           05  FILLER           PIC X(1)   VALUE SPACE.                 02100014
           05  FILLER           PIC X(26)   VALUE ALL '-'.              02110014
           05  FILLER           PIC X(3)   VALUE SPACE.                 02120014
           05  FILLER           PIC X(11)  VALUE ALL '-'.               02130014
           05  FILLER           PIC X(3)   VALUE SPACE.                 02140014
           05  FILLER           PIC X(11)  VALUE ALL '-'.               02150014
           05  FILLER           PIC X(4)   VALUE SPACE.                 02160014
           05  FILLER           PIC X(11)  VALUE ALL '-'.               02170014
           05  FILLER           PIC X(2)   VALUE SPACE.                 02180014
           05  FILLER           PIC x(7)   VALUE ALL '-'.               02190014
           05  FILLER           PIC X(31)  VALUE SPACE.                 02200014
                                                                        02210000
      **************************************************************    02220000
      * STORES INFORMATION ABOUT CURRENT CUSTOMER                  *    02230000
      * HOLDS THE BRANCH NUMBER, SALES REP NUMBER, CUSTOMER NUMBER,*    02240000
      * CUSTOMER NAME, SALES THIS AND LAST YEAR-TO-DATE,           *    02250000
      * DIFFERENCE BETWEEN THIS YEARS SALES AND LAST, AND THE      *    02260000
      * DIFFERENCE IN PERCENT.                                     *    02270000
      **************************************************************    02280000
       01  CUSTOMER-LINE.                                               02290000
           05  FILLER               PIC X(2)       VALUE SPACE.         02300034
           05  CL-BRANCH-NUMBER     PIC X(2).                           02310034
           05  FILLER               PIC X(3)       VALUE SPACE.         02320034
           05  CL-SALESREP-NUMBER   PIC X(2).                           02330034
           05  FILLER               PIC X(1)       VALUE SPACE.         02340034
           05  CL-SALESREP-NAME     PIC X(10).                          02350034
           05  FILLER               PIC X(1)       VALUE SPACE.         02360034
           05  CL-CUSTOMER-NUMBER   PIC X(5).                           02370034
           05  FILLER               PIC X(1)       VALUE SPACE.         02380034
           05  CL-CUSTOMER-NAME     PIC X(20).                          02390034
           05  FILLER               PIC X(6)       VALUE SPACE.         02400034
           05  CL-SALES-THIS-YTD    PIC ZZ,ZZ9.99-.                     02410034
           05  FILLER               PIC X(4)       VALUE SPACE.         02420034
           05  CL-SALES-LAST-YTD    PIC ZZ,ZZ9.99-.                     02430034
           05  FILLER               PIC X(4)       VALUE SPACE.         02440034
           05  CL-CHANGE-AMOUNT     PIC ZZ,ZZ9.99-.                     02450034
           05  FILLER               PIC X(2)       VALUE SPACE.         02460034
           05  CL-CHANGE-PERCENT    PIC +++9.9.                         02470034
           05  CL-CHANGE-PERCENT-R  REDEFINES  CL-CHANGE-PERCENT        02480034
                                    PIC X(6).                           02490034
           05  FILLER               PIC X(31)      VALUE SPACE.         02500034
                                                                        02510000
      **************************************************************    02520000
      * STORES THE BRANCH TOTAL LINE                               *    02530000
      * HOLDS THE TOTALS FOR THIS AND LAST YEAR-TO-DATE IN SALES   *    02540000
      * FOR THIS BRANCH AS WELL AS THE PERCENT DIFFERENCE          *    02550000
      * USED FOR OUTPUTTING                                        *    02560000
      **************************************************************    02570000
       01  BRANCH-TOTAL-LINE.                                           02580016
           05  FILLER               PIC X(36)   VALUE SPACE.            02590016
           05  FILLER               PIC X(16)   VALUE "  BRANCH TOTAL". 02600016
           05  BTL-SALES-THIS-YTD   PIC $$$,$$9.99-.                    02610016
           05  FILLER               PIC X(3)    VALUE SPACE.            02620016
           05  BTL-SALES-LAST-YTD   PIC $$$,$$9.99-.                    02630016
           05  FILLER               PIC X(3)    VALUE SPACE.            02640016
           05  BTL-CHANGE-AMOUNT    PIC $$$,$$9.99-.                    02650016
           05  FILLER               PIC X(2)    VALUE SPACE.            02660016
           05  BTL-CHANGE-PERCENT   PIC +++9.9.                         02670016
           05  BTL-CHANGE-PERCENT-R REDEFINES BTL-CHANGE-PERCENT        02680016
                                    PIC X(6).                           02690016
           05  FILLER               PIC X(31)   VALUE "**".             02700016
                                                                        02710000
      **************************************************************    02720000
      * STORES THE SALES REP TOTAL LINE                            *    02730000
      * HOLDS THE TOTALS FOR THIS AND LAST YEAR-TO-DATE IN SALES   *    02740000
      * FOR THIS REP AS WELL AS THE PERCENT DIFFERENCE             *    02750000
      * USED FOR OUTPUTTING                                        *    02760000
      **************************************************************    02770000
       01  SALESREP-TOTAL-LINE.                                         02780016
           05  FILLER               PIC X(36)   VALUE SPACE.            02790016
           05  FILLER               PIC X(16)   VALUE "SALESREP TOTAL". 02800016
           05  STL-SALES-THIS-YTD   PIC $$$,$$9.99-.                    02810016
           05  FILLER               PIC X(3)    VALUE SPACE.            02820016
           05  STL-SALES-LAST-YTD   PIC $$$,$$9.99-.                    02830016
           05  FILLER               PIC X(3)    VALUE SPACE.            02840016
           05  STL-CHANGE-AMOUNT    PIC $$$,$$9.99-.                    02850016
           05  FILLER               PIC X(2)    VALUE SPACE.            02860016
           05  STL-CHANGE-PERCENT   PIC +++9.9.                         02870016
           05  STL-CHANGE-PERCENT-R REDEFINES STL-CHANGE-PERCENT        02880016
                                    PIC X(6).                           02890016
           05  FILLER               PIC X(31)   VALUE "*".              02900016
      **************************************************************    02910000
      * STORES THE SECOND GRAND TOTAL LINE                         *    02920000
      * HOLDS THE TOTAL SALES FOR THIS AND LAST YEAR-TO-DATE,      *    02930000
      * THE TOTAL DIFFERENCE IN SALES MADE BETWEEN THE TWO YEARS   *    02940000
      * AND THE PERCENTAGE DIFFERENCE - FOR OUTPUTTING             *    02950000
      **************************************************************    02960000
       01  GRAND-TOTAL-LINE.                                            02970016
           05  FILLER               PIC X(36)    VALUE SPACE.           02980016
           05  FILLER               PIC X(14)    VALUE "   GRAND TOTAL".02990016
           05  GTL-SALES-THIS-YTD   PIC $,$$$,$$9.99-.                  03000016
           05  FILLER               PIC X(1)     VALUE SPACE.           03010016
           05  GTL-SALES-LAST-YTD   PIC $,$$$,$$9.99-.                  03020016
           05  FILLER               PIC X(1)     VALUE SPACE.           03030016
           05  GTL-CHANGE-AMOUNT    PIC $,$$$,$$9.99-.                  03040016
           05  FILLER               PIC X(2)     VALUE SPACE.           03050016
           05  GTL-CHANGE-PERCENT   PIC +++9.9.                         03060016
           05  GTL-CHANGE-PERCENT-R REDEFINES GTL-CHANGE-PERCENT        03070016
                                    PIC X(6).                           03080016
           05  FILLER               PIC X(31)    VALUE "***".           03090016
                                                                        03100000
       PROCEDURE DIVISION.                                              03110000
                                                                        03120000
      **************************************************************    03130000
      * OPENS AND CLOSES THE FILES AND DELEGATES THE WORK FOR      *    03140000
      * READING AND WRITING TO AND FROM THEM                       *    03150000
      **************************************************************    03160000
       000-PREPARE-SALES-REPORT.                                        03170000
           INITIALIZE SALESREP-TABLE.                                   03180034
                                                                        03190034
           OPEN INPUT  CUSTMAST                                         03200034
                INPUT INPUT-SALESREP                                    03210034
                OUTPUT ORPT6000.                                        03220034
                                                                        03230034
           PERFORM 205-LOAD-SALESREP-TABLE.                             03240034
                                                                        03250000
           *> GRABS THE DATE AND TIME INFORMATION FOR                   03260000
           *> THE HEADER LINES                                          03270000
           PERFORM 100-FORMAT-REPORT-HEADING.                           03280034
                                                                        03290000
           *> GRAB AND PRINT CUSTOMER SALES TO THE OUPUT FILE UNTIL     03300000
           *> THE END OF THE INPUT FILE                                 03310000
           PERFORM 200-PREPARE-SALES-LINES                              03320000
               UNTIL CUSTMAST-EOF-SWITCH = "Y".                         03330000
                                                                        03340034
                                                                        03350034
                                                                        03360034
                                                                        03370034
           *> OUTPUT THE GRAND TOTALS TO THE OUTPUT FILE                03380000
           PERFORM 300-PRINT-GRAND-TOTALS.                              03390000
                                                                        03400000
           CLOSE CUSTMAST                                               03410000
                 ORPT6000.                                              03420001
           STOP RUN.                                                    03430000
                                                                        03440000
      **************************************************************    03450000
      * FORMATS THE REPORT HEADER BY GRABBING THE DATE TIME AND    *    03460000
      * STORING IT IN THE RELEVENT HEADER DATA ITEMS               *    03470000
      **************************************************************    03480000
       100-FORMAT-REPORT-HEADING.                                       03490000
                                                                        03500000
           MOVE FUNCTION CURRENT-DATE TO CURRENT-DATE-AND-TIME.         03510000
                                                                        03520000
           *> MOVE THE RESULT OF THE DATE-TIME FUNCTION TO THE          03530000
           *> DIFFERENT HEADER LINE FIELDS ASSOCIATED WITH THEM         03540000
           *> SO WE CAN INCLUDE THE DATE IN THE OUTPUT HEADER           03550000
           MOVE CD-MONTH   TO HL1-MONTH.                                03560000
           MOVE CD-DAY     TO HL1-DAY.                                  03570000
           MOVE CD-YEAR    TO HL1-YEAR.                                 03580000
           MOVE CD-HOURS   TO HL2-HOURS.                                03590000
           MOVE CD-MINUTES TO HL2-MINUTES.                              03600000
                                                                        03610000
      **************************************************************    03620034
      * READS A LINE OF THE TABLE'S INPUT FILE                     *    03630034
      **************************************************************    03640034
       110-READ-SALESREP-TABLE-RECORD.                                  03650034
           READ INPUT-SALESREP                                          03660034
                AT END                                                  03670034
                    SET SALESREP-EOF TO TRUE.                           03680034
                                                                        03690034
      **************************************************************    03700000
      * CALLS THE PARAGRAPH TO READ A LINE OF THE CUSTOMER RECORD  *    03710000
      * THEN CALLS THE PARAGRAPH TO PRINT THE LINE IF ITS NOT THE  *    03720000
      * TERMINATING LINE OF THE FILE                               *    03730000
      **************************************************************    03740000
       200-PREPARE-SALES-LINES.                                         03750000
                                                                        03760000
           *> GRAB THE NEXT LINE FROM THE CUSTOMER RECORD               03770000
           PERFORM 210-READ-CUSTOMER-RECORD.                            03780000
                                                                        03790000
           *> PERFORMS DUTIES BASED ON THE ENTRY                        03800000
           *>  * IF WE RUN OUT OF DATA PRINT THE SALES AND BRANCH TOTALS03810000
           *>  * IF IT'S THE FIRST RECORD PRINT THE CUSTOMER LINE AND   03820000
           *>    STORE THE CURRENT SALESREP AND BRANCH NUMBER TO THE OLD03830000
           *>  * IF THE BRANCH NUMBER IS GREATER THAN THE CURRENT ONE   03840000
           *>    THEN PRINT THE SALES REP LINE, BRANCH TOTAL LINE, AND  03850000
           *>    THEN THE NEW CUSTOMER'S LINE. AFTER UPDATE THE BRANCH  03860000
           *>    AND SALESREP NUMBERS                                   03870000
           *>  * IF THE SALES REP NUMBER IS GREATER THAN THE CURRENT ONE03880000
           *>    PRINT SALES LINE THEN THE CURRENT CUSTOMER LINE AFTER  03890000
           *>    UPDATE THE SALES REP NUMBER                            03900000
           *>  * IF NOTHING ELSE JUST PRINT THE CUSTOMER RECORD         03910000
           EVALUATE TRUE                                                03920000
               WHEN CUSTMAST-EOF                                        03930000
                   PERFORM 250-PRINT-SALESREP-LINE                      03940000
                   PERFORM 240-PRINT-BRANCH-LINE                        03950000
               WHEN FIRST-RECORD                                        03960000
                   PERFORM 220-PRINT-CUSTOMER-LINE                      03970000
                   SET FIRST-RECORD TO FALSE                            03980000
                 *>MOVE "N" TO FIRST-RECORD-SWITCH                      03990000
                   MOVE CM-SALESREP-NUMBER TO OLD-SALESREP-NUMBER       04000000
                   MOVE CM-BRANCH-NUMBER TO OLD-BRANCH-NUMBER           04010000
               WHEN CM-BRANCH-NUMBER > OLD-BRANCH-NUMBER                04020000
                   PERFORM 250-PRINT-SALESREP-LINE                      04030000
                   PERFORM 240-PRINT-BRANCH-LINE                        04040000
                   PERFORM 220-PRINT-CUSTOMER-LINE                      04050000
                   MOVE CM-SALESREP-NUMBER TO OLD-SALESREP-NUMBER       04060000
                   MOVE CM-BRANCH-NUMBER TO OLD-BRANCH-NUMBER           04070000
               WHEN NOT (CM-SALESREP-NUMBER = OLD-SALESREP-NUMBER)      04080000
                   PERFORM 250-PRINT-SALESREP-LINE                      04090000
                   PERFORM 220-PRINT-CUSTOMER-LINE                      04100000
                   MOVE CM-SALESREP-NUMBER TO OLD-SALESREP-NUMBER       04110000
               WHEN OTHER                                               04120000
                   PERFORM 220-PRINT-CUSTOMER-LINE                      04130000
           END-EVALUATE.                                                04140034
                                                                        04150034
      **************************************************************    04160034
      * LOADS IN THE SALESREP TABLE FROM THE INPUT FILE            *    04170034
      **************************************************************    04180034
       205-LOAD-SALESREP-TABLE.                                         04190034
           PERFORM WITH TEST AFTER                                      04200034
                VARYING SRT-INDEX FROM 1 BY 1                           04210034
                UNTIL SALESREP-EOF OR SRT-INDEX = 100                   04220034
                PERFORM 110-READ-SALESREP-TABLE-RECORD                  04230034
                IF NOT SALESREP-EOF                                     04240034
                     MOVE SM-SALESREP-NUMBER                            04250034
                          TO SALESREP-NUMBER (SRT-INDEX)                04260034
                     MOVE SM-SALESREP-NAME TO SALESREP-NAME (SRT-INDEX) 04270034
                END-IF                                                  04280034
           END-PERFORM.                                                 04290034
                                                                        04300000
      **************************************************************    04310000
      * READS A LINE OF THE INPUT FILE AND IF ITS THE LAST ONE     *    04320000
      * UPDATES THE CUSTOMER-EOF-SWITCH (END-OF-FILE)              *    04330000
      **************************************************************    04340000
       210-READ-CUSTOMER-RECORD.                                        04350000
                                                                        04360000
           READ CUSTMAST                                                04370000
               AT END                                                   04380000
                   MOVE "Y" TO CUSTMAST-EOF-SWITCH.                     04390000
                                                                        04400000
      **************************************************************    04410000
      * PRINTS THE CURRENT CUSTOMER LINE TO THE OUTPUT FILE        *    04420000
      * UPDATES THE LINE COUNTER SO IT KNOWS WHEN IT HAS TO        *    04430000
      * REPRINT THE HEADER LINES FOR A NEW PAGE                    *    04440000
      **************************************************************    04450000
       220-PRINT-CUSTOMER-LINE.                                         04460000
                                                                        04470000
           *> IF INFORMATION WE HAVE PRINTED EXCEEDS THE PAGE LIMIT     04480000
           *> WE REPRINT THE HEADERS FOR THE NEW PAGE                   04490000
           IF LINE-COUNT >= LINES-ON-PAGE                               04500000
               PERFORM 230-PRINT-HEADING-LINES.                         04510000
                                                                        04520000
           *> PERFROMS DUTIES BASED ON THE ENTRY                        04530000
           *>  * IF IT'S THE FIRST RECORD PRINT THE BRANCH NUMBER       04540000
           *>    AND THE SALESREP NUMBER                                04550000
           *>  * IF IT'S A NEW BRANCH PRINT THE BRANCH NUMBER AND       04560000
           *>    SALES REP NUMBER                                       04570000
           *>  * IF IT'S A NEW SALES REP PRINT THE SALESREP NUMBER      04580000
           *>  * OTHERWISE PRINT SPACES IN THOSE LINES FOR PADDING      04590000
           EVALUATE TRUE                                                04600000
               WHEN FIRST-RECORD                                        04610000
                   MOVE CM-BRANCH-NUMBER TO CL-BRANCH-NUMBER            04620000
                   MOVE CM-SALESREP-NUMBER TO CL-SALESREP-NUMBER        04630000
                   PERFORM 223-MOVE-SALESREP-NAME                       04640008
               WHEN CM-BRANCH-NUMBER > OLD-BRANCH-NUMBER                04650000
                   MOVE CM-BRANCH-NUMBER TO CL-BRANCH-NUMBER            04660000
                   MOVE CM-SALESREP-NUMBER TO CL-SALESREP-NUMBER        04670000
                   PERFORM 223-MOVE-SALESREP-NAME                       04680008
               WHEN NOT (CM-SALESREP-NUMBER = OLD-SALESREP-NUMBER)      04690000
                   MOVE CM-SALESREP-NUMBER TO CL-SALESREP-NUMBER        04700000
                   PERFORM 223-MOVE-SALESREP-NAME                       04710008
                   MOVE SPACES TO CL-BRANCH-NUMBER                      04720000
               WHEN OTHER                                               04730000
                   MOVE SPACES TO CL-BRANCH-NUMBER                      04740000
                   MOVE SPACES TO CL-SALESREP-NUMBER                    04750000
                   MOVE SPACES TO CL-SALESREP-NAME                      04760034
           END-EVALUATE.                                                04770000
                                                                        04780000
           *> MOVE THE DATA PULLED FROM THE INPUT FILE INTO THE         04790000
           *> CUSTOMER LINE RECORD FOR LATER OUTPUT                     04800000
           MOVE CM-CUSTOMER-NUMBER  TO CL-CUSTOMER-NUMBER.              04810000
           MOVE CM-CUSTOMER-NAME    TO CL-CUSTOMER-NAME.                04820000
           MOVE CM-SALES-THIS-YTD   TO CL-SALES-THIS-YTD.               04830000
           MOVE CM-SALES-LAST-YTD   TO CL-SALES-LAST-YTD.               04840000
                                                                        04850000
           *> CALCULATE THE DIFFERENCE BETWEEN THIS YEAR'S SALES AND    04860000
           *> AND LAST THEN SAVE THESE RESULT TO CHANGE-AMOUNT AND      04870000
           COMPUTE CHANGE-AMOUNT =                                      04880000
               CM-SALES-THIS-YTD - CM-SALES-LAST-YTD.                   04890000
           MOVE CHANGE-AMOUNT TO CL-CHANGE-AMOUNT.                      04900000
                                                                        04910000
           *> CALCULATE THE PERCENT FOR THE CHANGE IN SALES BETWEEN     04920000
           *> THIS AND LAST YTD, IF THERE WAS NO LAST YEAR SALES        04930000
           *> NUMBER WE MOVE 999.9 TO THE PERECENTAGE SINCE IT'S        04940000
           *> A DIVIDE BY ZERO ERROR OTHERWISE                          04950000
           IF CM-SALES-LAST-YTD = ZERO                                  04960000
               MOVE "  N/A " TO CL-CHANGE-PERCENT-R                     04970000
           ELSE                                                         04980000
               COMPUTE CL-CHANGE-PERCENT ROUNDED =                      04990000
                   CHANGE-AMOUNT * 100 / CM-SALES-LAST-YTD              05000000
                   ON SIZE ERROR                                        05010000
                       MOVE "OVRFLW" TO CL-CHANGE-PERCENT-R.            05020000
                                                                        05030000
           *> PRINT THIS CUSTOMERS INFORMATION TO THE OUTPUT FILE       05040000
           MOVE CUSTOMER-LINE TO PRINT-AREA.                            05050000
           PERFORM 225-WRITE-REPORT-LINE.                               05060000
                                                                        05070000
           *> ADD THIS CUSTOMERS SALES TO THE SALESREP TOTALS           05080000
           ADD CM-SALES-THIS-YTD TO SALESREP-TOTAL-THIS-YTD.            05090000
           ADD CM-SALES-LAST-YTD TO SALESREP-TOTAL-LAST-YTD.            05100000
                                                                        05110008
      **************************************************************    05120008
      * TRIES TO FIND THE SALESREP NAME THAT MATCHES THE CURRENT   *    05130034
      * SALESREP NUMBER AND MOVES IT INTO THE CUSTOMER LINE        *    05140034
      * MOVES "UNKNOWN" IF NO MATCHING SALESREP NUMBER WAS FOUND   *    05150034
      **************************************************************    05160008
       223-MOVE-SALESREP-NAME.                                          05170010
           SET SRT-INDEX TO 1.                                          05180010
           SEARCH SALESREP-GROUP                                        05190010
               AT END                                                   05200010
                   MOVE "UNKNOWN" TO CL-SALESREP-NAME                   05210010
               WHEN SALESREP-NUMBER (SRT-INDEX) = CM-SALESREP-NUMBER    05220010
                   MOVE SALESREP-NAME (SRT-INDEX) TO CL-SALESREP-NAME   05230010
           END-SEARCH.                                                  05240010
                                                                        05250008
      **************************************************************    05260000
      * PRINT ALL THE HEADER LINES TO THE OUTPUT FILE, RAN ONCE    *    05270000
      * FOR EVERY PAGE                                             *    05280000
      **************************************************************    05290000
       225-WRITE-REPORT-LINE.                                           05300000
           WRITE PRINT-AREA.                                            05310000
           ADD 1 TO LINE-COUNT.                                         05320000
                                                                        05330000
      **************************************************************    05340000
      * PRINT ALL THE HEADER LINES TO THE OUTPUT FILE, RAN ONCE    *    05350000
      * FOR EVERY PAGE                                             *    05360000
      **************************************************************    05370000
       230-PRINT-HEADING-LINES.                                         05380000
                                                                        05390000
           *> HEADERS ARE PLACED AT THE START OF EVERY PAGE             05400000
           *> SO WE INCREASE THE PAGE COUNT HERE                        05410000
           ADD 1 TO PAGE-COUNT.                                         05420000
           MOVE PAGE-COUNT     TO HL1-PAGE-NUMBER.                      05430000
                                                                        05440000
           *> PRINT EACH HEADER LINE TO THE OUTPUT FILE                 05450000
           MOVE HEADING-LINE-1 TO PRINT-AREA.                           05460000
           WRITE PRINT-AREA.                                            05470000
           MOVE HEADING-LINE-2 TO PRINT-AREA.                           05480000
           WRITE PRINT-AREA.                                            05490000
           MOVE HEADING-LINE-3 TO PRINT-AREA.                           05500000
           WRITE PRINT-AREA.                                            05510000
           MOVE HEADING-LINE-4 TO PRINT-AREA.                           05520000
           WRITE PRINT-AREA.                                            05530000
           MOVE HEADING-LINE-5 TO PRINT-AREA.                           05540000
           WRITE PRINT-AREA.                                            05550000
           MOVE HEADING-LINE-6 TO PRINT-AREA.                           05560000
           WRITE PRINT-AREA.                                            05570000
                                                                        05580000
           *> RESET THE LINE COUNTER SINCE EVERY HEADER IS THE START    05590000
           *> OF A NEW PAGE                                             05600000
           MOVE ZERO TO LINE-COUNT.                                     05610000
                                                                        05620000
      **************************************************************    05630000
      * PRINTS THE CURRENT BRANCH LINE TOTALS, RAN ONCE FOR EVERY  *    05640000
      * BRANCH. ALSO CALCULATES THE CHANGE IN THE BRANCH           *    05650000
      **************************************************************    05660000
       240-PRINT-BRANCH-LINE.                                           05670000
                                                                        05680000
           *> MOVE THE BRANCH TOTALS TO THE BRANCH TOTAL LINE           05690000
           MOVE BRANCH-TOTAL-THIS-YTD TO BTL-SALES-THIS-YTD.            05700000
           MOVE BRANCH-TOTAL-LAST-YTD TO BTL-SALES-LAST-YTD.            05710000
                                                                        05720000
           *> CALCULATE THE CHANGE BETWEEN THIS-YTD AND LAST            05730000
           *> FOR THE CURRENT BRANCH AND ADD IT TO THE TOTAL LINE       05740000
           COMPUTE CHANGE-AMOUNT =                                      05750000
               BRANCH-TOTAL-THIS-YTD - BRANCH-TOTAL-LAST-YTD.           05760000
           MOVE CHANGE-AMOUNT TO BTL-CHANGE-AMOUNT.                     05770000
                                                                        05780000
           *> CALCULATE THE CHANGE PERCENT BETWEEN YTD'S                05790000
           *> THEN MOVE TO THE BRANCH TOTAL LINE                        05800000
           IF BRANCH-TOTAL-LAST-YTD = ZERO                              05810000
               MOVE "  N/A " TO BTL-CHANGE-PERCENT-R                    05820000
           ELSE                                                         05830000
               COMPUTE BTL-CHANGE-PERCENT ROUNDED =                     05840000
                   CHANGE-AMOUNT * 100 / BRANCH-TOTAL-LAST-YTD          05850000
                   ON SIZE ERROR                                        05860000
                       MOVE "OVRFLW" TO BTL-CHANGE-PERCENT-R.           05870000
                                                                        05880000
           *> PRINT BRANCH LINE                                         05890000
           MOVE BRANCH-TOTAL-LINE TO PRINT-AREA.                        05900000
           PERFORM 225-WRITE-REPORT-LINE.                               05910000
                                                                        05920000
           *> WRITE A BLANK SPACER LINE                                 05930000
           MOVE SPACES TO PRINT-AREA.                                   05940000
           PERFORM 225-WRITE-REPORT-LINE.                               05950000
                                                                        05960000
           *> ADD THE BRANCH TOTALS TO THE GRAND TOTALS                 05970000
           ADD BRANCH-TOTAL-THIS-YTD TO GRAND-TOTAL-THIS-YTD.           05980000
           ADD BRANCH-TOTAL-LAST-YTD TO GRAND-TOTAL-LAST-YTD.           05990000
                                                                        06000000
           *> ZERO OUT THE BRANCH TOTALS                                06010000
           INITIALIZE BRANCH-TOTAL-THIS-YTD                             06020009
                      BRANCH-TOTAL-LAST-YTD.                            06030009
                                                                        06040000
      **************************************************************    06050000
      * PRINTS THE CURRENT SALESREP'S TOTALS, RAN ONCE FOR EVERY   *    06060000
      * SALESREP. ALSO CALCULATES THE CHANGE BETWEEN YEARS         *    06070000
      **************************************************************    06080000
       250-PRINT-SALESREP-LINE.                                         06090000
                                                                        06100000
           *> MOVE THE SALESREP TOTALS TO THE SALESREP TOTAL LINE       06110000
           MOVE SALESREP-TOTAL-THIS-YTD TO STL-SALES-THIS-YTD.          06120000
           MOVE SALESREP-TOTAL-LAST-YTD TO STL-SALES-LAST-YTD.          06130000
                                                                        06140000
           *> CALCULATE THE CHANGE BETWEEN THIS-YTD AND LAST            06150000
           *> FOR THE CURRENT SALESREP AND ADD IT TO THE TOTAL LINE     06160000
           COMPUTE CHANGE-AMOUNT =                                      06170000
               SALESREP-TOTAL-THIS-YTD - SALESREP-TOTAL-LAST-YTD.       06180000
           MOVE CHANGE-AMOUNT TO STL-CHANGE-AMOUNT.                     06190000
                                                                        06200000
           *> CALCULATE THE CHANGE PERCENT BETWEEN YTD'S                06210000
           *> THEN MOVE TO THE SALESREP TOTAL LINE                      06220000
           IF SALESREP-TOTAL-LAST-YTD = ZERO                            06230000
               MOVE "  N/A " TO STL-CHANGE-PERCENT-R                    06240000
           ELSE                                                         06250000
               COMPUTE STL-CHANGE-PERCENT ROUNDED =                     06260000
                   CHANGE-AMOUNT * 100 / SALESREP-TOTAL-LAST-YTD        06270000
                   ON SIZE ERROR                                        06280000
                       MOVE "OVRFLW" TO STL-CHANGE-PERCENT-R.           06290000
                                                                        06300000
           *> PRINT SALESREP LINE                                       06310000
           MOVE SALESREP-TOTAL-LINE TO PRINT-AREA.                      06320000
           PERFORM 225-WRITE-REPORT-LINE.                               06330000
                                                                        06340000
           *> PRINT A SPACER LINE                                       06350000
           MOVE SPACES TO PRINT-AREA.                                   06360000
           PERFORM 225-WRITE-REPORT-LINE.                               06370000
                                                                        06380000
           *> ADD THE SALESREP TOTALS TO THE BRANCH TOTALS              06390000
           *> WHEN A BRANCH IS PRINTED THEN THOSE TOTALS ARE MOVED      06400000
           *> TO THE GRAND TOTALS                                       06410000
           *> CUSTOMER->SALESREP->BRANCH->GRAND-TOTAL                   06420000
           ADD SALESREP-TOTAL-THIS-YTD TO BRANCH-TOTAL-THIS-YTD.        06430000
           ADD SALESREP-TOTAL-LAST-YTD TO BRANCH-TOTAL-LAST-YTD.        06440000
                                                                        06450000
           *> ZERO OUT THE SALESREP TOTALS                              06460000
           MOVE ZERO TO SALESREP-TOTAL-THIS-YTD.                        06470000
           MOVE ZERO TO SALESREP-TOTAL-LAST-YTD.                        06480000
      **************************************************************    06490000
      * PRINTS THE GRAND TOTALS FOR ALL THE CUSTOMERS, RAN ONCE    *    06500000
      * AT THE VERY END OF THE PROGRAM WHEN ALL CUSTOMERS HAVE     *    06510000
      * BEEN PRINTED                                               *    06520000
      **************************************************************    06530000
       300-PRINT-GRAND-TOTALS.                                          06540000
                                                                        06550000
           *> MOVE THE GRAND TOTALS FOR THE SALES TO THE                06560000
           *> OUTPUT LINE FOR GRAND TOTALS                              06570000
           MOVE GRAND-TOTAL-THIS-YTD TO GTL-SALES-THIS-YTD.             06580000
           MOVE GRAND-TOTAL-LAST-YTD TO GTL-SALES-LAST-YTD.             06590000
                                                                        06600000
           *> COMPUTE THE GRAND TOTAL FOR THE CHANGE AMOUNT             06610000
           COMPUTE CHANGE-AMOUNT =                                      06620000
               GRAND-TOTAL-THIS-YTD - GRAND-TOTAL-LAST-YTD.             06630000
           MOVE CHANGE-AMOUNT TO GTL-CHANGE-AMOUNT.                     06640000
                                                                        06650000
           *> CALCULATE THE TOTAL CHANGE IN PERCENT BETWEEN             06660000
           *> THIS YTD AND LAST YTD FOR ALL CUSTOMERS                   06670000
           *> IF THERE WAS NO LAST YEAR FOR ANYONE DEFAULT TO           06680000
           *> A PERCENT OF 999.9 TO AVOID DIVIDE BY ZERO ERROR          06690000
           IF GRAND-TOTAL-LAST-YTD = ZERO                               06700000
               MOVE "  N/A " TO GTL-CHANGE-PERCENT-R                    06710000
           ELSE                                                         06720000
               COMPUTE GTL-CHANGE-PERCENT ROUNDED =                     06730000
                   CHANGE-AMOUNT * 100 / GRAND-TOTAL-LAST-YTD           06740000
                   ON SIZE ERROR                                        06750000
                       MOVE "OVRFLW" TO GTL-CHANGE-PERCENT-R.           06760000
                                                                        06770000
           *> PRINT THE GRAND-TOTAL TO THE OUTPUT FILE                  06780000
           MOVE GRAND-TOTAL-LINE TO PRINT-AREA.                         06790000
           PERFORM 225-WRITE-REPORT-LINE.                               06800000
