       IDENTIFICATION DIVISION.
       PROGRAM-ID. InCollege.
       ENVIRONMENT DIVISION.
       CONFIGURATION SECTION.
       SOURCE-COMPUTER. GNU-COBOL.
       OBJECT-COMPUTER. GNU-COBOL.

       INPUT-OUTPUT SECTION.
       FILE-CONTROL.
           SELECT INPUT-FILE
               ASSIGN TO 'InCollege-Input.txt'
               ORGANIZATION IS LINE SEQUENTIAL
               FILE STATUS IS WS-FS-IN.
           SELECT OUTPUT-FILE
               ASSIGN TO 'InCollege-Output.txt'
               ORGANIZATION IS LINE SEQUENTIAL
               FILE STATUS IS WS-FS-OUT.
           SELECT ACCOUNTS-FILE
               ASSIGN TO 'Accounts.dat'
               ORGANIZATION IS LINE SEQUENTIAL
               FILE STATUS IS WS-FS-ACCT.
           SELECT PROFILES-FILE
               ASSIGN TO 'Profiles.dat'
               ORGANIZATION IS LINE SEQUENTIAL
               FILE STATUS IS WS-FS-PROF.

       DATA DIVISION.
       FILE SECTION.
       FD  INPUT-FILE.
       01  IN-LINE                  PIC X(300).
       FD  OUTPUT-FILE.
       01  OUT-LINE                 PIC X(300).
       FD  ACCOUNTS-FILE.
       01  ACCT-REC                 PIC X(300).
       FD  PROFILES-FILE.
       01  PROF-REC                 PIC X(600).

       WORKING-STORAGE SECTION.
       01  WS-FS-IN                 PIC XX.
       01  WS-FS-OUT                PIC XX.
       01  WS-FS-ACCT               PIC XX.
       01  WS-FS-PROF               PIC XX.

       01  WS-CREATED               PIC X VALUE 'N'.
       01  WS-ATTEMPTS              PIC 9 VALUE 0.
       01  WS-IDX-DISP              PIC 9.
       01  WS-OK                    PIC X VALUE 'N'.  *> loop guard

       01  INPUT-MODE               PIC X VALUE 'F'.
           88 FROM-FILE                        VALUE 'F'.
           88 FROM-CONSOLE                     VALUE 'C'.

       01  WS-EOF-IN                PIC X VALUE 'N'.
           88 EOF-IN                          VALUE 'Y'.
           88 NOT-EOF-IN                      VALUE 'N'.

       01  WS-FOUND                 PIC X VALUE 'N'.
       01  WS-VALID-YEAR            PIC X VALUE 'N'.
       01  WS-MSG                   PIC X(300).
       01  WS-PROMPT                PIC X(300).
       01  WS-LAST-IN               PIC X(300).
       01  YEAR-TXT                 PIC X(4).
       01  F1                       PIC X(120).
       01  F2                       PIC X(120).
       01  F3                       PIC X(120).
       01  F4                       PIC X(120).
       01  F5                       PIC X(120).
       01  F6                       PIC X(200).

       01  TMP1                     PIC X(300).
       01  TMP2                     PIC X(300).
       01  TMP3                     PIC X(300).
       01  I                        PIC 9 VALUE 0.
       01  N                        PIC 9 VALUE 0.

       01  USERNAME                 PIC X(20).
       01  PASSWORD                 PIC X(20).
       01  LOGGED-IN                PIC X VALUE 'N'.
           88 IS-LOGGED-IN                    VALUE 'Y'.

       01  PROF-FIRST               PIC X(30).
       01  PROF-LAST                PIC X(30).
       01  PROF-UNIV                PIC X(60).
       01  PROF-MAJOR               PIC X(40).
       01  PROF-YEAR                PIC 9(4).
       01  PROF-ABOUT               PIC X(200).

       01  EXP-CNT                  PIC 9 VALUE 0.
       01  EXP-TITLE     OCCURS 3   PIC X(40).
       01  EXP-COMP      OCCURS 3   PIC X(40).
       01  EXP-DATES     OCCURS 3   PIC X(30).
       01  EXP-DESC      OCCURS 3   PIC X(100).

       01  EDU-CNT                  PIC 9 VALUE 0.
       01  EDU-DEG       OCCURS 3   PIC X(40).
       01  EDU-SCHOOL    OCCURS 3   PIC X(60).
       01  EDU-YEARS     OCCURS 3   PIC X(20).

       PROCEDURE DIVISION.
       MAIN-SECTION.
           PERFORM INIT-FILES

           MOVE "Welcome to InCollege!" TO WS-MSG
           PERFORM OUT-MSG

           PERFORM MAIN-MENU UNTIL IS-LOGGED-IN OR EOF-IN

           IF IS-LOGGED-IN
              STRING "Welcome, " DELIMITED BY SIZE
                     FUNCTION TRIM(USERNAME) DELIMITED BY SIZE
                     "!" DELIMITED BY SIZE
                     INTO WS-MSG
              END-STRING
              PERFORM OUT-MSG
              PERFORM POST-LOGIN-MENU UNTIL EOF-IN
           END-IF

           PERFORM CLOSE-FILES
           STOP RUN.

       INIT-FILES.
           OPEN INPUT  INPUT-FILE
           IF WS-FS-IN NOT = "00"
              MOVE "ERROR: cannot open InCollege-Input.txt" TO WS-MSG
              PERFORM OUT-MSG
              SET EOF-IN TO TRUE
           END-IF

           OPEN OUTPUT OUTPUT-FILE

           OPEN INPUT  ACCOUNTS-FILE
           IF WS-FS-ACCT NOT = "00"
              OPEN OUTPUT ACCOUNTS-FILE
              CLOSE ACCOUNTS-FILE
           END-IF
           CLOSE ACCOUNTS-FILE

           OPEN INPUT  PROFILES-FILE
           IF WS-FS-PROF NOT = "00"
              OPEN OUTPUT PROFILES-FILE
              CLOSE PROFILES-FILE
           END-IF
           CLOSE PROFILES-FILE.

       CLOSE-FILES.
           CLOSE INPUT-FILE
           CLOSE OUTPUT-FILE.

       OUT-MSG.
           DISPLAY WS-MSG
           MOVE WS-MSG TO OUT-LINE
           WRITE OUT-LINE.

       READ-NEXT-INPUT.
           IF FROM-FILE
              READ INPUT-FILE
                 AT END SET EOF-IN TO TRUE
              END-READ
           ELSE
              ACCEPT IN-LINE FROM CONSOLE
              IF FUNCTION UPPER-CASE(FUNCTION TRIM(IN-LINE)) = "EOF"
                 OR FUNCTION UPPER-CASE(FUNCTION TRIM(IN-LINE)) = "QUIT"
                 SET EOF-IN TO TRUE
              END-IF
           END-IF.

       PROMPT-AND-READ.
           MOVE WS-PROMPT TO WS-MSG
           PERFORM OUT-MSG
           PERFORM READ-NEXT-INPUT
           IF NOT EOF-IN
              MOVE IN-LINE TO WS-LAST-IN
           END-IF.

       MAIN-MENU.
           MOVE "1. Log In"             TO WS-MSG
           PERFORM OUT-MSG
           MOVE "2. Create New Account" TO WS-MSG
           PERFORM OUT-MSG
           MOVE "Enter your choice:"    TO WS-MSG
           PERFORM OUT-MSG
           PERFORM READ-NEXT-INPUT
           IF EOF-IN EXIT PARAGRAPH END-IF
           MOVE IN-LINE TO TMP1
           IF TMP1 = "1"
              PERFORM DO-LOGIN
           ELSE
              IF TMP1 = "2"
                 PERFORM DO-CREATE-ACCOUNT
              ELSE
                 MOVE "Invalid selection." TO WS-MSG
                 PERFORM OUT-MSG
              END-IF
           END-IF.

       DO-LOGIN.
           MOVE "Please enter your username:" TO WS-PROMPT
           PERFORM PROMPT-AND-READ
           MOVE WS-LAST-IN TO USERNAME
           IF EOF-IN EXIT PARAGRAPH END-IF
           MOVE "Please enter your password:" TO WS-PROMPT
           PERFORM PROMPT-AND-READ
           MOVE WS-LAST-IN TO PASSWORD
           IF EOF-IN EXIT PARAGRAPH END-IF

           IF FUNCTION TRIM(USERNAME) = SPACES OR FUNCTION TRIM(PASSWORD) = SPACES
              MOVE "Username and Password required." TO WS-MSG
              PERFORM OUT-MSG
              EXIT PARAGRAPH
           END-IF

           PERFORM VERIFY-LOGIN
           IF WS-FOUND = 'Y'
              SET IS-LOGGED-IN TO TRUE
           ELSE
              MOVE "Invalid username or password." TO WS-MSG
              PERFORM OUT-MSG
           END-IF.

       VERIFY-LOGIN.
           MOVE 'N' TO WS-FOUND
           OPEN INPUT ACCOUNTS-FILE
           PERFORM UNTIL 1 = 2
              READ ACCOUNTS-FILE INTO ACCT-REC
                 AT END EXIT PERFORM
              END-READ
              UNSTRING ACCT-REC DELIMITED BY '|'
                       INTO TMP1 TMP2
              IF FUNCTION TRIM(TMP1) = FUNCTION TRIM(USERNAME)
                 AND FUNCTION TRIM(TMP2) = FUNCTION TRIM(PASSWORD)
                 MOVE 'Y' TO WS-FOUND
                 EXIT PERFORM
              END-IF
           END-PERFORM
           CLOSE ACCOUNTS-FILE.

       CHECK-ACCOUNT-EXISTS.
           MOVE 'N' TO WS-FOUND
           OPEN INPUT ACCOUNTS-FILE
           PERFORM UNTIL 1 = 2
              READ ACCOUNTS-FILE INTO ACCT-REC
                 AT END EXIT PERFORM
              END-READ
              UNSTRING ACCT-REC DELIMITED BY '|'
                       INTO TMP1 TMP2
              IF FUNCTION TRIM(TMP1) = FUNCTION TRIM(USERNAME)
                 MOVE 'Y' TO WS-FOUND
                 EXIT PERFORM
              END-IF
           END-PERFORM
           CLOSE ACCOUNTS-FILE.

       CHECK-USERNAME-SPACES.
           MOVE 'N' TO WS-FOUND
           MOVE 1 TO N
           PERFORM UNTIL N > FUNCTION LENGTH(FUNCTION TRIM(USERNAME))
              IF USERNAME(N:1) = ' '
                 MOVE 'Y' TO WS-FOUND
                 EXIT PERFORM
              END-IF
              ADD 1 TO N
           END-PERFORM.

       DO-CREATE-ACCOUNT.
           MOVE 'N' TO WS-CREATED
           MOVE 0   TO WS-ATTEMPTS

           PERFORM UNTIL WS-CREATED = 'Y' OR EOF-IN
              ADD 1 TO WS-ATTEMPTS

              MOVE "Create Account - Enter a username (max 20 chars, no spaces):"
                   TO WS-PROMPT
              PERFORM PROMPT-AND-READ
              IF EOF-IN EXIT PERFORM END-IF
              MOVE WS-LAST-IN TO USERNAME

              MOVE "Enter a password (max 20 chars):" TO WS-PROMPT
              PERFORM PROMPT-AND-READ
              IF EOF-IN EXIT PERFORM END-IF
              MOVE WS-LAST-IN TO PASSWORD

              IF FUNCTION TRIM(USERNAME) = SPACES
                 MOVE "Username cannot be empty." TO WS-MSG
                 PERFORM OUT-MSG
              ELSE
                 PERFORM CHECK-USERNAME-SPACES
                 IF WS-FOUND = 'Y'
                    MOVE "Username must not contain spaces." TO WS-MSG
                    PERFORM OUT-MSG
                 ELSE
                    PERFORM CHECK-ACCOUNT-EXISTS
                    IF WS-FOUND = 'Y'
                       MOVE "That username is already taken. Try another."
                         TO WS-MSG
                       PERFORM OUT-MSG
                    ELSE
                       STRING FUNCTION TRIM(USERNAME) DELIMITED BY SIZE
                              "|"                       DELIMITED BY SIZE
                              FUNCTION TRIM(PASSWORD)   DELIMITED BY SIZE
                              INTO ACCT-REC
                       END-STRING
                       OPEN EXTEND ACCOUNTS-FILE
                       WRITE ACCT-REC
                       CLOSE ACCOUNTS-FILE
                       MOVE 'Y' TO WS-CREATED
                    END-IF
                 END-IF
              END-IF
           END-PERFORM

           IF WS-CREATED = 'Y'
              MOVE "Account created successfully." TO WS-MSG
              PERFORM OUT-MSG
              PERFORM CREATE-OR-EDIT-PROFILE
           ELSE
              MOVE "Account was not created." TO WS-MSG
              PERFORM OUT-MSG
           END-IF.

       POST-LOGIN-MENU.
           MOVE '1. Create/Edit My Profile' TO WS-MSG
           PERFORM OUT-MSG
           MOVE '2. View My Profile' TO WS-MSG
           PERFORM OUT-MSG
           MOVE '3. Search for User' TO WS-MSG
           PERFORM OUT-MSG
           MOVE '4. Learn a New Skill' TO WS-MSG
           PERFORM OUT-MSG
           MOVE 'Enter your choice:' TO WS-MSG
           PERFORM OUT-MSG
           PERFORM READ-NEXT-INPUT
           IF EOF-IN EXIT PARAGRAPH END-IF
           MOVE IN-LINE TO TMP1
           EVALUATE TMP1
              WHEN '1'
                 PERFORM CREATE-OR-EDIT-PROFILE
              WHEN '2'
                 PERFORM VIEW-MY-PROFILE
              WHEN '3'
                 PERFORM SEARCH-USER
              WHEN '4'
                 MOVE 'Feature coming soon.' TO WS-MSG
                 PERFORM OUT-MSG
              WHEN OTHER
                 MOVE 'Invalid selection.' TO WS-MSG
                 PERFORM OUT-MSG
           END-EVALUATE.

       CLEAR-PROFILE-BUFFERS.
           MOVE SPACES TO PROF-FIRST PROF-LAST PROF-UNIV PROF-MAJOR PROF-ABOUT
           MOVE 0      TO PROF-YEAR
           MOVE 0      TO EXP-CNT EDU-CNT
           PERFORM VARYING I FROM 1 BY 1 UNTIL I > 3
              MOVE SPACES TO EXP-TITLE(I) EXP-COMP(I) EXP-DATES(I) EXP-DESC(I)
              MOVE SPACES TO EDU-DEG(I)   EDU-SCHOOL(I) EDU-YEARS(I)
           END-PERFORM.

       CHECK-YEAR.
           MOVE 'N' TO WS-VALID-YEAR
           IF FUNCTION TEST-NUMVAL(TMP1) = 0
              IF FUNCTION LENGTH(FUNCTION TRIM(TMP1)) = 4
                 IF FUNCTION NUMVAL(TMP1) >= 1900 AND FUNCTION NUMVAL(TMP1) <= 2100
                    MOVE 'Y' TO WS-VALID-YEAR
                 END-IF
              END-IF
           END-IF.

       CREATE-OR-EDIT-PROFILE.
           MOVE '--- Create/Edit Profile ---' TO WS-MSG
           PERFORM OUT-MSG
           PERFORM CLEAR-PROFILE-BUFFERS

           MOVE 'Enter First Name:' TO WS-PROMPT
           PERFORM PROMPT-AND-READ
           MOVE WS-LAST-IN TO PROF-FIRST
           IF EOF-IN EXIT PARAGRAPH END-IF

           MOVE 'Enter Last Name:' TO WS-PROMPT
           PERFORM PROMPT-AND-READ
           MOVE WS-LAST-IN TO PROF-LAST
           IF EOF-IN EXIT PARAGRAPH END-IF

           MOVE 'Enter University/College Attended:' TO WS-PROMPT
           PERFORM PROMPT-AND-READ
           MOVE WS-LAST-IN TO PROF-UNIV
           IF EOF-IN EXIT PARAGRAPH END-IF

           MOVE 'Enter Major:' TO WS-PROMPT
           PERFORM PROMPT-AND-READ
           MOVE WS-LAST-IN TO PROF-MAJOR
           IF EOF-IN EXIT PARAGRAPH END-IF

           MOVE 'N' TO WS-VALID-YEAR
           PERFORM UNTIL WS-VALID-YEAR = 'Y'
              MOVE 'Enter Graduation Year (YYYY):' TO WS-PROMPT
              PERFORM PROMPT-AND-READ
              MOVE WS-LAST-IN TO TMP1
              IF EOF-IN EXIT PARAGRAPH END-IF
              PERFORM CHECK-YEAR
              IF WS-VALID-YEAR = 'Y'
                 MOVE FUNCTION NUMVAL(TMP1) TO PROF-YEAR
              ELSE
                 MOVE 'Invalid year. Enter a 4-digit year between 1900 and 2100.'
                   TO WS-MSG
                 PERFORM OUT-MSG
              END-IF
           END-PERFORM

           MOVE 'Enter About Me (optional, max 200 chars, enter blank line to skip):'
                TO WS-PROMPT
           PERFORM PROMPT-AND-READ
           MOVE WS-LAST-IN TO PROF-ABOUT
           IF EOF-IN EXIT PARAGRAPH END-IF

           *> Experience count (0–3)
           MOVE 'N' TO WS-OK
           PERFORM UNTIL WS-OK = 'Y' OR EOF-IN
              MOVE 'How many Experience entries (0-3)?' TO WS-PROMPT
              PERFORM PROMPT-AND-READ
              MOVE WS-LAST-IN TO TMP1
              IF EOF-IN EXIT PARAGRAPH END-IF
              IF FUNCTION TEST-NUMVAL(TMP1) = 0
                 AND FUNCTION NUMVAL(TMP1) >= 0
                 AND FUNCTION NUMVAL(TMP1) <= 3
                 MOVE FUNCTION NUMVAL(TMP1) TO EXP-CNT
                 MOVE 'Y' TO WS-OK
              ELSE
                 MOVE 'Please enter a number 0-3.' TO WS-MSG
                 PERFORM OUT-MSG
              END-IF
           END-PERFORM

           PERFORM VARYING I FROM 1 BY 1 UNTIL I > EXP-CNT
              MOVE I TO WS-IDX-DISP
              STRING 'Experience #' DELIMITED BY SIZE
                     WS-IDX-DISP     DELIMITED BY SIZE
                     ' - Title:'     DELIMITED BY SIZE
                     INTO WS-MSG
              END-STRING
              PERFORM OUT-MSG
              PERFORM READ-NEXT-INPUT
              IF EOF-IN EXIT PERFORM END-IF
              MOVE IN-LINE TO EXP-TITLE(I)

              MOVE I TO WS-IDX-DISP
              STRING 'Experience #' DELIMITED BY SIZE
                     WS-IDX-DISP     DELIMITED BY SIZE
                     ' - Company/Organization:' DELIMITED BY SIZE
                     INTO WS-MSG
              END-STRING
              PERFORM OUT-MSG
              PERFORM READ-NEXT-INPUT
              IF EOF-IN EXIT PERFORM END-IF
              MOVE IN-LINE TO EXP-COMP(I)

              MOVE I TO WS-IDX-DISP
              STRING 'Experience #' DELIMITED BY SIZE
                     WS-IDX-DISP     DELIMITED BY SIZE
                     ' - Dates (e.g., Summer 2024):' DELIMITED BY SIZE
                     INTO WS-MSG
              END-STRING
              PERFORM OUT-MSG
              PERFORM READ-NEXT-INPUT
              IF EOF-IN EXIT PERFORM END-IF
              MOVE IN-LINE TO EXP-DATES(I)

              MOVE I TO WS-IDX-DISP
              STRING 'Experience #' DELIMITED BY SIZE
                     WS-IDX-DISP     DELIMITED BY SIZE
                     ' - Description (optional, max 100 chars, blank to skip):'
                     DELIMITED BY SIZE
                     INTO WS-MSG
              END-STRING
              PERFORM OUT-MSG
              PERFORM READ-NEXT-INPUT
              IF EOF-IN EXIT PERFORM END-IF
              MOVE IN-LINE TO EXP-DESC(I)
           END-PERFORM

           *> Education count (0–3)
           MOVE 'N' TO WS-OK
           PERFORM UNTIL WS-OK = 'Y' OR EOF-IN
              MOVE 'How many Education entries (0-3)?' TO WS-PROMPT
              PERFORM PROMPT-AND-READ
              MOVE WS-LAST-IN TO TMP1
              IF EOF-IN EXIT PARAGRAPH END-IF
              IF FUNCTION TEST-NUMVAL(TMP1) = 0
                 AND FUNCTION NUMVAL(TMP1) >= 0
                 AND FUNCTION NUMVAL(TMP1) <= 3
                 MOVE FUNCTION NUMVAL(TMP1) TO EDU-CNT
                 MOVE 'Y' TO WS-OK
              ELSE
                 MOVE 'Please enter a number 0-3.' TO WS-MSG
                 PERFORM OUT-MSG
              END-IF
           END-PERFORM

           PERFORM VARYING I FROM 1 BY 1 UNTIL I > EDU-CNT
              MOVE I TO WS-IDX-DISP
              STRING 'Education #'  DELIMITED BY SIZE
                     WS-IDX-DISP     DELIMITED BY SIZE
                     ' - Degree:'     DELIMITED BY SIZE
                     INTO WS-MSG
              END-STRING
              PERFORM OUT-MSG
              PERFORM READ-NEXT-INPUT
              IF EOF-IN EXIT PERFORM END-IF
              MOVE IN-LINE TO EDU-DEG(I)

              MOVE I TO WS-IDX-DISP
              STRING 'Education #'  DELIMITED BY SIZE
                     WS-IDX-DISP     DELIMITED BY SIZE
                     ' - University/College:' DELIMITED BY SIZE
                     INTO WS-MSG
              END-STRING
              PERFORM OUT-MSG
              PERFORM READ-NEXT-INPUT
              IF EOF-IN EXIT PERFORM END-IF
              MOVE IN-LINE TO EDU-SCHOOL(I)

              MOVE I TO WS-IDX-DISP
              STRING 'Education #'  DELIMITED BY SIZE
                     WS-IDX-DISP     DELIMITED BY SIZE
                     ' - Years Attended (e.g., 2023-2025):' DELIMITED BY SIZE
                     INTO WS-MSG
              END-STRING
              PERFORM OUT-MSG
              PERFORM READ-NEXT-INPUT
              IF EOF-IN EXIT PERFORM END-IF
              MOVE IN-LINE TO EDU-YEARS(I)
           END-PERFORM

           PERFORM WRITE-PROFILE-BLOCK
           MOVE 'Profile saved successfully!' TO WS-MSG
           PERFORM OUT-MSG.

       WRITE-PROFILE-BLOCK.
           OPEN EXTEND PROFILES-FILE
           MOVE PROF-YEAR TO YEAR-TXT
           STRING FUNCTION TRIM(USERNAME)   DELIMITED BY SIZE "|"
                  FUNCTION TRIM(PROF-FIRST) DELIMITED BY SIZE "|"
                  FUNCTION TRIM(PROF-LAST)  DELIMITED BY SIZE "|"
                  FUNCTION TRIM(PROF-UNIV)  DELIMITED BY SIZE "|"
                  FUNCTION TRIM(PROF-MAJOR) DELIMITED BY SIZE "|"
                  YEAR-TXT                  DELIMITED BY SIZE "|"
                  FUNCTION TRIM(PROF-ABOUT) DELIMITED BY SIZE
                  INTO PROF-REC
           END-STRING
           WRITE PROF-REC

           PERFORM VARYING I FROM 1 BY 1 UNTIL I > EXP-CNT
              IF FUNCTION TRIM(EXP-TITLE(I)) NOT = SPACES OR
                 FUNCTION TRIM(EXP-COMP(I))  NOT = SPACES OR
                 FUNCTION TRIM(EXP-DATES(I)) NOT = SPACES OR
                 FUNCTION TRIM(EXP-DESC(I))  NOT = SPACES
                 STRING FUNCTION TRIM(USERNAME)      DELIMITED BY SIZE "|"
                        "EXP"                        DELIMITED BY SIZE "|"
                        FUNCTION TRIM(EXP-TITLE(I))  DELIMITED BY SIZE "|"
                        FUNCTION TRIM(EXP-COMP(I))   DELIMITED BY SIZE "|"
                        FUNCTION TRIM(EXP-DATES(I))  DELIMITED BY SIZE "|"
                        FUNCTION TRIM(EXP-DESC(I))   DELIMITED BY SIZE
                        INTO PROF-REC
                 END-STRING
                 WRITE PROF-REC
              END-IF
           END-PERFORM

           PERFORM VARYING I FROM 1 BY 1 UNTIL I > EDU-CNT
              IF FUNCTION TRIM(EDU-DEG(I))    NOT = SPACES OR
                 FUNCTION TRIM(EDU-SCHOOL(I)) NOT = SPACES OR
                 FUNCTION TRIM(EDU-YEARS(I))  NOT = SPACES
                 STRING FUNCTION TRIM(USERNAME)      DELIMITED BY SIZE "|"
                        "EDU"                        DELIMITED BY SIZE "|"
                        FUNCTION TRIM(EDU-DEG(I))    DELIMITED BY SIZE "|"
                        FUNCTION TRIM(EDU-SCHOOL(I)) DELIMITED BY SIZE "|"
                        FUNCTION TRIM(EDU-YEARS(I))  DELIMITED BY SIZE
                        INTO PROF-REC
                 END-STRING
                 WRITE PROF-REC
              END-IF
           END-PERFORM
           CLOSE PROFILES-FILE.

       VIEW-MY-PROFILE.
           MOVE '--- Your Profile ---' TO WS-MSG
           PERFORM OUT-MSG
           PERFORM CLEAR-PROFILE-BUFFERS
           PERFORM LOAD-LATEST-PROFILE

           IF PROF-FIRST = SPACES AND PROF-LAST = SPACES AND PROF-UNIV = SPACES
              MOVE 'No profile found. Please create a profile first.' TO WS-MSG
              PERFORM OUT-MSG
              EXIT PARAGRAPH
           END-IF

           STRING 'Name: ' DELIMITED BY SIZE
                  FUNCTION TRIM(PROF-FIRST) DELIMITED BY SIZE
                  ' ' DELIMITED BY SIZE
                  FUNCTION TRIM(PROF-LAST)  DELIMITED BY SIZE
                  INTO WS-MSG
           END-STRING
           PERFORM OUT-MSG
           STRING 'University: ' DELIMITED BY SIZE
                  FUNCTION TRIM(PROF-UNIV) DELIMITED BY SIZE
                  INTO WS-MSG
           END-STRING
           PERFORM OUT-MSG
           STRING 'Major: ' DELIMITED BY SIZE
                  FUNCTION TRIM(PROF-MAJOR) DELIMITED BY SIZE
                  INTO WS-MSG
           END-STRING
           PERFORM OUT-MSG
           MOVE PROF-YEAR TO YEAR-TXT
           STRING 'Graduation Year: ' DELIMITED BY SIZE
                  YEAR-TXT DELIMITED BY SIZE
                  INTO WS-MSG
           END-STRING
           PERFORM OUT-MSG
           IF FUNCTION TRIM(PROF-ABOUT) NOT = SPACES
              STRING 'About Me: ' DELIMITED BY SIZE
                     FUNCTION TRIM(PROF-ABOUT) DELIMITED BY SIZE
                     INTO WS-MSG
              END-STRING
              PERFORM OUT-MSG
           END-IF

           IF EXP-CNT > 0
              MOVE 'Experience:' TO WS-MSG
              PERFORM OUT-MSG
              PERFORM VARYING I FROM 1 BY 1 UNTIL I > 3
                 IF FUNCTION TRIM(EXP-TITLE(I)) NOT = SPACES OR
                    FUNCTION TRIM(EXP-COMP(I))  NOT = SPACES OR
                    FUNCTION TRIM(EXP-DATES(I)) NOT = SPACES OR
                    FUNCTION TRIM(EXP-DESC(I))  NOT = SPACES
                    STRING '  Title: '   DELIMITED BY SIZE
                           FUNCTION TRIM(EXP-TITLE(I)) DELIMITED BY SIZE
                           INTO WS-MSG
                    END-STRING
                    PERFORM OUT-MSG
                    STRING '  Company: ' DELIMITED BY SIZE
                           FUNCTION TRIM(EXP-COMP(I))  DELIMITED BY SIZE
                           INTO WS-MSG
                    END-STRING
                    PERFORM OUT-MSG
                    STRING '  Dates: '   DELIMITED BY SIZE
                           FUNCTION TRIM(EXP-DATES(I)) DELIMITED BY SIZE
                           INTO WS-MSG
                    END-STRING
                    PERFORM OUT-MSG
                    IF FUNCTION TRIM(EXP-DESC(I)) NOT = SPACES
                       STRING '  Description: ' DELIMITED BY SIZE
                              FUNCTION TRIM(EXP-DESC(I)) DELIMITED BY SIZE
                              INTO WS-MSG
                       END-STRING
                       PERFORM OUT-MSG
                    END-IF
                 END-IF
              END-PERFORM
           END-IF

           IF EDU-CNT > 0
              MOVE 'Education:' TO WS-MSG
              PERFORM OUT-MSG
              PERFORM VARYING I FROM 1 BY 1 UNTIL I > 3
                 IF FUNCTION TRIM(EDU-DEG(I))    NOT = SPACES OR
                    FUNCTION TRIM(EDU-SCHOOL(I)) NOT = SPACES OR
                    FUNCTION TRIM(EDU-YEARS(I))  NOT = SPACES
                    STRING '  Degree: '     DELIMITED BY SIZE
                           FUNCTION TRIM(EDU-DEG(I)) DELIMITED BY SIZE
                           INTO WS-MSG
                    END-STRING
                    PERFORM OUT-MSG
                    STRING '  University: ' DELIMITED BY SIZE
                           FUNCTION TRIM(EDU-SCHOOL(I)) DELIMITED BY SIZE
                           INTO WS-MSG
                    END-STRING
                    PERFORM OUT-MSG
                    STRING '  Years: '      DELIMITED BY SIZE
                           FUNCTION TRIM(EDU-YEARS(I))  DELIMITED BY SIZE
                           INTO WS-MSG
                    END-STRING
                    PERFORM OUT-MSG
                 END-IF
              END-PERFORM
           END-IF.

       LOAD-LATEST-PROFILE.
           OPEN INPUT PROFILES-FILE
           MOVE 0 TO EXP-CNT EDU-CNT
           PERFORM UNTIL 1 = 2
              READ PROFILES-FILE INTO PROF-REC
                 AT END EXIT PERFORM
              END-READ

              MOVE SPACES TO F1 F2 F3 F4 F5 F6 TMP1
              UNSTRING PROF-REC DELIMITED BY "|"
                       INTO F1 F2 F3 F4 F5 F6 TMP1
              END-UNSTRING

              IF FUNCTION TRIM(F1) = FUNCTION TRIM(USERNAME)
                 IF FUNCTION TRIM(F2) = "EXP"
                    IF EXP-CNT < 3
                       ADD 1 TO EXP-CNT
                       MOVE F3 TO EXP-TITLE(EXP-CNT)
                       MOVE F4 TO EXP-COMP(EXP-CNT)
                       MOVE F5 TO EXP-DATES(EXP-CNT)
                       MOVE F6 TO EXP-DESC(EXP-CNT)
                    END-IF
                 ELSE
                    IF FUNCTION TRIM(F2) = "EDU"
                       IF EDU-CNT < 3
                          ADD 1 TO EDU-CNT
                          MOVE F3 TO EDU-DEG(EDU-CNT)
                          MOVE F4 TO EDU-SCHOOL(EDU-CNT)
                          MOVE F5 TO EDU-YEARS(EDU-CNT)
                       END-IF
                    ELSE
                       MOVE F2 TO PROF-FIRST
                       MOVE F3 TO PROF-LAST
                       MOVE F4 TO PROF-UNIV
                       MOVE F5 TO PROF-MAJOR
                       IF FUNCTION TEST-NUMVAL(F6) = 0
                          MOVE FUNCTION NUMVAL(F6) TO PROF-YEAR
                       ELSE
                          MOVE 0 TO PROF-YEAR
                       END-IF
                       MOVE TMP1 TO PROF-ABOUT
                    END-IF
                 END-IF
              END-IF
           END-PERFORM
           CLOSE PROFILES-FILE.

       SEARCH-USER.
           MOVE 'Feature not implemented in this version.' TO WS-MSG
           PERFORM OUT-MSG.

       END PROGRAM InCollege.
