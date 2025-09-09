*> =======================================================
       *> InCollege - Complete System with Authentication & Features
       *> Inputs  : InCollege-Input.txt (one line per prompt)
       *> Outputs : InCollege-Output.txt (mirrors messages)
       *> Accounts: Accounts.dat (username|password per line)
       *> =======================================================

       IDENTIFICATION DIVISION.
       PROGRAM-ID. InCollege.

       ENVIRONMENT DIVISION.
       INPUT-OUTPUT SECTION.
       FILE-CONTROL.
           SELECT INPUT-FILE
               ASSIGN TO 'InCollege-Input.txt'
               ORGANIZATION IS LINE SEQUENTIAL
               FILE STATUS IS WS-FILE-STATUS.
           SELECT OUTPUT-FILE
               ASSIGN TO 'InCollege-Output.txt'
               ORGANIZATION IS LINE SEQUENTIAL
               FILE STATUS IS WS-OUTPUT-STATUS.
           SELECT ACCOUNTS-FILE
               ASSIGN TO 'Accounts.dat'
               ORGANIZATION IS LINE SEQUENTIAL
               FILE STATUS IS FS-ACCT.

       DATA DIVISION.
       FILE SECTION.
       FD  INPUT-FILE.
       01  InLine                         PIC X(200).

       FD  OUTPUT-FILE.
       01  OutLine                        PIC X(200).

       FD  ACCOUNTS-FILE.
       01  Account-Line                   PIC X(60).

       WORKING-STORAGE SECTION.
       *> ------- File status variables -------
       01  WS-FILE-STATUS                 PIC XX.
       01  WS-OUTPUT-STATUS               PIC XX.
       01  FS-ACCT                        PIC XX VALUE '00'.
       01  EOF-IN                         PIC X VALUE 'N'.
       01  ACCT-EOF                       PIC X VALUE 'N'.

       *> ------- Menu / input buffers -------
       01  UserChoice                     PIC 9.
       01  UserName                       PIC X(20).
       01  UserPassword                   PIC X(20).
       01  NameLen                        PIC 99.
       01  PassLen                        PIC 99.
       01  TempChar                       PIC X.
       01  I                              PIC 99.
       01  WS-MSG                         PIC X(200).

       *> ------- Main menu navigation -------
       01  WS-MENU-SELECTION              PIC X.
       01  WS-SKILL-SELECTION             PIC X.
       01  WS-INPUT-VALUE                 PIC X.
       01  WS-LOGGED-IN                   PIC X VALUE 'N'.
           88  USER-LOGGED-IN                   VALUE 'Y'.
           88  USER-NOT-LOGGED-IN               VALUE 'N'.

       *> ------- In-memory accounts (max 5) -------
       01  Account-Count                  PIC 9 VALUE 0.
       01  Accounts.
           05  Account OCCURS 5 TIMES.
               10  Acc-Username           PIC X(20).
               10  Acc-Password           PIC X(20).

       *> ------- Flags -------
       01  UE-Flag                        PIC 9 VALUE 0.
           88  Username-Exists                  VALUE 1.
           88  Username-Not-Exists              VALUE 0.

       01  VP-Valid-Flag                  PIC 9 VALUE 0.
           88  Pass-Is-Valid                    VALUE 1.
           88  Pass-Is-Invalid                  VALUE 0.
       01  Has-Upper                      PIC 9 VALUE 0.
       01  Has-Digit                      PIC 9 VALUE 0.
       01  Has-Special                    PIC 9 VALUE 0.
       01  CountVar                       PIC 99 VALUE 0.

       *> Special characters set (double the quote inside)
       01  Specials                       PIC X(40)
           VALUE '!@#$%^&*()-_=+[]{};:'',.<>/?'.

       *> Parse helpers
       01  U-Part                         PIC X(20).
       01  P-Part                         PIC X(20).

       PROCEDURE DIVISION.
       Main.
           PERFORM Open-Files
           PERFORM Load-Accounts-From-Disk

           *> Authentication Loop
           PERFORM UNTIL USER-LOGGED-IN
               PERFORM Show-Login-Menu
               PERFORM READ-NEXT-INPUT
               IF InLine = SPACES
                   MOVE '0' TO InLine(1:1)
               END-IF
               MOVE FUNCTION NUMVAL (InLine) TO UserChoice

               EVALUATE UserChoice
                   WHEN 1
                       PERFORM Do-Login
                   WHEN 2
                       PERFORM Do-Registration
                   WHEN OTHER
                       MOVE "Invalid choice. Please try again." TO WS-MSG
                       PERFORM OUT-MSG
               END-EVALUATE
           END-PERFORM

           *> Main Application Loop
           PERFORM UNTIL WS-MENU-SELECTION = "4" OR EOF-IN = "Y"
               PERFORM Show-Main-Menu
               PERFORM READ-NEXT-INPUT
               IF EOF-IN NOT = "Y"
                   MOVE WS-INPUT-VALUE TO WS-MENU-SELECTION
                   STRING "Selected: " WS-MENU-SELECTION 
                       DELIMITED BY SIZE INTO WS-MSG
                   PERFORM OUT-MSG
                   EVALUATE WS-MENU-SELECTION
                       WHEN "1"
                           MOVE "0" TO WS-MENU-SELECTION
                           PERFORM J-Search-Loop
                       WHEN "2"
                           MOVE "0" TO WS-MENU-SELECTION
                           PERFORM Find-Someone-Loop
                       WHEN "3"
                           MOVE "0" TO WS-MENU-SELECTION
                           PERFORM Skill-Loop
                       WHEN "4"
                           MOVE "Logging out... Goodbye!" TO WS-MSG
                           PERFORM OUT-MSG
                       WHEN OTHER
                           MOVE "0" TO WS-MENU-SELECTION
                           STRING "Invalid choice: " WS-MENU-SELECTION 
                               DELIMITED BY SIZE INTO WS-MSG
                           PERFORM OUT-MSG
                   END-EVALUATE
               END-IF
           END-PERFORM

           PERFORM Close-Files
           STOP RUN.

       *> -----------------------------
       *> File open/close + output + input
       *> -----------------------------
       Open-Files.
           OPEN INPUT  INPUT-FILE
           IF WS-FILE-STATUS NOT = "00"
               DISPLAY "Error opening input file: " WS-FILE-STATUS
               STOP RUN
           END-IF
           
           OPEN OUTPUT OUTPUT-FILE
           IF WS-OUTPUT-STATUS NOT = "00"
               DISPLAY "Error opening output file: " WS-OUTPUT-STATUS
               STOP RUN
           END-IF

           *> Try to open accounts for INPUT; if missing, create it.
           OPEN INPUT  ACCOUNTS-FILE
           IF FS-ACCT = "35"
               OPEN OUTPUT ACCOUNTS-FILE
               CLOSE ACCOUNTS-FILE
               OPEN INPUT ACCOUNTS-FILE
           END-IF
           CLOSE ACCOUNTS-FILE

           *> Reopen accounts in EXTEND for appends after startup load
           OPEN EXTEND ACCOUNTS-FILE
           .

       Close-Files.
           CLOSE INPUT-FILE
           CLOSE OUTPUT-FILE
           .

       *> Display to screen AND write to output file
       OUT-MSG.
           MOVE WS-MSG TO OutLine
           DISPLAY WS-MSG
           WRITE OutLine
           .

       *> Consume one line from input file per prompt
       READ-NEXT-INPUT.
           IF EOF-IN = 'Y'
               MOVE SPACES TO InLine
               EXIT PARAGRAPH
           END-IF
           READ INPUT-FILE
               AT END
                   MOVE 'Y' TO EOF-IN
                   MOVE "End of file reached" TO WS-MSG
                   PERFORM OUT-MSG
               NOT AT END
                   MOVE InLine(1:1) TO WS-INPUT-VALUE
           END-READ
           .

       *> -----------------------------
       *> LOGIN MENU
       *> -----------------------------
       Show-Login-Menu.
           MOVE "Welcome to InCollege!" TO WS-MSG
           PERFORM OUT-MSG
           MOVE "1. Log In" TO WS-MSG
           PERFORM OUT-MSG
           MOVE "2. Create New Account" TO WS-MSG
           PERFORM OUT-MSG
           MOVE "Enter your choice: " TO WS-MSG
           PERFORM OUT-MSG
           .

       *> -----------------------------
       *> LOGIN
       *> -----------------------------
       Do-Login.
           IF Account-Count = 0
               MOVE "No accounts exist yet. Please create an account first." TO WS-MSG
               PERFORM OUT-MSG
               GOBACK
           END-IF

           PERFORM WITH TEST AFTER UNTIL 1 = 0
               MOVE "Please enter your username: " TO WS-MSG
               PERFORM OUT-MSG
               PERFORM READ-NEXT-INPUT
               MOVE InLine TO UserName

               MOVE "Please enter your password: " TO WS-MSG
               PERFORM OUT-MSG
               PERFORM READ-NEXT-INPUT
               MOVE InLine TO UserPassword

               PERFORM Check-Credentials
               IF Pass-Is-Valid
                   MOVE "You have successfully logged in." TO WS-MSG
                   PERFORM OUT-MSG
                   SET USER-LOGGED-IN TO TRUE
                   EXIT PERFORM
               ELSE
                   MOVE "Incorrect username/password, please try again" TO WS-MSG
                   PERFORM OUT-MSG
               END-IF
           END-PERFORM
           .

       Check-Credentials.
           SET Pass-Is-Invalid TO TRUE
           PERFORM VARYING I FROM 1 BY 1 UNTIL I > Account-Count
               IF UserName = Acc-Username(I) AND
                  UserPassword = Acc-Password(I)
                   SET Pass-Is-Valid TO TRUE
                   EXIT PERFORM
               END-IF
           END-PERFORM
           .

       *> -----------------------------
       *> REGISTRATION
       *> -----------------------------
       Do-Registration.
           IF Account-Count >= 5
               MOVE "All permitted accounts have been created, please come back later" TO WS-MSG
               PERFORM OUT-MSG
               GOBACK
           END-IF

           SET Username-Exists TO TRUE

           *> --- Username step ---
           PERFORM UNTIL Username-Not-Exists
               MOVE "Create Account - Enter a username (max 20 chars, no spaces)." TO WS-MSG
               PERFORM OUT-MSG
               MOVE "Username: " TO WS-MSG
               PERFORM OUT-MSG

               PERFORM READ-NEXT-INPUT
               MOVE InLine TO UserName

               PERFORM Compute-Name-Length
               IF NameLen = 0
                   MOVE "Username cannot be empty." TO WS-MSG
                   PERFORM OUT-MSG
               ELSE
                   PERFORM Check-Username-Exists
                   IF Username-Exists
                       MOVE "That username is already taken. Try another." TO WS-MSG
                       PERFORM OUT-MSG
                   ELSE
                       SET Username-Not-Exists TO TRUE
                   END-IF
               END-IF
           END-PERFORM

           *> --- Password step ---
           MOVE "Password requirements:" TO WS-MSG
           PERFORM OUT-MSG
           MOVE "- 8 to 12 characters" TO WS-MSG
           PERFORM OUT-MSG
           MOVE "- At least one uppercase letter (A-Z)" TO WS-MSG
           PERFORM OUT-MSG
           MOVE "- At least one digit (0-9)" TO WS-MSG
           PERFORM OUT-MSG
           MOVE "- At least one special character (!@#$... etc.)" TO WS-MSG
           PERFORM OUT-MSG

           SET Pass-Is-Invalid TO TRUE
           PERFORM UNTIL Pass-Is-Valid
               MOVE "Please enter your password: " TO WS-MSG
               PERFORM OUT-MSG
               PERFORM READ-NEXT-INPUT
               MOVE InLine TO UserPassword

               PERFORM Validate-Password
               IF Pass-Is-Invalid
                   MOVE "Password does not meet requirements, please try again." TO WS-MSG
                   PERFORM OUT-MSG
               END-IF
           END-PERFORM

           *> --- Save in-memory ---
           ADD 1 TO Account-Count
           MOVE UserName     TO Acc-Username(Account-Count)
           MOVE UserPassword TO Acc-Password(Account-Count)

           *> --- Persist (already OPEN EXTEND) ---
           PERFORM Append-Account-To-Disk

           MOVE "Account created successfully." TO WS-MSG
           PERFORM OUT-MSG
           SET USER-LOGGED-IN TO TRUE
           .

       *> -----------------------------
       *> MAIN APPLICATION MENUS
       *> -----------------------------
       Show-Main-Menu.
           MOVE "===========================" TO WS-MSG
           PERFORM OUT-MSG
           MOVE " Student Manager Menu" TO WS-MSG
           PERFORM OUT-MSG
           MOVE " 1. Job/Internship Search" TO WS-MSG
           PERFORM OUT-MSG
           MOVE " 2. Find Someone You Know" TO WS-MSG
           PERFORM OUT-MSG
           MOVE " 3. Learn a New Skill" TO WS-MSG
           PERFORM OUT-MSG
           MOVE " 4. Exit" TO WS-MSG
           PERFORM OUT-MSG
           MOVE "===========================" TO WS-MSG
           PERFORM OUT-MSG
           MOVE "Reading choice from file..." TO WS-MSG
           PERFORM OUT-MSG
           .

       J-Search-Loop.
           PERFORM UNTIL WS-MENU-SELECTION = "1" OR EOF-IN = "Y"
               PERFORM J-Search-Menu
               PERFORM READ-NEXT-INPUT
               IF EOF-IN NOT = "Y"
                   MOVE WS-INPUT-VALUE TO WS-MENU-SELECTION
                   STRING "Selected: " WS-MENU-SELECTION 
                       DELIMITED BY SIZE INTO WS-MSG
                   PERFORM OUT-MSG
                   EVALUATE WS-MENU-SELECTION
                       WHEN "1"
                           MOVE "Returning..." TO WS-MSG
                           PERFORM OUT-MSG
                       WHEN OTHER 
                           MOVE "0" TO WS-MENU-SELECTION
                           STRING "Invalid choice: " WS-MENU-SELECTION 
                               DELIMITED BY SIZE INTO WS-MSG
                           PERFORM OUT-MSG
                   END-EVALUATE
               END-IF
           END-PERFORM
           .

       Find-Someone-Loop.
           PERFORM UNTIL WS-MENU-SELECTION = "1" OR EOF-IN = "Y"
               PERFORM Find-Someone-Menu
               PERFORM READ-NEXT-INPUT
               IF EOF-IN NOT = "Y"
                   MOVE WS-INPUT-VALUE TO WS-MENU-SELECTION
                   STRING "Selected: " WS-MENU-SELECTION 
                       DELIMITED BY SIZE INTO WS-MSG
                   PERFORM OUT-MSG
                   EVALUATE WS-MENU-SELECTION
                       WHEN "1"
                           MOVE "Returning..." TO WS-MSG
                           PERFORM OUT-MSG
                       WHEN OTHER 
                           MOVE "0" TO WS-MENU-SELECTION
                           STRING "Invalid choice: " WS-MENU-SELECTION 
                               DELIMITED BY SIZE INTO WS-MSG
                           PERFORM OUT-MSG
                   END-EVALUATE
               END-IF
           END-PERFORM
           .

       Skill-Loop.
           PERFORM UNTIL WS-MENU-SELECTION = "5" OR EOF-IN = "Y"
               PERFORM Skill-Menu
               PERFORM READ-NEXT-INPUT
               IF EOF-IN NOT = "Y"
                   MOVE WS-INPUT-VALUE TO WS-MENU-SELECTION
                   STRING "Selected: " WS-MENU-SELECTION 
                       DELIMITED BY SIZE INTO WS-MSG
                   PERFORM OUT-MSG
                   EVALUATE WS-MENU-SELECTION
                       WHEN "1"
                           MOVE "0" TO WS-MENU-SELECTION
                           PERFORM Web-Dev-Loop
                       WHEN "2" 
                           MOVE "0" TO WS-MENU-SELECTION
                           PERFORM Deep-Learning-Loop
                       WHEN "3" 
                           MOVE "0" TO WS-MENU-SELECTION
                           PERFORM Interview-Loop
                       WHEN "4"
                           MOVE "0" TO WS-MENU-SELECTION
                           PERFORM Resume-Loop
                       WHEN "5"
                           MOVE "Returning.." TO WS-MSG
                           PERFORM OUT-MSG
                       WHEN OTHER 
                           MOVE "0" TO WS-MENU-SELECTION
                           STRING "Invalid choice: " WS-MENU-SELECTION 
                               DELIMITED BY SIZE INTO WS-MSG
                           PERFORM OUT-MSG
                   END-EVALUATE
               END-IF
           END-PERFORM
           .

       Web-Dev-Loop.
           PERFORM UNTIL WS-MENU-SELECTION = "1" OR EOF-IN = "Y"
               PERFORM Web-Dev-Menu
               PERFORM READ-NEXT-INPUT
               IF EOF-IN NOT = "Y"
                   MOVE WS-INPUT-VALUE TO WS-MENU-SELECTION
                   STRING "Selected: " WS-MENU-SELECTION 
                       DELIMITED BY SIZE INTO WS-MSG
                   PERFORM OUT-MSG
                   EVALUATE WS-MENU-SELECTION
                       WHEN "1"
                           MOVE "Returning..." TO WS-MSG
                           PERFORM OUT-MSG
                       WHEN OTHER 
                           MOVE "0" TO WS-MENU-SELECTION
                           STRING "Invalid choice: " WS-MENU-SELECTION 
                               DELIMITED BY SIZE INTO WS-MSG
                           PERFORM OUT-MSG
                   END-EVALUATE
               END-IF
           END-PERFORM
           .

       Deep-Learning-Loop.
           PERFORM UNTIL WS-MENU-SELECTION = "1" OR EOF-IN = "Y"
               PERFORM Deep-Learning-Menu
               PERFORM READ-NEXT-INPUT
               IF EOF-IN NOT = "Y"
                   MOVE WS-INPUT-VALUE TO WS-MENU-SELECTION
                   STRING "Selected: " WS-MENU-SELECTION 
                       DELIMITED BY SIZE INTO WS-MSG
                   PERFORM OUT-MSG
                   EVALUATE WS-MENU-SELECTION
                       WHEN "1"
                           MOVE "Returning..." TO WS-MSG
                           PERFORM OUT-MSG
                       WHEN OTHER 
                           MOVE "0" TO WS-MENU-SELECTION
                           STRING "Invalid choice: " WS-MENU-SELECTION 
                               DELIMITED BY SIZE INTO WS-MSG
                           PERFORM OUT-MSG
                   END-EVALUATE
               END-IF
           END-PERFORM
           .

       Interview-Loop.
           PERFORM UNTIL WS-MENU-SELECTION = "1" OR EOF-IN = "Y"
               PERFORM Interview-Menu
               PERFORM READ-NEXT-INPUT
               IF EOF-IN NOT = "Y"
                   MOVE WS-INPUT-VALUE TO WS-MENU-SELECTION
                   STRING "Selected: " WS-MENU-SELECTION 
                       DELIMITED BY SIZE INTO WS-MSG
                   PERFORM OUT-MSG
                   EVALUATE WS-MENU-SELECTION
                       WHEN "1"
                           MOVE "Returning..." TO WS-MSG
                           PERFORM OUT-MSG
                       WHEN OTHER
                           MOVE "0" TO WS-MENU-SELECTION 
                           STRING "Invalid choice: " WS-MENU-SELECTION 
                               DELIMITED BY SIZE INTO WS-MSG
                           PERFORM OUT-MSG
                   END-EVALUATE
               END-IF
           END-PERFORM
           .

       Resume-Loop.
           PERFORM UNTIL WS-MENU-SELECTION = "1" OR EOF-IN = "Y"
               PERFORM Resume-Menu
               PERFORM READ-NEXT-INPUT
               IF EOF-IN NOT = "Y"
                   MOVE WS-INPUT-VALUE TO WS-MENU-SELECTION
                   STRING "Selected: " WS-MENU-SELECTION 
                       DELIMITED BY SIZE INTO WS-MSG
                   PERFORM OUT-MSG
                   EVALUATE WS-MENU-SELECTION
                       WHEN "1"
                           MOVE "Returning..." TO WS-MSG
                           PERFORM OUT-MSG
                       WHEN OTHER 
                           MOVE "0" TO WS-MENU-SELECTION
                           STRING "Invalid choice: " WS-MENU-SELECTION 
                               DELIMITED BY SIZE INTO WS-MSG
                           PERFORM OUT-MSG
                   END-EVALUATE
               END-IF
           END-PERFORM
           .

       *> -----------------------------
       *> MENU DISPLAY PARAGRAPHS
       *> -----------------------------
       Skill-Menu.
           MOVE "===========================" TO WS-MSG
           PERFORM OUT-MSG
           MOVE " Skill Menu" TO WS-MSG
           PERFORM OUT-MSG
           MOVE " 1. Learn Web Development" TO WS-MSG
           PERFORM OUT-MSG
           MOVE " 2. Learn Deep Learning" TO WS-MSG
           PERFORM OUT-MSG
           MOVE " 3. Learn How To Crack Interview Questions" TO WS-MSG
           PERFORM OUT-MSG
           MOVE " 4. Learn How To Optimize Your Resume" TO WS-MSG
           PERFORM OUT-MSG
           MOVE " 5. Return" TO WS-MSG
           PERFORM OUT-MSG
           MOVE "===========================" TO WS-MSG
           PERFORM OUT-MSG
           MOVE "Reading choice from file..." TO WS-MSG
           PERFORM OUT-MSG
           .

       J-Search-Menu.
           MOVE "===========================" TO WS-MSG
           PERFORM OUT-MSG
           MOVE "Job/Internship Search" TO WS-MSG
           PERFORM OUT-MSG
           MOVE "Under Construction" TO WS-MSG
           PERFORM OUT-MSG
           MOVE "1. Return" TO WS-MSG
           PERFORM OUT-MSG
           MOVE "===========================" TO WS-MSG
           PERFORM OUT-MSG
           MOVE "Reading choice from file..." TO WS-MSG
           PERFORM OUT-MSG
           .

       Find-Someone-Menu.
           MOVE "===========================" TO WS-MSG
           PERFORM OUT-MSG
           MOVE "Find Someone You Know" TO WS-MSG
           PERFORM OUT-MSG
           MOVE "Under Construction" TO WS-MSG
           PERFORM OUT-MSG
           MOVE "1. Return" TO WS-MSG
           PERFORM OUT-MSG
           MOVE "===========================" TO WS-MSG
           PERFORM OUT-MSG
           MOVE "Reading choice from file..." TO WS-MSG
           PERFORM OUT-MSG
           .

       Web-Dev-Menu.
           MOVE "===========================" TO WS-MSG
           PERFORM OUT-MSG
           MOVE "Learn Web Development" TO WS-MSG
           PERFORM OUT-MSG
           MOVE "Under Construction" TO WS-MSG
           PERFORM OUT-MSG
           MOVE "1. Return" TO WS-MSG
           PERFORM OUT-MSG
           MOVE "===========================" TO WS-MSG
           PERFORM OUT-MSG
           MOVE "Reading choice from file..." TO WS-MSG
           PERFORM OUT-MSG
           .

       Deep-Learning-Menu.
           MOVE "===========================" TO WS-MSG
           PERFORM OUT-MSG
           MOVE "Learn Deep Learning" TO WS-MSG
           PERFORM OUT-MSG
           MOVE "Under Construction..." TO WS-MSG
           PERFORM OUT-MSG
           MOVE "1. Return" TO WS-MSG
           PERFORM OUT-MSG
           MOVE "===========================" TO WS-MSG
           PERFORM OUT-MSG
           MOVE "Reading choice from file..." TO WS-MSG
           PERFORM OUT-MSG
           .

       Interview-Menu.
           MOVE "===========================" TO WS-MSG
           PERFORM OUT-MSG
           MOVE "Learn How To Crack Interview Questions" TO WS-MSG
           PERFORM OUT-MSG
           MOVE "Under Construction..." TO WS-MSG
           PERFORM OUT-MSG
           MOVE "1. Return" TO WS-MSG
           PERFORM OUT-MSG
           MOVE "===========================" TO WS-MSG
           PERFORM OUT-MSG
           MOVE "Reading choice from file..." TO WS-MSG
           PERFORM OUT-MSG
           .

       Resume-Menu.
           MOVE "===========================" TO WS-MSG
           PERFORM OUT-MSG
           MOVE "Learn How To Optimize Your Resume" TO WS-MSG
           PERFORM OUT-MSG
           MOVE "Under Construction..." TO WS-MSG
           PERFORM OUT-MSG
           MOVE "1. Return" TO WS-MSG
           PERFORM OUT-MSG
           MOVE "===========================" TO WS-MSG
           PERFORM OUT-MSG
           MOVE "Reading choice from file..." TO WS-MSG
           PERFORM OUT-MSG
           .

       *> -----------------------------
       *> Helper: compute username length (stop at first space)
       *> -----------------------------
       Compute-Name-Length.
           MOVE 0 TO NameLen
           PERFORM VARYING I FROM 1 BY 1 UNTIL I > 20
               MOVE UserName(I:1) TO TempChar
               IF TempChar = SPACE
                   EXIT PERFORM
               ELSE
                   ADD 1 TO NameLen
               END-IF
           END-PERFORM
           .

       *> -----------------------------
       *> Helper: check username uniqueness
       *> -----------------------------
       Check-Username-Exists.
           SET Username-Not-Exists TO TRUE
           SET UE-Flag            TO 0
           IF Account-Count > 0
               PERFORM VARYING I FROM 1 BY 1 UNTIL I > Account-Count
                   IF UserName = Acc-Username(I)
                       SET Username-Exists TO TRUE
                       EXIT PERFORM
                   END-IF
               END-PERFORM
           END-IF
           .

       *> -----------------------------
       *> Helper: password validation
       *> -----------------------------
       Validate-Password.
           SET Pass-Is-Invalid TO TRUE
           MOVE 0 TO Has-Upper Has-Digit Has-Special
           MOVE 0 TO PassLen

           *> length check (stop at first space)
           PERFORM VARYING I FROM 1 BY 1 UNTIL I > 20
               MOVE UserPassword(I:1) TO TempChar
               IF TempChar = SPACE
                   EXIT PERFORM
               ELSE
                   ADD 1 TO PassLen
               END-IF
           END-PERFORM

           IF PassLen < 8 OR PassLen > 12
               EXIT PARAGRAPH
           END-IF

           *> character checks
           PERFORM VARYING I FROM 1 BY 1 UNTIL I > PassLen
               MOVE UserPassword(I:1) TO TempChar
               IF TempChar >= "A" AND TempChar <= "Z"
                   MOVE 1 TO Has-Upper
               END-IF
               IF TempChar >= "0" AND TempChar <= "9"
                   MOVE 1 TO Has-Digit
               END-IF
               MOVE 0 TO CountVar
               INSPECT Specials TALLYING CountVar FOR ALL TempChar
               IF CountVar > 0
                   MOVE 1 TO Has-Special
               END-IF
           END-PERFORM

           IF Has-Upper = 1 AND Has-Digit = 1 AND Has-Special = 1
               SET Pass-Is-Valid TO TRUE
           END-IF
           .

       *> -----------------------------
       *> Load accounts from disk at startup (no GOTO)
       *> -----------------------------
       Load-Accounts-From-Disk.
           CLOSE ACCOUNTS-FILE
           OPEN INPUT ACCOUNTS-FILE
           MOVE 'N' TO ACCT-EOF

           PERFORM UNTIL ACCT-EOF = 'Y'
               READ ACCOUNTS-FILE
                   AT END
                       MOVE 'Y' TO ACCT-EOF
                   NOT AT END
                       MOVE SPACES TO U-Part P-Part
                       UNSTRING Account-Line DELIMITED BY '|'
                           INTO U-Part, P-Part
                       END-UNSTRING
                       IF U-Part NOT = SPACES AND P-Part NOT = SPACES
                           IF Account-Count < 5
                               ADD 1 TO Account-Count
                               MOVE U-Part TO Acc-Username(Account-Count)
                               MOVE P-Part TO Acc-Password(Account-Count)
                           END-IF
                       END-IF
               END-READ
           END-PERFORM

           CLOSE ACCOUNTS-FILE
           OPEN EXTEND ACCOUNTS-FILE
           .

       *> -----------------------------
       *> Append account to file
       *> -----------------------------
       Append-Account-To-Disk.
           STRING
               UserName     DELIMITED BY SIZE
               "|"          DELIMITED BY SIZE
               UserPassword DELIMITED BY SIZE
               INTO Account-Line
           END-STRING
           WRITE Account-Line
           .
           