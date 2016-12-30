--Name: Dewey Milton
--Date: 17 October 2016
--Description: To find Palindromes, Anti-Palindromes, or none at all

WITH Ada.Text_IO;          USE Ada.Text_IO;

WITH Ada.Integer_Text_IO;  USE Ada.Integer_Text_IO;

WITH Ada.Strings.Fixed;    USE Ada.Strings.Fixed;

WITH Ada.Characters.Handling;  USE Ada.Characters.Handling;

WITH Ada.Exceptions;       USE Ada.Exceptions;


PROCEDURE Antipals IS

   --Declare program exceptions

   --Raised by problem with a keyword
   Invalid_Keyword : EXCEPTION;

   --Raised if a problem with a character
   Invalid_Character : EXCEPTION;

   --Raised if a problem with a code
   Invalid_Code : EXCEPTION;

   --Raised if a problem with data format
   Invalid_Data : EXCEPTION;

   --Raised if without Quit or not only Quit
   Invalid_Quit : EXCEPTION;

   -- Declare program types

   --Number of expected code range
   SUBTYPE NumCodes IS Natural RANGE 0..100;

   --Number of expected word range
   SUBTYPE NumWords IS Natural Range 0..100;

   --A type for Code in expected range
   SUBTYPE CodeType IS String(1..28);

   --A type for Word in expected range
   SUBTYPE WordType IS String(1..50);

   --Array type for words
   TYPE WordArray IS ARRAY (1..100) OF WordType;

   --Array type for word Lengths
   TYPE WrdLength IS ARRAY (1..100) OF Natural;

   --Type for Keyword input
   TYPE Input IS (Code, Word, Quit);

   --In order to access Input in Put, Get Commands
   PACKAGE Input_IO IS NEW Ada.Text_IO.Enumeration_IO(Input);
   USE Input_IO;

   --A type for the status of a word
   TYPE StatType IS (Pal, Anti, NoPal);

   --Allows the use of StatType in Put and Get
   PACKAGE Stat_IO IS NEW Ada.Text_IO.Enumeration_IO(StatType);
   USE Stat_IO;

   -- Creates a record to hold information of each data set

   TYPE CodeWords IS RECORD

   -- contains each code
      CodeIs : CodeType;

   -- contains leangth of a code
      Clength : Natural := 0;

   -- contains array of words
      Words : WordArray;

   -- contains array of each words length
      Wlength : WrdLength;

   -- contains array of modified word lengths
      NewLength : WrdLength;

   -- contains the number of words in an array
      WCount : NumWords := 0;

   END RECORD;

   -- an array to hold records of each data set
   TYPE RecArray IS ARRAY (1..50) OF CodeWords;

   -- a record that stores the arrays of records
   -- codeCount stores number of codes
   -- runTime will store how many times a code is found
   TYPE Cabinet IS RECORD
      RArray : RecArray;
      CodeCount : NumCodes := 0;
      RunTime : Integer := 0;
   END RECORD;

        ----------------------------------------------------------
        -- Purpose: Checks if a code has valid inputs
        -- Parameters: CodeType : Code to be validated
        -- Precondition: Validation unknown
        -- Postcondition: Validation known
        ----------------------------------------------------------
PROCEDURE CheckCode(TmpCode : IN CodeType; length : IN Integer) IS

   Unique : Boolean := True; -- Becomes false if letter out of order

   Count : Integer := 0; -- counts number of quotations

   Index : Integer; -- Index through unique search

BEGIN


   -- checks for special characters in code and number of quotations
   FOR Pos IN 1 .. length  LOOP

      IF IS_Letter(TmpCode(Pos)) THEN

         Index := Pos + 1;

         While Index < Length AND Unique LOOP

            IF TmpCode(Pos) = TmpCode(Index) THEN

               Unique := False;

            END IF;

            Index := Index + 1;

         END LOOP;

     END IF;

      IF not Unique THEN
            RAISE Invalid_Character WITH "Code must have Unique letters";
          END IF;

      IF TmpCode(Pos) = '"' THEN

         Count := Count + 1;

      ELSIF NOT IS_LETTER(TmpCode(Pos)) THEN

         RAISE Invalid_Character WITH "Code must only have letters";

      END IF;

   END LOOP;


   -- validates quotation amount
   IF Count < 2 THEN

      RAISE Invalid_Data WITH "Does not have quotations!";

   END IF;

END CheckCode;

        ----------------------------------------------------------
        -- Purpose: Checks if a word has valid inputs
        -- Parameters: ALists : Record with code to check,
        -- TmpWord : Word to be checked
        -- Precondition: Validation unknown
        -- Postcondition: Validation known
        ----------------------------------------------------------
FUNCTION CheckWord(ALists : IN Cabinet; TmpWord : IN WordType;
                           Length : IN Integer) RETURN Boolean IS

   --Presumed valid until found false.
   Valid : Boolean := True;
BEGIN

   IF TmpWord(1) /= '"' THEN
      Put(TmpWord(1)); New_Line; Put("First");
      Valid := False;
   ELSIF TmpWord(Length) /= '"' THEN
      Put(TmpWord(TmpWord'Last)); New_Line;
      Valid := False; Put("2nd");
   END IF;

   FOR Pos IN 2 .. Length LOOP

      DECLARE
         --Becomes true if character is in code
         Found : Boolean := False;
         Index : Integer := 0; --Cycles through code
      BEGIN

       IF Is_Letter(TmpWord(Pos)) THEN
            WHILE NOT Found AND Index < Alists.RArray(Alists.CodeCount)
                                                        .CodeIs'Length LOOP

              IF TO_Lower(TmpWord(Pos)) = Alists.RArray(Alists.CodeCount)
                                                           .CodeIs(Index) THEN

               Found := True;

            ELSE
               Index := Index + 1;

            END IF;

         END LOOP;

         IF NOT Found THEN
               Valid := False;

               RAISE Invalid_Character WITH "Invalid Character in Word."
                                 & TmpWord(Pos) & " Not present in Code!";

         END IF;

       END IF;

      END;

   END LOOP;

   RETURN Valid;

END CheckWord;

        ----------------------------------------------------------
        -- Purpose: Takes a NewString and stores in the associated array
        -- Parameters: Cabinet, Input, String:
        -- NewString is stored into ALists of the correct InputType
        -- Done notifies Process Procedure to begin analysis
        -- Precondition: ALists arrays are empty
        -- Postcondition: ALists arrays filled with codes and words
        ----------------------------------------------------------
PROCEDURE Fill_Arrays(ALists : IN OUT Cabinet; InputType : IN Input;
                       NewString : IN String; Done : IN OUT Boolean) IS

   TmpCode : CodeType; -- Stores code till tested
   TmpWord : WordType; -- Stores word till tested

   -- The new strings first index
   First : CONSTANT Integer := NewString'First;

   -- The new strings size
   Size : Constant Natural := NewString'Length;

BEGIN

    --checks to see if word is before code
    IF InputType = Word AND ALists.CodeCount = 0 THEN
            RAISE Invalid_Keyword with "Word before Code!";

   ELSE

    --places string in the right array and tests validation
    CASE InputType IS

      WHEN Code =>

         IF Size > 28 THEN
            RAISE Invalid_Code WITH "Code To Long!";

         ELSIF Size < 4 THEN
            RAISE Invalid_Code WITH "Code To Short!";

         ELSE

         TmpCode(first..size) := NewString(First..size);

         ALists.CodeCount := Alists.CodeCount + 1;

         CheckCode(TmpCode, size);

         ALists.RArray(Alists.CodeCount).CodeIs(First..Size) := TmpCode;

         ALists.RArray(Alists.CodeCount).Clength := Size;

         END IF;

      WHEN Word =>

        TmpWord(First..Size) := NewString(First..Size);

            DECLARE
               Validated : Boolean; -- Becomes true if word is valid

            BEGIN
               Validated := CheckWord(ALists, TmpWord, Size);

               IF Validated THEN

               ALists.RArray(Alists.CodeCount).WCount :=
               ALists.RArray(Alists.CodeCount).WCount + 1;

           ALists.RArray(Alists.CodeCount).Wlength
           (ALists.RArray(Alists.CodeCount).WCount) := size;

            ALists.RArray(Alists.CodeCount).Words
            (ALists.RArray(Alists.CodeCount).WCount)
             (First..Size) := NewString(First..Size);


               ELSE

                  Done := True;

               END IF;

            END;


         WHEN Quit =>

            -- Just to Stop warning
            Done := True;

      END CASE;

   END IF;

END Fill_Arrays;


        ----------------------------------------------------------
        -- Purpose: Sets an array of modified word lengths
        -- Parameters: Cabinet to store, Pos of record being worked on,
        -- CurW array position of word associated to modified word,
        -- Output of the new modified word
        --
        -- Precondition: NewLength Array position empty
        -- Postcondition: NewLength array position has modified word length
        ----------------------------------------------------------
PROCEDURE NewLength(Codes : IN OUT Cabinet; Output : IN OUT String;
                               Pos : IN Integer; CurW : IN Integer) IS

   Found : Boolean := False; -- becomes true if a space is found

  --length of current word being checked
   Length : CONSTANT Integer := Codes.RArray(Pos).Wlength(CurW);

BEGIN

   IF Length > 3 THEN

      FOR Itr IN 1 .. Length LOOP

         IF Output(Itr) = ' ' AND NOT Found THEN

            Found := True;

            Output(Itr) := '"'; --places new ending quotation

            --sets the length of the new word
            Codes.RArray(Pos).Newlength(CurW) := Itr;

         END IF;

      END LOOP;

   END IF;

   IF NOT Found THEN

      --if not found sets old length as new length
      Codes.RArray(Pos).Newlength(CurW) := Codes.RArray(Pos).Wlength(CurW);

   END IF;

END NewLength;


        ----------------------------------------------------------
        -- Purpose: Reduces a string by taking out spaces
        -- Parameters: Codes of stored arrays, Output string being worked on,
        -- Pos of Code record, CurW current word array position
        --
        -- Precondition: Output may have spaces
        -- Postcondition: Output no longer contains spaces
        ----------------------------------------------------------
PROCEDURE XSpaces(Codes : IN Cabinet; Output : IN OUT String;
                         Pos : IN Integer; CurW : IN Integer;
                                      numSpaces : IN Integer) IS

   --Length of word being worked on
   Length : CONSTANT Integer := Codes.RArray(Pos).Wlength(CurW);

   -- becomes true if a word contains spaces
   Found : Boolean := false;

BEGIN

   IF Length >= 3 THEN

   FOR Itr IN 1 .. numSpaces LOOP

     FOR Index IN 2 .. Length LOOP

       IF Output(Index + 1) /= '"' THEN

               IF NOT Found AND Output(Index) = ' ' THEN

                  Found := True;

               IF Index < Length THEN

               Output(Index) := Output(Index +1);

               Output(Index +1) := ' ';

               END IF;

         ELSIF Found AND Output(Index) = ' ' AND Index < Length  THEN

               Output(Index) := Output(Index +1);

                  Output(Index +1) := ' ';

         END IF;

       END IF;

     END LOOP;

         Found := False;

      END LOOP;

   END IF;

END XSpaces;


        ----------------------------------------------------------
        -- Purpose: Controls procedures to Cleans out words of
        -- special characters and spaces
        --
        -- Parameters: Codes contains array of words, Output is the
        -- word being modified, Pos position of code array,
        -- CurW position of current word
        --
        -- Precondition: Output is in original form
        -- Postcondition: Output is lowercase letters no spaces
        ----------------------------------------------------------
PROCEDURE CleanUp(Codes : IN OUT Cabinet; Output : IN OUT WordType;
                               Pos : IN Integer; CurW : IN Integer) IS

   -- will contain length of current word
   Length : Integer;

   --counts number of spaces
   numSpaces : Integer;

BEGIN

   Length := Codes.RArray(Pos).Wlength(CurW);

   --makes letters lowercase and removes special characters
   FOR Index IN 2 .. Length LOOP

      IF Is_Letter(Output(Index)) THEN

         Output(Index) := To_Lower(Output(Index));

      ELSIF Output(Index) = ' ' THEN

         numSpaces := numSpaces + 1;

      ELSIF Output(Index) = '"' THEN

         Output(Index) := '"';

      ELSIF Is_Special(Output(Index)) THEN

         Output(Index) := ' ';

            numSpaces := numSpaces + 1;

      END IF;

   END LOOP;

   XSpaces(Codes, Output, Pos, CurW, numSpaces);

   NewLength(Codes, Output, Pos, CurW);


END CleanUp;


        ----------------------------------------------------------
        -- Purpose: Finds the location of a letter in the Code
        -- Parameters: Key is the code, Output is character being looked for.
        -- Precondition: Position unknown
        -- Postcondition: Position known
        ----------------------------------------------------------
FUNCTION FindLoc(Key : IN CodeType; Output : IN Character) RETURN Integer IS

   --becomes true if character is found
   Found : Boolean := False;

   --index of pointer through a code
   KIndex : Integer := 1;

BEGIN



   WHILE NOT Found LOOP

      IF Output = Key(Kindex) THEN
         Found := True;

      ELSE
         KIndex := KIndex + 1;

      END IF;

    END LOOP;

   RETURN KIndex;

END FindLoc;


        ----------------------------------------------------------
        -- Purpose: Shifts each letter to the associated key location
        -- Parameters: Codes: hold Code and Word arrays,
        -- Output: Word being shifted, ShiftedWord : New Output
        -- NumShift : Holds shift value, Pos and CurW Array Positions
        --
        -- Precondition:  ShiftedWord unknown
        -- Postcondition: ShiftedWord known and returned
        ----------------------------------------------------------
PROCEDURE Shift(Codes : IN Cabinet; Output : IN WordType;
          ShiftedWord : IN OUT WordType; NumShift : IN OUT Integer;
          Pos : IN Integer; CurW : IN Integer) IS

   Half : Integer; -- Holds the half mark of a word

   Key : CodeType; -- Will contain current code

   Wlength : Integer; -- Length of the Word

   KIndex : Integer; -- Position through a code

   WIndex : Integer; -- Position through a word

 --Becomes true if last character matches letter in the code
   FoundLoc : Boolean := False;

BEGIN

   Half := ((Codes.RArray(Pos).NewLength(CurW) - 2) / 2) + 2;

   Key(1..Codes.RArray(Pos).CLength - 1) :=
   Codes.RArray(Pos).CodeIs(2..Codes.Rarray(Pos).Clength - 1);

   NumShift := 0;

   KIndex := 1;

   WIndex := Codes.RArray(Pos).NewLength(CurW) - 1;

   WLength := Codes.RArray(Pos).NewLength(CurW) - 1;

   --will be the antipalindrome if one
   ShiftedWord(2 .. Wlength) := Output(2 .. Wlength);

   --cycles through the key till character found
   --tracks the number of shifts needed to do so
   --could be a procedure.. But you need to stop somewhere
   WHILE not FoundLoc LOOP


      IF Output(WIndex) = Key(KIndex) THEN

         WHILE NOT FoundLoc LOOP
            IF OutPut(2) = Key(KIndex) THEN
            FoundLoc := True;

            ELSIF Key(KIndex) = '"' THEN
               KIndex := 1;

            ELSE
            KIndex := KIndex + 1;
               NumShift := NumShift + 1;

            END IF;

         END LOOP;

       ELSIF Key(KIndex) = '"' THEN
         KIndex := 1;

       ELSE
         KIndex := KIndex + 1;

      END IF;

   END LOOP;

   --starts half way in a word and shifts letters down by numShift
   FOR Letter IN Half .. WLength LOOP

      KIndex := FindLoc(Key, ShiftedWord(Letter));

      DECLARE
         --Iterator to move through the code
         Itr : Integer := 0;

      BEGIN

         WHILE Itr < NumShift LOOP

            Itr := Itr + 1;
            KIndex := KIndex + 1;

          --If it reaches the end, position to the front
            IF Key(KIndex) = '"'  THEN
               KIndex := 1;


             END IF;

      END LOOP;
         --changes to desired character
         ShiftedWord(Letter) := Key(KIndex);

      END;

   END LOOP;

   --places quotations to the new word
   ShiftedWord(1) := '"';
   ShiftedWord(WLength + 1) := '"';

END Shift;



        ----------------------------------------------------------
        -- Purpose: Returns status of Palindrome or not
        -- Parameters: CodeType : Code to be validated,
        -- Output : String, Pos : Position in the Codes Array,
        -- CurW : Position of current word in array.
        --
        -- Precondition: Existance of a palindrome unknown
        -- Postcondition: Existance of a palindrome known
        ----------------------------------------------------------

FUNCTION Check4Pal(Codes : IN Cabinet; Output : IN WordType;
        Pos : IN Integer; CurW : IN Integer) RETURN StatType IS

   --Assumes palindrome until proven wrong
   IsPal : Boolean := True;

   -- holds the length of the word being checked
   Length : Integer;

   --Will hold values for the front and back position
   --of the word to be checked
   Front, Back : Integer;

   --value to know the half position
   ChkLength : Integer;

   --Holds the palindrome status of a word
   Status : StatType;

BEGIN


   Length := Codes.RArray(Pos).NewLength(CurW);

   Back := Length - 1;

   Front := 2;

   ChkLength := (Codes.RArray(Pos).NewLength(CurW) - 2) / 2;

   --only checks words of two or more characters
   IF ChkLength >= 1 THEN

      WHILE Front < Back AND IsPal LOOP

         IF Output(Front) /= Output(Back) THEN
            IsPal := False;

         ELSE
            Front := Front + 1;
            Back := Back - 1;

         END IF;

      END LOOP;

         IF IsPal THEN
            Status := Pal;

         ELSE
            Status := NoPal;

         END IF;

  ELSE
      Status := Pal;

  END IF;

  RETURN Status;

END Check4Pal;

        ----------------------------------------------------------
        -- Purpose: Checks if a word contains anti-palindrome
        -- Parameters: Codes : Contains word and code Array,
        -- Output : Modified word, ShiftedWord : New word placeholder
        -- Pos : Position for Array of records, CurW : Position in word array
        -- Status : Status of a palindrome, NumShift : How many shifts it will take
        -- Precondition: Antipalindrome existance unknown
        -- Postcondition: Antipalindrome existance known
        ----------------------------------------------------------
PROCEDURE Chck4AntiPal(Codes : IN Cabinet; OutPut : IN WordType;
                ShiftedWord : IN OUT WordType; Pos : IN Integer;
                    CurW : IN Integer; Status : IN OUT StatType;
                                      NumShift : IN OUT Integer) IS


BEGIN


   Shift(Codes, Output, ShiftedWord, NumShift, Pos, CurW);
   Status := Check4Pal(Codes, ShiftedWord, Pos, CurW);

   --Sets to antipalindrome if one is found
   IF Status = Pal THEN
      Status := Anti;
   END IF;



END Chck4AntiPal;


        ----------------------------------------------------------
        -- Purpose: Processes the arrays to find polidromes or antipolidromes
        -- Parameters: Codes : Has the arrays of codes and words,
        -- Pos : Position of Code array, CurW : Position of current word,
        -- Output : Word to process for analysis, NumShift : Will contain shift amount
        --
        -- Precondition: Original word unprocessed and polidrome existance unknown
        -- Postcondition: Original word modified and polidrome existance known
        ----------------------------------------------------------
PROCEDURE Process(Codes : IN OUT Cabinet; Pos : IN Integer;
               CurW : IN Integer; Output : IN OUT WordType;
       NumShift : IN OUT Integer; Status : IN OUT StatType;
                             ShiftedWord : IN OUT WordType) IS

BEGIN


   CleanUp(Codes, OutPut, Pos, CurW);

   Status := Check4Pal(Codes, Output, Pos, CurW);

   --if not found then it looks for antipalindrome
   IF Status = NoPal THEN
      Chck4AntiPal(Codes, OutPut, ShiftedWord, Pos, CurW, Status, NumShift);
  END IF;



END Process;

        ----------------------------------------------------------
        -- Purpose: Prints the outcome of each code and word
        -- Parameters: Codes : Holds the arrays, Pos : Position of code
        -- CurWord : Position of current word, ShiftedWord : anti-polidrome
        -- NumShift : Amount of shift, Status : Result of the word
        --
        -- Postcondition: Printed results
        ----------------------------------------------------------
PROCEDURE Print(Codes : IN Cabinet; Pos : IN Integer; CurWord : IN Integer;
    ShiftedWord : IN WordType; NumShift : IN Integer; Status : IN StatType) IS

   --length of the original word
   WLength : CONSTANT Integer := Codes.RArray(Pos).Wlength(CurWord);

   --length of the antipalindrome
   NLength : Constant Integer := Codes.RArray(Pos).Newlength(CurWord);
BEGIN


   Put("    ");
   Put("Word: ");
   Put(Codes.RArray(Pos).Words(CurWord)(1..WLength));
   New_Line;

   Put("        ");
   Put("Status : ");

   CASE Status IS

      WHEN Pal =>
         Put("Palindrome");

      WHEN Anti =>
         Put("Anti-palindrome");
         New_Line;
         Put("        ");
         Put("Shift amount: ");
         Put(NumShift, 2);
         New_Line;
         Put("        ");
         Put("Shifted Word: ");
         Put(ShiftedWord(1..NLength));

      WHEN NoPal =>
         Put("Not a palindrome or anti-palindrome");

   END CASE;


END Print;


        ----------------------------------------------------------
        -- Purpose: Controls the flow of processing and outputing polidrome results
        -- Parameters: Codes : Holds arrays for codes and words
        -- Precondition: Gathered codes and words
        -- Postcondition: Words are processed and results printed
        ----------------------------------------------------------
PROCEDURE FindAndOutput(Codes : IN OUT Cabinet) IS

  --used to label a palindrome
   Status : StatType;

  --used to count amount of shifts
   NumShift : Integer;

   --will contain a antipalindrome
   ShiftedWord : WordType;

   --code length
   CLength : Integer;
BEGIN


   --cycles through each code to process all the words
   FOR Pos IN 1 .. Codes.CodeCount LOOP

      CLength := Codes.RArray(Pos).Clength;
      New_Line;
      New_Line;
      Put("Code: ");
      Put(Codes.RArray(Pos).CodeIS(1..Clength));

      --cycles through each word in a code
      FOR CurWord IN 1 .. Codes.RArray(Pos).WCount LOOP

         DECLARE

            OutPut : String(1..Codes.RArray(Pos).Wlength(CurWord));

         BEGIN

            OutPut := Codes.RArray(Pos).Words(CurWord)
                (1..Codes.RArray(Pos).WLength(CurWord));

          Process(Codes, Pos, CurWord, OutPut, NumShift, Status, ShiftedWord);

          New_Line;

          Print(Codes, Pos, CurWord, ShiftedWord, NumShift, Status);

         END;

      END LOOP;

   END LOOP;

END FindAndOutput;


        ----------------------------------------------------------
        -- Purpose: used to read in standard input and then send to make arrays
        -- Parameters: Codes : Holds arrays for codes and words
        -- Precondition: Reads standard input
        -- Postcondition: Finds keyword quit
        ----------------------------------------------------------
PROCEDURE Get_Codes(Codes : IN OUT Cabinet) IS

   --stores a keyword to save correct data type
   InputType : Input;

   --True when keyword quit is submitted
   --begins palindrome search
   Done : Boolean := false;

BEGIN


   While not Done loop

      Get(InputType);


      IF InputType /= Quit THEN


      DECLARE
         --Takes in String
         InputString : CONSTANT String := Get_Line;

         --Gets length of Trimmed string
            InputLength : CONSTANT Integer :=
               (Ada.Strings.Fixed.Trim
               (InputString, Ada.Strings.Both)'Length) - 2;

            --Creates String of length
            NewString : String(1 .. InputLength);

      BEGIN

            --Stores String to send
         NewString := Ada.Strings.Fixed.Trim (InputString, Ada.Strings.Both)
         (3..InputString'length);

      Fill_Arrays(Codes, InputType, NewString, Done);

      END;

            IF End_Of_File AND NOT Done THEN
              Done := True;

              RAISE Invalid_Quit WITH "Program without Quit command!";

            END IF;

      ELSE
         Done := True;

       IF not End_Of_File THEN

               RAISE Invalid_Quit WITH "Quit Followed by other input";

          END IF;

      END IF;





END LOOP;

END Get_Codes;

   -- Stores an array of records which contains each code
   -- and corresponding words.
   Codes : Cabinet;

        ----------------------------------------------------------
        -- Purpose: Main
        ----------------------------------------------------------
BEGIN

   Get_Codes(Codes);

   FindAndOutput(Codes);

EXCEPTION
   WHEN E: Invalid_Keyword =>
      Put(Exception_Name(E));
      Put(". Entered Wrong Keyword  ");
      Put(Exception_Message(E));

   WHEN E: Invalid_Character =>
      Put(Exception_Name(E));
      Put(". Entered Wrong Character  ");
      Put(Exception_Message(E));

   WHEN E: Invalid_Code =>
      Put(Exception_Name(E));
      Put(". Invalid Code Type  ");
      Put(Exception_Message(E));

   WHEN E: Invalid_Data =>
      Put(Exception_Name(E));
      Put(". Invalid Input  ");
      Put(Exception_Message(E));

   WHEN E: Invalid_Quit =>
      Put(Exception_Name(E));
      Put(". Invalid Quit  ");
      Put(Exception_Message(E));

   WHEN E: OTHERS =>
      put("Invalid Data  ");
      put(exception_name(e));
      put(exception_message(e));



END Antipals;
