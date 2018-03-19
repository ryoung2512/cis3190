--  Ada : solveJumble
-- author: Rhys Young
-- student id: 0925398
-- description: takes input from user and finds the anagrams for the inputs

with Ada.Text_IO; use Ada.Text_IO;
with Ada.Integer_Text_IO; use Ada.Integer_Text_IO;
with Ada.Strings.Unbounded; use Ada.Strings.Unbounded;
with Ada.Strings.Unbounded.Text_IO; use Ada.Strings.Unbounded.Text_IO;

-- solveJumble This is a program that takes input from a user, asks them the number
-- of words they are going to enter. Then it finds anagrams for each word entered.
-- Searches up the words in the dictionary and then returns the ones that are actual words.
procedure solvejumble is
    -- Lets have a type of variable which is an array of strings
    type strArr is array(positive range <>) of Unbounded_String;

    -- countOccurences
    -- Count the number of occurences of the permutation, so that we do not add more.
    function countOccurences(s : in Unbounded_String; anagrams : in strArr ; index : in integer) return Boolean is
    begin
        for i in 1..index loop
            -- if the anagram already exists in our list of agram then return true
            if to_string(anagrams(i)) = to_string(s) then
                return true;
            end if;
        end loop;
        return false;
    end;

    -- permutation
    -- Generates all permutations of a word and stores it in a string array.
    procedure permutation(str : in out Unbounded_String ; left : in integer ; right: in integer ; anagrams : in out strArr ; index : in out integer) is
        temp : character;
        tempStr : String(1..right); -- a temp string to store the unbounded string
    begin
        if left = right and countOccurences(str, anagrams, index) = false then -- if we are done swapping and word isnt added, add it
            anagrams(index) := str; -- store the word
            index := index + 1;
        else
            for i in left..right loop
                -- switch unbounded string to a regular string so we can access it like an array
                tempStr := to_string(str);

                -- Swap elements
                temp := tempStr(left);
                tempStr(left) := tempStr(i);
                tempStr(i) := temp;

                -- switch back to an unbounded string so it can be passed back into the function
                str := to_unbounded_string(tempStr);

                -- recursion
                permutation(str, left + 1, right, anagrams, index);

                -- switch unbounded string to a regular string so we can access it like an array
                tempStr := to_string(str);

                -- Swap elements back to position
                temp := tempStr(left);
                tempStr(left) := tempStr(i);
                tempStr(i) := temp;

                -- switch back to an unbounded string so it can be passed back into the function
                str := to_unbounded_string(tempStr);
            end loop;
        end if;
    end;

    -- generateAnagram
    -- Generates all the anagrams of a word. Basically all it does is call permutation. And is a wrapper
    -- for the string array of anagrams.
    function generateAnagram(n : in integer ; word : in Unbounded_String; index : in out integer) return strArr is
       anas : strArr(1..1000);
       s : Unbounded_String;
    begin
         s := word;
         -- call the permutation procedure
         permutation(s, 1, to_string(s)'Length, anas, index);
         return anas;
    end generateAnagram;

    -- buildLEXICON
    -- Builds the lexicon, by opening the dictionary file and storing the file data in an array.
    function buildLEXICON return strArr is
        i :  integer;
        infp : file_type;
        s : Unbounded_string;
        lexi : strArr(1..51244); -- edit the number of lines in the file here
    begin
        i := 1;
        open(infp, in_file, "/usr/share/dict/canadian-english-small"); -- edit the file here as to where the dictionary is
        loop
        -- keep looping until end of file
            exit when end_of_file(infp);
            get_line(infp, s); -- get the line
            lexi(i) := s; -- store it
            i := i  + 1;
        end loop;
        close(infp); -- close the file
        return lexi;
    end buildLEXICON;


    -- findAnagram
    -- Finds all the anagrams that are actually words, by comparing the anagrams to the words in the dictionary
    -- if it finds a match it prints it out.
    procedure findAnagram(anagrams: in strArr ; index: in integer ; lex: in strArr) is
    begin
        for i in 1..index loop -- for each anagram
            for j in 1..51244 loop -- edit the number of lines in the file here
                -- if there is a match print it out
                if anagrams(i) = lex(j) then
                    put_line(anagrams(i)); -- print it out
                end if;
            end loop;
        end loop;
    end findAnagram;

    -- inputJumble
    -- The main input jumble program. It takes in the input from the user and calls the
    -- appropriate functions.
    procedure inputJumble is
        word : Unbounded_String;
        n, index : integer;
        -- an array to store the anagrams
        anagrams: strArr(1..1000);
        -- an array to store the strings
        lex: strArr(1..51244);
    begin
        index := 1;
        -- lets build the lex
        lex := buildLEXICON;
        -- find how many words the user wants to enter
        put("How many words would you like to enter: ");
        n := Integer'Value(get_line);
        -- ask for words one by one
        for i in 1..n loop
            put_line("Please enter your anagram:");
            word := get_line;
            -- lets get all anagrams for the word
            anagrams := generateAnagram(n, word, index);
            -- find which ones are actually words
            findAnagram(anagrams, index, lex);
            index := 1;
        end loop;
    end inputJumble;

begin
    -- Runs the main inputJumble procedure
    inputJumble;

end solvejumble;
