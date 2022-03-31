* ====================================================================
       character*(*) function Get_file_string
     .     (Unit_number, Section, Key_name, Optional)
* ====================================================================

*   Short description:
*     Go look in file for the specified key_name.  Return the data
*     string associated with the keyname.  If Optional is .true. then
*     no error message is displayed if cannot find key_name

*   Assumptions:
*     Assumes that unit_number is an open file ready for reading.

*   Notes:
*     None

*   Procedure attributes:
*      Version:         Any hardware/Fortran77
*      Extensions:      Long names <= 20 chars.
*                       Lowercase
*                       Underscore
*                       Inline comments
*                       Include
*                       implicit none

*   Changes:
*     DPH 14/06/95

*   Calls:
*     None

* ----------------------- Declaration section ------------------------

       implicit none

*   Subroutine arguments
      integer Unit_number              ! (INPUT) Unit number to read from
      character Key_name*(*)           ! (INPUT) Key name to search for
      character Section*(*)            ! (INPUT) Section name to search in
      logical Optional                 ! (INPUT) IF .true. no error is displayed

*   Global variables
      character Get_data_string*300    ! function
      character Get_section_name*300   ! function
      logical Find_section_name        ! function

*   Internal variables
      character Line*300               ! Line read from file
      integer IOStatus                 ! IOSTAT from read
      character Return_string*200      ! String to return to caller.
      logical End_section              ! Have we reached end of section?
      logical Found                    ! Have we found the section?


*   Constant values
*      none

*   Initial data values
*      none

* --------------------- Executable code section ----------------------

      Found = Find_section_name(Unit_number, Section)

10    continue
      call Read_line (Unit_number, Line, IOstatus)

      if (IOstatus .eq. 0) then
         End_section = (Get_section_name (Line) .ne. ' ')

         ! Go try and match key name.

         if (End_section) then
            Return_string = ' '

         else
            Return_string = Get_data_string(Line, Key_name)
            if (Return_string .eq. ' ') then
               goto 10
            else
               ! Got it. exit routine
            endif
         endif
      endif

      if (Return_string .eq. ' ' .and. .not. Optional) then
         ! Didn't find parameter.  Throw an error message.

         line = 'Cannot find parameter in parameter file.' //
     .          '  Keyname = ' // Key_name
         call Error(line)
      endif

      Get_file_string = Return_string

      return
      end

* ====================================================================
       subroutine Read_line (Logical_unit, Line, IOStatus)
* ====================================================================

*   Short description:
*      Read in the next uncommented, non blank line from the control file.
*      Return an I/O status (0=Ok, 1=End_of_runblock, 2=End_of_file).
*      The line returned is converted to lowercase.

*   Assumptions:
*      Assumes that line is long enough to hold an entire line read from
*      control file.

*   Notes:

*   Procedure attributes:
*      Version:         Any hardware/Fortran77
*      Extensions:      Long names <= 20 chars.
*                       Lowercase
*                       Underscore
*                       Inline comments
*                       Include
*                       implicit none

*   Changes:
*      DPH - 11/6/92
*      DPH - 23/11/92 Now passing in logical unit number
*      DPH - 9/06/93 Adjusted comments
*      DPH - 8/7/94  Added code to remove all types of comments
*                    include inline comments from line.

*   Calls:
*      Lower_case
*      No_leading_spaces

* ----------------------- Declaration section ------------------------

       implicit none

*   Subroutine arguments
       character Line*(*)              ! (OUTPUT) Line read from file
       integer Logical_unit            ! (INPUT) Logical unit to read from
       integer IOStatus                ! (OUTPUT) Status of read

*   Global variables
       character Lower_case*500        ! function
       character No_leading_spaces*500 ! function

*   Internal variables
       integer Comment_pos             ! Position of comment in line
       integer Read_status             ! Status of read.

*   Constant values
       character Comment*(*)           ! Comment specifier
       parameter (Comment='!')

       integer End_file_status         ! End of file status
       parameter (End_file_status=2)

       character End_run*(*)           ! End of run specifier
       parameter (End_run='end run')

       integer End_run_status          ! End of run block status
       parameter (End_run_status=1)

       integer Ok_status               ! Ok status
       parameter (Ok_status = 0)

*   Initial data values
*      none

* --------------------- Executable code section ----------------------

10    read (logical_unit, '(a)', iostat=read_status ) line

      ! Remove comment from line if necessary

      Comment_pos = index (Line, comment)
      if (Comment_pos .eq. 0) then
         ! No comment on line

      else
         Line(Comment_pos:) = ' '
      endif

      if (Read_status .eq. Ok_status) then

         Line = Lower_case(Line)
         Line = No_leading_spaces(Line)

         if (Line.eq.End_run) then
            IOStatus = End_run_status

         else if (Line.eq.' ') then
            goto 10

         else
            IOStatus = Ok_status

         endif

      else
         IOStatus = End_file_status

      endif

      return
      end

* ====================================================================
       character*(*) function Get_data_string (Char_string, Key_name)
* ====================================================================

*   Short description:
*      Get a parameter string from a character string that
*      matches Key_name.  Returns ' ' if not found.

*   Assumptions:
*      Example of a char_string is
*         sw=30 25 15 15 ! yield=1.6

*   Notes:
*     It is possible for this routine to return a ' ' string even
*     when the key_name was found but had no string to the right
*     of the equals sign.

*   Procedure attributes:
*      Version:         Any hardware/Fortran77
*      Extensions:      Long names <= 20 chars.
*                       Lowercase
*                       Underscore
*                       Inline comments
*                       Include
*                       implicit none

*   Changes:
*      DPH - 16/11/92
*      DPH - 9/06/93 Modified to use the get_next_variable routine
*                    Comments upgraded as suggested by PG.
*      DPH - 12/08/93 Removed leading spaces from return string.
*     JNGH 3/8/94 used assign_substring s/r to  set function.
*                 made character strings same matching lengths

*   Calls:
*      assign_string
*      Get_next_variable
*      Lower_case
*      No_spaces
*      No_leading_spaces

* ----------------------- Declaration section ------------------------

       implicit none

*   Subroutine arguments
       character Char_string*(*)       ! (INPUT) Character string to look in
       character Key_name*(*)          ! (INPUT) Key name to search for

*   Global variables

       character Lower_case*30         ! function
       character No_spaces*30          ! function
       character No_leading_spaces*(500)
                                       ! function

*   Internal variables
       character Char_string_lower
     .    *(500)       ! Lower case version of char_string
       character Key*30                ! Key pulled apart from key_name_lower
       character Key_name_lower*30     ! Lower case version of key name
       character Parameters
     .    *(500)       ! Parameters to right of '=' sign

*   Constant values
*     none

*   Initial data values
*      none

* --------------------- Executable code section ----------------------

      Key_name_lower = Lower_case (Key_name)
      Key_name_lower = No_spaces (Key_name_lower)

      Char_string_lower = Char_string

10    continue
      call Get_next_variable (Char_string_lower, Key, Parameters)

      if (Key .eq. Key_name_lower) then
         Get_data_string = No_leading_spaces (Parameters)

      else if (Key .eq. ' ') then
         Get_data_string = ' '

      else
         goto 10

      endif

      return
      end

* ====================================================================
       character*(*) function Lower_case (char_string)
* ====================================================================

*   Short description:
*      Convert a character string to lower case.

*   Assumptions:

*   Notes:
*      This routine does not assume the ASCII character set and should
*      be platform independant.  It does assume that the characters of
*      'A' through 'Z' are sequential, as are 'a' through 'z', in the
*      character set.

*   Procedure attributes:
*      Version:         Any hardware/Fortran77
*      Extensions:      Long names <= 20 chars.
*                       Lowercase
*                       Underscore
*                       Inline comments
*                       Include
*                       implicit none

*   Changes:
*      DPH 28/5/92
*      DPH 9/02/93 - modified terminating condition on do loop
*      NH 1/03/93 - tested
*      JNGH 3/8/94 - put in subroutine to check string bounds and thus
*                    removed the need to take minimum of lower_case string.
*                    Changed do loop to terminate at last non-' '.

*   Calls:
*      assign_string
*      check_string_bounds
*      ichar
*      len
*      char
*      lastnb

* ----------------------- Declaration section ------------------------

       implicit none

*   Subroutine arguments
       character char_string*(*)       ! (INPUT) character string to convert

*   Global variables
      integer    lastnb                ! function

*   Internal variables
       integer   char_code             ! Code of character (ASCII on PC's)
       integer   char_index            ! Index into character string
       character Charact*1             ! Current character
       integer   Code_diff             ! Difference in codes between 'A' - 'a'
       integer   lower_char            ! Lowercase character code
       integer   string_end            ! end of string position

*   Constant values
*      None

*   Initial data values
*      None

* --------------------- Executable code section ----------------------

      ! Calculate the difference between 'A' and 'a' and apply this difference
      ! to each character in the character string.

      Code_diff = ichar ('A') - ichar ('a')

      lower_case = char_string
      string_end = lastnb (lower_case)

      do 10 char_index = 1, string_end
         Charact = lower_case(char_index:char_index)
         if (Charact .ge. 'A' .and. Charact .le. 'Z') then

            ! Character is uppercase - convert to lowercase

            char_code = ichar (Charact)
            lower_char = char_code - Code_diff
            Lower_case(char_index:char_index) = char (lower_char)

         else
            ! Character is already lowercase.
         endif
10    continue
20    continue

      return
      end

* ====================================================================
       character*(*) function No_spaces (Char_string)
* ====================================================================

*   Short description:
*      Remove all leading and embeded spaces from a string.  Does not
*      remove trailing spaces.

*   Assumptions:
*      None

*   Notes:

*   Procedure attributes:
*      Version:         Any hardware/Fortran77
*      Extensions:      Long names <= 20 chars.
*                       Lowercase
*                       Underscore
*                       Inline comments
*                       Include
*                       implicit none

*   Changes:
*      DPH - 16/11/92
*      DPH 9/02/93 Modified terminating condition of DO loop
*     JNGH 3/8/94 used assign_substring s/r to  set function.
*                 changed new_char_index initialisation for better reading

*   Calls:
*      assign_substring
*      LastNB
*      len

* ----------------------- Declaration section ------------------------

       implicit none

*   Subroutine arguments
       character Char_string*(*)       ! (INPUT) String to scan for spaces

*   Global variables
       integer LastNB                  ! function

*   Internal variables
       character Charact*1             ! Character from string.
       integer   Char_index            ! Index into character string.
       integer   New_char_index        ! Character index of new string.
       integer   string_end            ! position of end of string

*   Constant values
      character  myname*(*)            ! procedure name
      parameter (myname = 'no_spaces')

*   Initial data values
*      none

* --------------------- Executable code section ----------------------

      New_char_index = 0

      No_spaces = ' '
      string_end = lastnb (char_string)
      do 10 Char_index = 1, string_end
         Charact = Char_string(Char_index:Char_index)
         if (Charact.eq.' ') then
            ! Don't add this character
*            No_spaces(new_char_index:) = char_string(char_index:)

         else
            New_char_index = New_char_index + 1
            no_spaces(new_char_index:new_char_index) = charact

         endif

10    continue
20    continue

      return
      end

* ====================================================================
       integer function LastNB (string)
* ====================================================================

*   Short description:
*      Find the last non ' ' character in textstring.

*   Assumptions:

*   Notes:

*   Procedure attributes:
*      Version:         Any hardware/Fortran77
*      Extensions:      Long names <= 20 chars.
*                       Lowercase
*                       Underscore
*                       Inline comments
*                       Include
*                       implicit none

*   Changes:
*      DPH 28/5/92
*      100394 jngh rewritten to use binary search
*                  allowed for case of null string

*   Calls:
*      Len

* ----------------------- Declaration section ------------------------

       implicit none

*   Subroutine arguments
       character string*(*)       ! (INPUT) character string to search

*   Global variables
*      none

*   Internal variables

       integer    first           ! starting point for substring
       integer    last            ! finishing point for current comparison
       integer    middle          ! mid point of substring

*   Constant values

*   Initial data values
*      None

* --------------------- Executable code section ----------------------

*     Do a binary search

      first = 1
      last = len (string)
      if (last.gt.0) then

               ! this algorithm relies on the search ending on the first ' '
               ! after the last non ' '

         if (string(last:last).eq.' ') then
1000        continue
               middle = (first+last)/2

               if (string(middle:last).eq.' ') then
                  if (last.eq.first) then        ! end of string found
                     lastnb = last - 1
                  else                           ! look to left of mid point
                     last = middle
                     goto 1000
                  endif

               else                              ! look to right of mid point
                  first = middle+1
                  goto 1000
               endif

         else                                    ! string is full
            lastnb = last
         endif

      else
         print*, '*** Warning err:- zero length string in lastnb'
         lastnb = 0
      endif

      return
      end

* ====================================================================
       character*(*) function No_leading_spaces (Char_string)
* ====================================================================

*   Short description:
*      Remove all leading spaces from a string.

*   Assumptions:
*      None

*   Notes:

*   Procedure attributes:
*      Version:         Any hardware/Fortran77
*      Extensions:      Long names <= 20 chars.
*                       Lowercase
*                       Underscore
*                       Inline comments
*                       Include
*                       implicit none

*   Changes:
*      DPH - 16/11/92
*      DPH - 9/02/93 Renamed char_index to First_non_' ' and removed
*                    dependency on common block
*      DPH - 8/03/93 Rewrote routine to use FirstNB
*     JNGH 3/8/94 used assign_string s/r to  set function.

*   Calls:
*     assign_string
*     FirstNB

* ----------------------- Declaration section ------------------------

       implicit none

*   Subroutine arguments
       character Char_string*(*)       ! (INPUT) String to scan for spaces

*   Global variables
      integer Firstnb                  ! function

*   Internal variables
       integer First_non_blank         ! Index into character string.

*   Constant values

*   Initial data values
*      none

* --------------------- Executable code section ----------------------

      if (Char_string .eq. ' ') then
         No_leading_spaces = ' '

      else
         First_non_blank = FirstNB(Char_string)
         No_leading_spaces = Char_string(First_non_blank:)
      endif

      return
      end

* ====================================================================
      integer function FirstNB (char_string)
* ====================================================================

*   Short description:
*      Find the First non blank character in text string.

*   Assumptions:
*      none

*   Notes:
*      none

*   Procedure attributes:
*      Version:         Any hardware/Fortran77
*      Extensions:      Long names <= 20 chars.
*                       Lowercase
*                       Underscore
*                       Inline comments
*                       Include
*                       implicit none

*   Changes:
*     CM - 8/03/93
*     jngh 4/8/94 removed common include, made local blank parameter and
*                 changed index to avoid fortran function conflict

*   Calls:
*      Len

* ----------------------- Declaration section ------------------------

      implicit none

*   Subroutine arguments
      character char_string*(*)       ! (INPUT) character string to search

*   Global variables
*      none

*   Internal variables
      integer str_index                ! index counter
      integer String_length            ! length of string including ' 's

*   Constant values

*   Initial data values
*      none

* --------------------- Executable code section ----------------------

                 ! Do a forward search for the first non blank

                 ! the length of the string passed to the procedure

      String_length = len (char_string)

                 ! Checks each character from the left side the page
                 ! forwards for a blank

      do 10 str_index = 1, String_length, 1
         if (char_string(str_index:str_index).ne.' ') goto 20
10    continue

                 ! if string line is all blank

      str_index = 0

20    continue

                 ! the position of the last non blank found

      FirstNB = str_index

      return
      end

* ====================================================================
       subroutine Get_next_variable (Variables_str,
     .            Var_name, Values_str)
* ====================================================================

*   Short description:
*      Returns the next variable from Variables_str and its
*      associated data.

*   Assumptions:
*      None

*   Notes:
*      Example of a typical variables_str is :-
*         sw=30 25 15 15 ! yield=1.6
*      In this example, this routine would return sw as the var_name
*      and "30 25 15 15" as the values_str.

*   Procedure attributes:
*      Version:         Any hardware/Fortran77
*      Extensions:      Long names <= 20 chars.
*                       Lowercase
*                       Underscore
*                       Inline comments
*                       Include
*                       implicit none

*   Changes:
*      DPH - 16/11/92
*      DPH - 9/06/93 Modified to accept situations when Variables_str
*                    doesn't have a MES_delimiter in it.
*      DPH - 10/09/93 Made sure Var_name was returned with no leading spaces
*     JNGH 3/8/94 used assign_string s/r to  set arguments.

*   Calls:
*      assign_string
* ----------------------- Declaration section ------------------------

       implicit none

*   Subroutine arguments
       character Variables_str*(*)     ! (INPUT & OUTPUT) String to break up
       character Var_name*(*)          ! (OUTPUT) Extracted variable name
       character Values_str*(*)        ! (OUTPUT) Extracted values string

*   Global variables
       character No_spaces*(500)
                                       ! function

*   Internal variables
       integer Pos                     ! Position in variables_str
       character String_right
     .    *(500)       ! String to right of equals sign

*   Constant values
       character Equals*(*)            ! Equals sign
       parameter (Equals='=')

*   Initial data values
*      none

* --------------------- Executable code section ----------------------

      call Split_line (Variables_str, Var_name, String_right, Equals)
      Variables_str = String_right

      Pos = index (Variables_str, ',')

      ! Handle the situation when no MES_delimiter is in Variables_str.

      if (Pos .eq. 0) then
         Values_str = Variables_str
         Variables_str = ' '

      else
         Values_str = Variables_str (1:Pos-1)
         Variables_str = Variables_str (Pos+1:)
      endif

      Var_name = No_spaces(Var_name)

      return
      end

* ====================================================================
       subroutine Split_line (Line, Left_string,
     .    Right_string, Delimiter)
* ====================================================================

*   Short description:
*      Split up a character string into the string to the left of a
*      delimiter (Left_string) and the string to the right of
*      a delimiter (Right_string)

*   Assumptions:
*      Assumes that Left_string and Right_string are big enough

*   Notes:

*   Procedure attributes:
*      Version:         Any hardware/Fortran77
*      Extensions:      Long names <= 20 chars.
*                       Lowercase
*                       Underscore
*                       Inline comments
*                       Include
*                       implicit none

*   Changes:
*      DPH 11/6/92
*      DPH 9/02/93 Changed Key_name and Param_string names to Left_string &
*                  Right_string.  Also removed common block dependancy.
*                  Re-worked entire routine to handle delimiter's > 1 in size
*     jngh 4/8/94 used assign_string s/r to detect truncations
*                 allowed for case of delimiter being first character

*   Calls:
*      assign_string
*      index
*      len

* ----------------------- Declaration section ------------------------

       implicit none

*   Subroutine arguments
       character Delimiter*(*)         ! (INPUT) Delimiter to look for
       character Left_string*(*)       ! (OUTPUT) Extracted left string
       character Line*(*)              ! (INPUT) Line to break apart
       character Right_string*(*)      ! (OUTPUT) Extracted right string

*   Global variables
*     none

*   Internal variables
       integer Delimiter_Pos           ! Position of delimiter on line

*   Constant values

      integer Not_found                ! Pos when index doesn't find string
      parameter (Not_Found = 0)

*   Initial data values
*      none

* --------------------- Executable code section ----------------------

      Delimiter_Pos = index (Line,  Delimiter)

      if (Delimiter_Pos .eq. Not_found) then
         Left_string = Line
         Right_string = ' '

      else
         if (delimiter_pos.eq.1) then
            Left_string = ' '
         else
            Left_string = Line(1:Delimiter_pos - 1)
         endif
         Delimiter_pos = Delimiter_pos + len(Delimiter)
         if (Delimiter_pos .gt. len(Line)) then
            Right_string = ' '

         else
            Right_string = Line(Delimiter_pos:)
         endif
      endif

      return
      end

* ====================================================================
       subroutine Get_next_word (Line, Word)
* ====================================================================

*   Short description:
*     Return the next word from Line.  A word is a set
*     of alphanumeric characters separated by at least one space.
*     An equals sign is treated as a space.  Line is updated to
*     reflect the rest of the character string.

*   Assumptions:
*      None

*   Notes:
*      e.g.  If Line is :-  'This is a string'
*            then this routine will set Line to 'is a string' and
*            return Word = 'This'

*   Procedure attributes:
*      Version:         Any hardware/Fortran77
*      Extensions:      Long names <= 20 chars.
*                       Lowercase
*                       Underscore
*                       Inline comments
*                       Include
*                       implicit none

*   Changes:
*     DPH - 23/11/92
*     DPH - 9/02/93  Re-worked routine to fix assorted bugs and remove
*                    dependancy on common block
*     DPH - 9/06/93 Fixed bug with first IF statement.  Updated comments.
*     DPH - 12/08/93 Re-wrote routine to make it simpler to read
*     jngh - 14/7/94 Added in check of position in string
*     jngh - 4/8/94 rewrote to take account of word finishing at end of line.
*                    used assign_string s/r to catch errors.

*   Calls:
*     assign_string
*     len

* ----------------------- Declaration section ------------------------

       implicit none

*   Subroutine arguments
      character Line*(*)               ! (INPUT&OUTPUT) Line to get word from
      character Word*(*)               ! (OUTPUT) Word to return to caller

*   Global variables
*     none

*   Internal variables
      character  Charact               ! Character from line
      integer    line_index            ! Current position in input string
      integer    start_word            ! start position of word in input
                                       ! string
      integer    end_word              ! end position of word in input string
      integer    end_of_line           ! end position of line

*   Constant values
      character Equals*(*)             ! Equals sign
      parameter (Equals='=')

*   Initial data values
*      none

* --------------------- Executable code section ----------------------


      start_word = 0
      end_word = 0
      end_of_line = len(line)
      line_index = 1

         ! first - find start of word

1000  continue
      if (line_index .le. end_of_line) then
         charact = line(line_index:line_index)

         if (Charact .eq. ' ' .or. Charact .eq. Equals) then
               ! start not reached yet
            line_index = line_index + 1
            goto 1000

         else
               ! start of word reached
            start_word = line_index
         endif
      else
            ! end of line hit
         start_word = 0

      endif



         ! now - find end of word

      if (start_word.gt.0) then

2000     continue
         if (line_index .le. end_of_line) then
            charact = line(line_index:line_index)

            if (Charact .eq. ' ' .or. Charact .eq. Equals) then
                  ! end of word reached
               end_word = line_index - 1
                  ! extract word
               word = line(start_word:end_word)
               Line = Line(line_index:)

            else
                  ! end not reached yet
               line_index = line_index + 1
               goto 2000
            endif
         else
               ! end of line hit
            end_word = end_of_line
               ! extract word
            word = line(start_word:end_word)
            Line = ' '

         endif

      else
            ! no word found
         Line = ' '
         Word = ' '

      endif

      return
      end

* ====================================================================
      subroutine string_to_double_var (value_string, value, numvals)
* ====================================================================

*   Short description:
*     Get a single value from value_string

*   Assumptions:
*      None

*   Notes:
*      None

*   Procedure attributes:
*      Version:         Any hardware/Fortran77
*      Extensions:      Long names <= 20 chars.
*                       Lowercase
*                       Underscore
*                       Inline comments
*                       Include
*                       implicit none

*   Changes:
*      19/10/94 DPH

*   Calls:
*      String_to_real_array

* ----------------------- Declaration section ------------------------

      implicit none

*   Subroutine arguments
      character  value_string*(*)      ! (INPUT) string of number
      double precision value           ! (OUTPUT) variable to be read
      integer numvals                  ! (OUTPUT) number of values

*   Global variables

*   Internal variables
      integer    Read_status           ! Status of read stmt
      character  values*(300)          ! values string
      character  word*100              ! word from values string
      character e_message*300          ! error message

*   Constant values
      character  myname*(*)            ! Name of subroutine
      parameter (myname = 'string_to_double_var')

      integer    Ok_status             ! Line was read from file ok.
      parameter (Ok_status = 0)

*   Initial data values
*      none

* --------------------- Executable code section ----------------------

      Values = Value_string
      call Get_next_word(Values, word)
      read (word, '(g25.0)', iostat = read_status) value

      if (read_status.ne.OK_status) then
         numvals = 0
         value = 0.0d0

         write (e_message, '(a, a, a)')
     :       'Unable to read double precision value from string :-'
     :       , value_string

         call Error(e_message)
      else
         ! Read was successful.

         Numvals = 1
      endif

      return
      end

* ====================================================================
      subroutine string_to_integer_var (value_string, value, numvals)
* ====================================================================

*   Short description:
*     Get a single value from value_string

*   Assumptions:
*      None

*   Notes:
*      None

*   Procedure attributes:
*      Version:         Any hardware/Fortran77
*      Extensions:      Long names <= 20 chars.
*                       Lowercase
*                       Underscore
*                       Inline comments
*                       Include
*                       implicit none

*   Changes:
*      19/10/94 DPH

*   Calls:
*      String_to_real_array

* ----------------------- Declaration section ------------------------

      implicit none

*   Subroutine arguments
      character  value_string*(*)      ! (INPUT) string of number
      integer    value                 ! (OUTPUT) variable to be read
      integer    numvals               ! (OUTPUT) number of values

*   Global variables
*      none

*   Internal variables
      real Real_value                  ! dummy real value

*   Constant values
*      none

*   Initial data values
*      none

* --------------------- Executable code section ----------------------

      call string_to_real_var
     .   (value_string, Real_value, numvals)
      Value = nint(Real_value)

      return
      end

* ====================================================================
      subroutine string_to_real_var (value_string, value, numvals)
* ====================================================================

*   Short description:
*     Get a single value from record

*   Assumptions:
*      None

*   Notes:
*      None

*   Procedure attributes:
*      Version:         Any hardware/Fortran77
*      Extensions:      Long names <= 20 chars.
*                       Lowercase
*                       Underscore
*                       Inline comments
*                       Include
*                       implicit none

*   Changes:
*      011292 jngh specified and programmed
*      19/10/94 DPH Changed name of routine from get_value
*                   Changed call to get_values to string_to_real_array
*      211094 jngh added lastnb to prevent exceeding string length
*      220295 jngh added setting of value to zero if blank.

*   Calls:
*      String_to_real_array
*      lastnb

* ----------------------- Declaration section ------------------------

      implicit none

*   Subroutine arguments
      character  value_string*(*)      ! (INPUT) string of number
      real       value                 ! (OUTPUT) variable to be read
      integer    numvals               ! (OUTPUT) number of values

*   Global variables

*   Internal variables
      integer    Read_status           ! Status of read stmt
      character  values*(300)          ! values string
      character  word*100              ! word from values string
      character e_message*300          ! error message

*   Constant values
      integer    Ok_status             ! Line was read from file ok.
      parameter (Ok_status = 0)

*   Initial data values
*      none

* --------------------- Executable code section ----------------------

      Values = Value_string
      call Get_next_word(Values, word)
      if (word .eq. ' ') then
         read_status = -1
         value = 0.0

      else
         read (word, '(g25.0)', iostat = read_status) value
      endif

      if (read_status.ne.OK_status) then
         numvals = 0
         value = 0.0

         e_message =
     :       'Unable to read real value from string :- '
     :       // value_string

         call error (e_message)
      else
         ! Read was successful.

         Numvals = 1
      endif

      return
      end

* ====================================================================
      subroutine String_to_char_array
     .   (Value_string, array, limit, numvals)
* ====================================================================

*   Short description:
*     Get an array of values from string

*   Assumptions:
*      None

*   Notes:

*   Procedure attributes:
*      Version:         Any hardware/Fortran77
*      Extensions:      Long names <= 20 chars.
*                       Lowercase
*                       Underscore
*                       Inline comments
*                       Include
*                       implicit none

*   Changes:
*        DPH 19/10/94

*   Calls:

* ----------------------- Declaration section ------------------------

      implicit none

*   Subroutine arguments
      character  array(*)*(*)         ! (OUTPUT) array to be read
      character   value_string*(*)    ! (INPUT) record string of array
      integer     limit               ! (INPUT) array size_of
      integer     numvals             ! (OUTPUT) number of values

*   Global variables
      integer word_count               ! function

*   Internal variables
      integer indx                     ! index into array
      character Temp_string*(300)
                                       ! Temporary storage string
      character Word*400               ! Next word on string
      character e_message*300          ! error message

*   Constant values

*   Initial data values
*      none

* --------------------- Executable code section ----------------------

      numvals = word_count (Value_string)

      if ( numvals.gt.limit) then

               ! We have more entities than expected

         write (e_message, '(a, i5, a, a, i5, a)')
     :             value_string
     :           , limit, ' values read. '
     :           , 'Remaining', numvals - limit, ' values ignored.'

         call error(e_message)
         numvals = limit

      else
               ! We have number of expected entities
      endif

      call Fill_char_array(array, ' ', limit)

      if (numvals.eq.0) then

               ! We have no entities

      else

         Temp_string = Value_string
         do 20 Indx = 1, Numvals
            call Get_next_word(Temp_string, word)
            array(indx) = word
20       continue

      endif

      return
      end

* ====================================================================
      subroutine string_to_logical_var (value_string, value, numvals)
* ====================================================================

*   Short description:
*     Get a single value from value_string

*   Assumptions:
*      None

*   Notes:
*      None

*   Procedure attributes:
*      Version:         Any hardware/Fortran77
*      Extensions:      Long names <= 20 chars.
*                       Lowercase
*                       Underscore
*                       Inline comments
*                       Include
*                       implicit none

*   Changes:
*      19/10/94 DPH
*     jngh 21/2/95 changed write to string to replacement statement.
*      5/7/95 DPH Put in (1:100) on value_string when creating e_message

*   Calls:
*      String_to_real_array

* ----------------------- Declaration section ------------------------

      implicit none

*   Subroutine arguments
      character  value_string*(*)      ! (INPUT) string of number
      logical    value                 ! (OUTPUT) variable to be read
      integer    numvals               ! (OUTPUT) number of values

*   Global variables
      character  lower_case*100        ! function

*   Internal variables
      character  values*(100)          ! values string
      character  word*100              ! word from values string

*   Constant values
*      none

*   Initial data values
*      none

* --------------------- Executable code section ----------------------

      Values = Value_string
      call Get_next_word(Values, word)
      Word = Lower_case(Word)
      if (Word .eq. 'yes' .or. Word .eq. 'no') then
         Value = (Word .eq. 'yes')
         Numvals = 1

      else
         word =
     :        'Invalid logical variable found :- '
     :       // Value_string(1:100)
         call error (word)
         Numvals = 0
         Value = .false.
      endif

      return
      end


* ====================================================================
      integer function word_count (string)
* ====================================================================

*   Short description:
*     count_of_real_vals number of entities in a string, separated by one or
*     more blanks
*     or commas

*   Assumptions:
*      None

*   Notes:
*      None

*   Procedure attributes:
*      Version:         Any hardware/Fortran77
*      Extensions:      Long names <= 20 chars.
*                       Lowercase
*                       Underscore
*                       Inline comments
*                       Include
*                       implicit none

*   Changes:
*       180892 JNGH specified and programmed
*       040992 JNGH Changed variable names, descriptions and structure
*                   to better document function.
*       030393 jngh changed name from numstr to word_count

*   Calls:
*       Index
*       Lastnb
*       pop_routine
*       push_routine

* ----------------------- Declaration section ------------------------

       implicit none

*   Subroutine arguments
      character string*(*)             ! string to be searched

*   Global variables
       integer LastNB                  ! function

*   Internal variables
      logical entity_start             ! flag indicating if starting an entity
      integer indx                     ! character index
      logical on_entity_now            ! flag indicating if on an entity
      logical prev_on_entity           ! flag indicating if previous
                                       ! character was on an entity
      integer string_end               ! position of end  of string

*   Constant values
      character  entity_delim*(*)      ! possible delimiters of entities
      parameter (entity_delim = ' ,')

*   Initial data values
*      none

* --------------------- Executable code section ----------------------

            ! Take each character in string in turn and check if it
            ! is a delimiter in the list.  Find start of an entity
            ! and count_of_real_vals entity.

      prev_on_entity = .false.
      word_count = 0
      string_end = lastnb (string)

      do 1000 indx = 1, string_end

         on_entity_now = index (entity_delim, string(indx:indx)).eq.0
         entity_start = on_entity_now .and. .not.prev_on_entity

         if (entity_start) then
            word_count = word_count + 1
         else
         endif

         prev_on_entity = on_entity_now

1000  continue

      return
      end

* ====================================================================
       subroutine Fill_char_array (array, string, size_of)
* ====================================================================

*   Short description:
*   Set elements of a character array to a specified string.

*   Assumptions:
*      None

*   Notes:

*   Procedure attributes:
*      Version:         Any hardware/Fortran77
*      Extensions:      Long names <= 20 chars.
*                       Lowercase
*                       Underscore
*                       Inline comments
*                       Include
*                       implicit none

*   Changes:
*   NeilH - 23-11-1994 - Programmed and Specified

*   Calls:
*   Assign_string
*   pop_routine
*   push_routine

* ----------------------- Declaration section ------------------------

       implicit none

*   Subroutine arguments
      character array(*)*(*)           ! character array
      character string*(*)             ! character string
      integer   size_of                ! array size_of

*   Global variables
*      none

*   Internal variables
      integer counter                  ! simple counter variable

*   Constant values

*   Initial data values
*      none

* --------------------- Executable code section ----------------------

      do 100 counter = 1, size_of
         array(counter) = string
  100 continue

      return
      end

* ====================================================================
       integer function Find_string_in_array(String, Array, numvals)
* ====================================================================

*   Short description:
*     Find the string in the specified array.  Return index into
*     array if found or -1 otherwise.

*   Assumptions:
*      None

*   Notes:
*     None

*   Procedure attributes:
*      Version:         Any hardware/Fortran77
*      Extensions:      Long names <= 20 chars.
*                       Lowercase
*                       Underscore
*                       Inline comments
*                       Include
*                       implicit none

*   Changes:
*     DPH - 8/11/94

*   Calls:
*     None

* ----------------------- Declaration section ------------------------

       implicit none

*   Subroutine arguments
      character String*(*)             ! (INPUT) String to find
      character Array(*)*(*)           ! (INPUT) Array of strings to search
      integer Numvals                  ! (INPUT) Number values in array

*   Global variables
*      none

*   Internal variables
      logical Found                    ! Found string ?
      integer Indx                     ! Index into array.

*   Constant values
*      none

*   Initial data values
*      none

* --------------------- Executable code section ----------------------

      ! Go find the module if possible.

      Indx = 1
10    continue
      if (Indx .gt. numvals) then
         Found = .false.

      else if (Array(Indx) .eq. String) then
         Found = .true.

      else
         Indx = Indx + 1
         goto 10
      endif

      if (Found) then
         Find_string_in_array = Indx

      else
         Find_string_in_array = -1
      endif

      return
      end

* ====================================================================
       logical function Find_section_name
     .         (Unit_number, Section_name)
* ====================================================================

*   Short description:
*     Go find the section name on the specified open logical unit number.
*     Return TRUE if found or FALSE otherwise.  The logical unit number
*     will be positioned to the line AFTER the section name if found.

*   Assumptions:
*      None

*   Notes:
*     None

*   Procedure attributes:
*      Version:         Any hardware/Fortran77
*      Extensions:      Long names <= 20 chars.
*                       Lowercase
*                       Underscore
*                       Inline comments
*                       Include
*                       implicit none

*   Changes:
*     DPH 8/11/94
*     010495 jngh prevented search through file after 'weather' subsection
*                 encountered.

*   Calls:
*     None

* ----------------------- Declaration section ------------------------

       implicit none

*   Subroutine arguments
      integer Unit_number              ! (INPUT) Unit number to read from
      character Section_name*(*)       ! (INPUT) Section name to search for

*   Global variables
      character Get_section_name*(300)
                                       ! function

*   Internal variables
      character Line*(300)
                                       ! Line read from file
      integer Read_status              ! Status of read_line
      logical Section_found            ! Have we found the section ?
      character section_name_read*(300)

*   Constant values
      integer Ok_status                ! Read was ok
      parameter (Ok_status=0)

*   Initial data values
*      none

* --------------------- Executable code section ----------------------

      rewind (Unit_number)

10    continue
      call Read_line (Unit_number, Line, Read_status)

         if (Read_status .eq. Ok_status) then
            section_name_read = get_section_name (line)
            If (section_name_read .ne. Section_name) then
               if (index (section_name_read, 'weather').gt.4) then
                     ! don't look past weather section
                  section_found = .false.
               else
                  goto 10
               endif

            else
               ! we have found the section we want

               Section_found = .true.
            endif
         else
            ! we have trouble reading file - probably EOF

            Section_found = .false.
            Line = 'Cannot find section name : ' // Section_name
            call Error(Line)
         endif

      Find_section_name = Section_found

      return
      end

* ====================================================================
       character*(*) function Get_Section_Name (record)
* ====================================================================

*   Short description:
*   This function extracts the name of a section in an initialisation file
*   from a section definition line.  If the record does not contain a
*   section definition a blank value is returned

*   Assumptions:
*      None

*   Notes:

*   Procedure attributes:
*      Version:         Any hardware/Fortran77
*      Extensions:      Long names <= 20 chars.
*                       Lowercase
*                       Underscore
*                       Inline comments
*                       Include
*                       implicit none

*   Changes:
*   NIH - 15/06/94 Programmed and specified
*   080994 jngh cleaned up
*   DPH 19/10/94 Removed routine name argument from call to warning_error
*   DPH 8/11/94 Added a quick test for left_delimiter to front of
*               routine for speed reasons.

*   Calls:
*     Assign_string
*   pop_routine
*   push_routine

* ----------------------- Declaration section ------------------------

       implicit none

*   Subroutine arguments
      character  record*(*)            ! record from initialisation file

*   Global variables
      character  No_Leading_spaces*(300)
                                       ! function
      character  Lower_Case*(300)
                                       ! function

*   Internal variables
      logical    Section_found         ! legal section has  been found
      integer    Left_delimiter_pos    ! position of left delimiter in record
      integer    Right_delimiter_pos   ! position of left delimiter in record
      character  Section*(300)
                                       ! name of section of ini file
      character  TempRecord*(300)
                                       ! internal copy of record

*   Constant valuesz
      character  open_delimiter*(*)    ! section open delimiter
      parameter (open_delimiter = '[')

      character  close_delimiter*(*)   ! section close delimiter
      parameter (close_delimiter = ']')

*   Initial data values
*      none

* --------------------- Executable code section ----------------------

      ! Firstly do a quick test for a section name.

      if (index(Record, Open_delimiter) .eq. 0) then
         ! There's no section here - exit.

         Section = ' '

      else
         ! OK we could have a section name - find out.

         TempRecord= No_Leading_Spaces (Record)
         Left_delimiter_Pos = index (TempRecord, open_delimiter)
         Right_delimiter_Pos = index (TempRecord, close_delimiter)

         Section_found = Left_delimiter_Pos .eq. 1
     :             .and. Right_delimiter_Pos .ge. 2

         If (Section_found) then
                  ! We have a section heading

            If (Right_delimiter_Pos .ge. 3) then
               Section = TempRecord
     :                 (left_delimiter_pos+1:Right_delimiter_Pos-1)
               Section = No_Leading_Spaces (Lower_case (Section))

            Else
                  ! This is an empty section heading
               section = ' '
            Endif

            if (section.eq.' ') then
                  ! we have no section name specified
               call error('Blank section name in record')
            else
            endif

         else
               ! This is not a section heading
            section = ' '
         endif
      endif

      Get_Section_Name = Section

      return
      end

* ====================================================================
       logical function Leap_year (Year)
* ====================================================================

*   Short description:
*      Returns TRUE if year YEAR is a leap year

*   Assumptions:
*      None

*   Notes:
*      One earth orbit around the sun does not take an integral
*      number of days - 365 + a small part of a day.  Since the
*      Gregorian calendar year is measured as 365 days, a correction
*      for this err is made every fourth year by adding one day
*      to the length of the year.  This correction is a little too
*      much, thus in the centesimal years the correction is not made.
*      However this over corrects, so in every fourth centesimal year,
*      the correction of adding one day is made.

*      To summarise -
*      If the year is divisible by 4 it is a leap year, unless it is
*      a centesimal year, in which case_z it must be divisible by 400.
*      i.e.  it is a leap year if either of the conditions hold:
*         (1) the year is divisible by 4 but not by 100;
*         (2) the year is divisible by 400.

*   Procedure attributes:
*      Version:         Any hardware/Fortran77
*      Extensions:      Long names <= 20 chars.
*                       Lowercase
*                       Underscore
*                       Inline comments
*                       Include
*                       implicit none

*   Changes:
*      JNGH 21/1/91 - first programmed in AUSSIM
*      DPH 15/6/92  - put into new system

*   Calls:

* ----------------------- Declaration section ------------------------

       implicit none

*   Subroutine arguments
       integer Year                    ! (INPUT) Year

*   Global variables
*      none

*   Internal variables
      logical Y4                       ! Leap year - 4th year
      logical Y100                     ! Not leap year - century
      logical Y400                     ! Leap year - 4th century

*   Constant values
*      none

*   Initial data values
*      none

* --------------------- Executable code section ----------------------

      ! Divisible by 4 ?

      y4 = mod (Year, 4).eq.0

      ! divisible by 100 - centesimal year?

      y100 = mod (Year, 100).eq.0

      ! divisible by 400 ?

      y400 = mod (Year,400).eq.0

      ! is the year is divisible by 4 but not by 100;
      ! or is the year is divisible by 400.

      Leap_year = (y4 .and. .not. y100) .or. y400

      return
      end



*     ===========================================================
      subroutine day_of_year_to_date (dyoyr, iyr, date)
*     ===========================================================

*   Short description:
*                given the day of year number dyoyr, within year iyr,
*                convert it to calendar day nd and month mo.

*   Assumptions:
*       none

*   Notes:
*       none

*   Procedure attributes:
*      Version:         Any hardware/Fortran77
*      Extensions:      Long names <= 20 chars.
*                       Lowercase
*                       Underscore
*                       Inline comments
*                       Include
*                       implicit none

*   Changes:
*       020392 jngh specified and programmed
*       270592 jngh changed year and day checking to use of
*                   simple bound checking - cr339
*       290592 jngh corrected calculation of day of month - cr354
*     DPH 19/10/94 changed call to bndchk to bound_check_integer_var

*   Calls:
*            Bound_check_integer_var
*            leapyr
*            traceback_routine
*            pop_routine
*            push_routine
*            warning_error_found

* ----------------------- Declaration section ------------------------

      implicit none

*   Subroutine arguments
      integer    date (3)              ! (OUTPUT) day, month, year
      integer    iyr                   ! (INPUT) year
      integer    dyoyr                 ! (INPUT) day of year

*   Global variables
      logical    leap_year             ! function

*   Internal variables
      integer    mo                    ! month number
      integer    month (12)            ! days in month
      integer    nd                    ! day number
      integer    ydays                 ! days in year

*   Constant values
      integer    day                   ! subscript number of day in date
      parameter (day = 1)

      integer    mnth                  ! subscript number of month in date
      parameter (mnth = 2)

      character  myname*(*)            ! subroutine name
      parameter (myname = 'day_of_year_to_date')

      integer    year                  ! subscript number of year in date
      parameter (year = 3)

      integer    yhigh                 ! max. valid year
      parameter (yhigh = 4200)         ! far enough in the future to cover
                                       !    all cases

      integer    ylow                  ! min valid year
      parameter (ylow = 1583)          ! gregorian calender introduced in 1582


*   Initial data values
      save      month

      data      month(1) / 31/
      data      month(2) / 28/
      data      month(3) / 31/
      data      month(4) / 30/
      data      month(5) / 31/
      data      month(6) / 30/
      data      month(7) / 31/
      data      month(8) / 31/
      data      month(9) / 30/
      data      month(10)/ 31/
      data      month(11)/ 30/
      data      month(12)/ 31/



* --------------------- Executable code section ----------------------

              ! check for leap year and set feb and year length accordingly

         if (leap_year (iyr)) then
            month(2) = 29
            ydays = 366

         else
            month(2) = 28
            ydays = 365

         endif

         ! calculate day and month

         mo = 1
         nd = dyoyr

100      continue
         if (nd.gt.0) then
            nd = nd - month(mo)
            mo = mo + 1
            goto 100

         else
         endif

         date(mnth) = mo - 1
         date(day) = nd + month(mo-1)
         date(year) = iyr

      return
      end


* ====================================================================
       DOUBLE PRECISION function Date_to_jday (Dayz, Monthz, Yearz)
* ====================================================================

*   Short description:
*      Return a date as a Julian day number

*   Assumptions:
*      Assumes the date is after 1583. If not the function returns 0.0.

*   Notes:
*      This implementation is only valid for dates in the Gregorian
*      calender (after 15 October 1582).
*      THIS IS BASED ON THE ALGORITHM BY FLIEGEL AND VAN FLANDERN IN
*      C.ACM VOL.11 (OCT,1968) P.657

*   Procedure attributes:
*      Version:         Any hardware/Fortran77
*      Extensions:      Long names <= 20 chars.
*                       Lowercase
*                       Underscore
*                       Inline comments
*                       Include
*                       implicit none

*   Changes:
*      Original coding by JNGH for AUSSIM - Modified DPH 30/6/92
*       100393  jngh changed date_to_jday arguments to integer.
*       270295  jngh changed real to dble intrinsic function.

*   Calls:
*      Aint
*      Check_date
*      Real

* ----------------------- Declaration section ------------------------

       implicit none

*   Subroutine arguments
       integer Dayz                    ! (INPUT) Day
       integer Monthz                  ! (INPUT) Month
       integer Yearz                   ! (INPUT) Year

*   Global variables

*   Internal variables
       double precision Day            ! Day
       double precision Month          ! Month
       double precision Quotnt         ! Quotient used in Fliegel calculations
       double precision Year           ! Year

*   Constant values
*      none

*   Initial data values
*      none

* --------------------- Executable code section ----------------------


      if (Yearz.gt.1582) then

         Day = dble (Dayz)
         Month = dble (Monthz)
         Year = dble (Yearz)

         Quotnt = aint ((Month - 14.0d0)/12.0d0)

         Date_to_jday = dble (
     :      Day - 32075.0d0
     :    + dint(1461.0d0* (Year + 4800.0d0 + Quotnt) /4.0d0)
     :    + dint(367.0d0* (Month - 2.0d0 - Quotnt*12.0d0) /12.0d0)
     :    - dint(3.0d0*dint((Year + 4900.0d0 + Quotnt) /100.0d0) /4.0d0)
     :    )

      else
         Date_to_jday = 0.0D0
      endif

      return
      end


