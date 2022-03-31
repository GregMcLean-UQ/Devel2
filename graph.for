* ====================================================================
       subroutine GR_CalcMinMax(X,Y,Nobs,XMin,XMax,YMin,YMax)
* ====================================================================

*   Short description:
*     Calculates the minimum and maxamum x & y values

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

*   Calls:
*     None

* ----------------------- Declaration section ------------------------

       implicit none

*   Subroutine arguments
      double precision X(*)            ! (INPUT) x values
      double precision Y(*)            ! (INPUT) y values
      integer Nobs                     ! (INPUT) number of observations
      real XMin                        ! (OUTPUT) minimum value for x axis
      real XMax                        ! (OUTPUT) maximum value for x axis
      real YMin                        ! (OUTPUT) minimum value for y axis
      real YMax                        ! (OUTPUT) maximum value for y axis

*   Global variables
*      none

*   Internal variables
      integer i

*   Constant values
*      none

*   Initial data values
*      none

* --------------------- Executable code section ----------------------

      XMin = X(1)
      XMax = X(1)
      YMin = Y(1)
      YMax = Y(1)
      do 10 i = 1,nobs
         if(X(i).gt.XMax) XMax = X(i)
         if(X(i).lt.XMin) XMin = X(i)
         if(Y(i).gt.YMax) YMax = Y(i)
         if(Y(i).lt.YMin) YMin = Y(i)
10    continue

      return
      end

* ====================================================================
       subroutine GR_AutoscaleX(XMin,XMax)
* ====================================================================

*   Short description:
*     Scale the x axis.  The values of XMin and XMax are the minimum 
*     and maximum x values that the axis must cover.  Both of these 
*     variables must by provided by the calling routine.

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

*   Calls:
*     None

* ----------------------- Declaration section ------------------------

       implicit none

*   Subroutine arguments
      real XMin                        ! (INPUT) Smallest X value
      real XMax                        ! (INPUT) Largest X value

*   Global variables
      include 'graph.inc'              ! graph common block

*   Internal variables
      integer XPlaces

*   Constant values
*      none

*   Initial data values
*      none

* --------------------- Executable code section ----------------------

      ! Ensure that XMin,XMax & YMin,YMax pairs are not equal.

      if (XMin.eq.XMax) then
         XMin = XMin - 1.0
         XMax = XMax - 1.0
      endif

      if (.not. Init) call GR_Initialise(0,0,0,0)
      call GR_ScaleAxis(XMin,XMax,CharX-17,XStart,XInt,XPlaces)
      XAxis = .true.
      call GR_DrawXAxis(XPlaces)
   
      return
      end

* ====================================================================
       subroutine GR_AutoscaleY(YMin,YMax)
* ====================================================================

*   Short description:
*     Scale the Y axis.  The values of XMin and XMax are the minimum 
*     and maximum Y values that the axis must cover.  Both of these 
*     variables must by provided by the calling routine.

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

*   Calls:
*     None

* ----------------------- Declaration section ------------------------

       implicit none

*   Subroutine arguments
      real YMin                        ! (INPUT) Smallest Y value
      real YMax                        ! (INPUT) Largest Y value

*   Global variables
      include 'graph.inc'              ! graph common block

*   Internal variables
      integer YPlaces

*   Constant values
*      none

*   Initial data values
*      none

* --------------------- Executable code section ----------------------

      if (YMin.eq.YMax) then
         YMin = YMin - 1.0
         YMax = YMax - 1.0
      endif
      if (.not. Init) call GR_Initialise(0,0,0,0)
      call GR_ScaleAxis(YMin,YMax,CharY-4,YStart,YInt,YPlaces)
      YAxis = .true.
      call GR_DrawYAxis(YPlaces)
      
      return
      end

* ====================================================================
       subroutine GR_ManualScaleX(AStart,AInt,APlaces)
* ====================================================================

*   Short description:
*     Manual scaling routiens

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

*   Calls:
*     None

* ----------------------- Declaration section ------------------------

       implicit none

*   Subroutine arguments
      real AStart                      ! (INPUT) Start of axis
      real AInt                        ! (INPUT) Interval of axis
      integer Aplaces                  ! (INPUT) Number of decimal places

*   Global variables
      include 'graph.inc'              ! graph common block

*   Internal variables
*      none

*   Constant values
*      none

*   Initial data values
*      none

* --------------------- Executable code section ----------------------

      if (.not. Init) call GR_Initialise(0,0,0,0)
      XStart = AStart
      XInt = AInt
      call GR_DrawXAxis(APlaces)
      XAxis = .true.
   
      return
      end

* ====================================================================
       subroutine GR_ManualScaleY(AStart,AInt,APlaces)
* ====================================================================

*   Short description:
*     Manual scaling routiens

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

*   Calls:
*     None

* ----------------------- Declaration section ------------------------

       implicit none

*   Subroutine arguments
      real AStart                      ! (INPUT) Start of axis
      real AInt                        ! (INPUT) Interval of axis
      integer Aplaces                  ! (INPUT) Number of decimal places

*   Global variables
      include 'graph.inc'              ! graph common block

*   Internal variables
*      none

*   Constant values
*      none

*   Initial data values
*      none

* --------------------- Executable code section ----------------------

      if (.not. Init) call GR_Initialise(0,0,0,0)
      YStart = AStart
      YInt = AInt
      call GR_DrawYAxis(APlaces)
      YAxis = .true.

      return
      end

* ====================================================================
       subroutine GR_DrawXAxis(XPlaces)
* ====================================================================

*   Short description:
*     Draw in the x axis and label the tick marks

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

*   Calls:
*     None

* ----------------------- Declaration section ------------------------

       implicit none

*   Subroutine arguments
      integer XPlaces                  ! (INPUT) NUmber of decimal places on axis

*   Global variables
      include 'graph.inc'              ! graph common block

*   Internal variables
      integer GR_Exponent
      integer ChPos
      integer Row
      integer AddOn
      real Lab
      character ForMT*10
      character Zero*10
      character Label*10
      integer i,j, k
      real xscale

*   Constant values
*      none

*   Initial data values
*      none

* --------------------- Executable code section ----------------------

      ! Draw in the XAxis line at row NCharY - 2

      i = 11
      do 30 k = 1,DivsX
         Buffer(i,CharY-2) = '+'
         Buffer(i+1,CharY-2) = '-'
         Buffer(i+2,CharY-2) = '-'
         Buffer(i+3,CharY-2) = '-'
         Buffer(i+4,CharY-2) = '-'
         i = i + 5
30    continue

      Buffer(i,CharY-2) = '+'

      ! Label the X Axis.

      XScale = XInt / 5
      if (abs(GR_Exponent(XInt)).le.3) then
         ForMT = '(f10.'//char(XPlaces+48)//')'
      else
         ForMT = '(e10.2)'
         XPlaces = 3
      endif
      Lab = XStart
      ChPos = 2 + XPlaces
      AddOn = 9 - XPlaces
      write(Zero,ForMt) 0.0
      do 55 i=1,DivsX+1
         write(Label,ForMt) Lab
         if (mod(i,2).ne.0) then
            Row = CharY-1  
            call GR_CopyLabel(Label,ChPos,Row,Buffer)

         else
            Row = CharY
         endif
         if (Label.eq.Zero.and.ChPos+AddOn.ne.11) then
            do 50 j=CharY-3,1,-1
               if (Buffer(ChPos+AddOn,j).eq.'-') then
                  Buffer(ChPos+AddOn,j) = '+'
               else
                  Buffer(ChPos+AddOn,j) = '|'
                endif
50         continue
         endif
         ChPos = ChPos + 5
         Lab = Lab + 5 * XScale
55    continue

      return
      end

* ====================================================================
       subroutine GR_DrawYAxis(YPlaces)
* ====================================================================

*   Short description:
*     Draw in the y axis and label the tick marks

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

*   Calls:
*     None

* ----------------------- Declaration section ------------------------

       implicit none

*   Subroutine arguments
      integer YPlaces                  ! (INPUT) NUmber of decimal places on axis

*   Global variables
      include 'graph.inc'              ! graph common block

*   Internal variables
      integer GR_Exponent
      integer ChPos
      real Lab
      character ForMT*10
      character Zero*10
      character Label*10
      integer i,j,k
      real yscale

*   Constant values
*      none

*   Initial data values
*      none

* --------------------- Executable code section ----------------------


      ! Draw in the YAxis line at column 11.

      j = 5
      do 10 i=CharY-2,1,-1
         if (mod(j,5).eq.0) then
            j = 1
            Buffer(11,i) = '+'
         else
            Buffer(11,i) = '|'
            j = j + 1
         endif
10      continue

      ! Label the Y Axis.

      YScale = YInt / 5.0
      if (abs(GR_Exponent(YInt)).le.3) then
         ForMT = '(f10.'//char(YPlaces+48)//')'
      else
         ForMT = '(e10.2)'
      endif
      Lab = YStart
      ChPos = CharY-2
      write(Zero,ForMt) 0.0
40    write(Label,ForMt) Lab
      call GR_CopyLabel(Label,1,ChPos,Buffer)
      if (Label.eq.Zero.and.ChPos.ne.CharY-2) then
         i = 12
         do 45 j = 1,DivsX
            do 42 k = 0,4
               Buffer(i+k,ChPos) = '-'
42          continue
            i = i + 5
45       continue
      endif
      ChPos = ChPos - 5
      Lab = Lab + 5 * YScale
      if (ChPos.ge.1) goto 40
      
      return
      end

* ====================================================================
       subroutine GR_CopyLabel(Lab,x,y,Buffer)
* ====================================================================

*   Short description:
*     This routine copys a Lab to the buffer at the x,y co-ordinates specified.

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

*   Calls:
*     None

* ----------------------- Declaration section ------------------------

       implicit none

*   Subroutine arguments
      character Lab*10
      character Buffer(140,140)*1
      integer x
      integer y
      
*   Global variables
      integer SaveX

*   Internal variables
      integer i

*   Constant values
*      none

*   Initial data values
*      none

* --------------------- Executable code section ----------------------


      SaveX = x
      do 10 i=1,10
         Buffer(x,y) = Lab(i:i)
         x = x + 1
10    continue
      x = SaveX
      
      return
      end

* ====================================================================
       subroutine GR_ScaleAxis
     .    (AMin,AMax,NCharA,AStart,AInterv,APlaces)
* ====================================================================

*   Short description:
*     Scale an axis calculating the tick labels

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

*   Calls:
*     None

* ----------------------- Declaration section ------------------------

       implicit none

*   Subroutine arguments
      real AMin
      real AMax
      integer NCharA
      real AStart
      real AInterv
      integer APlaces

*   Global variables
      include 'graph.inc'              ! graph common block
      real Gr_rounding                 ! function

*   Internal variables
      real AEnd,Diff,Ints(10)
      integer Divs,GR_Exponent,Places(10)
      integer i
      integer iexp

*   Constant values
*      none

*   Initial data values
      data Ints/1.0,1.5,2.0,2.5,4.0,5.0,6.0,7.5,8.0,10.0/
      data Places/0,1,0,1,0,0,0,1,0,0/

* --------------------- Executable code section ----------------------

      Divs = int(NCharA / 5)
      Diff = AMax - AMin

5     IExp = GR_Exponent(Diff/Divs)

      do 10 i = 1,9
         if (Diff/Divs.lt.Ints(i)*(10.0**IExp)) goto 20
10    continue
20    AInterv = Ints(i)*(10.0**IExp)
      APlaces = abs(IExp) + Places(i)

      AStart = GR_Rounding(AMin,AInterv,.false.)
      AEnd = Divs * AInterv + AStart
      if (AEnd .le. AMax) then
         Diff = 0.1 * Diff + Diff
         goto 5
      endif
      
      return
      end

* ====================================================================
       real function GR_Rounding(Num,Factor,Up)
* ====================================================================

*   Short description:
*     Rounds a number to the nearest factor.  If Up is true then the number will
*     be rounded up.

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

*   Calls:
*     None

* ----------------------- Declaration section ------------------------

       implicit none

*   Subroutine arguments
      real Num
      real Factor
      logical Up

*   Global variables
*      none

*   Internal variables
      real Save
      integer Sign

*   Constant values
*      none

*   Initial data values
*      none

* --------------------- Executable code section ----------------------

      Sign = 1
      Save = Num
      if (Num.lt.0.0) then
         Num = abs(Num)
         Sign = -1
         Up = .true.
      endif

      if (mod(Num,Factor).ne.0) then
         if (Up) then
            Num = aint(Num / Factor) * Factor + Factor
         else
            Num = aint(Num / Factor) * Factor
         endif
      endif
      GR_Rounding = Num * Sign
      Num = Save

      return
      end

* ====================================================================
      integer function GR_Exponent(Num)
* ====================================================================

*   Short description:
*     This routine calculates the exponent that the number NUM would have if it
*     was converted to scientific notation. [e.g. 1.567 x 10**-1 exp=-1]

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

*   Calls:
*     None

* ----------------------- Declaration section ------------------------

       implicit none

*   Subroutine arguments
      real Num

*   Global variables
*      none

*   Internal variables

*   Constant values
*      none

*   Initial data values
*      none

* --------------------- Executable code section ----------------------

      if (Num.eq.0.0) then
         GR_Exponent = 0
      else if(Num.ge.1) then
         GR_Exponent = int(log10(Num))
      else
         GR_Exponent = -int(abs(log10(abs(Num))) + 1.0)
      endif
   
      return
      end

* ====================================================================
       subroutine GR_DataPlot(X,Y,Nobs,Symbol)
* ====================================================================

*   Short description:
*     This routine places the data point onto the graph.  A symbol of ' ' or a
*     space will give automatic symbol generation based on the characters 0-9,
*     A-Z and *,  for 0 to 9, 10 to 36, and above 36 occurences of a data point on
*     a given location respectively.

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

*   Calls:
*     None

* ----------------------- Declaration section ------------------------

       implicit none

*   Subroutine arguments
      double precision X(*)            ! (INPUT) x values
      double precision Y(*)            ! (INPUT) y values
      integer nobs                     ! (INPUT) number of observations
      character Symbol*1               ! (INPUT) character symbol to use

*   Global variables
      include 'graph.inc'              ! graph common block

*   Internal variables
      integer ASCIICode
      real xmin,xmax
      real ymin,ymax
      real xscale,yscale
      integer i
      integer xpos, ypos

*   Constant values
*      none

*   Initial data values
*      none

* --------------------- Executable code section ----------------------

      ! Check if 2 axes have been defined.

      write(*,'(//a)') '     858'
      if (.not. Init) call GR_Initialise(0,0,0,0)
      write(*,'(//a)') '     860'
      if (.not.XAxis.or..not.YAxis)
     .    call GR_CalcMinMax(x,y,Nobs,XMin,XMax,YMin,YMax)
      write(*,'(//a)') '     863'
      if (.not.XAxis) call GR_AutoScaleX(XMin,XMax)
      write(*,'(//a)') '     865'
      if (.not.YAxis) call GR_AutoScaleY(YMin,YMax)
      write(*,'(//a)') '     867'

      ! Calculate the scaling value for the 2 axes.

      XScale = XInt / 5
      YScale = YInt / 5

      ! Put in the data points. XScale & YScale are the number of units per character
      ! position.

      if (Nobs.lt.1) return
      OffScale = .false.
	  write(*,'(//a)') '     874'
      do 57 i = 1,nobs
         XPos = 11 + nint((x(i) - XStart) / XScale)
         YPos = (CharY-2) - nint((y(i) - YStart) / YScale)
         if (XPos.lt.11.or.XPos.gt.CharX-6.or.
     .      YPos.lt.1.or.YPos.gt.CharY-2) then
            OffScale = .true.
         else
            if (Symbol.eq.' ') then
               ASCIICode = ichar(Buffer(XPos,YPos)) + 1
               if (ASCIICode.eq.33) ASCIICode = 49
               if (ASCIICode.eq.58) ASCIICode = 65
               if (ASCIICode.eq.91) ASCIICode = 42
               if (ASCIICode.eq.43) ASCIICode = 42
               if (ASCIICode.eq.46) ASCIICode = 49
               if (ASCIICode.eq.125) ASCIICode = 49
               if (ASCIICode.eq.44) ASCIICode = 49
               Buffer(XPos,YPos) = Char(ASCIICode)
            else
               Buffer(XPos,YPos) = Symbol
            endif
         endif
57    continue
      
      return
      end

* ====================================================================
       subroutine GR_Print(UnitNo)
* ====================================================================

*   Short description:
*     Send the graph to the unitno supplied by the caller

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

*   Calls:
*     None

* ----------------------- Declaration section ------------------------

       implicit none

*   Subroutine arguments
      integer UnitNo                   ! (INPUT) unit number to write to

*   Global variables
      include 'graph.inc'              ! graph common block

*   Internal variables
      integer i,j

*   Constant values
*      none

*   Initial data values
*      none

* --------------------- Executable code section ----------------------


      ! Check to see if there is anything to output.

      if (.not.XAxis) then
         write(UnitNO,*) 'Error - There is nothing to plot.'
         return
      endif

      ! Output the Buffer to the terminal.

      do 60 i = 1,CharY
         write(UnitNo,*) (Buffer(j,i),j=1,CharX)
60    continue
      if (OffScale) then
         write(UnitNo,'(10x,a)')
     .     'Due to the current scaling of the axes, some data points'
         write(UnitNO,'(10x,a)')
     .     'could not be plotted.'
      endif
      call GR_Initialise(0,0,0,0)
      
      return
      end

* ====================================================================
       subroutine GR_Initialise(NCharX,NCharY,NDivsX,NDivsY)
* ====================================================================

*   Short description:
*     Initialises several variables and clears the BUFFER array.

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

*   Calls:
*     None

* ----------------------- Declaration section ------------------------

       implicit none

*   Subroutine arguments
      integer NCharX
      integer NCharY
      integer NDivsX
      integer NDivsY

*   Global variables
      include 'graph.inc'              ! graph common block

*   Internal variables
      integer i,j

*   Constant values
*      none

*   Initial data values
*      none

* --------------------- Executable code section ----------------------
      write(*,'(//a)') '     1030'
      NDivsX = 1
      NDivsY = 2

      ! Check character number values.
      write(*,'(//a)') '     1031'
      if (NCharx.eq.0) then
         CharX = 79
         CharY = 24
      else
         CharX = NCharX
         CharY = NCharY
      endif
      CharX = Max(Min(Charx,133),10)
      CharY = Max(Min(Chary,133),10)
      write(*,'(//a)') '     1041'

      ! Initialise variables.

      write(*,'(//a)') '     1047'
      NDivsX = (CharX-17) / 5.0
      NDivsY = (CharY-4) / 5.0
!      DivsX = (CharX-17) / 5.0
!      DivsY = (CharY-4) / 5.0
	  write(*,'(//a)') '     1049'
	  write(*,*) DivsX, DivsY
	  write(*,*) NDivsX, NDivsY
!      NDivsX = DivsX
!      NDivsY = DivsY
	  write(*,'(//a)') '     1050'
      Init = .true.
      XAxis = .false.
      YAxis = .false.
      

      ! Initialise Buffer with spaces.
      write(*,'(//a)') '     1057'

      do 10 i = 1,140
         do 10 j = 1,140
            Buffer(i,j) = ' '
10    continue

      return
      end

