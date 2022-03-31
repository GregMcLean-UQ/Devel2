* ====================================================================
       program optim
* ====================================================================

*   Short description:
*     program to fit a curve to a set of x/y data points.

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
*     dph 8/5/96

*   Calls:
*     None

* ----------------------- Declaration section ------------------------

       implicit none

*   Subroutine arguments
*      none

*   Global variables
      include 'opt.inc'                ! optim include file.

*   Constant values
      
*   Internal variables
      double precision Initial_p(MAX_PARAM)
                                       ! initial parameter values
      double precision Final_p(MAX_PARAM)
                                       ! final parameter values                                       
      double precision Lower_bound(MAX_PARAM)
                                       ! lower bounds for each parameter
      double precision Upper_bound(MAX_PARAM)
                                       ! upper bound for each parameter                                                                      
      double precision R2              ! r squared value
      integer i

*   Initial data values
*      none

* --------------------- Executable code section ----------------------

      ! open files
      
      open (unit=LU_input, file='optim.in', status='old')
      rewind(LU_input)
      
      open (unit=LU_output, file='optim.out', status='unknown')
      rewind(LU_output)
      
      ! read in data points
      
      call Read_data_points (LU_input, X, Y, Num_data_points)
      
      ! setup parameters
      
      Num_param = 2
      Initial_P(1) = 10.0d0
      Initial_p(2) = -3.0d0
      Lower_bound(1) = 0.0d0
      Upper_bound(1) = 50.0d0      
      Lower_bound(2) = -10.0d0
      Upper_bound(2) = 10.0d0      
      Param_fixed(1) = .false.
      Param_fixed(2) = .false.
      Fit_quadratic = .false.
      
      ! call optimise
      
      call Optimise (LU_output,
     .               Num_param, Initial_p, Param_fixed,
     .               Lower_bound, Upper_bound,
     .               Num_data_points,
     .               Tolerance, Simplex,
     .               Final_p, Funct_value, SE,
     .               Fit_quadratic)

      ! calculate an r2 value.
      
      call RSquared (Num_data_points, Y, YEST, R2)

      ! output everything.
      
      write (LU_output, *) '-------------------------'
      write (LU_output, *) 'OPTIM output'
      write (LU_output, *) '-------------------------'
      write (LU_output, *)
      write (LU_output, *)
      write (LU_output, *) 'Final parameter values : '
      write (LU_output, *)
      do 10 i = 1, Num_param
         write (LU_output, '(a,i1,a, f8.3) ' ) 
     .       'Param(',i,') = ', Final_p(i)
10    continue

      write (LU_output, *)
      write (LU_output, *) 'R2 = ', R2
      write (LU_output, *)
      end
      
            
* ====================================================================
       subroutine Read_data_points (LU_Input, X, Y, Num_data_points)
* ====================================================================

*   Short description:
*     Read in all x/y data points from lu_input.

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
*     DPH 8/5/96

*   Calls:
*     None

* ----------------------- Declaration section ------------------------

       implicit none

*   Subroutine arguments
      integer LU_input                 ! (INPUT) unit number
      double precision X(*)            ! (OUTPUT) x data
      double precision Y(*)            ! (OUTPUT) y data
      integer Num_data_points          ! (OUTPUT) number of data points.

*   Global variables
*      none

*   Internal variables
      integer code

*   Constant values
*      none

*   Initial data values
*      none

* --------------------- Executable code section ----------------------


      Num_data_points = 1
10    continue
      read (LU_input, *, iostat=code) X(Num_data_points), 
     .                                Y(Num_data_points)                  
      if (code .eq. 0) then
         Num_data_points = Num_data_points + 1      
         goto 10
      endif
      
      Num_data_points = Num_data_points - 1      
      return
      end
           

* ====================================================================
       subroutine User_funct(Num_params, Param_values, Fun_value)
* ====================================================================

*   Short description:
*     Called from minim to calculate our function.

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
      integer Num_params               ! (INPUT) number of parameters
      double precision Param_values(*) ! (INPUT) parameter values
      double precision Fun_value       ! (OUTPUT) calculated function value.

*   Global variables
      include 'opt.inc'

*   Internal variables
      integer i

*   Constant values
*      none

*   Initial data values
*      none

* --------------------- Executable code section ----------------------

      do 10 i = 1, Num_data_points
         YEST(i) = Param_values(1) * exp(-Param_values(2) * X(i))
10    continue

      call SSR(Num_data_points, Y, YEST, Fun_value)

      return
      end
               
