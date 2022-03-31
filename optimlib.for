* ====================================================================
       subroutine Optimlib_setup (Num_data_points)
* ====================================================================

*   Short description:
*     Setup the optimlib library code.

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
*     DPH 14/06/95

*   Calls:
*     None

* ----------------------- Declaration section ------------------------

       implicit none

*   Subroutine arguments
      integer Num_data_points          ! (INPUT) number of data points

*   Global variables
      include 'optimlib.inc'           ! optimlib common

*   Internal variables
*      none

*   Constant values
*      none

*   Initial data values
*      none

* --------------------- Executable code section ----------------------

      g_jack_num = 0
      g_group_size = Num_data_points

      return
      end

* ====================================================================
       subroutine Calc_center_diff
     .    (Num_params, Param_lower, Param_upper,
     .     Param_center, Param_diff)
* ====================================================================

*   Short description:
*     Calculate the center and difference of each of the parameters.
*
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
*     DPH - 13/06/95

*   Calls:

* ----------------------- Declaration section ------------------------

       implicit none

*   Subroutine arguments
      integer Num_params               ! (INPUT) Total number of parameters
      double precision Param_lower(*)  ! (INPUT) Lower bound of parameter
      double precision Param_upper(*)  ! (INPUT) Upper bound of parameter
      double precision Param_center(*) ! (OUTPUT) Initial parameter center values
      double precision Param_diff(*)   ! (OUTPUT) Initial parameter difference values

*   Global variables

*   Constant values

*   Internal variables
      integer Param_index              ! Index into parameter values

*   Initial data values

* --------------------- Executable code section ----------------------

      ! Scale the parameters so that they are all of similiar magnitude

      do 10 Param_index = 1, Num_params
         Param_center(Param_index) = (Param_lower(Param_index) +
     .                        Param_upper(Param_index)) / 2.0
         Param_diff(Param_index) = (Param_upper(Param_index) -
     .                        Param_lower(Param_index)) / 6.0
         if (Param_diff(Param_index) .eq. 0) then
            Param_diff(Param_index) = 1.0
         endif
10    continue

      return
      end

* ====================================================================
       subroutine Param_to_scaled
     .    (Num_params, Param_values)
* ====================================================================

*   Short description:
*     Scale the parameters so that they are of similar magnitude.
*
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
*     DPH - 13/06/95

*   Calls:

* ----------------------- Declaration section ------------------------

       implicit none

*   Subroutine arguments
      integer Num_params               ! (INPUT) Total number of parameters
      double precision Param_values(*) ! (INPUT & OUTPUT) Parameter values to convert

*   Global variables
      include 'optimlib.inc'           ! optimlib common

*   Constant values

*   Internal variables
      integer Param_index              ! Index into parameter values

*   Initial data values

* --------------------- Executable code section ----------------------

      ! Scale the parameters so that they are all of similiar magnitude

      do 10 Param_index = 1, Num_params
         if (g_Param_center(Param_index) .ne. 0) then
            Param_values(Param_index) = (Param_values(Param_index) -
     .        g_Param_center(Param_index)) / g_Param_diff(Param_index)
         endif
10    continue

      return
      end

* ====================================================================
       subroutine Scaled_to_param
     .    (Num_params, Param_values)
* ====================================================================

*   Short description:
*     Remove scaling off the parameters i.e. undo what was done in
*     the param_to_scaled routine.
*
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
*     DPH - 13/06/95

*   Calls:

* ----------------------- Declaration section ------------------------

       implicit none

*   Subroutine arguments
      integer Num_params               ! (INPUT) Total number of parameters
      double precision Param_values(*) ! (INPUT & OUTPUT) Parameter values to convert

*   Global variables
      include 'optimlib.inc'           ! optimlib common

*   Constant values

*   Internal variables
      integer Param_index              ! Index into parameter values

*   Initial data values

* --------------------- Executable code section ----------------------

      ! Remove the scaling

      do 10 Param_index = 1, Num_params
         if (g_Param_center(Param_index) .ne. 0) then
           Param_values(Param_index) = Param_values(Param_index) *
     .         g_Param_diff(Param_index) + g_Param_center(Param_index)
         endif
10    continue

      return
      end

* ====================================================================
       subroutine Scaled_to_Z
     .    (Num_params, Param_values)
* ====================================================================

*   Short description:
*     Convert the scaled parameter values passed in into Z values

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
*     DPH - 13/06/95

*   Calls:

* ----------------------- Declaration section ------------------------

       implicit none

*   Subroutine arguments
      integer Num_params               ! (INPUT) Total number of parameters
      double precision Param_values(*) ! (INPUT & OUTPUT) Parameter values to convert

*   Global variables
      include 'optimlib.inc'           ! optimlib common block

*   Constant values

*   Internal variables
      integer Param_index              ! Index into parameter values

*   Initial data values

* --------------------- Executable code section ----------------------

      do 20 Param_index = 1, Num_params
         ! Don't convert fixed parameters.

         if (Param_values(Param_index) .ne. 
     .       g_Scaled_lower(Param_index)) then
            if (g_Param_center(Param_index) .ne. 0) then
               Param_values(Param_index) = sqrt( log(
     .         (g_Scaled_upper(Param_index)-
     .             g_Scaled_lower(Param_index)) /
     .         (Param_values(Param_index)-
     .             g_Scaled_lower(Param_index))))
            endif
         endif

20    continue

      return
      end

* ====================================================================
       subroutine Z_to_scaled
     .    (Num_params, Param_values)
* ====================================================================

*   Short description:
*     Convert the Z parameter values passed in into scaled values

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
*     DPH - 13/06/95

*   Calls:

* ----------------------- Declaration section ------------------------

       implicit none

*   Subroutine arguments
      integer Num_params               ! (INPUT) Total number of parameters
      double precision Param_values(*) ! (INPUT & OUTPUT) Parameter values to convert

*   Global variables
      include 'optimlib.inc'           ! optimlib common block

*   Constant values

*   Internal variables
      integer Param_index              ! Index into parameter values

*   Initial data values

* --------------------- Executable code section ----------------------

      do 20 Param_index = 1, Num_params
         if (g_Param_center(Param_index) .ne. 0) then
            Param_values(Param_index) = g_Scaled_lower(Param_index) +
     .      (g_Scaled_upper(Param_index)-g_Scaled_lower(Param_index))/
     .      exp((Param_values(Param_index))**2)
         endif
20    continue

      return
      end

* ====================================================================
       subroutine Check_bounds (Num_params, Param_values,
     .                          Param_lower, Param_upper)
* ====================================================================

*   Short description:
*     Check the bounds of the parameters passed in.  Alter any params
*     where they fall outside the bounds.

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
*     DPH - 13/06/95

*   Calls:

* ----------------------- Declaration section ------------------------

       implicit none

*   Subroutine arguments
      integer Num_params               ! (INPUT) Total number of parameters
      double precision Param_values(*) ! (INPUT & OUTPUT) Parameter values to convert
      double precision Param_lower(*)  ! (INPUT & OUTPUT) Lower bound of parameter
      double precision Param_upper(*)  ! (INPUT & OUTPUT) Upper bound of parameter

*   Global variables
      include 'optimlib.inc'           ! optimlib common block

*   Constant values

*   Internal variables
      integer Param_index              ! Index into parameter values

*   Initial data values

* --------------------- Executable code section ----------------------

      do 20 Param_index = 1, Num_params
         if (g_Param_center(Param_index) .ne. 0) then
            Param_values(Param_index) = max(Param_lower(Param_index),
     .                                      Param_values(Param_index))
            Param_values(Param_index) = min(Param_upper(Param_index),
     .                                      Param_values(Param_index))
         endif
20    continue

      return
      end

* ====================================================================
       subroutine Optimise (LU_output,
     .                      Num_params, Init_param, Param_fixed,
     .                      Param_lower, Param_upper,
     .                      Num_data_points,
     .                      Tolerance, Simplex,
     .                      Final_params, Funct_value, SE,
     .                      Fit_quadratic)
* ====================================================================

*   Short description:
*     Minimise the specified user_function given the initial values
*     of the parameters and an array indicating which parameters to
*     fix.  The final calculated parameter values and the calculated
*     function value are returned to caller.
*
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
*     DPH - 13/06/95

*   Calls:

* ----------------------- Declaration section ------------------------

       implicit none

*   Subroutine arguments
      integer LU_output                ! (INPUT) Logical unit for output.
      integer Num_params               ! (INPUT) Total number of parameters
      double precision Init_param(*)   ! (INPUT) Initial parameter values
      logical Param_fixed(*)           ! (INPUT) Array indicating which parameters
                                       !         are fixed.
      double precision Param_lower(*)  ! (INPUT) Lower bound of parameter
      double precision Param_upper(*)  ! (INPUT) Upper bound of parameter
      integer Num_data_points          ! (INPUT) Number of data points
      double precision Tolerance       ! (INPUT) Tolerance value
      double precision Simplex         ! (INPUT) Simplex value
      double precision Final_params(*) ! (OUTPUT) Final calculated parameter values
      double precision Funct_value     ! (OUTPUT) Final calculated function value
      double precision SE(*)           ! (OUTPUT) Standard errors for each param.
      logical Fit_quadratic            ! (INPUT) Fit quadratic surface ?

*   Global variables
      include 'optimlib.inc'           ! optimlib common block
      external minim_funct             ! function
      integer lastnb                   ! function

*   Constant values

*   Internal variables
      double precision Step_size(Max_num_param)
                                       ! Minim step size
      integer Param_index              ! Index into parameter arrays
      integer Fail_code                ! Fail code returned by NAG
      integer IPRINT                   ! Minim print option
      integer nloop                    ! for minim
      integer iquad                    ! for minim
      integer Num_float                ! number of floating params

      double precision VAR(Max_num_param)
                                       ! Working array used by NAG
      character line*150               ! line to output
      character param_str*20           ! parameter string

*   Initial data values

* --------------------- Executable code section ----------------------

      ! Setup common block variables.

      g_num_params = Num_params
      g_num_iterations = 0
      do 10 Param_index = 1, Num_params
         g_param_lower(Param_index) = Param_lower(Param_index)
         g_param_upper(Param_index) = Param_upper(Param_index)
         g_scaled_lower(Param_index) = Param_lower(Param_index)
         g_scaled_upper(Param_index) = Param_upper(Param_index)
         g_z_lower(Param_index) = -10.0d0
         g_z_upper(Param_index) = 10.0d0
         Final_params(Param_index) = Init_param(Param_index)
10    continue

      ! Calculate center/diff values

      call Calc_center_diff
     .    (g_Num_params, g_Param_lower, g_Param_upper,
     .     g_Param_center, g_Param_diff)

      ! Calculate scaled and Z'd lower and upper bounds.

      call Param_to_scaled (g_Num_params, g_Scaled_lower)
      call Param_to_scaled (g_Num_params, g_Scaled_upper)

      ! Scale the parameters to Z values

      call Param_to_scaled (g_Num_params, Final_params)
      call Scaled_to_Z (g_Num_params, Final_params)

      Num_float = 0
      do 30 Param_index = 1, Num_params
         Step_size(Param_index) = 0.1D0 * Final_params(Param_index)
         if (Step_size(Param_index) .le. 1.0D-12) then
            Step_size(Param_index) = 0.1D0
         endif

         if (Param_fixed(Param_index)) then
            Step_size(Param_index) = 0.0D0
            Num_float = Num_float + 1
         endif
30    continue

      ! Output headings

      Line = '  iter     Fvalue'
      do 40 Param_index = 1, Num_params
         if (param_index .lt. 10) then
            write (Param_str, '(a,i1)') '       Par', Param_index
         else
            write (Param_str, '(a,i2)') '      Par', Param_index
         endif
         Line = Line(1:lastnb(line)) // Param_str
40    continue

      write (LU_output, *)
      write (LU_output, *)

      write (LU_output, *) Line

      ! Go call minim function

      Fail_code = 0
      Funct_value = 0.0d0
      iprint=0
      nloop=20
      if (Fit_quadratic) then
         iquad=1
      else
         iquad=0
      endif
      print*,"Before minim"
      call minim(LU_Output, 
     .           Final_params, Step_size, g_Num_params, Funct_value,
     .           Max_iterations, iprint, Tolerance, nloop, iquad,
     .           simplex, VAR, minim_funct, Fail_code)

      do 50 Param_index = 1, g_Num_params

         ! ????????? **tom3--for "var's" stored in w3 ==>(convert back to scaled vars)

         if (Fit_quadratic) then
            VAR(Param_index) = 2.0D0 * ABS(Final_params(Param_index)) *
     .       sqrt(2.0d0 * Funct_value * VAR(Param_index) /
     .       (Num_data_points - Num_float)) * Final_params(Param_index)
           SE(Param_index) = g_Param_diff(Param_index)*VAR(Param_index)
         else
            SE(Param_index) = 0.0d0
         endif
         
50    continue
      print*,"After minim"
      call Check_bounds (g_Num_params, Final_params,
     .                   g_z_lower, g_z_upper)
      call Z_to_scaled(g_Num_params, Final_params)
      call Scaled_to_param(g_Num_params, Final_params)

      return
      end

* ====================================================================
       subroutine Profile(LU_output,
     .                    Param_index,
     .                    Num_params, Init_param, Param_fixed,
     .                    Param_lower, Param_upper,
     .                    Num_data_points,
     .                    Tolerance, Simplex,
     .                    Num_steps,
     .                    Graph_x, Graph_y,
     .                    Quadratic_fit)
* ====================================================================

*   Short description:
*     Profiling is the process of selecting one parameter, giving
*     it a set of starting values over the allowable range,  fixing
*     all other parameters and floating just that one parameter.This process
*     should be repeated for all parameters.  This routine, though, only
*     profiles 1 parameter.  2 Graphing arrays are also passed in.  These
*     arrays are filled with the X (starting param value) and Y (Function
*     value) values to perform 1 profile plot.

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
*     DPH - 2/06/93

*   Calls:

* ----------------------- Declaration section ------------------------

       implicit none

*   Subroutine arguments
      integer LU_output                ! (INPUT) logical unit for output
      integer Param_index              ! (INPUT) Parameter number to profile
      integer Num_params               ! (INPUT) Total number of parameters
      double precision Init_param(*)   ! (INPUT) Initial parameter values
      logical Param_fixed(*)           ! (INPUT) Array indicating which parameters
                                       !         are fixed.
      double precision Param_lower(*)  ! (INPUT) Lower bound of parameter
      double precision Param_upper(*)  ! (INPUT) Upper bound of parameter
      integer Num_data_points          ! (INPUT) Number of data points
      double precision Tolerance       ! (INPUT) Tolerance value
      double precision Simplex         ! (INPUT) Simplex value
      integer Num_steps                ! (INPUT) Number of profile steps to take
      double precision Graph_x(*)      ! (OUTPUT) Profile plot - x values
      double precision Graph_y(*)      ! (OUTPUT) Profile plot - y values
      logical Quadratic_fit            ! (INPUT) Do we want to do a quadratic fit?

*   Global variables
      include 'optimlib.inc'           ! optimlib common

*   Internal variables
      integer P                        ! Our parameter index
      double precision Step_size       ! profile step size.
      integer Profile_number           ! profile number
      double precision Final_params(max_num_param)
                                       ! final parameter estimates
      double precision funct_value     ! final function value
      double precision SE(max_num_param)
                                       ! standard errors for each param.
      logical Our_fixed(Max_num_param)
                                       ! our param_fixed array
      double precision Saved_param     ! saved parameter value

*   Constant values
*      none

*   Initial data values
*      none

* --------------------- Executable code section ----------------------

      if (Param_fixed(Param_index)) then
         ! Don't profile the parameter

      else

         do 10 P = 1, Num_params
            if (Param_fixed(P)) then
               Our_fixed(P) = .true.
            else
               Our_fixed(P) = .false.
            endif
               
10       continue

         Saved_param = Init_param(Param_index)
         Our_fixed(param_index) = .true.

         Step_size = (Param_upper(Param_index) -
     .                Param_lower(Param_index)) / (Num_steps - 1)

         ! Give the current parameter a range of starting values

         do 20 Profile_number = 1, Num_steps
            Init_param(Param_index) = Param_lower(Param_index) +
     .                                (Profile_number - 1) * Step_size

            call Optimise (LU_output,
     .                     Num_params, Init_param, Our_fixed,
     .                     Param_lower, Param_upper,
     .                     Num_data_points,
     .                     Tolerance, Simplex,
     .                     Final_params, Funct_value, SE,
     .                     Quadratic_fit)

            Graph_x(Profile_number) = init_param(Param_index)
            Graph_y(Profile_number) = Funct_value
20       continue

         Init_param(Param_index) = Saved_param
         Our_fixed(Param_index) = .false.

      endif
30    continue

      return
      end


* ====================================================================
       subroutine Jackknife (LU_output,
     .                      Num_params, Init_param, Param_fixed,
     .                      Param_lower, Param_upper,
     .                      Num_data_points, Group_size,
     .                      Tolerance, Simplex,
     .                      Y,
     .                      Final_params, Funct_value, SE,
     .                      Partial_estimates, Bias_estimates, YEST,
     .                      Quadratic_fit)
* ====================================================================

*   Short description:
*     Perform the jackknife procedure by removing the specified number
*     of data points, calling Optimise, remove the next set of data
*     points, re-calling Optimise and so on.  This routine also
*     calculates the final parameter estimates and standard errors.

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
*     DPH - 2/06/93

*   Calls:

* ----------------------- Declaration section ------------------------

       implicit none

*   Subroutine arguments
      integer LU_output                ! (INPUT) Logical unit for output
      integer Num_params               ! (INPUT) Total number of parameters
      double precision Init_param(*)   ! (INPUT) Initial parameter values
      logical Param_fixed(*)           ! (INPUT) Array indicating which parameters
                                       !         are fixed.
      double precision Param_lower(*)  ! (INPUT) Lower bound of parameter
      double precision Param_upper(*)  ! (INPUT) Upper bound of parameter
      integer Num_data_points          ! (INPUT) Number of data points
      integer Group_size               ! (INPUT) Jackknife group size
      double precision Tolerance       ! (INPUT) Tolerance value
      double precision Simplex         ! (INPUT) Simplex value
      double precision Y(*)            ! (INPUT) Y estimates
      double precision Final_params(*) ! (OUTPUT) Final calculated parameter values
      double precision Funct_value     ! (OUTPUT) Final calculated function value
      double precision SE(*)           ! (OUTPUT) Standard errors for each param.
      double precision Partial_estimates(*)
                                       ! (OUTPUT) Partial estimates for each param.
      double precision Bias_estimates(*)
                                       ! (OUTPUT) Bias estimates for each param.
      double precision YEST(*)         ! (OUTPUT) Estimates Y values
      logical Quadratic_fit            ! (INPUT) Do we want to do a quadratic fit?

*   Global variables
      include 'optimlib.inc'           ! Optimlib common block

*   Internal variables
      integer Data_point               ! index into data arrays
      integer Param_index              ! Index into parameter arrays
      integer Num_groups               ! Number of jackknife groups
      double precision Overall_params(Max_num_param)
                                       ! overall parameter estimates
      double precision Jack_YEST(Max_num_data_points)
                                       ! jackknife y estimates.
      double precision Jack_se(Max_num_param)
                                       ! Jackknife se estimates
      integer i

*   Constant values
*      none

*   Initial data values
*      none

* --------------------- Executable code section ----------------------

      ! Get initial overall parameter estimates.

      g_Jack_num = 0
      call Optimise (LU_output,
     .               Num_params, Init_param, Param_fixed,
     .               Param_lower, Param_upper,
     .               Num_data_points,
     .               Tolerance, Simplex,
     .               Overall_params, Funct_value, SE, Quadratic_fit)

      ! Calculate the number of jackknife groups.

      g_Group_size = Group_size
      if (g_Group_size .eq. 1) then
         Num_groups = Num_data_points

      else
         Num_groups = (Num_data_points + 1) / g_Group_size
      endif

      ! Zero out the jack_se and partial_estimates array

      do 10 Param_index = 1, Num_params
         Jack_SE(Param_index) = 0.0d0
         Partial_estimates(Param_index) = 0.0d0
10    continue

      ! Loop through each group of data points, calling optimise once
      ! for each time through the loop.

      do 40 g_Jack_num = 1, Num_groups
         call Optimise (LU_output,
     .                  Num_params, Init_param, Param_fixed,
     .                  Param_lower, Param_upper,
     .                  Num_data_points,
     .                  Tolerance, Simplex,
     .                  Final_params, Funct_value, SE,
     .                  Quadratic_fit)

         write (LU_output_file, '(a)') 
     .       ' ----------------------------------'
         write (LU_output_file, '(a,i3)') 
     .       ' Jackknife group = ', g_Jack_num
         write (LU_output_file, '(a, i3)') 
     .       ' Number iterations = ', g_Num_iterations
         write (LU_output_file, '(a,20(3x, f12.6))') 
     .       ' Optimised parameters : ',
     .       (Final_params(i),i = 1, num_params)
         write (LU_output_file, '(a,f12.6)')
     .       ' Function value = ', Funct_value
         write (LU_output_file, '(a)') 
     .       ' ----------------------------------'
     

         ! Copy the y estimates over to the jackknife y estimates
         ! array for the current group of data points only.

         do 20 Data_point = (g_Jack_num-1) * g_Group_size + 1,
     .                       g_Jack_num * g_Group_size
            Jack_YEST(Data_point) = YEST(Data_point)
20       continue

         ! Calculate the sum of all parameter estimates for each parameter.
         ! This is used later to calculate the mean of the partial estimates.
         ! Also calculate the square of the parameter estimates.  This is
         ! used later in the standard error calculation.

         do 30 Param_index = 1, Num_params
            Partial_estimates(Param_index) =
     .         Partial_estimates(Param_index) +
     .         Final_params(Param_index)

            Jack_SE(Param_index) = Jack_SE(Param_index) +
     .         Final_params(Param_index) ** 2.0
30       continue
40    continue

      write(LU_output_file, *) 'FIRST ESTIMATES'
      write(LU_output_file, *) '---------------'
      write(LU_output_file,'(62x,a)') 'BOUNDS'
      call Output_params (LU_output_file, Num_params,
     .   Overall_params, Param_fixed, Param_lower, Param_upper)

      ! Copy all y estimates into the return array YEST

      do 50 Data_point = 1, Num_data_points
         YEST(Data_point) = Jack_YEST(Data_point)
50    continue

      ! At end of jackknife procedure.
      ! Firstly calculate the Jackknife sums of squares of residuals.

      call SSR(Num_data_points, Y, YEST, Funct_value)

      ! Loop through all parameters and calculate the partial estimates,
      ! the bias estimates and the jackknife estimates.

      do 60 Param_index = 1, Num_params
         Partial_estimates(Param_index) =
     .      Partial_estimates(Param_index) / Num_groups

         if (Param_fixed(Param_index)) then
            ! Parameter is fixed - don't change parameter

            Bias_estimates(Param_index) = 0
            Final_params(Param_index) = Init_param(Param_index)

         else

            SE(Param_index) = (Jack_SE(Param_index) - Num_groups *
     .        Partial_estimates(Param_index) ** 2.0) *
     .        (Num_groups - 1) / Num_groups

            if (SE(Param_index) .lt. 0.0) then
               SE(Param_index) = 0.0d0

            else
               SE(Param_index) = SQRT(SE(Param_index))
            endif

            Bias_estimates(Param_index) = -(Num_groups-1) *
     .        (Overall_params(Param_index) -
     .         Partial_estimates(Param_index))

            Final_params(Param_index) = Overall_params(Param_index) -
     .        Bias_estimates(Param_index)
         endif
60    continue

      return
      end

* ====================================================================
       subroutine minim_funct (minim_values, Funct_value)
* ====================================================================

*   Short description:
*     This routine is called by the optimisation routine minim.
*     It converts the parameters passed in into normal unscaled
*     parameters and calls the user supplied routine to calculate
*     the return function value.

*   Assumptions:
*     It is assumed that this routine is called from minim ONLY.
*     The minim values passed in are Z values.

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

*   Calls:

* ----------------------- Declaration section ------------------------

       implicit none

*   Subroutine arguments
      double precision minim_values(*) ! (INPUT) Parameters for this iteration
      double precision Funct_value     ! (OUTPUT) Calculated function value.

*   Global variables
      include 'optimlib.inc'           ! optlimlib common block

*   Internal variables
      double precision Our_params(max_num_param)
                                       ! our unscaled parameters.
      integer Param_index              ! current parameter index

*   Constant values
*      none

*   Initial data values
*      none

* --------------------- Executable code section ----------------------

      do 10 Param_index = 1, g_Num_params
         Our_params(Param_index) = minim_values(Param_index)
         
         ! make sure we constrain the values minim is trying to sensible
         ! values i.e. between 0 and 5

         if (g_param_center(Param_index) .ne. 0) then     
            Our_params(Param_index) = min(Our_params(Param_index), 
     .                                    5.0d0)
            Our_params(Param_index) = max(Our_params(Param_index), 
     .                                    0.0d0)
         endif

10    continue

      call Z_to_scaled (g_Num_params, Our_params)
      call Scaled_to_param (g_Num_params, Our_params)

      call Calc_funct(g_Num_params, Our_params, funct_value)

      if (mod(g_Num_iterations, 10) .eq. 0) then
         write (6,21) g_Num_iterations,Funct_value,
     .               (Our_params(Param_index),
     .         Param_index=1,g_Num_params)
21       format(1x,I6,30f11.5)
      endif

      return
      end

* ====================================================================
       subroutine Calc_funct
     .     (Num_params, Param_values, Funct_value)
* ====================================================================

*   Short description:
*     This routine calculates a function value and SSR from the normal
*     unscaled, un-z'd parameters passed in.
*
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

*   Calls:

* ----------------------- Declaration section ------------------------

       implicit none

*   Subroutine arguments
      integer Num_params               ! (INPUT) Number of parameters
      double precision Param_values(*) ! (INPUT) Parameters for this iteration
      double precision Funct_value     ! (OUTPUT) Calculated function value.

*   Global variables
      include 'optimlib.inc'           ! optimlib common block

*   Internal variables
      integer param_index              ! index into param_values array

*   Constant values
*      none

*   Initial data values
*      none

* --------------------- Executable code section ----------------------

      call User_funct(Num_params, Param_values, Funct_value)

      ! Increment the iteration count for this jackknife

      g_Num_iterations = g_Num_iterations + 1

      ! Store parameters minim has tried in Param_matrix so that 10%f
      ! values can be calculated later.  Only do this if Jack_num
      ! equals zero.

      if (g_Jack_num .eq. 0 .and.
     .    g_Num_iterations .lt. Max_iterations) then
         do 30 Param_index = 1, Num_params
            g_Param_matrix(g_Num_iterations, Param_index) =
     .          Param_values(Param_index)
30       continue

         g_Param_matrix(g_Num_iterations, Max_num_param + 1) =
     .          Funct_value

      else
         ! Don't store minim parameters
      endif

      return
      end

* ====================================================================
       subroutine SSR(Num_data_points, Y, YEST, SSR_value)
* ====================================================================

*   Short description:
*     Calculate a sums of squares of residuals from the y and
*     estimated y values.

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
      integer Num_data_points          ! (INPUT) Number of data points
      double precision Y(*)            ! (INPUT) Y values
      double precision YEST(*)         ! (INPUT) YEST values
      double precision SSR_value       ! (OUTPUT) SSR value

*   Global variables
      include 'optimlib.inc'           ! optimlib common block

*   Internal variables
      integer Data_point               ! index into y arrays

*   Constant values
*      none

*   Initial data values
*      none

* --------------------- Executable code section ----------------------

      SSR_value = 0.0D0
      do 20 Data_point = 1, Num_data_points
         if(Data_point .gt. (g_Jack_num - 1) * g_Group_size .and.
     .      Data_point .le. g_Jack_num * g_Group_size) then

            ! Don't include this point in the SSR calculation

         else
            SSR_value = SSR_value +
     .          (Y(Data_point) - YEST(Data_point)) ** 2
         endif

20    continue

      return
      end

* ====================================================================
       subroutine Output_params (Unit_number, Num_params,
     .   Param_values, Param_fixed, Lower, Upper)
* ====================================================================

*   Short description:
*     Output parameter estimates.  The Index variable passed in is
*     a flag that indicates what type of output is required.

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
*     DPH - 14/06/95

*   Calls:

* ----------------------- Declaration section ------------------------

       implicit none

*   Global variables

*   Subroutine arguments
      integer Unit_number              ! (INPUT) Unit number to write to
      integer Num_params               ! (INPUT) Number of parameters to write
      double precision Param_values(*) ! (INPUT) parameter values to write
      logical Param_fixed(*)           ! (INPUT) parameters being fixed
      double precision Lower(*)        ! (INPUT) Lower bound to print
      double precision Upper(*)        ! (INPUT) Upper bound to print

*   Internal variables
      integer Param_index              ! Index into param array
      character Fixed_key_word*9       ! Fixed key word to display

*   Constant values
*      none

*   Initial data values
*      none

* --------------------- Executable code section ----------------------

      do 20 Param_index = 1, Num_params
         if (Param_fixed(Param_index)) then
            Fixed_key_word = 'Fixed'
         else
            Fixed_key_word = 'Float'
         endif

         write(Unit_number, 10)
     .      ' param(', Param_index, ') = ',
     .      Param_values(Param_index),
     .      Fixed_key_word,
     .      '-->',
     .      Lower(Param_index), ' to ', Upper(Param_index)
10          format(a,i2,a,f12.6,a15,a10,f12.4,a,f12.4)
20    continue

      write(Unit_number, *)

      return
      end

* ====================================================================
       subroutine Range_10f (Num_params, Lower, Upper, Funct_value)
* ====================================================================

*   Short description:
*     This subroutine calculates the minimum and maximum function value
*     that is within 10% of F for each parameter.

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

*   Calls:

* ----------------------- Declaration section ------------------------

       implicit none

*   Subroutine arguments
      integer Num_params               ! (INPUT) Number of parameters
      double precision Funct_value     ! (INPUT) Function value
      double precision Lower(*)        ! (OUTPUT) Lowest param within 10% of f
      double precision Upper(*)        ! (OUTPUT) Highest param within 10% of f

*   Global variables
      include 'optimlib.inc'           ! Optimlib common block

*   Internal variables
      integer param_index              ! parameter index
      integer Iteration                ! Iteration index into Param_matrix
      logical First_time               ! First time through matrix for this param?
      logical In_range                 ! Is this iteration in range of our F value?
      double precision Lower_f         ! Lowest f value to consider
      double precision Upper_f         ! Highest f value to consider

*   Constant values
*      none

*   Initial data values
*      none

* --------------------- Executable code section ----------------------

      Lower_f = Funct_value - (0.1 * Funct_value)
      Upper_f = Funct_value + (0.1 * Funct_value)

      do 20 Param_index = 1, Num_params

         First_time = .true.
         do 10 Iteration = 1, g_Num_iterations
            In_range =
     .       (g_Param_matrix(Iteration, Max_num_param + 1) .ge. Lower_f
     .        .or.
     .        g_Param_matrix(Iteration, Max_num_param + 1) .le. Upper_f
     .       )

            if (In_range) then
               if (First_time) then
                  Lower(Param_index) =
     .               g_Param_matrix(Iteration, Param_index)
                  Upper(Param_index) =
     .               g_Param_matrix(Iteration, Param_index)
                  First_time = .false.

               else
                  Lower(param_index) = min(Lower(Param_index),
     .               g_Param_matrix(Iteration, Param_index))
                  Upper(param_index) = max(Upper(Param_index),
     .               g_Param_matrix(Iteration, Param_index))
               endif
            endif
10       continue
20    continue

      return
      end

* ====================================================================
       subroutine RSquared (Num_data_points, Y, YEST, R2)
* ====================================================================

*   Short description:
*     This routine calculates an r squared value from the Y values
*     and the Y estimates.

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
*     DPH - 14/06/95

*   Calls:

* ----------------------- Declaration section ------------------------

       implicit none

*   Subroutine arguments
      integer Num_data_points          ! (INPUT) Number of data points
      double precision Y(*)            ! (INPUT) Y values
      double precision YEST(*)         ! (INPUT) YEST values
      double precision R2              ! (OUTPUT) R squared value

*   Global variables
*      none

*   Internal variables
      integer Data_point               ! Current data point
      double precision YBAR
      double precision YESTBAR
      double precision YxYESTBAR
      double precision YSS
      double precision YESTSS
      double precision YFSS

*   Constant values
*      none

*   Initial data values
*      none

* --------------------- Executable code section ----------------------

      YBAR  = 0.0d0
      YESTBAR  = 0.0d0
      YxYESTBAR = 0.0d0
      YSS   = 0.0d0
      YESTSS   = 0.0d0
      YFSS = 0.0d0

      do 10 Data_point = 1, Num_data_points
         YBAR  = YBAR + y(Data_point)
         YESTBAR  = YESTBAR + yest(Data_point)
         YxYESTBAR = YxYESTBAR + y(Data_point)*yest(Data_point)
10    continue

      YBAR  = YBAR / float(Num_data_points)
      YESTBAR  = YESTBAR / float(Num_data_points)
      YxYESTBAR = YxYESTBAR / float(Num_data_points)

      do 20 Data_point = 1, Num_data_points
         YSS  = YSS  + (y(Data_point) - YBAR) ** 2
         YESTSS  = YESTSS  + (yest(Data_point) - YESTBAR) ** 2
         YFSS = YFSS + (y(Data_point) - YBAR) *
     .          (yest(Data_point) - YESTBAR)
20    continue

      if (YESTSS .eq. 0.0d0 .or. YSS .eq. 0.0d0) then
         R2 = 0.0d0
      else
         R2 = YFSS * YFSS / (YSS * YESTSS)
      endif

      return
      end


c
c----------------------------------------------------------------------
c
      SUBROUTINE MINIM(LU_OUT,
     1  P,STEP,NOP,FUNC,MAX,IPRINT,STOPCR,NLOOP,IQUAD,
     1  SIMP,VAR,FUNCTN,IFAULT)
C
C     A PROGRAM FOR FUNCTION MINIMIZATION USING THE SIMPLEX METHOD.
C     The minimum found will often be a local, not a global, minimum.
C
C     FOR DETAILS, SEE NELDER & MEAD, THE COMPUTER JOURNAL, JANUARY 1965
C
C     PROGRAMMED BY D.E.SHAW,
C     CSIRO, DIVISION OF MATHEMATICS & STATISTICS
C     P.O. BOX 218, LINDFIELD, N.S.W. 2070
C
C     WITH AMENDMENTS BY R.W.M.WEDDERBURN
C     ROTHAMSTED EXPERIMENTAL STATION
C     HARPENDEN, HERTFORDSHIRE, ENGLAND
C
C     Further amended by Alan Miller,
C     CSIRO, Division of Mathematics & Statistics
C     Private Bag 10, CLAYTON, VIC. 3168
C
C     Added LU_OUT to parameter list.  Done by Dean Holzworth (28/11/95)
C
C     ARGUMENTS:-
C     LU_OUT  = LOGICAL UNIT FOR OUTPUT   -- Added by Dean Holzworth (28/11/95)
C     P()     = INPUT, STARTING VALUES OF PARAMETERS
C               OUTPUT, FINAL VALUES OF PARAMETERS
C     STEP()  = INPUT, INITIAL STEP SIZES
C     NOP     = INPUT, NO. OF PARAMETERS, INCL. ANY TO BE HELD FIXED
C     FUNC    = OUTPUT, THE FUNCTION VALUE CORRESPONDING TO THE FINAL
C               PARAMETER VALUES
C     MAX     = INPUT, THE MAXIMUM NO. OF FUNCTION EVALUATIONS ALLOWED
C     IPRINT  = INPUT, PRINT CONTROL PARAMETER
C                     < 0 NO PRINTING
C                     = 0 PRINTING OF PARAMETER VALUES AND THE FUNCTION
C                         VALUE AFTER INITIAL EVIDENCE OF CONVERGENCE.
C                     > 0 AS FOR IPRINT = 0 PLUS PROGRESS REPORTS AFTER
C                         EVERY IPRINT EVALUATIONS, PLUS PRINTING FOR THE
C                         INITIAL SIMPLEX.
C     STOPCR  = INPUT, STOPPING CRITERION
C     NLOOP   = INPUT, THE STOPPING RULE IS APPLIED AFTER EVERY NLOOP
C               FUNCTION EVALUATIONS.
C     IQUAD   = INPUT, = 1 IF THE FITTING OF A QUADRATIC SURFACE IS REQUIRED
C                      = 0 IF NOT
C     SIMP    = INPUT, CRITERION FOR EXPANDING THE SIMPLEX TO OVERCOME
C               ROUNDING ERRORS BEFORE FITTING THE QUADRATIC SURFACE.
C     VAR()   = OUTPUT, CONTAINS THE DIAGONAL ELEMENTS OF THE INVERSE OF
C               THE INFORMATION MATRIX.
C     FUNCTN  = INPUT, NAME OF THE USER'S SUBROUTINE - ARGUMENTS (P,FUNC)
C               WHICH RETURNS THE FUNCTION VALUE FOR A GIVEN SET OF
C               PARAMETER VALUES IN ARRAY P.
C****   FUNCTN MUST BE DECLARED EXTERNAL IN THE CALLING PROGRAM.
C       IFAULT  = OUTPUT, = 0 FOR SUCCESSFUL TERMINATION
C                         = 1 IF MAXIMUM NO. OF FUNCTION EVALUATIONS EXCEEDED
C                         = 2 IF INFORMATION MATRIX IS NOT +VE SEMI-DEFINITE
C                         = 3 IF NOP < 1
C                         = 4 IF NLOOP < 1
C
C       Advice on usage:
C       If the function minimized can be expected to be smooth in the vicinity
C       of the minimum, users are strongly urged to use the quadratic-surface
C       fitting option.   This is the only satisfactory way of testing that the
C       minimum has been found.   The value of SIMP should be set to at least
C       1000 times the rounding error in calculating the fitted function.
C       e.g. in double precision on a micro- or mini-computer with about 16
C       decimal digit representation of floating-point numbers, the rounding
C       errors in calculating the objective function may be of the order of
C       1.E-12 say in a particular case.   A suitable value for SIMP would then
C       be 1.E-08.   However, if numerical integration is required in the
C       calculation of the objective function, it may only be accurate to say
C       1.E-05 and an appropriate value for SIMP would be about 0.1.
C       If the fitted quadratic surface is not +ve definite (and the function
C       should be smooth in the vicinity of the minimum), it probably means
C       that the search terminated prematurely and you have not found the
C       minimum.
C
C       N.B. P, STEP AND VAR (IF IQUAD = 1) MUST HAVE DIMENSION AT LEAST NOP
C            IN THE CALLING PROGRAM.
C       THE DIMENSIONS BELOW ARE FOR A MAXIMUM OF 20 PARAMETERS.
C      The dimension of BMAT should be at least NOP*(NOP+1)/2.
C
C****      N.B. This version is in DOUBLE PRECISION throughout
C
C       LATEST REVISION - 11 August 1991
C
C*****************************************************************************
C
      implicit double precision (a-h, o-z)
      external FUNCTN
      DIMENSION P(NOP),STEP(NOP),VAR(NOP)
      DIMENSION G(21,20),H(21),PBAR(20),PSTAR(20),PSTST(20),AVAL(20),
     1  BMAT(210),PMIN(20),VC(210),TEMP(20)
      DATA ZERO/0.D0/, ONE/1.D0/, TWO/2.D0/, THREE/3.D0/, HALF/0.5D0/
C
C     A = REFLECTION COEFFICIENT, B = CONTRACTION COEFFICIENT, AND
C     C = EXPANSION COEFFICIENT.
C
      DATA A,B,C/1.D0, 0.5D0, 2.D0/
C
C     SET LOUT = LOGICAL UNIT NO. FOR OUTPUT
C
      LOUT = LU_OUT
C
C     IF PROGRESS REPORTS HAVE BEEN REQUESTED, PRINT HEADING
C
      IF(IPRINT.GT.0) WRITE(LOUT,1000) IPRINT
 1000 FORMAT(' PROGRESS REPORT EVERY',I4,' FUNCTION EVALUATIONS'/,
     1  ' EVAL.  FUNC.',15X,'PARAMETER VALUES')
C
C     CHECK INPUT ARGUMENTS
C
      IFAULT=0
      IF(NOP.LE.0) IFAULT=3
      IF(NLOOP.LE.0) IFAULT=4
      IF(IFAULT.NE.0) RETURN
C
C     SET NAP = NO. OF PARAMETERS TO BE VARIED, I.E. WITH STEP.NE.0
C
      NAP=0
      LOOP=0
      IFLAG=0
      DO 10 I=1,NOP
      IF(STEP(I).NE.ZERO) NAP=NAP+1
   10 CONTINUE
C
C     IF NAP = 0 EVALUATE FUNCTION AT THE STARTING POINT AND RETURN
C
      IF(NAP.GT.0) GO TO 30
      CALL FUNCTN(P,FUNC)
      RETURN
C
C     SET UP THE INITIAL SIMPLEX
C
   30 DO 40 I=1,NOP
   40 G(1,I)=P(I)
      IROW=2
      DO 60 I=1,NOP
      IF(STEP(I).EQ.ZERO) GO TO 60
      DO 50 J=1,NOP
   50   G(IROW,J)=P(J)
      G(IROW,I)=P(I)+STEP(I)
      IROW=IROW+1
   60 CONTINUE
      NP1=NAP+1
      NEVAL=0
      DO 90 I=1,NP1
      DO 70 J=1,NOP
   70   P(J)=G(I,J)
      CALL FUNCTN(P,H(I))
      NEVAL=NEVAL+1
      IF(IPRINT.LE.0) GO TO 90
      WRITE(LOUT,1010) NEVAL,H(I),(P(J),J=1,NOP)
 1010   FORMAT(/I4, 2X, G12.5, 2X, 5G12.5, 3(/20X, 5G12.5))
   90 CONTINUE
C
C     START OF MAIN CYCLE.
C
C     FIND MAX. & MIN. VALUES FOR CURRENT SIMPLEX (HMAX & HMIN).
C
  100 LOOP=LOOP+1
      IMAX=1
      IMIN=1
      HMAX=H(1)
      HMIN=H(1)
      DO 120 I=2,NP1
      IF(H(I).LE.HMAX) GO TO 110
      IMAX=I
      HMAX=H(I)
      GO TO 120
  110   IF(H(I).GE.HMIN) GO TO 120
      IMIN=I
      HMIN=H(I)
  120 CONTINUE
C
C     FIND THE CENTROID OF THE VERTICES OTHER THAN P(IMAX)
C
      DO 130 I=1,NOP
  130 PBAR(I)=ZERO
      DO 150 I=1,NP1
      IF(I.EQ.IMAX) GO TO 150
      DO 140 J=1,NOP
  140   PBAR(J)=PBAR(J)+G(I,J)
  150 CONTINUE
      DO 160 J=1,NOP
      FNAP = NAP
  160 PBAR(J)=PBAR(J)/FNAP
C
C     REFLECT MAXIMUM THROUGH PBAR TO PSTAR,
C     HSTAR = FUNCTION VALUE AT PSTAR.
C
      DO 170 I=1,NOP
  170 PSTAR(I)=A*(PBAR(I)-G(IMAX,I))+PBAR(I)
      CALL FUNCTN(PSTAR,HSTAR)
      NEVAL=NEVAL+1
      IF(IPRINT.LE.0) GO TO 180
      IF(MOD(NEVAL,IPRINT).EQ.0) WRITE(LOUT,1010) NEVAL,HSTAR,
     1  (PSTAR(J),J=1,NOP)
C
C     IF HSTAR < HMIN, REFLECT PBAR THROUGH PSTAR,
C     HSTST = FUNCTION VALUE AT PSTST.
C
  180 IF(HSTAR.GE.HMIN) GO TO 220
      DO 190 I=1,NOP
  190 PSTST(I)=C*(PSTAR(I)-PBAR(I))+PBAR(I)
      CALL FUNCTN(PSTST,HSTST)
      NEVAL=NEVAL+1
      IF(IPRINT.LE.0) GO TO 200
      IF(MOD(NEVAL,IPRINT).EQ.0) WRITE(LOUT,1010) NEVAL,HSTST,
     1  (PSTST(J),J=1,NOP)
C
C     IF HSTST < HMIN REPLACE CURRENT MAXIMUM POINT BY PSTST AND
C     HMAX BY HSTST, THEN TEST FOR CONVERGENCE.
C
  200 IF(HSTST.GE.HMIN) GO TO 320
      DO 210 I=1,NOP
      IF(STEP(I).NE.ZERO) G(IMAX,I)=PSTST(I)
  210 CONTINUE
      H(IMAX)=HSTST
      GO TO 340
C
C     HSTAR IS NOT < HMIN.
C     TEST WHETHER IT IS < FUNCTION VALUE AT SOME POINT OTHER THAN
C     P(IMAX).   IF IT IS REPLACE P(IMAX) BY PSTAR & HMAX BY HSTAR.
C
  220 DO 230 I=1,NP1
      IF(I.EQ.IMAX) GO TO 230
      IF(HSTAR.LT.H(I)) GO TO 320
  230 CONTINUE
C
C     HSTAR > ALL FUNCTION VALUES EXCEPT POSSIBLY HMAX.
C     IF HSTAR <= HMAX, REPLACE P(IMAX) BY PSTAR & HMAX BY HSTAR.
C
      IF(HSTAR.GT.HMAX) GO TO 260
      DO 250 I=1,NOP
      IF(STEP(I).NE.ZERO) G(IMAX,I)=PSTAR(I)
  250 CONTINUE
      HMAX=HSTAR
      H(IMAX)=HSTAR
C
C     CONTRACTED STEP TO THE POINT PSTST,
C     HSTST = FUNCTION VALUE AT PSTST.
C
  260 DO 270 I=1,NOP
  270 PSTST(I)=B*G(IMAX,I) + (1.d0-B)*PBAR(I)
      CALL FUNCTN(PSTST,HSTST)
      NEVAL=NEVAL+1
      IF(IPRINT.LE.0) GO TO 280
      IF(MOD(NEVAL,IPRINT).EQ.0) WRITE(LOUT,1010) NEVAL,HSTST,
     1  (PSTST(J),J=1,NOP)
C
C     IF HSTST < HMAX REPLACE P(IMAX) BY PSTST & HMAX BY HSTST.
C
  280 IF(HSTST.GT.HMAX) GO TO 300
      DO 290 I=1,NOP
      IF(STEP(I).NE.ZERO) G(IMAX,I)=PSTST(I)
  290 CONTINUE
      H(IMAX)=HSTST
      GO TO 340
C
C     HSTST > HMAX.
C     SHRINK THE SIMPLEX BY REPLACING EACH POINT, OTHER THAN THE CURRENT
C     MINIMUM, BY A POINT MID-WAY BETWEEN ITS CURRENT POSITION AND THE
C     MINIMUM.
C
  300 DO 315 I=1,NP1
      IF(I.EQ.IMIN) GO TO 315
      DO 310 J=1,NOP
      IF(STEP(J).NE.ZERO) G(I,J)=(G(I,J)+G(IMIN,J))*HALF
       P(J)=G(I,J)
  310   CONTINUE
      CALL FUNCTN(P,H(I))
      NEVAL=NEVAL+1
      IF(IPRINT.LE.0) GO TO 315
      IF(MOD(NEVAL,IPRINT).EQ.0) WRITE(LOUT,1010) NEVAL,H(I),
     1              (P(J),J=1,NOP)
  315 CONTINUE
      GO TO 340
C
C     REPLACE MAXIMUM POINT BY PSTAR & H(IMAX) BY HSTAR.
C
  320 DO 330 I=1,NOP
      IF(STEP(I).NE.ZERO) G(IMAX,I)=PSTAR(I)
  330 CONTINUE
      H(IMAX)=HSTAR
C
C     IF LOOP = NLOOP TEST FOR CONVERGENCE, OTHERWISE REPEAT MAIN CYCLE.
C
  340 IF(LOOP.LT.NLOOP) GO TO 100
C
C     CALCULATE MEAN & STANDARD DEVIATION OF FUNCTION VALUES FOR THE
C     CURRENT SIMPLEX.
C
      HSTD=ZERO
      HMEAN=ZERO
      DO 350 I=1,NP1
  350 HMEAN=HMEAN+H(I)
      FNP1 = NP1
      HMEAN=HMEAN/FNP1
      DO 360 I=1,NP1
  360 HSTD=HSTD+(H(I)-HMEAN)**2
      HSTD=SQRT(HSTD/FLOAT(NP1))
C
C     IF THE RMS > STOPCR, SET IFLAG & LOOP TO ZERO AND GO TO THE
C     START OF THE MAIN CYCLE AGAIN.
C
      IF(HSTD.LE.STOPCR.OR.NEVAL.GT.MAX) GO TO 410
      IFLAG=0
      LOOP=0
      GO TO 100
C
C     FIND THE CENTROID OF THE CURRENT SIMPLEX AND THE FUNCTION VALUE THERE.
C
  410 DO 380 I=1,NOP
      IF(STEP(I).EQ.ZERO) GO TO 380
      P(I)=ZERO
      DO 370 J=1,NP1
  370   P(I)=P(I)+G(J,I)
      FNP1 = NP1
      P(I)=P(I)/FNP1
  380 CONTINUE
      CALL FUNCTN(P,FUNC)
      NEVAL=NEVAL+1
      IF(IPRINT.LE.0) GO TO 390
      IF(MOD(NEVAL,IPRINT).EQ.0) WRITE(LOUT,1010) NEVAL,FUNC,
     1  (P(J),J=1,NOP)
C
C     TEST WHETHER THE NO. OF FUNCTION VALUES ALLOWED, MAX, HAS BEEN
C     OVERRUN; IF SO, EXIT WITH IFAULT = 1.
C
  390 IF(NEVAL.LE.MAX) GO TO 420
      IFAULT=1
      IF(IPRINT.LT.0) RETURN
      WRITE(LOUT,1020) MAX
 1020 FORMAT(' NO. OF FUNCTION EVALUATIONS EXCEEDS',I5)
      WRITE(LOUT,1030) HSTD
 1030 FORMAT(' RMS OF FUNCTION VALUES OF LAST SIMPLEX =',G14.6)
      WRITE(LOUT,1040)(P(I),I=1,NOP)
 1040 FORMAT(' CENTROID OF LAST SIMPLEX =',4(/1X,6G13.5))
      WRITE(LOUT,1050) FUNC
 1050 FORMAT(' FUNCTION VALUE AT CENTROID =',G14.6)
      RETURN
C
C     CONVERGENCE CRITERION SATISFIED.
C     IF IFLAG = 0, SET IFLAG & SAVE HMEAN.
C     IF IFLAG = 1 & CHANGE IN HMEAN <= STOPCR THEN SEARCH IS COMPLETE.
C
  420 IF(IPRINT.LT.0) GO TO 430
      WRITE(LOUT,1060)
 1060 FORMAT(/' EVIDENCE OF CONVERGENCE')
      WRITE(LOUT,1040)(P(I),I=1,NOP)
      WRITE(LOUT,1050) FUNC
  430 IF(IFLAG.GT.0) GO TO 450
      IFLAG=1
  440 SAVEMN=HMEAN
      LOOP=0
      GO TO 100
  450 IF(ABS(SAVEMN-HMEAN).GE.STOPCR) GO TO 440
      IF(IPRINT.LT.0) GO TO 460
      WRITE(LOUT,1070) NEVAL
 1070 FORMAT(//' MINIMUM FOUND AFTER',I5,' FUNCTION EVALUATIONS')
      WRITE(LOUT,1080)(P(I),I=1,NOP)
 1080 FORMAT(' MINIMUM AT',4(/1X,6G13.6))
      WRITE(LOUT,1090) FUNC
 1090 FORMAT(' FUNCTION VALUE AT MINIMUM =',G14.6)
  460 IF(IQUAD.LE.0) RETURN
C-------------------------------------------------------------------
C
C     QUADRATIC SURFACE FITTING
C
      IF(IPRINT.GE.0) WRITE(LOUT,1110)
      write(*,1110)
 1110 FORMAT(/' QUADRATIC SURFACE FITTING ABOUT SUPPOSED MINIMUM'/)
C
C     EXPAND THE FINAL SIMPLEX, IF NECESSARY, TO OVERCOME ROUNDING
C     ERRORS.
C
      NEVAL=0
      DO 490 I=1,NP1
  470   TEST=ABS(H(I)-FUNC)
      IF(TEST.GE.SIMP) GO TO 490
      DO 480 J=1,NOP
      IF(STEP(J).NE.ZERO) G(I,J)=(G(I,J)-P(J))+G(I,J)
      PSTST(J)=G(I,J)
  480   CONTINUE
      CALL FUNCTN(PSTST,H(I))
      NEVAL=NEVAL+1
      GO TO 470
  490 CONTINUE
C
C     FUNCTION VALUES ARE CALCULATED AT AN ADDITIONAL NAP POINTS.
C
      DO 510 I=1,NAP
      I1=I+1
      DO 500 J=1,NOP
  500   PSTAR(J)=(G(1,J)+G(I1,J))*HALF
      CALL FUNCTN(PSTAR,AVAL(I))
      NEVAL=NEVAL+1
  510 CONTINUE
C
C     THE MATRIX OF ESTIMATED SECOND DERIVATIVES IS CALCULATED AND ITS
C     LOWER TRIANGLE STORED IN BMAT.
C
      A0=H(1)
      DO 540 I=1,NAP
      I1=I-1
      I2=I+1
      IF(I1.LT.1) GO TO 540
      DO 530 J=1,I1
      J1=J+1
      DO 520 K=1,NOP
  520     PSTST(K)=(G(I2,K)+G(J1,K))*HALF
      CALL FUNCTN(PSTST,HSTST)
      NEVAL=NEVAL+1
      L=I*(I-1)/2+J
      BMAT(L)=TWO*(HSTST+A0-AVAL(I)-AVAL(J))
  530   CONTINUE
  540 CONTINUE
      L=0
      DO 550 I=1,NAP
      I1=I+1
      L=L+I
      BMAT(L)=TWO*(H(I1)+A0-TWO*AVAL(I))
  550 CONTINUE
C
C     THE VECTOR OF ESTIMATED FIRST DERIVATIVES IS CALCULATED AND
C     STORED IN AVAL.
C
      DO 560 I=1,NAP
      I1=I+1
      AVAL(I)=TWO*AVAL(I)-(H(I1)+THREE*A0)*HALF
  560 CONTINUE
C
C     THE MATRIX Q OF NELDER & MEAD IS CALCULATED AND STORED IN G.
C
      DO 570 I=1,NOP
  570 PMIN(I)=G(1,I)
      DO 580 I=1,NAP
      I1=I+1
      DO 580 J=1,NOP
      G(I1,J)=G(I1,J)-G(1,J)
  580 CONTINUE
      DO 590 I=1,NAP
      I1=I+1
      DO 590 J=1,NOP
      G(I,J)=G(I1,J)
  590 CONTINUE
C
C     INVERT BMAT
C
      nn=nap*(nap+1)/2
      CALL SYMINV(BMAT,NAP,nn,BMAT,TEMP,NULLTY,IFAULT)
      IF(IFAULT.NE.0) GO TO 600
      IRANK=NAP-NULLTY
      GO TO 610
  600 continue
      if(IPRINT.GE.0) write(lout,601) NULLTY
  601 format(' RANK DEFFICIENCY OF HESSIAN MATRIX =',i3)
      IF(IPRINT.GE.0) WRITE(LOUT,1120)
      write(*,1120)
 1120 FORMAT(/' MATRIX OF ESTIMATED SECOND DERIVATIVES NOT +VE DEFN.'/
     1  ' MINIMUM PROBABLY NOT FOUND'/)
      IFAULT=2
      RETURN
C
C     BMAT*A/2 IS CALCULATED AND STORED IN H.
C
  610 DO 650 I=1,NAP
      H(I)=ZERO
      DO 640 J=1,NAP
      IF(J.GT.I) GO TO 620
      L=I*(I-1)/2+J
      GO TO 630
  620     L=J*(J-1)/2+I
  630     H(I)=H(I)+BMAT(L)*AVAL(J)
  640   CONTINUE
  650 CONTINUE
C
C     FIND THE POSITION, PMIN, & VALUE, YMIN, OF THE MINIMUM OF THE
C     QUADRATIC.
C
      YMIN=ZERO
      DO 660 I=1,NAP
  660 YMIN=YMIN+H(I)*AVAL(I)
      YMIN=A0-YMIN
      DO 670 I=1,NOP
      PSTST(I)=ZERO
      DO 670 J=1,NAP
  670 PSTST(I)=PSTST(I)+H(J)*G(J,I)
      DO 680 I=1,NOP
  680 PMIN(I)=PMIN(I)-PSTST(I)
      IF(IPRINT.LT.0) GO TO 682
      WRITE(LOUT,1130) YMIN,(PMIN(I),I=1,NOP)
 1130 FORMAT(' MINIMUM OF QUADRATIC SURFACE =',G14.6,' AT',
     1  4(/1X,6G13.5))
      WRITE(LOUT,1150)
 1150 FORMAT(' IF THIS DIFFERS BY MUCH FROM THE MINIMUM ESTIMATED',
     1  1X,'FROM THE MINIMIZATION,'/
     2  ' THE MINIMUM MAY BE FALSE &/OR THE INFORMATION MATRIX MAY BE',
     3  1X,'INACCURATE'/)
c
c     Calculate true function value at the minimum of the quadratic.
c
  682 neval = neval + 1
      call functn(pmin, hstar)
c
c     If HSTAR < FUNC, replace search minimum with quadratic minimum.
c
      if (hstar .ge. func) go to 690
      func = hstar
      do 684 i = 1, nop
  684 p(i) = pmin(i)
      write(lout, 1140) func
 1140 format(' True func. value at minimum of quadratic = ', g14.6/)
C
C     Q*BMAT*Q'/2 IS CALCULATED & ITS LOWER TRIANGLE STORED IN VC
C
  690 DO 760 I=1,NOP
      DO 730 J=1,NAP
      H(J)=ZERO
      DO 720 K=1,NAP
       IF(K.GT.J) GO TO 700
       L=J*(J-1)/2+K
       GO TO 710
  700       L=K*(K-1)/2+J
  710       H(J)=H(J)+BMAT(L)*G(K,I)*HALF
  720     CONTINUE
  730   CONTINUE
      DO 750 J=I,NOP
      L=J*(J-1)/2+I
      VC(L)=ZERO
      DO 740 K=1,NAP
  740     VC(L)=VC(L)+H(K)*G(K,J)
  750   CONTINUE
  760 CONTINUE
C
C     THE DIAGONAL ELEMENTS OF VC ARE COPIED INTO VAR.
C
      J=0
      DO 770 I=1,NOP
      J=J+I
      VAR(I)=VC(J)
  770    CONTINUE
      IF(IPRINT.LT.0) RETURN
      WRITE(LOUT,1160) IRANK
 1160 FORMAT(' RANK OF INFORMATION MATRIX =',I3/
     1  ' GENERALIZED INVERSE OF INFORMATION MATRIX:-')
      IJK=1
      GO TO 880
  790 CONTINUE
      WRITE(LOUT,1170)
 1170 FORMAT(/' IF THE FUNCTION MINIMIZED WAS -LOG(LIKELIHOOD),'/
     1  ' THIS IS THE COVARIANCE MATRIX OF THE PARAMETERS'/
     2  ' IF THE FUNCTION WAS A SUM OF SQUARES OF RESIDUALS'/
     3  ' THIS MATRIX MUST BE MULTIPLIED BY TWICE THE ESTIMATED',
     4  1X'RESIDUAL VARIANCE'/' TO OBTAIN THE COVARIANCE MATRIX.'/)
      CALL SYMINV(VC,NAP,nn,BMAT,TEMP,NULLTY,IFAULT)
C
C     BMAT NOW CONTAINS THE INFORMATION MATRIX
C
      WRITE(LOUT,1190)
 1190 FORMAT(' INFORMATION MATRIX:-'/)
      IJK=3
      GO TO 880
c
c     alculate correlations of parameter estimates, put into VC.
c
  800 IJK=2
      II=0
      IJ=0
      DO 840 I=1,NOP
      II=II+I
      IF(VC(II).LE.ZERO) GO TO 810
      VC(II)=ONE/SQRT(VC(II))
      GO TO 820
  810   VC(II)=ZERO
  820   IF(I.EQ.1) GO TO 840
      IM1=I-1
      JJ=0
      DO 830 J=1,IM1
      JJ=JJ+J
      IJ=IJ+1
      VC(IJ)=VC(IJ)*VC(II)*VC(JJ)
  830   CONTINUE
  840 IJ=IJ+1
      WRITE(LOUT,1200)
 1200 FORMAT(/' CORRELATION MATRIX:-')
      II=0
      DO 850 I=1,NOP
      II=II+I
      IF(VC(II).NE.ZERO) VC(II)=ONE
  850 CONTINUE
      GO TO 880
  860 WRITE(LOUT,1210) NEVAL
 1210 FORMAT(/' A FURTHER',I4,' FUNCTION EVALUATIONS HAVE BEEN USED'/)
      RETURN
c
c     Pseudo-subroutine to print VC if IJK = 1 or 2, or
c     BMAT if IJK = 3.
c
  880 L=1
  890 IF(L.GT.NOP) GO TO (790,860,800),IJK
      II=L*(L-1)/2
      DO 910 I=L,NOP
      I1=II+L
      II=II+I
      I2=MIN(II,I1+5)
      IF(IJK.EQ.3) GO TO 900
      WRITE(LOUT,1230)(VC(J),J=I1,I2)
      GO TO 910
  900   WRITE(LOUT,1230)(BMAT(J),J=I1,I2)
  910 CONTINUE
 1230 FORMAT(1X,6G13.5)
      WRITE(LOUT,1240)
 1240 FORMAT(/)
      L=L+6
      GO TO 890
      END
*----------
*X-Sun-Data-Type: default
*X-Sun-Data-Name: as6
*X-Sun-Content-Lines: 163
*X-Sun-Content-Length: 4398

C This file contains AS6 and the enhanced version ASR44.   See AS7 also.
C
C
      SUBROUTINE CHOL (A,N,NN,U,NULLTY,IFAULT)
C
C       Algorithm AS6, Applied Statistics, vol.17, (1968)
C
C       Given a symmetric matrix order n as lower triangle in a( )
C       calculates an upper triangle, u( ), such that uprime * u = a.
C       a must be positive semi-definite.  eta is set to multiplying
C       factor determining effective zero for pivot.
C
C       arguments:-
C       a()     = input, a +ve definite matrix stored in lower-triangula
C                 form.
C       n       = input, the order of a
C       nn      = input, the size of the a and u arrays      n*(n+1)/2
C       u()     = output, a lower triangular matrix such that u*u' = a.
C                 a & u may occupy the same locations.
C       nullty  = output, the rank deficiency of a.
C       ifault  = output, error indicator
C                       = 1 if n < 1
C                       = 2 if a is not +ve semi-definite
C                       = 3 if nn < n*(n+1)/2
C                       = 0 otherwise
C
C***********************************************************************
C
      DOUBLE PRECISION A(NN),U(NN),ETA,ETA2,X,W,ZERO
C
C       The value of eta will depend on the word-length of the
C       computer being used.  See introductory text.
C
      DATA ETA,ZERO/1.D-5,0.0D0/
C
      IFAULT=1
      IF (N.LE.0) RETURN
      IFAULT=3
      IF (NN.LT.N*(N+1)/2) RETURN
      IFAULT=2
      NULLTY=0
      J=1
      K=0
      ETA2=ETA*ETA
      II=0
C
C       Factorize column by column, icol = column no.
C
      DO 80 ICOL=1,N
      II=II+ICOL
      X=ETA2*A(II)
      L=0
      KK=0
C
C       IROW = row number within column ICOL
C
      DO 40 IROW=1,ICOL
      KK=KK+IROW
      K=K+1
       W=A(K)
      M=J
      DO 10 I=1,IROW
       L=L+1
       IF (I.EQ.IROW) GO TO 20
       W=W-U(L)*U(M)
       M=M+1
 10       CONTINUE
 20       IF (IROW.EQ.ICOL) GO TO 50
      IF (U(L).EQ.ZERO) GO TO 30
      U(K)=W/U(L)
      GO TO 40
 30       IF (W*W.GT.ABS(X*A(KK))) RETURN
      U(K)=ZERO
 40     CONTINUE
 50     IF (ABS(W).LE.ABS(ETA*A(K))) GO TO 60
      IF (W.LT.ZERO) RETURN
      U(K)=SQRT(W)
      GO TO 70
 60     U(K)=ZERO
      NULLTY=NULLTY+1
 70     J=J+ICOL
 80   CONTINUE
      IFAULT=0
      END

C
C
C
C
*----------
*X-Sun-Data-Type: default
*X-Sun-Data-Name: as7
*X-Sun-Content-Lines: 185
*X-Sun-Content-Length: 5821

c This file contains AS7 and an enhanced alternative - ASR44.  See also AS6.
c
c
      subroutine syminv(a, n, nn, c, w, nullty, ifault)
c
c       Algorithm AS7, Applied Statistics, vol.17, 1968, p.198.
c
c       Forms in c( ) as lower triangle, a generalised inverse
c       of the positive semi-definite symmetric matrix a( )
c       order n, stored as lower triangle.
c
c       arguments:-
c       a()     = input, the symmetric matrix to be inverted, stored in
c                 lower triangular form
c       n       = input, order of the matrix
c       nn      = input, the size of the a and c arrays     n*(n+1)/2
c       c()     = output, the inverse of a (a generalized inverse if c is
c                 singular), also stored in lower triangular.
c                 c and a may occupy the same locations.
c       w()     = workspace, dimension at least n.
c       nullty  = output, the rank deficiency of a.
c       ifault  = output, error indicator
c                       = 1 if n < 1
c                       = 2 if a is not +ve semi-definite
c                       = 3 if nn < n*(n+1)/2
c                       = 0 otherwise
c
c***************************************************************************
c
      double precision a(nn), c(nn), w(n), x, zero, one
c
      data zero, one /0.0d0, 1.0d0/
c
c       cholesky factorization of a, result in c
c
      call chol(a, n, nn, c, nullty, ifault)
      if(ifault.ne.0) return
c
c       invert c & form the product (cinv)'*cinv, where cinv is the inverse
c       of c, row by row starting with the last row.
c       irow = the row number, ndiag = location of last element in the row.
c
      irow=n
      ndiag=nn
   10   l=ndiag
      if (c(ndiag) .eq. zero) goto 60
      do 20 i=irow,n
      w(i)=c(l)
      l=l+i
   20    continue
      icol=n
      jcol=nn
      mdiag=nn
   30   l=jcol
      x=zero
      if(icol.eq.irow) x=one/w(irow)
      k=n
   40   if(k.eq.irow) go to 50
      x=x-w(k)*c(l)
      k=k-1
      l=l-1
      if(l.gt.mdiag) l=l-k+1
      go to 40
   50   c(l)=x/w(irow)
      if(icol.eq.irow) go to 80
      mdiag=mdiag-icol
      icol=icol-1
      jcol=jcol-1
      go to 30
   60   do 70 j=irow,n
       c(l)=zero
      l=l+j
   70   continue
   80   ndiag=ndiag-irow
      irow=irow-1
      if(irow.ne.0) go to 10
      return
      end
c
c
c
c

