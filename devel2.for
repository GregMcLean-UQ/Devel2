* ====================================================================
       program devel2
* ====================================================================
*   Short description:
*     main program for devel version 2.0

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
*     GMcL - 29/3/2022  Add 4th parameter to 3_hour_broken_linear

*   Calls:
*

* ----------------------- Declaration section ------------------------

       implicit none

*   Subroutine arguments
*      none

*   Global variables
      include 'devel2.inc'             ! devel2 common block
      logical Convert_to_days          ! function

*   Internal variables
      integer Obs                      ! current observation
      double precision Funct_value     ! function value
      double precision Final_values(max_num_param)
                                       ! final optimised parameters
      double precision SE(max_num_param)
                                       ! Standard errors of each parameter
      logical Err                      ! has an error occurred?
      integer Param_index              ! parameter index
      integer Step_index               ! current step index (profile stuff)
      integer Start_profile_param      ! starting parameter to profile
      integer End_profile_param        ! ending parameter to profile      

*   Constant values

*   Initial data values
*     none

*  --------------------- Executable code section ----------------------

!      open(unit=5, file='c:\bin\bean.par', status='old')

      call unlink('AvRateAvPP.out')
	   call unlink('AvRateAvTemp.out')
      call unlink('PredObsDays.out')
	   call unlink('Res_avPP.out')
	   call unlink('Res_avTemp.out')
      write(LU_Output_file, *) '    DEVEL 2 Phenology Optimisation'
      write(LU_Output_file, *) '    ------------------------------'
      write(LU_Output_file, *)

      ! Get all DEVEL 2 parameters from parameter file

      call Devel_read_params ()
      call Devel_output_init_table ()

      ! Multiply the tolerance and simplex values by the number of data points
      
      g_Tolerance = g_Tolerance * g_Num_obs
      g_Simplex = g_Simplex * g_Num_obs

      ! Setup the y values.

      do 10 Obs = 1, g_Num_obs
         if (g_optim_type .eq. RATE_OPTIM) then
            g_Y(Obs) = 1.0d0
         else
            g_Y(Obs) = g_met_end(Obs) - g_met_start(Obs)    ! DAS
            
         endif
10    continue

      if (g_optim_type .eq. RATE_OPTIM) then
         call output_rate_graphs ()
      endif
      
      call output_header('Initial parameters')

      if (g_run_type .eq. NORMAL_RUN) then

         ! Do an initial 1 iteration and output results.

         call Initial_function_value
     .       (g_Num_params, g_param_values, g_param_fixed,
     .        g_Param_lower, g_param_upper, g_Num_Obs,
     .        Funct_value)
         call output_header('Optimising...')

         ! Perform optimisation
         call Optimise (LU_output_file,
     .        g_Num_params, g_Param_values, g_param_fixed,
     .        g_Param_lower, g_param_upper, g_Num_Obs,
     .        g_Tolerance, g_Simplex,
     .        final_values, funct_value, SE, g_Quadratic_fit)

      else if (g_Run_type .eq. JACKKNIFE_RUN) then
         call Jackknife (LU_output_file,
     .        g_Num_params, g_Param_values, g_param_fixed,
     .        g_Param_lower, g_param_upper, g_Num_Obs, g_group_size,
     .        g_Tolerance, g_Simplex, g_Y, 
     .        final_values, funct_value, SE,
     .        g_Partial_est, g_Bias_est, g_YEST, g_Quadratic_fit)

         write(LU_output_file, *) 'PARTIAL ESTIMATES'
         write(LU_output_file, *) '-----------------'
         write(LU_output_file,'(62x,a)') 'BOUNDS'
         call Output_params (LU_output_file, g_num_params,
     .        g_Partial_est, g_param_fixed,
     .        g_Param_lower, g_Param_upper)

         write(LU_output_file, *) 'BIAS ESTIMATES'
         write(LU_output_file, *) '-----------------'
         write(LU_output_file,'(62x,a)') 'BOUNDS'
         call Output_params (LU_output_file, g_num_params,
     .        g_Bias_est, g_param_fixed,
     .        g_Param_lower, g_Param_upper)

         ! Calculate and print the +- 2 std err range

!         do 15 param_index = 1, Num_params
!            g_2SE_Lower(param_index) = Final_values(param_index)
!     .            - 2.0d0 * SE(param_index)
!            g_2SE_Upper(param_index) = Final_values(param_index)
!     .            + 2.0d0 * SE(param_index)
!15       continue
!         write(LU_output_file, *) 'JACKKNIFE ESTIMATES'
!         write(LU_output_file, *) '-----------------'
!         write(LU_output_file,'(62x,a)') '+/- 2 std errs'
!         call Output_params (LU_output_file, g_Num_params,
!     .        Final_values, g_param_fixed,
!     .        g_2SE_Lower, g_2SE_Upper)

      else if (g_Run_type .eq. PROFILE_RUN) then
         
         ! Loop through all parameters and profile each one.
         
         if (g_profile_param .eq. 0) then
            Start_profile_param = 1
            End_profile_param = g_Num_params
         else
            Start_profile_param = g_profile_param
            End_profile_param = g_profile_param
         endif 

         do 30 Param_index = Start_profile_param, End_profile_param

            if (.not. g_param_fixed(Param_index)) then
               write(6, *) 'Profiling parameter : ', Param_index
               write(6, *)

               call Profile (LU_output_file,
     .              Param_index,
     .              g_Num_params, g_Param_values, g_param_fixed,
     .              g_Param_lower, g_param_upper, g_Num_Obs,
     .              g_Tolerance, g_Simplex, 10,
     .              g_Graph_x, g_Graph_y, g_Quadratic_fit)
     
               write(LU_Output_file, '(//a,i2/)' ) 
     .            ' Data for profile plot for parameter : ', 
     .            Param_index
               write(LU_output_file, '(a)' ) 
     .            ' Step       Param value            FValue'
               do 20 Step_index = 1, 10
                  write(LU_output_file, '(1x,i4,2(2x,f16.5))' ) 
     .               Step_index, g_Graph_x(Step_index), 
     .               g_Graph_y(Step_index)
20             continue

               write(LU_Output_file,'(//a,i2)' )
     .            '           Profile plot for parameter : ', 
     .            Param_index 
	 
	           call dataOut(g_Graph_X,g_Graph_Y,10,'parameter.out')

!               call Gr_DataPlot(g_Graph_X, g_Graph_Y, 10, '*')
!               call Gr_Print(LU_Output_file)
            endif
30       continue
      endif

      call output_header('Final output')
      call output_residual_graphs ()

      ! Convert Y and YEST arrays into actual day numbers instead
      ! of 0 to 1 scale.

      if (g_optim_type .eq. RATE_OPTIM) then
        Err = Convert_to_days(final_values, g_Y, g_YEST)
      endif
      
      ! Write out output

      call Output_final_params (g_Y, g_YEST,
     .     g_Num_params, g_param_fixed,
     .     g_Num_Obs,
     .     final_values, funct_value, SE, .not. Err)

      call Devel_output_final_table ()

      call output_pred_obs_graphs ()

      close(LU_input_file)
      
      stop
      end

* ====================================================================
       subroutine Initial_function_value
     .    (Num_params, Init_param, param_fixed,
     .     Param_lower, param_upper, Num_data_points,
     .     Funct_value)
* ====================================================================

*   Short description:
*     Perform 1 iteration through system with initial parameters.

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
      integer Num_params               ! (INPUT) Total number of parameters
      double precision Init_param(*)   ! (INPUT) Initial parameter values
      logical Param_fixed(*)           ! (INPUT) Array indicating which parameters
                                       !         are fixed.
      double precision Param_lower(*)  ! (INPUT) Lower bound of parameter
      double precision Param_upper(*)  ! (INPUT) Upper bound of parameter
      integer Num_data_points          ! (INPUT) Number of data points
      double precision Funct_value     ! (OUTPUT) Final calculated function value

*   Global variables
      include 'devel2.inc'             ! devel2 common block

*   Internal variables
*      none

*   Constant values
*      none

*   Initial data values
*      none

* --------------------- Executable code section ----------------------

      ! output number of data points

      write(LU_output_file, *)
      write(LU_output_file, *)
      write(LU_output_file, *)
     .   ' Number of data points = ', Num_data_points

      ! Write out stuff to output file.

      write(LU_output_file, *)
      write(LU_output_file, *)
      write(LU_output_file, *) ' INITIAL PARAMETER ESTIMATES:'
      write(LU_output_file, '(61x,a)') '  BOUNDS'
      call Output_params (LU_output_file, Num_params,
     .                    Init_param, param_fixed,
     .                    param_lower, param_upper)

      ! Calculate initial function value and output it.

      call Calc_funct (Num_params, Init_param,
     .                 funct_value)

      write(LU_output_file, *) 'Initial SSR = ', funct_value

      return
      end

* ====================================================================
       subroutine output_final_params (Y, YEST,
     .     Num_params, param_fixed,
     .     Num_data_points,
     .     final_values, funct_value, SE, Calc_rsquared)
* ====================================================================

*   Short description:
*     Output all data to output file.  If Calc_rsquared = .true.
*     then an rsquared value will be calculated and displayed.


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
      integer Num_params               ! (INPUT) Total number of parameters
      logical Param_fixed(*)           ! (INPUT) Array indicating which parameters
                                       !         are fixed.
      integer Num_data_points          ! (INPUT) Number of data points
      double precision Final_values(*) ! (INPUT) Final calculated parameter values
      double precision Funct_value     ! (INPUT) Final calculated function value
      double precision SE(*)           ! (INPUT) Standard errors for each param.
      logical calc_rsquared            ! (INPUT) calculate an rsquared value

*   Global variables
      include 'devel2.inc'             ! devel2 common

*   Internal variables
      integer param_index              ! parameter index
      double precision Lower(max_num_param)
                                       ! lower bound of each parameter
      double precision Upper(Max_num_param)
                                       ! upper bound of each parameter
      double precision R2              ! calculated r squared.
      double precision Y(*)            ! actual y values
      double precision YEST(*)         ! estimated y values

*   Constant values
*      none

*   Initial data values
*      none

* --------------------- Executable code section ----------------------

      write(LU_output_file, *)
      write(LU_output_file, *)
      write(LU_output_file, *) 'Final SSR = ', funct_value
      write(LU_output_file, *)
      write(LU_output_file, *) 'OVERALL ESTIMATES'
      write(LU_output_file, *) '-----------------'

      ! Calculate and print the 10%f range.

      call Range_10f(Num_params, Lower, Upper, funct_value)
      write(LU_output_file,'(62x,a)') '10%f Range'
      call Output_params (LU_output_file, num_params,
     .     Final_values, param_fixed,
     .     Lower, Upper)

      ! Calculate and print the +- 2 std err range if SE's were predicted.

      if (SE(1) .gt. 0.0d0) then
      
         do 10 param_index = 1, Num_params
            Lower(param_index) = Final_values(param_index)
     .            - 2.0d0 * SE(param_index)
            Upper(param_index) = Final_values(param_index)
     .            + 2.0d0 * SE(param_index)
10       continue
         write(LU_output_file,'(62x,a)') '+/- 2 std errs'
         call Output_params (LU_output_file, num_params,
     .        Final_values, param_fixed,
     .        Lower, Upper)
      endif

      ! Calculate and print the R squared value.

      if (Calc_rsquared) then
         call RSquared (Num_data_points, Y, YEST, R2)
         write(LU_output_file, *) 'R-Squared = ', R2
         write(LU_output_file, *)
      endif

      return
      end


* ====================================================================
       subroutine Devel_output_init_table ()
* ====================================================================

*   Short description:
*     Output the inital data table.

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
*      none

*   Global variables
      include 'devel2.inc'             ! Devel common block
      double precision Devel2_calc_rate ! function

*   Internal variables
      integer Exper_index              ! experiment index
      integer met_index                ! met index
      integer Obs                      ! Current observation
      real av_temp                     ! average temperature for obs
      real av_pp                       ! average photoperiod for obs
      integer Num_days                 ! number of days in period
      double precision pstage          ! pstage for obs

*   Constant values
*      none

*   Initial data values
*      none

* --------------------- Executable code section ----------------------

      call output_header('Observed data')

      if (g_optim_type .eq. RATE_OPTIM) then
         write(LU_output_file, *)
     .      'obs     ndays    avrate    avtemp      avpp  acc_rate',
     .      '  Experiment  Treatment'
      else
         write(LU_output_file, *)
     .      'obs     ndays      avtemp      avpp  ',
     .      '  Experiment  Treatment'
         
      endif
         
      do 50 Exper_index = 1, g_Num_experiments
         do 40 Obs = g_obs_start_row(Exper_index),
     .               g_obs_end_row(Exper_index)
            if (Obs .ne. 0) then
               av_temp = 0.0
               av_pp = 0.0
               pstage = 0.0d0
               Num_days = g_met_end(obs) - g_met_start(obs) + 1
      
               do 10 Met_index = g_met_start(Obs), g_met_end(Obs)
                  av_temp = av_temp +
     .               (g_MaxT(Met_index) + g_MinT(Met_index)) / 2.0
                  av_pp = av_pp + g_pp(Met_index)
                  if (g_optim_type .eq. RATE_OPTIM) then
                     pstage = pstage +  Devel2_calc_rate
     .                                 (Obs, Met_index, g_Param_values)
                  endif
10             continue
   
               av_temp = av_temp / Num_days
               av_pp = av_pp / Num_days
      
               if (g_optim_type .eq. RATE_OPTIM) then
                  write(LU_Output_file, 20) Obs, Num_days,1.0/Num_days,
     .                  Av_temp, Av_pp, pstage,
     .                  g_Experiment_name(Exper_index),
     .                  g_Obs_name(Obs)
20                format(i4,5x,i5,2x,f8.5,2(4x,f6.2),2x,f8.5,2(2x,a10))
               else
                  write(LU_Output_file, 30) Obs, Num_days, 
     .                  Av_temp, Av_pp,
     .                  g_Experiment_name(Exper_index),
     .                  g_Obs_name(Obs)
30                format(i4,5x,i5,2x,2(4x,f6.2),2x,2(2x,a10))
               endif
            endif          
40       continue
50    continue
         
      return
      end

* ====================================================================
       subroutine Devel_output_final_table ()
* ====================================================================

*   Short description:
*     Output the final data table.

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
*      none

*   Global variables
      include 'devel2.inc'             ! Devel common block

*   Internal variables
      integer Exper_index              ! experiment index
      integer Obs                      ! Current observation
      integer Num_days                 ! number of days in period
	  integer est
	  integer tErr

*   Constant values
*      none

*   Initial data values
*      none

* --------------------- Executable code section ----------------------

      write(LU_output_file, *)
     .   'obs    Pred. days       Obs. Days   True Err   Start Day',
     .   '  Experiment  Treatment'
      
      do 40 Exper_index = 1, g_Num_experiments
         do 30 Obs = g_obs_start_row(Exper_index),
     .               g_obs_end_row(Exper_index)
            if (Obs .ne. 0) then

               Num_days = g_met_end(obs) - g_met_start(obs) + 1
               est = g_YEST(Obs)
			   tErr = g_YEST(Obs) - Num_days
               write(LU_Output_file, 20) Obs, est, Num_days,
     .            tErr,
     .            g_day(g_met_start(Obs)),
     .            g_Experiment_name(Exper_index),
     .            g_Obs_name(Obs), Obs
   
20             format(i4,7x,i3,14x,i3,9x,i3,5x,i4,6x,a10,2x,a10,2x,i3)
            endif
30       continue
40    continue

      return
      end


* ====================================================================
       subroutine output_rate_graphs ()
* ====================================================================

*   Short description:
*     output the rate vs average temperature and rate vs average
*     photoperiod graphs.

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
*      none

*   Global variables
      include 'devel2.inc'             ! Devel2 common block

*   Internal variables
      integer Obs                      ! Observation number
      integer Met_index                ! index into met arrays
      integer Num_days                 ! Number of days

*   Constant values
*      none

*   Initial data values
*      none

* --------------------- Executable code section ----------------------

      do 20 Obs = 1, g_Num_obs
         Num_days = g_met_end(Obs) - g_met_start(Obs) + 1
         g_Graph_Y(Obs) = 1.0d0 / Num_days

         g_Graph_X(Obs) = 0.0
         do 10 Met_index = g_met_start(Obs), g_met_end(Obs)
            g_Graph_X(Obs) = g_Graph_X(Obs) +
     .         (g_MaxT(Met_index) + g_MinT(Met_index)) / 2.0
10       continue

         g_Graph_X(Obs) = g_Graph_X(Obs) / Num_days

20    continue

      write(LU_Output_file,'(//a)')
     .   '           Average rate vs average temperature'
	 	           
	  call dataOut(g_Graph_X,g_Graph_Y,g_Num_obs,'AvRateAvTemp.out')

!      call Gr_DataPlot(g_Graph_X, g_Graph_Y, g_Num_obs, ' ')
!      call Gr_Print(LU_Output_file)

      do 40 Obs = 1, g_Num_obs
         Num_days = g_met_end(Obs) - g_met_start(Obs) + 1

         g_Graph_X(Obs) = 0.0
         do 30 Met_index = g_met_start(Obs), g_met_end(Obs)
            g_Graph_X(Obs) = g_Graph_X(Obs) + g_pp(Met_index)
30       continue

         g_Graph_X(Obs) = g_Graph_X(Obs) / Num_days

40    continue

      write(LU_Output_file,'(//a)')
     .   '           Average rate vs average photoperiod'
	 	  
      call dataOut(g_Graph_X,g_Graph_Y,g_Num_obs,'AvRateAvPP.out')

!      call Gr_DataPlot(g_Graph_X, g_Graph_Y, g_Num_obs, ' ')
!      call Gr_Print(LU_Output_file)

      return
      end

* ====================================================================
       subroutine output_residual_graphs ()
* ====================================================================

*   Short description:
*     output the residual vs average temperature and residual vs average
*     photoperiod graphs.

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
*      none

*   Global variables
      include 'devel2.inc'             ! Devel2 common block

*   Internal variables
      integer Obs                      ! Observation number
      integer Met_index                ! index into met arrays
      integer Num_days                 ! Number of days
	  

*   Constant values
*      none

*   Initial data values
*      none

* --------------------- Executable code section ----------------------

      do 20 Obs = 1, g_Num_obs
         Num_days = g_met_end(Obs) - g_met_start(Obs) + 1
         g_Graph_Y(Obs) = (g_Y(Obs) - g_YEST(Obs)) / g_Y(Obs)
            
         g_Graph_X(Obs) = 0.0
         do 10 Met_index = g_met_start(Obs), g_met_end(Obs)
            g_Graph_X(Obs) = g_Graph_X(Obs) +
     .         (g_MaxT(Met_index) + g_MinT(Met_index)) / 2.0
10       continue

         g_Graph_X(Obs) = g_Graph_X(Obs) / Num_days

20    continue

      write(LU_Output_file,'(//a)')
     .   '           Residual vs average temperature'

!       call Gr_DataPlot(g_Graph_X, g_Graph_Y, g_Num_obs, ' ')
      
	  call dataOut(g_Graph_X,g_Graph_Y,g_Num_obs,'Res_avTemp.out')
	   
	   
!       call Gr_Print(LU_Output_file)
     

      do 40 Obs = 1, g_Num_obs
         Num_days = g_met_end(Obs) - g_met_start(Obs) + 1

         g_Graph_X(Obs) = 0.0
         do 30 Met_index = g_met_start(Obs), g_met_end(Obs)
            g_Graph_X(Obs) = g_Graph_X(Obs) + g_pp(Met_index)
30       continue

         g_Graph_X(Obs) = g_Graph_X(Obs) / Num_days

40    continue

      write(LU_Output_file,'(//a)')
     .   '           Residual vs average photoperiod'
!      call Gr_DataPlot(g_Graph_X, g_Graph_Y, g_Num_obs, ' ')
	  call dataOut(g_Graph_X,g_Graph_Y,g_Num_obs,'Res_avPP.out')
!      call Gr_Print(LU_Output_file)

      return
      end

* ====================================================================
	  subroutine dataOut(X,Y,nobs,fName)
* ====================================================================
      double precision X(*)            ! (INPUT) x values
      double precision Y(*)            ! (INPUT) y values
      integer nobs                     ! (INPUT) number of observations
	 
	  character(len=*) fName

	  	  
	  
	  open(unit=25, file=fName, status='replace')

	  
	  
	  do 57 i = 1,nobs
	     write(25,*)X(i),',',Y(i)
57    continue
      close(25)
      return
      end

* ====================================================================
       subroutine output_pred_obs_graphs ()
* ====================================================================

*   Short description:
*     output the predicted vs observed graph

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
*      none

*   Global variables
      include 'devel2.inc'             ! Devel2 common block

*   Internal variables

*   Constant values
*      none

*   Initial data values
*      none

* --------------------- Executable code section ----------------------

      write(LU_Output_file,'(//a)')
     .   '           Predicted vs observed number of days'
!      call Gr_DataPlot(g_Y, g_YEST, g_Num_obs, ' ')
	  call dataOut(g_Y,g_YEST,g_Num_obs,'PredObsDays.out')
!      call Gr_Print(LU_Output_file)

      return
      end

* ====================================================================
       logical function Convert_to_days (Param_values, Y, YEST)
* ====================================================================

*   Short description:
*     Convert the Y and YEST arrays into day numbers.  Return
*     .true. if an error occurred.

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
      double precision Param_values(*) ! (INPUT) parameter values to use in calculation
      double precision Y(*)            ! (OUTPUT) Y values to modify
      double precision YEST(*)         ! (OUTPUT) Estimated Y values to change

*   Global variables
      include 'devel2.inc'             ! DEVEL 2 common block
      double precision Devel2_calc_rate ! function

*   Internal variables
      double precision Acc_rate_of_devel
                                       ! Accumulated rate of development
      logical Err                      ! Has an error occurred?
      integer Num_days                 ! Number of days so far
      integer Obs                      ! Observation number
      integer Met_index                ! current met index.
      integer Met_end_index            ! Ending index of this met file
      integer Met_file_indx            ! Index into met file arrays

*   Constant values
*      none

*   Initial data values
*      none

* --------------------- Executable code section ----------------------

      Err = .false.

      do 20 Obs = 1, g_Num_obs
         Acc_rate_of_devel = 0.0
         Num_days = 0
         Met_index = g_met_start(Obs)

         ! Look through the met arrays for the met_index.  When found
         ! we will know the met file for this observation and also
         ! how much met data we have.

         Met_file_indx = 1
5        continue
         if (Met_index .ge. g_met_start_index(Met_file_indx) .and.
     .       Met_index .le. g_met_end_index(Met_file_indx)) then
            Met_end_index = g_met_end_index(Met_file_indx)
         else
            Met_file_indx = Met_file_indx + 1
            goto 5
         endif
                       
10       continue
         if (Met_index .gt. Met_end_index) then
            write(LU_Output_file, *)
            write(LU_Output_file, *) '??????????????????????????????'
            write(LU_Output_file, *) '?       DEVEL2 WARNING       ?'
            write(LU_Output_file, *) '??????????????????????????????'
            write(LU_Output_file, *)
     .         'Insufficient met. data for observation : ', Obs
            write(LU_Output_file, *)
     .         'This occurred while trying to calculate an R squared'
            write(LU_Output_file, *)
     .         'value.'
            write(LU_Output_file, *)
     .         'This error can be caused either by a loose model fit'
            write(LU_Output_file, *)
     .         'or insufficient met. data past the end of the'
            write(LU_Output_file, *)
     .         'phenological period being modelled.'
            write(LU_Output_file, *) '??????????????????????????????'
            write(LU_Output_file, *)
            write(LU_Output_file, *)
            Err = .true.
            goto 100
         else
            if (Acc_rate_of_devel .lt. 1.0d0) then
               Acc_rate_of_devel = Acc_rate_of_devel +
     .         Devel2_calc_rate (Obs, Met_index, Param_values)
               Num_days = Num_days + 1
               Met_index = Met_index + 1
               goto 10

            else if (Acc_rate_of_devel .ge. 1.0d0) then
               YEST(Obs) = Num_days
               Y(Obs) = g_Met_end(Obs) - g_Met_start(Obs)
            endif
         endif
20    continue
100   continue

      Convert_to_days = Err

      return
      end

* ====================================================================
       character*(*) function Read_param
     .     (Section, Key_name, Key_name_optional)
* ====================================================================

*   Short description:
*     Read in a parameter from the input file in the specified section that has
*     a keyword matching the specified key_name passed in.  Echo the
*     line to the output file.

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
*     DPH 15/06/95

*   Calls:
*     None

* ----------------------- Declaration section ------------------------

       implicit none

*   Subroutine arguments
      character section*(*)            ! (INPUT) section name to search for
      character key_name*(*)           ! (INPUT) key name to search for
      logical Key_name_optional        ! (INPUT) If .true. then no error is displayed

*   Global variables
      include 'devel2.inc'             ! devel2 common
      character Get_file_string*300    ! function

*   Internal variables
      character Line*300               ! line read from file

*   Constant values
*      none

*   Initial data values
*      none

* --------------------- Executable code section ----------------------

      Line = Get_file_string(LU_input_file, Section, Key_name,
     .                       Key_name_optional)

      Read_param = Line

      return
      end

* ====================================================================
       subroutine Error (msg)
* ====================================================================

*   Short description:
*     Display an error message and quit program.

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
      character msg*(*)                ! (INPUT) Error message to display

*   Global variables
      integer lastnb                   ! function

*   Internal variables
*      none

*   Constant values
*      none

*   Initial data values
*      none

* --------------------- Executable code section ----------------------

      write(6, *) '?????????????????????????????????????'
      write(6, *) '?  An error has occurred in DEVEL2  ?'
      write(6, *) '?????????????????????????????????????'
      write(6, *)
      write(6, '(1x,a)') msg(1:lastnb(msg))
      write(6, *)
      write(6, *) '?????????????????????????????????????'
      stop
      end

* ====================================================================
       subroutine output_header(Header_name)
* ====================================================================

*   Short description:
*     Send a header line to the output file.

*   Assumptions:
*      None

*   Notes:
*     e.g header line :-    ==== Input parameters ===================

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
      character Header_name*(*)        ! (INPUT) name of header.

*   Global variables
      include 'devel2.inc'             ! devel2 common
      integer lastnb                   ! function

*   Internal variables
      character Line*80                ! Line we're going to output
      integer i

*   Constant values
*      none

*   Initial data values
*      none

* --------------------- Executable code section ----------------------

      write(Line, '(a4, a)') '====', Header_name
      do 10 i = Lastnb(line) + 1, 80
         Line(i:i) = '='
10    continue

      write(LU_output_file, *)
      write(LU_output_file, *) Line
      write(LU_output_file, *)

      return
      end

* ====================================================================
       subroutine Copy_input_to_output ()
* ====================================================================

*   Short description:
*     Copy the input file contents to the output file.

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

*   Global variables
      include 'devel2.inc'             ! Devel2 common block
      integer lastnb                   ! function

*   Internal variables
      integer Err_code                 ! IOSTAT error code
      character line*400               ! line read from file

*   Constant values
*      none

*   Initial data values
*      none

* --------------------- Executable code section ----------------------

      ! open temporary file
      
      open (unit=LU_input_file, file='devel2.tmp', status='unknown')
      rewind(LU_input_file)

      call Output_header('Input parameters')

10    continue
      read(5, '(a)', iostat=Err_code) line
      write(LU_input_file, '(a)') Line
      if (Err_code .eq. 0) then
         if (line .eq. ' ') then
            write(lu_output_file, *)
         else
            write(lu_output_file, '(1x,a)') line(1:lastnb(line))
         endif
         goto 10
      endif

      ! rewind temporary file ready for reading.
      
      rewind(LU_input_file)
      
      return
      end

* ====================================================================
       subroutine Devel_read_params ()
* ====================================================================

*   Short description:
*     Read in parameters from parameter file and x/y data from data file.

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
*     DPH - 15/06/95

*   Calls:

* ----------------------- Declaration section ------------------------

       implicit none

*   Subroutine arguments

*   Global variables
      include 'devel2.inc'             ! DEVEL common block
      character Read_param*300         ! function
      character No_spaces*300          ! function

*   Internal variables
      integer Err_code                 ! error code of open stmt
      character msg*300                ! Error message to display
      character line*300               ! line read from file
      character key_word*30            ! key word to search for
      integer numvals                  ! number of values converted

*   Constant values

*   Initial data values
*     none

* --------------------- Executable code section ----------------------

      ! Copy input file contents to output file

      call Copy_input_to_output ()

      ! Setup all devel common blocks

      g_Num_experiments = 0
      g_Num_obs = 0
      g_Num_funct = 0
      g_Num_met_files = 0

      ! get tolerance

      Line = Read_param('devel', 'tolerance', Not_optional)
      call string_to_double_var (Line, g_Tolerance, numvals)

      ! get simplex

      Line = Read_param('devel', 'simplex', Not_optional)
      call string_to_double_var (Line, g_Simplex, numvals)

      ! get quadratic_fit

      Line = Read_param('devel', 'quadratic_fit', Not_optional)
      call string_to_logical_var (Line, g_Quadratic_fit, numvals)

      ! Find out if this is a normal optimisation run, a profile
      ! run or a jackknife run.

      Line = Read_param('devel', 'run_type', Not_optional)
      if (Line .eq. 'normal') then
         g_run_type = NORMAL_RUN

      else if (index(Line, 'profile') .eq. 1) then
         g_run_type = PROFILE_RUN
         if (Line(8:) .eq. ' ') then
            g_Profile_param = 0
         else
            call string_to_integer_var(Line(8:), 
     .           g_Profile_param, numvals)
         endif
            

      else if (Line. eq. 'jackknife') then
         g_run_type = JACKKNIFE_RUN

         ! Because this is a jackknife run we need to get group_size

         Line = Read_param('devel', 'group_size', Not_optional)
         call string_to_integer_var (Line, g_Group_size, numvals)

      else
         msg = 'Run_type should be one of (normal, profile, jackknife)'
         call Error(msg)
      endif

      ! Determine if we're dealing with a rate optimisation or
      ! a thermal time/photoperiod optimisation.
      
      Line = Read_param('devel', 'objective_model', Not_optional)
      if (Line .eq. 'rate') then
         g_optim_type = RATE_OPTIM

      else if (Line .eq. 'tt_target') then
         g_optim_type = TTPP_OPTIM

      else
         msg = 'Optimisation_type should be one of (rate, tt vs pp)'
         call Error(msg)
      endif

      ! Get name of plant data file from parameter file.

      Line = Read_param('devel', 'observed_file', Not_optional)

      open(unit=LU_Plant_file, file=Line, status='old',
     .     iostat=Err_code)

      if (Err_code .eq. 0) then
         ! File exists - continue

      else

         msg = 'Cannot find the observated file : ' // Line
         call Error (msg)
      endif

      ! Read in start and end column number

      Line = Read_param('devel', 'observed_file_start_column',
     .                  Not_optional)
      call string_to_integer_var(Line, Start_col, numvals)
      if (Start_col .lt. 0 .or.
     .    Start_col .gt. Max_plant_columns) then
         msg = 'Bad observed_file_start_column : ' // Line
         call Error (msg)
      endif

      Line = Read_param('devel', 'observed_file_end_column',
     .                  Not_optional)
      call string_to_integer_var(Line, End_col, numvals)
      if (End_col .le. Start_col .or.
     .    End_col .gt. max_plant_columns) then
         msg = 'Bad observed_file_end_column : ' // Line
         call Error (msg)
      endif

      ! Loop through each experiment and call lower level routine
      ! to read in data for that treatment.

10    continue

      ! Make sure we dont have too many experiments

      g_Num_experiments = g_Num_experiments + 1
      if (g_Num_experiments .gt. Max_num_experiments) then
         write(msg, '(a,i3)')
     .      'Too many experiments have been defined.  Maximum = ',
     .      Max_num_experiments
         call Error(msg)
      endif

      write(Key_word, '(a,i3)') 'experiment', g_Num_experiments
      Key_word = No_spaces(Key_word)
      Line = Read_param('devel', Key_word, Optional)
      if (Line .ne. ' ') then
         ! Found an experiment - go get all data for that experiment

         call Read_experiment (Line)
         goto 10

      else
         ! No more experiments to go - exit

         g_Num_experiments = g_Num_experiments - 1
      endif

      ! go read in all observations

      call Read_obs()

      close(lu_plant_file)

      return
      end

* ====================================================================
       subroutine Read_experiment (Experiment_name)
* ====================================================================

*   Short description:
*     Read in all data for the specified experiment

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
      character Experiment_name*(*)    ! (INPUT) name of experiment to process

*   Global variables
      include 'devel2.inc'             ! DEVEL common block
      character Read_param*300         ! function
      integer Store_met_file           ! function
      character No_spaces*300          ! function

*   Internal variables
      character line*300               ! line read from file
      integer Current_treatment        ! current treatment number
      character key_word*30            ! key word to search for

*   Constant values
*      none

*   Initial data values
*      none

* --------------------- Executable code section ----------------------

      ! Get met file.

      Line = Read_param(Experiment_name, 'met_file', Not_optional)

      ! Store met file

      g_Met_file_number(g_Num_experiments) =  Store_met_file(Line)

      ! Store experiment name

      g_Experiment_name(g_Num_experiments) = Experiment_name

      ! Loop through each treatment and call a lower level routine to
      ! get the parameters for each treatment

      Current_treatment = 1
      g_OBS_start_row(g_Num_experiments) = 0
      g_OBS_end_row(g_Num_experiments) = 0
      g_TRT_start_row(g_Num_experiments) = 0
      g_TRT_end_row(g_Num_experiments) = 0

10    continue
      write(Key_word, '(a,i3)') 'treatment', Current_treatment
      Key_word = No_spaces(Key_word)
      Line = Read_param(Experiment_name, Key_word, Optional)
      if (Line .ne. ' ') then
         ! Found a treatment - go get all data for that treatment

         call Store_treatment (Line,
     .       g_TRT_start_row(g_Num_experiments),
     .       g_TRT_end_row(g_Num_experiments))
         Current_treatment = Current_treatment + 1
         goto 10

      else
         ! No more experiments to go - exit

      endif

      return
      end

* ====================================================================
       subroutine Store_treatment(Line, Trt_start, Trt_end)
* ====================================================================

*   Short description:
*     Store the specified treatment in the treatment arrays
*     Return the start and end index into the treatment array

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
      character Line*(*)               ! (INPUT) Treatment line
      integer Trt_start                ! (INPUT & OUTPUT) Start index of treatments
      integer Trt_end                  ! (INPUT & OUTPUT) End index of treatments

*   Global variables
      include 'devel2.inc'             ! Devel2 common
      integer Store_funct              ! function

*   Internal variables
      character Treatment_options(4)*(30)
                                       ! Current treatment options
      character msg*300                ! Error message to display
      integer numvals                  ! number of values returned

*   Constant values
*      none

*   Initial data values
*      none

* --------------------- Executable code section ----------------------

      ! Break line up into the various treatment options.

      call String_to_char_array(Line, Treatment_options, 4, numvals)

      ! If there are not 4 options then issue error.

      if (numvals .ne. 4) then
         msg =
     .      'The treatment line must have 4 words on it. Treatment = '
     .    // Line
         call Error(msg)

      endif

      ! make sure we haven't got too many treatments

      g_Num_trts = g_Num_trts + 1
      if (G_Num_trts .gt. max_num_treatments) then
         write(msg, '(a, i4)')
     .      ' Too many treatments read from input file.  Maximum = ',
     .      Max_num_treatments
         call Error(msg)
      endif

      ! Store each of the functions for this treatment

      g_trt_name(g_Num_trts) = Treatment_options(1)
      g_trt_ropt(g_Num_trts) = Store_funct(Treatment_options(2))
      g_trt_temp(g_Num_trts) = Store_funct(Treatment_options(3))
      g_trt_pp(g_Num_trts) = Store_funct(Treatment_options(4))
      
      if (Trt_start .eq. 0) then
         Trt_start = g_Num_trts
      endif
      Trt_end = g_Num_trts

      return
      end

* ====================================================================
       subroutine Read_OBS ()
* ====================================================================

*   Short description:
*     Read in all observation data from plant file.  When a match
*     is found store the observation and update the various indexes
*     for the experiment.

*   Assumptions:
*      None

*   Notes:
*     The line parameter is taken directly from the parameter file
*     e.g.  '622x430 ROPT QUADTEMP NOPP'
*     i.e.  It is the bit after the 'treatment1=' keyword

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

*   Global variables
      include 'devel2.inc'             ! DEVEL common block

*   Internal variables
      integer numcols                  ! number of column in plant file
      integer numvals                  ! number of values returned
      character msg*300                ! Error message to display
      character Plant_row(max_plant_columns)*(20)
                                       ! 1 row read from plant file
      integer year                     ! Year for current observation
      integer start_day                ! start day for current observation
      integer end_day                  ! end day for current observation
      integer IOStatus                 ! IOSTAT error code
      integer Sow_day                  ! sowing day
      integer Experiment_index         ! index into experiment arrays
      integer Treatment_index          ! index into treatment arrays
      character Line*300               ! line read from file

*   Constant values

*   Initial data values
*      none

* --------------------- Executable code section ----------------------

      ! Loop through each observation in observation file and try
      ! to find a match for the current experiment and treatment name.
      ! When a match is found, store the data in the obs tables.

      rewind(LU_plant_file)
      read(LU_plant_file, *)
      read(LU_plant_file, *)
      numcols = 0

10    continue
      call Read_line(LU_plant_file, Line, IOStatus)
      if (IOStatus .eq. 0) then
         call String_to_char_array(Line, Plant_row,
     .        max_plant_columns, numvals)
         if (numcols .eq. 0) numcols = Numvals
         if (numcols .ne. numvals) then
            msg = 'Missing column of data in plant file.  Line = '
     .            // Line
            call Error(msg)
         endif

         ! Try and match the current observation.

         call Search_experiments (Plant_row(1), Plant_row(2),
     .        Experiment_index, Treatment_index)
         if (Experiment_index .gt. 0) then

            ! Got a match.  Get the met. indexes for this observation

            if (Start_col .lt. 4 .or. Start_col .gt. numcols) then
               msg = 'Bad plant_file_start_column number.  Does not'
     .            // 'match up with plant observation file.'
               call Error(msg)
            endif

            if (End_col .lt. 5 .or. End_col .gt. numcols) then
               msg = 'Bad plant_file_end_column number.  Does not'
     .            // 'match up with plant observation file.'
               call Error(msg)
            endif

            call String_to_integer_var(Plant_row(3), year, numvals)
            call String_to_integer_var(Plant_row(4), sow_day, numvals)
            call String_to_integer_var
     .           (Plant_row(Start_col), start_day, numvals)
            call String_to_integer_var
     .           (Plant_row(End_col), end_day, numvals)

            if (year .ne. -1 .and. sow_day .ne. -1 .and.
     .          start_day .ne. -1 .and. end_day .ne. -1) then
               ! No missing data - check validity of startcol

               g_Num_obs = g_Num_obs + 1
               if (g_Num_obs .gt. Max_num_obs) then
                  write(msg, '(2a,i5)')
     .               'Too many observations read from plant file.'
     .               ,'Maximum = ', Max_num_obs
                  call Error(msg)
               endif

               if (g_OBS_start_row(Experiment_index) .eq. 0) then
                  g_OBS_start_row(Experiment_index) = g_Num_obs
               endif
               g_OBS_end_row(Experiment_index) = g_Num_obs

               ! convert start_col and end_col to day number rather
               ! than days after sowing.

               if (start_col .ne. 4) then
                  start_day = sow_day + start_day
               endif
               end_day = sow_day + end_day

               ! Get met indexes for these dates

               call Get_met_indexes(
     .              g_Met_file_number(Experiment_index),
     .              year,
     .              start_day, end_day,
     .              g_met_start(g_Num_obs),
     .              g_met_end(g_Num_obs))
               
               ! If this is an ttpp optimisation run then we need
               ! to store the end of the met record in g_met_end
               ! rather than the met index of the end day in the
               ! plant file.
               
               g_met_end_data(g_Num_obs) = 
     .            g_met_end_index(g_Met_file_number(Experiment_index))

               ! Store the observation name

               g_Obs_name(g_Num_obs) = Plant_row(2)
               g_Ropt_funct(g_Num_obs) = g_trt_ropt(Treatment_index)
               g_Temp_funct(g_Num_obs) = g_trt_temp(Treatment_index)
               g_PP_funct(g_Num_obs) = g_trt_pp(Treatment_index)
            endif
         endif
         goto 10
      else
         ! End of plant file - exit

      endif

      return
      end

* ====================================================================
       subroutine Search_experiments (Exp_name, Trt_name,
     .        Experiment_index, Treatment_index)
* ====================================================================

*   Short description:
*     Search the experiemnt names and treatment names for the
*     exp_name and trt_name and return the indexes into the
*     arrays if found.  Return 0 as indexes if not found.

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
      character Exp_name*(*)           ! (INPUT) Experiment name to search for
      character Trt_name*(*)           ! (INPUT) Treatment name to search for
      integer Experiment_index         ! (OUTPUT) Index into experiment arrays
      integer Treatment_index          ! (OUTPUT) Index into treatment arrays

*   Global variables
      include 'devel2.inc'             ! devel2 common block
      integer Find_string_in_array     ! function

*   Internal variables
      logical Found                    ! Found the treatment yet ?

*   Constant values
*      none

*   Initial data values
*      none

* --------------------- Executable code section ----------------------

      Experiment_index = Find_string_in_array(Exp_name,
     .                     g_Experiment_name, g_Num_experiments)

      if (Experiment_index .ne. -1) then
         ! Found the experiment.  Go search through the treatments
         ! for this experiment

         Treatment_index = g_Trt_start_row(Experiment_index)
10       continue
         if (Treatment_index .le. g_Trt_end_row(Experiment_index)) then
            Found = (Trt_name .eq. g_trt_name(Treatment_index))
            if (.not. Found) then
               Treatment_index = Treatment_index + 1
               goto 10
            endif
         endif

      else
         Found = .false.
      endif

      if (.not. Found) then
         Treatment_index = 0
         Experiment_index = 0
      endif

      return
      end

* ====================================================================
       subroutine Get_met_indexes(Met_file_index,
     .                            start_year, start_day, end_day,
     .                            met_start, met_end)
* ====================================================================

*   Short description:
*     For the specified met_file return the start and end indexes
*     for the specified start and end date.

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
      integer met_file_index           ! (INPUT) met file index
      integer start_year               ! (INPUT) start year
      integer start_day                ! (INPUT) start day
      integer end_day                  ! (INPUT) end day
      integer met_start                ! (OUTPUT) met start index
      integer met_end                  ! (OUTPUT) met end index

*   Global variables
      include 'devel2.inc'             ! devel2 common
      double precision Date_to_jday    ! function
      logical leap_year                ! function

*   Internal variables
      integer Date(3)                  ! day, month, year
      integer Start_index              ! starting metdata index for this met file
      integer End_year                 ! year for end_day
      double precision Start_jul_day   ! starting met date
      double precision Start_day_jul_day
                                       ! start_day as a julian day
      double precision End_day_jul_day ! end_day as a julian day
      character msg*300                ! Error message to display
      integer Days_in_year             ! number of days in year

*   Constant values
*      none

*   Initial data values
*      none

* --------------------- Executable code section ----------------------

      Start_index = g_met_start_index(Met_file_index)

      ! Calculate the julian day number for the first day of met data

      call day_of_year_to_date (g_day(Start_index),
     .                          g_year(Start_index),
     .                          Date)
      Start_jul_day = Date_to_jday (Date(1), Date(2), Date(3))

      ! Calculate the julian day number for the start_day

      call day_of_year_to_date (Start_day,
     .                          Start_year,
     .                          Date)
      Start_day_jul_day = Date_to_jday (Date(1), Date(2), Date(3))

      ! Calculate the julian day number for the end_day

      if (Leap_year(Start_year)) then
         Days_in_year = 366
      else
         Days_in_year = 365
      endif

      if (End_day .gt. Days_in_year) then
         End_year = Start_year + 1
         End_day = End_day - Days_in_year
      else
         End_year = Start_year
      endif

      call day_of_year_to_date (End_day,
     .                          End_year,
     .                          Date)
      End_day_jul_day = Date_to_jday (Date(1), Date(2), Date(3))

      Met_start = Start_index + Start_day_jul_day - Start_jul_day
      Met_end = Start_index + End_day_jul_day - Start_jul_day

      ! need to check that the 2 indexes point to where they're
      ! supposed to.  Otherwise there's an error in the met file.

      if (g_year(Met_start) .eq. Start_year .and.
     .    g_day(Met_start) .eq. Start_day .and.
     .    g_year(Met_end) .eq. End_year .and.
     .    g_day(Met_end) .eq. End_day) then
         ! Good all ok
      else
         write(msg, '(2a)')
     .     'There are days missing/inserted in met file : ',
     .     g_Met_file_name(Met_file_index)
         call Error(msg)
      endif

      ! When accumulating rates of development in the user function
      ! make sure we start the day after the phenological date.

      Met_start = Met_start + 1

      return
      end

* ====================================================================
       integer function Store_funct(Funct_name)
* ====================================================================

*   Short description:
*     Store the function name in the function tables.  Return it's
*     index to caller.

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
      character Funct_name*(*)         ! (INPUT) name of function to store

*   Global variables
      include 'devel2.inc'             ! DEVEL common block
      character Read_param*300         ! function
      integer Find_string_in_array     ! function
      integer Store_param              ! function

*   Internal variables
      character line*300               ! line read from file
      character msg*300                ! Error message to display
      integer Num_params_to_read       ! number of parameters to read in
      integer Param_index              ! current parameter number
      integer Return_index             ! Index to return to caller
      integer Indx                     ! index value

*   Constant values
*      none

*   Initial data values
*      none

* --------------------- Executable code section ----------------------

      ! Let's see if we have already loaded this function group.
      ! If so then simply return it's index to caller.

      Return_index = Find_string_in_array
     .    (Funct_name, g_Funct_name, g_Num_funct)

      if (Return_index .eq. -1) then
         ! Cannot find function group.  Load it in.

         Line = Read_param(Funct_name, 'function', Not_optional)

         g_Num_funct = g_Num_funct + 1
         if (g_Num_funct .gt. Max_num_funct) then
            write(msg, '(2a,i3)')
     .      'Too many parameter function groups have been specified.',
     .      '  Maximum = ', Max_num_funct
            call Error(msg)
         endif

         g_funct_name(g_Num_funct) = Funct_name

         if (line .eq. 'single_value') then
            g_Funct_response(g_Num_funct) = 0
            Num_params_to_read = 1

         else if (Line .eq. 'linear') then
            g_Funct_response(g_Num_funct) = 1
            Num_params_to_read = 1

         else if (line .eq. 'quadratic') then
            g_Funct_response(g_Num_funct) = 2
            Num_params_to_read = 2

         else if (line .eq. 'power') then
            g_Funct_response(g_Num_funct) = 3
            Num_params_to_read = 2
            
         else if (line .eq. 'exponential') then
            g_Funct_response(g_Num_funct) = 4
            Num_params_to_read = 2

         else if (line .eq. 'sigmoid') then
            g_Funct_response(g_Num_funct) = 5
            Num_params_to_read = 3

         else if (line .eq. 'broken_linear') then
            g_Funct_response(g_Num_funct) = 6
            Num_params_to_read = 3

         else if (line .eq. 'triple_broken_linear') then
            g_Funct_response(g_Num_funct) = 7
            Num_params_to_read = 3

         else if (line .eq. '3_hour_broken_linear') then
            g_Funct_response(g_Num_funct) = 8
            
!            if (g_optim_type .eq. RATE_OPTIM) then
               Num_params_to_read = 4
!            else
!               Num_params_to_read = 3
!            endif
            

         else if (line .eq. 'curvilinear') then
            g_Funct_response(g_Num_funct) = 9
            Num_params_to_read = 3

         else if (line .eq. 'none') then
            g_Funct_response(g_Num_funct) = -1
            Num_params_to_read = 0

         else
            msg = 'Invalid function type specified : ' // Line
            call Error(msg)
         endif

         ! Read in all parameters

         do 10 Param_index = 1, Num_params_to_read
            Line = Read_param
     .         (Funct_name, char(97+Param_index-1), Not_optional)
            Indx = Store_param (Line)
            if (Param_index .eq. 1) then
               g_param_start_index(g_Num_funct) = Indx
            endif
10       continue
         Return_index = g_Num_funct
      endif

      Store_funct = Return_index

      return
      end

* ====================================================================
       integer function Store_param (Param_line)
* ====================================================================

*   Short description:
*     Store the specified parameter line in the parameter tables.
*     Return an index to it's position.

*   Assumptions:
*      None

*   Notes:
*     param_line is the line as specified in the parameter file.
*     e.g. 5.0 fixed 0.0 100.0
*     i.e. The bit to the right of the equals sign.

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
      character Param_line*(*)         ! (INPUT) Parameter line

*   Global variables
      include 'devel2.inc'             ! devel2 common block

*   Internal variables
      integer Numvals                  ! number of values on param_line
      character Param_options(4)*(30)  ! broken up parameter line
      character msg*300                ! Error message to display

*   Constant values
*      none

*   Initial data values
*      none

* --------------------- Executable code section ----------------------

      ! Break param_line up into the various parameter options.

      call String_to_char_array(Param_line, Param_options, 4, numvals)
      if (numvals .ne. 2 .and. numvals .ne. 4) then
         msg = 'Expected either 2 or 4 values on parameter line : '
     .         // Param_line
         call Error(msg)
      endif

      g_Num_params = g_Num_params + 1
      if (g_Num_params .gt. max_num_param) then
         write(msg, '(a,i3)')
     .    'Too many parameters have been specified.  Maximum = ',
     .    max_num_param
         call Error(msg)
      endif

      call String_to_double_var(Param_options(1),
     .                          g_param_values(g_Num_params),
     .                          numvals)

      if (param_options(2) .eq. 'fixed') then
         g_param_fixed(g_Num_params) = .true.

      else if (param_options(2) .eq. 'float') then
         g_param_fixed(g_Num_params) = .false.

      else
         msg = 'Expected either fixed or float.  Value = ' //
     .         Param_options(2)
         call Error(msg)
      endif

      if (.not. g_param_fixed(g_num_params)) then
         call String_to_double_var(Param_options(3),
     .                             g_param_lower(g_Num_params),
     .                             numvals)

         call String_to_double_var(Param_options(4),
     .                             g_param_upper(g_Num_params),
     .                             numvals)

         if (g_param_lower(g_Num_params) .ge. 
     .       g_param_upper(g_Num_params)) then
            msg = 'The lower bound of a parameter is above the upper '
     .            // 'bound.  Parameter line = ' // Param_line
     
            call Error(msg)
            
         else if (g_param_values(g_Num_params) .le. 
     .            g_param_lower(g_Num_params) .or.
     .            g_param_values(g_Num_params) .ge.
     .            g_param_upper(g_Num_params)) then
            msg = 'The parameter value is not in range.  ' //
     .            'Parameter line = ' // Param_line
            call Error(msg)
         endif
         
      endif

      Store_param = g_Num_params

      return
      end

* ====================================================================
       integer function Store_met_file(File_name)
* ====================================================================

*   Short description:
*     Store the contents of the specified met file into the met tables.
*     Return the index of the met file to caller.

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
      character File_name*(*)          ! (INPUT) met file name

*   Global variables
      include 'devel2.inc'             ! devel common block
      integer Find_string_in_array     ! function

*   Internal variables
      integer Err_code                 ! error code of open
      character msg*300                ! Error message to display
      integer Return_index             ! Index to return to caller
      character line*300               ! line read from met file
      character met_row(5)*(10)        ! broken up line from met file
      integer numvals                  ! number of values

*   Constant values
      integer LU_met_file              ! unit number of met file
      parameter (LU_met_File=30)

*   Initial data values
*      none

* --------------------- Executable code section ----------------------

      ! Let's see if we have already loaded this met file.
      ! If so then simply return it's index to caller.

      Return_index = Find_string_in_array
     .    (File_name, g_met_file_name, g_Num_met_files)

      if (Return_index .eq. -1) then
         ! haven't loaded met file - go load it now.

         open(unit=lu_met_file, file=File_name, status='old',
     .       iostat = Err_code)

         if (Err_code .ne. 0) then
            msg = 'Cannot open met file : ' // File_name
            call Error(msg)
         endif

         g_Num_met_files = g_Num_met_files + 1
         if (g_Num_met_files .gt. max_num_met_files) then
            write(msg, '(a,i3)')
     .        'Too many met files loaded.  Maximum = ',
     .        max_num_met_files
            call Error(msg)
         endif

         ! Skip past the 2 heading lines.

         read(lu_met_file, *)
         read(lu_met_file, *)

         g_met_file_name(g_Num_met_files) = File_name
         g_met_start_index(g_Num_met_files) = g_Num_met_data + 1

         ! Go read in all data.

10       continue
         call Read_line(LU_met_file, Line, Err_code)
         if (Err_code .eq. 0) then
            g_Num_met_data = g_Num_met_data + 1
            if (g_Num_met_data .gt. max_num_met_data) then
               write(msg, '(2a,i5)')
     .         'Too much met data has been loaded. ',
     .         'Maximum number of lines : ',
     .         max_num_met_data
               call Error(msg)
            endif

            call String_to_char_array(Line, met_row, 5, numvals)
            if (Numvals .ne. 5) then
               msg = 'Bad line found in met file : ' // File_name
     .         // '.  Line = ' // Line
               call Error(msg)
            endif

            call string_to_integer_var(met_row(1),
     .             g_year(g_num_met_data), numvals)
            call string_to_integer_var(met_row(2),
     .             g_day(g_num_met_data), numvals)
            call string_to_real_var(met_row(3),
     .             g_maxt(g_num_met_data), numvals)
            call string_to_real_var(met_row(4),
     .             g_mint(g_num_met_data), numvals)
            call string_to_real_var(met_row(5),
     .             g_pp(g_num_met_data), numvals)

            goto 10
         endif
         close (LU_met_file)

         g_met_end_index(g_Num_met_files) = g_num_met_data
         Return_index = g_Num_met_files

      endif

      Store_met_file = Return_index

      return
      end

* ====================================================================
       subroutine User_funct (Num_params, Param_values, Funct_value)
* ====================================================================

*   Short description:
*     User supplied function.  This routine takes the parameters passed
*     in and returns a function value.  In this routine an SSR is
*     returned as the function value

*   Assumptions:
*      None

*   Notes:
*     This routine should be modified by the user for their specific
*     set of equations.

*   Procedure attributes:
*      Version:         Any hardware/Fortran77
*      Extensions:      Long names <= 20 chars.
*                       Lowercase
*                       Underscore
*                       Inline comments
*                       Include
*                       implicit none

*   Changes:
*     DPH - 12/01/93

*   Calls:

* ----------------------- Declaration section ------------------------

       implicit none

*   Global variables
      include 'devel2.inc'             ! DEVEL common block

*   Subroutine arguments
      integer Num_params               ! (INPUT) Number of parameters
      double precision Param_values(Max_num_param)
                                       ! (INPUT) Current parameter values.
      double precision Funct_value     ! (OUTPUT) Calculated function value

*   Internal variables

*   Constant values
*     none

*   Initial data values
*      none

* --------------------- Executable code section ----------------------

      Num_params = Num_params       ! Get rid of compiler warning

      if (g_optim_type .eq. RATE_OPTIM) then
         call Rate(Param_values, Funct_value)
         
      else
         call ttpp(Param_values, Funct_value)
      endif
      
      return
      end
      
* ====================================================================
       subroutine Rate (Param_values, Funct_value)
* ====================================================================

*   Short description:
*     User supplied function.  This routine takes the parameters passed
*     in and returns a function value.  In this routine an SSR is
*     returned as the function value.  The routine uses rates of
*     development to calculate the function value.

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
*     DPH - 12/01/93

*   Calls:

* ----------------------- Declaration section ------------------------

       implicit none

*   Global variables
      include 'devel2.inc'             ! DEVEL common block
      double precision Devel2_calc_rate
                                       ! function

*   Subroutine arguments
      double precision Param_values(Max_num_param)
                                       ! (INPUT) Current parameter values.
      double precision Funct_value     ! (OUTPUT) Calculated function value

*   Internal variables
      double precision Acc_rate_of_devel
                                       ! Accumulated rate of devel. for period
      integer Met_index                ! Index for met data arrays.
      integer Obs                      ! Index for plant observation arrays

*   Constant values
*     none

*   Initial data values
*      none

* --------------------- Executable code section ----------------------

      do 20 Obs = 1, g_Num_obs
         Acc_rate_of_devel = 0.0

          do 10 Met_index = g_met_start(Obs),
     .                      g_met_end(Obs)
            Acc_rate_of_devel = Acc_rate_of_devel +
     .         Devel2_calc_rate (Obs, Met_index, Param_values)

10       continue
         g_YEST(Obs) = Acc_rate_of_devel
20    continue

      call SSR(g_Num_obs, g_Y, g_YEST, Funct_value)
      return
      end

* ====================================================================
       subroutine Devel_SSR(Num_data_points, Y, YEST, SSR_value)
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
     .          ((Y(Data_point) - YEST(Data_point)) / Y(Data_point))**2
         endif

20    continue

      return
      end

* ====================================================================
       subroutine ttpp (Param_values, Funct_value)
* ====================================================================

*   Short description:
*     User supplied function.  This routine takes the parameters passed
*     in and returns a function value.  In this routine an SSR is
*     returned as the function value.  The routine uses thermal times
*     and photoperiods to calculate the function value.

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
*     DPH - 12/01/93

*   Calls:

* ----------------------- Declaration section ------------------------

       implicit none

*   Global variables
      include 'devel2.inc'             ! DEVEL common block
      double precision Response        ! function

*   Subroutine arguments
      double precision Param_values(Max_num_param)
                                       ! (INPUT) Current parameter values.
      double precision Funct_value     ! (OUTPUT) Calculated function value

*   Internal variables
      double precision Acc_tt          ! Accumulated thermal time (oC day)
      double precision TT              ! Calculated thermal time (oC day)
      double precision TT_target       ! Calculated target thermal time (oC day)
      integer Met_index                ! Index for met data arrays.
      integer Obs                      ! Index for plant observation arrays
      integer Start_index              ! starting index for temp. params
      double precision ropt            ! ropt 

*   Constant values
*     none

*   Initial data values
*      none

* --------------------- Executable code section ----------------------

      do 30 Obs = 1, g_Num_obs
         Acc_tt = 0.0d0
         Start_index = g_param_start_index(g_temp_funct(obs))

         do 10 Met_index = g_met_start(Obs),
     .                     g_met_end_data(Obs)
            
            Start_index = g_param_start_index(g_temp_funct(obs))

            TT = Response(g_funct_response(g_temp_funct(Obs)),
     .                    g_maxt(Met_index),
     .                    g_mint(Met_index),
     .                    Param_values,
     .                    Start_index)
     
            ! Because all responses are 0 to 1 modifiers we need to 
            ! multiply TT by the difference between the optimum and 
            ! minimum temperatures.
            
            ropt = Param_values(Start_index + 1) - 
     .             Param_values(Start_index)
            TT = TT * ropt
            
            ! Accumulate thermal time.
            
            Acc_tt = Acc_tt + TT
             
            ! calculate target thermal time from the current photoperiod
             
            ropt = Param_values(g_param_start_index
     .                         (g_ropt_funct(obs)))
            TT_target = Response(g_funct_response(g_pp_funct(Obs)),
     .                  g_pp(Met_index),
     .                  g_pp(Met_index),
     .                  Param_values,
     .                  g_param_start_index(g_pp_funct(obs)))
            TT_target = TT_target * ropt

            ! Are we at target yet?
            
            if (Acc_tt .ge. TT_target) then
               goto 20
            endif

10       continue
20       continue
         g_YEST(Obs) = Met_index - g_met_start(obs)   ! DAS
30    continue

      call Devel_SSR(g_Num_obs, g_Y, g_YEST, Funct_value)
      return
      end

* ====================================================================
       double precision function Devel2_calc_rate
     .     (Obs, Met_index, Param_values)
* ====================================================================

*   Short description:
*     Calculate a rate of development for a single day for a
*     single observation.

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
*     DPH - 25/02/93

*   Calls:
*     Funct

* ----------------------- Declaration section ------------------------

       implicit none

*   Global variables
      include 'devel2.inc'             ! DEVEL2 common block
      double precision Response        ! function

*   Subroutine arguments
      integer Met_index                ! (INPUT) Current index into met arrays
      integer Obs                      ! (INPUT) Current observation
      double precision Param_values(Max_num_param)
                                       ! (INPUT) Current parameter values.

*   Internal variables
      double precision FTemp           ! Temperature function
      double precision FPP             ! Photoperiod function
      double precision ropt            ! ropt 

*   Constant values
*     none

*   Initial data values
*      none

* --------------------- Executable code section ----------------------

      if (g_ropt_funct(Obs) .ne. 0) then
         ropt = Param_values(g_param_start_index
     .                      (g_ropt_funct(obs)))
     
      else
         ropt = 1.0d0
      endif

      ! Go call the temperature response routine.

      if (g_funct_response(g_temp_funct(Obs)) .gt. 0) then
         fTemp = Response(g_funct_response(g_temp_funct(Obs)),
     .                    g_maxt(Met_index),
     .                    g_mint(Met_index),
     .                    Param_values,
     .                    g_param_start_index(g_temp_funct(obs)))

      else if (g_funct_response(g_temp_funct(Obs)) .eq. 0) then
         fTemp = Param_values(g_param_start_index(g_temp_funct(obs)))

      else
         fTemp = 1.0
      endif

      ! Go call the photoperiod response routine.

      if (g_funct_response(g_pp_funct(Obs)) .gt. 0) then
         fpp = Response(g_funct_response(g_pp_funct(Obs)),
     .                  g_pp(Met_index),
     .                  g_pp(Met_index),
     .                  Param_values,
     .                  g_param_start_index(g_pp_funct(obs)))

      else if (g_funct_response(g_pp_funct(Obs)) .eq. 0) then
         fpp = Param_values(g_param_start_index(g_pp_funct(obs)))

      else
          fpp = 1.0
      endif

      Devel2_calc_rate = ropt * fTemp * fpp
      return
      end

* ====================================================================
       double precision function Response
     .      (Response_number, Max_value, Min_value,
     .       Param_values, Param_start_index)
* ====================================================================

*   Short description:
*     Calculate a rate of development based on the Response_number
*     and function parameters passed in.  For temperature functions
*     Max_value = maximum temperature and Min_value = minimum temperature.
*     For photoperiod functions Max_value = Min_value = photoperiod.

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
*     DPH - 15/01/93

*   Calls:

* ----------------------- Declaration section ------------------------

       implicit none

*   Global variables
      include 'devel2.inc'             ! devel2 common

*   Subroutine arguments
      integer Response_number          ! (INPUT) Model response number
      double precision Param_values(*) ! (INPUT) Parameters to use
      real Max_value                   ! (INPUT) Maximum value (oC or hrs)
      real Min_value                   ! (INPUT) Minimum value (oC or hrs)
      integer Param_start_index        ! (INPUT) Index into param_values array

*   Internal variables
      real Mean                        ! mean of max_value and min_value
      double precision P(4)            ! local copies of parameters
      integer Param_index              ! index
      double precision Beta            ! used in curvilinear
      double precision Alpha           ! used in curvilinear
      real ttmp                        ! used in 3hour broken linear.
      real sum                         ! used in 3hr broken stick
      integer Hour                     ! current hour number 3hr broken stick
      real tmfak                       ! used in 3hr broken stick
      real minRate                     ! used in 3hr broken stick

*   Constant values
*      none

*   Initial data values
*      none

* --------------------- Executable code section ----------------------

      Mean = (Max_value + Min_value) / 2.0

      ! Copy our parameter values to local array.

      do 10 Param_index = Param_start_index, min(g_Num_params,
     .                                       Param_start_index+3)

         P(Param_index-Param_start_index+1) = Param_values(param_index)
10    continue

      ! Go do function calculation based on respons_number

      if (Response_number .eq. 1) then
         ! LINEAR
         
         Response = max(Mean - P(1), 0.0d0)

      else if (Response_number .eq. 2) then
         ! QUADRATIC
         Response = max(Mean - P(1), 0.0d0) * (P(2) - Mean) *
     .             ((P(2) - P(1)) / 2) ** (-2.0)

      else if (Response_number .eq. 3) then
         ! POWER
         Response = max(Mean - P(1), 0.0d0) ** P(2)

      else if (Response_number .eq. 4) then
         ! EXPONENTIAL
         Response = 1.0d0 - exp(-P(2) * max(Mean - P(1), 0.0d0))

      else if (Response_number .eq. 5) then
         ! SIGMOID
         Response = P(3) + (1.0d0 - P(3)) /
     .           (1.0d0 + exp(-P(2) * (Mean - P(1))))

      else if (Response_number .eq. 6) then
         ! BROKEN LINEAR
         if (Mean .le. P(1)) then
            Response = 0.0d0
            
         else if (Mean .le. P(2)) then
            Response = (Mean - P(1)) / (P(2) - P(1))
            
         else if (Mean .le. P(3)) then
            Response = 1.0d0 - (Mean - P(2)) / (P(3) - P(2))
            
         else
            Response = 0.0d0
         endif

      else if(Response_number .eq. 7) then
         ! TRIPLE BROKEN LINEAR
         
         if (P(1) .ge. 0.0d0) then
            if (Mean .lt. P(2)) then
               Response = 1.0d0 - (P(3) - P(2)) * P(1)

            else if (Mean .lt. P(3)) then
               Response = 1.0d0 - (P(3) - Mean) * P(1)

            else
               Response = 1.0d0
            endif

         else
            if (Mean .lt. P(2)) then
               Response = 1.0d0
               
            else if (Mean .lt. P(3)) then
               Response = 1.0d0 + (Mean - P(2)) * P(1)

            else
               Response = 1.0d0 + (P(3) - P(2)) * P(1)
            endif
         endif

      else if (Response_number .eq. 8) then
         P(4)= max(P(4),P(1))
         minRate = max((P(4) - P(1))/(P(2)-P(1)),0.0)
      !   if (Min_value .gt. P(1) .and. Max_value .lt. P(2)) then
      !      Response = (Mean - P(1)) / (P(2) - P(1))
      !  else if (Max_value .lt. P(1)) then
         if (Max_value .lt. P(1) .or. Min_value .gt. P(3)) then
            Response = 0.0d0
         else
            sum = 0.0
            do 20 Hour = 1, 8
               tmfak = 0.931
     .               + 0.1140 * Hour
     .               - 0.0703 * Hour**2
     .               + 0.0053 * Hour**3
               ttmp = Min_value + tmfak * (Max_value - Min_value)
               if(ttmp .le. P(4)) then
                  sum = sum + minRate / 8.0
               else if (ttmp .gt. P(4) .and. ttmp .lt. P(2)) then
                  sum = sum + (ttmp - P(1)) / (P(2) - P(1)) / 8.0
                  
               else if (ttmp .gt. P(2) .and. ttmp .lt. P(3)) then
                  sum = sum + (1.0 - (ttmp - P(2)) / (P(3) - P(2)))/8.0
                  
               endif              
20          continue
            Response = sum
         endif
      !   open(unit=5, file='c:\temp\junk.out', access = 'append')
      !   write(5,*)Response,Min_value,Max_value,Mean
      !  close(5)

      else
         if (P(1).lt.P(2) .and. P(2).lt.P(3) .and.
     .       Mean .gt. P(1) .and. Mean .lt. P(3)) then
            Beta = (P(3) - P(2)) / (P(2) - P(1))
            Alpha = 1.0d0 / ( (P(2)-P(1)) * (P(3)-P(2)+.00001) ** Beta)
            Response = Alpha * max(Mean - P(1), 0.0) *
     .              (P(3) - Mean) ** Beta

         else
             Response = 0.0d0
         endif
      endif

      Response = max(Response, 0.0d0)
      return
      end


