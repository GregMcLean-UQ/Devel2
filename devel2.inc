      integer LU_input_file            ! Unit number for input
      parameter (LU_input_file=7)

      integer LU_output_file           ! Unit number for output
      parameter (LU_output_file=6)

      integer LU_plant_file            ! Unit number for plant file
      parameter (LU_plant_file=20)

      logical Not_optional             ! Used in 3rd parameter in call to read_param
      parameter (Not_optional=.false.)

      logical Optional                 ! Used in 3rd parameter in call to read_param
      parameter (Optional=.true.)

      integer Max_plant_columns        ! maximum number of columns allowed in plant file
      parameter (max_plant_columns=15)

      integer max_num_obs              ! maximum number of observations
      parameter (max_num_obs=2000)

      integer max_num_param            ! Maximum number of parameters allowed
      parameter (max_num_param=50)

      integer NORMAL_RUN               ! Run_type = normal
      parameter (NORMAL_RUN=0)

      integer PROFILE_RUN              ! Run_type = profile
      parameter (PROFILE_RUN=1)

      integer JACKKNIFE_RUN            ! Run_type = jackknife
      parameter (JACKKNIFE_RUN=2)

      integer RATE_OPTIM               ! Optimisation_type = rate
      parameter (RATE_OPTIM=1)
      
      integer TTPP_OPTIM               ! Optimisation_type = tt vs pp
      parameter (TTPP_OPTIM=2)

* ====================================================================
      common /devel_integer/
     .   Start_col,
     .   End_col,
     .   g_run_type,
     .   g_Group_size,
     .   g_optim_type,
     .   g_profile_param
     
      common /devel_double/
     .   g_Y,
     .   g_YEST,
     .   g_Graph_X,
     .   g_Graph_Y,
     .   g_Tolerance,
     .   g_Simplex,
     .   g_Partial_est,
     .   g_Bias_est,
     .   g_2SE_lower,
     .   g_2SE_upper
     
      common /devel_logical/
     .   g_Quadratic_fit
     
* ====================================================================

*   Short description:
*     Common block for general devel stuff
*                        g
*   Notes:

*   Changes:
*     DPH 14/6/95

*   Calls:

* ----------------------- Declaration section ------------------------

*   Constant variables

*   Global variables
      integer Start_col                ! Start column in plant file
      integer End_col                  ! End column in plant file
      integer g_run_type               ! type of run we're doing
      integer g_group_size             ! jackknife group size.
      integer g_optim_type             ! type of optimisation run we're doing
      integer g_profile_param          ! Specific parameter to profile 0 = all params
      double precision g_Y(max_num_obs)
                                       ! Y values
      double precision g_YEST(max_num_obs)
                                       ! Estimated y values
      double precision g_Graph_X(Max_num_obs)
                                       ! X values for graphing
      double precision g_Graph_Y(Max_num_obs)
                                       ! Y values for graphing
      double precision g_Partial_est(Max_num_param)
                                       ! Partial estimates from jackknifing
      double precision g_Bias_est(Max_num_param)
                                       ! Bias estimates from jackknifing.
      double precision g_2SE_lower(Max_num_param)
                                       ! final param value minus 2 * SE (jackknife)
      double precision g_2SE_upper(Max_num_param)
                                       ! final param value plus 2 * SE (jackknife)
      double precision g_tolerance     ! Tolerance value
      double precision g_simplex       ! Simplex value
      logical g_Quadratic_fit          ! Does the user want a quadratic fit done ?

* ====================================================================
      common /exper_integer/
     .   g_Num_experiments,
     .   g_Met_file_number,
     .   g_OBS_start_row,
     .   g_OBS_end_row,
     .   g_Trt_start_row,
     .   g_Trt_end_row

      common /exper_char/
     .   g_Experiment_name
* ====================================================================

*   Short description:
*     Common block for all devel experiment data
*
*   Notes:

*   Changes:
*     DPH 14/6/95

*   Calls:

* ----------------------- Declaration section ------------------------

*   Constant variables
      integer Max_num_experiments      ! maximum number of allowed experiments
      parameter (max_num_experiments=100)

      integer Max_exper_name           ! maximum size for each experiment name
      parameter (Max_exper_name=50)

*   Global variables
      integer g_Num_experiments        ! number of experiments
      integer g_Met_file_number(Max_num_experiments)
                                       ! met file number for each experiment
      character g_Experiment_name(Max_num_experiments)*(max_exper_name)
                                       ! experiment name for each experiment
      integer g_OBS_start_row(Max_num_experiments)
                                       ! start row in observation table
      integer g_OBS_end_row(Max_num_experiments)
                                       ! end row in observation table
      integer g_Trt_start_row(Max_num_experiments)
                                       ! Treatment start row in observation table
      integer g_Trt_end_row(Max_num_experiments)
                                       ! Treatment end row in observation table

* ====================================================================
      common /trt_integer/
     .   g_Num_trts,
     .   g_trt_ropt,
     .   g_trt_temp,
     .   g_trt_pp
      common /trt_char/
     .   g_trt_name
* ====================================================================

*   Short description:
*     Common block for all devel experiment data
*
*   Notes:

*   Changes:
*     DPH 14/6/95

*   Calls:

* ----------------------- Declaration section ------------------------

*   Constant variables
      integer Max_num_Treatments       ! maximum number of allowed treatments
      parameter (max_num_treatments=500)

      integer Max_trt_name             ! maximum size of a treatment name
      parameter (Max_trt_name=20)

*   Global variables
      integer g_Num_trts               ! number of experiments
      integer g_trt_ropt(Max_num_treatments)
                                       ! ropt group for this treatment
      integer g_trt_temp(Max_num_treatments)
                                       ! temperature group for this treatment
      integer g_trt_pp(Max_num_treatments)
                                       ! photoperiod group for this treatment
      character g_trt_name(Max_num_treatments)*(max_trt_name)
                                       ! treatment name for each experiment


* ====================================================================
      common /metfile_integer/
     .   g_Num_met_files,
     .   g_met_start_index,
     .   g_met_end_index

      common /metfile_char/
     .   g_met_file_name

* ====================================================================

*   Short description:
*     Common block for all devel met files
*
*   Notes:

*   Changes:
*     DPH 14/6/95

*   Calls:

* ----------------------- Declaration section ------------------------

*   Constant variables
      integer max_num_met_files        ! maximum number of met files
      parameter (max_num_met_files=50)

*   Global variables
      integer g_Num_met_files          ! number of met files loaded.
      character g_met_file_name(max_num_met_files)*80
                                       ! list of loaded met files.
      integer g_met_start_index(max_num_met_files)
                                       ! start index into met tables for each met file
      integer g_met_end_index(max_num_met_files)
                                       ! end index into met tables for each met file

* ====================================================================
      common /metdata_integer/
     .   g_Num_met_data,
     .   g_year,
     .   g_day

      common /metdata_real/
     .   g_maxt,
     .   g_mint,
     .   g_pp
* ====================================================================

*   Short description:
*     Common block for all devel met data
*
*   Notes:

*   Changes:
*     DPH 14/6/95

*   Calls:

* ----------------------- Declaration section ------------------------

*   Constant variables
      integer max_num_met_data         ! maximum number of lines of met data
      parameter (max_num_met_data=30000)

*   Global variables
      integer g_Num_met_data           ! number of lines of met data loaded
      integer g_year(max_num_met_data) ! year
      integer g_day(max_num_met_data)  ! day
      real g_maxt(max_num_met_data)    ! maximum temperature
      real g_mint(max_num_met_data)    ! minimum temperature
      real g_pp(Max_num_met_data)      ! photoperiod.

* ====================================================================
      common /obs_integer/
     .   g_Num_OBS,
     .   g_met_start,
     .   g_met_end,
     .   g_met_end_data,
     .   g_ropt_funct,
     .   g_temp_funct,
     .   g_pp_funct
      common /obs_char/
     .   g_obs_name
* ====================================================================

*   Short description:
*     Common block for all devel observed data
*
*   Notes:

*   Changes:
*     DPH 14/6/95

*   Calls:

* ----------------------- Declaration section ------------------------

*   Constant variables

*   Global variables
      integer g_Num_OBS                ! Number of observations
      integer g_met_start(Max_num_obs)
                                       ! start met index for each observation
      integer g_met_end(Max_num_obs)
                                       ! end met index for each observation
      integer g_met_end_data(Max_num_obs)
                                       ! end met index for end of met data for each observation
      character g_obs_name(max_num_obs)*30
                                       ! Name of each observation
      integer g_ropt_funct(max_num_obs)! ropt function to each for each observation
      integer g_temp_funct(max_num_obs)! temperature function to use for each observation
      integer g_pp_funct(max_num_obs)  ! photoperiod function to use for each observation



* ====================================================================
      common /funct_integer/
     .   g_Num_funct,
     .   g_Funct_response,
     .   g_Param_start_index

      common /funct_char/
     .   g_Funct_name
* ====================================================================

*   Short description:
*     Common block for all devel function groups
*
*   Notes:

*   Changes:
*     DPH 14/6/95

*   Calls:

* ----------------------- Declaration section ------------------------

*   Constant variables
      integer max_num_funct            ! maximum number of function groups allowed
      parameter (max_num_funct=50)

*   Global variables
      integer g_Num_funct              ! number of function groups
      integer g_Funct_response(max_num_funct)
                                       ! function response of each function group
      integer g_param_start_index(max_num_funct)
                                       ! starting parameter index for each function
      character g_Funct_name(max_num_funct)*30
                                       ! function name for each function

* ====================================================================
      common /param_integer/
     .   g_Num_params

      common /param_logical/
     .   g_Param_fixed

      common /param_double/
     .   g_Param_values,
     .   g_param_lower,
     .   g_param_upper
* ====================================================================

*   Short description:
*     Common block for all devel parameter data
*
*   Notes:

*   Changes:
*     DPH 14/6/95

*   Calls:

* ----------------------- Declaration section ------------------------

*   Constant variables

*   Global variables
      integer g_Num_params             ! number of parameter in tables
      logical g_param_fixed(max_num_param)
                                       ! .true. if parameter is fixed.
      double precision g_Param_values(max_num_param)
                                       ! parameter values
      double precision g_param_lower(max_num_param)
                                       ! lower bound of each parameter
      double precision g_param_upper(max_num_param)
                                       ! upper bound of each parameter

