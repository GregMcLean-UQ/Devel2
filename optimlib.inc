* ====================================================================
      common /optimlib_integer/
     .   g_Jack_num,
     .   g_Group_size,
     .   g_Num_params,
     .   g_Num_iterations,
     .   g_Num_data_points
      common /optimlib_double/
     .   g_Param_lower,
     .   g_Param_upper,
     .   g_Param_center,
     .   g_Param_diff,
     .   g_Param_matrix,
     .   g_scaled_lower,
     .   g_scaled_upper,
     .   g_z_lower,
     .   g_z_upper
* ====================================================================

*   Short description:
*     Common block for optimlib optimisation library
*
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
*     DPH 14/6/95

*   Calls:

* ----------------------- Declaration section ------------------------

*   Constant variables

      integer Max_num_param            ! maximum number of allowed parameters
      parameter (Max_num_param=50)

      integer Max_iterations           ! maximum number of iterations that
      parameter (Max_iterations=1000)  ! will be stored in g_param_matrix array

      integer Max_num_data_points      ! maximum number of data points allowed
      parameter (Max_num_data_points=500)

      integer LU_Param_file            ! unit number for parameter file
      parameter (LU_Param_file=5)

      integer LU_Output_file           ! unit number for output file
      parameter (LU_Output_file=6)

*   Global variables

      integer g_Jack_num               ! Current jackknife number
      integer g_Group_size             ! Current jackknife group size
      integer g_Num_params             ! number of params.
      integer g_Num_iterations         ! number of iterations
      integer g_Num_data_points        ! number of data points
      double precision g_Param_lower(Max_num_param)
                                       ! (INPUT & OUTPUT) Lower bound of parameter
      double precision g_Param_upper(Max_num_param)
                                       ! (INPUT & OUTPUT) Upper bound of parameter
      double precision g_Param_center(Max_num_param)
                                       ! Parameter centers used in scaling
      double precision g_Param_diff(Max_num_param)
                                       ! Parameters diffs used in scaling
      double precision g_Param_matrix(Max_iterations, Max_num_param + 1)
                                       ! Array containing the parameters for the first
                                       ! 1000 iterations.  Used in calculating 10%f range
      double precision g_scaled_lower(max_num_param)
                                       ! Scaled lower bounds
      double precision g_scaled_upper(max_num_param)
                                       ! Scaled upper bounds
      double precision g_z_lower(max_num_param)
                                       ! Z lower bounds
      double precision g_z_upper(max_num_param)
                                       ! Z upper bounds

