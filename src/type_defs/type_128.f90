module Type128

   use iso_fortran_env, only: int32, int64, real32, real64
   implicit none

   Private
   Public :: T128, all_subs_T128

   type T128
      integer           :: K1 = 0
      integer           :: K2 = 0
      integer           :: M1 = 0
      integer           :: M2 = 0
      integer           :: prop1 = 0
      integer           :: prop2 = 0
      integer           :: prop3 = 0
      integer           :: prop4 = 0
      integer           :: prop5 = 0
      integer           :: N1 = 0
      integer           :: N2 = 0
      integer           :: A = 0
      integer           :: B = 0
      integer           :: C = 0
      integer           :: i_first_knot_S = 0
      integer           :: i_last_knot_S = 0
      integer           :: i_first_knot_T = 0
      integer           :: i_last_knot_T = 0
      integer           :: i_first_weight_W = 0
      integer           :: i_last_weight_W = 0
      integer           :: i_first_point_P = 0
      integer           :: i_last_point_P = 0
      integer           :: i_first_param_U = 0
      integer           :: i_last_param_U = 0
      integer           :: i_first_param_V = 0
      integer           :: i_last_param_V = 0
      integer           :: n_knots_S = 0
      integer           :: n_knots_T = 0
      integer           :: n_weights_W = 0
      integer           :: n_control_points_P = 0
      integer           :: n_params_U = 0
      integer           :: n_params_V = 0
      integer           :: n_bezier_patches = 0
      integer           :: n_PD_ints = 0
      integer           :: n_PD_reals = 0
      real(real64), allocatable    :: S(:)
      real(real64), allocatable    :: T(:)
      real(real64), allocatable    :: W(:)
      real(real64), allocatable    :: P(:)
      real(real64), allocatable    :: U(:)
      real(real64), allocatable    :: V(:)
      integer, allocatable :: Parameter_ints(:)
      real(real64), allocatable    :: Parameter_reals(:)
      integer              :: n_PD_entries = 0
      integer              :: Type_ID = 0
      character(len=4000)  :: Raw_Parameter_Data = ''
   contains
      procedure  :: test_type => test_type_T128
      procedure  :: alloc_P => allocate_PD_initial_T128
      procedure  :: read_initial => read_raw_data_T128
      procedure  :: eval_props => evaluate_param_properties_T128
      procedure  :: initial_calcs => initial_calcs_T128
      procedure  :: alloc => allocate_T128
      procedure  :: init => initialize_T128
      !procedure  :: read          => read_data_T128
      procedure  :: call_all_subs => all_subs_T128
   end type T128

contains

   subroutine all_subs_T128(this, Type_ID, n_PD_entries, Raw_Parameter_Data)
      class(T128) :: this
      character(len=4000), intent(in) :: Raw_Parameter_Data
      integer, intent(in) :: Type_ID
      integer, intent(in)  :: n_PD_entries
      this%n_PD_entries = n_PD_entries
      this%Raw_Parameter_Data = Raw_Parameter_Data
      this%Type_ID = Type_ID
      call test_type_T128(this)
      call allocate_PD_initial_T128(this)
      call read_raw_data_T128(this)
      call evaluate_param_properties_T128(this)
      call initial_calcs_T128(this)
      call allocate_T128(this)
      call initialize_T128(this)
      call read_data_T128(this)
   end subroutine all_subs_T128

   subroutine test_type_T128(this)
      class(T128) :: this
      if (this%Type_ID == 128) then
         write (*, *) 'Parameter type to read in is 128, continuing...'
      else
         STOP 'ERROR: Parameter Type is not 128, see mod_T128.f90...'
      end if
   end subroutine test_type_T128

   subroutine allocate_PD_initial_T128(this)
      class(T128) :: this
      this%n_PD_ints = (9)
      this%n_PD_reals = (this%n_PD_entries - 1)   ! DNH Note: Pad first 9 entries for indexes
      allocate (this%Parameter_ints(this%n_PD_ints))
      allocate (this%Parameter_reals(this%n_PD_reals))
      this%Parameter_ints = 0
      this%Parameter_reals = 0.0
   end subroutine allocate_PD_initial_T128

   subroutine read_raw_data_T128(this)
      class(T128)         :: this
      read (this%Raw_Parameter_Data, fmt=*) this%Type_ID, &
         this%Parameter_ints, &
         this%Parameter_reals(10:size(this%Parameter_reals))
      this%K1 = this%Parameter_ints(1)
      this%K2 = this%Parameter_ints(2)
      this%M1 = this%Parameter_ints(3)
      this%M2 = this%Parameter_ints(4)
      this%prop1 = this%Parameter_ints(5)
      this%prop2 = this%Parameter_ints(6)
      this%prop3 = this%Parameter_ints(7)
      this%prop4 = this%Parameter_ints(8)
      this%prop5 = this%Parameter_ints(9)
      !write(*,*) this%Parameter_ints
      !write(*,*) this%Parameter_reals
   end subroutine read_raw_data_T128

   subroutine evaluate_param_properties_T128(this)
      class(T128) :: this
   end subroutine evaluate_param_properties_T128

   subroutine initial_calcs_T128(this)
      class(T128) :: this
      this%N1 = (1 + this%K1 - this%M1)
      this%N2 = (1 + this%K2 - this%M2)
      this%A = (this%N1 + (2*this%M1))
      this%B = (this%N2 + (2*this%M2))
      this%C = ((1 + this%K1)*(1 + this%K2))
      this%i_first_knot_S = 10
      this%i_last_knot_S = (10 + this%A)
      this%i_first_knot_T = (11 + this%A)
      this%i_last_knot_T = (11 + this%A + this%B)
      this%i_first_weight_W = (12 + this%A + this%B)
      this%i_last_weight_W = (11 + this%A + this%B + this%C)
      this%i_first_point_P = (12 + this%A + this%B + this%C)
      this%i_last_point_P = (11 + this%A + this%B + (4*this%C))
      this%i_first_param_U = (12 + this%A + this%B + (4*this%C))
      this%i_last_param_U = (13 + this%A + this%B + (4*this%C))
      this%i_first_param_V = (14 + this%A + this%B + (4*this%C))
      this%i_last_param_V = (15 + this%A + this%B + (4*this%C))
      this%n_knots_S = (this%i_last_knot_S - this%i_first_knot_S + 1)
      this%n_knots_T = (this%i_last_knot_T - this%i_first_knot_T + 1)
      this%n_weights_W = (this%i_last_weight_W - this%i_first_weight_W + 1)
      this%n_control_points_P = (this%i_last_point_P - this%i_first_point_P + 1)
      this%n_params_U = (this%i_last_param_U - this%i_first_param_U + 1)
      this%n_params_V = (this%i_last_param_V - this%i_first_param_V + 1)
      !this%n_bezier_patches = this%n_control_points - this%K + 1 !Update for patches DNH: 01-13-23
   end subroutine initial_calcs_T128

   subroutine allocate_T128(this)
      class(T128) :: this
      allocate (this%S(this%n_knots_S))
      allocate (this%T(this%n_knots_T))
      allocate (this%W(this%n_weights_W))
      allocate (this%P(this%n_control_points_P))
      allocate (this%U(this%n_params_U))
      allocate (this%V(this%n_params_V))
   end subroutine allocate_T128

   subroutine initialize_T128(this)
      class(T128) :: this
      this%S = 0.0
      this%T = 0.0
      this%W = 0.0
      this%P = 0.0
      this%U = 0.0
      this%V = 0.0
   end subroutine initialize_T128

   subroutine read_data_T128(this)
      class(T128) :: this
      this%S = this%Parameter_reals(this%i_first_knot_S:this%i_last_knot_S)
      this%T = this%Parameter_reals(this%i_first_knot_T:this%i_last_knot_T)
      this%W = this%Parameter_reals(this%i_first_weight_W:this%i_last_weight_W)
      this%P = this%Parameter_reals(this%i_first_point_P:this%i_last_point_P)
      this%U = this%Parameter_reals(this%i_first_param_U:this%i_last_param_U)
      this%V = this%Parameter_reals(this%i_first_param_V:this%i_last_param_V)
      !write(*,*) this%S
      !write(*,*) this%T
      !write(*,*) this%W
      !write(*,*) this%P
      !write(*,*) this%U
      !write(*,*) this%V
   end subroutine read_data_T128

end module Type128
