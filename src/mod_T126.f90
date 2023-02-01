module mod_T126

  use iso_fortran_env, only: int32, int64, real32, real64
  implicit none

  Private
  Public :: T126, all_subs_T126

  type T126
    integer           :: K                = 0
    integer           :: M                = 0
    integer           :: prop1            = 0
    integer           :: prop2            = 0
    integer           :: prop3            = 0
    integer           :: prop4            = 0
    integer           :: N                = 0
    integer           :: A                = 0
    real(real64)      :: Xnorm            = 0.0
    real(real64)      :: Ynorm            = 0.0
    real(real64)      :: Znorm            = 0.0
    integer           :: i_first_knot     = 0
    integer           :: i_last_knot      = 0
    integer           :: i_first_weight   = 0
    integer           :: i_last_weight    = 0
    integer           :: i_first_point    = 0
    integer           :: i_last_point     = 0
    integer           :: i_first_param    = 0
    integer           :: i_last_param     = 0
    integer           :: i_Xnorm          = 0
    integer           :: i_Ynorm          = 0
    integer           :: i_Znorm          = 0
    integer           :: n_knots          = 0
    integer           :: n_weights        = 0
    integer           :: n_control_points = 0
    integer           :: n_params         = 0
    integer           :: n_bezier_curves  = 0
    integer           :: n_PD_ints        = 0
    integer           :: n_PD_reals       = 0
    real(real64),allocatable    :: T(:)
    real(real64),allocatable    :: W(:)
    real(real64),allocatable    :: P(:)
    real(real64),allocatable    :: V(:)
    integer,allocatable         :: Parameter_ints(:)
    real(real64),allocatable    :: Parameter_reals(:)
    integer              :: n_PD_entries       = 0
    integer              :: Type_ID            = 0
    character(len=4000)  :: Raw_Parameter_Data = ''
    contains
      procedure  :: test_type     => test_type_T126
      procedure  :: alloc_P       => allocate_PD_initial_T126
      procedure  :: read_initial  => read_raw_data_T126
      procedure  :: eval_props    => evaluate_param_properties_T126
      procedure  :: initial_calcs => initial_calcs_T126
      procedure  :: alloc         => allocate_T126
      procedure  :: init          => initialize_T126
      !procedure  :: read          => read_data_T126
      procedure  :: call_all_subs => all_subs_T126
  end type T126

contains

  subroutine all_subs_T126(this, Type_ID, n_PD_entries, Raw_Parameter_Data)
      class(T126) :: this
      character(len=4000),intent(in) :: Raw_Parameter_Data
      integer, intent(in) :: Type_ID
      integer,intent(in)  :: n_PD_entries
      this%n_PD_entries       = n_PD_entries
      this%Raw_Parameter_Data = Raw_Parameter_Data
      this%Type_ID            = Type_ID
      call test_type_T126(this)
      call allocate_PD_initial_T126(this)
      call read_raw_data_T126(this)
      call evaluate_param_properties_T126(this)
      call initial_calcs_T126(this)
      call allocate_T126(this)
      call initialize_T126(this)
      call read_data_T126(this)
  end subroutine all_subs_T126

  subroutine test_type_T126(this)
    class(T126) :: this
    if (this%Type_ID == 126) then
      write(*,*) 'Parameter type to read in is 126, continuing...'
    else
      STOP 'ERROR: Parameter Type is not 126, see mod_T126.f90...'
    end if
  end subroutine test_type_T126

  subroutine allocate_PD_initial_T126(this)
    class(T126) :: this
    this%n_PD_ints = 6
    this%n_PD_reals = (this%n_PD_entries - 1)   ! DNH Note: Pad first 6 entries for indexes
    allocate(this%Parameter_ints(this%n_PD_ints))
    allocate(this%Parameter_reals(this%n_PD_reals))
    this%Parameter_ints  = 0
    this%Parameter_reals = 0.0
  end subroutine allocate_PD_initial_T126

  subroutine read_raw_data_T126(this)
    class(T126)         :: this
    read(this%Raw_Parameter_Data, fmt=*) this%Type_ID, &
                                         this%Parameter_ints, &
                                         this%Parameter_reals(7:size(this%Parameter_reals))
    this%K     = this%Parameter_ints(1)
    this%M     = this%Parameter_ints(2)
    this%prop1 = this%Parameter_ints(3)
    this%prop2 = this%Parameter_ints(4)
    this%prop3 = this%Parameter_ints(5)
    this%prop4 = this%Parameter_ints(6)
    write(*,*) this%Parameter_ints
  end subroutine read_raw_data_T126

  subroutine evaluate_param_properties_T126(this)
    class(T126) :: this
  end subroutine evaluate_param_properties_T126

  subroutine initial_calcs_T126(this)
    class(T126) :: this
    this%N = (1 + this%K - this%M)
    this%A = (this%N + (2 * this%M))
    this%n_knots = (this%K + this%n_control_points + 1)
    this%n_weights = ((8 + this%A + this%K) - (8 + this%A) + 1)
    this%n_params = ((13 + this%A + 4 * this%K) - (12 + this%A + 4 * this%K) + 1)
    this%n_control_points = (((11+this%A+4*this%K)-(9+this%A+this%K)+1)/this%K)
    this%n_bezier_curves = this%n_control_points - this%K + 1
    this%i_first_knot = 7
    this%i_last_knot = (7 + this%A)
    this%i_first_weight = (8 + this%A)
    this%i_last_weight = (8 + this%A + this%K)
    this%i_first_point = (9 + this%A + this%K)
    this%i_last_point = (11 + this%A + (4 * this%K))
    this%i_first_param = (12 + this%A + (4 * this%K))
    this%i_last_param = (13 + this%A + (4 * this%K))
    this%i_Xnorm = (14 + this%A + (4 * this%K))
    this%i_Ynorm = (15 + this%A + (4 * this%K))
    this%i_Znorm = (16 + this%A + (4 * this%K))
  end subroutine initial_calcs_T126

  subroutine allocate_T126(this)
    class(T126) :: this
    allocate(this%T(this%n_knots))
    allocate(this%W(this%n_weights))
    allocate(this%P(this%n_control_points))
    allocate(this%V(this%n_params))
  end subroutine allocate_T126

  subroutine initialize_T126(this)
    class(T126) :: this
    this%T = 0.0
    this%W = 0.0
    this%P = 0.0
    this%V = 0.0
  end subroutine initialize_T126

  subroutine read_data_T126(this)
    class(T126) :: this
    this%T = this%Parameter_reals(this%i_first_knot : this%i_last_knot)
    this%W = this%Parameter_reals(this%i_first_weight : this%i_last_weight)
    this%P = this%Parameter_reals(this%i_first_point : this%i_last_point)
    this%V = this%Parameter_reals(this%i_first_param : this%i_last_param)
    this%Xnorm = this%Parameter_reals(this%i_Xnorm)
    this%Ynorm = this%Parameter_reals(this%i_Ynorm)
    this%Znorm = this%Parameter_reals(this%i_Znorm)
    write(*,*) this%T
    write(*,*) this%W
    write(*,*) this%P
    write(*,*) this%V
    write(*,*) this%Xnorm
    write(*,*) this%Ynorm
    write(*,*) this%Znorm
  end subroutine read_data_T126

end module mod_T126
