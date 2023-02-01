module mod_T141

  use mod_T128

  implicit none

  Private
  Public :: T141, all_subs_T141

  type T141
    integer              :: Type_BS      = 0
    integer              :: PREF         = 0
    integer              :: SPTR         = 0
    integer              :: N            = 0
    integer              :: CRVPT_1      = 0
    integer              :: SENSE_1      = 0
    integer              :: K1           = 0
    integer              :: CRVPT_N      = 0
    integer              :: SENSE_N      = 0
    integer              :: K_N          = 0
    integer              :: M            = 0
    integer              :: iCRVPT_N     = 0
    integer              :: iSENSE_N     = 0
    integer              :: iK_N         = 0
    integer              :: iPSCPT_1_1   = 0
    integer              :: iPSCPT_1_K1  = 0
    integer              :: iPSCPT_N_1   = 0
    integer              :: iPSCPT_N_KN  = 0
    integer              :: n_PSCPT_1_K  = 0
    integer              :: n_PSCPT_N_K  = 0
    integer, allocatable :: PSCPT_1_K(:)
    integer, allocatable :: PSCPT_N_K(:)
    integer, allocatable :: Parameter_Data(:)
    integer              :: n_PD_entries       = 0
    integer              :: Type_ID            = 0
    character(len=4000)  :: Raw_Parameter_Data = ''
    contains
      procedure  :: read_raw      => read_raw_data_T141
      procedure  :: test_type     => test_type_T141
      procedure  :: index_calcs   => index_calcs_T141
      procedure  :: alloc         => allocate_T141
      procedure  :: read_data     => read_param_data_T141
      procedure  :: eval_props    => evaluate_param_properties_T141
      procedure  :: T141_operations => all_subs_T141
  end type T141

contains

  subroutine all_subs_T141(this, Type_ID, n_PD_entries, Raw_Parameter_Data)
      class(T141) :: this
      character(len=4000),intent(in) :: Raw_Parameter_Data
      integer, intent(in) :: Type_ID
      integer,intent(in) :: n_PD_entries
      this%n_PD_entries       = n_PD_entries
      this%Raw_Parameter_Data = Raw_Parameter_Data
      this%Type_ID            = Type_ID
      call read_raw_data_T141(this)
      call test_type_T141(this)
      call index_calcs_T141(this)
      call allocate_T141(this)
      call read_param_data_T141(this)
      call evaluate_param_properties_T141(this)
  end subroutine all_subs_T141

  subroutine read_raw_data_T141(this)
    class(T141),intent(inout)  :: this
    allocate(this%Parameter_Data(this%n_PD_entries - 1))
    this%Parameter_Data = 0

    read(this%Raw_Parameter_Data, fmt=*) this%Type_ID, this%Parameter_Data
    this%TYPE_BS  = this%Parameter_Data(1)
    this%PREF     = this%Parameter_Data(2)
    this%SPTR     = this%Parameter_Data(3)
    this%N        = this%Parameter_Data(4)
    this%CRVPT_1  = this%Parameter_Data(5)
    this%SENSE_1  = this%Parameter_Data(6)
    this%K1       = this%Parameter_Data(7)
    !write(*,*) this%Parameter_Data
  end subroutine read_raw_data_T141

  subroutine test_type_T141(this)
    class(T141) :: this
    if (this%Type_ID == 141) then
      write(*,*) 'Parameter type to read in is 141, continuing...'
    else
      STOP 'ERROR: Parameter Type is not 141, see mod_T141.f90...'
    end if
  end subroutine test_type_T141

  subroutine index_calcs_T141(this)
    class(T141) :: this
    this%iPSCPT_1_1  = 8
    this%iPSCPT_1_K1 = (7 + this%K1)
    this%M = 12 + 3*(this%N - 1) + (this%K1) !DNH: See pg 160 of IGES manual for K1
    this%iCRVPT_N    = (this%M)
    this%iSENSE_N    = (1 + this%M)
    this%iK_N        = (2 + this%M)
    this%iPSCPT_N_1  = (3 + this%M)
    this%iPSCPT_N_KN = (2 + this%K_N + this%M)
  end subroutine index_calcs_T141

  subroutine allocate_T141(this)
    class(T141) :: this
    this%n_PSCPT_1_K = (this%iPSCPT_1_K1 - this%iPSCPT_1_1 + 1)
    this%n_PSCPT_N_K = (this%iPSCPT_N_KN - this%iPSCPT_N_1 + 1)
    allocate(this%PSCPT_1_K(this%n_PSCPT_1_K))
    allocate(this%PSCPT_N_K(this%n_PSCPT_N_K))
    this%PSCPT_1_K      = 0
    this%PSCPT_N_K      = 0
  end subroutine allocate_T141

  subroutine read_param_data_T141(this)
    class(T141) :: this
    if (this%N > 1) then
      this%PSCPT_1_K = this%Parameter_Data(this%iPSCPT_1_1 : this%iPSCPT_1_K1)
      this%CRVPT_N   = this%Parameter_Data(this%iCRVPT_N)
      this%SENSE_N   = this%Parameter_Data(this%iSENSE_N)
      this%K_N       = this%Parameter_Data(this%iK_N)
      this%PSCPT_N_K = this%Parameter_Data(this%iPSCPT_N_1 : this%iPSCPT_N_KN)
    else
      !write(*,*) 'this%N is < 2, nothing to do for read_param_data_T141'
    end if
  end subroutine read_param_data_T141

  subroutine evaluate_param_properties_T141(this)
    class(T141) :: this
  end subroutine evaluate_param_properties_T141

end module mod_T141
