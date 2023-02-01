module mod_T141_object

  use mod_read_raw_parametric
  use mod_T126
  use mod_T128
  use mod_T141
  use mod_T143
  use mod_T314

  implicit none

  type T141_object
    integer                :: Type_BS      = 0
    integer                :: PREF         = 0
    integer                :: SPTR         = 0
    integer                :: N            = 0
    type(T126),allocatable :: Bspline_T126(:)
    integer                :: CRVPT_1      = 0
    integer                :: SENSE_1      = 0
    integer                :: K1           = 0
    integer                :: CRVPT_N      = 0
    integer                :: SENSE_N      = 0
    integer                :: K_N          = 0
    integer                :: M            = 0
    integer                :: iCRVPT_N     = 0
    integer                :: iSENSE_N     = 0
    integer                :: iK_N         = 0
    integer                :: iPSCPT_1_1   = 0
    integer                :: iPSCPT_1_K1  = 0
    integer                :: iPSCPT_N_1   = 0
    integer                :: iPSCPT_N_KN  = 0
    integer                :: n_PSCPT_1_K  = 0
    integer                :: n_PSCPT_N_K  = 0
    integer, allocatable   :: PSCPT_1_K(:)
    integer, allocatable   :: PSCPT_N_K(:)
  contains
    procedure :: alloc_object => alloc_T141_object
    procedure :: map_T141     => map_T141_data
  end type T141_object

contains

  subroutine alloc_T141_object(this)
    class(T141_object) :: this
    allocate(this%BSpline_T126(this%N))
    print *
  end subroutine alloc_T141_object

  subroutine map_T141_data(this)
    class(T141_object),intent(in) :: this
    !call read(this, fileunit, n_PD_records, num_record_start)
    !call import_Parametric_data(this)
!    call move_alloc(this%Type_BS, this%T141_Data%Type_BS)
!    call move_alloc(this%PREF, this%T141_Data%PREF)
!    call move_alloc(this%SPTR, this%T141_Data%SPTR)
!    call move_alloc(this%N, this%T141_Data%N)
!    call move_alloc(this%Bspline_T126, this%T141_Data%CRVPT_1)
!    call move_alloc(this%SENSE_1, this%T141_Data%SENSE_1)
!    call move_alloc(this%K1, this%T141_Data%K1)
!    call move_alloc(this%CRVPT_N, this%T141_Data%CRVPT_N)
!    call move_alloc(this%SENSE_N, this%T141_Data%SENSE_N)
!    call move_alloc(this%K_N, this%T141_Data%K_N)
!    call move_alloc(this%M, this%T141_Data%M)
!    call move_alloc(this%iCRVPT_N, this%T141_Data%iCRVPT_N)
!    call move_alloc(this%iSENSE_N, this%T141_Data%iSENSE_N)
!    call move_alloc(this%iK_N, this%T141_Data%iK_N)
!    call move_alloc(this%iPSCPT_1_1, this%T141_Data%iPSCPT_1_1)
!    call move_alloc(this%iPSCPT_1_K1, this%T141_Data%iPSCPT_1_K1)
!    call move_alloc(this%iPSCPT_N_1, this%T141_Data%iPSCPT_N_1)
!    call move_alloc(this%iPSCPT_N_KN, this%T141_Data%iPSCPT_N_KN)
!    call move_alloc(this%n_PSCPT_1_K, this%T141_Data%n_PSCPT_1_K)
!    call move_alloc(this%n_PSCPT_N_K, this%T141_Data%n_PSCPT_N_K)
!    call move_alloc(this%PSCPT_1_K, this%T141_Data%PSCPT_1_K)
!    call move_alloc(this%PSCPT_N_K, this%T141_Data%PSCPT_N_K)
  end subroutine map_T141_data

end module mod_T141_object
