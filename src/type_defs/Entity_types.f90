module Entity_types

   implicit none

   private
   public :: T314, T128

   type T314
      integer           :: ID
      real              :: CC1
      real              :: CC2
      real              :: CC3
      character(len=80) :: Cname
   end type T314

   type T128
      integer :: ID
      integer :: K1
      integer :: K2
      integer :: M1
      integer :: M2
      integer :: PROP1
      integer :: PROP2
      integer :: PROP3
      integer :: PROP4
      integer :: PROP5
   contains
      !procedure :: calcs => T128_calculations
   end type T128

contains

!  subroutine T130_calculations(this)
!    class(T128), intent(inout) :: this
!    integer :: K1
!    integer :: K2
!    integer :: M1
!    integer :: M2
!    integer :: N1
!    integer :: N2
!    integer :: A
!    integer :: B
!    integer :: C
!    integer, allocatable :: k1(:)
!    integer, allocatable :: k2(:)
!
!    N1 = K1 - M1
!    N2 = K2 - M2
!    A = N1 + (2 * M1)
!    B = N2 + (2 * M2)
!    C = (1 + K1) * (1 + K2)
!
!    k1_first_index = 10
!    k1_last_index = 10 + A
!    k2_first_index = 11+ A
!    k2_last_index = 11 + A + B
!    first_weight_index = 12 + A + B
!    last_weight_index = 13 + A + B
!
!  end subroutine T128_calculations

end module Entity_types
