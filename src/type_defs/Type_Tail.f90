module Type_Tail
   use iso_fortran_env, only: int32, int64, real32, real64
   implicit none

   private
   !public :: Tail, initialize_T

   Type, public :: Tail
      integer(int32)        :: num_S
      integer(int32)        :: num_G
      integer(int32)        :: num_D
      integer(int32)        :: num_P
      integer(int32)        :: num_T
   contains
      procedure, public  :: initialize_T => init_T
      procedure, public  :: read_T_data => read_T
   end type Tail

contains

   subroutine init_T(this)
      class(Tail), intent(inout) :: this
      this%num_S = 0
      this%num_G = 0
      this%num_D = 0
      this%num_P = 0
      this%num_T = 0
   end subroutine init_T

   subroutine read_T(this, fileunit, T_record_num)
      class(Tail), intent(inout) :: this
      integer, intent(in)  :: fileunit
      integer, intent(in)  :: T_record_num
      character            :: S, G, D, P, T
      integer              :: blanks(5) = 0
      character(len=80)    :: T_record_format

      T_record_format = '(4(a1,i7), 5(i8), (a1,i7))'
      read (fileunit, rec=T_record_num, fmt=T_record_format) S, this%num_S, &
         G, this%num_G, &
         D, this%num_D, &
         P, this%num_P, &
         blanks, &
         T, this%num_T

   end subroutine read_T

end module Type_Tail
