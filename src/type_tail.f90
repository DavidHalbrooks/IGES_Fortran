module type_tail
   use iso_fortran_env, only: int32, int64, real32, real64
   implicit none

   private

   Type, public :: T_tail
      integer(int32)        :: num_S
      integer(int32)        :: num_G
      integer(int32)        :: num_D
      integer(int32)        :: num_P
      integer(int32)        :: num_T
   contains
      procedure, public  :: init_tail_data => init_tail
      procedure, public  :: read_tail_data => read_tail
   end type T_tail

contains

   subroutine init_tail(this)
      class(T_tail), intent(inout) :: this
      this%num_S = 0
      this%num_G = 0
      this%num_D = 0
      this%num_P = 0
      this%num_T = 0
   end subroutine init_tail

   subroutine read_tail(this, fileunit, T_record_num)
      class(T_tail), intent(inout) :: this
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

   end subroutine read_tail

end module type_tail
