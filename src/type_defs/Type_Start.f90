module Type_Start
   use iso_fortran_env, only: int32, int64, real32, real64
   implicit none

   private
   public :: Start

   type Start
      character(len=400) :: Header
   contains
      procedure :: read_S_data => read
   end type Start

contains

   subroutine read (this, fileunit, num_records_S)
      class(Start) :: this
      integer(int32), intent(in)      :: fileunit
      integer(int32), intent(in)      :: num_records_S
      integer(int32)                  :: i
      character(len=20)               :: S_format = ''
      integer(int32)                  :: S_record_num = 1
      character(len=82), allocatable  :: buffer_record(:)
      character(len=400), allocatable :: buffer_data

      this%Header = 'test_string'
      allocate (buffer_record(num_records_S))
      S_format = '(a72)'
      do i = 1, num_records_S
         read (fileunit, rec=S_record_num, fmt=S_format) &
            buffer_record(i)
         if (i >= 2) then
            buffer_data = buffer_record(i - 1)//buffer_record(i)
         else
            buffer_data = buffer_record(i)
         end if
         this%Header = buffer_data
         !print *, this%Header
      end do
   end subroutine read

end module Type_Start
