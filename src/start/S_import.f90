module S_import

   implicit none

   private
   public :: S, read_S_data

   type S
      character(len=400) :: Start_Data = ''
   contains
      procedure :: read => read_S_data
   end type S

contains

   subroutine read_S_data(this, fileunit, num_records_S)
      class(S) :: this
      integer, intent(in)  :: fileunit
      integer, intent(in)  :: num_records_S
      integer              :: i = 0
      character(len=20)    :: S_format = ''
      integer              :: S_record_num = 1
      character(len=82), allocatable  :: buffer_record(:)
      character(len=400), allocatable :: buffer_data

      allocate (buffer_record(num_records_S))
      S_format = '(a72)'
      !write(*,*) 'Starting: read_S_Data(this)'
      do i = 1, num_records_S
         read (fileunit, rec=S_record_num, fmt=S_format) &
            buffer_record(i)
         if (i >= 2) then
            buffer_data = buffer_record(i - 1)//buffer_record(i)
         else
            buffer_data = buffer_record(i)
         end if
         this%Start_Data = buffer_data
         !write(*,*) '   this%Start_Data'
         !write(*,*) '   ', trim(this%Start_Data)
      end do
   end subroutine read_S_data

end module S_import
