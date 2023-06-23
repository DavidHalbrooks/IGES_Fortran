module read_ascii_pd
   use iso_fortran_env, only: int32, int64, real32, real64
   implicit none

   private
   public :: read_pd

contains

   subroutine read_pd(fileunit, &
                      num_record_start, &
                      num_records, &
                      Type_ID, &
                      Raw_Parameter_Data)
      integer, intent(in)                  :: fileunit
      integer, intent(in)                  :: num_records
      integer, intent(in)                  :: num_record_start
      integer, intent(out)                 :: Type_ID
      character(len=4000), intent(out)     :: Raw_Parameter_Data
      integer                              :: PD_record_num = 0
      integer                              :: i = 0
      character(len=4000), allocatable     :: buffer(:)
      character(len=4000)                  :: buffer_total
      character(len=10)                    :: string_format
      character(len=20)                    :: read_format

      allocate (buffer(num_records))
      string_format = '(a64)'
      buffer = ''
      buffer_total = ''
      Raw_Parameter_Data = ''
      Type_ID = 0
      PD_record_num = num_record_start

      do i = 1, num_records
         read (fileunit, rec=PD_record_num, fmt=string_format) buffer(i)
         PD_record_num = PD_record_num + 1
         if (i < 2) then
            buffer(i) = trim(buffer(i))
         else
            buffer(i) = trim(buffer(i - 1))//trim(buffer(i))
         end if
         buffer_total = trim(buffer(i))
      end do
      Raw_Parameter_Data = buffer_total

   end subroutine read_pd

end module read_ascii_pd
