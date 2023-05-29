module read_raw_PD

   use count_PD

   implicit none

   private
   public :: read_Raw_Parametric_Data

contains

   subroutine read_Raw_Parametric_Data(fileunit, &
                                       n_PD_records, num_record_start, &
                                       num_PD_entries, &
                                       Type_ID, Raw_Parameter_Data)
      integer, intent(in)                  :: fileunit
      integer, intent(in)                  :: n_PD_records
      integer, intent(in)                  :: num_record_start
      integer, intent(out)                 :: num_PD_entries
      integer, intent(out)                 :: Type_ID
      character(len=4000), intent(out)      :: Raw_Parameter_Data
      integer                              :: PD_record_num = 0
      integer                              :: i = 0
      character(len=4000), allocatable     :: buffer(:)
      character(len=4000)                  :: buffer_total = ''
      character(len=10)                    :: string_format = ''
      character(len=20)                    :: read_format = ''

      allocate (buffer(n_PD_records))
      string_format = '(a64)'
      buffer = ''
      buffer_total = ''
      Raw_Parameter_Data = ''
      Type_ID = 0
      num_PD_entries = 0
      PD_record_num = num_record_start
      do i = 1, n_PD_records
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
      num_PD_entries = count_PD_entries(Raw_Parameter_Data)

      read_format = '(i3)'
      Raw_Parameter_Data = trim(Raw_Parameter_Data)
      read (Raw_Parameter_Data, fmt=read_format) Type_ID
   end subroutine read_Raw_Parametric_Data

end module read_raw_PD
