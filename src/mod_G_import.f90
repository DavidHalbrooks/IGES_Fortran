module mod_G_import

  implicit none

  private
  public :: G, read_G_data

  type G
    character(len=10) :: parameter_delim        = ''
    character(len=80) :: record_delim           = ''
    character(len=80) :: product_id             = ''
    character(len=80) :: filename               = ''
    character(len=80) :: system_id              = ''
    character(len=80) :: preproc_version        = ''
    integer           :: int_binary_bits        = 0
    integer           :: max_power_single_float = 0
    integer           :: num_sig_digits         = 0
    integer           :: max_power_double       = 0
    integer           :: sig_digits_double      = 0
    character(len=80) :: product_id_recieving   = ''
    real              :: model_space_scale      = 0.0
    integer           :: units_flag             = 0
    character(len=80) :: units_name             = ''
    integer           :: max_num_line_weights   = 0
    real              :: max_line_weight        = 0.0
    character(len=80) :: date_time_generated    = ''
    real              :: min_resolution         = 0.0
    real              :: max_coord_value        = 0.0
    character(len=80) :: author                 = ''
    character(len=80) :: organization           = ''
    integer           :: version_flag           = 0
    integer           :: drafting_flag          = 0
    character(len=80) :: date_time_modified     = ''
    character(len=80) :: mil_spec               = ''
  contains
    procedure :: read      => read_G_data
  end type G

contains

  subroutine read_G_data(this, fileunit, &
                  n_records_G, num_record_start)
    class(G), intent(inout) :: this
    integer, intent(in) :: fileunit
    integer, intent(in) :: n_records_G
    integer, intent(in) :: num_record_start
    integer            :: i            = 0
    !character(len=20)  :: G_format     = ''
    character(len=20)  :: string_format = ''
    integer            :: G_record_num = 0
    character(len=400), allocatable :: buffer(:)
    character(len=400)  :: buffer_string
    !character(len=400)  :: G_Data_unformatted(26)
    !type(G)             :: test
    !integer            :: n_global_entries = 26

    allocate(buffer(n_records_G))
    G_record_num = num_record_start
    string_format = '(a72)'
    do i=1, n_records_G
      read(fileunit, rec=G_record_num, fmt=string_format) buffer(i)
      if (i < 2) then
        buffer(i) = trim(buffer(i))
      else
        buffer(i)  = trim(buffer(i-1))// trim(buffer(i))
      end if
      G_record_num = G_record_num + 1
      buffer_string = buffer(i)
    end do
    !read(buffer_string, delim=",") test
    write(*,*) buffer_string
  end subroutine read_G_data


end module mod_G_import
