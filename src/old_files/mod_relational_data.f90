module mod_relational_data

  use iso_fortran_env, only: int32, real32, &
                             stdin  => input_unit,  &
                             stdout => output_unit, &
                             stderr => error_unit

  implicit none

  private
  public :: Model, Iges, S, G, DE

  type Model
    character(len=100)  :: filename        = 'INIT_FILENAME'
    integer(int32)      :: fileunit        = 0
    integer             :: total_n_records = 0
  contains
    procedure :: get_filename    => get_filename_Model
    procedure :: get_num_records => get_num_records_Model
    procedure :: open_file       => open_file_Model
    procedure :: close_file      => close_file_Model
  end type Model

  type, extends(Model) :: Iges
    integer  :: n_records_S = 0
    integer  :: n_records_G = 0
    integer  :: n_records_D = 0
    integer  :: n_records_P = 0
    integer  :: n_records_T = 0
  contains
    procedure :: read_sect_info => read_T_section_Iges
  end type Iges

  type, extends(Iges) :: S
    character(len=800), allocatable :: Data(:)
    character(len=1), allocatable   :: ID(:)
    integer, allocatable            :: Pointer(:)
  contains
    procedure :: alloc => alloc_S
    procedure :: init  => init_S
    procedure :: read  => read_S
    procedure :: sub_calls  => subroutine_calls_S
  end type S

  type, extends(Iges) :: G
    character(len=800), allocatable :: Data(:)
    character(len=1), allocatable   :: ID(:)
    integer, allocatable            :: Pointer(:)
  contains
    procedure :: alloc => alloc_G
    procedure :: init  => init_G
    procedure :: read  => read_G
    procedure :: parse => parse_G
    procedure :: sub_calls  => subroutine_calls_G
  end type G

  type, extends(Iges) :: DE
    integer :: entity_type_num
    integer :: parameter_data
    integer :: structure
    integer :: line_font
    integer :: level
    integer :: view
    integer :: transform_matrix
    integer :: label_disp_assoc
    integer :: status_number
    integer :: sequence_number1
    integer :: entity_type_num_2
    integer :: line_weight_num
    integer :: color_number
    integer :: param_line_count
    integer :: form_number
    integer :: reserved_16
    integer :: reserved_17
    integer :: entity_label
    integer :: entity_subscript
    integer :: sequence_num2
    integer :: n_DE_entries
    integer, allocatable :: Data(:)
  contains
    procedure :: alloc => alloc_DE
    procedure :: init  => init_DE
    procedure :: read  => read_DE
    !procedure :: parse => parse_DE
    procedure :: sub_calls => subroutine_calls_DE
  end type DE

contains

  subroutine read_DE(this)
    class(DE) :: this
    integer            :: i            = 0
    integer            :: DE_record_num = 0
    character(len=200) :: buffer1 = ''
    character(len=200) :: buffer2 = ''
    character(len=100) :: buffer_format = ''
    character(len=180) :: buffer3 = ''
    character(len=400) :: DE_format = ''
    integer            :: DE_ints1(9)
    integer            :: DE_ints3
    integer            :: DE_ints4(9)
    integer            :: DE_ints6
    character          :: DE_chars2
    character          :: DE_chars5

    write(*,*) 'Starting: read_DE(this)'
    DE_record_num = this%n_records_S + this%n_records_G + 1
    DE_format = '(9(i8), (a), (i7), 9(i8), (a), (i7))'
    buffer_format = '(a80)'
    do i=1, this%n_DE_entries
      buffer1 = ''
      buffer2 = ''
      buffer3 = ''
      read(this%fileunit, rec=DE_record_num, fmt=buffer_format) buffer1
      DE_record_num = DE_record_num + 1
      read(this%fileunit, rec=DE_record_num, fmt=buffer_format) buffer2
      DE_record_num = DE_record_num + 1
      buffer3 = trim(buffer1) // trim(buffer2)
      read(buffer3, fmt=DE_format) DE_ints1, DE_chars2, DE_ints3, &
                                   DE_ints4, DE_chars5, DE_ints6
      write(*,*) DE_ints1, DE_chars2, DE_ints3, &
                 DE_ints4, DE_chars5, DE_ints6
    end do
    print *
    write(*,*) 'Completed: read_DE(this)'
  end subroutine read_DE


  subroutine subroutine_calls_DE(this)
    class(DE) :: this
    call this%get_filename()
    call this%get_num_records()
    call this%open_file()
    call this%read_sect_info()
    call this%alloc
    call this%init
    call this%read
    call this%close_file()
  end subroutine subroutine_calls_DE

  subroutine alloc_DE(this)
    class(DE) :: this
    !integer  :: i

    this%n_DE_entries = (this%n_records_D)/2
    write(*,*) this%Iges%Model%total_n_records

    write(*,*) 'Starting: alloc_DE(this)'
    !write(*,*) 'this%n_records_D :', this%n_records_D
    allocate(this%Data(this%n_DE_entries))
    !write(*,*) 'The size of this%Data is: ', size(this%Data)
    write(*,*) 'Completed: alloc_DE(this)'
  end subroutine alloc_DE

  subroutine init_DE(this)
    class(DE) :: this
    integer       :: i

    write(*,*) 'Starting: init_DE(this)'
    do i=1, this%n_DE_entries
      this%Data    = 0
    end do
    write(*,*) 'Completed: init_DE(this)'
  end subroutine init_DE

  subroutine parse_G(this)
    class(G) :: this
    character(len=80) :: G_format               = ''
    character(len=2)  :: parameter_delim        = ''
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

    character(len=400) :: buffer = ''
    integer           :: n_global_entries       = 0
    integer           :: i                      = 0
    n_global_entries = 26

    !buffer = this%all_G_Data
    !allocate(this%Data(n_global_entries))
    !G_format = '((a72), (a), (i7))'
    !read(this%all_G_Data, fmt=*, iostat=rc) this%Data

  end subroutine parse_G

  subroutine subroutine_calls_S(this)
    class(S) :: this
    call this%get_filename()
    call this%get_num_records()
    call this%open_file()
    call this%read_sect_info()
    call this%alloc
    call this%init
    call this%read
    call this%close_file()
  end subroutine subroutine_calls_S

  subroutine subroutine_calls_G(this)
    class(G) :: this
    call this%get_filename()
    call this%get_num_records()
    call this%open_file()
    call this%read_sect_info()
    call this%alloc
    call this%init
    call this%read
    call this%parse
    call this%close_file()
  end subroutine subroutine_calls_G

  subroutine get_filename_Model(this)
    class(Model)        :: this
    character(len=100)  :: filename

    write(*,*) 'Starting  : get_filename_DataFormat(this)'
    ! Set Variables
    filename        = 'hollowvase.igs'  ! Iges filename to open
    this % filename = trim(filename)
    write(*,*) '  Filename : ', this%filename
    write(*,*) 'Completed : get_filename_DataFormat(this)'
  end subroutine get_filename_Model

  subroutine get_num_records_Model(this)
    class(Model)      :: this
    integer           :: file_io       = 0
    integer           :: file_unit     = 0
    character(len=10) :: file_access   = 'sequential'
    character(len=4)  :: file_action   = 'read'
    character(len=6)  :: file_position = 'rewind'
    character(len=3)  :: file_status   = 'old'
    integer(int32)    :: n = 0
    this % total_n_records = 0
    print *
    write(*,*) 'Starting : records_num(this)'
    open(newunit=file_unit, file     = this%filename, &
                            access   = file_access,   &
                            action   = file_action,   &
                            position = file_position, &
                            status   = file_status)
    n = 0
    do
      read(file_unit,*, iostat=file_io)
        !print *, file_io
        if (file_io/=0) exit
      n = n + 1
      !print *, n
    end do
    close(file_unit)
    this % total_n_records = n
    write(*,*) 'Total number of records: ', this%total_n_records
    write(*,*) 'Completed: records_num(this)'
  end subroutine get_num_records_Model

  subroutine open_file_Model(this)
    class(Model)   :: this
    integer           :: fileunit           = 0
    character(len=6)  :: file_access        = 'direct'
    character(len=4)  :: file_action        = 'read'
    character(len=4)  :: file_delimiter     = 'none'
    character(len=10) :: file_format        = 'formatted'
    character(len=3)  :: file_status        = 'OLD'
    integer           :: file_record_length = 82
    ! Open file and assign fileunit
    print *
    write(*,*) 'Starting: open_file_Data_Format(this)'
    write(*,*) '  Filename : ', this%filename
    open(newunit=fileunit, file     = this%filename,  &
                           access   = file_access,    &
                           action   = file_action,    &
                           delim    = file_delimiter, &
                           form     = file_format,    &
                           status   = file_status,    &
                           recl     = file_record_length)
    this%fileunit = fileunit
    write(*,*) '  this%fileunit :', this%fileunit
    write(*,*) 'Completed: open_file_Data_Format(this)'
  end subroutine open_file_Model

  subroutine close_file_Model(this)
    class(Model) :: this
    !integer      :: fileunit

    print *
    write(*,*) 'Starting : close_file_DataFormat(this)'
    write(*,*) 'this%fileunit :', this%fileunit
    close(this%fileunit)
    write(*,*) 'Completed: close_file_Data_Format(this)'
  end subroutine close_file_Model


  subroutine read_T_section_Iges(this)
    class(Iges) :: this
    integer           :: T_record_num = 0
    character(len=80) :: T_format
    character(len=8)  :: buffer_char = ''
    integer           :: buffer_int  = 0

    !call this%get_filename()
    !call this%get_num_records()
    !call this%open_file()

    T_record_num = this%total_n_records
    print *
    write(*,*) 'Starting : read_T_section_Iges(this)'
    T_format = '(4(a1,i7), 5(i8), (a1,i7))'
    read(this%fileunit, rec=T_record_num, fmt=T_format) &
                                               buffer_char,      &
                                               this%n_records_S, &
                                               buffer_char,      &
                                               this%n_records_G, &
                                               buffer_char,      &
                                               this%n_records_D, &
                                               buffer_char,      &
                                               this%n_records_P, &
                                               buffer_int,       &
                                               buffer_int,       &
                                               buffer_int,       &
                                               buffer_int,       &
                                               buffer_int,       &
                                               buffer_char,      &
                                               this%n_records_T
    write(*,*) 'The T data read in is: ', &
                        this%n_records_S, &
                        this%n_records_G, &
                        this%n_records_D, &
                        this%n_records_P, &
                        this%n_records_T
    write(*,*) 'Completed: read_T_section_Iges(this)'

    !call this%close_file()
  end subroutine read_T_section_Iges

  subroutine alloc_S(this)
    class(S) :: this

    write(*,*) 'Starting: alloc_S_Data(this)'
    !call this%read_sect_info()
    allocate(this%Data(this%n_records_S))
    allocate(this%ID(this%n_records_S))
    allocate(this%Pointer(this%n_records_S))
    !write(*,*) 'The size of this%Data is: ', size(this%Data)
    !write(*,*) 'The size of this%ID is: ', size(this%ID)
    !write(*,*) 'The size of this%POINTER is: ', size(this%Pointer)
    write(*,*) 'Completed: alloc_S_Data(this)'
  end subroutine alloc_S

  subroutine init_S(this)
    class(S) :: this
    integer       :: i

    write(*,*) 'Starting: init_S_Data(this)'
    do i=1, this%n_records_S
      this%Data(i)    = 'INITIALIZED S DATA'
      this%ID(i)      = 'O'
      this%Pointer(i) = 0
    end do
    write(*,*) 'Completed: init_S_Data(this)'
  end subroutine init_S

  subroutine read_S(this)
    class(S) :: this
    integer            :: i            = 0
    character(len=20)  :: S_format     = ''
    integer            :: S_record_num = 1
    character(len=82), allocatable  :: buffer_record(:)
    character(len=400), allocatable :: buffer_data

    write(*,*) 'The S data is :', this%Iges%n_records_S
    print *
    allocate(buffer_record(this%n_records_S))
    S_format = '((a72), (a), (i7))'
    write(*,*) 'Starting: read_S_Data(this)'
    do i=1, this%n_records_S
      read(this%fileunit, rec=S_record_num, fmt=S_format) &
           buffer_record(i), this%ID(i), this%Pointer(i)
      if (i >= 2) then
        buffer_data = buffer_record(i-1)//buffer_record(i)
      else
        buffer_data = buffer_record(i)
      end if
      this%Data(i) = buffer_data
      write(*,*) '   this%Data(i), this%ID(i), this%Pointer(i) :'
      write(*,*) '   ', trim(this%Data(i)), this%ID(i), this%Pointer(i)
    end do
    write(*,*) 'Completed: read_S(this)'
  end subroutine read_S


  subroutine alloc_G(this)
    class(G) :: this
    !integer  :: i

    write(*,*) this%Iges%Model%total_n_records

    write(*,*) 'Starting: alloc_G(this)'
    !call this%read_sect_info()
    write(*,*) 'this%n_records_G :', this%n_records_G
    allocate(this%Data(this%n_records_G))
    allocate(this%ID(this%n_records_G))
    allocate(this%Pointer(this%n_records_G))
    write(*,*) 'The size of this%Data is: ', size(this%Data)
    write(*,*) 'The size of this%ID is: ', size(this%ID)
    write(*,*) 'The size of this%POINTER is: ', size(this%Pointer)
    write(*,*) 'Completed: alloc_G(this)'
  end subroutine alloc_G

  subroutine init_G(this)
    class(G) :: this
    integer       :: i

    write(*,*) 'Starting: init_G(this)'
    do i=1, this%n_records_G
      this%Data(i)    = 'INITIALIZED S DATA'
      this%ID(i)      = 'O'
      this%Pointer(i) = 0
    end do
    write(*,*) 'Completed: init_G(this)'
  end subroutine init_G

  subroutine read_G(this)
    class(G) :: this
    integer            :: i            = 0
    character(len=20)  :: G_format     = ''
    integer            :: G_record_num = 0
    character(len=400) :: buffer_record
    character(len=400) :: buffer
    character(len=400) :: all_G_Data
    integer            :: n_global_entries = 26

    write(*,*) 'Starting: read_G(this)'
    G_record_num = this%n_records_S + 1
    G_format = '((a72), (a), (i7))'
    do i=1, this%n_records_G
      read(this%fileunit, rec=G_record_num, fmt=G_format) &
           this%Data(i), this%ID(i), this%Pointer(i)
      G_record_num = G_record_num + 1
    end do
    buffer = ''
    buffer_record = ''
    do i = 1, this%n_records_G
      if (i < 2) then
        buffer = trim(this%Data(i))
        buffer_record = buffer
        !print *, trim(buffer_record)
      else
        buffer  = trim(buffer_record)// trim(this%Data(i))
        buffer_record = trim(buffer)
        !print *, trim(buffer_record)
      end if
    end do
    all_G_Data = trim(buffer)
    write(*,*) all_G_Data
    !write(*,*) '   ', this%Data, this%ID, this%Pointer
    !allocate(this%Data(n_global_entries))
    read(all_G_Data, fmt=*) this%Data
    print *
    print *, 'this%Data'
    write(*,*) this%Data
    write(*,*) 'Completed: read_G(this)'
  end subroutine read_G



end module mod_relational_data
