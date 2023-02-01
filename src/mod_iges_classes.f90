module mod_iges_classes

  use iso_fortran_env, only: int32, real32, &
                             stdin => input_unit,   &
                             stdout => output_unit, &
                             stderr => error_unit

  implicit none

  type Iges
    private
    character(len=100) :: filename
    integer(int32)     :: fileunit
  contains
    procedure :: init           => iges_init
    procedure :: print_debug    => iges_print_debug
    procedure :: close_file     => iges_close_file
  end type Iges

  type, extends(Iges) :: Section
    private
    integer(int32)   :: num_sections = 5
    character(len=1) :: Start
    character(len=1) :: Global
    character(len=1) :: Directory_Entry
    character(len=1) :: Parameter_Data
    character(len=1) :: Terminate
    contains
      procedure :: init   => section_init
  end type Section

  type, extends(Iges) :: Records
    private
    integer(int32) :: m_records_num(10)
    integer(int32) :: start_data_num
    integer(int32) :: global_data_num
    integer(int32) :: directory_entry_data_num
    integer(int32) :: parametric_data_num
    integer(int32) :: terminate_data_num
  contains
    procedure :: init            => records_init
    procedure :: get_records_num => records_num
    procedure :: get_records_num => records_num
    procedure :: print_debug     => records_print_debug
  end type Records


contains

  subroutine section_init(this)
    class(Sections)  :: this
    Start = 'S'
    Global          = 'G'
    Directory_Entry = 'D'
    Parameter_Data  = 'P'
    Terminate       = 'T'
  end subroutine section_init

  subroutine iges_init(this)
    class(Iges)         :: this
    character(len=100)  :: filename
    integer(int32)      :: fileunit
    character(len=20)   :: file_position
    character(len=20)   :: file_action

    write(*,*) 'Starting : iges_init(this)'

    ! Initialize variables
    filename      = 'ENTER_IGES_FILENAME'
    fileunit      = 0
    file_position = 'rewind'
    file_action   = 'read'

    ! Iges filename to open
    filename        = 'hollowvase.igs'
    this % filename = trim(filename)

    ! Open file and assign fileunit
    open(newunit=fileunit, file=trim(this%filename), &
         position=trim(file_position), action=trim(file_action))
    this % fileunit = fileunit

    write(*,*) 'Completed: iges_init(this)'
  end subroutine iges_init


  subroutine iges_print_debug(this)
    class(Iges) :: this

    print *
    write(*,*) 'Starting : iges_print_debug(this)'
    print *
    write(*,*) 'IGES filename read into the program is: ', this%filename
    print *
    write(*,*) "The filenunit that's been assigned is: ", this%fileunit
    print *
    write(*,*) 'Completed: iges_print_debug(this)'
  end subroutine iges_print_debug


  subroutine iges_close_file(this)
    class(Iges) :: this

    print *
    write(*,*) 'Starting : iges_close_file(this)'
    close(this % fileunit)
    write(*,*) 'Completed: iges_close_file(this)'
  end subroutine iges_close_file

  subroutine records_init(this)
    class(Records)     :: this

    print *
    write(*,*) 'Starting : records_init(this)'

    write(*,*) 'Calling  : iges_init(this)'
    call iges_init(this)

    this % m_records_num = 0

    write(*,*) 'Completed: records_init(this)'
  end subroutine records_init

  subroutine records_num(this)
    class(Records)    :: this
    integer(int32)    :: file_io
    integer(int32)    :: nrecords

    print *
    write(*,*) 'Starting : records_num(this)'
    ! Initialize variables
    nrecords = 0
    file_io  = 0

    rewind(this%fileunit)
    do
      read(this%fileunit,*, iostat=file_io)
        !print *, file_io
        if (file_io/=0) exit
      nrecords = nrecords + 1
      !print *, nrecords
    end do
    rewind(this%fileunit)

    this % m_records_num(1) = nrecords

    write(*,*) 'Completed: records_num(this)'
  end subroutine records_num

  subroutine records_print_debug(this)
    class(Records) :: this

    print *
    write(*,*) 'Starting : records_print_debug(this)'
    ! Debugging information
    print *
    write(*,*) 'The filename in records_num(this) is: ', this%filename
    print *
    write(*,*) 'The filenunit in records_num_file(this) is: ', this%fileunit
    print *
    write(*,*) 'The total number of records in the IGES file is: ', &
                this % m_records_num(1)
    write(*,*) 'Completed: records_printy_debug(this)'

  end subroutine records_print_debug

  subroutine terminate_data_init(this)
    class(TerminateData) :: this

    print *
    write(*,*) 'Starting : terminate_data_init(this)'

    write(*,*) 'Calling  : records_init(this)'
    call records_init(this)

    this % m_records_num = 0

    write(*,*) 'Completed: terminate_data_init(this)'

  end subroutine terminate_data_init

end module mod_iges_classes
