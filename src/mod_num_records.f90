module mod_num_records

  implicit none

private
public :: get_num_records


contains

  subroutine get_num_records(filename, n_records)
    character(len=200),intent(in) :: filename
    integer              :: file_io       = 0
    integer              :: file_unit     = 0
    character(len=10)    :: file_access   = 'sequential'
    character(len=4)     :: file_action   = 'read'
    character(len=6)     :: file_position = 'rewind'
    character(len=3)     :: file_status   = 'old'
    integer              :: n             = 0
    integer, intent(out) :: n_records

    print *
    write(*,*) '***************************************'
    write(*,*) 'Starting: get_num_records'
    print *
    open(newunit=file_unit, file     = filename,      &
                            access   = file_access,   &
                            action   = file_action,   &
                            position = file_position, &
                            status   = file_status)
    do
      read(file_unit,*, iostat=file_io)
        !print *, file_io
        if (file_io/=0) exit
      n = n + 1
      !print *, n
    end do
    close(file_unit)
    n_records = n
    write(*,*) 'Total number of records: ', n_records
    print *
    write(*,*) 'Completed: get_num_records'
    write(*,*) '***************************************'
  end subroutine get_num_records

end module mod_num_records
