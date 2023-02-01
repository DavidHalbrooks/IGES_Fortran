module mod_open_close

  implicit none


contains

  subroutine open_file(filename, fileunit)
    character(len=200), intent(in) :: filename
    integer, intent(out)           :: fileunit
    character(len=6)  :: file_access        = 'direct'
    character(len=4)  :: file_action        = 'read'
    character(len=4)  :: file_delimiter     = 'none'
    character(len=10) :: file_format        = 'formatted'
    character(len=3)  :: file_status        = 'OLD'
    integer           :: file_record_length = 82
    ! Open file and assign fileunit
    print *
    write(*,*) '***************************************'
    write(*,*) 'Starting: open_file_Data_Format(this)'
    print *
    write(*,*) '  Filename opened: ', filename
    open(newunit=fileunit, file     = filename,  &
                           access   = file_access,    &
                           action   = file_action,    &
                           delim    = file_delimiter, &
                           form     = file_format,    &
                           status   = file_status,    &
                           recl     = file_record_length)
    !this%fileunit = file_unit
    write(*,*) '  The assigned fileunit:', fileunit
    print *
    write(*,*) 'Completed: open_file_Data_Format(this)'
    write(*,*) '***************************************'
  end subroutine open_file


  subroutine close_file(fileunit)
    integer, intent(in) :: fileunit
    close(fileunit)
  end subroutine close_file

end module mod_open_close
