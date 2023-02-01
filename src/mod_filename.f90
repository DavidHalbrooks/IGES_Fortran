module mod_filename

  implicit none

contains

  function set_filename() result(filename)
    character(len=100)  :: filename

    print *
    write(*,*) '***************************************'
    write(*,*) 'Starting: set_filename()'
    print *
    ! Set Variables
    filename = 'hollowvase.igs'  ! Iges filename to open
    filename = trim(filename)
    write(*,*) '  Filename : ', filename
    print *
    write(*,*) 'Completed: set_filename()'
    write(*,*) '***************************************'
  end function set_filename


end module mod_filename
