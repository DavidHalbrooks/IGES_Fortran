module filename

   implicit none

contains

   function set_filename() result(filename)
      character(len=512)  :: filename

      ! print *
      ! write (*, *) '***************************************'
      ! write (*, *) 'Starting: set_filename()'
      ! print *
      ! Set Variables
      !filename = './iges_files/hollowvase.igs'  ! Iges filename to open
      filename = './iges_files/Hand_Wheel2.igs'  ! Iges filename to open
      filename = trim(filename)
      ! write (*, *) '  Filename : ', filename
      ! !print *
      ! write (*, *) 'Completed: set_filename()'
      ! write (*, *) '***************************************'
   end function set_filename

end module filename
