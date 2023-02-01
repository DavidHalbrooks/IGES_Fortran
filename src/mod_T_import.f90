module mod_T_import

  implicit none

  private
  public :: read_T_section_Iges

contains

  subroutine read_T_section_Iges(fileunit,T_record_num, &
                                  nS, nG, nD, nP, nT)
    integer, intent(in)  :: fileunit
    integer, intent(in)  :: T_record_num
    integer, intent(out) :: nS
    integer, intent(out) :: nG
    integer, intent(out) :: nD
    integer, intent(out) :: nP
    integer, intent(out) :: nT
    character            :: S, G, D, P, T
    integer              :: blanks(5)=0
    character(len=100)   :: T_record_format

    print *
    write(*,*) '***************************************'
    write(*,*) 'Starting: read_T_section_Iges()'

    T_record_format = '(4(a1,i7), 5(i8), (a1,i7))'
    read(fileunit, rec=T_record_num, fmt=T_record_format) S, nS, &
                                                          G, nG, &
                                                          D, nD, &
                                                          P, nP, &
                                                          blanks, &
                                                          T, nT
    print *
    write(*,*) 'Completed: read_T_section_Iges()'
    write(*,*) '***************************************'

  end subroutine read_T_section_Iges


end module mod_T_import
