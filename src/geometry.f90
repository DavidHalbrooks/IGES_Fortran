module geometry

   use filename
   use num_records
   use open_close
   use T_import
   use S_import
   use G_import
   use DE_import
   use PD_import
   use array_utils
   use obj_T143

   implicit none

   private
   public :: Model, Entity

   type Model
      character(len=200)        :: filename
      integer                   :: fileunit
      integer                   :: n_records
      integer                   :: n_records_S
      integer                   :: n_records_G
      integer                   :: n_records_D
      integer                   :: n_records_P
      integer                   :: n_records_T
      type(S)                   :: Start_Data
      type(G)                   :: Global_Data
   contains
      procedure :: set_filename => set_filename_Model
      procedure :: get_num_records => get_num_records_Model
      procedure :: open_file => open_file_Model
      procedure :: read_sect_info => read_T_section_Model
      procedure :: read_S => read_Start_data
      procedure :: read_G => read_Global_data
      !procedure :: close_file      => close_file_Model
      procedure :: sub_calls => subroutine_calls_Model
   end type Model

   type, extends(Model) :: Entity
      type(Attributes), allocatable     :: ED(:)
      type(Parametric), allocatable     :: PD(:)
      integer                           :: n_elements_T143_array = 0
      integer, allocatable               :: T143_indices_array(:)
      type(T143_object), allocatable     :: T143_object(:)
   contains
      procedure :: alloc_ED => alloc_ED_Entity
      procedure :: read_attributes => read_attributes_Entity
      procedure :: alloc_PD => alloc_PD_Parameter
      !procedure :: read_parameter  => read_PD_Entity
      procedure :: alloc_objs => alloc_T143_objects
      procedure :: build_objs => build_T143_objects
      procedure :: sub_calls => subroutine_calls_Entity
   end type Entity

contains

   subroutine alloc_T143_objects(this)
      class(Entity), intent(inout) :: this
      allocate (this%T143_object(this%n_elements_T143_array))
   end subroutine alloc_T143_objects

   subroutine build_T143_objects(this)
      class(Entity) :: this
      integer :: i, j
      integer :: n_PD_143_records
      integer :: n_PD_128_records
      integer :: n_PD_141_records
      integer :: n_PD_126_records
      integer :: PD_ptr
      integer :: initial_PD_start_index
      integer :: initial_DE_start_index
      integer :: index_PD_record_start_T143
      integer :: index_PD_record_start_T128
      type(T143) :: T143_Data
      type(T128) :: T128_Data
      type(T141) :: T141_Data
      type(T126) :: T126_Data
      print *, '***************************************'
      print *, 'Starting: build_T143_objects(this)'
      print *
      do i = 1, this%n_elements_T143_array
         print *, 'Processing T143 object:', i
         n_PD_143_records = this%ED(this%T143_indices_array(i))%param_line_count
         initial_PD_start_index = this%n_records_S + &
                                  this%n_records_G + &
                                  this%n_records_D
         initial_DE_start_index = this%n_records_S + &
                                  this%n_records_G
         index_PD_record_start_T143 = initial_PD_start_index + &
                                      this%ED(this%T143_indices_array(i))%parameter_data
         call import_T143_data(this%PD(i), this%fileunit, &
                               n_PD_143_records, index_PD_record_start_T143)
         T143_Data = this%PD(i)%T143_Data
         write (*, *) T143_Data%Type_BS, T143_Data%SPTR, T143_Data%N, T143_Data%BDPT
         print *
         index_PD_record_start_T128 = initial_PD_start_index + &
                                      T143_Data%SPTR
         n_PD_126_records = this%ED(T143_Data%SPTR)%param_line_count
         PD_ptr = this%ED(T143_Data%SPTR)%parameter_data
         !> NOTE: The line bolow was commented out and needs to be fixed
         !write(*,*) '  A', this%PD(this%ED(T143_Data%
         write (*, *) '  B', this%fileunit
         write (*, *) '  C', n_PD_126_records
         write (*, *) '  D', index_PD_record_start_T128
         write (*, *) '  E', size(this%ED)
!      call import_T128_Data(this%ED(T143_Data%SPTR), this%fileunit, &
!                   this%ED(14), index_PD_record_start_T128)
      end do
      write (*, *) 'Read in parametric data for ', i - 1, 'Entities'
      print *
      write (*, *) 'Completed: build_T143_objects(this)'
      print *, '***************************************'
   end subroutine build_T143_objects

!  subroutine read_PD_Entity(this)
!    class(Entity) :: this
!    integer :: i
!    integer :: n_PD_records
!    integer :: num_PD_record_start
!    integer :: stride = 2
!    print *
!    write(*,*) '***************************************'
!    write(*,*) 'Starting: read_PD_Entity(this)'
!    print *
!    do i=1, size(this%PD), stride
!      print *
!      write(*,*) 'i :', i
!      n_PD_records = this%ED(i)%param_line_count
!      num_PD_record_start = this%n_records_S + this%n_records_G + &
!                            this%n_records_D + this%ED(i)%parameter_data
!      call read_raw(this%PD(i), this%fileunit, &
!                                n_PD_records, num_PD_record_start)
!      !call read_Raw_Parametric_data(this%PD(i), this%fileunit, &
!      !                          n_PD_records, num_PD_record_start)
!      !write(*,*) this%PD(i)%T143_Data%N
!    end do
!    write(*,*) 'Read in parametric data for ', (i-1)/2, 'Entities'
!    print *
!    write(*,*) 'Completed: read_PD_Entity(this)'
!    write(*,*) '***************************************'
!  end subroutine read_PD_Entity

   subroutine alloc_PD_Parameter(this)
      class(Entity) :: this
      allocate (this%PD(this%Model%n_records_D))
      print *
      write (*, *) 'Allocated memory for'
      print *
      write (*, *) '       ', size(this%ED), 'Parameter entries'
      print *
      write (*, *) 'Entity allocation complete'
   end subroutine alloc_PD_Parameter

   subroutine subroutine_calls_Model(this)
      class(Model) :: this
      call this%set_filename()
      call this%get_num_records()
      call this%open_file()
      call this%read_sect_info()
      call this%read_S
      call this%read_G
      !call this%read
      !call this%close_file()
   end subroutine subroutine_calls_Model

   subroutine subroutine_calls_Entity(this)
      class(Entity) :: this
      call subroutine_calls_Model(this)
      call this%alloc_ED()
      call this%alloc_PD()
      call this%read_attributes()
      !call this%read_parameter()
      call this%alloc_objs()
      call this%build_objs()
   end subroutine subroutine_calls_Entity

   subroutine read_Start_data(this)
      class(Model) :: this
      integer  :: num_record_start = 1
      print *
      write (*, *) '***************************************'
      write (*, *) 'Starting: read_Start_data(this)'
      num_record_start = this%n_records_S
      call read_S_data(this%Start_Data, this%fileunit, &
                       this%n_records_S)
      num_record_start = num_record_start + 2
      print *
      write (*, *) this%Start_Data
      print *
      write (*, *) 'Completed: read_Start_data(this)'
      write (*, *) '***************************************'
   end subroutine read_Start_data

   subroutine read_Global_data(this)
      class(Model) :: this
      integer  :: num_record_start
      print *
      write (*, *) '***************************************'
      write (*, *) 'Starting: read_Global_data(this)'
      print *
      num_record_start = this%n_records_S + 1
      call read_G_data(this%Global_Data, this%fileunit, &
                       this%n_records_G, num_record_start)
      print *
      write (*, *) 'Completed: read_Global_data(this)'
      write (*, *) '***************************************'
   end subroutine read_Global_data

   subroutine read_attributes_Entity(this)
      class(Entity)        :: this
      integer              :: i
      integer              :: num_record_start
      integer, parameter    :: stride = 2
      print *
      write (*, *) '***************************************'
      write (*, *) 'Starting: read_attributes_Entity(this)'
      print *
      allocate (this%T143_indices_array(0))
      this%T143_indices_array = 0
      num_record_start = this%n_records_S + this%n_records_G + 1
      do i = 1, size(this%ED), stride
         call read_entity_Attributes(this%ED(i), this%fileunit, &
                                     num_record_start)
         num_record_start = num_record_start + 2
!      write(*,*) this%ED(i)
         if (this%ED(i)%entity_type_num == 143) then
            call resize_array(this%T143_indices_array, &
                              this%n_elements_T143_array, &
                              this%ED(i)%sequence_num1)
!        print *
!        write(*,*) 'T143_indices_array :', this%T143_indices_array
!        write(*,*) 'n_array_elements   :', this%n_elements_T143_array
         end if
      end do
      write (*, *) '  Read in', (i - 1)/2, 'total entities from DE data'
      write (*, *) '  Read in', this%n_elements_T143_array, 'T143 Bounded Surface entities'
      print *
      !write(*,*) this%ED
      write (*, *) 'Completed: read_attributes_Entity(this)'
      write (*, *) '***************************************'
   end subroutine read_attributes_Entity

   subroutine alloc_ED_Entity(this)
      class(Entity) :: this
      allocate (this%ED(this%Model%n_records_D))
      print *
      write (*, *) 'Allocated memory for'
      print *
      write (*, *) '       ', size(this%ED), 'entities'
      print *
      write (*, *) 'Entity allocation complete'
   end subroutine alloc_ED_Entity

   subroutine set_filename_Model(this)
      class(Model) :: this
      this%filename = set_filename()
   end subroutine set_filename_Model

   subroutine get_num_records_Model(this)
      class(Model) :: this
      call get_num_records(this%filename, this%n_records)
   end subroutine get_num_records_Model

   subroutine open_file_Model(this)
      class(Model) :: this
      call open_file(this%filename, this%fileunit)
   end subroutine open_file_Model

   subroutine read_T_section_Model(this)
      class(Model) :: this
      call read_T_section_Iges(this%fileunit, this%n_records, &
                               this%n_records_S, this%n_records_G, &
                               this%n_records_D, this%n_records_P, &
                               this%n_records_T)
   end subroutine read_T_section_Model

end module geometry
