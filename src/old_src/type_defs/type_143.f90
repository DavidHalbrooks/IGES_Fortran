module type_143
   use iso_fortran_env, only: int32, int64, real32, real64
   use read_ascii_pd, only: read_pd
   use count_pd, only: count_pd_entries
   use type_128, only: t_128
   use type_141 !, only: t_141
   implicit none

   private

   type, public :: t_143_dir
      integer(int32) :: entity_type_num1 = 0
      integer(int32) :: parameter_data = 0
      integer(int32) :: structure = 0
      integer(int32) :: line_font = 0
      integer(int32) :: level = 0
      integer(int32) :: view = 0
      integer(int32) :: transform_matrix = 0
      integer(int32) :: label_disp_assoc = 0
      integer(int32) :: status_number = 0
      integer(int32) :: sequence_num1 = 0
      integer(int32) :: entity_type_num2 = 0
      integer(int32) :: line_weight = 0
      integer(int32) :: color_number = 0
      integer(int32) :: param_line_count = 0
      integer(int32) :: form_number = 0
      integer(int32) :: reserved_16 = 0
      integer(int32) :: reserved_17 = 0
      integer(int32) :: entity_label = 0
      integer(int32) :: entity_subscript = 0
      integer(int32) :: sequence_num2 = 0
   end type t_143_dir

   type, public :: t_143_pd
      integer(int32)              :: Type_BS = 0
      integer(int32)              :: SPTR = 0
      integer(int32)              :: N = 0
      integer(int32), allocatable :: BDPT(:)
   end type t_143_pd

   !! NOTE: THIS MAY NOT BE NEEDED
   type, public :: t_143_deps
      type(t_128) :: dep_t128
      !type(t_128) :: dep_t141p
      !type(t_141) :: t128_data
   end type t_143_deps

   type, public :: t_143_metadata
      integer(int32)              :: Type_ID = 0
      character(len=4000)         :: ascii_t143_pd_data = ''
      integer(int32)              :: num_PD_entries = 0
   end type t_143_metadata

   type, private :: share_fileinfo_data
      integer(int32) :: fileunit
      integer(int32) :: D_section_start_index
      integer(int32) :: P_section_start_index
   end type share_fileinfo_data

   type, public :: t_143
      type(t_143_dir)            :: t143dir
      type(t_143_pd)             :: t143param
      type(t_143_metadata)       :: t143metadata
      type(share_fileinfo_data)  :: t143sharedata
      type(t_128)                :: dep_t128
      type(t_141), allocatable   :: dep_t141(:)
   contains
      procedure :: exchange_share_data => exchange_share
      procedure :: read_t143_dir_entries => read_entries
      procedure :: print_t143_dir_entries => print_entries
      procedure :: read_t143_pd_entries => read_pd_entries
      procedure :: read_t128_data => read_t128
      procedure :: allocate_t141_array => allocate_t141
      procedure :: read_t141_data => read_t141
   end type t_143

contains

   subroutine exchange_share(this, fileunit, D_start_index, P_start_index)
      class(t_143), intent(inout) :: this
      integer(int32), intent(in) :: fileunit
      integer(int32), intent(in) :: D_start_index
      integer(int32), intent(in) :: P_start_index
      ! Send the fileunit to share_data for use by children types
      this%t143sharedata%fileunit = fileunit
      this%t143sharedata%D_section_start_index = D_start_index
      this%t143sharedata%P_section_start_index = P_start_index

   end subroutine exchange_share

   subroutine read_entries(this, record_start_index)
      class(t_143), intent(inout)    :: this
      integer(int32), intent(in)     :: record_start_index
      integer(int32)                 :: D_record_num
      integer(int32), parameter      :: record_span = 2
      integer(int32), parameter      :: num_dir_entries = 20
      character(len=80), allocatable :: buffer(:)
      character(len=160)             :: buffer_ascii
      character(len=1)               :: type_letter1
      character(len=1)               :: type_letter2
      integer                        :: i

      D_record_num = record_start_index

      allocate (buffer(record_span))
      do i = 1, record_span
         read (this%t143sharedata%fileunit, rec=D_record_num, fmt='(a80)') buffer(i)
         buffer_ascii = buffer(i - 1)//buffer(i)
         D_record_num = D_record_num + 1
      end do

      read (buffer_ascii, fmt='(9i8,a1,i7,9i8,a1,i7)') &
         this%t143dir%entity_type_num1, &
         this%t143dir%parameter_data, &
         this%t143dir%structure, &
         this%t143dir%line_font, &
         this%t143dir%level, &
         this%t143dir%view, &
         this%t143dir%transform_matrix, &
         this%t143dir%label_disp_assoc, &
         this%t143dir%status_number, &
         type_letter1, &
         this%t143dir%sequence_num1, &
         this%t143dir%entity_type_num2, &
         this%t143dir%line_weight, &
         this%t143dir%color_number, &
         this%t143dir%param_line_count, &
         this%t143dir%form_number, &
         this%t143dir%reserved_16, &
         this%t143dir%reserved_17, &
         this%t143dir%entity_label, &
         this%t143dir%entity_subscript, &
         type_letter2, &
         this%t143dir%sequence_num2

   end subroutine read_entries

   subroutine print_entries(this)
      class(t_143), intent(inout) :: this
      print *
      print *, this%t143dir
   end subroutine print_entries

   subroutine read_pd_entries(this)
      class(t_143), intent(inout)    :: this
      integer(int32)                 :: record_start_index
      integer(int32)                 :: num_records
      integer(int32)                 :: num_pd_entries
      integer(int32)                 :: type_id
      integer(int32)                 :: temp
      character(len=4000)            :: ascii_pd_buffer

      ascii_pd_buffer = ''
      num_pd_entries = 0
      temp = 0

      record_start_index = (this%t143sharedata%P_section_start_index + &
                            this%t143dir%parameter_data - 1)
      call read_pd(this%t143sharedata%fileunit, &
                   record_start_index, &
                   this%t143dir%param_line_count, &
                   type_id, &
                   ascii_pd_buffer)
      this%t143metadata%num_pd_entries = count_pd_entries(ascii_pd_buffer)

      read (ascii_pd_buffer, fmt=*) temp, temp, temp, this%t143param%N
      allocate (this%t143param%BDPT(this%t143param%N))

      print *
      print *, trim(ascii_pd_buffer)
      ascii_pd_buffer = trim(ascii_pd_buffer)
      read (ascii_pd_buffer, fmt=*) &
         this%t143metadata%Type_ID, &
         this%t143param%Type_BS, &
         this%t143param%SPTR, &
         this%t143param%N, &
         this%t143param%BDPT

      print *, this%t143param%BDPT

   end subroutine read_pd_entries

   subroutine read_t128(this)
      class(t_143), intent(inout) :: this
      call this%dep_t128%exchange_sharedata_t128(this%t143sharedata%fileunit, &
                                                 this%t143sharedata%D_section_start_index, &
                                                 this%t143sharedata%P_section_start_index)
      call this%dep_t128%read_t128_dir_entries(this%t143param%SPTR)
      !call this%t128dep%t128dir%print_t128_dir_entries
      call this%dep_t128%read_ascii_t128_pd
      call this%dep_t128%allocate_t128_pd_initial
      call this%dep_t128%read_t128pd_integers
      call this%dep_t128%t128_index_calculations
      call this%dep_t128%allocate_t128_pd_vectors
      call this%dep_t128%initialize_t128_pd_vectors
      call this%dep_t128%read_in_t128_pd_data
      call this%dep_t128%print_t128_data
   end subroutine read_t128

   subroutine allocate_t141(this)
      class(t_143), intent(inout) :: this
      allocate (this%dep_t141(this%t143param%N))
   end subroutine allocate_t141

   subroutine read_t141(this)
      class(t_143), intent(inout) :: this
      integer(int32) :: i

      print *, 'param%N :', this%t143param%N
      do i = 1, this%t143param%N
         print *, 't143sharedata :', this%t143sharedata

         call this%dep_t141(i)%exchange_sharedata_t141(this%t143sharedata%fileunit, &
                                                       this%t143sharedata%D_section_start_index, &
                                                       this%t143sharedata%P_section_start_index)
         ! call this%dep_t128%read_t128_dir_entries(this%t143param%SPTR)
         ! !!call this%t128dep%t128dir%print_t128_dir_entries
         ! call this%dep_t128%read_ascii_t128_pd
         ! call this%dep_t128%allocate_t128_pd_initial
         ! call this%dep_t128%read_t128pd_integers
         ! call this%dep_t128%t128_index_calculations
         ! call this%dep_t128%allocate_t128_pd_vectors
         ! call this%dep_t128%initialize_t128_pd_vectors
         ! call this%dep_t128%read_in_t128_pd_data
         ! call this%dep_t128%print_t128_data
      end do
   end subroutine read_t141

end module type_143
