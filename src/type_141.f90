module type_141
   use iso_fortran_env, only: int32, int64, real32, real64
   use read_ascii_pd, only: read_pd
   use count_pd, only: count_pd_entries
   use type_128
   use type_126

   implicit none

   private

   type, public :: t_141_dir
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
   end type t_141_dir

   type, public :: t_141_pd
      integer(int32)              :: Type_BS = 0
      integer(int32)              :: PREF = 0
      integer(int32)              :: SPTR = 0
      integer(int32)              :: N = 0
      integer(int32), allocatable :: CRVPT(:)
      integer(int32), allocatable :: SENSE(:)
      integer(int32), allocatable :: K(:)
      integer(int32), allocatable :: PSCPT(:, :)
   end type t_141_pd

   type, public :: t_141_metadata
      integer(int32)              :: Type_ID = 0
      integer(int32)              :: M = 0
      character(len=160)          :: ascii_t141_dir_data = ''
      character(len=4000)         :: ascii_t141_pd_data = ''
      integer(int32)              :: num_PD_entries = 0
   end type t_141_metadata

   type, private :: t_141_sharedata
      integer(int32) :: fileunit = 0
      integer(int32) :: D_section_start_index = 0
      integer(int32) :: P_section_start_index = 0
   end type t_141_sharedata

   type, public :: t_141
      type(t_141_dir)          :: t141dir
      type(t_141_pd)           :: t141param
      type(t_141_metadata)     :: t141metadata
      type(t_141_sharedata)    :: t141sharedata
      type(t_128)              :: dep_t128
      type(t_126), allocatable :: dep_t126(:)

   contains
      procedure :: exchange_sharedata_t141 => exchange_share
      procedure :: read_t141_dir_entries => read_entries
      procedure :: read_ascii_t141_pd => read_ascii_pd
      procedure :: read_t141pd_ints_initial => read_pd_ints_initial
      procedure :: allocate_t141_pd_vectors => allocate_pd_vectors
      procedure :: t141_index_calculations => index_calcs
      procedure :: read_t141_pd_vectors => read_pd_vectors
      procedure :: print_t141_dir_data => print_dir_data
      procedure :: print_t141_pd_data => print_pd_data
      procedure :: read_t128_data => read_t128
      procedure :: read_t126_data => read_t126
   end type t_141

contains

   subroutine exchange_share(this, fileunit, D_start_index, P_start_index)
      class(t_141), intent(inout) :: this
      integer(int32), intent(in)  :: fileunit
      integer(int32), intent(in)  :: D_start_index
      integer(int32), intent(in)  :: P_start_index
      this%t141sharedata%fileunit = fileunit
      this%t141sharedata%D_section_start_index = D_start_index
      this%t141sharedata%P_section_start_index = P_start_index
   end subroutine exchange_share

   subroutine read_entries(this, D_record_index)
      class(t_141), intent(inout)    :: this
      integer(int32), intent(in)     :: D_record_index
      integer(int32)                 :: D_record_num
      integer, parameter             :: record_span = 2
      integer, parameter             :: num_dir_entries = 20
      character(len=80), allocatable :: buffer(:)
      character(len=160)             :: buffer_ascii
      character(len=1)               :: type_letter1
      character(len=1)               :: type_letter2
      integer                        :: i
      D_record_num = (D_record_index + this%t141sharedata%D_section_start_index - 1)
      allocate (buffer(record_span))
      do i = 1, record_span
         read (this%t141sharedata%fileunit, rec=D_record_num, fmt='(a80)') buffer(i)
         D_record_num = D_record_num + 1
         if (i < 2) then
            buffer(i) = trim(buffer(i))
         else
            buffer_ascii = trim(buffer(i - 1))//trim(buffer(i))
         end if
      end do
      this%t141metadata%ascii_t141_dir_data = buffer_ascii
      read (buffer_ascii, fmt='(9i8,a1,i7,9i8,a1,i7)') &
         this%t141dir%entity_type_num1, &
         this%t141dir%parameter_data, &
         this%t141dir%structure, &
         this%t141dir%line_font, &
         this%t141dir%level, &
         this%t141dir%view, &
         this%t141dir%transform_matrix, &
         this%t141dir%label_disp_assoc, &
         this%t141dir%status_number, &
         type_letter1, &
         this%t141dir%sequence_num1, &
         this%t141dir%entity_type_num2, &
         this%t141dir%line_weight, &
         this%t141dir%color_number, &
         this%t141dir%param_line_count, &
         this%t141dir%form_number, &
         this%t141dir%reserved_16, &
         this%t141dir%reserved_17, &
         this%t141dir%entity_label, &
         this%t141dir%entity_subscript, &
         type_letter2, &
         this%t141dir%sequence_num2
   end subroutine read_entries

   subroutine read_ascii_pd(this)
      class(t_141) :: this
      integer(int32)                 :: record_start_index
      integer(int32)                 :: num_records
      integer(int32)                 :: type_id
      character(len=4000)            :: ascii_pd_buffer
      record_start_index = (this%t141sharedata%P_section_start_index + &
                            this%t141dir%parameter_data - 1)
      call read_pd(this%t141sharedata%fileunit, &
                   record_start_index, &
                   this%t141dir%param_line_count, &
                   type_id, &
                   ascii_pd_buffer)
      this%t141metadata%num_pd_entries = count_pd_entries(ascii_pd_buffer)
      this%t141metadata%ascii_t141_pd_data = trim(ascii_pd_buffer)
   end subroutine read_ascii_pd

   subroutine read_pd_ints_initial(this)
      class(t_141)         :: this
      this%t141metadata%ascii_t141_pd_data = &
         trim(this%t141metadata%ascii_t141_pd_data)
      read (this%t141metadata%ascii_t141_pd_data, fmt=*) &
         this%t141metadata%Type_ID, &
         this%t141param%Type_BS, &
         this%t141param%PREF, &
         this%t141param%SPTR, &
         this%t141param%N
   end subroutine read_pd_ints_initial

   subroutine allocate_pd_vectors(this)
      class(t_141), intent(inout) :: this
      allocate (this%t141param%CRVPT(this%t141param%N))
      allocate (this%t141param%SENSE(this%t141param%N))
      allocate (this%t141param%K(this%t141param%N))
      allocate (this%t141param%PSCPT(this%t141param%N, size(this%t141param%K)))
      this%t141param%CRVPT = 0
      this%t141param%SENSE = 0
      this%t141param%K = 0
      this%t141param%PSCPT = 0
   end subroutine allocate_pd_vectors

   subroutine index_calcs(this)
      class(t_141) :: this
      this%t141metadata%M = 12 + 3*(this%t141param%N - 1) + sum(this%t141param%K)
   end subroutine index_calcs

   subroutine read_pd_vectors(this)
      class(t_141)   :: this
      integer(int32) :: num(5)
      integer(int32) :: i
      do i = 1, (this%t141param%N)
         if (i .gt. 1) then
            print *, 'T141: N > 1 not yet implemented...'
            EXIT
         end if
         read (this%t141metadata%ascii_t141_pd_data, fmt=*) &
            num, &
            this%t141param%CRVPT(i), &
            this%t141param%SENSE(i), &
            this%t141param%K(i)
      end do
   end subroutine read_pd_vectors

   subroutine print_dir_data(this)
      class(t_141) :: this
      print *
      print *, 'T141 Directory Data'
      print *, this%t141metadata%ascii_t141_dir_data(1:80)
      print *, this%t141metadata%ascii_t141_dir_data(81:160)
      !print *, this%t141dir
   end subroutine print_dir_data

   subroutine print_pd_data(this)
      class(t_141) :: this
      print *
      print *, 'T141 Parameter Data'
      print *, trim(this%t141metadata%ascii_t141_pd_data)
      print *, 'Type_BS :', this%t141param%Type_BS
      print *, 'PREF    :', this%t141param%PREF
      print *, 'SPTR    :', this%t141param%SPTR
      print *, 'N       :', this%t141param%N
      print *, 'CRVPT   :', this%t141param%CRVPT
      print *, 'SENSE   :', this%t141param%SENSE
      print *, 'K       :', this%t141param%K
      print *, 'PSCPT   :', this%t141param%PSCPT
   end subroutine print_pd_data

   subroutine read_t128(this)
      class(t_141), intent(inout) :: this
      call this%dep_t128%exchange_sharedata_t128(this%t141sharedata%fileunit, &
                                                 this%t141sharedata%D_section_start_index, &
                                                 this%t141sharedata%P_section_start_index)
      call this%dep_t128%read_t128_dir_entries(this%t141param%SPTR)
      call this%dep_t128%read_ascii_t128_pd
      call this%dep_t128%allocate_t128_pd_initial
      call this%dep_t128%read_t128pd_integers
      call this%dep_t128%t128_index_calculations
      call this%dep_t128%allocate_t128_pd_vectors
      call this%dep_t128%initialize_t128_pd_vectors
      call this%dep_t128%read_in_t128_pd_data
      call this%dep_t128%print_t128_dir_data
      call this%dep_t128%print_t128_pd_data
   end subroutine read_t128

   subroutine read_t126(this)
      class(t_141), intent(inout) :: this
      integer(int32) :: i
      allocate (this%dep_t126(this%t141param%N))
      do i = 1, this%t141param%N
         call this%dep_t126(i)%exchange_sharedata_t126(this%t141sharedata%fileunit, &
                                                       this%t141sharedata%D_section_start_index, &
                                                       this%t141sharedata%P_section_start_index)
         call this%dep_t126(i)%read_t126_dir_entries(this%t141param%CRVPT(i))
         call this%dep_t126(i)%read_ascii_t126_pd
         call this%dep_t126(i)%allocate_t126_pd_initial
         call this%dep_t126(i)%read_t126pd_integers
         call this%dep_t126(i)%t126_index_calculations
         call this%dep_t126(i)%allocate_t126_pd_vectors
         call this%dep_t126(i)%initialize_t126_pd_vectors
         call this%dep_t126(i)%read_in_t126_pd_data
         call this%dep_t126(i)%print_t126_dir_data
         call this%dep_t126(i)%print_t126_pd_data
      end do
   end subroutine read_t126

   ! subroutine test_type_T141(this)
   !    class(T141) :: this
   !    if (this%Type_ID == 141) then
   !       write (*, *) 'Parameter type to read in is 141, continuing...'
   !    else
   !       STOP 'ERROR: Parameter Type is not 141, see mod_T141.f90...'
   !    end if
   ! end subroutine test_type_T141

end module type_141
