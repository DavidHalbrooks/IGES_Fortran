module PD_import

   use read_raw_PD
   use Type126
   use Type128
   use Type141
   use Type143
   use Type314
   use obj_T143

   implicit none

   private
   public :: Parametric, &
             read_raw, &
             import_T126_data, import_T128_data, &
             import_T141_data, import_T143_data, &
             import_T314_data, &
             import_parametric_data !import_Parametric_Data

   type Parametric
      integer :: Type_ID = 0
      integer :: num_PD_entries = 0
      character(len=4000) :: Raw_Parameter_Data = ''
      type(T126)             :: T126_Data
      type(T128)             :: T128_Data
      type(T141)             :: T141_Data
      type(T143)             :: T143_Data
      type(T314)             :: T314_Data
      type(T143_object)      :: T143_object
      !type(T141_object)      :: T141_object
   contains
      procedure :: read => read_raw
      procedure :: import_T143 => import_T143_data
      procedure :: import_T128 => import_T128_data
      procedure :: import_T126 => import_T126_data
      procedure :: import_T141 => import_T141_data
      procedure :: import_T314 => import_T314_data
   end type Parametric

contains

   subroutine read_raw(this, fileunit, n_PD_records, num_record_start)
      class(Parametric) :: this
      integer, intent(inout) :: fileunit
      integer, intent(inout) :: n_PD_records
      integer, intent(inout) :: num_record_start
      call read_Raw_Parametric_Data(fileunit, n_PD_records, &
                                    num_record_start, &
                                    this%num_PD_entries, &
                                    this%Type_ID, &
                                    this%Raw_Parameter_Data)
      !call import_T143_data(this)
      !call import_Parametric_data(this)
   end subroutine read_raw

   subroutine import_T143_data(this, fileunit, n_PD_records, num_record_start)
      class(Parametric) :: this
      integer, intent(inout) :: fileunit
      integer, intent(inout) :: n_PD_records
      integer, intent(inout) :: num_record_start
      call read_raw(this, fileunit, n_PD_records, num_record_start)
      call all_subs_T143(this%T143_Data, this%Type_ID, this%num_PD_entries, this%Raw_Parameter_Data)
   end subroutine import_T143_data

   subroutine import_T126_data(this)
      class(Parametric) :: this
      call all_subs_T126(this%T126_Data, &
                         this%Type_ID, &
                         this%num_PD_entries, &
                         this%Raw_Parameter_Data)
      write (*, *) this%T126_Data%T
      write (*, *) this%T126_Data%W
      write (*, *) this%T126_Data%P
      write (*, *) this%T126_Data%V
   end subroutine import_T126_data

   subroutine import_T128_data(this, fileunit, n_PD_records, num_record_start)
      class(Parametric) :: this
      integer, intent(inout) :: fileunit
      integer, intent(inout) :: n_PD_records
      integer, intent(inout) :: num_record_start
      call read_raw(this, fileunit, n_PD_records, num_record_start)
      call all_subs_T128(this%T128_Data, this%Type_ID, this%num_PD_entries, this%Raw_Parameter_Data)
      write (*, *) this%T128_Data%S
      write (*, *) this%T128_Data%T
      write (*, *) this%T128_Data%W
      write (*, *) this%T128_Data%P
      write (*, *) this%T128_Data%U
      write (*, *) this%T128_Data%V
   end subroutine import_T128_data

   subroutine import_T141_data(this)
      class(Parametric) :: this
      call all_subs_T141(this%T141_Data, this%Type_ID, this%num_PD_entries, this%Raw_Parameter_Data)
      write (*, *) this%T141_Data%Type_BS, this%T141_data%PREF, &
         this%T141_Data%SPTR, this%T141_Data%N, &
         this%T141_Data%CRVPT_1, this%T141_Data%SENSE_1, &
         this%T141_Data%K1
   end subroutine import_T141_data

   subroutine import_T314_data(this)
      class(Parametric) :: this
      call all_subs_T314(this%T314_Data, this%Type_ID, this%num_PD_entries, this%Raw_Parameter_Data)
      write (*, *) this%T314_Data%CC1, this%T314_Data%CC2, this%T314_Data%CC3, this%T314_Data%CNAME
   end subroutine import_T314_data

   subroutine import_Parametric_Data(this)
      class(Parametric) :: this
      if (this%Type_ID == 126) then
         write (*, *) 'Type 126 Identified'
         call all_subs_T126(this%T126_Data, &
                            this%Type_ID, &
                            this%num_PD_entries, &
                            this%Raw_Parameter_Data)
         write (*, *) this%T126_Data%T
         write (*, *) this%T126_Data%W
         write (*, *) this%T126_Data%P
         write (*, *) this%T126_Data%V
      else if (this%Type_ID == 128) then
         write (*, *) 'Type 128 Identified'
         call all_subs_T128(this%T128_Data, this%Type_ID, this%num_PD_entries, this%Raw_Parameter_Data)
         write (*, *) this%T128_Data%S
         write (*, *) this%T128_Data%T
         write (*, *) this%T128_Data%W
         write (*, *) this%T128_Data%P
         write (*, *) this%T128_Data%U
         write (*, *) this%T128_Data%V
      else if (this%Type_ID == 141) then
         write (*, *) 'Type 141 Identified'
         call all_subs_T141(this%T141_Data, this%Type_ID, this%num_PD_entries, this%Raw_Parameter_Data)
         write (*, *) this%T141_Data%Type_BS, this%T141_data%PREF, &
            this%T141_Data%SPTR, this%T141_Data%N, &
            this%T141_Data%CRVPT_1, this%T141_Data%SENSE_1, &
            this%T141_Data%K1
      else if (this%Type_ID == 143) then
         write (*, *) 'Type 143 Identified'
         call all_subs_T143(this%T143_Data, this%Type_ID, this%num_PD_entries, this%Raw_Parameter_Data)
         write (*, *) this%T143_Data%Type_BS, this%T143_data%SPTR, this%T143_Data%N, this%T143_Data%iBDPT_1, &
            this%T143_Data%iBDPT_N !this%T143_Data%BDPT
      else if (this%Type_ID == 314) then
         write (*, *) 'Type 314 Identified'
         call all_subs_T314(this%T314_Data, this%Type_ID, this%num_PD_entries, this%Raw_Parameter_Data)
         write (*, *) this%T314_Data%CC1, this%T314_Data%CC2, this%T314_Data%CC3, this%T314_Data%CNAME
      else
         print *
         write (*, *) 'Type was not identified...'
      end if
   end subroutine import_Parametric_Data

end module PD_import
