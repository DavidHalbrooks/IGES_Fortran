module Type_Metadata2
   use iso_fortran_env, only: int32, int64, real32, real64
   implicit none

   private
   public :: Metadata2

   type Metadata2
      integer(int32)     :: num_records = 0
      integer(int32)     :: num_S_records = 0
      integer(int32)     :: num_G_records = 0
      integer(int32)     :: num_D_records = 0
      integer(int32)     :: num_P_records = 0
      integer(int32)     :: num_T_records = 0
      integer(int32)     :: G_record_start = 0
      integer(int32)     :: D_record_start = 0
      integer(int32)     :: P_record_start = 0
      integer(int32)     :: T_record_start = 0
      integer(int32)     :: num_T126_Entities = 0
      integer(int32)     :: num_T128_Entities = 0
      integer(int32)     :: num_T141_Entities = 0
      integer(int32)     :: num_T314_Entities = 0
      integer(int32)     :: num_unidentified_Entities = 0
   contains
      !procedure :: calc_record_start_pos => calc
   end type Metadata2

contains

end module Type_Metadata2
