project('Iges_Fortran', 'fortran', meson_version: '>=0.49')
src = files([
  'src/main.f90', 
  'src/type_defs/iges_master.f90',
  'src/type_defs/type_dir_meta.f90',
  'src/type_defs/type_directory.f90',
  'src/type_defs/type_global.f90',
  'src/type_defs/type_metadata2.f90',
  'src/type_defs/type_start.f90',
  'src/type_defs/type_tail.f90',
  'src/utils/open_close.f90',
  'src/utils/filename.f90',
  'src/utils/num_records.f90',
  'src/utils/record_pos_calcs.f90',
  'src/utils/read_global_ascii.f90'
])
executable('import', src)
