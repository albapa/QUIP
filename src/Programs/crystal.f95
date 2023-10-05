program md

  use libatoms_module

  implicit none
  type(Dictionary)      :: params

  type(Atoms)::at, at2
  real(dp)::energy, a0, c0
  integer :: Z, Z2, n1, n2, n3

  logical :: help, has_Z2
  character(len=STRING_LENGTH) :: lattice_type

  call system_initialise(verbosity=PRINT_NORMAL)

  call initialise(params)
  call param_register(params, 'lattice_type', 'diamond8', lattice_type,"")
  call param_register(params, 'a0', '5.431', a0,"")
  call param_register(params, 'c0', '1.000', c0,"")
  call param_register(params, 'Z', '14', Z,"")
  call param_register(params, 'Z2', '14', Z2,"", has_value_target=has_Z2)
  call param_register(params, 'n1', '1', n1,"")
  call param_register(params, 'n2', '1', n2,"")
  call param_register(params, 'n3', '1', n3,"")
  call param_register(params, 'help', 'F', help,"")

  if (.not. param_read_args(params, check_mandatory = .true.)) &
  & call system_abort('Exit: problem when parsing command-line arguments')
  call finalise(params)


  if( help ) then
     call print("Usage: crystal [lattice_type] [a0=5.431] [c0=1.000] [Z=14] &
     & [Z2=14] [n1=1] [n2=1] [n3=1]")
     call print("Available lattice types: diamond2, diamond8, graphite, &
     & graphite_rhombohedral, fcc, beta_tin, beta_tin4, bcc, bcc1, hcp, wurtzite" )
     call system_abort('Exiting.')
  endif
  if( .not. has_Z2 ) Z2 = Z

  select case(lower_case(trim(lattice_type)))
    case('diamond2')
        call diamond2(at2, a0, Z, Z2)
    case('diamond8')
       call diamond(at2, a0, (/Z, Z2/) )
    case('graphite')
       call graphite(at2,a0,c0, Z)
    case('graphite_rhombohedral')
       call graphite_rhombohedral(at2, a0,c0, Z)
    case('fcc')
       call fcc(at2,a0,Z)
    case('beta_tin')
       call beta_tin(at2,a0,c0,Z)
    case('beta_tin4')
       call beta_tin4(at2,a0,c0,Z)
    case('bcc')
       call bcc(at2,a0,Z)
    case('bcc1')
       call bcc1(at2,a0,Z)
    case('hcp')
       call hcp(at2,a0,Z)
    case('wurtzite')
       call wurtzite(at2,a0,Z1=Z,Z2=Z2)
    case('alpha_quartz')
       !call alpha_quartz(at2,a0,c0,0.46970_dp, 0.41350_dp, 0.26690_dp, 0.11910_dp)
       call alpha_quartz(at2,a0,c0,0.474452_dp, 0.414382_dp, 0.259186_dp, 0.125512_dp)
    case('sh')
       call sh(at2,a0, c0, Z)
    case('sh2')
       call sh2(at2,a0, c0, Z)
    case('imma')
       call imma(at2, 4.7459_dp, 4.5086_dp, 2.5485_dp, 0.185_dp, Z)
    case default
       call system_abort('Lattice type '//trim(lattice_type)// ' unknown')
  endselect
       
  call supercell(at,at2,n1,n2,n3)
  call write(at,'crystal.xyz')

  call finalise(at,at2)

  call system_finalise()

end program md
