program test_move_alloc
    integer, allocatable :: a(:), b(:)

    allocate(a(3))
    a = [ 1, 2, 3 ]
    write(*,*) 'size a:', size(a)
    print *, a

    allocate(b(size(a)+1))
    b = 0
    write(*,*) 'size b:', size(b)

    b(1:size(a)) = a
    !call move_alloc(a, b)
    print *, allocated(a), allocated(b)
    print *, b

    write(*,*) 'size b:', size(b)
    call move_alloc(b, a)

    !allocate(a(size(b)))
    !write(*,*) 'size a:', size(a)
    !a = 0

    write(*,*) a
    write(*,*) 'size a:', size(a)

end program test_move_alloc
