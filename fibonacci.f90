program fibonacci

    implicit none
    integer(8) :: n,s,c1,c2,c3,c4,r,av,bv,cv
    character(len = *), parameter :: fnum = "(a, '  ', i18)", ftime = "(f13.9, ' seconds')"

    print '("Type in the number")'

    read *, n

    call system_clock(count=c1)

    av = a(n)

    call system_clock(count=c2)

    bv = b(n)

    call system_clock(count=c3)

    cv = c(n)

    call system_clock(count=c4,count_rate=r)

    print fnum, "a", av
    print ftime, real(c2-c1,8)/real(r,8)
    print fnum, "b", bv
    print ftime, real(c3-c2,8)/real(r,8)
    print fnum, "c", cv
    print ftime, real(c4-c3,8)/real(r,8)

    contains

    integer(8) recursive function a(n) result (r)
        integer(8) :: n
        intent(in) :: n

        if (n > 2) then
            r = a(n-1) + a(n-2)
        else
            r = 1
        end if
        
        return
    end function a

    integer(8) recursive function b(n) result (r)
        integer(8) :: n
        intent(in) :: n

        if (n > 2) then
            r = b(n-2)*2 + b(n-3)
        else if (n > 0) then
            r = 1
        else
            r = 0
        end if

        return
    end function b

    integer(8) function c(n)
        integer(8) :: n,i,x=0,y=1,z=1
        intent(in) :: n

        do, i=1, n-1
            z = x + y
            x = y
            y = z
        end do

        c = y

        return
    end function c

end program fibonacci

