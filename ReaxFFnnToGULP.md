# Implement the ReaxFF-nn machine learning potenitial to the General Utility Lattice Program

## Step 1

Insert the following code in module.f90 between line 3386-3387:

```fortran
    !!!! The flowing lines are used by Neural network bond-correction
    logical,                                       save :: lreaxFFNNboCorrect = .false. ! used by gfeng
    integer(i4),                                   save :: mfn = 12  
    integer(i4),                                   save :: mfh = 2  
    real(dp),         dimension(:,:,:),   pointer, save :: reaxFFfwi => null() 
    real(dp),         dimension(:,:),     pointer, save :: reaxFFfbi => null()   
    real(dp),         dimension(:,:,:,:), pointer, save :: reaxFFfwh => null()  
    real(dp),         dimension(:,:,:),   pointer, save :: reaxFFfbh => null()  
    real(dp),         dimension(:,:,:),     pointer, save :: reaxFFfwo => null()  
    real(dp),         dimension(:,:),     pointer, save :: reaxFFfbo => null() 
    logical,                                       save :: lreaxFFNNEbond = .false. ! used by gfeng
    integer(i4),                                   save :: ben = 12  
    integer(i4),                                   save :: beh = 1  
    real(dp),         dimension(:,:,:),   pointer, save :: reaxFFewi => null() 
    real(dp),         dimension(:,:),     pointer, save :: reaxFFebi => null()   
    real(dp),         dimension(:,:,:,:), pointer, save :: reaxFFewh => null()  
    real(dp),         dimension(:,:,:),   pointer, save :: reaxFFebh => null()  
    real(dp),         dimension(:,:),     pointer, save :: reaxFFewo => null()  
    real(dp),         dimension(:,:),     pointer, save :: reaxFFebo => null() 
    logical,                                       save :: lreaxFFNNVdw = .false.   ! used by vdw taper function
    integer(i4),                                   save :: vwn = 9  
    integer(i4),                                   save :: vwh = 0  
    real(dp),         dimension(:,:,:),   pointer, save :: reaxFFvwi => null()      ! vdw taper function
    real(dp),         dimension(:,:),     pointer, save :: reaxFFvbi => null()   
    real(dp),         dimension(:,:),     pointer, save :: reaxFFvwo => null()  
    real(dp),         dimension(:,:),     pointer, save :: reaxFFvbo => null() 
    !!!! The above lines are used by Neural network bond-correction
```

## Step 2 
Insert the following code in boword.F90 after line 7000:

```fortran
!
!*************************************************
!  Parameters for the bond-order neural networks *
!*************************************************

! Feng G.
700 continue
!
!  Neural network layers for Bond-Order corrections
!
  lreaxFFNNboCorrect = .true.
  if (nfloat.ge.2) then
    mfn = nint(floats(1))
    mfh = nint(floats(2))
  else
    call outerror('Incorrect mflayer input',iline)
    call stopnow('boword')
  endif
  lwordok = .true.
  return

710 continue
  lbocorr1 = .false.
  lbocorr2 = .false.
  if (nword.gt.1) then
    i = 2
    lwi = .false.
    lbi = .false.
    lwo = .false.
    lbo = .false.
    lwh = .false.
    lbh = .false.
    do while (i.le.nword)
      if (index(words(i),'wi').eq.1) then
        lwi = .true.
      elseif (index(words(i),'bi').eq.1) then
        lbi = .true.
      elseif (index(words(i),'wo').eq.1) then
        lwo = .true.
      elseif (index(words(i),'bo').eq.1) then
        lbo = .true.
      elseif (index(words(i),'wh').eq.1) then
        lwh = .true.
      elseif (index(words(i),'bh').eq.1) then
        lbh = .true.
      endif
      i = i + 1
    enddo
  endif

715 line = '  '
  read(iin,'(a)',end=718) line
  iline = iline + 1
  call linepro(iin,line,iline)
!
!  Check whether input is symbol or option
!
  if ((nword+nfloat).eq.0) goto 715   

  if (nword.gt.0) then
    word = words(1)
    call worsy(word,lsymbol,.true.)
    if (.not.lsymbol) then
      l55 = .true.
      goto 718
    endif
  elseif (nfloat.gt.0) then
!
!  If this is a coefficient line then skip
!
    goto 715
  endif
!
!  Process symbol input
!
  call getpotsymbol1(iline,llibrary,nvar1,itype1,sym1,0_i4,ind,lvalidpot)
  if (.not.lvalidpot) goto 715
!
!  Find matches for species 1 within list if they exists
!
  nmatch1 = 0
  do nrs = 1,nreaxFFspec
    if (lmatch(natreaxFFspec(nrs),ntypreaxFFspec(nrs),nvar1,itype1,.false.)) then
      nmatch1 = nmatch1 + 1
      if (nmatch1.gt.maxmatch) then
        call outerror('maxmatch parameter exceeded - increase and recompile',iline)
        call stopnow('boword')
      endif
      nsmatch1(nmatch1) = nrs
    endif
  enddo
!
!  If species haven't been found add to the list
!
  if (nmatch1.eq.0) then
    nreaxFFspec = nreaxFFspec + 1
    if (nreaxFFspec.gt.maxreaxFFspec) then
      maxreaxFFspec = nreaxFFspec
      call changemaxreaxFFspec
    endif
    natreaxFFspec(nreaxFFspec) = nvar1
    ntypreaxFFspec(nreaxFFspec) = itype1
    if (nvar1.le.10) lreaxFFunder(nreaxFFspec) = .true.
    symbolreaxFFspec(nreaxFFspec) = sym1
    nmatch1 = 1
    nsmatch1(1) = nreaxFFspec
  endif
!
! fitting this potential is not supported
!
!  Loop over pairs of matches
!
  do nm1 = 1,nmatch1
    ns1 = nsmatch1(nm1)
!
!  Assign coefficients and cutoffs
!
    if (lwi) then                           ! read input layer parameters
        do i = 1,mfn
          reaxFFfwi(i,1,ns1) = floats(i)
        enddo
        do ii = 2,3
          line = ' '
          read(iin,'(a)',end=718) line      ! read the parameter line
          iline = iline + 1
          call linepro(iin,line,iline)
          do i = 1,mfn
              reaxFFfwi(i,ii,ns1) = floats(i)
          enddo
        enddo
    elseif (lbi) then
        do i = 1,mfn
          reaxFFfbi(i,ns1) = floats(i)
        enddo
    elseif (lwo) then                       ! read the output layer parameters
        do i = 1,mfn
          reaxFFfwo(i,1,ns1) = floats(i)
        enddo
        do ii = 2,mfn
        line = ' '
        read(iin,'(a)',end=718) line      ! read the parameter line
        iline = iline + 1
        call linepro(iin,line,iline)
        do i = 1,3
            reaxFFfwo(i,ii,ns1) = floats(i)
        enddo
      enddo
    elseif (lbo) then
        do i =1,3
          reaxFFfbo(i,ns1) = floats(i)
        enddo
    elseif (lwh) then                      ! read the hiden layer weight
        do i = 1,mfn
          reaxFFfwh(i,1,1,ns1) = floats(i)
        enddo
        do ii = 2,mfn
          line = ' '
          read(iin,'(a)',end=718) line     
          iline = iline + 1
          call linepro(iin,line,iline)
          do i = 1,mfn
              reaxFFfwh(i,ii,1,ns1) = floats(i)
          enddo
        enddo

        if (mfh>1) then
          do ifl = 2,mfh
              do ii = 1,mfn
                line = ' '
                read(iin,'(a)',end=718) line     
                iline = iline + 1
                call linepro(iin,line,iline)
                do i = 1,mfn
                    reaxFFfwh(i,ii,ifl,ns1) = floats(i)
                enddo
              enddo
          enddo
        endif
    elseif (lbh) then                     ! read the hiden layer bias
        do i = 1,mfn
          reaxFFfbh(i,1,ns1) = floats(i)
        enddo
        if (mfh>1) then
          do ii = 2,mfh
              read(iin,'(a)',end=718) line     
              iline = iline + 1
              call linepro(iin,line,iline)
              do i = 1,mfn
                reaxFFfbh(i,ii,ns1) = floats(i)
            enddo
          enddo
        endif
    else
      call outerror('Incorrect reaxFF neural network corrections input',iline)
      call stopnow('boword')
    endif
  enddo
!   do i=1,mfn
!      write(*,*)(reaxFFfwh(ii,i,1,1),ii=1,6 )
!   enddo
  goto 715
718 if (.not.l55) l1000 = .true.
  lwordok = .true.
  return
!
!*************************************************
! Parameters for the bond energy neural networks *
!*************************************************
! Feng G.
800 continue
!
!  Neural network layers for Bond-Energy calculations
!
  lreaxFFNNEbond = .true.
  if (nfloat.ge.2) then
    ben = nint(floats(1))
    beh = nint(floats(2))
  else
    call outerror('Incorrect belayer input',iline)
    call stopnow('boword')
  endif
  lwordok = .true.
  return

810 continue
  lbocorr1 = .false.
  lbocorr2 = .false.
  if (nword.gt.1) then
    i = 2
    lwi = .false.
    lbi = .false.
    lwo = .false.
    lbo = .false.
    lwh = .false.
    lbh = .false.
    do while (i.le.nword)
      if (index(words(i),'wi').eq.1) then
        lwi = .true.
      elseif (index(words(i),'bi').eq.1) then
        lbi = .true.
      elseif (index(words(i),'wo').eq.1) then
        lwo = .true.
      elseif (index(words(i),'bo').eq.1) then
        lbo = .true.
      elseif (index(words(i),'wh').eq.1) then
        lwh = .true.
      elseif (index(words(i),'bh').eq.1) then
        lbh = .true.
      endif
      i = i + 1
    enddo
  endif

815 line = '  '
  read(iin,'(a)',end=818) line
  iline = iline + 1
  call linepro(iin,line,iline)
!
!  Check whether input is symbol or option
!
  if ((nword+nfloat).eq.0) goto 815   

  if (nword.gt.0) then
    word = words(1)
    call worsy(word,lsymbol,.true.)
    if (.not.lsymbol) then
      l55 = .true.
      goto 818
    endif
  elseif (nfloat.gt.0) then
!
!  If this is a coefficient line then skip
!
    goto 815
  endif
!
!  Process symbol input
!
  call getpotsymbol2(iline,llibrary,nvar1,itype1,sym1,nvar2,itype2,sym2,nbeg,lvalidpot)
  if (.not.lvalidpot) goto 815
!
!  Find matches for species 1 within list if they exists
!
  nmatch1 = 0
  do nrs = 1,nreaxFFspec
    if (lmatch(natreaxFFspec(nrs),ntypreaxFFspec(nrs),nvar1,itype1,.false.)) then
      nmatch1 = nmatch1 + 1
      if (nmatch1.gt.maxmatch) then
        call outerror('maxmatch parameter exceeded - increase and recompile',iline)
        call stopnow('boword')
      endif
      nsmatch1(nmatch1) = nrs
    endif
  enddo
!
!  If species haven't been found add to the list
!
  if (nmatch1.eq.0) then
    nreaxFFspec = nreaxFFspec + 1
    if (nreaxFFspec.gt.maxreaxFFspec) then
      maxreaxFFspec = nreaxFFspec
      call changemaxreaxFFspec
    endif
    natreaxFFspec(nreaxFFspec) = nvar1
    ntypreaxFFspec(nreaxFFspec) = itype1
    if (nvar1.le.10) lreaxFFunder(nreaxFFspec) = .true.
    symbolreaxFFspec(nreaxFFspec) = sym1
    nmatch1 = 1
    nsmatch1(1) = nreaxFFspec
  endif
!
!  Find matches for species 2 within list if they exists
!
  nmatch2 = 0
  do nrs = 1,nreaxFFspec
    if (lmatch(natreaxFFspec(nrs),ntypreaxFFspec(nrs),nvar2,itype2,.false.)) then
      nmatch2 = nmatch2 + 1
      if (nmatch2.gt.maxmatch) then
        call outerror('maxmatch parameter exceeded - increase and recompile',iline)
        call stopnow('boword')
      endif
      nsmatch2(nmatch2) = nrs
    endif
  enddo
  if (nmatch2.eq.0) then
    nreaxFFspec = nreaxFFspec + 1
    if (nreaxFFspec.gt.maxreaxFFspec) then
      maxreaxFFspec = nreaxFFspec
      call changemaxreaxFFspec
    endif
    natreaxFFspec(nreaxFFspec) = nvar2
    ntypreaxFFspec(nreaxFFspec) = itype2
    if (nvar2.le.10) lreaxFFunder(nreaxFFspec) = .true.
    symbolreaxFFspec(nreaxFFspec) = sym2
    nmatch2 = 1
    nsmatch2(1) = nreaxFFspec
  endif
!
! fitting this potential is not supported
!
!  Loop over pairs of matches
!
  do nm1 = 1,nmatch1
    ns1 = nsmatch1(nm1)
    do nm2 = 1,nmatch2
      ns2 = nsmatch2(nm2)
!
!  Compute index of place to store pairwise reaxFF data
!
      if (ns1.ge.ns2) then
        ind = ns1*(ns1-1)/2 + ns2
      else
        ind = ns2*(ns2-1)/2 + ns1
      endif
!
!  Assign coefficients and cutoffs
!
      if (lwi) then                           ! read input layer parameters
         do i = 1,ben
            reaxFFewi(i,1,ind) = floats(i)
         enddo
         do ii = 2,3
            line = ' '
            read(iin,'(a)',end=818) line      ! read the parameter line
            iline = iline + 1
            call linepro(iin,line,iline)
            do i = 1,ben
               reaxFFewi(i,ii,ind) = floats(i)
            enddo
         enddo
      elseif (lbi) then
         do i = 1,ben
            reaxFFebi(i,ind) = floats(i)
         enddo
      elseif (lwo) then                       ! read the output layer parameters
         do i = 1,ben
            reaxFFewo(i,ind) = floats(i)
         enddo
      elseif (lbo) then
         reaxFFebo(1,ind) = floats(1)
      elseif (lwh) then                      ! read the hiden layer weight
         do i = 1,ben
            reaxFFewh(i,1,1,ind) = floats(i)
         enddo
         do ii = 2,ben
            line = ' '
            read(iin,'(a)',end=818) line     
            iline = iline + 1
            call linepro(iin,line,iline)
            do i = 1,ben
               reaxFFewh(i,ii,1,ind) = floats(i)
            enddo
         enddo

         if (beh>1) then
            do ifl = 2,beh
               do ii = 1,ben
                  line = ' '
                  read(iin,'(a)',end=818) line     
                  iline = iline + 1
                  call linepro(iin,line,iline)
                  do i = 1,ben
                     reaxFFewh(i,ii,ifl,ind) = floats(i)
                  enddo
               enddo
            enddo
         endif
      elseif (lbh) then                     ! read the hiden layer bias
         do i = 1,ben
            reaxFFebh(i,1,ind) = floats(i)
         enddo
         if (beh>1) then
            do ii = 2,beh
               read(iin,'(a)',end=818) line     
               iline = iline + 1
               call linepro(iin,line,iline)
               do i = 1,ben
                  reaxFFebh(i,ii,ind) = floats(i)
              enddo
            enddo
         endif
      else
        call outerror('Incorrect reaxFF neural network bond energy input',iline)
        call stopnow('boword')
      endif
!
    enddo
  enddo

!   do i=1,mfn
!      write(*,*)(reaxFFewh(ii,i,1,1),ii=1,6 )
!   enddo
!
  goto 815
818 if (.not.l55) l1000 = .true.
  lwordok = .true.
  return


910 continue
  lbocorr1 = .false.
  lbocorr2 = .false.
  if (nword.gt.1) then
    i = 2
    lwi = .false.
    lbi = .false.
    lwo = .false.
    lbo = .false.
    do while (i.le.nword)
      if (index(words(i),'wi').eq.1) then
        lwi = .true.
      elseif (index(words(i),'bi').eq.1) then
        lbi = .true.
      elseif (index(words(i),'wo').eq.1) then
        lwo = .true.
      elseif (index(words(i),'bo').eq.1) then
        lbo = .true.
      endif
      i = i + 1
    enddo
  endif

915 line = '  '
  read(iin,'(a)',end=918) line
  iline = iline + 1
  call linepro(iin,line,iline)
!
!  Check whether input is symbol or option
!
  if ((nword+nfloat).eq.0) goto 915   

  if (nword.gt.0) then
    word = words(1)
    call worsy(word,lsymbol,.true.)
    if (.not.lsymbol) then
      l55 = .true.
      goto 918
    endif
  elseif (nfloat.gt.0) then
!
!  If this is a coefficient line then skip
!
    goto 915
  endif
!
!  Process symbol input
!
  call getpotsymbol2(iline,llibrary,nvar1,itype1,sym1,nvar2,itype2,sym2,nbeg,lvalidpot)
  if (.not.lvalidpot) goto 915
!
!  Find matches for species 1 within list if they exists
!
  nmatch1 = 0
  do nrs = 1,nreaxFFspec
    if (lmatch(natreaxFFspec(nrs),ntypreaxFFspec(nrs),nvar1,itype1,.false.)) then
      nmatch1 = nmatch1 + 1
      if (nmatch1.gt.maxmatch) then
        call outerror('maxmatch parameter exceeded - increase and recompile',iline)
        call stopnow('boword')
      endif
      nsmatch1(nmatch1) = nrs
    endif
  enddo
!
!  If species haven't been found add to the list
!
  if (nmatch1.eq.0) then
    nreaxFFspec = nreaxFFspec + 1
    if (nreaxFFspec.gt.maxreaxFFspec) then
      maxreaxFFspec = nreaxFFspec
      call changemaxreaxFFspec
    endif
    natreaxFFspec(nreaxFFspec) = nvar1
    ntypreaxFFspec(nreaxFFspec) = itype1
    if (nvar1.le.10) lreaxFFunder(nreaxFFspec) = .true.
    symbolreaxFFspec(nreaxFFspec) = sym1
    nmatch1 = 1
    nsmatch1(1) = nreaxFFspec
  endif
!
!  Find matches for species 2 within list if they exists
!
  nmatch2 = 0
  do nrs = 1,nreaxFFspec
    if (lmatch(natreaxFFspec(nrs),ntypreaxFFspec(nrs),nvar2,itype2,.false.)) then
      nmatch2 = nmatch2 + 1
      if (nmatch2.gt.maxmatch) then
        call outerror('maxmatch parameter exceeded - increase and recompile',iline)
        call stopnow('boword')
      endif
      nsmatch2(nmatch2) = nrs
    endif
  enddo
  if (nmatch2.eq.0) then
    nreaxFFspec = nreaxFFspec + 1
    if (nreaxFFspec.gt.maxreaxFFspec) then
      maxreaxFFspec = nreaxFFspec
      call changemaxreaxFFspec
    endif
    natreaxFFspec(nreaxFFspec) = nvar2
    ntypreaxFFspec(nreaxFFspec) = itype2
    if (nvar2.le.10) lreaxFFunder(nreaxFFspec) = .true.
    symbolreaxFFspec(nreaxFFspec) = sym2
    nmatch2 = 1
    nsmatch2(1) = nreaxFFspec
  endif
!
! fitting this potential is not supported
!
!  Loop over pairs of matches
!
  do nm1 = 1,nmatch1
    ns1 = nsmatch1(nm1)
    do nm2 = 1,nmatch2
      ns2 = nsmatch2(nm2)
!
!  Compute index of place to store pairwise reaxFF data
!
      if (ns1.ge.ns2) then
        ind = ns1*(ns1-1)/2 + ns2
      else
        ind = ns2*(ns2-1)/2 + ns1
      endif
!
!  Assign coefficients and cutoffs
!
      if (lwi) then                           ! read input layer parameters
         do i = 1,vwn
            reaxFFvwi(i,1,ind) = floats(i)
         enddo
      elseif (lbi) then
         do i = 1,vwn
            reaxFFvbi(i,ind) = floats(i)
         enddo
      elseif (lwo) then                       ! read the output layer parameters
         do i = 1,vwn
            reaxFFvwo(i,ind) = floats(i)
         enddo
      elseif (lbo) then
         reaxFFvbo(1,ind) = floats(1)
      else
        call outerror('Incorrect reaxFF neural network bond energy input',iline)
        call stopnow('boword')
      endif
!
    enddo
  enddo

  goto 915
918 if (.not.l55) l1000 = .true.
  lwordok = .true.
  return

  end
```

## step 3

Insert the following code in changemaxreaxffspec.F90 after line 122:

```fortran
  !
  ! allocate memmory used by neural network bond order corrections
  !
  call realloc(reaxFFfwi,mfn,3,maxreaxFFspec,ierror)
  if (ierror.ne.0) call outofmemory('changemaxreaxFFfwi','reaxFFfwi')
  call realloc(reaxFFfbi,mfn,maxreaxFFspec,ierror)
  if (ierror.ne.0) call outofmemory('changemaxreaxFFfbi','reaxFFfbi')
  call realloc(reaxFFfwo,3,mfn,maxreaxFFspec,ierror)
  if (ierror.ne.0) call outofmemory('changemaxreaxFFfwo','reaxFFfwo')
  call realloc(reaxFFfbo,3,maxreaxFFspec,ierror)
  if (ierror.ne.0) call outofmemory('changemaxreaxFFfbo','reaxFFfbo')
  if (mfh>0) then
     call realloc(reaxFFfwh,mfn,mfn,mfh,maxreaxFFspec,ierror)
     if (ierror.ne.0) call outofmemory('changemaxreaxFFfwh','reaxFFfwh')
     call realloc(reaxFFfbh,mfn,mfh,maxreaxFFspec,ierror)
     if (ierror.ne.0) call outofmemory('changemaxreaxFFfbh','reaxFFfbh')
  endif
  !
  ! for neural network bond energy
  !
  call realloc(reaxFFewi,ben,3,maxreaxFFspec2,ierror)
  if (ierror.ne.0) call outofmemory('changemaxreaxFFewi','reaxFFewi')
  call realloc(reaxFFebi,ben,maxreaxFFspec2,ierror)
  if (ierror.ne.0) call outofmemory('changemaxreaxFFebi','reaxFFebi')
  call realloc(reaxFFewo,ben,maxreaxFFspec2,ierror)
  if (ierror.ne.0) call outofmemory('changemaxreaxFFewo','reaxFFewo')
  call realloc(reaxFFebo,1,maxreaxFFspec2,ierror)
  if (ierror.ne.0) call outofmemory('changemaxreaxFFebo','reaxFFebo')
  if (beh>0) then
     call realloc(reaxFFewh,ben,ben,beh,maxreaxFFspec2,ierror)
     if (ierror.ne.0) call outofmemory('changemaxreaxFFewh','reaxFFewh')
     call realloc(reaxFFebh,ben,beh,maxreaxFFspec2,ierror)
     if (ierror.ne.0) call outofmemory('changemaxreaxFFebh','reaxFFebh')
  endif
  !
```

## step 4

Insert the following code in initmaxreaxffspecdefaults.F90 after line 83 and before enddo:

```fortran
!
      ! parameters used by neural networks Bond-Order correction
      !
      reaxFFfwi(1:mfn,1:3,j) = 0.0_dp
      reaxFFfbi(1:mfn,j) = 0.0_dp
      reaxFFfwo(1:3,1:mfn,j) = 0.0_dp
      reaxFFfbo(1:3,j) = 0.0_dp
      if (mfh>0) then
         reaxFFfwh(1:mfn,1:mfn,1:mfh,j) = 0.0_dp
         reaxFFfbh(1:mfn,1:mfh,j) = 0.0_dp
      endif

```

Insert the following code in initmaxreaxffspecdefaults.F90 after line 101 and before enddo:


```fortran
      ! parameter for the bond energy
      reaxFFewi(1:ben,1:3,j) = 0.0_dp
      reaxFFebi(1:ben,j) = 0.0_dp
      reaxFFewo(1:ben,j) = 0.0_dp
      reaxFFebo(1,j) = 0.0_dp
      if (beh>0) then
         reaxFFewh(1:ben,1:ben,1:beh,j) = 0.0_dp
         reaxFFebh(1:ben,1:beh,j) = 0.0_dp
      endif
```
## step 5

chang the value of the following:

```fortran
 maxlinelength = 300  ! Maximum length of a line
 maxwordlength = 120  ! Maximum length of a string
```

## step 6

add a if condition at about line 1528:

```fortran
!
!  Raise BOij to the power of Pbe,2 (in paper says Pbe,1) but I think this is a typo!
!
          if (lreaxFFNNEbond) then  !!! added for Neural Networks
            call reaxFF_enn(nboij,BOij_s,BOij_pi,BOij_pipi,eij,deijdBO_s,deijdBO_pi,deijdBO_pipi,lgrad1,scale,scale)
          else
            BOijpbe2 = BOij_s**(reaxFFpbe(2,nboij))
```

add a if condition at about line 1560:
```fortran
!
!  Derivatives of Bond Order potential energy
!
          if (lgrad1) then
            if (.not. lreaxFFNNEbond) then !!! added for Neural Networks
              deijdBO_s    = - scale*reaxFFDe(1,nboij)*expij*(1.0_dp - reaxFFpbe(1,nboij)*reaxFFpbe(2,nboij)*BOijpbe2)
              deijdBO_pi   = - scale*reaxFFDe(2,nboij) 
              deijdBO_pipi = - scale*reaxFFDe(3,nboij) 
            endif                          !!! added for Neural Networks
```

## step 7

Insert the following code in reaxffmd.F90 after line 1781:

```fortran
            if (lreaxFFNNEbond) then
              call reaxFF_enn(nboij,BOij_s,BOij_pi,BOij_pipi,eij,deijdBO_s,deijdBO_pi,deijdBO_pipi,lgrad1,0.5_dp,1.0_dp)
            else
```

Insert the following code in reaxffmd.F90 after line 1819:

```fortran
              if (.not. lreaxFFNNEbond) then
                deijdBO_s    = - reaxFFDe(1,nboij)*expij*(1.0_dp - reaxFFpbe(1,nboij)*reaxFFpbe(2,nboij)*BOijpbe2)
                deijdBO_pi   = - reaxFFDe(2,nboij) 
                deijdBO_pipi = - reaxFFDe(3,nboij)
              endif 
```
Insert the following code in reaxffmd.F90 after line 1931:
```fortran
            if (lreaxFFNNEbond) then
              call reaxFF_enn(nboij,BOij_s,BOij_pi,BOij_pipi,eij,deijdBO_s,deijdBO_pi,deijdBO_pipi,lgrad1,scale,scale)
            else
```

Insert the following code in reaxffmd.F90 after line 1971:

```fortran
              if (.not. lreaxFFNNEbond) then
                deijdBO_s    = - reaxFFDe(1,nboij)*expij*(1.0_dp - reaxFFpbe(1,nboij)*reaxFFpbe(2,nboij)*BOijpbe2)
                deijdBO_pi   = - reaxFFDe(2,nboij) 
                deijdBO_pipi = - reaxFFDe(3,nboij)
              endif 
```

## step 8

Insert the following code in reaxff_bo.F90 after line 627:

```fortran
    call reaxFF_fnn(nspeci,nspecj,BOpij,deltapi,deltapj,f4s,f4p,f4pp,  &
                    df4sddpi,df4pddpi,df4ppddpi,             &
                    df4sdbo,df4pdbo,df4ppdbo,                &
                    df4sddpj,df4pddpj,df4ppddpj,             &
                    lgrad1)
    call reaxFF_fnn(nspecj,nspeci,BOpij,deltapj,deltapi,f5s,f5p,f5pp,  &
                    df5sddpj,df5pddpj,df5ppddpj,             &
                    df5sdbo,df5pdbo,df5ppdbo,                &
                    df5sddpi,df5pddpi,df5ppddpi,             &
                    lgrad1)
```

## step 9

Add the following code in the end ofreaxff_funcs.F90 after:

```fortran
subroutine reaxFF_fnn(nspeci,nspecj,BOpij,deltapi,deltapj,f4s,f4p,f4pp,  &
                      df4sddpi,df4pddpi,df4ppddpi,           &
                      df4sdbo,df4pdbo,df4ppdbo,              &
                      df4sddpj,df4pddpj,df4ppddpj,           &
                      lgrad1)

    use reaxFFdata
    use iochannels
#ifdef TRACE
  use trace,          only : trace_in, trace_out
#endif
      implicit none
    !
    !  Passed variables
    !
    ! integer(i4), intent(in)             :: ind
      integer(i4), intent(in)             :: nspeci
      integer(i4), intent(in)             :: nspecj
      real(dp),    intent(in)             :: BOpij
      real(dp),    intent(in)             :: deltapi
      real(dp),    intent(in)             :: deltapj
      real(dp),    intent(out)            :: f4s
      real(dp),    intent(out)            :: f4p
      real(dp),    intent(out)            :: f4pp
      real(dp),    intent(out)            :: df4sddpi
      real(dp),    intent(out)            :: df4pddpi
      real(dp),    intent(out)            :: df4ppddpi
      real(dp),    intent(out)            :: df4sddpj
      real(dp),    intent(out)            :: df4pddpj
      real(dp),    intent(out)            :: df4ppddpj
      real(dp),    intent(out)            :: df4sdbo
      real(dp),    intent(out)            :: df4pdbo
      real(dp),    intent(out)            :: df4ppdbo
      logical,     intent(in)             :: lgrad1
      !logical,    intent(in)             :: lgrad2
    !
    !  Local variables
    !
      integer(i4)                         :: i,j,k,l
      real(dp)                            :: deltapi_boc
      real(dp)                            :: ai(mfn)
      real(dp)                            :: zi(mfn)
      real(dp)                            :: zh(mfn)
      real(dp)                            :: ah(mfn)
      real(dp)                            :: zo(3) 
      real(dp)                            :: ao(3)
      real(dp)                            :: spi(mfn) 
      real(dp)                            :: sph(mfn,mfh)
      real(dp)                            :: spo(3)
      real(dp)                            :: delta_i(mfn,3) != 0.0
      real(dp)                            :: delta_h(mfn,3,mfh) != 0.0
      real(dp)                            :: wh(mfn,mfn) 
      real(dp)                            :: wo(3,mfn) 
      real(dp)                            :: delta(3,3)  != 0.0
      real(dp)                            :: x(3)
      !logical                             :: lbond = .false.

#ifdef TRACE
  call trace_in('reaxff_fnn')
#endif
!
!  Assign local variables of lambda values for this species
!
! using the neural network corrections
! 
      x(1) = deltapi + reaxFFval(1,nspeci) - BOpij !reaxFFval(2,nspeci)
      x(2) = BOpij 
      x(3) = deltapj + reaxFFval(1,nspecj) - BOpij !reaxFFval(2,nspecj)

      call linear(x,reaxFFfwi(:,:,nspeci),reaxFFfbi(:,nspeci),zi,3,mfn)              ! input layer
      call sigmoid(zi,mfn,ai,spi,lgrad1)                                       ! input layer
      !write(ioout,*) 'ai: ',ai

      if (mfh>0) then
         do i = 1,mfh
            if (i==1) then
               call linear(ai,reaxFFfwh(:,:,i,nspeci),reaxFFfbh(:,1,nspeci),zh,mfn,mfn) ! hidden layer
            else
               call linear(ah,reaxFFfwh(:,:,i,nspeci),reaxFFfbh(:,i,nspeci),zh,mfn,mfn) ! hidden layer
            endif
            call sigmoid(zh,mfn,ah,sph(:,i),lgrad1)
         enddo
      endif 

      if (mfh>0) then
         call linear(ah,reaxFFfwo(:,:,nspeci),reaxFFfbo(:,nspeci),zo,mfn,3)         ! out put layer  
      else
         call linear(ai,reaxFFfwo(:,:,nspeci),reaxFFfbo(:,nspeci),zo,mfn,3)         ! out put layer  
      endif   
      call sigmoid(zo,3,ao,spo,lgrad1)

      f4s  = ao(1)
      f4p  = ao(2)
      f4pp = ao(3)
    !
    !  Calculate derivatives
    !
      if (lgrad1) then
         delta_i(1:mfn,1:3) = 0.0_dp
         delta_h(1:mfn,1:3,1:mfh) = 0.0_dp
         delta(1:3,1:3)   = 0.0_dp

         do j = 1,3
            do i = 1,mfn
               delta_i(i,j) = spi(i)*reaxFFfwi(i,j,nspeci)  ! reaxFFwi 6*3 delta_i 6*3
            enddo
         enddo


         if (mfh>0) then
            do l =1,mfh
               do j = 1,mfn
                  do i = 1,mfn
                     wh(i,j) = sph(i,l)*reaxFFfwh(i,j,l,nspeci)
                  enddo
               enddo

  !            matrix multiply
               do j = 1,3
                  do k = 1,mfn
                     do i = 1,mfn
                        if (l==1) then
                           delta_h(i,j,l) = delta_h(i,j,l) + wh(i,k)*delta_i(k,j)   ! 6*3
                        else 
                          delta_h(i,j,l) = delta_h(i,j,l) + wh(i,k)*delta_h(k,j,l-1)  ! 6*3
                        endif
                     enddo
                 enddo
               enddo
            enddo
         endif

         do j =1,mfn        ! Hadamard product
            do i = 1,3
               wo(i,j) =  spo(i)*reaxFFfwo(i,j,nspeci)
            enddo
         enddo
    !    matrix multiply for out put layer
         do j = 1,3
            do k=1,mfn 
               do i=1,3 
                  if (mfh>0) then
                     delta(i,j) = delta(i,j) + wo(i,k)*delta_h(k,j,mfh)        ! output layer
                  else
                     delta(i,j) = delta(i,j) + wo(i,k)*delta_i(k,j)        ! output layer
                  endif
               enddo
            enddo
         enddo
         df4sddpi  = delta(1,1) !  the dirvative matrix
         df4pddpi  = delta(2,1)
         df4ppddpi = delta(3,1)
         
         df4sdbo   = delta(1,2)
         df4pdbo   = delta(2,2)
         df4ppdbo  = delta(3,2)
         
         df4sddpj  = delta(1,3)
         df4pddpj  = delta(2,3)
         df4ppddpj = delta(3,3)
      endif
    
#ifdef TRACE
  call trace_out('reaxff_fnn')
#endif
    !
  return
end
    !
    
subroutine linear(x,w,b,z,m,n)
    ! a linear neural network model m*n
    use reaxFFdata
#ifdef TRACE
  use trace,          only : trace_in, trace_out
#endif
    implicit none
    integer(i4), intent(in)             :: m
    integer(i4), intent(in)             :: n
    real(dp),    intent(in)             :: x(m)
    real(dp),    intent(out)            :: z(n)
    real(dp),    intent(in)             :: w(n,m)
    real(dp),    intent(in)             :: b(n)
    integer(i4)                         :: i
    integer(i4)                         :: j
#ifdef TRACE
  call trace_in('linear')
#endif
    ! m,n = 3,6
    z = 0.0_dp
    
    do i=1,n
       do j=1,m
           z(i) = z(i) + w(i,j)*x(j) 
       enddo
       z(i) = z(i) + b(i)
    enddo 
#ifdef TRACE
  call trace_out('linear')
#endif
  return
end
    
subroutine sigmoid(x,n,s,s_p,lgrad1)
    ! sigmoid activation function
      use reaxFFdata
#ifdef TRACE
  use trace,          only : trace_in, trace_out
#endif
    implicit none

    integer(i4), intent(in)             :: n
    real(dp),    intent(in)             :: x(n)
    real(dp),    intent(out)            :: s(n)
    real(dp),    intent(out)            :: s_p(n)
    logical,     intent(in)             :: lgrad1
    integer(i4)                         :: i
#ifdef TRACE
  call trace_in('sigmoid')
#endif    
    do i=1,n
       s(i) = 1.0_dp/(1.0_dp+exp(-x(i)))
    enddo
    
    if (lgrad1) then
       do i=1,n
          s_p(i) = s(i)*(1.0_dp-s(i))
       enddo
    endif
#ifdef TRACE
  call trace_out('sigmoid')
#endif
  return
end
    
subroutine reaxFF_enn(ind,BOij_s,BOij_pi,BOij_pipi,eij,deijdBO_s,deijdBO_pi,deijdBO_pipi,lgrad1,scale,scald)
    !
    !  Subroutine to calculate the bond energy by neural networ for reaxFF. 
    !
    !  On entry : 
    !
    !  ind             = reaxFF species number of i-j
    !  BOij_s          = sigma bond order primed for i-j
    !  BOij_pi         = \pi bond order primed for i-j
    !  BOij_pipi       = \pi\pi bond order primed for i-j

    !  On exit :
    !
    !  eij             = bond energy of i j
    !  deijdBO_s       = first derivative of eij w.r.t. deijdBO_s if lgrad1
    !  deijdBO_pi      = first derivative of eij w.r.t. deijdBO_pi if lgrad1
    !  deijdBO_pipi    = first derivative of eij w.r.t. deijdBO_pipi if lgrad1
    !
    !  ben             = number of neurons
    !  beh             = number of hidden layers
    !
    use reaxFFdata
    use iochannels
#ifdef TRACE
  use trace,          only : trace_in, trace_out
#endif
      implicit none
    !
    !  Passed variables
    !
      integer(i4), intent(in)             :: ind
      real(dp),    intent(in)             :: BOij_s
      real(dp),    intent(in)             :: BOij_pi
      real(dp),    intent(in)             :: BOij_pipi
      real(dp),    intent(in)             :: scale
      real(dp),    intent(in)             :: scald
      real(dp),    intent(out)            :: eij
      real(dp),    intent(out)            :: deijdBO_s 
      real(dp),    intent(out)            :: deijdBO_pi
      real(dp),    intent(out)            :: deijdBO_pipi 
      logical,     intent(in)             :: lgrad1
    !
    !  Local variables
    !
      integer(i4)                         :: i,j,k
      real(dp)                            :: ai(ben)
      real(dp)                            :: zi(ben)
      real(dp)                            :: zh(ben)
      real(dp)                            :: ah(ben)
      real(dp)                            :: zo(1) 
      real(dp)                            :: ao(1)
      real(dp)                            :: spi(ben) 
      real(dp)                            :: sph(ben)
      real(dp)                            :: spo(1)
      real(dp)                            :: delta_i(ben,3) != 0.0
      real(dp)                            :: delta_h(ben,3) != 0.0
      real(dp)                            :: wh(ben,ben) 
      real(dp)                            :: wo(ben) 
      real(dp)                            :: delta(ben)  != 0.0
      real(dp)                            :: x(3)

#ifdef TRACE
  call trace_in('reaxff_enn')
#endif
!
!  Assign local variables of lambda values for this species
!
! using the neural network corrections
!
      x(1) = BOij_s
      x(2) = BOij_pi
      x(3) = BOij_pipi
        
      call linear(x,reaxFFewi(:,:,ind),reaxFFebi(:,ind),zi,3,ben)            ! input layer
      call sigmoid(zi,ben,ai,spi,lgrad1)  

      if (beh>0) then
         !do i = 1,beh
         call linear(ai,reaxFFewh(:,:,1,ind),reaxFFebh(:,1,ind),zh,ben,ben)     ! hidden layer
         call sigmoid(zh,ben,ah,sph,lgrad1)
         !enddo
      endif
      
      if (beh>0) then
         call linear(ah,reaxFFewo(:,ind),reaxFFebo(:,ind),zo,ben,1)             ! out put layer
      else
         call linear(ai,reaxFFewo(:,ind),reaxFFebo(:,ind),zo,ben,1)
      endif
      call sigmoid(zo,1,ao,spo,lgrad1)

      eij = -scale*reaxFFDe(1,ind)*ao(1)
    !
    !  Calculate derivatives
    !
      if (lgrad1) then
         delta_i(1:ben,1:3) = 0.0_dp
         delta_h(1:ben,1:3) = 0.0_dp
         delta(1:3)   = 0.0_dp
         do j = 1,3
            do i = 1,ben
               delta_i(i,j) = spi(i)*reaxFFewi(i,j,ind)  ! reaxFFwi 6*3 delta_i 6*3
            enddo
         enddo
    
         if (beh>0) then
            do j = 1,ben
                do i = 1,ben
                  wh(i,j) = sph(i)*reaxFFewh(i,j,1,ind)
                enddo
            enddo
    !        matrix multiply
            do j = 1,3
                do k = 1,ben
                  do i = 1,ben
                      delta_h(i,j) = delta_h(i,j) + wh(i,k)*delta_i(k,j)   ! 6*3
                  enddo
                enddo
            enddo
         endif

         do i = 1,ben
            wo(i) =  spo(1)*reaxFFewo(i,ind)
         enddo
    !    matrix multiply
         do i = 1,3
            do k=1,ben 
               if (beh>0) then
                  delta(i) = delta(i) + wo(k)*delta_h(k,i)        ! output layer
               else
                  delta(i) = delta(i) + wo(k)*delta_i(k,i)        ! output layer
               endif
            enddo
         enddo
         deijdBO_s    = -scald*reaxFFDe(1,ind)*delta(1)
         deijdBO_pi   = -scald*reaxFFDe(1,ind)*delta(2)
         deijdBO_pipi = -scald*reaxFFDe(1,ind)*delta(3)
      endif
      !write(ioout,*) 'D:',delta(1),delta(2),delta(3)
#ifdef TRACE
  call trace_out('reaxff_enn')
#endif
    !
  return
end
```


## step 10

Insert the following code in nullpointer.F90 after line 1642:

```fortran
    ! neural network
    nullify(reaxFFfwi)
    nullify(reaxFFfbi)
    nullify(reaxFFfwh)
    nullify(reaxFFfbh)
    nullify(reaxFFfwo)
    nullify(reaxFFfbo)
    nullify(reaxFFewi)
    nullify(reaxFFebi)
    nullify(reaxFFewh)
    nullify(reaxFFebh)
    nullify(reaxFFewo)
    nullify(reaxFFebo)
    ! neural network

```


