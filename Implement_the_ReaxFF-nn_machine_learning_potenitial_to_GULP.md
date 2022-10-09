# Implement the ReaxFF-nn machine learning potenitial to the General Utility Lattice Program

## Step 1

Insert the following code in module.f90 between line 3386-3387:

```fortran
    !! The flowing lines are used by Neural network bond-correction
    logical,                                       save :: lreaxFFNNboCorrect = .false. ! used by gfeng
    integer(i4),                                   save :: mfn = 9  
    integer(i4),                                   save :: mfh = 1  
    real(dp),         dimension(:,:,:),   pointer, save :: reaxFFfwi => null() 
    real(dp),         dimension(:,:),     pointer, save :: reaxFFfbi => null()   
    real(dp),         dimension(:,:,:,:), pointer, save :: reaxFFfwh => null()  
    real(dp),         dimension(:,:,:),   pointer, save :: reaxFFfbh => null()  
    real(dp),         dimension(:,:),     pointer, save :: reaxFFfwo => null()  
    real(dp),         dimension(:,:),     pointer, save :: reaxFFfbo => null() 
    logical,                                       save :: lreaxFFNNEbond = .false. ! used by gfeng
    integer(i4),                                   save :: ben = 9  
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
    !! The above lines are used by Neural network bond-correction
```

