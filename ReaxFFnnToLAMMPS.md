## Implement the ReaxFF-nn machine learning potenitial to the LAMMPS Program
insert the following code in reaxff_types.h between line 170-184
```c++
struct network_parameters{
  double wi1[4][27];
  double bi1[4][9];
  double wo1[4][27];
  double bo1[4][3];
  double wh1[4][81];
  double bh1[4][9];
  double wi2[10][27];
  double bi2[10][9];
  double wo2[10][9];
  double bo2[10][1];
  double wh2[10][81];
  double bh2[10][9];
};
```

insert the following code in reaxff_ffield.cpp in line 128

 ```c++
 auto &fenn = reax->fenn;
 ```



insert the following code in reaxff_ffield.cpp in line 152

 ```c++
 memory->destroy(fenn);
 ```



insert the following code in reaxff_ffield.cpp in line 174

```c++
memory->create(fenn,n,"reaxff:fenn");
```

insert the following code in reaxff_ffield.cpp in line 197

```c++
memset(&fenn[0],0,sizeof(network_parameters)*n);
```

insert the following code in reaxff_ffield.cpp between line 650-767

```c++
//next lines is read network parametersconst 
int alnt = 12;
const int nt = 4;
const int ns = 27;
lineno += 7;
for (i = 0; i < alnt; ++i){
for (j = 0; j < nt; ++j) {
    values = reader.next_values(0);
    lineno += 3;
    if (values.count() < 9)
     THROW_ERROR("Invalid force field file format");
//copy element symbol in uppercase and truncate stored element symbol if necessary// auto element =uppercase(values.next_string());
//strncpy(sbp[i].name,element.c_str(),2);
      //next lines is read network parameters
        const int alnt   = 12;
        const int nt     = 4;
        const int ns     = 27;
        lineno += 7;
        for (i = 0; i < alnt; ++i){
          for (j = 0; j < nt; ++j) {


            values = reader.next_values(0);
            lineno += 3; 

            if (values.count() < 9)
              THROW_ERROR("Invalid force field file format");

            // copy element symbol in uppercase and truncate stored element symbol if necessary
            // auto element = uppercase(values.next_string());
            // strncpy(sbp[i].name,element.c_str(),2);
            // sbp[i].name[3] = '\0';
            
            for (k = 0; k < ns; ++k){
              fenn[i].wi1[j][k]       = values.next_double();
              if ((k+1) % 9 == 0)
                ++lineno;
            ++lineno;
            }
          }
          lineno += 2;
          for (j = 0; j < nt; ++j){
            for (k = 0; k < 9; ++k){
              fenn[i].bi1[j][k]       = values.next_double();
            }
            ++lineno;
          }
          lineno += 2;
          for (j = 0;j < nt; ++j){
            for (k = 0; k < ns; ++k){
              fenn[i].wo1[j][k]       = values.next_double();
              if ((k+1) % 3 == 0){
                ++lineno;
              }
              ++lineno;
            }
          }
          lineno += 2;
          for (j = 0;j < nt; ++j){
            for (k = 0; k < 3; ++k){
             fenn[i].bo1[j][k]       = values.next_double();
              }
              ++lineno;
            }

          lineno += 2;
          for (j = 0; j < nt; ++j){
            for (k = 0; k < 81; ++k){
              fenn[i].wh1[j][k]       = values.next_double();
              if ((k+1) % 9 == 0)
                ++lineno;
            }
            ++lineno;
          }
          lineno += 2;
          for (j = 0;j < nt; ++j){
            for (k = 0; k < 9; ++k){
              fenn[i].bh1[j][k]       = values.next_double();
              }
              ++lineno;
            }
          
          lineno += 7;
          for (j = 0; j < 10; ++j){
            for (k = 0; k < 27; ++k){
              fenn[i].wi2[j][k]       = values.next_double();
              if ((k+1) % 9 == 0){
                ++lineno;
              }
            ++lineno;
            }
          }
          lineno += 2;
          for (j = 0;j < 10; ++j){
            for (k = 0; k < 9; ++k){
              fenn[i].bi2[j][k]       = values.next_double();
              }
              ++lineno;
            }
          lineno += 2;
          for (j = 0; j < 10; ++j){
            for (k = 0; k < 9; ++k){
              fenn[i].wo2[j][k]       = values.next_double();
            }
            ++lineno;
          }
          lineno += 2;
          for (j = 0; j < 10; ++j){
            for (k = 0; k < 1; ++k){
              fenn[i].bo2[j][k]       = values.next_double();
            }
            ++lineno;
          }
          lineno += 2;
          for (j = 0; j < 10; ++j){
            for (k = 0; k < 81; ++k){
              fenn[i].wh2[j][k]       = values.next_double();
              if ((k+1) % 9 == 0){
                ++lineno;
              }
            ++lineno;
            }
          }
          lineno += 2;
          for (j = 0; j < 10; ++j){
            for (k = 0; k < 9; ++k){
              fenn[i].bh2[j][k]       = values.next_double();
              }
            ++lineno;
            }
        }
```



 