# table-for-fortran
Table for fortran working in a similar way as for `astropy` or `pandas`. At the momment can only read and write csv. 

# Installation

## Git submodule  

This module can be used as a git submodule in your git project. Go in your source code folder and run the command:
```
git submodule add git@github.com:aschneeberger/table-for-fortran.git
```
A folder `table-for-fortran` should appear in your source code folder. 
## Integrating it to your project 

The `TABLE` module is contained in the `table.f90`. You can integrate it to your project. First by compiling the source code as a module: 

```
gfortran -c [PATH-TO-MODULE]/src/table.f90 
```
And then compiling the module object `table.o` with your programme.
````
gfortran [YOUR-CODE] table.o 
````
To import the module in your main programm just add the following line: 

````fortran 
USE TABLE
````

# Usage 

## The Table

The Table is defined as a `type` called `data_table`. This type contains :
 - `n_cols` : The number of columns 
 - `n_rows` : The number of rows 
 - `header` : All column headers (an array of character strings)
 - `table`  : The table itself, a matrix of `double precision` of dimension `(n_rows,n_cols)`

All objects in the data_table type can accessed as in any FORTRAN classes with the `%`symbole. 

__Example__: 
```fortran
type(data_table) : table ! Declare the table 
table%n_cols = 2         ! Set the number of columns to 2 
```

## Create a Table 

### From existing data 

A table can be created from existing data by using the `create_table` function. 

__Example__
````Fortran
USE TABLE ! Import the table module 

type(data_type) : my_table ! Declare the table 

double precision, dimension(4) :: data1, data2, data3 ! data array with 4 rows each  

! fill your data arrays before

! Then create the table with 4 rows and 3 columns
my_table = create_table('colname1,colname2,colname3',[data1,data2,data3],4,3) 
````

### From a CSV 

You can read CSVs with the TABLE module. The CSV table is is transposed in a `data_table`. You only need to know the path and the csv name. 

__Example__
```Fortran 
USE TABLE  ! Import the table moduel 

type(data_table) : my_table  ! declare the table
 
! Read a CSV file and import it in the table
my_table= read_csv('path_to_file','filename')
```

## Read the table

Each column in the table can be accessed with its header via the method `get_column`. You only need to declare an `allocatable` array before. 

__Example__

If we want to get the column with the header `colname1` in the table `my_table` from the previous examples.
```fortran
! Declare the array where the column will be stored
double precision , dimension(:) , allocatable :: retreived_data 

! Get the the column from the table 
retreived_data = my_table%get_column('column1')
```

## Modify the table 

### Add a column

It is possible you want to add a column in your table after it is created. The column you want to add must have the same number of rows as the `data_table` where you add the column. You can add a column with the `add_column` method. 

__Example__

If you want to add a column with header `colname4` and containing the data in the array `data4` in the table `my_table` from the previous examples

```Fortran
! Create the array withe size 4, the number of rows in my_table
double precision, dimension(4) :: data4 

! Add the column to my_table
my_table%add_column('column4',data4)
```

# Future development 

There are still some method that need to be implemented such as an `add_row` method. Don't hesitate to submit pull request or open issues if you want to add some features.