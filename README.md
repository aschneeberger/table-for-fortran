# table-for-fortran
Table for fortran works in a similar way as `astropy` or `pandas` python libraries. At the moment it can only read and write CSVs. 

# Installation

## Git submodule  

This module can be installed as a git submodule in your git project. To do so, go in your source code folder and run the following command:
```
git submodule add git@github.com:aschneeberger/table-for-fortran.git
```
A folder `table-for-fortran` should appears in your source code folder. 
## Integrating it to your project 

The `TABLE` module is contained in the `table.f90` file. To integrate it in your project, you need to import the module in your project and then to compile it as a module object with your source code. 

To import the module in your main program just add the following line at the beginning of your code: 

````fortran 
USE TABLE
````

Then compile it as a module object, run the following comment:  

```
gfortran -c [PATH-TO-MODULE]/src/table.f90 
```
Finally compile the module object `table.o` with your programme.
````
gfortran [YOUR-CODE] table.o 
````

# Usage 

## The Table

The Table is defined as a `type` called `data_table`. This type contains :
 - `n_cols` : The number of columns 
 - `n_rows` : The number of rows 
 - `header` : All column headers (an array of character strings)
 - `table`  : The table itself, a matrix of `double precision` of dimension `(n_rows,n_cols)`

All objects in the `data_table` type can accessed as in any FORTRAN class with the `%` symbol. 

__Example__: 
```fortran
type(data_table) : table ! Declare the table 
table%n_cols = 2         ! Set the number of columns to 2 
```

## Create a Table 

### From existing data 

A table can be created from existing data with the `create_table` function. 

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

You can read CSVs with the TABLE module. The CSV table is transposed in a `data_table`. You only need to know the path and the CSV file name. 

__Example__
```Fortran 
USE TABLE  ! Import the table  module 

type(data_table) : my_table  ! declare the table
 
! Read a CSV file and import it in the table
my_table = read_csv('path_to_file','filename')
```

## Read the table

Each column in the table can be accessed with its header via the method `get_column`. You only need to declare an `allocatable` array before. 

__Example__

If we want to get the column with the header `colname1` in the table `my_table` from the previous examples.
```fortran
! Declare the array where the column will be stored
double precision , dimension(:) , allocatable :: retreived_data 

! Get the the column from the table 
retrieved_data = my_table%get_column('column1')
```

## Modify the table 

### Add a column

To add a column in your table after its creation use the `add_column` method. The column you want to add must have the same number of rows as the `data_table` which you want to append the column.

__Example__

If you want to add a column with header `colname4` and containing the data in the array `data4` in the table `my_table` from the previous examples

```Fortran
! Create the array withe size 4, the number of rows in my_table
double precision, dimension(4) :: data4 

! Add the column to my_table
my_table%add_column('column4',data4)
```

## Save the table in a CSV 

To save the table in a CSV file, use the  `write_csv` method. It will save the table in a formatted double precision csv table. 

__Example__

To save the table in file `my_table.csv` in the folder `my_folder`: 

```Fortran 
! Using the table my_table from previous examples 

my_table%write_csv('my_folder','my_table.csv')

```

# Future development 

There are still some methods that need to be implemented such as an `add_row` method. Don't hesitate to submit pull requests or open issues if you want to add some features.