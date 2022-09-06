!! @author : Antoine Schneeberger 
!! @mail : antoine.schneeberger@protonmail.com


module TABLE 
    ! Module made for working environment management 
    ! It containes all path variables and will aim 
    ! to contains parallelisation status. 
    ! it will work like a classe. As I don't like it, 
    ! and produces more error pronne program at runtime,
    ! i try to reduce the class implementation to the minimum


    implicit none 

    ! Table type, table caracteristics
    type data_table
        integer :: n_rows                                           ! Number of rows in the table 
        integer :: n_cols                                           ! Number of columns 
        character(len=50), dimension(:), allocatable :: header      ! Header, column names 
        double precision, dimension(:,:), allocatable :: table      ! The table itself under the form of a matrix 

        contains 
        ! Procedure that modify or read the table  
        procedure , public :: get_column                     ! Return the array corresponding the provided column name
        procedure , public :: add_column                     ! Add a column to the table
        procedure , public :: write_csv                      ! Write the table in a CSV file

    end type 

    contains 


    function listdir(path) 
        !Function that return a string containing all directories and files
        !-----
        !INPUT
        !-----
        !
        ! PATH : character*n : path to the target location (absloute)
        !
        !------
        !OUTPUT
        !------
        !
        ! listdir : character*n : string with all the elements in the path sparated by a comma

        !IN/OUT
        CHARACTER(len=*) :: path  !The size of path is inferred from the input var 
        CHARACTER(len=:) , ALLOCATABLE :: listdir      
        CHARACTER(len=255) :: file


        !INTERNALS
        integer :: stat !temp file reading status

        !The strategie is to write the ouput ofa ls command to a tmp file and then read it
        call execute_command_line("ls "//Trim(path)//" >> "//Trim(path)//"/listdir.tmp")

        !Open the listdir.tmp file 
        open(unit=70,file=Trim(path)//"/listdir.tmp",status='old',action='read')

        !Parse the files in a character array 
        do   
            read(70,'(A)',iostat=stat) file
            !verify if we are at end of file, if so the loop stop
            IF(IS_IOSTAT_END(stat)) exit

            !concatenate all files with a delimiter 
            listdir=listdir//Trim(file)//','

        end do  
        close(unit=70)

        !garbage collection, the tmp file is not needed anymore
        call execute_command_line("rm "//Trim(path)//"/listdir.tmp")
        
    end function 

    subroutine write_csv(me,path,fname)
        ! Subroutine parsing, formating and writing datas in a file
        !-----
        !INPUT
        !-----
        !
        ! fname : character : name of the  file 
        ! path : character : path to the file 

        !IN/OUT
        class(data_table) :: me 
        character(len=*),intent(in) :: fname ! file name 
        character(len=*),intent(in) :: path ! file name 
        

        !Internals
        integer :: i,j ! iterators 
        character(len=1000) :: line 
        integer :: n_character


        ! Create the file 
        open(unit=300,file=Trim(path)//'/'//Trim(fname),status='new')

        ! Write the columns names
        103 format(*(A,","))
        
        write(300,103) (trim(me%header(j)) , j=1,me%n_cols)
        
        ! Format and write values
        101 format(*(E24.17e3, ","))
        do i=1,me%n_rows
            ! write each line of the file 
            write(line,101) (me%table(i,j) , j=1,me%n_cols)
            ! remove the last comma before write
            n_character = len(trim(line))
            write(300,'(A)') line(:n_character-1)
        end do 

        close(unit=300)
        
    end subroutine 

    function read_csv(path,fname)
        ! Subroutine reading csv files and parsing them into an array of colnames 
        ! and matrix of values. 
        !
        !-------
        ! INPUT
        !-------
        !
        ! fname : character : Name of the file 
        ! path : character : path to the file 
        ! 
        !-------
        ! OUTPUT
        !-------
        !
        ! colnames : character*n : array of columns names
        !
        ! vales : double precision : matrix reprenting the data_table
        
        !IN/OUT

        character(len=*), intent(in) :: fname
        character(len=*), intent(in) :: path
        
        type(data_table) :: read_csv

        ! INTERNALS 
        character(len=1000) :: line_string                                            ! First line of the file                                    
        integer :: n_rows, n_cols                                                       ! Number of columns and rows 
        character(len=24), dimension(:), allocatable  :: line_split  
        character(len=50), dimension(:), allocatable :: header                    ! Array of column names 

        integer :: j,k                                                                  ! Iterators 

        ! Get the number of rows (-1 for the header)
        read_csv%n_rows = get_number_of_lines(path,fname) - 1

        ! Open the file 
        open(unit=300,file=Trim(path)//'/'//Trim(fname),status='old')     ! Open the file

        ! The first line is an array of colnames        
        
        read(300,'(A)') line_string                    ! Get the first line  
        header = split(line_string,',')                ! Split it into an array of column names 
        read_csv%n_cols = size(header)                    ! Get the Number of columns
        
    
        ! Allocate the matrix table size 
        allocate(read_csv%table(read_csv%n_rows,read_csv%n_cols),read_csv%header(n_cols))

        read_csv%header = header 

        ! read the table 
        do j=1, read_csv%n_rows 

            ! get the line as a character string 
            read(300,'(A)') line_string  

            ! Split the line in an array of character string 
            line_split = split(line_string,',')

            ! Convert it into double precision for each values 
            do k=1, read_csv%n_cols
                read(line_split(k),'(E24.17e3)') read_csv%table(j,k)
            end do 

        end do 

        close(300)
        
    end function 


    function get_number_of_lines(path,fname)
        ! Get the number of line of a file
        !
        !-------
        ! INPUT
        !-------
        !  
        ! path  : character : Path to the file 
        ! fname : character : Filename 
        ! 
        !--------
        ! OUTPUT
        !--------
        !
        ! get_number_of_lines : integer : number of line in the file 

        !IN/OUT

        character(len=*) :: fname       ! File name 
        character(len=*) :: path        ! Path to the file
        integer :: get_number_of_lines  ! Number of line in the file 

        ! INTERNAL
        integer :: io                   ! read_status

        !Open the file 
        open(unit=200, file=Trim(path)//'/'//Trim(fname),status='old')

        ! initialize the counter
        get_number_of_lines = 0 

        ! loop while we do not reach the end of file
        do     
            ! read a line 
            read(200,*,iostat=io)

            ! If we reach the end of the file (iostat != 0) exit the loop  
            if (io /= 0) exit 
            !increment the counter
            get_number_of_lines = get_number_of_lines + 1

        end do

        close(200)

    end function 
    


    function split(str,token)
        ! Split a character string using a token.
        !
        !-------
        ! INPUT
        !-------
        !
        ! str : character : String to split into chunks
        ! token : character len=1 : Separator
        !
        !--------
        ! Output
        !--------
        !
        ! split : character array : array of the splited str

        ! IN/OUT
        character(len=*) :: str                               ! Input character string 
        character(len=1) :: token                             ! Separator token 

        character(len=50), allocatable, dimension(:) :: split ! Output array of character strings (default size of 1024)

        ! INTERNALS
        integer :: i                                          !  Iterator
        integer :: itoken , itmp
        character(len=50) ,dimension(1024) :: tmp             ! Output array of character strings (default size of 1024)


        ! Go through the string and find index of tokens 
        
        i=1                         ! Initialise the interator 
        itoken= scan(str,token)     ! Initialise with the index of the first token  
        tmp(1) = str(:itoken-1)     ! transfer the first string in a temp array 
        
        ! Loop while there are remaining tokens
        do 
            i=i+1                                           ! Increment the interator 
            itmp = scan(str(itoken+1:),token) + itoken      ! Find the next token. Beginning from the last found token 
            
            if (itmp == itoken) then                        ! If scan return 0, there are no more tokens
                tmp(i) = str(itoken+1:)                     ! Get the last bit of string 
                exit                                        ! Exit the loop 
            end if 

            ! Store the string in the tmp array 
            tmp(i) = str(itoken+1:itmp-1)
            
            ! Old  token index replanced with the new one 
            itoken = itmp

        end do 

        ! Allocate the output array with the number of token found + 1 
        allocate(split(i)) 
        
        ! Transfert to the output array the string stored in the tmp array
        split = tmp(:i)


    end function 

    function get_column(me,colname)
        ! Return the values of a column from a table
        ! 
        !-------
        ! INPUT
        !-------
        !
        ! me : data_table : data_table where the column is needed 
        ! colname : character : name of the column 
        !
        !--------
        ! OUTPUT
        !--------
        !
        ! read_column : double precision array : values from the table

        ! IN/OUT
        character(len=*) :: colname
        class(data_table) :: me         
        double precision, dimension(:), allocatable :: get_column 

        ! INTERNALS
        integer :: iloc = 0
        integer :: i

        ! Scan all column name of the header to get the column index
        do i=1, me%n_cols
            if (me%header(i) == colname ) then 

                iloc = i 
                exit 

            end if 
        end do

        ! If the location index is 0, it mean that the asked column does not exist 
        if (iloc == 0) then
            ! Return errors 
            write(30,*) "[TABLE] The column", colname, "is not in the table" 
            write(30,*) "[TABLE] Provided header : ", me%header 
            stop
        end if 

        ! Allocate the output array with the table number of rows 
        allocate(get_column(me%n_rows))

        ! Return the data in the column
        get_column = me%table(:,iloc)

    end function 

    subroutine add_column(me,header,col_data)
        ! Add a column to the data table 
        !
        !-------------
        ! INPUT/OUPUT
        !-------------
        !
        ! me : data_table [IN/OUT] : the data table at which we add a column
        ! header : character [IN] : the name of the new column 
        ! col_data : double precision array [IN] : array containing the column data 

        ! IN/OUT

        class(data_table), intent(inout) :: me 
        character(len=*), intent(in) :: header
        double precision, dimension(:), intent(in) :: col_data

        ! INTERNALS
        double precision, dimension(:,:), allocatable :: new_table
        character(len=50), dimension(:), allocatable :: new_header

        ! Before doing anything, check of the added column have the same size
        ! as the data_table columns
        if (size(col_data) /= me%n_rows) then 
            ! If not, return an error and stop 
            write(30,*) "[TABLE] Added column does not have the same number of rows as the table "
            write(30,*) "[TABLE] Header of the added column :", trim(header)
            stop
        end if 

        ! Allocate the new table with a n_cols+1 column
        allocate(new_table(me%n_rows,me%n_cols + 1)) 

        ! Allocate the new header with n_cols + 1 column
        allocate(new_header(me%n_cols + 1))

        ! Transfert header names in the new header 
        new_header(:me%n_cols) = me%header

        ! Transfert table data to the new one 
        new_table(:,:me%n_cols) = me%table

        ! Update the number of column in the table 
        me%n_cols = me%n_cols + 1 
        
        ! The last element of the header array is the added column's name 
        new_header(me%n_cols) = header 
        
        ! Idem with the table 
        new_table(:,me%n_cols) = col_data

        ! Transfert the new header to the data_table header 
        call move_alloc(new_header, me%header)

        ! Idem with the table 
        call move_alloc(new_table, me%table)

    end subroutine 

    function create_table(header,data_array,n_rows,n_cols)
        ! Create a data_table using an array of headers and data
        ! 
        ! USAGE : create_table('a,b,c',[A,B,C]) 
        ! Where A,B,C are the arrays correponding to the headers 'a','b','c'
        !
        !-------
        ! INPUT
        !-------
        !
        ! header : charater string : string of all the header separated by a comma
        ! data_array : double precision array : 1D data array of form [col1,col2,col3,..,colN]
        !
        !-------
        ! OUPUT
        !-------
        !
        ! create_table : type(data_table) : resulting data table

        ! IN/OUT
        character(len=*) :: header
        double precision, dimension(:) :: data_array  
        integer :: n_cols, n_rows

        type(data_table) :: create_table 

        ! Internals 
        character(len=50), dimension(:), allocatable :: tmp_header 
        integer :: i,j

        ! Check if the  size of data_array is coherent with the asked numbers of rows and cols 
        if (n_rows * n_cols .ne. size(data_array)) then 
            ! If not, output errors and stop programms

            WRITE(*,*) "[TABLE] ERROR In create_table function, data_array size not equal to n_rows * n_cols"
            WRITE(*,*) "Header :", header
            Write(*,*) "Asked number of rows: " , n_rows
            Write(*,*) "Asked number of columns" , n_cols
            WRITE(*,*) "Array length :" , size(data_array)
            Write(*,*) "n_cols * n_rows" , n_cols * n_rows

            stop
        end if 

        ! Allocate the number of columns and rows  
        create_table%n_cols = n_cols
        create_table%n_rows = n_rows

        ! Allocate header and table arrays in create_table object
        allocate(create_table%header(n_cols))
        allocate(create_table%table(n_rows,n_cols))

        ! First get the header array
        tmp_header = split(header,",")              ! Create the temporary header array 

        ! verify if there is enought header entries
        if (size(tmp_header) /= n_cols) then  
            ! If not, output errors and stop programms
            
            write(*,*) "[TABLE] Not the same number of header entries and asked number of columns"
            write(*,*) "Header: ", header
            write(*,*) "Header size: ", size(tmp_header)
            write(*,*) "Asked number of columns", n_cols
            
            stop
        end if 

        ! store the header in the object 
        create_table%header = tmp_header

        ! Fill the table
        do i=1,n_rows
            do j=1, n_cols

            ! Fill the table row by row
            create_table%table(i,j) =  data_array(i+(j-1)*n_rows)

            end do 
        end do 


    end function 

end module TABLE